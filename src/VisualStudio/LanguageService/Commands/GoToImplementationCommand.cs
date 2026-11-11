//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.FindAllReferences;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Threading;
using XSharp.LanguageService.FindReferences;
using XSharp.LanguageService.ForeignSource;
using XSharp.LanguageService.GoToImplementation;
using XSharp.Settings;
using XSharp.Support;
using XSharpModel;
using Task = System.Threading.Tasks.Task;

namespace XSharp.LanguageService.Commands
{
    /// <summary>
    /// Go To Implementation (Ctrl+F12) and Go To Base (Alt+Home) for X#: down to derived classes, implementers and
    /// overrides, up to the base class, interfaces and the overridden member. The starting symbol may come from a
    /// DLL; results come from the X# source (Go To Base may also reach a DLL type). One result: jump there; several:
    /// the Find All References window.
    /// </summary>
    internal class GoToImplementationCommand : AbstractCommand
    {
        public static async Task InitializeAsync()
        {
            var implementation = await VS.Commands.FindCommandAsync("Edit.GoToImplementation");
            var goToBase = await VS.Commands.FindCommandAsync("Edit.GoToBase");
            if (implementation != null)
                await VS.Commands.InterceptAsync(implementation, () => Execute(GoToImplementation));
            // Ctrl+F12 is bound to Go To Implementation only in the C# and VB editors; elsewhere it is Go To Declaration
            await VS.Commands.InterceptAsync(VSConstants.VSStd97CmdID.GotoDecl, () => Execute(GoToImplementation));
            if (goToBase != null)
                await VS.Commands.InterceptAsync(goToBase, () => Execute(GoToBase));
            else
                Logger.Information("Go To Base: the command Edit.GoToBase does not exist in this Visual Studio");
        }

        private static void GoToImplementation(DocumentView doc)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            Run(doc, true);
        }

        private static void GoToBase(DocumentView doc)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            Run(doc, false);
        }

        private static void Run(DocumentView doc, bool down)
        {
            var command = down ? "Go To Implementation" : "Go To Base";
            ThreadHelper.ThrowIfNotOnUIThread();
            IXSymbol target;
            CompletionState state;
            try
            {
                target = TargetUnderCursor.Resolve(doc.TextView, out _, out state, out _);
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, $"{command}: cannot determine the symbol under the cursor");
                target = null;
                state = default;
            }
            if (!(target is IXTypeSymbol) && !(target is IXMemberSymbol member && IsMethodOrProperty(member)))
            {
                VS.StatusBar.ShowMessageAsync(target == null ? "No X# symbol under the cursor"
                    : $"{command}: '{target.Name}' is no class, interface, method or property").FireAndForget();
                return;
            }
            var textView = doc.TextView;
            // Do not wait for the search on the UI thread: Find References does the same (see there)
            var package = XSharpLanguagePackage.Instance;
            var factory = package?.JoinableTaskFactory ?? ThreadHelper.JoinableTaskFactory;
            factory.RunAsync(async delegate
            {
                var watch = System.Diagnostics.Stopwatch.StartNew();
                IReadOnlyList<object> results;
                var hierarchy = new SourceHierarchy();
                try
                {
                    await TaskScheduler.Default;
                    results = down ? Down(target, hierarchy) : Up(target, hierarchy);
                }
                catch (Exception ex)
                {
                    Logger.Exception(ex, $"{command} '{target.Name}' failed");
                    results = Array.Empty<object>();
                }
                Logger.Information($"{command} '{target.FullName}': {results.Count} of {hierarchy.Candidates} candidates, {watch.ElapsedMilliseconds} ms");
                await ShowAsync(command, down, target, results, hierarchy, textView, state);
            }).Task.Forget();
        }

        private static bool IsMethodOrProperty(IXMemberSymbol member)
        {
            return member.Kind == Kind.Method || SymbolIdentity.IsPropertyLike(member.Kind);
        }

        /// <summary>
        /// Derived classes and implementers (as TypeNodes, straight from the database rows), or overrides and
        /// implementing members: only the types that declare a member of that name are loaded for the slot check.
        /// </summary>
        private static IReadOnlyList<object> Down(IXSymbol target, SourceHierarchy hierarchy)
        {
            if (target is IXTypeSymbol type)
                return Types(SourceHierarchy.NodeOf(type)).Cast<object>().ToList();
            var member = (IXMemberSymbol)target;
            if (member.ParentType == null)
                return Array.Empty<object>();
            var nodes = Types(SourceHierarchy.NodeOf(member.ParentType));
            var withMember = hierarchy.TypesWithMember(member.Name);
            var types = nodes.Where(n => withMember.Contains(n.Key)).Select(hierarchy.SymbolOf).Where(t => t != null);
            return TypeHierarchy.Overrides(member, types).Cast<object>().ToList();

            IReadOnlyList<TypeNode> Types(TypeNode parent) => parent.Kind == Kind.Interface
                ? hierarchy.Implementers(parent)
                : hierarchy.DerivedTypes(parent);
        }

        /// <summary>The base class and interfaces, or the overridden member and the implemented interface members.</summary>
        private static IReadOnlyList<object> Up(IXSymbol target, SourceHierarchy hierarchy)
        {
            if (target is IXTypeSymbol type)
                return TypeHierarchy.BaseTypes(type, hierarchy.BaseOf, hierarchy.InterfacesOf).Cast<object>().ToList();
            return TypeHierarchy.BaseMembers((IXMemberSymbol)target, hierarchy.BaseOf, hierarchy.InterfacesOf).Cast<object>().ToList();
        }

        private static async Task ShowAsync(string command, bool down, IXSymbol target, IReadOnlyList<object> results, SourceHierarchy hierarchy,
            ITextView textView, CompletionState state)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            if (results.Count == 0)
            {
                await VS.StatusBar.ShowMessageAsync(down
                    ? $"{command}: no implementations of '{target.Name}' in the X# source"
                    : $"{command}: '{target.Name}' has no base in the X# source");
                return;
            }
            if (results.Count == 1)
            {
                Navigate(results[0], hierarchy, textView, state);
                return;
            }
            var hits = results.Select(r => Hit(r, hierarchy)).Where(h => h != null).ToList();
            var service = await VS.GetRequiredServiceAsync<SVsFindAllReferences, IFindAllReferencesService>();
            var window = service.StartSearch(down ? $"'{target.Name}' implementations" : $"'{target.Name}' base types");
            var source = new ReferencesTableDataSource(target, await FindReferencesCommand.CreateTextFormatAsync());
            window.Manager.AddSource(source, ReferencesTableDataSource.Columns);
            source.Add(hits);
            source.Complete();
        }

        private static void Navigate(object result, SourceHierarchy hierarchy, ITextView textView, CompletionState state)
        {
            // a type found in the database: its declaring part (partial classes: the one with INHERIT / IMPLEMENTS)
            if (result is TypeNode node && node.Row != null)
                XDocuments.Open(node.Row.FileName, node.Row.StartLine, node.Row.StartColumn, false);
            else if (result is XSourceEntity entity)
                entity.OpenEditor();
            else if (result is IXSymbol symbol)
                XSharpGotoDefinition.Goto(symbol, textView, state);
        }

        /// <summary>One row for a DLL symbol from a C# or VB project of the solution: its declaration in that source.</summary>
        private static ReferenceHit ForeignHit(IXSymbol symbol)
        {
            IReadOnlyList<ForeignLocation> found = null;
            if (symbol is XPETypeSymbol peType)
                found = ForeignNavigator.Find(peType, peType);
            else if (symbol is XPEMemberSymbol peMember && peMember.Parent is XPETypeSymbol parent)
                found = ForeignNavigator.Find(parent, peMember);
            if (found == null || found.Count == 0)
                return null;
            var first = found[0];
            var lineText = System.IO.File.Exists(first.FilePath)
                ? System.IO.File.ReadLines(first.FilePath).Skip(first.StartLine).FirstOrDefault() : null;
            return DeclarationHit.Create(first.FilePath, null, first.StartLine, first.StartColumn, lineText, first.Name, first.ContainerName);
        }

        /// <summary>One row: the declaration, for a DLL symbol in the generated source F12 opens.</summary>
        private static ReferenceHit Hit(object result, SourceHierarchy hierarchy)
        {
            if (result is TypeNode node)
            {
                var row = node.Row;
                return row == null ? null : DeclarationHit.Create(row.FileName, System.IO.Path.GetFileNameWithoutExtension(row.Project),
                    row.StartLine, row.StartColumn, row.SourceCode, node.Name, node.FullName);
            }
            var symbol = result as IXSymbol;
            var foreign = ForeignHit(symbol);
            if (foreign != null)
                return foreign;
            var entity = symbol as XSourceEntity;
            if (entity == null && symbol is XPETypeSymbol peType)
                entity = XSharpGotoDefinition.FindSystemElement(peType, peType);
            else if (entity == null && symbol is XPEMemberSymbol peMember && peMember.Parent is XPETypeSymbol parent)
                entity = XSharpGotoDefinition.FindSystemElement(parent, peMember);
            if (entity?.File == null)
                return null;
            var sourceCode = (entity as XSourceTypeSymbol)?.SourceCode ?? (entity as XSourceMemberSymbol)?.SourceCode;
            return DeclarationHit.Create(entity.File.FullPath, entity.File.Project?.Name, entity.Range.StartLine, entity.Range.StartColumn,
                sourceCode, entity.Name, (entity as IXMemberSymbol)?.ParentType?.FullName ?? entity.FullName);
        }
    }
}
