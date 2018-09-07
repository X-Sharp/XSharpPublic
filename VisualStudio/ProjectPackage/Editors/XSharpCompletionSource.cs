//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Utilities;
using XSharpModel;
using Microsoft.VisualStudio.Shell;
using System.Windows.Media;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Reflection;
using Microsoft.VisualStudio;
using LanguageService.CodeAnalysis.XSharp;
using System.Diagnostics;
using System.Collections.Immutable;
using XSharpColorizer;
using XSharp.Project.OptionsPages;
using System.Runtime.CompilerServices;
using Microsoft.VisualStudio.Text.Tagging;
using static XSharp.Parser.VsParser;
using LanguageService.CodeAnalysis.Text;
using XSharp.Project;

namespace XSharpLanguage
{
    [Export(typeof(ICompletionSourceProvider))]
    [ContentType("XSharp")]
    [Name("XSharpCompletion")]
    class XSharpCompletionSourceProvider : ICompletionSourceProvider
    {
        [Import]
        internal SVsServiceProvider ServiceProvider = null;

        [Import]
        internal IGlyphService GlyphService = null;

        [Import]
        IBufferTagAggregatorFactoryService aggregator = null;

        public ICompletionSource TryCreateCompletionSource(ITextBuffer textBuffer)
        {

            return new XSharpCompletionSource(this, textBuffer, aggregator);
        }
    }

    partial class XSharpCompletionSource : ICompletionSource
    {
        private ITextBuffer _buffer;
        private bool _disposed = false;
        private XSharpCompletionSourceProvider _provider;
        private bool _settingIgnoreCase;
        // Keep a trace of the Context of the TokenList build
        private IToken _stopToken;

        private XFile _file;
        private bool _coreDialect;
        private bool _showTabs;
        private bool _keywordsInAll;
        private bool _dotUniversal;
        private IBufferTagAggregatorFactoryService aggregator;
        private IntellisenseOptionsPage _optionsPage;

        internal static bool StringEquals(string lhs, string rhs)
        {
            if (String.Equals(lhs, rhs, StringComparison.OrdinalIgnoreCase))
                return true;
            return false;
        }

        public XSharpCompletionSource(XSharpCompletionSourceProvider provider, ITextBuffer buffer, IBufferTagAggregatorFactoryService aggregator)
        {
            _provider = provider;
            _buffer = buffer;
            _file = buffer.GetFile();
            // Currently, set as default, but should be VS Settings Based
            // Retrieve from Project properties later: _file.Project.ProjectNode.ParseOptions.
            var prj = _file.Project.ProjectNode;
            var parseoptions = prj.ParseOptions;
            _coreDialect = parseoptions.Dialect == XSharpDialect.Core;
            _settingIgnoreCase = true;
            _stopToken = null;
            this.aggregator = aggregator;
            var package = XSharp.Project.XSharpProjectPackage.Instance;
            _optionsPage = package.GetIntellisenseOptionsPage();
        }

        public void AugmentCompletionSession(ICompletionSession session, IList<CompletionSet> completionSets)
        {
            XSharpProjectPackage.Instance.DisplayOutPutMessage("-->> AugmentCompletionSessions");
            try
            {
                if (_optionsPage.DisableCodeCompletion)
                    return;
                XSharpModel.ModelWalker.Suspend();
                if (_disposed)
                    throw new ObjectDisposedException("XSharpCompletionSource");
                _showTabs = _optionsPage.CompletionListTabs;
                _keywordsInAll = _optionsPage.KeywordsInAll;
                if (_coreDialect)
                {
                    _dotUniversal = _optionsPage.UseDotAsUniversalSelector;
                }
                else
                {
                    _dotUniversal = false;
                }
                // Where does the StartSession has been triggered ?
                ITextSnapshot snapshot = _buffer.CurrentSnapshot;
                var triggerPoint = (SnapshotPoint)session.GetTriggerPoint(snapshot);
                if (triggerPoint == null)
                    return;
                // The "parameters" coming from CommandFilter
                uint cmd = (uint)session.Properties["Command"];
                VSConstants.VSStd2KCmdID nCmdId = (VSConstants.VSStd2KCmdID)cmd;
                char typedChar = (char)session.Properties["Char"];
                if (typedChar == '\0')
                {

                }
                // Reset the StopToken
                this._stopToken = null;
                // What is the character were it starts ?
                var line = triggerPoint.GetContainingLine();
                ////////////////////////////////////////////
                //
                SnapshotSpan lineSpan = new SnapshotSpan(line.Start, line.Length);
                SnapshotPoint caret = triggerPoint;
                var tagAggregator = aggregator.CreateTagAggregator<IClassificationTag>(_buffer);
                var tags = tagAggregator.GetTags(lineSpan);
                //List<IMappingTagSpan<IClassificationTag>> tagList = new List<IMappingTagSpan<IClassificationTag>>();
                IMappingTagSpan<IClassificationTag> lastTag = null;
                foreach (var tag in tags)
                {
                    //tagList.Add(tag);
                    SnapshotPoint ptStart = tag.Span.Start.GetPoint(_buffer, PositionAffinity.Successor).Value;
                    SnapshotPoint ptEnd = tag.Span.End.GetPoint(_buffer, PositionAffinity.Successor).Value;
                    //tagList.Add(tag);
                    if ((ptStart != null) && (ptEnd != null))
                        if ((caret.Position >= ptStart.Position) && (caret.Position <= ptEnd.Position))
                        {
                            lastTag = tag;
                            break;
                        }
                }
                if (lastTag != null)
                {
                    var name = lastTag.Tag.ClassificationType.Classification.ToLower();
                    // No Intellisense in Comment
                    if (name == "comment")
                        return;
                }
                ////////////////////////////////////////////
                SnapshotPoint start = triggerPoint;
                //while (start > line.Start && !char.IsWhiteSpace((start - 1).GetChar()))
                //{
                //    start -= 1;
                //}
                //
                var applicableTo = snapshot.CreateTrackingSpan(new SnapshotSpan(start, triggerPoint), SpanTrackingMode.EdgeInclusive);
                //
                if (_file == null)
                {
                    // Uhh !??, Something went wrong
                    return;
                }
                // The Completion list we are building
                CompletionList compList = new CompletionList();
                CompletionList kwdList = new CompletionList();
                // The CompletionType we will use to fill the CompletionList
                CompletionType cType = null;
                if (session.Properties.ContainsProperty("Type"))
                {
                    cType = (CompletionType)session.Properties["Type"];
                }
                // Start of Process
                String filterText = "";
                // Check if we can get the member where we are
                var containingline = triggerPoint.GetContainingLine().LineNumber;
                XTypeMember member = XSharpTokenTools.FindMember(containingline, this._file);
                XType currentNamespace = XSharpTokenTools.FindNamespace(triggerPoint.Position, this._file);
                // Standard TokenList Creation (based on colon Selector )
                int currentLine = triggerPoint.GetContainingLine().LineNumber;
                List<String> tokenList = XSharpTokenTools.GetTokenList(triggerPoint.Position, currentLine, _buffer.CurrentSnapshot, out _stopToken, false, _file, false, member);
                // We might be here due to a COMPLETEWORD command, so we have no typedChar
                // but we "may" have a incomplete word like System.String.To
                // Try to Guess what TypedChar could be
                if (typedChar == '\0')
                {
                    if (tokenList.Count > 0)
                    {
                        String extract = tokenList[tokenList.Count - 1];
                        typedChar = extract[extract.Length - 1];
                        if ((typedChar != '.') && (typedChar != ':'))
                        {
                            if (tokenList.Count == 1)
                            {
                                //
                                filterText = tokenList[0];
                                int dotPos = extract.LastIndexOf(".");
                                if (dotPos > -1)
                                {
                                    String startToken = extract.Substring(0, dotPos);
                                    filterText = extract.Substring(dotPos + 1);
                                    typedChar = '.';
                                    tokenList[0] = startToken + ".";
                                }
                            }
                            else
                            {
                                // So, we get the last Token as a Filter
                                filterText = tokenList[tokenList.Count - 1];
                            }
                            // but what could be the typedChar?
                            if (tokenList.Count > 1)
                            {
                                extract = tokenList[tokenList.Count - 2];
                                typedChar = extract[extract.Length - 1];
                                tokenList.RemoveAt(tokenList.Count - 1);
                                //tokenLine = TokenListAsString(tokenList, 0);
                            }
                            // Include the filter as the text to replace
                            start -= filterText.Length;
                            applicableTo = snapshot.CreateTrackingSpan(new SnapshotSpan(start, triggerPoint), SpanTrackingMode.EdgeInclusive);
                        }
                    }
                }
                // Special Phil
                bool dotSelector = (typedChar == '.');
                bool showInstanceMembers = (typedChar == ':');
                //
                if (dotSelector && _dotUniversal)
                {
                    showInstanceMembers = true;
                }
                // Alternative Token list (dot is a selector)
                List<String> altTokenList;
                if (dotSelector && _dotUniversal)
                    altTokenList = XSharpTokenTools.GetTokenList(triggerPoint.Position, currentLine, _buffer.CurrentSnapshot, out _stopToken, false, _file, true, member);
                else
                    altTokenList = tokenList;

                HashSet<String> Usings = new HashSet<String>(_file.Usings, StringComparer.OrdinalIgnoreCase);
                if (currentNamespace != null)
                {
                    Usings.Add(currentNamespace.Name);
                }
                if (member != null && member.Parent != null && (member.Parent.Kind == Kind.Class || member.Parent.Kind == Kind.Interface))
                {
                    var ns = member.Parent.Parent;
                    string nsName = "";
                    if ((ns != null) && (ns.Kind == Kind.Namespace))
                    {
                        nsName = ns.Name + ".";
                    }
                    var name = nsName + member.Parent.Name;
                    var pos = name.LastIndexOf(".");
                    if (pos >= 0)
                    {
                        name = name.Substring(0, pos);
                        if (!Usings.Contains(name))
                        {
                            Usings.Add(name);
                        }
                    }
                }
                // TODO: Based on the Project.Settings, we should add the Vulcan.VO namespace
                int tokenType = XSharpLexer.UNRECOGNIZED;
                if (this._stopToken != null)
                {
                    tokenType = this._stopToken.Type;
                }
                // LookUp for the BaseType, reading the TokenList (From left to right)
                CompletionElement foundElement;
                String currentNS = "";
                if (currentNamespace != null)
                {
                    currentNS = currentNamespace.Name;
                }
                //
                cType = XSharpTokenTools.RetrieveType(_file, tokenList, member, currentNS, null, out foundElement, snapshot, currentLine);
                if (!cType.IsEmpty())
                {
                    session.Properties["Type"] = cType;
                }
                else
                {
                    if (dotSelector && _dotUniversal)
                    {
                        cType = XSharpTokenTools.RetrieveType(_file, altTokenList, member, currentNS, null, out foundElement, snapshot, currentLine);
                        if (!cType.IsEmpty())
                        {
                            session.Properties["Type"] = cType;
                        }
                    }
                }
                //
                if (dotSelector)
                {
                    if (String.IsNullOrEmpty(filterText))
                    {
                        filterText = TokenListAsString(tokenList, 0);
                        if (!filterText.EndsWith("."))
                            filterText += ".";
                    }
                    switch (tokenType)
                    {
                        case XSharpLexer.USING:
                            // It can be a namespace
                            AddNamespaces(compList, _file.Project, filterText);
                            break;
                        case XSharpLexer.AS:
                        case XSharpLexer.IS:
                        case XSharpLexer.REF:
                        case XSharpLexer.INHERIT:
                            // It can be a namespace
                            AddNamespaces(compList, _file.Project, filterText);
                            // It can be Type, FullyQualified
                            // we should also walk all the USINGs, and the current Namespace if any, to search Types
                            AddTypeNames(compList, _file.Project, filterText, Usings);
                            //
                            AddXSharpTypesTypeNames(kwdList, filterText);
                            break;
                        case XSharpLexer.IMPLEMENTS:
                            // It can be a namespace
                            AddNamespaces(compList, _file.Project, filterText);
                            // TODO: add Interfaces only
                            break;
                        default:
                            // It can be a namespace
                            AddNamespaces(compList, _file.Project, filterText);
                            // It can be Type, FullyQualified
                            // we should also walk all the USINGs, and the current Namespace if any, to search Types
                            AddTypeNames(compList, _file.Project, filterText, Usings);
                            //
                            AddXSharpTypesTypeNames(kwdList, filterText);
                            // it can be a static Method/Property/Enum
                            if (cType != null)
                            {
                                // First we need to keep only the text AFTER the last dot
                                int dotPos = filterText.LastIndexOf('.');
                                filterText = filterText.Substring(dotPos + 1, filterText.Length - dotPos - 1);
                                BuildCompletionList(compList, cType, Modifiers.Public, true, filterText);
                            }
                            //
                            break;
                    }
                }
                //
                if (showInstanceMembers)
                {
                    // Member call
                    if (cType != null)
                    {
                        Modifiers visibleAs = Modifiers.Public;
                        if (foundElement != null)
                        {
                            if (String.Compare(foundElement.Name, "self", true) == 0)
                            {
                                visibleAs = Modifiers.Private;
                            }
                            else if (String.Compare(foundElement.Name, "super", true) == 0)
                            {
                                visibleAs = Modifiers.Protected;
                            }

                        }
                        else if (member.ParentName == cType.FullName)
                        {
                            visibleAs = Modifiers.Private;
                        }
                        // Now, Fill the CompletionList with the available members, from there
                        BuildCompletionList(compList, cType, visibleAs, false, filterText);
                    }
                }
                //
                if (!dotSelector && !showInstanceMembers)
                {
                    // Empty line ?
                    // If we have only one Token, it can be the start of a Parameter/Local/Property/Method/Type/...
                    // .........
                    // .........
                    // We were able to determine the Type, so Get the Members
                    //if (cType != null)
                    //{
                    //    Modifiers visibleAs = Modifiers.Public;
                    //    if (member.ParentName == cType.FullName)
                    //    {
                    //        visibleAs = Modifiers.Private;
                    //    }
                    //    // Now, Fill the CompletionList with the available members, from there
                    //    BuildCompletionList(compList, cType, visibleAs, false, filterText);

                    //}
                    //else
                    {
                        switch (tokenType)
                        {
                            case XSharpLexer.USING:
                                // It can be a namespace
                                AddNamespaces(compList, _file.Project, filterText);
                                break;
                            case XSharpLexer.AS:
                            case XSharpLexer.IS:
                            case XSharpLexer.REF:
                            case XSharpLexer.INHERIT:
                                // It can be a namespace
                                AddNamespaces(compList, _file.Project, filterText);
                                // It can be Type, FullyQualified
                                // we should also walk all the USINGs, and the current Namespace if any, to search Types
                                AddTypeNames(compList, _file.Project, filterText, Usings);
                                //
                                AddXSharpTypesTypeNames(kwdList, filterText);
                                break;
                            case XSharpLexer.IMPLEMENTS:
                                // It can be a namespace
                                AddNamespaces(compList, _file.Project, filterText);
                                // TODO: add Interfaces only
                                break;
                            default:
                                if (member != null)
                                {
                                    // Fill with the context ( Parameters and Locals )
                                    BuildCompletionList(compList, member, filterText, currentLine);
                                    // Context Type....
                                    cType = new CompletionType(member.Parent.Clone);
                                    if (!cType.IsEmpty())
                                    {
                                        // Get the members also
                                        BuildCompletionList(compList, cType, Modifiers.Private, false, filterText);
                                    }
                                }
                                // Now Add Functions and Procedures
                                BuildCompletionList(compList, _file.Project.Lookup(XType.GlobalName, true), Modifiers.Public, false, filterText);
                                // and Add NameSpaces
                                AddNamespaces(compList, _file.Project, filterText);
                                // and Types
                                AddTypeNames(compList, _file.Project, filterText, Usings);
                                //
                                AddXSharpTypesTypeNames(kwdList, filterText);
                                //
                                AddUsingStaticMembers(compList, _file, filterText);
                                break;
                        }
                    }
                }
                // Add Keywors to the ALL Tab ?
                if ((kwdList.Count > 0) && _keywordsInAll)
                {
                    foreach (var item in kwdList.Values)
                        compList.Add(item);
                }
                // Sort in alphabetical order
                // and put in the SelectionList
                var values = compList.Values;
                // Create the All Tab
                completionSets.Add(new CompletionSet("All", "All", applicableTo, values, Enumerable.Empty<Completion>()));
                if (_showTabs)
                {
                    if (compList.HasEnumMembers)
                    {
                        var sub = values.Where(item => item.Kind == Kind.EnumMember);
                        completionSets.Add(new CompletionSet("Values", "Values", applicableTo, sub, Enumerable.Empty<Completion>()));
                    }
                    if (compList.HasMethods)
                    {
                        var sub = values.Where(item => item.Kind == Kind.Method);
                        completionSets.Add(new CompletionSet("Methods", "Methods", applicableTo, sub, Enumerable.Empty<Completion>()));
                    }
                    if (compList.HasFields)
                    {
                        var sub = values.Where(item => item.Kind.IsField());
                        completionSets.Add(new CompletionSet("Fields", "Fields", applicableTo, sub, Enumerable.Empty<Completion>()));
                    }
                    if (compList.HasProperties)
                    {
                        var sub = values.Where(item => item.Kind == Kind.Property);
                        completionSets.Add(new CompletionSet("Properties", "Properties", applicableTo, sub, Enumerable.Empty<Completion>()));
                    }
                    if (compList.HasEvents)
                    {
                        var sub = compList.Values.Where(item => item.Kind == Kind.Event);
                        completionSets.Add(new CompletionSet("Events", "Events", applicableTo, sub, Enumerable.Empty<Completion>()));
                    }
                    if (compList.HasTypes)
                    {
                        var sub = values.Where(item => item.Kind.IsType());
                        completionSets.Add(new CompletionSet("Types", "Types", applicableTo, sub, Enumerable.Empty<Completion>()));
                    }
                    if (compList.HasNamespaces)
                    {
                        var sub = values.Where(item => item.Kind == Kind.Namespace);
                        completionSets.Add(new CompletionSet("Namespaces", "Namespaces", applicableTo, sub, Enumerable.Empty<Completion>()));
                    }
                }
                // Keywords are ALWAYS in a separate Tab anyway
                if (kwdList.Count > 0)
                {
                    completionSets.Add(new CompletionSet("Keywords", "Keywords", applicableTo, kwdList.Values, Enumerable.Empty<Completion>()));
                }
            }
            catch (Exception ex)
            {
                XSharpProjectPackage.Instance.DisplayOutPutMessage("AugmentCompletionSessions failed: " );
                XSharpProjectPackage.Instance.DisplayException(ex);
            }
            finally
            {
                XSharpModel.ModelWalker.Resume();
            }
            XSharpProjectPackage.Instance.DisplayOutPutMessage("<<-- AugmentCompletionSessions");
        }

        private void AddUsingStaticMembers(CompletionList compList, XFile file, string filterText)
        {
            //
            foreach (string staticType in file.AllUsingStatics)
            {
                BuildCompletionList(compList, new CompletionType(staticType, file, ""), Modifiers.Public, true, filterText);
            }
            // And what about Global Types in referenced Assemblies ?
            foreach (AssemblyInfo asm in file.Project.AssemblyReferences)
            {
                List<AssemblyInfo> oneAssembly = new List<AssemblyInfo>();
                oneAssembly.Add(asm);
                Type globalType = SystemTypeController.Lookup("Functions", oneAssembly);
                //
                if (globalType != null)
                {
                    BuildCompletionList(compList, new CompletionType(globalType), Modifiers.Public, true, filterText);
                }
                //
            }
            //
        }

        private void AddTypeNames(CompletionList compList, XProject project, string startWith, HashSet<String> usings)
        {
            AddTypeNames(compList, project, startWith);
            foreach (String nspace in usings)
            {
                AddTypeNames(compList, project, nspace + "." + startWith);
            }
        }

        private void AddTypeNames(CompletionList compList, XProject project, string startWith)
        {
            // We are looking for NameSpaces, in References
            int startLen = 0;
            int dotPos = startWith.LastIndexOf('.');
            if (dotPos != -1)
                startLen = dotPos + 1;
            //
            // Resolve projects. This adds them also to the AssemblyReferences
            //
            var sprjs = project.StrangerProjects;
            var prjs = project.ReferencedProjects;

            // Check of MsCorlib is included
            var references = project.AssemblyReferences.ToList();
            bool hasCorLib = false; ;
            foreach (var reference in references)
            {
                if (reference?.FileName != null && reference.FileName.EndsWith("mscorlib.dll", StringComparison.OrdinalIgnoreCase))
                {
                    hasCorLib = true;
                    break;
                }
            }
            if (!hasCorLib)
            {
                references.Add(SystemTypeController.MsCorLib);
            }
            foreach (AssemblyInfo assemblyInfo in references)
            {
                foreach (KeyValuePair<string, System.Type> typeInfo in assemblyInfo.Types.Where(ti => nameStartsWith(ti.Key, startWith)))
                {
                    if (XSharpTokenTools.isGenerated(typeInfo.Value))
                        continue;
                    TypeAnalysis typeAnalysis = new TypeAnalysis(typeInfo.Value.GetTypeInfo());
                    String realTypeName = typeAnalysis.Name;
                    // Nested Type ?
                    if (realTypeName.Contains("+"))
                    {
                        realTypeName = realTypeName.Replace('+', '.');
                    }
                    // remove the start
                    if (startLen > 0)
                        realTypeName = realTypeName.Substring(startLen);
                    // Do we have another part file
                    dotPos = realTypeName.IndexOf('.');
                    // Then remove it
                    if (dotPos > 0)
                        realTypeName = realTypeName.Substring(0, dotPos);
                    if (IsHiddenName(realTypeName))
                        continue;

                    //
                    ImageSource icon = _provider.GlyphService.GetGlyph(typeAnalysis.GlyphGroup, typeAnalysis.GlyphItem);
                    if (!compList.Add(new XSCompletion(realTypeName, realTypeName, typeAnalysis.Description, icon, null, Kind.Class)))
                        break;
                }
            }
            //
            // And our own Types
            AddXSharpTypeNames(compList, project, startWith);
            //// We should also add the external TypeNames
            //var prjs = project.ReferencedProjects;
            //foreach (var prj in prjs)
            //{
            //    AddXSharpTypeNames(compList, prj, startWith);
            //}
            //// And Stranger Projects
            //var sprjs = project.StrangerProjects;
            //foreach (var prj in sprjs)
            //{
            //    AddStrangerTypeNames(compList, prj, startWith);
            //}
        }

        private bool IsHiddenName(string realTypeName)
        {
            if (realTypeName.Length > 2 && realTypeName.StartsWith("__"))
                return true;
            if (realTypeName.Length > 4)
            {
                // event add
                if (realTypeName.StartsWith("add_"))
                    return true;
                // property get
                if (realTypeName.StartsWith("get_"))
                    return true;
                // property set
                if (realTypeName.StartsWith("set_"))
                    return true;
            }
            // event remove
            if (realTypeName.Length > 7 && realTypeName.StartsWith("remove_"))
                return true;
            return false;
        }

        private void AddXSharpTypeNames(CompletionList compList, XProject project, string startWith)
        {
            //
            int startLen = 0;
            int dotPos = startWith.LastIndexOf('.');
            if (dotPos != -1)
                startLen = dotPos + 1;
            //
            foreach (XFile file in project.SourceFiles)
            {
                if (file.TypeList != null)
                {
                    foreach (XType typeInfo in file.TypeList.Values.Where(ti => nameStartsWith(ti.FullName, startWith)))
                    {
                        String realTypeName = typeInfo.FullName;
                        // remove the start
                        if (startLen > 0)
                            realTypeName = realTypeName.Substring(startLen);
                        // Do we have another part
                        dotPos = realTypeName.IndexOf('.');
                        // Then remove it
                        if (dotPos > 0)
                            realTypeName = realTypeName.Substring(0, dotPos);
                        ImageSource icon = _provider.GlyphService.GetGlyph(typeInfo.GlyphGroup, typeInfo.GlyphItem);
                        if (!compList.Add(new XSCompletion(realTypeName, realTypeName, typeInfo.Description, icon, null, Kind.Class)))
                            break;
                    }
                }
            }
        }

        private void AddXSharpTypesTypeNames(CompletionList compList, string startWith)
        {
            //
            int startLen = 0;
            int dotPos = startWith.LastIndexOf('.');
            if (dotPos != -1)
                startLen = dotPos + 1;
            //
            // And our own Types
            var xsharpTypes = XSharpTypes.Get();
            foreach (XType typeInfo in xsharpTypes.Where(ti => nameStartsWith(ti.FullName, startWith)))
            {
                String realTypeName = typeInfo.FullName;
                // remove the start
                if (startLen > 0)
                    realTypeName = realTypeName.Substring(startLen);
                // Do we have another part
                dotPos = realTypeName.IndexOf('.');
                // Then remove it
                if (dotPos > 0)
                    realTypeName = realTypeName.Substring(0, dotPos);
                ImageSource icon = _provider.GlyphService.GetGlyph(typeInfo.GlyphGroup, typeInfo.GlyphItem);
                if (!compList.Add(new XSCompletion(realTypeName, realTypeName, typeInfo.Description, icon, null, Kind.Class)))
                    break;
            }
        }


        private void AddNamespaces(CompletionList compList, XProject project, String startWith)
        {
            // We are looking for NameSpaces, in References
            var namespaces = project.GetAssemblyNamespaces();
            // Calculate the length we must remove
            int startLen = 0;
            int dotPos = startWith.LastIndexOf('.');
            if (dotPos != -1)
                startLen = dotPos + 1;
            XType fakeNS = new XType("fake", Kind.Namespace, Modifiers.None, Modifiers.Public, TextRange.Empty, TextInterval.Empty);
            ImageSource icon = _provider.GlyphService.GetGlyph(fakeNS.GlyphGroup, fakeNS.GlyphItem);
            foreach (String nameSpace in namespaces.Where(ns => nameStartsWith(ns, startWith)))
            {
                String realNamespace = nameSpace;
                // remove the start
                if (startLen > 0)
                    realNamespace = realNamespace.Substring(startLen);
                // Do we have another part
                dotPos = realNamespace.IndexOf('.');
                // Then remove it
                if (dotPos > 0)
                    realNamespace = realNamespace.Substring(0, dotPos);
                //
                if (!compList.Add(new XSCompletion(realNamespace, realNamespace, "Namespace " + nameSpace, icon, null, Kind.Namespace)))
                    break;
            }
            //
            // And our own Namespaces
            AddXSharpNamespaces(compList, project, startWith, icon);
            // We should also add the external NameSpaces
            var prjs = project.ReferencedProjects;
            foreach (var prj in prjs)
            {
                AddXSharpNamespaces(compList, prj, startWith, icon);
            }
            // And Stranger Projects
            //var sprjs = project.StrangerProjects;
            //foreach (var prj in sprjs)
            //{
            //    AddStrangerNamespaces(compList, prj, startWith, icon);
            //}
        }

        private void AddXSharpNamespaces(CompletionList compList, XProject project, String startWith, ImageSource icon)
        {
            // Calculate the length we must remove
            int startLen = 0;
            int dotPos = startWith.LastIndexOf('.');
            if (dotPos != -1)
                startLen = dotPos + 1;
            // And our own Namespaces
            var xsNamespaces = project.Namespaces;
            foreach (XType nameSpace in xsNamespaces.Where(ns => nameStartsWith(ns.Name, startWith)))
            {
                String realNamespace = nameSpace.Name;
                // remove the start
                if (startLen > 0)
                    realNamespace = realNamespace.Substring(startLen);
                // Do we have another part
                dotPos = realNamespace.IndexOf('.');
                // Then remove it
                if (dotPos > 0)
                    realNamespace = realNamespace.Substring(0, dotPos);
                if (!compList.Add(new XSCompletion(realNamespace, realNamespace, nameSpace.Description, icon, null, Kind.Namespace)))
                    break;
            }
        }

        private void BuildCompletionList(CompletionList compList, XTypeMember currentMember, String startWith, int currentLine)
        {
            if (currentMember == null)
            {
                return;
            }
            // First, look after Parameters
            foreach (XVariable paramVar in currentMember.Parameters.Where(p => nameStartsWith(p.Name, startWith)))
            {
                //
                ImageSource icon = _provider.GlyphService.GetGlyph(paramVar.GlyphGroup, paramVar.GlyphItem);
                if (!compList.Add(new XSCompletion(paramVar.Name, paramVar.Name, paramVar.Description, icon, null, Kind.Parameter)))
                    break;
            }
            // Then, look for Locals
            foreach (XVariable localVar in currentMember.GetLocals(_buffer.CurrentSnapshot, currentLine).Where(l => nameStartsWith(l.Name, startWith)))
            {
                //
                ImageSource icon = _provider.GlyphService.GetGlyph(localVar.GlyphGroup, localVar.GlyphItem);
                if (!compList.Add(new XSCompletion(localVar.Name, localVar.Name, localVar.Description, icon, null, Kind.Local)))
                    break;
            }
            // Ok, now look for Members of the Owner of the member... So, the class a Method
            //

            if (currentMember.Kind.IsClassMember())
            {
                var classElement = currentMember.Parent;
                foreach (var member in classElement.Members.Where(m => m.Kind == Kind.Field && nameStartsWith(m.Name, startWith)))
                {
                    ImageSource icon = _provider.GlyphService.GetGlyph(member.GlyphGroup, member.GlyphItem);
                    if (!compList.Add(new XSCompletion(member.Name, member.Name, member.Description, icon, null, Kind.Field)))
                        break;
                }
            }
        }

        /// <summary>
        /// Fill the CompletionList by enumerating the members of the Parent
        /// </summary>
        /// <param name="compList"></param>
        /// <param name="parent">The XType element</param>
        /// <param name="minVisibility">The minimum Visibility</param>
        /// <param name="staticOnly">Static member only ?</param>
        /// <param name="startWith">The filter text</param>
        private void BuildCompletionList(CompletionList compList, XElement parent, Modifiers minVisibility, bool staticOnly, String startWith)
        {
            if (parent == null)
            {
                return;
            }
            if (!(parent is XType))
            {
                return;
            }
            //
            XType Owner = parent as XType;
            //
            foreach (XTypeMember elt in Owner.Members.Where(e => nameStartsWith(e.Name, startWith)))
            {
                if (elt.Kind == Kind.Constructor)
                    continue;
                if (elt.IsStatic != staticOnly)
                    continue;
                if (elt.Visibility < minVisibility)
                    continue;
                //
                ImageSource icon = _provider.GlyphService.GetGlyph(elt.GlyphGroup, elt.GlyphItem);
                String toAdd = "";
                if ((elt.Kind == Kind.Method) || (elt.Kind == Kind.Function) || (elt.Kind == Kind.Procedure))
                {
                    toAdd = "(";
                }
                if (!compList.Add(new XSCompletion(elt.Name, elt.Name + toAdd, elt.Description, icon, null, elt.Kind)))
                    break;
            }
            // Hummm, we should call for Owner of the Owner.. Super !
            Owner.ForceComplete();
            if (Owner.Parent != null)
            {
                BuildCompletionList(compList, parent, Modifiers.Protected, staticOnly, startWith);
            }
        }

        private void BuildCompletionList(CompletionList compList, CompletionType cType, Modifiers minVisibility, bool staticOnly, string startWith)
        {
            if (cType == null)
            {
                return;
            }
            //
            if (cType.XType != null)
            {
                FillMembers(compList, cType.XType, minVisibility, staticOnly, startWith);
                // Hummm, we should call for Owner of the Owner.. Super !
                cType.XType.ForceComplete();
                if (cType.XType.Parent != null)
                {
                    // Parent is a XElement, so one of our Types
                    BuildCompletionList(compList, new CompletionType((XType)cType.XType.Parent), Modifiers.Protected, staticOnly, startWith);
                }
                else if (cType.XType.ParentName != null)
                {
                    // Parent has just a Name, so one of the System Types
                    BuildCompletionList(compList, new CompletionType(cType.XType.ParentName, _file, cType.XType.FileUsings), Modifiers.Protected, staticOnly, startWith);
                }
            }
            else if (cType.SType != null)
            {
                // Now add Members for System types
                FillMembers(compList, cType.SType, minVisibility, staticOnly, startWith);
            }
        }


        private bool nameStartsWith(string name, string startWith)
        {
            return name.StartsWith(startWith, this._settingIgnoreCase, System.Globalization.CultureInfo.InvariantCulture);
        }

        /// <summary>
        /// Add Members for our Project Types
        /// </summary>
        /// <param name="compList"></param>
        /// <param name="xType"></param>
        /// <param name="minVisibility"></param>
        private void FillMembers(CompletionList compList, XType xType, Modifiers minVisibility, bool staticOnly, String startWith)
        {
            // Add Members for our Project Types
            foreach (XTypeMember elt in xType.Members.Where(x => nameStartsWith(x.Name, startWith)))
            {
                bool add = true;
                switch (elt.Kind)
                {
                    case Kind.EnumMember:
                        add = true;
                        break;
                    case Kind.Constructor:
                        add = false;
                        break;
                    default:
                        if (elt.IsStatic != staticOnly)
                            add = false;
                        if (elt.Visibility < minVisibility)
                            add = false;
                        if (IsHiddenName(elt.Name))
                            add = false;
                        break;
                }
                if (!add)
                    continue;
                //
                ImageSource icon = _provider.GlyphService.GetGlyph(elt.GlyphGroup, elt.GlyphItem);
                String toAdd = "";
                if ((elt.Kind == Kind.Method) || (elt.Kind == Kind.Function) || (elt.Kind == Kind.Procedure))
                {
                    toAdd = "(";
                }
                if (!compList.Add(new XSCompletion(elt.Name, elt.Name + toAdd, elt.Description, icon, null, elt.Kind)))
                    break;
            }
        }

        /// <summary>
        /// Add Members from System Types
        /// </summary>
        /// <param name="compList"></param>
        /// <param name="sType"></param>
        /// <param name="minVisibility"></param>
        private void FillMembers(CompletionList compList, System.Type sType, Modifiers minVisibility, bool staticOnly, String startWith)
        {
            MemberInfo[] members;
            //
            if (minVisibility < Modifiers.Public)
            {
                // Get Public, Internal, Protected & Private Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                members = sType.GetMembers(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance
                    | BindingFlags.Static | BindingFlags.DeclaredOnly);
            }
            else
            {
                //  Get Public Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                members = sType.GetMembers();
            }
            //
            foreach (var member in members.Where(x => nameStartsWith(x.Name, startWith)))
            {
                if (XSharpTokenTools.isGenerated(member))
                    continue;
                try
                {
                    MemberAnalysis analysis = new MemberAnalysis(member);
                    if (member is MethodInfo)
                    {
                        var mi = member as MethodInfo;
                        if (mi.IsSpecialName)
                            continue;

                    }
                    if ((analysis.IsInitialized) && (minVisibility <= analysis.Visibility))
                    {
                        if (analysis.Kind == Kind.Constructor)
                            continue;
                        if (analysis.IsStatic != staticOnly)
                        {
                            continue;
                        }
                        if (IsHiddenName(analysis.Name))
                            continue;
                        String toAdd = "";
                        if ((analysis.Kind == Kind.Method))
                        {
                            toAdd = "(";
                        }
                        //
                        StandardGlyphItem imgI = analysis.GlyphItem;
                        if (analysis.IsStatic)
                        {
                            imgI = StandardGlyphItem.GlyphItemShortcut;
                        }
                        //
                        ImageSource icon = _provider.GlyphService.GetGlyph(analysis.GlyphGroup, imgI);
                        if (!compList.Add(new XSCompletion(analysis.Name, analysis.Name + toAdd, analysis.Description, icon, null, analysis.Kind)))
                            break;
                    }
                }
                catch (Exception e)
                {
                    XSharpProjectPackage.Instance.DisplayOutPutMessage("FillMembers failed: " );
                    XSharpProjectPackage.Instance.DisplayException(e);
                }
            }
            // fill members of parent class
            if (sType.BaseType != null)
            {
                if (minVisibility == Modifiers.Private)
                    minVisibility = Modifiers.Protected;
                FillMembers(compList, sType.BaseType, minVisibility, staticOnly, startWith);
            }

            foreach (var _interf in sType.GetInterfaces())
            {
                FillMembers(compList, _interf, minVisibility, staticOnly, startWith);
            }

            // We will miss the System.Object members
            if (sType.IsInterface)
            {
                System.Type obj = typeof(object);
                FillMembers(compList, obj, minVisibility, staticOnly, startWith);
            }
            if (sType.IsEnum)
            {
                var scope = StandardGlyphItem.GlyphItemPublic;
                // must be public or nested, otherwise we would not see it.
                if (sType.IsNested)
                {
                    scope = StandardGlyphItem.GlyphItemFriend;
                }
                ImageSource icon = _provider.GlyphService.GetGlyph(StandardGlyphGroup.GlyphGroupEnumMember, scope);
                // Fill enum members
                foreach (string name in sType.GetEnumNames())
                {
                    if (!compList.Add(new XSCompletion(name, name, "", icon, null, Kind.EnumMember)))
                        break;
                }

            }
        }



        public void Dispose()
        {
            if (!_disposed)
            {
                // Was missing, but doesn't solved the Deadlock with Intellisense
                GC.SuppressFinalize(this);
                _disposed = true;
            }
        }

        /// <summary>
        /// Flatten the TokenList as a String
        /// </summary>
        /// <param name="tokenList"></param>
        /// <returns></returns>
        private String TokenListAsString(List<String> tokenList, int less)
        {
            String retValue = "";
            for (int pos = 0; pos < tokenList.Count - less; pos++)
            {
                String tk = tokenList[pos];
                retValue += tk;
            }
            return retValue;
        }
    }
    internal class MemberAnalysis
    {
        class ParamInfo
        {
            public String Name;
            public String TypeName;
            public String Direction;
            public bool Optional;

            internal ParamInfo(String n, String t, String dir)
            {
                this.Name = n;
                this.TypeName = t;
                this.Direction = dir;
            }
            internal ParamInfo(System.Reflection.ParameterInfo p)
            {
                this.Name = p.Name;
                if (p.IsIn)
                {
                    Direction = "IN";
                }
                else if (p.IsOut)
                {
                    Direction = "OUT";
                }
                else
                    Direction = "AS";
                string type = p.ParameterType.FullName;
                if (type == null)
                    type = p.ParameterType.Name;
                if (type != null && type.EndsWith("&"))
                    Direction = "REF";

                this.TypeName = p.ParameterType.GetXSharpTypeName();
                Optional = p.IsOptional;
            }
        }


        private String _name;
        private Modifiers _modifiers;
        private Modifiers _visibility;
        private Kind _kind;
        private bool _isStatic;
        private String _typeName;
        private List<ParamInfo> _parameters;

        /// <summary>
        /// Process a MemberInfo in order to provide usable informations ( TypeName, Glyph, ... )
        /// </summary>
        internal MemberAnalysis(MemberInfo member)
        {
            Type declType;
            //
            this._name = member.Name;
            this._kind = Kind.Class;
            this._modifiers = Modifiers.None;
            this._visibility = Modifiers.Public;
            this._typeName = "";
            this._parameters = new List<ParamInfo>();
            //
            AppDomain.CurrentDomain.AssemblyResolve += CurrentDomain_AssemblyResolve;
            switch (member.MemberType)
            {
                case MemberTypes.Constructor:
                    this._name = "Constructor";
                    this._kind = Kind.Constructor;
                    ConstructorInfo constInfo = member as ConstructorInfo;
                    this._isStatic = constInfo.IsStatic;
                    //
                    if (constInfo.IsAbstract)
                    {
                        this._modifiers = Modifiers.Abstract;
                    }
                    //
                    if (constInfo.IsPrivate)
                    {
                        this._visibility = Modifiers.Private;
                    }
                    else if (constInfo.IsAssembly)
                    {
                        this._visibility = Modifiers.Internal;
                    }
                    else if (constInfo.IsFamily)
                    {
                        this._visibility = Modifiers.Protected;
                    }
                    //
                    //
                    addParameters(constInfo, constInfo.GetParameters());
                    //
                    declType = constInfo.DeclaringType;
                    this._typeName = declType.GetXSharpTypeName();
                    break;
                case MemberTypes.Event:
                    this._kind = Kind.Event;
                    EventInfo evt = member as EventInfo;
                    MethodInfo methodInfo = evt.GetAddMethod(true);
                    if (methodInfo == null)
                    {
                        methodInfo = evt.GetRemoveMethod(true);
                    }
                    //
                    this._isStatic = methodInfo.IsStatic;
                    //
                    if (methodInfo.IsAbstract)
                    {
                        this._modifiers = Modifiers.Abstract;
                    }
                    //
                    if (methodInfo.IsPrivate)
                    {
                        this._visibility = Modifiers.Private;
                    }
                    else if (methodInfo.IsAssembly)
                    {
                        this._visibility = Modifiers.Internal;
                    }
                    else if (methodInfo.IsFamily)
                    {
                        this._visibility = Modifiers.Protected;
                    }
                    //
                    declType = evt.EventHandlerType;
                    this._typeName = declType.GetXSharpTypeName();
                    break;
                case MemberTypes.Field:
                    this._kind = Kind.Field;
                    if (member.DeclaringType.IsEnum)
                        this._kind = Kind.EnumMember;
                    FieldInfo field = member as FieldInfo;
                    if (field.IsStatic)
                    {
                        if (field.DeclaringType.Name.ToLower() == "functions")
                        {
                            if (field.Attributes.HasFlag(FieldAttributes.InitOnly))
                                this.Kind = Kind.VODefine;
                            else if (field.Attributes.HasFlag(FieldAttributes.Literal))
                                this.Kind = Kind.VODefine;
                            else
                                this.Kind = Kind.VOGlobal;
                        }
                    }

                    //
                    this._isStatic = field.IsStatic;
                    //
                    if (field.IsPrivate)
                    {
                        this._visibility = Modifiers.Private;
                    }
                    else if (field.IsAssembly)
                    {
                        this._visibility = Modifiers.Internal;
                    }
                    else if (field.IsFamily)
                    {
                        this._visibility = Modifiers.Protected;
                    }
                    //
                    declType = field.FieldType;
                    this._typeName = declType.GetXSharpTypeName();
                    break;
                case MemberTypes.Method:
                    this.Kind = Kind.Method;
                    MethodInfo method = member as MethodInfo;
                    if (method.IsStatic)
                    {
                        if (method.DeclaringType.Name.ToLower() == "functions")
                        {
                            if (method.ReturnType.FullName != "System.Void")
                                this.Kind = Kind.Function;
                            else
                                this.Kind = Kind.Procedure;
                        }
                    }
                    if (method.IsSpecialName)
                    {
                        // The SpecialName bit is set to flag members that are treated in a special way by some compilers (such as property accessors and operator overloading methods).
                        this._name = null;
                        break;
                    }
                    //
                    this._isStatic = method.IsStatic;
                    //
                    if (method.IsPrivate)
                    {
                        this._visibility = Modifiers.Private;
                    }
                    else if (method.IsAssembly)
                    {
                        this._visibility = Modifiers.Internal;
                    }
                    else if (method.IsFamily)
                    {
                        this._visibility = Modifiers.Protected;
                    }
                    //
                    addParameters(method, method.GetParameters());
                    //
                    declType = method.ReturnType;
                    this._typeName = declType.GetXSharpTypeName();
                    break;
                case MemberTypes.Property:
                    this.Kind = Kind.Property;
                    PropertyInfo prop = member as PropertyInfo;
                    MethodInfo propInfo = prop.GetGetMethod(true);
                    if (propInfo == null)
                    {
                        propInfo = prop.GetSetMethod(true);
                    }
                    //
                    this._isStatic = propInfo.IsStatic;
                    //
                    if (propInfo.IsPrivate)
                    {
                        this._visibility = Modifiers.Private;
                    }
                    else if (propInfo.IsAssembly)
                    {
                        this._visibility = Modifiers.Internal;
                    }
                    else if (propInfo.IsFamily)
                    {
                        this._visibility = Modifiers.Protected;
                    }
                    //
                    addParameters(propInfo, propInfo.GetParameters());
                    declType = prop.PropertyType;
                    this._typeName = declType.GetXSharpTypeName();
                    break;
                // Todo: Event ?

                default:
                    // Mark as Not-Initialized
                    this._name = null;
                    break;
            }
            AppDomain.CurrentDomain.AssemblyResolve -= CurrentDomain_AssemblyResolve;
        }

        private Assembly CurrentDomain_AssemblyResolve(object sender, ResolveEventArgs args)
        {
            // return "OUR" copy of the assembly. Most likely we have it
            var name = args.Name;
            var request = args.RequestingAssembly;
            var asmLoc = SystemTypeController.FindAssemblyByName(name);
            return AssemblyInfo.LoadAssemblyFromFile(asmLoc);
        }

        private void addParameters(MemberInfo member, ParameterInfo[] parameters)
        {

            if (parameters.Length == 1)
            {
                try
                {
                    var atts = member.GetCustomAttributes(false);
                    if (atts != null)
                    {
                        foreach (var custattr in atts)
                        {
                            if (custattr.ToString().EndsWith("ClipperCallingConventionAttribute"))
                            {
                                string[] names = (string[])custattr.GetType().GetProperty("ParameterNames").GetValue(custattr, null);
                                if (names.Length == 0)
                                {
                                    ; // no parameters defined
                                      //this._parameters.Add(new ParamInfo("[params", "USUAL]", "AS"));
                                }
                                else
                                {
                                    foreach (var param in names)
                                    {
                                        this._parameters.Add(new ParamInfo(param, "USUAL", "AS"));
                                    }
                                }
                                return;
                            }
                        }
                    }
                }
                catch (Exception e)
                {
                    XSharpProjectPackage.Instance.DisplayOutPutMessage("Error reading Clipper params " );
                    XSharpProjectPackage.Instance.DisplayException(e);
                }

            }

            foreach (ParameterInfo p in parameters)
            {
                this._parameters.Add(new ParamInfo(p));
            }

        }

        internal MemberAnalysis(EnvDTE.CodeElement member)
        {
            EnvDTE.CodeTypeRef declType;
            //
            this._name = member.Name;
            this._kind = Kind.Class;
            this._modifiers = Modifiers.None;
            this._visibility = Modifiers.Public;
            this._typeName = "";
            this._parameters = new List<ParamInfo>();
            //
            switch (member.Kind)
            {
                case EnvDTE.vsCMElement.vsCMElementEvent:
                    this._kind = Kind.Event;
                    EnvDTE80.CodeEvent evt = member as EnvDTE80.CodeEvent;
                    //
                    this._isStatic = evt.IsShared;
                    //
                    if (evt.Access == EnvDTE.vsCMAccess.vsCMAccessPrivate)
                    {
                        this._visibility = Modifiers.Private;
                    }
                    else if (evt.Access == EnvDTE.vsCMAccess.vsCMAccessAssemblyOrFamily)
                    {
                        this._visibility = Modifiers.Internal;
                    }
                    else if (evt.Access == EnvDTE.vsCMAccess.vsCMAccessProtected)
                    {
                        this._visibility = Modifiers.Protected;
                    }
                    //
                    declType = evt.Type;
                    this._typeName = declType.AsFullName;
                    break;
                case EnvDTE.vsCMElement.vsCMElementVariable:
                    this._kind = Kind.Field;
                    EnvDTE.CodeVariable field = member as EnvDTE.CodeVariable;
                    //
                    this._isStatic = field.IsShared;
                    //
                    if (field.Access == EnvDTE.vsCMAccess.vsCMAccessPrivate)
                    {
                        this._visibility = Modifiers.Private;
                    }
                    else if (field.Access == EnvDTE.vsCMAccess.vsCMAccessAssemblyOrFamily)
                    {
                        this._visibility = Modifiers.Internal;
                    }
                    else if (field.Access == EnvDTE.vsCMAccess.vsCMAccessProtected)
                    {
                        this._visibility = Modifiers.Protected;
                    }
                    //
                    declType = field.Type;
                    this._typeName = declType.AsFullName;
                    break;
                case EnvDTE.vsCMElement.vsCMElementFunction:
                    this.Kind = Kind.Method;
                    EnvDTE.CodeFunction method = member as EnvDTE.CodeFunction;
                    //
                    if (method.FunctionKind == EnvDTE.vsCMFunction.vsCMFunctionConstructor)
                    {
                        this.Kind = Kind.Constructor;
                    }
                    //
                    this._isStatic = method.IsShared;
                    //
                    if (method.Access == EnvDTE.vsCMAccess.vsCMAccessPrivate)
                    {
                        this._visibility = Modifiers.Private;
                    }
                    else if (method.Access == EnvDTE.vsCMAccess.vsCMAccessAssemblyOrFamily)
                    {
                        this._visibility = Modifiers.Internal;
                    }
                    else if (method.Access == EnvDTE.vsCMAccess.vsCMAccessProtected)
                    {
                        this._visibility = Modifiers.Protected;
                    }
                    //
                    EnvDTE.CodeElements pars = method.Parameters;
                    foreach (EnvDTE.CodeParameter p in pars)
                    {
                        this._parameters.Add(new ParamInfo(p.Name, p.Type.AsFullName, "AS"));
                    }
                    //
                    declType = method.Type;
                    this._typeName = declType.AsFullName;
                    break;
                case EnvDTE.vsCMElement.vsCMElementProperty:
                    this.Kind = Kind.Property;
                    EnvDTE.CodeProperty prop = member as EnvDTE.CodeProperty;
                    //
                    if (prop.Access == EnvDTE.vsCMAccess.vsCMAccessPrivate)
                    {
                        this._visibility = Modifiers.Private;
                    }
                    else if (prop.Access == EnvDTE.vsCMAccess.vsCMAccessAssemblyOrFamily)
                    {
                        this._visibility = Modifiers.Internal;
                    }
                    else if (prop.Access == EnvDTE.vsCMAccess.vsCMAccessProtected)
                    {
                        this._visibility = Modifiers.Protected;
                    }
                    //
                    declType = prop.Type;
                    this._typeName = declType.AsFullName;
                    break;
                default:
                    // Mark as Not-Initialized
                    this._name = null;
                    break;
            }
        }

        public bool IsInitialized
        {
            get
            {
                return (this.Name != null);
            }
        }

        public string Name
        {
            get
            {
                return _name;
            }
        }

        public String Description
        {
            get
            {
                String modVis = "";
                if (this.Modifiers != Modifiers.None)
                {
                    modVis += this.Modifiers.ToString() + " ";
                }
                modVis += this.Visibility.ToString() + " ";
                //
                String desc = modVis;
                //
                if ((this.Kind != Kind.Field) && (this.Kind != Kind.Constructor))
                {
                    if (this.Kind == Kind.VODefine)
                    {
                        desc += "Define" + " ";
                    }
                    else if (this.Kind == Kind.VOGlobal)
                    {
                        desc += "Global" + " ";
                    }
                    else
                    {
                        desc += this.Kind.ToString() + " ";
                    }
                }
                desc += this.Prototype;
                //
                return desc;
            }
        }

        public String Prototype
        {
            get
            {
                string vars = "";
                if (this.Kind.HasParameters())
                {
                    vars = "(";
                    foreach (var var in this.Parameters)
                    {
                        if (vars.Length > 1)
                            vars += ", ";
                        vars += var.Name + " " + var.Direction + " " + var.TypeName;
                    }
                    vars += ")";
                }
                //
                string desc = this.Name;
                desc += vars;
                //
                if (this.Kind.HasReturnType())
                {
                    desc += " AS " + this.TypeName;
                }
                //
                return desc;
            }
        }

        public StandardGlyphGroup GlyphGroup
        {
            get
            {
                StandardGlyphGroup imgG;
                //
                switch (this.Kind)
                {
                    case Kind.Namespace:
                        imgG = StandardGlyphGroup.GlyphGroupNamespace;
                        break;
                    case Kind.Constructor:
                    case Kind.Destructor:
                    case Kind.Method:
                    case Kind.Function:
                    case Kind.Procedure:
                        imgG = StandardGlyphGroup.GlyphGroupMethod;
                        break;
                    case Kind.Structure:
                    case Kind.Union:
                        imgG = StandardGlyphGroup.GlyphGroupStruct;
                        break;
                    case Kind.Access:
                    case Kind.Assign:
                    case Kind.Property:
                        imgG = StandardGlyphGroup.GlyphGroupProperty;
                        break;
                    case Kind.Local:
                        imgG = StandardGlyphGroup.GlyphGroupVariable;
                        break;
                    case Kind.Enum:
                        imgG = StandardGlyphGroup.GlyphGroupEnumMember;
                        break;
                    case Kind.VOGlobal:
                    case Kind.Field:
                        imgG = StandardGlyphGroup.GlyphGroupField;
                        break;
                    case Kind.Delegate:
                        imgG = StandardGlyphGroup.GlyphGroupDelegate;
                        break;
                    case Kind.Event:
                        imgG = StandardGlyphGroup.GlyphGroupEvent;
                        break;
                    case Kind.Interface:
                        imgG = StandardGlyphGroup.GlyphGroupInterface;
                        break;
                    case Kind.VODefine:
                        imgG = StandardGlyphGroup.GlyphGroupConstant;
                        break;
                    case Kind.Class:
                    default:
                        imgG = StandardGlyphGroup.GlyphGroupClass;
                        break;
                }
                return imgG;
            }
        }

        /// <summary>
        /// Glyph Item used by CompletionList in CompletionSource
        /// - See also GlyphGroup
        ///  http://glyphlist.azurewebsites.net/standardglyphgroup/
        /// </summary>
        public StandardGlyphItem GlyphItem
        {
            get
            {
                StandardGlyphItem imgI;
                //
                switch (this.Visibility)
                {
                    case Modifiers.Protected:
                        imgI = StandardGlyphItem.GlyphItemProtected;
                        break;
                    case Modifiers.Private:
                        imgI = StandardGlyphItem.GlyphItemPrivate;
                        break;
                    case Modifiers.Internal:
                        imgI = StandardGlyphItem.GlyphItemInternal;
                        break;
                    case Modifiers.ProtectedInternal:
                        imgI = StandardGlyphItem.GlyphItemFriend;
                        break;
                    case Modifiers.Public:
                    default:
                        imgI = StandardGlyphItem.GlyphItemPublic;
                        break;
                }
                //
                return imgI;
            }
        }

        public Kind Kind
        {
            get
            {
                return _kind;
            }

            set
            {
                _kind = value;
            }
        }

        public Modifiers Modifiers
        {
            get
            {
                return _modifiers;
            }
        }

        public Modifiers Visibility
        {
            get
            {
                return _visibility;
            }
        }

        public string TypeName
        {
            get
            {
                return _typeName;
            }
        }

        private List<ParamInfo> Parameters
        {
            get
            {
                return _parameters;
            }

            set
            {
                _parameters = value;
            }
        }

        public bool IsStatic
        {
            get
            {
                return _isStatic;
            }

            set
            {
                _isStatic = value;
            }
        }
    }

    /// <summary>
    /// Process a TypeInfo in order to provide usable informations (TypeName, Glyph, ... )
    /// </summary>
    internal class TypeAnalysis
    {
        private String _name;
        private Modifiers _modifiers;
        private Modifiers _visibility;
        private Kind _kind;
        private bool _isStatic;


        internal TypeAnalysis(TypeInfo typeInfo)
        {
            //
            this._name = typeInfo.FullName;
            this._kind = Kind.Class;
            this._modifiers = Modifiers.None;
            this._visibility = Modifiers.Public;
            //
            if (typeInfo.IsClass)
            {
                this._kind = Kind.Class;
                if (typeInfo.IsSubclassOf(typeof(Delegate)))
                {
                    this._kind = Kind.Delegate;
                }
            }
            else if (typeInfo.IsEnum)
            {
                this._kind = Kind.Enum;
            }
            else if (typeInfo.IsInterface)
            {
                this._kind = Kind.Interface;
            }
            else if (typeInfo.IsValueType && !typeInfo.IsPrimitive)
            {
                this._kind = Kind.Structure;
            }
            //
            this._isStatic = (typeInfo.IsAbstract && typeInfo.IsSealed);
            //
            if (!this.IsStatic)
            {
                if (typeInfo.IsAbstract)
                {
                    this._modifiers = Modifiers.Abstract;
                }
            }
            //
            if (typeInfo.IsGenericType)
            {
                string genName = typeInfo.FullName;
                int index = genName.IndexOf('`');
                if (index != -1)
                {
                    genName = genName.Substring(0, index);
                }
                genName += "<";
                int count = 0;
                int max = typeInfo.GenericTypeParameters.Length;
                foreach (Type genType in typeInfo.GenericTypeParameters)
                {
                    genName += genType.Name;
                    count++;
                    if ((count < max))
                        genName += ", ";
                }
                genName += ">";
                //
                this._name = genName;
            }
            ////
            //if (typeInfo.IsPrivate)
            //{
            //    this._visibility = Modifiers.Private;
            //}
            //if (typeInfo.IsAssembly)
            //{
            //    this._visibility = Modifiers.Internal;
            //}
            //if (typeInfo.IsFamily)
            //{
            //    this._visibility = Modifiers.Protected;
            //}
            //
        }

        internal TypeAnalysis(EnvDTE.CodeType typeInfo)
        {
            //
            this._name = typeInfo.Name;
            this._kind = Kind.Class;
            this._modifiers = Modifiers.None;
            this._visibility = Modifiers.Public;
            //
            if (typeInfo.Kind == EnvDTE.vsCMElement.vsCMElementClass)
            {
                this._kind = Kind.Class;
            }
            else if (typeInfo.Kind == EnvDTE.vsCMElement.vsCMElementEnum)
            {
                this._kind = Kind.Enum;
            }
            else if (typeInfo.Kind == EnvDTE.vsCMElement.vsCMElementInterface)
            {
                this._kind = Kind.Interface;
            }
            else if (typeInfo.Kind == EnvDTE.vsCMElement.vsCMElementStruct)
            {
                this._kind = Kind.Structure;
            }
            //
            /*
            this._isStatic = (typeInfo.IsAbstract && typeInfo.IsSealed);
            //
            if (!this.IsStatic)
            {
                if (typeInfo.IsAbstract)
                {
                    this._modifiers = Modifiers.Abstract;
                }
            }*/
        }

        public bool IsInitialized
        {
            get
            {
                return (this.Name != null);
            }
        }

        public string Name
        {
            get
            {
                return _name;
            }
        }

        public String Description
        {
            get
            {
                String modVis = "";
                if (this.Modifiers != Modifiers.None)
                {
                    modVis += this.Modifiers.ToString() + " ";
                }
                modVis += this.Visibility.ToString() + " ";
                //
                String desc = modVis;
                //
                if (this.Kind != Kind.Field)
                    desc += this.Kind.ToString() + " ";
                desc += this.Prototype;
                //
                return desc;
            }
        }

        public String Prototype
        {
            get
            {
                return this.Name;
                //
            }
        }

        public StandardGlyphGroup GlyphGroup
        {
            get
            {
                StandardGlyphGroup imgG;
                //
                switch (this.Kind)
                {
                    case Kind.Class:
                    default:
                        imgG = StandardGlyphGroup.GlyphGroupClass;
                        break;
                    case Kind.Interface:
                        imgG = StandardGlyphGroup.GlyphGroupInterface;
                        break;
                    case Kind.Enum:
                        imgG = StandardGlyphGroup.GlyphGroupEnum;
                        break;
                    case Kind.Delegate:
                        imgG = StandardGlyphGroup.GlyphGroupDelegate;
                        break;

                }
                return imgG;
            }
        }

        /// <summary>
        /// Glyph Item used by CompletionList in CompletionSource
        /// - See also GlyphGroup
        ///  http://glyphlist.azurewebsites.net/standardglyphgroup/
        /// </summary>
        public StandardGlyphItem GlyphItem
        {
            get
            {
                StandardGlyphItem imgI;
                //
                switch (this.Visibility)
                {
                    case Modifiers.Public:
                    default:
                        imgI = StandardGlyphItem.GlyphItemPublic;
                        break;
                    case Modifiers.Protected:
                        imgI = StandardGlyphItem.GlyphItemProtected;
                        break;
                    case Modifiers.Private:
                        imgI = StandardGlyphItem.GlyphItemPrivate;
                        break;
                    case Modifiers.Internal:
                        imgI = StandardGlyphItem.GlyphItemInternal;
                        break;
                    case Modifiers.ProtectedInternal:
                        imgI = StandardGlyphItem.GlyphItemFriend;
                        break;

                }
                //
                return imgI;
            }
        }

        public Kind Kind
        {
            get
            {
                return _kind;
            }

            set
            {
                _kind = value;
            }
        }

        public Modifiers Modifiers
        {
            get
            {
                return _modifiers;
            }
        }

        public Modifiers Visibility
        {
            get
            {
                return _visibility;
            }
        }

        public bool IsStatic
        {
            get
            {
                return _isStatic;
            }

            set
            {
                _isStatic = value;
            }
        }
    }
    /// <summary>
    /// XSharp CompletionList
    /// Overload the Add() Method to support "overloads"
    /// </summary>
    internal class CompletionList : SortedDictionary<string, XSCompletion>
    {
        internal bool HasMethods { get; private set; }
        internal bool HasProperties { get; private set; }
        internal bool HasFields { get; private set; }
        internal bool HasEvents { get; private set; }
        internal bool HasEnumMembers { get; private set; }
        internal bool HasTypes { get; private set; }
        internal bool HasNamespaces { get; private set; }
        internal CompletionList() : base(StringComparer.OrdinalIgnoreCase)
        {
        }
        public bool Add(XSCompletion item)
        {

            if (ContainsKey(item.DisplayText))
            {
                // only methods have overloads
                // we do not want to the overloads message for partial classes that appear in more than 1 file
                // and if a method in a subclass has the same params we also do not want to show that there are overloads
                var found = this[item.DisplayText];
                if (item.Kind == Kind.Method && !string.Equals(found.Description, item.Description, StringComparison.OrdinalIgnoreCase))
                {
                    int overloads = 0;
                    // Already exists in the List !!
                    // First Overload ?
                    var comp = this[item.DisplayText];
                    if (comp.Properties.ContainsProperty("overloads"))
                    {
                        // No ...
                        overloads = (int)comp.Properties.GetProperty("overloads");
                    }
                    else
                    {
                        comp.Properties.AddProperty("overloads", overloads);
                    }
                    overloads += 1;
                    // Set the number of Overload(s)
                    comp.Properties["overloads"] = overloads;
                    // Now, hack the Description text
                }
                // Ok, Forget about the newly added Completion please
                return true;

            }
            if (!String.IsNullOrEmpty(item.DisplayText))
            {
                switch (item.Kind)
                {
                    case Kind.Method:
                    case Kind.Function:
                    case Kind.Procedure:
                    case Kind.Operator:
                    case Kind.VODLL:
                        HasMethods = true;
                        break;
                    case Kind.Event:
                        HasEvents = true;
                        break;
                    case Kind.Property:
                    case Kind.Access:
                    case Kind.Assign:
                        HasProperties = true;
                        break;
                    case Kind.Namespace:
                        HasNamespaces = true;
                        break;
                    case Kind.EnumMember:
                        HasEnumMembers = true;
                        break;
                    case Kind.Constructor:
                        break;
                    case Kind.Destructor:
                        break;
                    case Kind.Local:
                        break;
                    case Kind.Parameter:
                        break;
                    case Kind.Delegate:
                        break;
                    case Kind.Using:
                        break;
                    case Kind.Keyword:
                        break;
                    default:
                        if (item.Kind.IsField())
                            HasFields = true;
                        else if (item.Kind.IsType())
                            HasTypes = true;
                        break;
                }
                base.Add(item.DisplayText, item);
            }
            return true;
        }
    }

    /// <summary>
    /// XSharp Completion class.
    /// Overload the Description property in order to add "overload" text at the end
    /// </summary>
    [DebuggerDisplay("{DisplayText,nq}")]
    public class XSCompletion : Completion
    {
        public XSharpModel.Kind Kind { get; set; }
        public XSCompletion(string displayText, string insertionText, string description, ImageSource iconSource, string iconAutomationText, XSharpModel.Kind kind)
            : base(displayText, insertionText, description, iconSource, iconAutomationText)
        {
            Kind = kind;
        }

        public override string Description
        {
            get
            {
                string desc;
                int overloads = 0;
                if (this.Properties.ContainsProperty("overloads"))
                {
                    // No ...
                    overloads = (int)this.Properties.GetProperty("overloads");
                }
                if (overloads > 0)
                {
                    desc = base.Description;
                    desc += " (+" + overloads + " overload";
                    if (overloads > 1)
                        desc += "s";
                    desc += ")";
                }
                else
                {
                    desc = base.Description;
                }
                //
                return desc;
            }
            set
            {
                base.Description = value;
            }
        }

    }


    /// <summary>
    /// Static class Tools. Offer services to get TokenList, Search members, ...
    /// </summary>
    public static class XSharpTokenTools
    {
        public static bool isGenerated(System.Type type)
        {
            var att = type.GetCustomAttribute(typeof(CompilerGeneratedAttribute));
            return att != null;
        }
        public static bool isGenerated(MemberInfo m)
        {
            var att = m.GetCustomAttribute(typeof(CompilerGeneratedAttribute));
            return att != null;
        }

        public static bool StringEquals(string lhs, string rhs)
        {
            return XSharpCompletionSource.StringEquals(lhs, rhs);
        }

        /// <summary>
        /// Retrieve a List of Token, based on a position in buffer.
        /// Moving back in the buffer, all tokens are retrieved and stored.
        /// </summary>
        /// <param name="triggerPointPosition">The position in the buffer </param>
        /// <param name="triggerPointLineNumber"></param>
        /// <param name="bufferText"></param>
        /// <param name="stopToken">The IToken that stops the move backwards</param>
        /// <param name="fromGotoDefn">Indicate if the call is due to Goto Definition</param>
        /// <param name="file">XFile object to use for the context</param>
        /// <param name="dotAsSelector">dot is used as 'standard' selector, like colon </param>
        /// <param name="fromMember">The Member containing the position</param>
        /// <returns></returns>
        public static List<String> GetTokenList(int triggerPointPosition, int triggerPointLineNumber,
            ITextSnapshot snapshot, out IToken stopToken, bool fromGotoDefn, XFile file, bool dotAsSelector, XTypeMember fromMember)
        {
            List<String> tokenList = new List<string>();
            String token;
            string fileName;
            var bufferText = snapshot.GetText();
            //
            stopToken = null;
            // lex the entire document
            // Get compiler options
            XSharpParseOptions parseoptions;
            if (file != null)
            {
                var prj = file.Project.ProjectNode;
                parseoptions = prj.ParseOptions;
                fileName = file.FullPath;
            }
            else
            {
                parseoptions = XSharpParseOptions.Default;
                fileName = "MissingFile.prg";
            }
            //////////////////////////////////////
            //////////////////////////////////////
            // Try to speedup the process, Tokenize only the Member source if possible (and not the FULL source text)
            if (fromMember != null && fromMember.Interval.Start < triggerPointPosition && fromMember.Kind.HasBody())
            {
                // if the triggerpoint is after the end of the member then the member information is old and
                // we should use that position to determine the end of the buffer to scan
                // And make sure we also add some more because that does not hurt.
                int nWidth;
                if (triggerPointPosition > fromMember.Interval.Stop)
                {
                    nWidth = triggerPointPosition - fromMember.Interval.Start + 500;
                }
                else
                {
                    nWidth = fromMember.Interval.Width + 500;
                }
                nWidth = Math.Min(nWidth, bufferText.Length - fromMember.Interval.Start);
                bufferText = bufferText.Substring(fromMember.Interval.Start, nWidth);
                // Adapt the positions.
                triggerPointPosition = triggerPointPosition - fromMember.Interval.Start;
                triggerPointLineNumber = triggerPointLineNumber - (fromMember.Range.StartLine - 1);
            }
            else
            {
                // no need to parse the whole buffer. It could be huge
                int maxLen = triggerPointPosition + 500;
                if (maxLen > bufferText.Length)
                    maxLen = bufferText.Length;
                bufferText = bufferText.Substring(0, maxLen);
                // no need to adjust the line and position since we are working from the start of the buffer
            }


            ITokenStream tokenStream;
            var reporter = new ErrorIgnorer();
            bool ok = XSharp.Parser.VsParser.Lex(bufferText, fileName, parseoptions, reporter, out tokenStream);
            var tokens = tokenStream as BufferedTokenStream;
            // locate the last token before the trigger point
            // Use binary search in stead of linear search
            var list = tokens.GetTokens();
            int current = 0;
            int bottom = 0;
            int top = list.Count;
            while (top - bottom > 1)
            {
                // determine middle
                current = (bottom + top) / 2;
                var check = list[current];
                if (check.StartIndex > triggerPointPosition)
                {
                    top = current;
                }
                else if (check.StartIndex < triggerPointPosition)
                {
                    bottom = current;
                }
                else
                {
                    break;
                }
            }
            if (current > list.Count - 1 || current < 0)
                return tokenList;
            // on the right row now. Find the first token on the row
            for (int iToken = current; iToken >= 0; iToken--)
            {
                // Out tokens line numbers are 1 based
                if (list[iToken].Line != triggerPointLineNumber + 1)
                    break;
                current = iToken;
            }
            // now look forward and find the first token that is on or after the triggerpoint
            IToken nextToken = list[current];
            while (true)
            {
                current++;
                if (list[current].StartIndex < triggerPointPosition)
                    nextToken = list[current];
                else
                    break;
            }
            // nextToken now points to the first token that has its startindex >= triggerPointPosition

            //////////////////////////////////////
            // We are looking at line
            // Out tokens line numbers are 1 based
            int lineTP = triggerPointLineNumber + 1;
            // And we are on line
            int lineNT = nextToken.Line;
            //
            if (lineNT != lineTP)
            {
                // should not happen.
                //return tokenList;
            }
            nextToken = list[((XSharpToken)nextToken).OriginalTokenIndex + 1];
            if (stopToken == null)
            {
                stopToken = nextToken;
            }
            // Now, let's build the Token chain, so we can guess what to add in the CompletionList
            IToken triggerToken = null;
            token = "";
            switch (nextToken.Type)
            {
                case XSharpLexer.LPAREN:
                    token = "()";
                    break;
                case XSharpLexer.LCURLY:
                    token = "{}";
                    break;
                case XSharpLexer.LBRKT:
                    token = "[]";
                    break;
            }
            if (!String.IsNullOrEmpty(token))
                tokenList.Add(token);
            triggerToken = GetPreviousToken(tokens, nextToken);
            //
            while (triggerToken != null)
            {
                token = triggerToken.Text;
                switch (triggerToken.Type)
                {
                    // For ) ] }, we will search the counter part, and remove all stuff in between
                    case XSharpLexer.RPAREN:
                        // Search for the Left Parenthesis
                        triggerToken = XSharpTokenTools.ProcessBounds(tokens, triggerToken, XSharpLexer.LPAREN, XSharpLexer.RPAREN);
                        // we had a trouble in the previous process ?
                        if (triggerToken == null)
                            break;
                        // ...
                        token = "()";
                        break;
                    case XSharpLexer.RBRKT:
                        // Search for the Left Bracket
                        triggerToken = XSharpTokenTools.ProcessBounds(tokens, triggerToken, XSharpLexer.LBRKT, XSharpLexer.RBRKT);
                        // we had a trouble in the previous process ?
                        if (triggerToken == null)
                            break;
                        // ...
                        token = "[]";
                        break;
                    case XSharpLexer.RCURLY:
                        // Search for the Left Curly
                        triggerToken = XSharpTokenTools.ProcessBounds(tokens, triggerToken, XSharpLexer.LCURLY, XSharpLexer.RCURLY);
                        // we had a trouble in the previous process ?
                        if (triggerToken == null)
                            break;
                        // ...
                        token = "{}";
                        break;
                    case XSharpLexer.ASSIGN_OP:
                    //case XSharpLexer.COMMA:
                    case XSharpLexer.USING:
                    case XSharpLexer.AS:
                    case XSharpLexer.IS:
                    case XSharpLexer.REF:
                    case XSharpLexer.IMPLEMENTS:
                    case XSharpLexer.INHERIT:
                    case XSharpLexer.LPAREN:
                    case XSharpLexer.LCURLY:
                    case XSharpLexer.LBRKT:
                    case XSharpLexer.SL_COMMENT:
                    case XSharpLexer.ML_COMMENT:
                        //case XSharpLexer.VAR:
                        //case XSharpLexer.IMPLIED:
                        // Stop here
                        stopToken = triggerToken;
                        token = null;
                        if (fromGotoDefn)
                        {
                            switch (triggerToken.Type)
                            {
                                case XSharpLexer.LPAREN:
                                    token = "()";
                                    break;
                                case XSharpLexer.LCURLY:
                                    token = "{}";
                                    break;
                                case XSharpLexer.LBRKT:
                                    token = "[]";
                                    break;
                                default:
                                    token = null;
                                    triggerToken = null;
                                    break;
                            }
                            // One shot
                            fromGotoDefn = false;
                        }
                        else
                            triggerToken = null;
                        break;
                    case XSharpLexer.DOT:
                    case XSharpLexer.COLON:
                    case XSharpLexer.COLONCOLON:
                    case XSharpLexer.SELF:
                    case XSharpLexer.SUPER:
                        break;
                    default:
                        // allow FLoat{} or String{}
                        if (XSharpLexer.IsType(triggerToken.Type))
                            break;
                        else if (XSharpLexer.IsKeyword(triggerToken.Type))
                        {
                            token = null;
                            triggerToken = null;
                        }
                        else if (XSharpLexer.IsOperator(triggerToken.Type))
                        {
                            token = null;
                            triggerToken = null;
                        }
                        break;
                }
                //
                if (token != null)
                    tokenList.Add(token);
                //
                if (triggerToken != null)
                {
                    var last = triggerToken;
                    triggerToken = GetPreviousToken(tokens, triggerToken);
                    // BOF reached
                    if (last == triggerToken)
                        break;
                }

            }
            //
            tokenList.Reverse();
            // Now, we may have some post-treatment
            List<String> returnList = new List<string>();
            int i = 0;
            bool prevWasDot = false;
            while (i < tokenList.Count)
            {
                token = tokenList[i];
                if ((token.CompareTo("()") == 0) || (token.CompareTo("{}") == 0) || (token.CompareTo("[]") == 0))
                {
                    if (returnList.Count > 0)
                    {
                        String prevToken = returnList[returnList.Count - 1];
                        prevToken = prevToken + token;
                        returnList[returnList.Count - 1] = prevToken;
                    }
                }
                else if ((token.CompareTo(".") == 0) && !dotAsSelector)
                {
                    if (returnList.Count > 0)
                    {
                        String prevToken = returnList[returnList.Count - 1];
                        prevToken = prevToken + token;
                        returnList[returnList.Count - 1] = prevToken;
                        prevWasDot = true;
                    }
                }
                else
                {
                    if (prevWasDot)
                    {
                        String prevToken = returnList[returnList.Count - 1];
                        prevToken = prevToken + token;
                        returnList[returnList.Count - 1] = prevToken;
                        prevWasDot = false;
                    }
                    else
                        returnList.Add(token);
                }
                i++;
            }
            //
            return returnList;
        }
        public static XType FindNamespace(int position, XFile file)
        {
            if (file == null)
            {
                return null;
            }
            if (file.TypeList == null)
                return null;
            //
            XType found = null;
            foreach (XType eltType in file.TypeList.Values)
            {

                if (eltType.Interval.ContainsInclusive(position))
                {
                    switch (eltType.Kind)
                    {
                        case Kind.Namespace:
                            return eltType;
                        case Kind.Class:
                        case Kind.Interface:
                        case Kind.Structure:
                        case Kind.Enum:
                            found = eltType;
                            break;
                    }
                }
            }
            //
            if (found != null)
            {
                string name = found.Name;
                if (found.NameSpace?.Length > 0)
                    name = found.NameSpace + "." + name;
                var pos = name.LastIndexOf('.');
                if (pos > 0)
                {
                    name = name.Substring(0, pos);
                }
                XType nSpace = new XType(name, Kind.Namespace, Modifiers.None,
                    Modifiers.Public, found.Range, found.Interval);
                return nSpace;
            }
#if TRACE
                // a source file without a namespace is really not a problem
                //Support.Debug(String.Format("Cannot find namespace at position {0} in file {0} .", position, fileName));
#endif
            return null;
        }
        public static XTypeMember FindMemberAtPosition(int nPosition, XFile file)
        {
            if (file == null)
            {
                return null;
            }
            var member = file.FindMemberAtPosition(nPosition);
            if (member is XTypeMember)
            {
                return member as XTypeMember;
            }
            // if we can't find a member then look for the global type in the file
            // and return its last member
            var xType = file.TypeList.FirstOrDefault();
            if (xType.Value != null)
            {
                return xType.Value.Members.LastOrDefault();
            }
            XSharpProjectPackage.Instance.DisplayOutPutMessage(String.Format("Cannot find member at 0 based position {0} in file {0} .", nPosition, file.FullPath));
            return null;

        }

        public static XTypeMember FindMember(int nLine, XFile file)
        {
            if (file == null)
            {
                return null;
            }
            var member = file.FindMemberAtRow(nLine);
            if (member is XTypeMember)
            {
                return member as XTypeMember;
            }
            if (member is XType)
            {
                var xtype = member as XType;
                if (xtype.Members.Count > 0)
                {
                    return xtype.Members.LastOrDefault();
                }
            }
            // try a few rows before
            member = file.FindMemberAtRow(Math.Max(nLine-10,1));
            if (member is XTypeMember)
            {
                return member as XTypeMember;
            }
            if (member is XType)
            {
                var xtype = member as XType;
                if (xtype.Members.Count > 0)
                {
                    return xtype.Members.LastOrDefault();
                }
            }

            // if we can't find a member then look for the global type in the file
            // and return its last member
            var ent = file.EntityList.LastOrDefault();
            if (ent is XTypeMember)
                return ent as XTypeMember;
            if (ent is XType)
                return ((XType)ent).Members.LastOrDefault();


#if DEBUG
            XSharpProjectPackage.Instance.DisplayOutPutMessage(String.Format("Cannot find member at 0 based line {0} in file {0} .", nLine, file.FullPath));
#endif
            return null;
        }

        /// <summary>
        /// Retrieve the CompletionType based on :
        ///  The Token list returned by GetTokenList()
        ///  The Token that stops the building of the Token List.
        /// </summary>
        /// <param name="file"></param>
        /// <param name="tokenList"></param>
        /// <param name="currentMember"></param>
        /// <param name="currentNS"></param>
        /// <param name="stopToken"></param>
        /// <param name="foundElement"></param>
        /// <returns></returns>
        public static CompletionType RetrieveType(XFile file, List<string> tokenList, XTypeMember currentMember, String currentNS,
            IToken stopToken, out CompletionElement foundElement, ITextSnapshot snapshot, int currentLine)
        {
            //
#if TRACE
                Stopwatch stopWatch = new Stopwatch();
                stopWatch.Start();
#endif
            foundElement = null;
            CompletionType cType = null;
            int currentPos = 0;
            var startOfExpression = true;
            String currentToken = "";
            //
            if (currentMember == null)
            {
                // try to find the first member in the file
                if (file != null)
                {
                    XElement elt = file.FindMemberAtRow(currentLine);
                    if (elt is XTypeMember)
                    {
                        currentMember = (XTypeMember)elt;
                    }
                    else if (elt is XType)
                    {
                        // We might be in the Class Declaration !?
                        switch (stopToken.Type)
                        {
                            case XSharpLexer.IMPLEMENTS:
                            case XSharpLexer.INHERIT:
                                if (tokenList.Count == 1)
                                {
                                    currentToken = tokenList[currentPos];
                                    cType = new CompletionType(currentToken, file, ((XType)(elt)).NameSpace);
                                }
                                break;
                            default:
                                cType = new CompletionType(elt);
                                break;
                        }
                        if (!cType.IsEmpty())
                        {
                            SearchConstructorIn(cType, Modifiers.Private, out foundElement);
                            if (foundElement.XSharpElement == null && cType.XType != null)
                            {
                                foundElement = new CompletionElement(cType.XType);
                            }
                            return cType;
                        }
                    }
                }
                //
                if (currentMember == null)
                {
#if TRACE
                        stopWatch.Stop();
                        Support.Debug(String.Format("Retrieve current Type : Member cannot be null."));
#endif
                    return null;
                }
            }
            else
            {
                file = currentMember.File;
            }
            //
            // we have to walk the tokenList, searching for the current Type
            // As we have separators every even token, we will walk by step 2


            CompletionType cTemp = null;
            if (tokenList.Count == 0)
                return null;
            // Context Type....
            cType = new CompletionType(currentMember.Parent.Clone);
            Modifiers visibility = Modifiers.Private;
            //
            while (currentPos < tokenList.Count)
            {
                currentToken = tokenList[currentPos];
                var lastToken = currentToken;

                //
                int dotPos = currentToken.LastIndexOf(".");
                if (dotPos > -1)
                {
                    String startToken = currentToken.Substring(0, dotPos);
                    if (String.IsNullOrEmpty(startToken))
                    {
                        currentPos += 1;
                        continue;
                    }
                    cType = new CompletionType(startToken, currentMember.File, currentMember.Parent.NameSpace);
                    if (cType.IsEmpty())
                    {
                        // could be namespace.Type
                        // so now try with right side of the string
                        startToken = currentToken.Substring(dotPos + 1);
                        if (String.IsNullOrEmpty(startToken))
                            break;
                        cType = new CompletionType(startToken, currentMember.File, currentMember.Parent.NameSpace);
                        //if (!cType.IsEmpty())
                        //    return cType;
                    }
                    if (!cType.IsEmpty())
                    {
                        currentToken = currentToken.Substring(dotPos + 1);
                        startOfExpression = false;
                        if (String.IsNullOrEmpty(currentToken))
                        {
                            currentPos += 2;
                            continue;
                        }
                    }
                }
                //
                if (stopToken != null)
                {
                    switch (stopToken.Type)
                    {
                        case XSharpLexer.AS:
                        case XSharpLexer.IS:
                        case XSharpLexer.REF:
                        case XSharpLexer.IMPLEMENTS:
                        case XSharpLexer.INHERIT:
                            if (tokenList.Count == 1)
                            {
                                // One Token, after such keyword, this is a type
                                // So we are looking for a Type, and we must end with a {}
                                if (!currentToken.EndsWith("{}"))
                                {
                                    currentToken += "{}";
                                }
                            }
                            break;
                        default:
                            break;
                    }
                }
                //
                if (currentToken.EndsWith("{}"))
                {
                    // Look for a type
                    currentToken = currentToken.Substring(0, currentToken.Length - 2);
                    cType = new CompletionType(currentToken, currentMember.File, currentMember.Parent.NameSpace);
                    if (!cType.IsEmpty())
                    {
                        SearchConstructorIn(cType, visibility, out foundElement);
                        if (foundElement.XSharpElement == null && cType.XType != null)
                        {
                            foundElement = new CompletionElement(cType.XType);
                        }
                    }
                }
                else if (currentToken.EndsWith("()"))
                {
                    // this a Method call
                    currentToken = currentToken.Substring(0, currentToken.Length - 2);
                    // Do we already know in which Type we are ?
                    if (currentToken.ToLower() == "self")
                    {
                        SearchConstructorIn(cType, visibility, out foundElement);
                    }
                    else if (currentToken.ToLower() == "super")
                    {
                        SearchConstructorIn(cType.ParentType, visibility, out foundElement);
                    }
                    // The first token in the list can be a Function or a Procedure
                    if (currentPos == 0)
                    {
                        var globType = SearchFunctionIn(currentMember.File, currentToken, out foundElement);
                        if (foundElement != null)
                        {
                            return globType;
                        }
                    }
                    if (!cType.IsEmpty())
                    {
                        // Now, search for a Method
                        cType = SearchMethodTypeIn(cType, currentToken, visibility, false, out foundElement);
                    }
                    if (cType.IsEmpty())
                    {
                        // check to see if this is a method from the Object Type, such as ToString().
                        cTemp = SearchMethodTypeIn(new CompletionType(typeof(object)), currentToken, visibility, false, out foundElement);
                        if (!cTemp.IsEmpty())
                        {
                            cType = cTemp;
                        }
                    }
                    if (cType.IsEmpty())
                    {
                        // Could it be Static Method with "Using Static"
                        // Now, search for a Method
                        cType = SearchMethodStaticIn(currentMember.File, currentToken, out foundElement);
                    }
                    if (cType.IsEmpty())
                    {
                        cType = null;
                    }
                }
                else
                {
                    if (currentToken.EndsWith("[]"))
                    {
                        currentToken = currentToken.Substring(0, currentToken.Length - 2);
                    }
                    // First token, so it could be a parameter or a local var
                    if (startOfExpression)
                    {
                        // Search in Parameters, Locals, Field and Properties
                        foundElement = FindIdentifier(currentMember, currentToken, ref cType, visibility, currentNS, snapshot, currentLine);
                    }
                    else
                    {
                        // We can have a Property/Field of the current CompletionType
                        if (!cType.IsEmpty())
                        {
                            cType = SearchPropertyOrFieldIn(cType, currentToken, visibility, out foundElement);
                        }
                    }
                    if (foundElement == null)
                    {
                        cType = SearchType(file, currentToken, out foundElement, currentNS);
                    }
                }
                //
                if (cType.IsEmpty())
                {

                    cType = null;
                }
                // Next Token
                currentPos += 1;
                if (currentPos >= tokenList.Count)
                {
                    break;
                }
                currentToken = tokenList[currentPos];
                if (currentToken == "." || currentToken == ":")
                {
                    currentPos += 1;
                    if (currentPos >= tokenList.Count)
                    {
                        break;
                    }
                }
                if (lastToken.EndsWith("(") || lastToken.EndsWith(")") || lastToken.EndsWith("{") || lastToken.EndsWith("}"))
                {
                    startOfExpression = true;
                }
                else
                {
                    startOfExpression = false;
                }
                //
                visibility = Modifiers.Public;
                if ((foundElement != null) && (foundElement.IsInitialized))
                {
                    if (String.Compare(foundElement.Name, "self", true) == 0)
                    {
                        visibility = Modifiers.Private;
                    }
                    else if (String.Compare(foundElement.Name, "super", true) == 0)
                    {
                        visibility = Modifiers.Protected;
                    }
                }
            }
#if TRACE
                //
                stopWatch.Stop();
                // Get the elapsed time as a TimeSpan value.
                TimeSpan ts = stopWatch.Elapsed;
                // Format and display the TimeSpan value.
                string elapsedTime = String.Format("{0:00}h {1:00}m {2:00}.{3:00}s",
                    ts.Hours, ts.Minutes, ts.Seconds,
                    ts.Milliseconds / 10);
                //
                XSharpProjectPackage.Instance.DisplayOutPutMessage("XSharpTokenTools::RetrieveType : Done in " + elapsedTime);
#endif
            return cType;
        }

        static public CompletionType SearchType(XFile xFile, string currentToken, out CompletionElement foundElement, string currentNs)
        {
            foundElement = null;
            CompletionType cType = null;
            WriteOutputMessage($"SearchType in file {xFile.SourcePath} {currentToken}");
            // check for a system type
            var usings = xFile.Usings.ToList();
            usings.Add(currentNs);
            var stype = xFile.Project.FindSystemType(currentToken, usings);
            if (stype != null)
            {
                cType = new CompletionType(stype);
                foundElement = new CompletionElement(stype);
            }
            if (cType == null)
            {
                var type = xFile.Project.Lookup(currentToken, true);
                if (type == null && !string.IsNullOrEmpty(currentNs))
                {
                    type = xFile.Project.Lookup(currentNs + "." + currentToken, true);
                }
                if (type != null)
                {
                    cType = new CompletionType(type);
                    foundElement = new CompletionElement(type);
                }
            }
            return cType;
        }


        static public CompletionElement FindIdentifier(XTypeMember member, string name, ref CompletionType cType,
            Modifiers visibility, string currentNS, ITextSnapshot snapshot, int currentLine)
        {
            WriteOutputMessage($"--> FindIdentifier in {cType.FullName}, {name} ");
            XElement element;
            CompletionElement foundElement = null;
            if (cType.IsEmpty())
            {
                cType = new CompletionType(member.Parent);
            }
            element = member.Parameters.Where(x => StringEquals(x.Name, name)).FirstOrDefault();
            if (element == null)
            {
                // then Locals
                element = member.GetLocals(snapshot, currentLine).Where(x => StringEquals(x.Name, name)).LastOrDefault();
                if (element == null)
                {
                    // We can have a Property/Field of the current CompletionType
                    if (!cType.IsEmpty())
                    {
                        cType = SearchPropertyOrFieldIn(cType, name, visibility, out foundElement);
                        if (foundElement != null)
                        {
                            element = foundElement.XSharpElement;
                        }

                    }
                    // Find Defines and globals in this file
                    if (element == null && cType.IsEmpty() && member.File.GlobalType != null)
                    {
                        element = member.File.GlobalType.Members.Where(x => StringEquals(x.Name, name)).FirstOrDefault();
                    }
                    if (element == null)
                    {
                        var type = member.File.Project.Lookup(XSharpModel.XType.GlobalName, true);
                        if (type != null)
                        {
                            element = type.Members.Where(x => StringEquals(x.Name, name)).FirstOrDefault();
                        }
                    }
                    if (element == null)
                    {
                        // Now, search for a Global in external Assemblies
                        //
                        cType = SearchGlobalFieldIn(member.File, name, out foundElement);
                    }
                }
            }
            if (element != null)
            {
                if (element is XVariable)
                {
                    cType = new CompletionType((XVariable)element, currentNS);
                    foundElement = new CompletionElement(element);
                }
                else
                {
                    cType = new CompletionType((XElement)element);
                    foundElement = new CompletionElement(element);

                }
            }
            return foundElement;
        }

        /// <summary>
        /// Search for the Constructor in the corresponding Type,
        /// no return value, the constructor is returned by foundElement
        /// </summary>
        /// <param name="cType"></param>
        /// <param name="minVisibility"></param>
        /// <param name="foundElement"></param>
        private static void SearchConstructorIn(CompletionType cType, Modifiers minVisibility, out CompletionElement foundElement)
        {
            WriteOutputMessage($"--> SearchConstructorIn {cType.FullName}");
            foundElement = null;
            if (cType.XType != null)
            {
                //
                XTypeMember xMethod = cType.XType.Members.Where(x =>
                {
                    if ((x.Kind == Kind.Constructor))
                    {
                        return true;
                    }
                    return false;
                }).FirstOrDefault();
                //if (elt.IsStatic)
                //    continue;
                if ((xMethod != null) && (xMethod.Visibility < minVisibility))
                {
                    xMethod = null;
                }
                //
                /*
                 * No More search for Constructors in parent
                if (xMethod == null)
                {
                    // Hummm, we should look inside the Owner
                    cType.XType.ForceComplete();
                    //
                    if (minVisibility == Modifiers.Private)
                        minVisibility = Modifiers.Protected;
                    if (cType.XType.Parent != null)
                    {
                        // Parent is a XElement, so one of our Types
                        SearchConstructorIn(new CompletionType(cType.XType.Parent), minVisibility, out foundElement);
                        return;
                    }
                    else if (cType.XType.ParentName != null)
                    {
                        // Parent has just a Name, so one of the System Types
                        SearchConstructorIn(new CompletionType(cType.XType.ParentName, cType.XType.FileUsings), minVisibility, out foundElement);
                        return;
                    }
                }
                else
                */
                {
                    foundElement = new CompletionElement(xMethod);
                    return;
                }
            }
            else if (cType.SType != null)
            {
                MemberInfo[] members;
                //
                if (minVisibility < Modifiers.Public)
                {
                    // Get Public, Internal, Protected & Private Members, we also get Instance vars,  all that WITHOUT inheritance
                    // But NO static constructors!
                    members = cType.SType.GetConstructors(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
                }
                else
                {
                    //  Get Public Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = cType.SType.GetConstructors(BindingFlags.Public | BindingFlags.Instance);
                }
                //
                MemberInfo method = null;
                foreach (var member in members)
                {
                    if (member.MemberType == MemberTypes.Constructor)
                    {
                        method = member as MemberInfo;
                        break;
                    }
                }
                /*
                 * No More search for Constructors in parent
                if (method == null)
                {
                    // In the parent ?
                    if (cType.SType.BaseType != null)
                    {
                        SearchConstructorIn(new CompletionType(cType.SType.BaseType), Modifiers.Public, out foundElement);
                        return;
                    }
                    else if (cType.SType.IsInterface)
                    {
                        SearchConstructorIn(new CompletionType(typeof(object)), Modifiers.Public, out foundElement);
                        return;
                    }
                }
                else
                */
                {
                    foundElement = new CompletionElement(method);
                    return;
                }
            }
            return;
        }


        /// <summary>
        /// Search for a Property or a Field, in a CompletionType, based on the Visibility.
        /// A Completion can have a XType (XSharp parsed type) or a SType (A System type or a Type found inside a library Reference)
        /// </summary>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="currentToken">The Property we are searching</param>
        /// <param name="minVisibility"></param>
        /// <returns>The CompletionType of the Property (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// </returns>
        internal static CompletionType SearchPropertyOrFieldIn(CompletionType cType, string currentToken, Modifiers minVisibility, out CompletionElement foundElement)
        {
            CompletionType result = SearchPropertyTypeIn(cType, currentToken, minVisibility, out foundElement);
            if (result.IsEmpty())
            {
                result = SearchFieldTypeIn(cType, currentToken, minVisibility, out foundElement);
            }
            return result;
        }

        private static CompletionType SearchExternalGlobalFieldIn(XFile xFile, string currentToken, out CompletionElement foundElement)
        {
            WriteOutputMessage($"<-- SearchExternalGlobalFieldIn {xFile.SourcePath}, {currentToken} ");
            foundElement = null;
            if (xFile == null)
            {
                return null;
            }
            //
            CompletionType cType = null;
            List<String> emptyUsings = new List<String>();
            foreach (string staticUsing in xFile.AllUsingStatics)
            {
                // Provide an Empty Using list, so we are looking for FullyQualified-name only
                CompletionType tempType = new CompletionType(staticUsing, xFile, emptyUsings);
                //
                cType = SearchPropertyOrFieldIn(tempType, currentToken, Modifiers.Public, out foundElement);
                if (!cType.IsEmpty())
                    break;
            }
            //
            return cType;
        }


        /// <summary>
        /// Search for a Property, in a CompletionType, based on the Visibility.
        /// A Completion can have a XType (XSharp parsed type) or a SType (A System type or a Type found inside a library Reference)
        /// </summary>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="currentToken">The Property we are searching</param>
        /// <param name="minVisibility"></param>
        /// <returns>The CompletionType of the Property (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// </returns>
        private static CompletionType SearchPropertyTypeIn(CompletionType cType, string currentToken, Modifiers minVisibility, out CompletionElement foundElement)
        {
            WriteOutputMessage($" SearchPropertyTypeIn {cType.FullName} , {currentToken}");

            foundElement = null;
            if (cType.XType != null)
            {
                XTypeMember element = cType.XType.Members.Where(x =>
                {
                    if ((x.Kind == Kind.Property) || (x.Kind == Kind.Access) || (x.Kind == Kind.Assign))
                    {
                        return StringEquals(x.Name, currentToken);
                    }
                    return false;
                }).FirstOrDefault();
                //
                if ((element != null) && (element.Visibility < minVisibility))
                {
                    element = null;
                }
                //
                if (element == null)
                {
                    // Hummm, we should look inside the Owner
                    cType.XType.ForceComplete();
                    if (cType.XType.Parent != null)
                    {
                        // Parent is a XElement, so one of our Types
                        return SearchPropertyTypeIn(new CompletionType(cType.XType.Parent), currentToken, Modifiers.Public, out foundElement);
                    }
                    else if (cType.XType.ParentName != null)
                    {
                        // Parent has just a Name, so one of the System Types
                        return SearchPropertyTypeIn(new CompletionType(cType.XType.ParentName, cType.File, cType.XType.FileUsings), currentToken, Modifiers.Public, out foundElement);
                    }
                }
                else
                {
                    cType = new CompletionType((XTypeMember)element);
                    foundElement = new CompletionElement(element);
                    return cType;
                }
            }
            else if (cType.SType != null)
            {
                MemberInfo[] members;
                //
                if (minVisibility < Modifiers.Public)
                {
                    // Get Public, Internal, Protected & Private Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = cType.SType.GetProperties(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
                }
                else
                {
                    //  Get Public Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = cType.SType.GetProperties(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static);
                }
                //
                Type declType = null;
                PropertyInfo prop = null;
                foreach (var member in members)
                {
                    if (isGenerated(member))
                        continue;

                    if (StringEquals(member.Name, currentToken))
                    {
                        prop = member as PropertyInfo;
                        declType = prop.PropertyType;
                        break;
                    }
                }
                if (declType == null)
                {
                    // In the parent ?
                    if (cType.SType.BaseType != null)
                    {
                        var result = SearchPropertyTypeIn(new CompletionType(cType.SType.BaseType), currentToken, Modifiers.Public, out foundElement);
                        if (!result.IsEmpty())
                            return result;
                    }

                    foreach (var type in cType.SType.GetInterfaces())
                    {
                        var result = SearchPropertyTypeIn(new CompletionType(type), currentToken, Modifiers.Public, out foundElement);
                        if (!result.IsEmpty())
                            return result;
                    }

                    // not needed: no properties in object type
                    //else if (cType.SType.IsInterface)
                    //{
                    //    return SearchPropertyTypeIn(new CompletionType(typeof(object)), currentToken, Modifiers.Public, out foundElement);
                    //}
                }
                else
                {
                    cType = new CompletionType(declType);
                    foundElement = new CompletionElement(prop);
                    return cType;
                }
            }
            // Sorry, not found
            return new CompletionType();

        }

        /// <summary>
        /// Search for a Field, in a CompletionType, based on the Visibility.
        /// A Completion can have a XType (XSharp parsed type) or a SType (A System type or a Type found inside a library Reference)
        /// </summary>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="currentToken">The Field we are searching</param>
        /// <param name="minVisibility"></param>
        /// <returns>The CompletionType of the Field (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// </returns>
        private static CompletionType SearchFieldTypeIn(CompletionType cType, string currentToken, Modifiers minVisibility, out CompletionElement foundElement)
        {
            WriteOutputMessage($" SearchFieldTypeIn {cType.FullName} , {currentToken}");
            foundElement = null;
            if (cType.XType != null)
            {
                XTypeMember element = cType.XType.Members.Where(x =>
                {
                    if (x.Kind.IsField())
                    {
                        return StringEquals(x.Name, currentToken);
                    }
                    return false;
                }).FirstOrDefault();
                //
                if ((element != null) && (element.Visibility < minVisibility))
                {
                    element = null;
                }
                //
                if (element == null)
                {
                    // Hummm, we should look inside the Owner
                    cType.XType.ForceComplete();
                    if (cType.XType.Parent != null)
                    {
                        // Parent is a XElement, so one of our Types
                        return SearchFieldTypeIn(new CompletionType(cType.XType.Parent), currentToken, Modifiers.Protected, out foundElement);
                    }
                    else if (cType.XType.ParentName != null)
                    {
                        // Parent has just a Name, so one of the System Types
                        return SearchFieldTypeIn(new CompletionType(cType.XType.ParentName, cType.File, cType.XType.FileUsings), currentToken, Modifiers.Protected, out foundElement);
                    }
                }
                else
                {
                    cType = new CompletionType((XTypeMember)element);
                    foundElement = new CompletionElement(element);
                    return cType;
                }
            }
            else if (cType.SType != null)
            {
                MemberInfo[] members;
                //
                if (minVisibility < Modifiers.Public)
                {
                    // Get Public, Internal, Protected & Private Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = cType.SType.GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
                }
                else
                {
                    //  Get Public Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = cType.SType.GetFields(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static);
                }
                //
                Type declType = null;
                FieldInfo field = null;
                foreach (var member in members)
                {
                    if (isGenerated(member))
                        continue;

                    if (StringEquals(member.Name, currentToken))
                    {
                        field = member as FieldInfo;
                        declType = field.FieldType;
                        break;
                    }
                }
                if (declType == null)
                {
                    // In the parent ?
                    if (cType.SType.BaseType != null)
                    {
                        return SearchFieldTypeIn(new CompletionType(cType.SType.BaseType), currentToken, Modifiers.Public, out foundElement);
                    }
                    // not needed: no fields in object type
                    //else if (cType.SType.IsInterface)
                    //{
                    //    return SearchFieldTypeIn(new CompletionType(typeof(object)), currentToken, Modifiers.Public, out foundElement);
                    //}
                }
                else
                {
                    foundElement = new CompletionElement(field);
                    return new CompletionType(declType);
                }
            }
            // Sorry, not found
            return new CompletionType();
        }

        /// <summary>
        /// Search for a Method, in a CompletionType, based on the Visibility.
        /// A Completion can have a XType (XSharp parsed type) or a SType (A System type or a Type found inside a library Reference)
        /// </summary>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="currentToken">The Method we are searching</param>
        /// <param name="minVisibility"></param>
        /// <returns>The CompletionType that the Method will return (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// </returns>
        internal static CompletionType SearchMethodTypeIn(CompletionType cType, string currentToken, Modifiers minVisibility, bool staticOnly, out CompletionElement foundElement)
        {
            WriteOutputMessage($" SearchMethodTypeIn {cType.FullName} , {currentToken}");
            foundElement = null;
            if (cType.XType != null)
            {
                //
                XTypeMember xMethod = cType.XType.Members.Where(x =>
                {
                    if ((x.Kind == Kind.Method))
                    {
                        return StringEquals(x.Name, currentToken);
                    }
                    return false;
                }).FirstOrDefault();
                //
                if ((xMethod != null) && staticOnly && !xMethod.IsStatic)
                {
                    xMethod = null;
                }
                //
                if ((xMethod != null) && (xMethod.Visibility < minVisibility))
                {
                    xMethod = null;
                }
                //
                if (xMethod == null)
                {
                    // Hummm, we should look inside the Owner
                    cType.XType.ForceComplete();
                    //
                    if (minVisibility == Modifiers.Private)
                        minVisibility = Modifiers.Protected;
                    if (cType.XType.Parent != null)
                    {
                        // Parent is a XElement, so one of our Types
                        return SearchMethodTypeIn(new CompletionType(cType.XType.Parent), currentToken, minVisibility, staticOnly, out foundElement);
                    }
                    else if (cType.XType.ParentName != null)
                    {
                        // Parent has just a Name, so one of the System Types
                        return SearchMethodTypeIn(new CompletionType(cType.XType.ParentName, cType.File, cType.XType.FileUsings), currentToken, minVisibility, staticOnly, out foundElement);
                    }
                }
                else
                {
                    foundElement = new CompletionElement(xMethod);
                    if (xMethod.Parent != null)
                    {
                        // Parent is a XElement, so one of our Types
                        return new CompletionType(xMethod.Parent.Clone);
                    }
                    else if (xMethod.ParentName != null)
                    {
                        // Parent has just a Name, so one of the System Types
                        return new CompletionType(xMethod.ParentName, xMethod.File, xMethod.FileUsings);
                    }
                }
            }
            else if (cType.SType != null)
            {
                MemberInfo[] members;
                //
                if (minVisibility < Modifiers.Public)
                {
                    // Get Public, Internal, Protected & Private Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = cType.SType.GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
                }
                else
                {
                    //  Get Public Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = cType.SType.GetMethods(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static);
                }
                //
                Type declType = null;
                MethodInfo method = null;
                foreach (var member in members)
                {
                    if (member.MemberType == MemberTypes.Method)
                    {
                        if (StringEquals(member.Name, currentToken))
                        {
                            method = member as MethodInfo;
                            declType = cType.SType; // method.ReturnType;
                            break;
                        }
                    }
                }
                //
                if ((method != null) && staticOnly && !method.IsStatic)
                {
                    method = null;
                }
                if (method == null)
                {
                    // In the parent ?
                    if (cType.SType.BaseType != null)
                    {
                        var result = SearchMethodTypeIn(new CompletionType(cType.SType.BaseType), currentToken, Modifiers.Public, staticOnly, out foundElement);
                        if (!result.IsEmpty())
                            return result;
                    }
                    foreach (var type in cType.SType.GetInterfaces())
                    {
                        var result = SearchMethodTypeIn(new CompletionType(type), currentToken, Modifiers.Public, staticOnly, out foundElement);
                        if (!result.IsEmpty())
                            return result;
                    }
                    if (cType.SType.IsInterface)
                    {
                        var result = SearchMethodTypeIn(new CompletionType(typeof(object)), currentToken, Modifiers.Public, staticOnly, out foundElement);
                        if (!result.IsEmpty())
                            return result;
                    }


                }
                else
                {
                    foundElement = new CompletionElement(method);
                    return new CompletionType(declType);
                }
            }
            // Sorry, not found
            return new CompletionType();
        }


        private static CompletionType SearchMethodStaticIn(XFile xFile, string currentToken, out CompletionElement foundElement)
        {
            WriteOutputMessage($" SearchMethodStaticIn {xFile.SourcePath}, {currentToken} ");
            foundElement = null;
            if (xFile == null)
            {
                return null;
            }
            //
            CompletionType cType = null;
            List<String> emptyUsings = new List<String>();
            foreach (string staticUsing in xFile.AllUsingStatics)
            {
                // Provide an Empty Using list, so we are looking for FullyQualified-name only
                CompletionType tempType = new CompletionType(staticUsing, xFile, emptyUsings);
                //
                cType = SearchMethodTypeIn(tempType, currentToken, Modifiers.Public, true, out foundElement);
                if (!cType.IsEmpty())
                    break;
            }
            //
            return cType;
        }

        private static CompletionType SearchGlobalFieldIn(XFile xFile, string currentToken, out CompletionElement foundElement)
        {
            WriteOutputMessage($" SearchGlobalFieldIn {xFile.SourcePath}, {currentToken} ");
            foundElement = null;
            if (xFile == null)
            {
                return null;
            }
            if (xFile.Project == null)
            {
                return null;
            }
            if (xFile.Project.AssemblyReferences == null)
            {
                return null;
            }
            //
            CompletionType cType = null;
            List<String> emptyUsings = new List<String>();
            foreach (AssemblyInfo asm in xFile.Project.AssemblyReferences)
            {
                List<AssemblyInfo> oneAssembly = new List<AssemblyInfo>();
                oneAssembly.Add(asm);
                Type globalType = SystemTypeController.Lookup("Functions", oneAssembly);
                //
                if (globalType != null)
                {
                    // Now, search for the Field
                    MemberInfo[] members;
                    //  Get Public Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = globalType.GetFields(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static);
                    //
                    Type declType = null;
                    FieldInfo field = null;
                    foreach (var member in members)
                    {
                        if (StringEquals(member.Name, currentToken))
                        {
                            field = member as FieldInfo;
                            declType = field.FieldType;
                            break;
                        }
                    }
                    if (declType != null)
                    {
                        foundElement = new CompletionElement(field);
                        return new CompletionType(declType);
                    }
                }
                //
            }
            //
            return cType;
        }

        private static CompletionType SearchFunctionIn(XFile xFile, string currentToken, out CompletionElement foundElement)
        {
            WriteOutputMessage($" SearchFunctionIn {xFile.SourcePath}, {currentToken} ");

            foundElement = null;
            if (xFile == null)
            {
                return null;
            }
            if (xFile.Project == null)
            {
                return null;
            }
            //
            CompletionType cType = null;
            //
            //
            XTypeMember xMethod = xFile.Project.FindFunction(currentToken);
            //
            if (xMethod == null)
            {
                foreach (var project in xFile.Project.ReferencedProjects)
                {
                    xMethod = project.FindFunction(currentToken);
                    if (xMethod != null)
                        break;
                }
            }
            foundElement = new CompletionElement(xMethod);
            if (xMethod?.Parent != null)
            {
                // Parent is a XElement, so one of our Types
                return new CompletionType(xMethod.Parent.Clone);
            }
            //
            return cType;
        }

        internal static IToken ProcessBounds(ITokenStream tokens, IToken currentToken, int LeftElement, int RightElement)
        {
            // Count the elements
            int rightElt = 1;
            while ((currentToken != null) && (rightElt != 0))
            {
                //currentToken = tokens.Lt(-1);
                currentToken = GetPreviousToken(tokens, currentToken);
                if (currentToken != null)
                {
                    // Bump the Left ?
                    if (currentToken.Column == 0)
                    {
                        if (rightElt != 0)
                        {
                            // If the counter is not null, we have a open element
                            currentToken = null;
                        }
                    }
                    // A Left Element ?
                    else if (currentToken.Type == LeftElement)
                    {
                        rightElt--;
                    }
                    // Another Right Parenthesis
                    else if (currentToken.Type == RightElement)
                    {
                        rightElt++;
                    }
                }
            }
            //
            return currentToken;
        }

        internal static IToken GetPreviousToken(ITokenStream tokens, IToken currentToken)
        {
            XSharpToken prev = null;
            if (currentToken != null)
            {
                prev = (XSharpToken)currentToken;
                int Line = prev.Line;
                do
                {
                    if (prev.OriginalTokenIndex == 0)
                        break;
                    prev = (XSharpToken)tokens.Get(prev.OriginalTokenIndex - 1);
                    if (prev.Line != Line)
                    {
                        prev = null;
                    }
                } while ((prev != null) && (String.IsNullOrWhiteSpace(prev.Text)));
            }
            return prev;
        }
        static void WriteOutputMessage(string message)
        {
            XSharpProjectPackage.Instance.DisplayOutPutMessage("XSharp.Codecompletion :"+message);
        }

    }

    /// <summary>
    /// Class that contains informations about the Code Element we have found during
    /// type searching.
    /// Used by Goto Definition, Parameter Info, ...
    /// </summary>
    public class CompletionElement
    {
        object foundElement = null;

        public CompletionElement(XElement XSharpElement)
        {
            this.foundElement = XSharpElement;
        }

        public CompletionElement(MemberInfo SystemElement)
        {
            this.foundElement = SystemElement;
        }


        public bool IsInitialized => this.foundElement != null;

        public MemberInfo SystemElement => foundElement as MemberInfo;

        public XElement XSharpElement => foundElement as XElement;

        public string Name
        {
            get
            {
                string name ;
                if (this.foundElement is MemberInfo)
                {
                    name = ((MemberInfo)this.foundElement).Name;
                }
                else if (foundElement is XElement)
                {
                    name = ((XElement) this.foundElement).Name;
                }
                else
                {
                    name = "";
                }
                return name;
            }
        }

    }

    // Build a list of all Keywords
    internal static class XSharpTypes
    {
        static ImmutableList<XType> _xTypes;

        static XSharpTypes()
        {
            // Dummy call to a Lexer; just to copy the Keywords, Types, ...
            // Pass default options so this will be the core dialect and no
            // 4 letter abbreviations will be in the list
            var lexer = XSharpLexer.Create("", "", XSharpParseOptions.Default);
            //

            var xTypes = new List<XType>();
            //
            foreach (var keyword in lexer.KwIds)
            {
                xTypes.Add(new XType(keyword.Key, Kind.Keyword, Modifiers.None, Modifiers.Public, TextRange.Empty, TextInterval.Empty));
            }
            //
            _xTypes = xTypes.ToImmutableList();
        }

        internal static ImmutableList<XType> Get()
        {
            return _xTypes;
        }
    }
    public class ErrorIgnorer : IErrorListener
    {
        #region IErrorListener
        public void ReportError(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            ; //  _errors.Add(new XError(fileName, span, errorCode, message, args));
        }

        public void ReportWarning(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            ; //  _errors.Add(new XError(fileName, span, errorCode, message, args));
        }
        #endregion
    }
    internal static class MemberExtensions
    {
        /// <summary>
        /// Retrieve the locals for a particular member
        /// </summary>
        /// <param name="member"></param>
        /// <param name="snapshot"></param>
        /// <param name="iCurrentLine"></param>
        /// <returns></returns>
        internal static IList<XVariable> GetLocals(this XTypeMember member, ITextSnapshot snapshot, int iCurrentLine)
        {
            var sourceWalker = new SourceWalker(member.File);
            // get the text of the member
            iCurrentLine = Math.Min(snapshot.LineCount - 1, iCurrentLine);
            // create a walker with just the contents of the current member
            var walker = new SourceWalker(member.File);
            var start = member.Range.StartLine - 1; // range = 1 based
            var lines = new List<String>();
            for (int i = start; i <= iCurrentLine; i++)
            {
                lines.Add(snapshot.GetLineFromLineNumber(i).GetText());
            }
            var info = walker.Parse(lines, true);
            var locals = new List<XVariable>();
            // Add the normal locals for class members
            if (member.Kind.IsClassMember() && !member.Modifiers.HasFlag(Modifiers.Static))
            {
                var XVar = new XVariable(member, "SELF", Kind.Local, member.Range, member.Interval, member.ParentName, false);
                locals.Add(XVar);
                if (member?.Parent?.ParentName != null)
                {
                    XVar = new XVariable(member, "SUPER", Kind.Local, member.Range, member.Interval, member.Parent.ParentName, false);
                    locals.Add(XVar);
                }
            }
            // add the locals found in the code.
            int nLineOffSet = member.Range.StartLine - 1;
            iCurrentLine += 1; // our ranges are 1 based
            foreach (EntityObject local in info.Locals)
            {
                var line = local.nStartLine + nLineOffSet;
                if (line <= iCurrentLine)
                {
                    var range = new TextRange(line, local.nCol, line, local.nCol + local.cName.Length);
                    var XVar = new XVariable(member, local.cName, Kind.Local, range, member.Interval, local.cRetType, false);
                    XVar.File = member.File;
                    locals.Add(XVar);
                }
            }
            return locals;
        }
    }
}



