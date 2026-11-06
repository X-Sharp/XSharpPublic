//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Controls;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using XSharpModel;
using Task = System.Threading.Tasks.Task;

namespace XSharp.LanguageService.Editors.LightBulb.QuickActions
{
    /// <summary>Shared parts of the quick actions: code style of a buffer, placing members in a class, editing.</summary>
    internal abstract class QuickAction : CommonAction, ISuggestedAction
    {
        protected readonly QuickActionContext Context;

        protected QuickAction(ITextView view, QuickActionContext context) : base(view)
        {
            Context = context;
        }

        public override Task<object> GetPreviewAsync(CancellationToken cancellationToken)
        {
            var preview = PreviewText();
            if (string.IsNullOrEmpty(preview))
                return Task.FromResult<object>(null);
            return Task.FromResult<object>(new TextBlock { Text = preview.Trim('\r', '\n'), Padding = new System.Windows.Thickness(5) });
        }

        protected abstract string PreviewText();

        public override void Invoke(CancellationToken cancellationToken)
        {
            try
            {
                ThreadHelper.JoinableTaskFactory.Run(InvokeAsync);
            }
            catch (Exception e)
            {
                XSharp.Support.Logger.Exception(e, GetType().Name);
            }
        }

        protected abstract Task InvokeAsync();

        internal static CodeStyle StyleOf(ITextBuffer buffer, XDialect dialect)
        {
            var settings = buffer.GetSettings();
            var size = settings?.IndentSize > 0 ? settings.IndentSize : 4;
            var unit = settings != null && !settings.TabsAsSpaces ? "\t" : new string(' ', size);
            return new CodeStyle(dialect, s => XLiterals.FormatKeyword(s), unit, "\r\n");
        }

        /// <summary>The view on the part of <paramref name="type"/>: this file when it holds a part, else the first part's file (opened).</summary>
        protected async Task<ITextView> ViewForAsync(XSourceTypeSymbol type)
        {
            if (PartIn(Context.File, type) != null)
                return m_textView;
            var path = type.File?.FullPath;
            if (string.IsNullOrEmpty(path))
                return null;
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            var documentView = await VS.Documents.OpenAsync(path);
            return documentView?.TextView;
        }

        internal static XSourceTypeSymbol PartIn(XFile file, XSourceTypeSymbol type)
        {
            return file?.EntityList.OfType<XSourceTypeSymbol>()
                .FirstOrDefault(t => string.Equals(t.FullName, type.FullName, StringComparison.OrdinalIgnoreCase));
        }

        /// <summary>
        /// Where new members of <paramref name="type"/> go in the file of <paramref name="view"/>: before END CLASS,
        /// or after the last member of a VO-style class; with the indentation of the existing members.
        /// </summary>
        internal static TargetClass Placement(ITextView view, XSourceTypeSymbol type, CodeStyle style)
        {
            var snapshot = view.TextBuffer.CurrentSnapshot;
            var file = view.TextBuffer.GetFile();
            var part = PartIn(file, type) ?? type;
            var endLine = Math.Max(0, Math.Min(part.Range.EndLine, snapshot.LineCount - 1));
            // a class without END CLASS ends where the next type starts: its last line may be END METHOD etc.
            bool hasEnd = MethodSkeleton.IsEndClassLine(snapshot.GetLineFromLineNumber(endLine).GetText());
            var members = file?.EntityList.OfType<XSourceMemberSymbol>()
                .Where(m => m.ParentType != null && string.Equals(m.ParentType.FullName, part.FullName, StringComparison.OrdinalIgnoreCase))
                .ToList() ?? new List<XSourceMemberSymbol>();
            int insertLine = hasEnd ? endLine : (members.Count > 0 ? members.Max(m => m.Range.EndLine) : endLine) + 1;
            insertLine = Math.Min(insertLine, snapshot.LineCount);
            var classIndent = Indent(snapshot, part.Range.StartLine);
            var indent = members.Count > 0 ? Indent(snapshot, members[0].Range.StartLine) : (hasEnd ? classIndent + style.IndentUnit : classIndent);
            return new TargetClass(part.Name, hasEnd, insertLine, indent);
        }

        private static string Indent(ITextSnapshot snapshot, int line)
        {
            if (line < 0 || line >= snapshot.LineCount)
                return "";
            var text = snapshot.GetLineFromLineNumber(line).GetText();
            return text.Substring(0, text.TakeWhile(char.IsWhiteSpace).Count());
        }

        /// <summary>Inserts <paramref name="text"/> at the start of 0-based <paramref name="line"/> in one undo step; returns the insert position or -1.</summary>
        internal static int Insert(ITextBuffer buffer, int line, string text)
        {
            var snapshot = buffer.CurrentSnapshot;
            var position = line < snapshot.LineCount ? snapshot.GetLineFromLineNumber(line).Start.Position : snapshot.Length;
            using (var edit = buffer.CreateEdit())
            {
                edit.Insert(position, text);
                if (!edit.HasEffectiveChanges)
                {
                    edit.Cancel();
                    return -1;
                }
                edit.Apply();
            }
            return position;
        }

        protected static void MoveCaret(ITextView view, int position)
        {
            var snapshot = view.TextBuffer.CurrentSnapshot;
            var point = new SnapshotPoint(snapshot, Math.Max(0, Math.Min(position, snapshot.Length)));
            view.Caret.MoveTo(point);
            view.ViewScroller.EnsureSpanVisible(new SnapshotSpan(point, 0), EnsureSpanVisibleOptions.AlwaysCenter);
        }
    }

    /// <summary>USING Namespace for a type name that does not resolve.</summary>
    internal sealed class AddUsingAction : QuickAction
    {
        private readonly string _namespace;

        internal AddUsingAction(ITextView view, QuickActionContext context, string ns) : base(view, context)
        {
            _namespace = ns;
        }

        public override string DisplayText => "USING " + _namespace;

        protected override string PreviewText() => "USING " + _namespace;

        protected override Task InvokeAsync()
        {
            var buffer = m_textView.TextBuffer;
            var snapshot = buffer.CurrentSnapshot;
            var lines = snapshot.Lines.Select(l => l.GetText()).ToList();
            var ends = (Context.File.UsingObjects ?? new List<XSourceUsing>())
                .Where(u => u.Interval.Stop > u.Interval.Start)
                .Select(u => u.Range.EndLine);
            var insertion = UsingInserter.Insert(lines, ends, _namespace, StyleOf(buffer, Context.Dialect));
            Insert(buffer, insertion.Line, insertion.Text);
            return Task.CompletedTask;
        }
    }

    /// <summary>An empty method in the target class for a call to a method that does not exist yet.</summary>
    internal sealed class GenerateMethodAction : QuickAction
    {
        private readonly XSourceTypeSymbol _target;

        internal GenerateMethodAction(ITextView view, QuickActionContext context, XSourceTypeSymbol target) : base(view, context)
        {
            _target = target;
        }

        public override string DisplayText => $"Generate method '{Context.Name.Text}' in class '{_target.Name}'";

        protected override string PreviewText()
        {
            var style = new CodeStyle(Context.Dialect, s => XLiterals.FormatKeyword(s), "    ", "\r\n");
            return MethodSkeleton.Build(Context.Name.Text, Context.Arguments, new TargetClass(_target.Name, true, 0, ""), style).Text;
        }

        protected override async Task InvokeAsync()
        {
            var view = await ViewForAsync(_target);
            if (view == null)
                return;
            var dialect = _target.File?.Project?.Dialect ?? Context.Dialect;
            var style = StyleOf(view.TextBuffer, dialect);
            var placement = Placement(view, _target, style);
            var insertion = MethodSkeleton.Build(Context.Name.Text, Context.Arguments, placement, style);
            var position = Insert(view.TextBuffer, insertion.Line, insertion.Text);
            if (position >= 0 && insertion.Caret >= 0)
                MoveCaret(view, position + insertion.Caret);
        }
    }

    /// <summary>"Generate overrides...": a dialog with the overridable members; the selected ones are inserted.</summary>
    internal sealed class GenerateOverridesAction : QuickAction
    {
        private readonly IReadOnlyList<IXMemberSymbol> _candidates;

        internal GenerateOverridesAction(ITextView view, QuickActionContext context, IReadOnlyList<IXMemberSymbol> candidates) : base(view, context)
        {
            _candidates = candidates;
        }

        public override string DisplayText => "Generate overrides...";

        protected override string PreviewText() => null;

        protected override Task InvokeAsync()
        {
            var type = Context.EnclosingClass;
            IList<IXMemberSymbol> selected;
            using (var dialog = new OverridesDlg(type.Name, _candidates))
            {
                if (dialog.ShowDialog() != System.Windows.Forms.DialogResult.OK)
                    return Task.CompletedTask;
                selected = dialog.Selected;
            }
            if (selected.Count == 0)
                return Task.CompletedTask;
            var buffer = m_textView.TextBuffer;
            var style = StyleOf(buffer, Context.Dialect);
            var placement = Placement(m_textView, type, style);
            var className = placement.HasEndClass ? null : type.Name;
            var text = string.Concat(selected.Select(m => OverrideSkeleton.Build(m, placement.MemberIndent, WriteOverride(m), style, className)));
            var position = Insert(buffer, placement.InsertLine, text);
            if (position >= 0)
                MoveCaret(m_textView, position + style.NewLine.Length);
            return Task.CompletedTask;
        }

        /// <summary>OVERRIDE for real .NET virtual base members; VO methods without VIRTUAL are virtual through the dialect.</summary>
        private static bool WriteOverride(IXMemberSymbol member)
        {
            return (member.Modifiers & (Modifiers.Virtual | Modifiers.Abstract | Modifiers.Override)) != 0;
        }
    }
}
