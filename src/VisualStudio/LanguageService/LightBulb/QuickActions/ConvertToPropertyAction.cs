//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using XSharp.LanguageService.FindReferences;
using XSharpModel;
using Task = System.Threading.Tasks.Task;

namespace XSharp.LanguageService.Editors.LightBulb.QuickActions
{
    /// <summary>Converts one ACCESS/ASSIGN pair, or all pairs of a class, into PROPERTY declarations in one undo step.</summary>
    internal sealed class ConvertToPropertyAction : QuickAction
    {
        private readonly IReadOnlyList<AccessAssignPair> _pairs;
        private readonly string _title;

        internal ConvertToPropertyAction(ITextView view, QuickActionContext context, IReadOnlyList<AccessAssignPair> pairs, string title) : base(view, context)
        {
            _pairs = pairs;
            _title = title;
        }

        public override string DisplayText => _title;

        protected override string PreviewText() => null;

        protected override Task InvokeAsync()
        {
            var buffer = m_textView.TextBuffer;
            var snapshot = buffer.CurrentSnapshot;
            // the member ranges come from the parse of this text version
            if (buffer.GetDocument()?.SnapShot?.Version != snapshot.Version || Context.Version != snapshot.Version.VersionNumber)
            {
                XSharp.Support.Logger.Information("Convert to property: the text changed since the light bulb was opened");
                return Task.CompletedTask;
            }
            var file = Context.File;
            var style = StyleOf(buffer, Context.Dialect);
            string LineText(int i) => snapshot.GetLineFromLineNumber(i).GetText();
            var part = Context.EnclosingPart ?? Context.EnclosingClass;
            var boundaries = file.EntityList.Select(e => e.Range.StartLine).ToList();
            if (MethodSkeleton.IsEndClassLine(LineText(Math.Min(part.Range.EndLine, snapshot.LineCount - 1))))
                boundaries.Add(part.Range.EndLine);
            boundaries.Sort();
            var replacements = new List<LineReplacement>();
            foreach (var pair in _pairs)
            {
                var edit = ForPair(pair, LineText, snapshot.LineCount, boundaries, style, text => ReferenceDocumentSource.Lex(file, text));
                if (edit != null)
                    replacements.AddRange(edit);
            }
            if (replacements.Count == 0 || !PropertyEdit.NonOverlapping(replacements))
                return Task.CompletedTask;
            using (var edit = buffer.CreateEdit())
            {
                foreach (var r in replacements.OrderByDescending(r => r.StartLine))
                {
                    var start = snapshot.GetLineFromLineNumber(r.StartLine).Start.Position;
                    var end = snapshot.GetLineFromLineNumber(r.EndLine).EndIncludingLineBreak.Position;
                    edit.Replace(Span.FromBounds(start, end), r.Text);
                }
                edit.Apply();
            }
            return Task.CompletedTask;
        }

        /// <summary>The replacements for one pair, or null when its headers carry other modifiers than the visibility.</summary>
        internal static IList<LineReplacement> ForPair(AccessAssignPair pair, Func<int, string> lineText, int lineCount, IList<int> boundaries,
            CodeStyle style, Func<string, IList<IToken>> lex)
        {
            var access = Source(pair.Access, false, lineText, lineCount, boundaries, out var accessLines);
            var assign = Source(pair.Assign, true, lineText, lineCount, boundaries, out var assignLines);
            if ((pair.Access != null && access == null) || (pair.Assign != null && assign == null))
                return null;
            // #region/#ifdef inside a member would be unbalanced by moving or removing it
            if ((accessLines.HasValue && PropertyEdit.HasPreprocessorLine(lineText, accessLines.Value)) ||
                (assignLines.HasValue && PropertyEdit.HasPreprocessorLine(lineText, assignLines.Value)))
                return null;
            var place = pair.Access != null ? accessLines.Value : assignLines.Value;
            var removed = pair.Access != null ? assignLines : null;
            var header = lineText(place.HeaderLine);
            var indent = header.Substring(0, header.Length - header.TrimStart().Length);
            var text = PropertyConverter.Build(pair.Name, TypeOf(pair), access, assign, indent, style, lex);
            return text == null ? null : PropertyEdit.Replacements(lineText, place, removed, text);
        }

        private static AccessorSource Source(XSourceMemberSymbol member, bool isAssign, Func<int, string> lineText, int lineCount,
            IList<int> boundaries, out MemberLines? lines)
        {
            lines = null;
            if (member == null)
                return null;
            var headerLine = member.Range.StartLine;
            if (!PropertyEdit.HeaderVisibility(lineText(headerLine), out var visibility))
                return null;
            var next = boundaries.Where(b => b > headerLine).DefaultIfEmpty(lineCount).Min();
            var range = PropertyEdit.MemberLines(lineText, lineCount, headerLine, next);
            lines = range;
            var body = string.Join("\r\n", Enumerable.Range(headerLine + 1, Math.Max(0, range.EndLine - headerLine)).Select(lineText));
            var parameters = member.Parameters ?? new List<IXParameterSymbol>();
            var indexParameters = (isAssign ? parameters.Skip(1) : parameters).ToList();
            var index = indexParameters.Select(p => p.IsTyped ? p.Name + " AS " + p.TypeName : p.Name).ToList();
            return new AccessorSource(body, isAssign && parameters.Count > 0 ? parameters[0].Name : null, index, visibility,
                indexParameters.Select(p => p.Name).ToList());
        }

        private static string TypeOf(AccessAssignPair pair)
        {
            if (pair.Access != null && pair.Access.IsTyped)
                return pair.Access.TypeName;
            var value = pair.Assign?.Parameters?.FirstOrDefault();
            return value != null && value.IsTyped ? value.TypeName : "";
        }
    }
}
