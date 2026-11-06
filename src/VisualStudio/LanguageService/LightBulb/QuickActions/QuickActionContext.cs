//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Text;
using XSharp.LanguageService.FindReferences;
using XSharp.Settings;
using XSharpModel;

namespace XSharp.LanguageService.Editors.LightBulb.QuickActions
{
    /// <summary>
    /// What the quick actions need to know about the caret position, computed once per text version and caret
    /// position in the background: the enclosing class, and for an identifier under the caret what it resolves to,
    /// its receiver and the arguments of the call.
    /// </summary>
    internal sealed class QuickActionContext
    {
        internal int Version { get; private set; }
        internal int Position { get; private set; }
        internal ITextSnapshot Snapshot { get; private set; }
        internal XFile File { get; private set; }
        internal XProject Project => File?.Project;
        internal string CurrentNamespace { get; private set; } = "";
        internal IList<string> Usings { get; private set; } = new List<string>();

        /// <summary>The source class the caret is in (the merged type when the model has one), or null.</summary>
        internal XSourceTypeSymbol EnclosingClass { get; private set; }

        /// <summary>The part of <see cref="EnclosingClass"/> in this file: its file knows the USINGs to resolve the base class.</summary>
        internal XSourceTypeSymbol EnclosingPart { get; private set; }

        /// <summary>The identifier under the caret, or null.</summary>
        internal IToken Name { get; private set; }
        internal IList<IXSymbol> Symbols { get; private set; } = new List<IXSymbol>();
        internal ReceiverKind Receiver { get; private set; }
        internal IXTypeSymbol ReceiverType { get; private set; }
        internal bool IsCall { get; private set; }
        internal IList<CallArgument> Arguments { get; private set; } = new List<CallArgument>();

        internal bool CaseSensitive => Project?.ParseOptions?.CaseSensitive ?? false;
        internal XDialect Dialect => Project?.Dialect ?? XDialect.Core;

        /// <summary>
        /// The context at <paramref name="position"/> of <paramref name="snapshot"/>, or null when the editor's
        /// document is not parsed for that snapshot (yet) or the search was cancelled.
        /// </summary>
        internal static QuickActionContext Compute(ITextBuffer buffer, ITextSnapshot snapshot, int position, CancellationToken ct)
        {
            var file = buffer.GetFile();
            var document = buffer.GetDocument();
            if (file == null || file.Project == null || document?.SnapShot == null || document.SnapShot.Version != snapshot.Version)
                return null;
            var tokens = document.Tokens;
            if (tokens == null)
                return null;
            var result = new QuickActionContext { Version = snapshot.Version.VersionNumber, Position = position, Snapshot = snapshot, File = file };
            ModelWalker.Suspend();
            try
            {
                var ns = XSharpTokenTools.FindNamespace(position, file);
                result.CurrentNamespace = ns?.FullName ?? "";
                var member = XSharpLookup.FindMemberAtPosition(position, file);
                result.EnclosingClass = EnclosingClassOf(file, member, position, out var part);
                result.EnclosingPart = part;
                ct.ThrowIfCancellationRequested();
                var name = tokens.FirstOrDefault(t => t.Type == XSharpLexer.ID && t.Channel == XSharpLexer.DefaultTokenChannel &&
                                                      t.StartIndex <= position && position <= t.StopIndex + 1);
                var line = name != null ? name.Line - 1 : snapshot.GetLineNumberFromPosition(position);
                var location = new XSharpSearchLocation(document, file, member, snapshot, line, name?.StartIndex ?? position, result.CurrentNamespace);
                result.Usings = location.Usings ?? new List<string>();
                if (name == null)
                    return result;
                result.Name = name;
                var context = location;
                result.Symbols = SymbolResolver.ResolveAt(ref location, out _, out _);
                ct.ThrowIfCancellationRequested();
                var index = IndexOf(tokens, name);
                result.IsCall = NextType(tokens, index) == XSharpLexer.LPAREN;
                result.Receiver = ReceiverOf(tokens, index, out var receiver);
                if (result.Receiver == ReceiverKind.Variable)
                {
                    var typeName = SymbolResolver.TypeOf(context, receiver);
                    if (!string.IsNullOrEmpty(typeName))
                        result.ReceiverType = context.FindType(typeName) ?? file.FindType(typeName);
                }
                if (result.IsCall)
                {
                    var groups = OverloadFilter.Arguments(tokens, name) ?? new List<IList<IToken>>();
                    result.Arguments = groups.Select(g => Argument(context, g)).ToList();
                }
                return result;
            }
            catch (OperationCanceledException)
            {
                return null;
            }
            finally
            {
                ModelWalker.Resume();
            }
        }

        /// <summary>The class of the member at the caret (VO-style members too), else the innermost class whose text holds the caret.</summary>
        private static XSourceTypeSymbol EnclosingClassOf(XFile file, XSourceMemberSymbol member, int position, out XSourceTypeSymbol part)
        {
            part = null;
            // only a member that holds the caret tells the class (on a class header the lookup returns the member before it)
            var type = member != null && member.Interval.Start <= position && position <= member.Interval.Stop
                ? member.ParentType as XSourceTypeSymbol
                : null;
            if (type == null || type.IsGlobal)
            {
                type = file.EntityList.OfType<XSourceTypeSymbol>()
                    .Where(t => !t.IsGlobal && t.Kind == Kind.Class && t.Interval.Start <= position && position <= t.Interval.Stop)
                    .OrderBy(t => t.Interval.Stop - t.Interval.Start)
                    .FirstOrDefault();
            }
            if (type == null || type.IsGlobal || type.Kind != Kind.Class)
                return null;
            part = type;
            // the merged type knows the members of all parts
            return file.FindType(type.FullName) as XSourceTypeSymbol ?? type;
        }

        private static CallArgument Argument(XSharpSearchLocation context, IList<IToken> group)
        {
            var tokens = group.Where(t => t.Channel == XSharpLexer.DefaultTokenChannel).ToList();
            if (tokens.Count == 1 && tokens[0].Type == XSharpLexer.ID)
            {
                var type = SymbolResolver.TypeOf(context, tokens[0]);
                // the lookup reports System.Int32 where the source says INT
                return new CallArgument(tokens[0].Text, string.IsNullOrEmpty(type) ? null : type.GetXSharpTypeName());
            }
            return new CallArgument(null, null);
        }

        private static ReceiverKind ReceiverOf(IList<IToken> tokens, int index, out IToken receiver)
        {
            receiver = null;
            var op = Previous(tokens, index);
            if (op < 0 || (tokens[op].Type != XSharpLexer.COLON && tokens[op].Type != XSharpLexer.DOT))
                return ReceiverKind.None;
            var r = Previous(tokens, op);
            if (r < 0)
                return ReceiverKind.Other;
            receiver = tokens[r];
            switch (receiver.Type)
            {
                case XSharpLexer.SELF:
                    return ReceiverKind.Self;
                case XSharpLexer.SUPER:
                    return ReceiverKind.Super;
                case XSharpLexer.ID:
                    var before = Previous(tokens, r);
                    if (before >= 0 && (tokens[before].Type == XSharpLexer.COLON || tokens[before].Type == XSharpLexer.DOT))
                        return ReceiverKind.Other;
                    return ReceiverKind.Variable;
                default:
                    return ReceiverKind.Other;
            }
        }

        private static int IndexOf(IList<IToken> tokens, IToken token)
        {
            var index = token.TokenIndex;
            if (index >= 0 && index < tokens.Count && tokens[index] == token)
                return index;
            return tokens.IndexOf(token);
        }

        private static int Previous(IList<IToken> tokens, int index)
        {
            for (int i = index - 1; i >= 0; i--)
            {
                if (tokens[i].Channel == XSharpLexer.DefaultTokenChannel)
                    return i;
            }
            return -1;
        }

        private static int NextType(IList<IToken> tokens, int index)
        {
            for (int i = index + 1; i < tokens.Count; i++)
            {
                if (tokens[i].Channel == XSharpLexer.DefaultTokenChannel)
                    return tokens[i].Type;
            }
            return XSharpLexer.Eof;
        }
    }
}
