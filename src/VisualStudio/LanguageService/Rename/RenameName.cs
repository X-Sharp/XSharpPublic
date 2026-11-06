//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;

namespace XSharp.LanguageService.Rename
{
    internal enum RenameNameResult
    {
        Valid,
        /// <summary>Only the case differs: a valid rename in a case-insensitive language</summary>
        CaseOnly,
        Unchanged,
        Empty,
        Invalid,
        /// <summary>A keyword; write it with @@ to use it as a name</summary>
        Keyword
    }

    internal static class RenameName
    {
        /// <summary>
        /// Is <paramref name="newName"/> a name the symbol can get? <paramref name="lex"/> lexes it with the options of
        /// the target's project. Keywords that are names in most places (positional keywords) are valid.
        /// </summary>
        internal static RenameNameResult Check(string oldName, string newName, Func<string, IList<IToken>> lex)
        {
            newName = (newName ?? "").Trim();
            if (newName.Length == 0)
                return RenameNameResult.Empty;
            if (string.Equals(StripVerbatim(newName), StripVerbatim(oldName), StringComparison.Ordinal))
                return RenameNameResult.Unchanged;
            // the old name is a name already, also when it lexes as a keyword (Close, Name)
            if (string.Equals(StripVerbatim(newName), StripVerbatim(oldName), StringComparison.OrdinalIgnoreCase))
                return RenameNameResult.CaseOnly;
            var tokens = new List<IToken>();
            foreach (var token in lex(newName))
            {
                if (token.Channel == XSharpLexer.DefaultTokenChannel && token.Type != XSharpLexer.Eof && token.Type != XSharpLexer.EOS)
                    tokens.Add(token);
            }
            if (tokens.Count != 1 || tokens[0].Text.Length != newName.Length)
                return RenameNameResult.Invalid;
            var type = tokens[0].Type;
            if (type != XSharpLexer.ID)
            {
                if (XSharpLexer.IsKeyword(type) && !XSharpLexer.IsPositionalKeyword(type))
                    return RenameNameResult.Keyword;
                if (!XSharpLexer.IsPositionalKeyword(type))
                    return RenameNameResult.Invalid;
            }
            return RenameNameResult.Valid;
        }

        /// <summary>@@Name is the name Name, written so that a keyword can be used.</summary>
        private static string StripVerbatim(string name)
        {
            return name != null && name.StartsWith("@@", StringComparison.Ordinal) ? name.Substring(2) : name;
        }
    }
}
