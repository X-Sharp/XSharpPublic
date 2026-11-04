//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;

namespace XSharp.LanguageService.FindReferences
{
    /// <summary>
    /// Builds the per line token list, the identifier index and the line state of an XDocument
    /// with the same rules as XSharpClassifier, for documents that have no classifier (closed files).
    /// </summary>
    internal static class XDocumentIndexer
    {
        internal static void BuildIndexes(IList<IToken> tokens,
            out Dictionary<int, IList<IToken>> lineTokens,
            out ConcurrentDictionary<string, IList<IToken>> identifiers,
            out XSharpLineState lineState)
        {
            lineTokens = new Dictionary<int, IList<IToken>>();
            identifiers = new ConcurrentDictionary<string, IList<IToken>>(StringComparer.OrdinalIgnoreCase);
            lineState = new XSharpLineState();
            IList<IToken> currentLine = null;
            int lastLine = -1;
            foreach (var token in tokens)
            {
                if (token.Type == XSharpLexer.Eof)
                    continue;
                if (token.Type == XSharpLexer.ID)
                {
                    identifiers.GetOrAdd(token.Text, _ => new List<IToken>()).Add(token);
                }
                // token lines are 1 based, VS lines 0 based
                var line = token.Line - 1;
                if (token.Line != lastLine)
                {
                    lastLine = token.Line;
                    if (!lineTokens.TryGetValue(line, out currentLine))
                    {
                        currentLine = new List<IToken>();
                        lineTokens.Add(line, currentLine);
                        if (token.Type == XSharpLexer.LBRKT)
                        {
                            lineState.Set(line, LineFlags.StartsWithAttribute);
                        }
                    }
                }
                currentLine.Add(token);
                if (token.Type == XSharpLexer.LINE_CONT)
                {
                    // the NEXT line is a continuation
                    lineState.SetFlags(token.Line, LineFlags.IsContinued);
                }
            }
        }
    }
}
