//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Text;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using XSharpModel;

namespace XSharp.LanguageService
{
    /// <summary>
    /// This type stores tokens and the snapshot that they are based on in the TextBuffer of an open editor window
    /// It also stores (after parsing) the list of Entities in the buffer
    /// </summary>
    internal class XDocument
    {
        /// <summary>
        /// Result from the Lexer
        /// </summary>
        public BufferedTokenStream TokenStream { get; internal set; }
        /// <summary>
        /// Snapshot on which the info is based
        /// </summary>
        public ITextSnapshot SnapShot { get; internal set; }
        public IList<string> IncludeFiles { get; internal set; }
        public XDocument(BufferedTokenStream tokenstream, ITextSnapshot snapshot, IList<string> includeFiles)
        {
            TokenStream = tokenstream;
            SnapShot = snapshot;
            Entities = null;
            Lines = new Dictionary<int, IList<XSharpToken>>();
            IncludeFiles = includeFiles;
            LineKeywords = new XSharpLineKeywords(snapshot);
            LineState = new XSharpLineState(snapshot);
        }
        public bool Complete => TokenStream != null && SnapShot != null;
        /// <summary>
        /// Collection of entities (after parsing)
        /// </summary>
        public IList<XSourceEntity> Entities { get; set; }
        /// <summary>
        /// Collection of tokens per line on the default channel
        /// </summary>
        public Dictionary<int, IList<XSharpToken>> Lines;
        internal XSharpLineState LineState { get; set; }
        internal XSharpLineKeywords LineKeywords{ get; set; }
        internal bool HasLineState(int line, LineFlags flag)
        {
            if (LineState.Lines.ContainsKey(line))
            {
                return LineState.Lines[line].HasFlag(flag);
            }
            return false;
        }

    }
}
