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
    public class XSharpTokens
    {
        /// <summary>
        /// Result from the Lexer
        /// </summary>
        public BufferedTokenStream TokenStream { get; set; }
        /// <summary>
        /// Snapshot on which the info is based
        /// </summary>
        public ITextSnapshot SnapShot { get; set; }
        public XSharpTokens(BufferedTokenStream tokenstream, ITextSnapshot snapshot)
        {
            TokenStream = tokenstream;
            SnapShot = snapshot;
            Entities = null;
            Lines = new Dictionary<int, IList<XSharpToken>>();
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

    }
}
