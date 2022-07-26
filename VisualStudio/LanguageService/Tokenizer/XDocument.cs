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

        public XDocument(BufferedTokenStream stream, ITextSnapshot snapshot, IList<string> files)
        {
            tokenStream = stream;
            snapShot = snapshot;
            entities = null;
            tokensPerLine = new Dictionary<int, IList<IToken>>();
            includeFiles = files;
            lineKeywords = new XSharpLineKeywords(snapshot);
            lineState = new XSharpLineState(snapshot);
            identifiers = new Dictionary<string,string>(StringComparer.OrdinalIgnoreCase);
        }

        #region fields
        private BufferedTokenStream tokenStream;
        private IList<XSourceEntity> entities;
        private Dictionary<int, IList<IToken>> tokensPerLine;
        private XSharpLineState lineState;
        private XSharpLineKeywords lineKeywords;
        private IList<string> includeFiles;
        private ITextSnapshot snapShot;
        private IDictionary<string, string> identifiers;


        #endregion
        #region Properties
        internal BufferedTokenStream TokenStream => tokenStream;
        public bool Complete => TokenStream != null && SnapShot != null;
        internal ITextSnapshot SnapShot => snapShot;
        public IList<string> IncludeFiles => includeFiles;
        internal IList<XSourceEntity> Entities => entities;
        internal Dictionary<int, IList<IToken>> TokensPerLine => tokensPerLine;
        internal XSharpLineState LineState => lineState;
        internal XSharpLineKeywords LineKeywords => lineKeywords;
        internal IDictionary<string, string> Identifiers => identifiers;
        #endregion

        internal bool HasLineState(int line, LineFlags flag)
        {
            lock (this)
            {
                if (LineState.Lines.ContainsKey(line))
                {
                    return LineState.Lines[line].HasFlag(flag);
                }
            }
            return false;
        }
        internal bool GetKeyword(int line, out XKeyword keyword)
        {
            lock (this)
            {
                if (LineKeywords.Lines.ContainsKey(line))
                {
                    keyword = LineKeywords.Lines[line];
                    return true;
                }
                keyword = new XKeyword();
            }
            return false;
        }
        internal bool GetTokens(int line, out IList<IToken> tokens)
        {
            lock (this)
            {
                if (TokensPerLine.ContainsKey(line))
                {
                    tokens = TokensPerLine[line];
                    return tokens.Count > 0;
                }
                tokens = null;
            }
            return false;
        }
        
        internal void SetTokens(Dictionary<int, IList<IToken>> tokens)
        {
            lock (this)
            {
                tokensPerLine = tokens;
            }
        }
       
        internal void SetLineState(XSharpLineState state)
        {
            lock (this)
            {
                lineState = state;
            }
        }
        internal void SetLineKeywords(XSharpLineKeywords keywords)
        {
            lock (this)
            {
                lineKeywords = keywords;
            }
        }

        internal void SetIdentifiers(IDictionary<string, string> ids)
        {
            lock (this)
            {
                identifiers = ids;
            }
        }

        internal void SetEntities(IList<XSourceEntity> entities)
        {
            lock (this)
            {
                this.entities = entities;
            }
        }
        internal void SetData(BufferedTokenStream stream, ITextSnapshot ss, IList<string> files)
        {
            lock (this)
            {
                tokenStream = stream;
                snapShot = ss;
                includeFiles = files;
            }
        }
    }
}
