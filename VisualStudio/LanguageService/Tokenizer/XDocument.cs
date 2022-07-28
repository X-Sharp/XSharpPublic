//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Text;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using XSharpModel;

namespace XSharp.LanguageService
{
    /// <summary>
    /// This type stores tokens and the snapshot that they are based on the TextBuffer of an open editor window
    /// It also stores (after parsing) the list of Entities in the buffer
    /// </summary>
    internal class XDocument
    {
        public XDocument(IList<IToken> tokens, ITextSnapshot snapshot, IList<string> includeFiles)
        {
            _tokens = tokens;
            _snapShot = snapshot;
            _entities = null;
            _tokensPerLine = new Dictionary<int, IList<IToken>>();
            _includeFiles = includeFiles;
            _lineKeywords = new XSharpLineKeywords();
            _lineState = new XSharpLineState();
            _identifiers = new ConcurrentDictionary<string,string>(StringComparer.OrdinalIgnoreCase);
        }

        #region fields
        private IList<IToken> _tokens;
        private IList<XSourceEntity> _entities;
        private Dictionary<int, IList<IToken>> _tokensPerLine;
        private XSharpLineState _lineState;
        private XSharpLineKeywords _lineKeywords;
        private IList<string> _includeFiles;
        private ITextSnapshot _snapShot;
        private ConcurrentDictionary<string, string> _identifiers;


        #endregion
        #region Properties
        internal IList<IToken> Tokens => _tokens;
        internal ITextSnapshot SnapShot => _snapShot;
        public IList<string> IncludeFiles => _includeFiles;
        internal IList<XSourceEntity> Entities => _entities;
        internal Dictionary<int, IList<IToken>> TokensPerLine => _tokensPerLine;
        internal XSharpLineState LineState => _lineState;
        internal XSharpLineKeywords LineKeywords => _lineKeywords;
        internal IDictionary<string, string> Identifiers => _identifiers;
        #endregion

        internal bool HasLineState(int line, LineFlags flag)
        {
            return LineState.Get(line, out var flags ) && flags.HasFlag(flag);
        }
        internal bool GetKeyword(int line, out XKeyword keyword)
        {
            return LineKeywords.Get(line, out keyword);
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
                _tokensPerLine = tokens;
            }
        }
       
        internal void SetData(XSharpLineState state, XSharpLineKeywords keywords,ITextSnapshot ss)
        {
            lock (this)
            {
                _lineState = state;
                _lineKeywords = keywords;
                _snapShot = ss;
            }
        }

        internal void SetIdentifiers(ConcurrentDictionary<string, string> ids)
        {
            lock (this)
            {
                _identifiers = ids;
            }
        }

        internal void SetEntities(IList<XSourceEntity> entities)
        {
            lock (this)
            {
                this._entities = entities;
            }
        }
        internal void SetData(IList<IToken> tokens, ITextSnapshot ss, IList<string> files)
        {
            lock (this)
            {
                _tokens = tokens;
                _snapShot = ss;
                _includeFiles = files;
            }
        }
    }
}
