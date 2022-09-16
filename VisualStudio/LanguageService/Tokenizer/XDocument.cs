//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis.XSharp;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
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
        public XDocument(ITextBuffer buffer,IList<IToken> tokens, ITextSnapshot snapshot)
        {
            _buffer = buffer;
            _tokens = tokens;
            _snapShot = snapshot;
            _entities = null;
            _tokensPerLine = new Dictionary<int, IList<IToken>>();
            _lineKeywords = new XSharpLineKeywords();
            _lineState = new XSharpLineState();
            _identifiers = new ConcurrentDictionary<string,string>(StringComparer.OrdinalIgnoreCase);
            NeedsKeywords = false;
        }

        #region fields
        private IList<IToken> _tokens;
        private IList<XSourceEntity> _entities;
        private Dictionary<int, IList<IToken>> _tokensPerLine;
        private XSharpLineState _lineState;
        private XSharpLineKeywords _lineKeywords;
        private ITextSnapshot _snapShot;
        private ConcurrentDictionary<string, string> _identifiers;
        private ITextBuffer _buffer;


        #endregion
        #region Properties
        internal IList<IToken> Tokens => _tokens;
        internal ITextSnapshot SnapShot => _snapShot;
        internal IList<XSourceEntity> Entities => _entities;
        internal Dictionary<int, IList<IToken>> TokensPerLine => _tokensPerLine;
        internal XSharpLineState LineState => _lineState;
        internal XSharpLineKeywords LineKeywords => _lineKeywords;
        internal IDictionary<string, string> Identifiers => _identifiers;
        internal bool NeedsKeywords { get; set; }
        #endregion

        internal bool HasLineState(int line, LineFlags flag)
        {
            return LineState.Get(line, out var flags ) && flags.HasFlag(flag);
        }

        internal void SetTokens(Dictionary<int, IList<IToken>> tokens)
        {
            lock (this)
            {
                _tokensPerLine = tokens;
            }
        }

        internal void SetState(XSharpLineState state, ITextSnapshot ss)
        {
            lock (this)
            {
                _lineState = state;
                _snapShot = ss;
            }
        }

        internal void SetKeywords(XSharpLineKeywords keywords)
        {
            lock (this)
            {
                _lineKeywords = keywords;
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
        internal void SetTokens(IList<IToken> tokens, ITextSnapshot ss)
        {
            lock (this)
            {
                _tokens = tokens;
                _snapShot = ss;
            }
        }

        internal ITextSnapshotLine GetLine(int lineNo)
        {
            return _buffer.CurrentSnapshot.GetLineFromLineNumber(lineNo);
        }
        internal IList<IToken> GetTokensInLine(int lineNo)
        {
            var line = _buffer.CurrentSnapshot.GetLineFromLineNumber(lineNo);
            var result = GetTokensInLine(line);
            var offset = line.Start.Position;
            foreach (var token in result)
            {
                if (token is XSharpToken xt)
                {
                    xt.StartIndex += offset;
                    xt.StopIndex += offset;
                }
            }
            return result;
        }
        internal IList<IToken> GetTokensInLine(ITextSnapshotLine line)
        {
            if (line.Length == 0)
                return new List<IToken>();
            var text = line.GetText();
            return GetTokens(text);
        }
        internal IList<IToken> GetTokens(string text)
        {
            var tokens = new List<IToken>();
            try
            {
                string fileName;
                var file = _buffer.GetFile();
                if (file != null)
                {
                    fileName = file.FullPath;
                }
                else
                {
                    fileName = "MissingFile.prg";
                }
                var reporter = new ErrorIgnorer();
                bool ok = Parser.VsParser.Lex(text, fileName, this.ParseOptions, reporter, out ITokenStream tokenStream);
                var stream = tokenStream as BufferedTokenStream;
                tokens.AddRange(stream.GetTokens());
                return tokens;
            }
            catch (Exception e)
            {
                XSettings.LogException(e, "GetTokens");
            }
            return tokens;
        }
        private XSharpParseOptions ParseOptions
        {
            get
            {
                XSharpParseOptions parseoptions;
                var file = _buffer.GetFile();
                if (file != null)
                {
                    parseoptions = file.Project.ParseOptions;
                }
                else
                {
                    parseoptions = XSharpParseOptions.Default;
                }
                return parseoptions;
            }
        }


    }
}
