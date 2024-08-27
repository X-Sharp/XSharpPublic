//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Windows.Controls;
using XSharpModel;
using static XSharp.Parser.VsParser;
namespace XSharp.LanguageService
{
    /// <summary>
    /// This type stores tokens and the snapshot that they are based on the TextBuffer of an open editor window
    /// It also stores (after parsing) the list of Entities in the buffer
    /// </summary>
    internal class XDocument : IErrorListener
    {
        public XDocument(ITextBuffer buffer, IList<IToken> tokens, ITextSnapshot snapshot)
        {
            _buffer = buffer;
            _tokens = tokens;
            _snapShot = snapshot;
            _entities = new List<XSourceEntity>();
            _blocks = new List<XSourceBlock>();
            _tokensPerLine = new Dictionary<int, IList<IToken>>();
            _lineState = new XSharpLineState();
            _identifiers = new ConcurrentDictionary<string, IList<IToken>>(StringComparer.OrdinalIgnoreCase);
        }

        #region fields
        private readonly IList<IToken> _tokens;
        private readonly IList<XSourceEntity> _entities;
        private readonly Dictionary<int, IList<IToken>> _tokensPerLine;
        private XSharpLineState _lineState;
        private ITextSnapshot _snapShot;
        private readonly ConcurrentDictionary<string, IList<IToken>> _identifiers;
        private readonly ITextBuffer _buffer;
        private readonly IList<XSourceBlock> _blocks;

        #endregion
        #region Properties
        internal IList<IToken> Tokens => _tokens;
        internal ITextSnapshot SnapShot => _snapShot;
        internal IList<XSourceEntity> Entities => _entities;
        internal Dictionary<int, IList<IToken>> TokensPerLine => _tokensPerLine;
        internal XSharpLineState LineState => _lineState;
        internal IDictionary<string, IList<IToken>> Identifiers => _identifiers;
        internal IList<XSourceBlock> Blocks => _blocks;
        internal ITextBuffer Buffer => _buffer;

        #endregion


        internal void Clear()
        {

            lock (this)
            {
                _tokens.Clear();
                _identifiers.Clear();
                _lineState.Clear();
                _tokensPerLine.Clear();
                _entities.Clear();
                _blocks.Clear();
            }
        }

        internal bool HasLineState(int line, LineFlags flag)
        {
            return LineState.Get(line, out var flags) && flags.HasFlag(flag);
        }
        internal bool LineAfterAttribute(int line)
        {
            while (line > 0)
            {
                line -= 1;
                LineState.Get(line, out var flags);
                if (flags.HasFlag(LineFlags.StartsWithAttribute))
                    return true;
                if (!flags.HasFlag(LineFlags.IsContinued))
                    return false;
            }
            return false;
        }
        internal void SetTokens(Dictionary<int, IList<IToken>> tokens)
        {
            lock (this)
            {
                _tokensPerLine.Clear();
                _tokensPerLine.AddRange(tokens);
            }
        }

        internal void SetBlocks(IList<XSourceBlock> blocks)
        {
            lock (this)
            {
                _blocks.Clear();
                _blocks.AddRange(Blocks);
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

        internal void SetIdentifiers(ConcurrentDictionary<string, IList<IToken>> ids)
        {
            lock (this)
            {
                _identifiers.Clear();
                _identifiers.AddRange(ids);
            }
        }

        internal void SetEntities(IList<XSourceEntity> entities)
        {
            lock (this)
            {
                this._entities.Clear();
                this._entities.AddRange(entities);
            }
        }
        internal void SetTokens(IList<IToken> tokens, ITextSnapshot ss)
        {
            lock (this)
            {
                _tokens.Clear();
                _tokens.AddRange(tokens);
                _snapShot = ss;
            }
        }

        internal ITextSnapshotLine GetLine(int lineNo)
        {
            return _buffer.CurrentSnapshot.GetLineFromLineNumber(lineNo);
        }
        /// <summary>
        /// Returns token in the line, and when the line is continued also from the continued line
        /// </summary>
        /// <param name="lineNo">Starting Line Number</param>
        /// <returns>Tokens from the line and the lines following it when the statement is spread over multiple lines</returns>
        internal IList<IToken> GetTokensInLineAndFollowing(int lineNo)
        {
            var line = _buffer.CurrentSnapshot.GetLineFromLineNumber(lineNo);
            var result = GetTokensInSingleLine(line, true);
            lineNo += 1;
            while (HasLineState(lineNo, LineFlags.IsContinued))
            {
                line = _buffer.CurrentSnapshot.GetLineFromLineNumber(lineNo);
                var temp = GetTokensInSingleLine(line, true);
                result.AddRange(temp);
                lineNo += 1;
            }
            return result;
        }

        internal IList<IToken> GetTokensInSingleLine(ITextSnapshotLine line, bool allowCached)
        {
            List<IToken> tokens = new List<IToken>();
            if (line.Length == 0)
                return tokens;
            if (line.Snapshot.Version == this.SnapShot.Version && allowCached)
            {
                // When the snapshot matches, return a clone of the line we already have. 
                // no need to lex again
                if (this._tokensPerLine.TryGetValue(line.LineNumber, out var token))
                    tokens.AddRange(token);
                return tokens;
            }
            var text = line.GetText();
            tokens.AddRange(GetTokens(text));
            var offset = line.Start.Position;
            foreach (var token in tokens)
            {
                // Since this is the result of lexing a single line we have to adjust the indices 
                // and the linenumber
                if (token is XSharpToken xt)
                {
                    xt.StartIndex += offset;
                    xt.StopIndex += offset;
                    xt.Line = line.LineNumber;
                }
            }
            return tokens;

        }
        internal XSourceEntity GetCurrentEntity(IWpfTextView textView)
        {
            var currentChar = textView.Caret.Position.BufferPosition;
            int currentLine = currentChar.GetContainingLine().LineNumber;
            // LastOrDefault because we want the innermost entity
            // CLASS
            // METHOD
            // line        this line is both inside the class and the method. We want the method
            // END METHOD
            // END CLASS
            var currentEntity = _entities.LastOrDefault(e => e.Range.StartLine <= currentLine && e.Range.EndLine >= currentLine);
            return currentEntity;

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
                bool ok = Parser.VsParser.Lex(text, fileName, ParseOptions, this, out ITokenStream tokenStream, out var _);
                var stream = tokenStream as BufferedTokenStream;
                tokens.AddRange(stream.GetTokens());
                return tokens;
            }
            catch (Exception e)
            {
                Logger.Exception(e, "GetTokens");
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

        #region IErrorListener
        public void ReportError(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            ; //  _errors.Add(new XError(fileName, span, errorCode, message, args));
        }

        public void ReportWarning(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            ; //  _errors.Add(new XError(fileName, span, errorCode, message, args));
        }
        #endregion



    }
}
