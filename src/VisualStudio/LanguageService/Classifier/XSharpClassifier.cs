//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
//------------------------------------------------------------------------------

using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Language.StandardClassification;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Threading;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using XSharpModel;
using Task = System.Threading.Tasks.Task;
using XSharp.Settings;
namespace XSharp.LanguageService
{
    /// <summary>
    /// Classifier that classifies all text as an instance of the "XSharpClassifier" classification type.
    /// There is one classifier per (visible) editor window.
    /// VS delays creating the classifier until the window is shown for the first time
    /// We can store data in the classifier
    /// </summary>
    public class XSharpClassifier : IClassifier
    {
        #region Static Fields
        static private IClassificationType xsharpKeywordType;
        static private IClassificationType xsharpIdentifierType;
        static private IClassificationType xsharpCommentType;
        static private IClassificationType xsharpOperatorType;
        static private IClassificationType xsharpStringType;
        static private IClassificationType xsharpNumberType;
        static private IClassificationType xsharpPPType;
        static private IClassificationType xsharpRegionStart;
        static private IClassificationType xsharpRegionStop;
        static private IClassificationType xsharpInactiveType;
        static private IClassificationType xsharpLiteralType;
        static private IClassificationType xsharpTextType;
        #endregion

        #region Private Fields
        private readonly object gate = new object();
        private readonly SourceWalker _sourceWalker;
        private readonly ITextBuffer _buffer;
        private XDocument _document = null;

        private XClassificationSpans _colorTags = new XClassificationSpans();
        private IList<ClassificationSpan> _lexerRegions = null;
        private IList<ClassificationSpan> _parserRegions = null;
        private readonly bool _first = true;

        private readonly List<String> _xtraKeywords;
        private XSharpLineState _lineState;
        private XSharpLineKeywords _lineKeywords;
        private bool IsLexing = false;
        private bool IsStarted = false;
        private bool _needsKeywords = false;

        #endregion

        #region Properties
        public ITextSnapshot Snapshot
        {
            get
            {
                var xdoc = GetDocument();
                if (xdoc != null)
                    return xdoc.SnapShot;
                return null;
            }
        }
        public SourceWalker SourceWalker => _sourceWalker;

        #endregion


        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpClassifier"/> class.
        /// </summary>
        /// <param name="registry">Classification registry.</param>

        internal XSharpClassifier(ITextBuffer buffer, IClassificationTypeRegistryService registry, ITextDocumentFactoryService _)
        {
            XFile file = null;
            this._buffer = buffer;
            if (buffer.Properties.ContainsProperty(typeof(XFile)))
            {
                file = buffer.GetFile();
            }
            if (file == null)
            {
                return;
            }
            // we do not check for the existence of the XDocument here.
            // The classifier may be called before the XDocument was created
            //
            _lineState = new XSharpLineState();
            _lineKeywords = new XSharpLineKeywords();
            _xtraKeywords = new List<string>();
            // Initialize our background workers
            _buffer.Changed += Buffer_Changed;

            if (xsharpKeywordType == null)
            {
                // These fields are static so only initialize the first time
                xsharpKeywordType = registry.GetClassificationType(PredefinedClassificationTypeNames.Keyword);
                xsharpIdentifierType = registry.GetClassificationType(PredefinedClassificationTypeNames.Identifier);
                xsharpCommentType = registry.GetClassificationType(PredefinedClassificationTypeNames.Comment);
                xsharpOperatorType = registry.GetClassificationType(PredefinedClassificationTypeNames.Operator);
                xsharpPPType = registry.GetClassificationType(PredefinedClassificationTypeNames.PreprocessorKeyword);
                xsharpNumberType = registry.GetClassificationType(PredefinedClassificationTypeNames.Number);
                xsharpStringType = registry.GetClassificationType(PredefinedClassificationTypeNames.String);
                xsharpInactiveType = registry.GetClassificationType(PredefinedClassificationTypeNames.ExcludedCode);
                xsharpLiteralType = registry.GetClassificationType(PredefinedClassificationTypeNames.Literal);
                xsharpTextType = registry.GetClassificationType(ColorizerConstants.XSharpTextEndTextFormat);
                xsharpRegionStart = registry.GetClassificationType(ColorizerConstants.XSharpRegionStartFormat);
                xsharpRegionStop = registry.GetClassificationType(ColorizerConstants.XSharpRegionStopFormat);
            }
            // Run a synchronous scan to set the initial buffer colors
            _sourceWalker = new SourceWalker(file);
            ClassifyBuffer();
            _first = false;
            // start the model builder to do build a code model and the regions asynchronously
            var x = ThreadHelper.JoinableTaskFactory.RunAsync(LexAsync);

        }

        #region Lexer Methods

        private void Buffer_Changed(object sender, TextContentChangedEventArgs e)
        {
            if (!IsLexing)
            {
                if (!IsStarted)
                {
                    IsStarted = true;
#if DEV17
                    _ = ThreadHelper.JoinableTaskFactory.StartOnIdle(LexAsync,VsTaskRunContext.UIThreadIdlePriority);
#else
                    _ = ThreadHelper.JoinableTaskFactory.StartOnIdleShim(StartLex, VsTaskRunContext.UIThreadIdlePriority);
#endif
                }
            }
            else
            {
                Debug("Buffer_Changed: Suppress lexing because classifier is active");
            }
    }
#if !DEV17
#pragma warning disable VSTHRD100 // Avoid async void methods
        public async void StartLex()
#pragma warning restore VSTHRD100 // Avoid async void methods
        {
            await LexAsync();
        }
#endif
        private XDocument GetDocument()
        {
            lock (gate)
            {
                if (_document == null)
                {
                    _document = _buffer.GetDocument();
                }
            }
            return _document;
        }
        internal async Task<XSharpLineKeywords> GetKeywordsAsync()
        {
            try
            {
                _needsKeywords = true;
                var _ = await LexAsync();
                return _lineKeywords;
            }
            finally
            {
                _needsKeywords = false;
            }
        }

        public async Task ForceClassifyAsync()
        {
            var _ = await LexAsync();
        }
        public async Task ClassifyWhenNeededAsync()
        {
            XDocument xDocument = GetDocument();
            if (!IsLexing)
            {
                if (xDocument == null || xDocument.SnapShot.Version != _buffer.CurrentSnapshot.Version)
                {
                    var _ = await LexAsync();
                }
            }
        }
        private async Task<bool> LexAsync()
        {
            var success = false;
            //await TaskScheduler.Default;
            try
            {
                IsLexing = true;
                success = ClassifyBuffer();
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "LexAsync");
            }
            finally
            {
                IsLexing = false;
                IsStarted = false;
                if (success)
                {
                    success = await ParseAsync();
                }
            }
            return success;
        }
        public void Parse()
        {
            XDocument xDocument = GetDocument();
            if (xDocument != null)
            {
                var _= ThreadHelper.JoinableTaskFactory.RunAsync(ParseAsync);
            }
        }

        private bool ClassifyBuffer()
        {
            bool result = false;
            if (XEditorSettings.DisableSyntaxHighlighting)
                return result;
            var snapshot = _buffer.CurrentSnapshot;
            XDocument xDocument = GetDocument();
            if (xDocument == null || xDocument.SnapShot.Version != snapshot.Version)
            {
                _lineState = new XSharpLineState();
                _lineKeywords = new XSharpLineKeywords();
                Debug("Starting classify at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
                var tokenstream = _sourceWalker.Lex(snapshot.GetText());
                lock (gate)
                {
                    var tokens = tokenstream.GetTokens();
                    if (xDocument == null)
                    {
                        xDocument = new XDocument(_buffer, tokens, snapshot);
                        _buffer.Properties[typeof(XDocument)] = xDocument;
                    }
                    else
                    {
                        xDocument.Clear();
                        xDocument.SetTokens(tokens, snapshot);
                    }
                    xDocument.SetState(_lineState, snapshot);
                }
            }
            BuildColorClassifications();
            Debug("Ending classify at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            result = true;
            return result;
        }

        private void TriggerRepaint(ITextSnapshot snapshot)
        {
            if (ClassificationChanged != null)
            {
                Logger.Information("-->> XSharpClassifier.triggerRepaint()");
                if (snapshot != null && _buffer?.CurrentSnapshot != null)
                {
                    // tell the editor that we have new info
                    if (!_first)
                    {
                        ClassificationChanged(this, new ClassificationChangedEventArgs(
                                new SnapshotSpan(snapshot, Span.FromBounds(0, snapshot.Length))));
                    }
                }
                Logger.Information("<<-- XSharpClassifier.triggerRepaint()");
            }
        }

#endregion

#region Parser Methods

        private async Task<bool> ParseAsync()
        {
            if (XSettings.DisableEntityParsing)
                return false;
            if (IsLexing)
                return false;
            await TaskScheduler.Current;
            var snapshot = _buffer.CurrentSnapshot;
            var xDocument = GetDocument();
            if (xDocument == null) // should not happen
                return false;
            if (xDocument.SnapShot != snapshot)
            {
                Logger.Information($"XSharpClassifier.ParseAsync() aborted because snapshot is version {xDocument.SnapShot.Version} and buffer has version {snapshot.Version}");
                return false;
            }
            Logger.Information("-->> XSharpClassifier.ParseAsync()");
            // Note this runs in the background
            if (xDocument.Tokens != null)
            {
                Debug("Starting model build  at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
                _sourceWalker.SaveToDisk = true;
                // Parse the source using the tokens that we have already
                // collected for the colorization
                _sourceWalker.ParseBlocks(xDocument.Tokens);
                RegisterEntityBoundaries();
                var regionTags = BuildRegionTags(_sourceWalker.EntityList, _sourceWalker.BlockList, snapshot, xsharpRegionStart, xsharpRegionStop);
                lock (gate)
                {
                    _parserRegions = regionTags.ToArray();
                }
                DoRepaintRegions();
                Debug("Ending model build  at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            }
            Logger.Information("<<-- XSharpClassifier.ParseAsync()");
            return true;
        }
#endregion

        private void RegisterEntityBoundaries()
        {
            // Register the entity boundaries for the line separators
            // The LineState array already has info about inactive lines
            // and comment lines
            XDocument xDocument = GetDocument();
            if (xDocument == null)
                return;
            xDocument.SetEntities(_sourceWalker.EntityList);
            foreach (var entity in _sourceWalker.EntityList)
            {
                // Include the leading XML comments for the Dividers
                var line = entity.Range.StartLine;
                if (entity.StartOfXmlComments >= 0 && !string.IsNullOrEmpty(entity.XmlComments))
                    line = entity.StartOfXmlComments;
                if (entity.SingleLine)
                {
                    _lineState.SetFlags(line, LineFlags.SingleLineEntity);
                }
                else
                {
                    _lineState.SetFlags(line, LineFlags.EntityStart);
                }
            }
            LineStateChanged?.Invoke(this, new EventArgs());
        }
        private void DoRepaintRegions()
        {
            if (_buffer.Properties.ContainsProperty(typeof(XSharpOutliningTagger)))
            {
                var tagger = _buffer.Properties[typeof(XSharpOutliningTagger)] as XSharpOutliningTagger;
                tagger.Update();
            }

        }

        private IList<ClassificationSpan> BuildRegionTags(IList<XSourceEntity> entities, IList<XSourceBlock> blocks, ITextSnapshot snapshot, IClassificationType _, IClassificationType _1)
        {
            if (XEditorSettings.DisableRegions)
            {
                return new List<ClassificationSpan>();
            }
            Logger.Information("-->> XSharpClassifier.BuildRegionTags()");
            var regions = new List<ClassificationSpan>();
            foreach (var entity in entities)
            {
                if (entity is XSourceMemberSymbol member)
                {
                    if (member.SingleLine)
                    {
                        continue;
                    }
                }
                var startPos = entity.Interval.Start;
                var endPos = entity.Interval.Stop;
                AddRegionSpan(regions, snapshot, startPos, endPos);
            }
            _document.SetBlocks(blocks);

            foreach (var block in blocks)
            {
                var startPos = block.Token.StartIndex;
                var endPos = block.Last.Token.StopIndex;

                AddRegionSpan(regions, snapshot, startPos, endPos);
                if (block.Children.Count > 1)
                {
                    var lastline = block.Token.Line;
                    var canJump = block.CanJump;
                    foreach (var child in block.Children)
                    {
                        // Jump statements are also children of the block 
                        // but should not result in a region
                        // HACK
                        // the two JUMP keywords are hard coded for simplicity
                        bool addRegion = true;
                        if (canJump)
                        {
                            addRegion = child.Token.Type != XSharpLexer.EXIT && child.Token.Type != XSharpLexer.LOOP;
                        }
                        if (addRegion)
                        {
                            if (child.Token.Line > lastline + 1 && child.Token.Line >= 2)
                            {
                                // the child is a line with CASE, ELSE, ELSEIF CATCH etc.
                                // we want the last token of the previous like to be the end of the previous block
                                endPos = snapshot.GetLineFromLineNumber(child.Token.Line - 2).End;
                                AddRegionSpan(regions, snapshot, startPos, endPos);
                            }
                            startPos = child.Token.StartIndex;
                            lastline = child.Token.Line;
                        }
                    }
                }
            }
            Logger.Information("<<-- XSharpClassifier.BuildRegionTags()");
            return regions;
        }
        private void AddRegionSpan(List<ClassificationSpan> regions, ITextSnapshot snapshot, int startPos, int endPos)
        {
            try
            {
                TextSpan tokenSpan;
                ClassificationSpan span;
                if (endPos > snapshot.Length)
                {
                    endPos = snapshot.Length;
                }
                if (startPos < snapshot.Length)
                {
                    var line = snapshot.GetLineFromPosition(startPos);
                    int nLineLength = line.Length;
                    tokenSpan = new TextSpan(startPos, nLineLength);
                    span = tokenSpan.ToClassificationSpan(snapshot, xsharpRegionStart);
                    regions.Add(span);
                    line = snapshot.GetLineFromPosition(endPos);
                    endPos = line.Start;
                    nLineLength = line.Length;
                    tokenSpan = new TextSpan(endPos, nLineLength);
                    span = tokenSpan.ToClassificationSpan(snapshot, xsharpRegionStop);
                    regions.Add(span);
                }
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine("Error setting region: " + e.Message);
            }
            return;
        }

        /// <summary>
        /// Group several Tokens in a special span, specifying it's type
        /// </summary>
        /// <param name="start"></param>
        /// <param name="stop"></param>
        /// <param name="snapshot"></param>
        /// <param name="type"></param>
        /// <returns></returns>
        private ClassificationSpan Token2ClassificationSpan(IToken start, IToken stop, ITextSnapshot snapshot, IClassificationType type)
        {
            TextSpan tokenSpan = new TextSpan(start.StartIndex, stop.StopIndex - start.StartIndex + 1);
            ClassificationSpan span = tokenSpan.ToClassificationSpan(snapshot, type);
            return span;
        }

        /// <summary>
        /// "Mark" a Token, specifying it's type and the span it covers
        /// </summary>
        /// <param name="token"></param>
        /// <param name="snapshot"></param>
        /// <param name="type"></param>
        /// <returns></returns>
        private ClassificationSpan Token2ClassificationSpan(IToken token, ITextSnapshot snapshot, IClassificationType type )
        {
            if (token != null && snapshot != null)
            {
                TextSpan tokenSpan = new TextSpan(token.StartIndex, token.StopIndex - token.StartIndex + 1);
                ClassificationSpan span = tokenSpan.ToClassificationSpan(snapshot, type);
                return span;
            }
            return default;
        }

        private ClassificationSpan ClassifyToken(IToken token, IList<ClassificationSpan> regionTags, ITextSnapshot snapshot, IToken lastToken)
        {
            var tokenType = token.Type;
            ClassificationSpan result = null;
            switch (token.Channel)
            {
                case XSharpLexer.PREPROCESSORCHANNEL:
                    // #define, #ifdef etc
                    _lineState.SetFlags(token.Line - 1, LineFlags.Preprocessor);
                    result = Token2ClassificationSpan(token, snapshot, xsharpPPType);
                    if (!XEditorSettings.DisableRegions)
                    {
                        switch (token.Type)
                        {
                            case XSharpLexer.PP_REGION:
                                regionTags.Add(Token2ClassificationSpan(token, snapshot, xsharpRegionStart));
                                break;
                            case XSharpLexer.PP_ENDREGION:
                                regionTags.Add(Token2ClassificationSpan(token, snapshot, xsharpRegionStop));
                                break;
                            default:
                                break;
                        }
                    }
                    break;
                case XSharpLexer.DEFOUTCHANNEL:                // code in an inactive #ifdef
                    result = Token2ClassificationSpan(token, snapshot, xsharpInactiveType);
                    _lineState.SetFlags(token.Line - 1, LineFlags.Inactive);
                    break;
                case XSharpLexer.XMLDOCCHANNEL:
                case XSharpLexer.Hidden:
                    if (token.Type == XSharpLexer.LINE_CONT)
                    {
                        // the NEXT line is a continuation. The token Line number is 1 based. VS is 0 based.
                        _lineState.SetFlags(token.Line , LineFlags.IsContinued);
                    }
                    if (XSharpLexer.IsComment(token.Type))
                    {
                        result = Token2ClassificationSpan(token, snapshot, xsharpCommentType);
                        if (token.Type == XSharpLexer.ML_COMMENT)
                        {
                            regionTags.Add(Token2ClassificationSpan(token, snapshot, xsharpRegionStart));
                            regionTags.Add(Token2ClassificationSpan(token, snapshot, xsharpRegionStop));
                            var startline = result.Span.Start.GetContainingLine().LineNumber;
                            var endLine = result.Span.End.GetContainingLine().LineNumber;
                            for (int i = startline; i <= endLine; i++)
                            {
                                _lineState.SetFlags(i, LineFlags.MultiLineComments);
                            }
                        }
                        else
                        {
                            if (token.Type == XSharpLexer.DOC_COMMENT)
                            {
                                var startline = result.Span.Start.GetContainingLine().LineNumber;
                                var endLine = result.Span.End.GetContainingLine().LineNumber;
                                for (int i = startline; i <= endLine; i++)
                                {
                                    _lineState.SetFlags(i, LineFlags.DocComments);
                                }
                            }
                            else
                                _lineState.SetFlags(token.Line - 1, LineFlags.SingleLineComments);
                        }
                    }
                    break;
                default: // Normal channel
                    IClassificationType type = null;
                    if (XSharpLexer.IsIdentifier(tokenType))
                    {
                        if (_xtraKeywords.Find(kw => string.Compare(kw, token.Text, true) == 0) != null)
                        {
                            type = xsharpKeywordType;
                        }
                        else
                            type = xsharpIdentifierType;
                    }
                    else if (XSharpLexer.IsConstant(tokenType))
                    {
                        switch (tokenType)
                        {
                            case XSharpLexer.STRING_CONST:
                            case XSharpLexer.CHAR_CONST:
                            case XSharpLexer.ESCAPED_STRING_CONST:
                            case XSharpLexer.INTERPOLATED_STRING_CONST:
                            case XSharpLexer.INCOMPLETE_STRING_CONST:
                                type = xsharpStringType;
                                break;
                            case XSharpLexer.BRACKETED_STRING_CONST:
                                if (lastToken != null && lastToken.Type != XSharpLexer.LPAREN && lastToken.Type != XSharpLexer.COMMA)
                                {
                                    type = xsharpStringType;
                                }
                                break;
                            case XSharpLexer.TEXT_STRING_CONST:
                                type = xsharpTextType;
                                break;
                            case XSharpLexer.FALSE_CONST:
                            case XSharpLexer.TRUE_CONST:
                                type = xsharpKeywordType;
                                break;
                            case XSharpLexer.VO_AND:
                            case XSharpLexer.VO_NOT:
                            case XSharpLexer.VO_OR:
                            case XSharpLexer.VO_XOR:
                            case XSharpLexer.SYMBOL_CONST:
                            case XSharpLexer.NIL:
                                type = xsharpLiteralType;
                                break;
                            case XSharpLexer.BIN_CONST:
                            case XSharpLexer.HEX_CONST:
                            case XSharpLexer.REAL_CONST:
                            case XSharpLexer.INT_CONST:
                            case XSharpLexer.DATE_CONST:
                            case XSharpLexer.DATETIME_CONST:
                                type = xsharpNumberType;
                                break;
                            default:
                                if ((tokenType >= XSharpLexer.FIRST_NULL) && (tokenType <= XSharpLexer.LAST_NULL))
                                {
                                    type = xsharpKeywordType;
                                    break;
                                }
                                else
                                    type = xsharpNumberType;
                                break;
                        }

                    }
                    else if (XSharpLexer.IsKeyword(tokenType))
                    {
                        type = xsharpKeywordType;
                    }
                    else if (XSharpLexer.IsOperator(tokenType))
                    {
                        type = xsharpOperatorType;
                        // we are no longer marking these with BraceOpen and BraceClose
                        // to avoid accidental matching of an Open paren with an ENDIF keyword


                    }
                    if (type != null)
                    {
                        result = Token2ClassificationSpan(token, snapshot, type);
                    }
                    break;
            }
            return result;
        }

        private void ScanForRegion(IToken token, int iToken, IList<IToken> tokens,
            ref int iLast, ITextSnapshot snapshot, IList<ClassificationSpan> regionTags)
        {
            if (iToken > iLast)
            {
                var lastToken = ScanForLastToken(token.Type, iToken, tokens, out iLast);
                if (token != lastToken)
                {
                    regionTags.Add(Token2ClassificationSpan(token, snapshot, xsharpRegionStart));
                    regionTags.Add(Token2ClassificationSpan(lastToken, snapshot, xsharpRegionStop));
                }
            }
        }

        private void addKw(XKeyword kw, int iLine)
        {
            
            if (kw.IsEmpty)
                return;
            var isStart = kw.IsStart();
            var isEnd = kw.IsStop();
            var isMiddle = kw.IsMiddle();
            if (isStart)
            {
                var rule = XFormattingRule.GetFirstRuleByStart(kw);
                RuleStack.Push(rule);
            }

            if (isEnd || isMiddle || isStart)
            {
                _lineKeywords.Set(iLine, kw);
                if (kw.IsSingle && kw.IsEnd && CurrentRule != null)
                {
                    if (CurrentRule.Flags.HasFlag(XFormattingFlags.End))
                        isEnd = true;
                }
            }
            if (isEnd)
            {
                if (RuleStack.Count > 0)
                    RuleStack.Pop();
            }
        }
        private XFormattingRule CurrentRule => RuleStack?.Count == 0 ? null : RuleStack.Peek();
        private Stack<XFormattingRule> RuleStack;
        private void BuildColorClassifications()
        {
            if (_document == null)
                return;
            var snapshot = _document.SnapShot;
            if (_buffer.CurrentSnapshot != snapshot)
                return;
            var tokens = _document.Tokens;
            Debug("Start building Classifications at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            XClassificationSpans newtags;
            var lineTokens = new Dictionary<int, IList<IToken>>(snapshot.LineCount);
            var currentLine = new List<IToken>();
            var ids = new ConcurrentDictionary<string, IList<IToken>>(StringComparer.OrdinalIgnoreCase);
            RuleStack = new Stack<XFormattingRule>();
            var regionTags = new List<ClassificationSpan>();
            if (tokens != null)
            {
                int iLastInclude = -1;
                int iLastPPDefine = -1;
                int iLastDefine = -1;
                int iLastSLComment = -1;
                int iLastDocComment = -1;
                int iLastUsing = -1;
                XKeyword kw;
                newtags = new XClassificationSpans();
                IToken keywordContext = null;
                IToken lastToken = null;
                int iLastLine = -1;
                for (var iToken = 0; iToken < tokens.Count; iToken++)
                {
                    var token = tokens[iToken];
                    if (token.Type == XSharpLexer.ID && (XEditorSettings.IdentifierCase || !XEditorSettings.DisableHighLightWord))
                    {
                        if (!ids.ContainsKey(token.Text))
                        {
                            var xtokens = new List<IToken> { token };
                            ids.TryAdd(token.Text, xtokens);
                        }
                        else
                        {
                            var xtokens = ids[token.Text];
                            xtokens.Add(token);
                        }
                    }
                    // store the tokens per line in a dictionary so we can quickly look them up
                    var line = token.Line - 1;  // VS Has 0 based line numbers
                    if (token.Line != iLastLine)
                    {
                        // register the type for the previous line
                        if (currentLine?.Count > 0 && _needsKeywords)
                        {
                            kw = XSharpLineKeywords.Tokens2Keyword(currentLine);
                            addKw(kw, iLastLine-1);
                        }
                        iLastLine = token.Line;
                        if (!lineTokens.ContainsKey(line))
                        {
                            currentLine = new List<IToken>();
                            lineTokens.Add(line, currentLine);
                            if (token.Type == XSharpLexer.LBRKT)
                            {
                                _lineState.Set(token.Line-1, LineFlags.StartsWithAttribute);
                            }
                        }
                    }
                    currentLine.Add(token);
 
                    // Orphan End ?
                    if ((keywordContext != null) && (keywordContext.Line != token.Line) && (keywordContext.Type == XSharpLexer.END))
                    {
                        newtags.Add(Token2ClassificationSpan(keywordContext, snapshot, xsharpKeywordType));
                        keywordContext = null;
                    }
                    var span = ClassifyToken(token, regionTags, snapshot, lastToken);

                    if (span != null)
                    {
                        // don't forget the current one
                        newtags.Add(span);
#if FALSE
                        // We can have some Open/Close keyword ( FOR..NEXT; WHILE...ENDDO; IF...ENDIF)
                        if (span.ClassificationType == xsharpKeywordType)
                        {
                            var list = ClassifyKeyword(token, snapshot, ref keywordContext);
                            foreach (var item in list)
                                newtags.Add(item);
                        }
#endif
                        if (!XEditorSettings.DisableRegions)
                        {
                            // now look for Regions of similar code lines
                            switch (token.Type)
                            {
                                case XSharpLexer.PP_INCLUDE:
                                    ScanForRegion(token, iToken, tokens, ref iLastInclude, snapshot, regionTags);
                                    break;
                                case XSharpLexer.PP_DEFINE:
                                    ScanForRegion(token, iToken, tokens, ref iLastPPDefine, snapshot, regionTags);
                                    break;
                                case XSharpLexer.DEFINE:
                                    ScanForRegion(token, iToken, tokens, ref iLastDefine, snapshot, regionTags);
                                    break;
                                case XSharpLexer.SL_COMMENT:
                                    ScanForRegion(token, iToken, tokens, ref iLastSLComment, snapshot, regionTags);
                                    break;
                                case XSharpLexer.DOC_COMMENT:
                                    ScanForRegion(token, iToken, tokens, ref iLastDocComment, snapshot, regionTags);
                                    break;
                                case XSharpLexer.USING:
                                    ScanForRegion(token, iToken, tokens, ref iLastUsing, snapshot, regionTags);
                                    break;
                                default:
                                    break;
                            }
                        }
                    }
                    if (token.Channel != Lexer.Hidden)
                    {
                        lastToken = token;
                    }
                }
                if (currentLine?.Count > 0 && _needsKeywords)
                {
                    kw = XSharpLineKeywords.Tokens2Keyword(currentLine);
                    addKw(kw, iLastLine - 1);
                }

                // Orphan End ?
                _document.SetTokens(lineTokens);
                _document.SetIdentifiers(ids);
                if ((keywordContext != null) && (keywordContext.Type == XSharpLexer.END))
                {
                    newtags.Add(Token2ClassificationSpan(keywordContext, snapshot, xsharpKeywordType));
                    keywordContext = null;
                }
            }
            else
            {
                newtags = _colorTags;
            }
            Logger.Information("-->> XSharpClassifier.BuildColorClassifications()");
            lock (gate)
            {
                _colorTags = newtags;
                _lexerRegions = regionTags;
            }
            Logger.Information("<<-- XSharpClassifier.BuildColorClassifications()");
            Debug("End building Classifications at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            TriggerRepaint(snapshot);
        }

        IToken ScanForLastToken(int type, int start, IList<IToken> tokens, out int iLast)
        {
            var lastFound = tokens[start];
            int iLine = lastFound.Line;
            iLast = start;
            IToken nextToken;
            for (int i = start + 1; i < tokens.Count - 2; i++)
            {
                nextToken = tokens[i];
                IToken nextToken2 = tokens[i + 2];
                if (nextToken.Line > iLine)
                {
                    if (nextToken.Type == type || (nextToken2.Type == type && nextToken.Type == XSharpLexer.STATIC))
                    {
                        lastFound = nextToken;
                        iLine = nextToken.Line;
                        iLast = i;
                    }
                    else if (nextToken.Type != XSharpLexer.WS)
                    {
                        break;
                    }
                }
            }

            for (int i = iLast; i < tokens.Count; i++)
            {
                nextToken = tokens[i];
                if (nextToken.Line == lastFound.Line
                    && nextToken.Type != XSharpLexer.NL
                    && nextToken.Type != XSharpLexer.EOS)
                    lastFound = nextToken;
                else
                    break;
            }
            return lastFound;
        }

        public IList<ClassificationSpan> GetRegionTags()
        {
            Logger.Information($"-->> XSharpClassifier.GetRegionTags()");
            List<ClassificationSpan> result = new List<ClassificationSpan>();
            lock (gate)
            {
                if (_lexerRegions != null)
                {
                    result.AddRange(_lexerRegions);
                }
                if (_parserRegions != null)
                {
                    result.AddRange(_parserRegions);
                }
            }
            Logger.Information($"<<-- XSharpClassifier.GetRegionTags() {result.Count}");
            return result;
        }

        public IList<ClassificationSpan> GetTags()
        {
            Logger.Information("-->> XSharpClassifier.GetTags()");
            IList<ClassificationSpan> ret;
            lock (gate)
            {
                ret = _colorTags.Tags;
            }
            Logger.Information("<<-- XSharpClassifier.GetTags()");
            return ret;
        }



#region IClassifier

#pragma warning disable 67

        /// <summary>
        /// An event that occurs when the classification of a span of text has changed.
        /// </summary>
        /// <remarks>
        /// This event gets raised if a non-text change would affect the classification in some way,
        /// for example typing /* would cause the classification to change in C# without directly
        /// affecting the span.
        /// </remarks>
        public event EventHandler<ClassificationChangedEventArgs> ClassificationChanged;
        public event EventHandler<EventArgs> LineStateChanged;

#pragma warning restore 67

        /// <summary>
        /// Gets all the <see cref="ClassificationSpan"/> objects that intersect with the given range of text.
        /// </summary>
        /// <remarks>
        /// This method scans the given SnapshotSpan for potential matches for this classification.
        /// In this instance, it classifies everything and returns each span as a new ClassificationSpan.
        /// </remarks>
        /// <param name="span">The span currently being classified.</param>
        /// <returns>A list of ClassificationSpans that represent spans identified to be of this classification.</returns>
        public IList<ClassificationSpan> GetClassificationSpans(SnapshotSpan span)
        {
            // Todo:
            // We can probably avoid building all tags in BuildColorClassifications.
            // and directly create the necessary tags here from the List
            // In that case we need to keep a reference to the tokenstream in stead of the tags
            // There also must be a smart way to find the first matching tag.
            var result = new List<ClassificationSpan>();
            var tags = _colorTags;
            if (tags.Count == 0)
                return result;
            int iStart = span.Start.GetContainingLine().LineNumber;
            int iEnd = span.End.GetContainingLine().LineNumber;
            for (int i = iStart; i <= iEnd; i++)
            {
                var tagsForLine = tags.GetItemsForLine(i);
                // Use the Span.Span property to avoid the check for the same Snapshot
                if (tagsForLine != null)
                {
                    result.AddRange(tagsForLine);
                }
            }
            return result;
        }

#endregion

        static internal XSharpClassifier Create(ITextBuffer buffer, IClassificationTypeRegistryService registry, ITextDocumentFactoryService factory)
        {
            XSharpClassifier colorizer = buffer.Properties.GetOrCreateSingletonProperty(
                () => new XSharpClassifier(buffer, registry, factory));
            return colorizer;
        }

        internal static void Debug(string msg, params object[] o)
        {
            Logger.Information(string.Format("XSharpClassifier: " + msg, o));
        }
    }


    internal class XClassificationSpans
    {
        private readonly IList<ClassificationSpan> _tags;
        private readonly object gate = new object();
        private readonly IDictionary<int, List<ClassificationSpan>> _hash;
        private readonly IList<ClassificationSpan> _multilineTokens;
        internal XClassificationSpans()
        {
            lock (gate)
            {
                _tags = new List<ClassificationSpan>();
                _hash = new Dictionary<int, List<ClassificationSpan>>();
                _multilineTokens = new List<ClassificationSpan>();
            }
        }
        internal void Add(ClassificationSpan span)
        {
            lock (gate)
            {
                _tags.Add(span);
                int start = span.Span.Start.GetContainingLine().LineNumber;
                int end = span.Span.End.GetContainingLine().LineNumber;
                if (end > start)
                {
                    _multilineTokens.Add(span);
                }
                else
                {
                    if (!_hash.ContainsKey(start))
                    {
                        _hash.Add(start, new List<ClassificationSpan>());
                    }
                    _hash[start].Add(span);
                }
            }
        }

        internal List<ClassificationSpan> GetItemsForLine(int line)
        {
            lock (gate)
            {
                List<ClassificationSpan> result;
                {
                    if (_hash.ContainsKey(line))
                    {
                        result = _hash[line];
                    }
                    else
                    {
                        result = new List<ClassificationSpan>();
                    }
                }
                if (_multilineTokens.Count > 0)
                {
                    List<ClassificationSpan> multi = null;
                    foreach (var span in _multilineTokens)
                    {

                        if (span.Span.Start.GetContainingLine().LineNumber <= line && span.Span.End.GetContainingLine().LineNumber >= line)
                        {
                            if (multi == null)
                                multi = new List<ClassificationSpan>();
                            multi.Add(span);
                        }
                    }
                    if (multi?.Count > 0)
                    {
                        if (result.Count == 0)
                        {
                            result = multi;
                        }
                        else
                        {
                            multi.AddRange(result);
                            result = multi;
                        }
                    }
                }
                return result;
            }
        }
        internal int Count
        {
            get
            {
                lock (gate)
                {
                    return _tags.Count;
                }
            }
        }
        internal IList<ClassificationSpan> Tags
        {
            get
            {
                lock (gate)
                {
                    var tags = new ClassificationSpan[_tags.Count];
                    _tags.CopyTo(tags, 0);
                    return tags;
                }
            }
        }
    }
}

