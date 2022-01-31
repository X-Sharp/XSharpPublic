﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
//------------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.ComponentModel;
using XSharpModel;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using Microsoft.VisualStudio.Language.StandardClassification;

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
        static private IClassificationType xsharpKwOpenType;
        static private IClassificationType xsharpKwCloseType;
        #endregion

        #region Private Fields
        private readonly object gate = new object();
        private readonly BackgroundWorker _bwClassify = null;
        private readonly BackgroundWorker _bwBuildModel = null;
        private readonly SourceWalker _sourceWalker;
        private readonly ITextBuffer _buffer;

        private XClassificationSpans _colorTags = new XClassificationSpans();
        private IList<ClassificationSpan> _lexerRegions = null;
        private IList<ClassificationSpan> _parserRegions = null;
        private readonly bool _first = true;
        private IToken keywordContext;
        private readonly List<String> xtraKeywords;
        private readonly XSharpLineState lineState;
        #endregion

        #region Properties
        public ITextSnapshot Snapshot
        {
            get
            {
                var xtokens = GetTokens();
                if (xtokens != null)
                    return xtokens.SnapShot;
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

            //
            lineState = new XSharpLineState(buffer.CurrentSnapshot);
            buffer.Properties.AddProperty(typeof(XSharpLineState), lineState);
            xtraKeywords = new List<string>();
            // Initialize our background workers
            _buffer.Changed += Buffer_Changed;
            _bwClassify = new BackgroundWorker();
            _bwClassify.RunWorkerCompleted += ClassifyCompleted;
            _bwClassify.DoWork += DoClassify;

            _bwBuildModel = new BackgroundWorker();
            _bwBuildModel.DoWork += BuildModelDoWork;


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
                xsharpKwOpenType = registry.GetClassificationType(ColorizerConstants.XSharpKwOpenFormat);
                xsharpKwCloseType = registry.GetClassificationType(ColorizerConstants.XSharpKwCloseFormat);
            }
            // Run a synchronous scan to set the initial buffer colors
            var snapshot = buffer.CurrentSnapshot;
            _sourceWalker = new SourceWalker(file);
            ClassifyBuffer(snapshot);
            _first = false;
            // start the model builder to do build a code model and the regions asynchronously
            try
            {
                _bwBuildModel.RunWorkerAsync();
            }
            catch { }

        }
        #region Lexer Methods

        private void Buffer_Changed(object sender, TextContentChangedEventArgs e)
        {
            if (!_bwClassify.IsBusy && !_bwBuildModel.IsBusy)
            {
                try
                {
                    _bwClassify.RunWorkerAsync();
                }
                catch { }
            }

        }

        private XSharpTokens GetTokens()
        {
            XSharpTokens xTokens;
            lock (gate)
            {
                xTokens = _buffer.GetTokens();
            }
            return xTokens;
        }

        public void ClassifyWhenNeeded()
        {
            XSharpTokens xTokens = GetTokens();
            if (xTokens == null || xTokens.SnapShot.Version != _buffer.CurrentSnapshot.Version)
            {
                Classify();
            }
        }
        public void Classify()
        {
            ClassifyBuffer(this._buffer.CurrentSnapshot);
        }
        public void Parse()
        {
            XSharpTokens xTokens = GetTokens();
            if (xTokens.Entities == null)
                ParseEntities();
        }

        private void ClassifyBuffer(ITextSnapshot snapshot)
        {
            if (XSettings.DisableSyntaxHighlighting)
                return;
            XSharpTokens xTokens = GetTokens();

            if (xTokens == null || xTokens.SnapShot.Version != snapshot.Version)
            {
                lineState.Clear();
                Debug("Starting classify at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
                ITokenStream tokens = _sourceWalker.Lex(snapshot.GetText());
                lock (gate)
                {
                    xTokens = new XSharpTokens((BufferedTokenStream)tokens, snapshot, _sourceWalker.IncludeFiles);
                    
                    _buffer.Properties[typeof(XSharpTokens)] = xTokens;
                }
            }
            BuildColorClassifications();
            Debug("Ending classify at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            return;
        }

        private void DoClassify(object sender, DoWorkEventArgs e)
        {
            // Note this runs in the background
            // Wait a little and then get the current snapshot. They may have typed fast or the buffer may have been updated by the formatter
            // wait a second before actually starting to lex the buffer
            if (XSettings.DisableSyntaxHighlighting)
                return;
            System.Threading.Thread.Sleep(1000);
            // and then take the current snapshot because it may have changed in the meantime
            var snapshot = _buffer.CurrentSnapshot;
            ClassifyBuffer(snapshot);
            e.Result = snapshot; // so we know the version of the snapshot that was used in this classifier
        }
        private void TriggerRepaint(ITextSnapshot snapshot)
        {
            if (ClassificationChanged != null)
            {
                XSettings.LogMessage("-->> XSharpClassifier.triggerRepaint()");
                if (snapshot != null && _buffer?.CurrentSnapshot != null)
                {
                    // tell the editor that we have new info
                    if (!_first)
                    {
                        ClassificationChanged(this, new ClassificationChangedEventArgs(
                                new SnapshotSpan(snapshot, Span.FromBounds(0, snapshot.Length))));
                    }
                }
                XSettings.LogMessage("<<-- XSharpClassifier.triggerRepaint()");
            }
        }
        private void ClassifyCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            XSettings.LogMessage("-->> XSharpClassifier.ClassifyCompleted()");
            try
            {
                if (e.Cancelled)
                {
                    return;
                }
                if (e.Error == null)
                {
                    if (e.Result is ITextSnapshot snapshot)
                    {
                        TriggerRepaint(snapshot);
                        // if the buffer has changed in the mean time then restart the classification
                        // because we have 'missed' the buffer_changed event because we were busy
                        var newSnapshot = _buffer.CurrentSnapshot;
                        if (newSnapshot.Version != snapshot.Version)
                        {
                            // buffer was changed, so restart
                            if (!_bwClassify.IsBusy)
                            {
                                _bwClassify.RunWorkerAsync();
                            }
                        }
                        else
                        {
                            TriggerRepaint(snapshot);
                            if (!_bwBuildModel.IsBusy)
                            {
                                _bwBuildModel.RunWorkerAsync();
                            }

                        }
                    }

                }
            }
            catch (Exception ex)
            {
                XSettings.LogException(ex, "XSharpClassifier.ClassifyCompleted()");
            }
            XSettings.LogMessage("<<-- XSharpClassifier.ClassifyCompleted()");
        }

        #endregion

        #region Parser Methods
        private void BuildModelDoWork(object sender, DoWorkEventArgs e)
        {
            ParseEntities();
        }
        private void ParseEntities()
        {
            if (XSettings.DisableEntityParsing)
                return;
            XSettings.LogMessage("-->> XSharpClassifier.ParseEntities()");
            // Note this runs in the background
            // parse for positional keywords that change the colors
            // and get a reference to the tokenstream
            // do we need to create a new tree
            // this happens the first time in the buffer only
            var snapshot = _buffer.CurrentSnapshot;
            var xTokens = GetTokens();
            var tokens = xTokens.TokenStream;
            if (tokens != null)
            {
                Debug("Starting model build  at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
                _sourceWalker.SaveToDisk = true;
                _sourceWalker.ParseTokens(tokens, true, false);
                RegisterEntityBoundaries();
                var regionTags = BuildRegionTags(_sourceWalker.EntityList, _sourceWalker.BlockList, snapshot, xsharpRegionStart, xsharpRegionStop);
                lock (gate)
                {
                    _parserRegions = regionTags.ToArray();
                }
                DoRepaintRegions();
                Debug("Ending model build  at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            }
            XSettings.LogMessage("<<-- XSharpClassifier.ParseEntities()");
        }
        #endregion

        private void RegisterEntityBoundaries()
        {
            // Register the entity boundaries for the line separators
            XSharpTokens xTokens = GetTokens();
            if (xTokens == null)
                return;
            xTokens.Entities = _sourceWalker.EntityList;
            foreach (var entity in _sourceWalker.EntityList)
            {
                var line = entity.Range.StartLine;
                if (entity.StartOfXmlComments > 0 && !string.IsNullOrEmpty(entity.XmlComments))
                    line = entity.StartOfXmlComments;
                if (entity.SingleLine)
                {
                    lineState.SetFlags(line, LineFlags.SingleLineEntity);
                }
                else
                {
                    lineState.SetFlags(line, LineFlags.EntityStart);
                }
            }
            lineState.Snapshot = this.Snapshot;
            if (LineStateChanged != null)
            {
                LineStateChanged(this, new EventArgs());
            }
                
        }
        private void DoRepaintRegions()
        {
            if (_buffer.Properties.ContainsProperty(typeof(XSharpOutliningTagger)))
            {
                var tagger = _buffer.Properties[typeof(XSharpOutliningTagger)] as XSharpOutliningTagger;
                tagger.Update();
            }

        }

        public IList<ClassificationSpan> BuildRegionTags(IList<XSourceEntity> entities, IList<XSourceBlock> blocks, ITextSnapshot snapshot, IClassificationType _, IClassificationType _1)
        {
            if (XSettings.DisableRegions)
            {
                return new List<ClassificationSpan>();
            }
            XSettings.LogMessage("-->> XSharpClassifier.BuildRegionTags()");
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

            foreach (var block in blocks)
            {
                var startPos = block.Token1.StartIndex;
                var endPos = block.Last.Token2.StopIndex;

                AddRegionSpan(regions, snapshot, startPos, endPos);
                if (block.Children.Count > 1)
                {
                    var lastline = block.Token1.Line;
                    foreach (var child in block.Children)
                    {
                        if (child.Token1.Line > lastline+1 && child.Token1.Line >= 2)
                        {
                            // the child is a line with CASE, ELSE, ELSEIF CATCH etc.
                            // we want the last token of the previous like to be the end of the previous block
                            endPos = snapshot.GetLineFromLineNumber(child.Token1.Line - 2).End;
                            AddRegionSpan(regions, snapshot, startPos, endPos);
                        }
                        startPos = child.Token1.StartIndex;
                        lastline = child.Token1.Line;
                    }
                }
            }
            XSettings.LogMessage("<<-- XSharpClassifier.BuildRegionTags()");
            return regions;
        }
        private void AddRegionSpan(List<ClassificationSpan> regions, ITextSnapshot snapshot, int startPos, int endPos)
        {
            try
            {
                TextSpan tokenSpan;
                ClassificationSpan span;
                int nLineLength = snapshot.GetLineFromPosition(startPos).Length;
                tokenSpan = new TextSpan(startPos, nLineLength);
                span = tokenSpan.ToClassificationSpan(snapshot, xsharpRegionStart);
                regions.Add(span);
                endPos = snapshot.GetLineFromPosition(endPos).Start;
                nLineLength = snapshot.GetLineFromPosition(endPos).Length;
                tokenSpan = new TextSpan(endPos, nLineLength);
                span = tokenSpan.ToClassificationSpan(snapshot, xsharpRegionStop);
                regions.Add(span);
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
            XsClassificationSpan span = tokenSpan.ToClassificationSpan(snapshot, type);
            span.startTokenType = start.Type;
            span.endTokenType = stop.Type;
            return span;
        }

        /// <summary>
        /// "Mark" a Token, specifying it's type and the span it covers
        /// </summary>
        /// <param name="token"></param>
        /// <param name="snapshot"></param>
        /// <param name="type"></param>
        /// <returns></returns>
        private ClassificationSpan Token2ClassificationSpan(IToken token, ITextSnapshot snapshot, IClassificationType type)
        {
            TextSpan tokenSpan = new TextSpan(token.StartIndex, token.StopIndex - token.StartIndex + 1);
            XsClassificationSpan span = tokenSpan.ToClassificationSpan(snapshot, type);
            span.startTokenType = token.Type;
            span.endTokenType = -1;
            return span;
        }


        private ClassificationSpan ClassifyToken(IToken token, IList<ClassificationSpan> regionTags, ITextSnapshot snapshot,IToken lastToken)
        {
            var tokenType = token.Type;
            ClassificationSpan result = null;
            switch (token.Channel)
            {
                case XSharpLexer.PREPROCESSORCHANNEL:
                    // #define, #ifdef etc
                    result = Token2ClassificationSpan(token, snapshot, xsharpPPType);
                    switch (token.Type)
                    {
                        case XSharpLexer.PP_REGION:
                            //case XSharpLexer.PP_IFDEF:
                            //case XSharpLexer.PP_IFNDEF:
                            regionTags.Add(Token2ClassificationSpan(token, snapshot, xsharpRegionStart));
                            break;
                        case XSharpLexer.PP_ENDREGION:
                            //case XSharpLexer.PP_ENDIF:
                            regionTags.Add(Token2ClassificationSpan(token, snapshot, xsharpRegionStop));
                            break;
                        default:
                            break;
                    }
                    break;
                case XSharpLexer.DEFOUTCHANNEL:                // code in an inactive #ifdef
                    result = Token2ClassificationSpan(token, snapshot, xsharpInactiveType);
                    lineState.SetFlags(token.Line-1, LineFlags.Inactive);
                    break;
                case XSharpLexer.XMLDOCCHANNEL:
                case XSharpLexer.Hidden:
                    if (token.Type == XSharpLexer.LINE_CONT)
                    {
                        var line = token.Line;
                        lineState.SetFlags(line, LineFlags.Continued);
                    }
                    if (XSharpLexer.IsComment(token.Type))
                    {
                        result = Token2ClassificationSpan(token, snapshot, xsharpCommentType);
                        if (token.Type == XSharpLexer.ML_COMMENT && token.Text.IndexOf("\r") >= 0)
                        {
                            regionTags.Add(Token2ClassificationSpan(token, snapshot, xsharpRegionStart));
                            regionTags.Add(Token2ClassificationSpan(token, snapshot, xsharpRegionStop));
                            var startline = result.Span.Start.GetContainingLine().LineNumber;
                            var endLine = result.Span.End.GetContainingLine().LineNumber;
                            for (int i = startline; i <= endLine; i++)
                            {
                                lineState.SetFlags(i, LineFlags.MultiLineComments);
                            }
                        }
                        else
                        {
                            if (token.Type == XSharpLexer.DOC_COMMENT)
                                lineState.SetFlags(token.Line - 1, LineFlags.DocComments);
                            else
                                lineState.SetFlags(token.Line-1, LineFlags.SingleLineComments);
                        }
                    }
                    break;
                default: // Normal channel
                    IClassificationType type = null;
                    if (XSharpLexer.IsIdentifier(tokenType))
                    {
                        if (xtraKeywords.Find(kw => string.Compare(kw, token.Text, true) == 0) != null)
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


        private List<ClassificationSpan> ClassifyKeyword(IToken token, ITextSnapshot snapshot)
        {
            var tokenType = token.Type;
            var result = new List<ClassificationSpan>();
            IClassificationType type = null;
            IClassificationType type2 = null;
            IToken startToken = null;
            if (keywordContext != null)
            {
                startToken = keywordContext;
                if (startToken.Line != token.Line)
                {
                    keywordContext = null;
                    startToken = null;
                }
            }
            //
            switch (tokenType)
            {
                case XSharpLexer.DO:
                    if (startToken != null)
                    {
                        if (startToken.Type == XSharpLexer.END) // END DO
                        {
                            keywordContext = null;
                            type = xsharpKwCloseType;
                        }
                        else
                            startToken = null;
                    }
                    else
                        keywordContext = token;
                    break;

                case XSharpLexer.BEGIN:     // followed by another keyword
                    keywordContext = token;
                    //type = xsharpKwOpenType;
                    break;

                case XSharpLexer.SWITCH:
                    type = xsharpKwOpenType;
                    if (startToken != null)
                        if (startToken.Type == XSharpLexer.END)     // END SWITCH
                        {
                            type = xsharpKwCloseType;
                            keywordContext = null;
                        }
                        else if ((startToken.Type != XSharpLexer.DO) || (startToken.Type != XSharpLexer.BEGIN))  // DO SWITCH or BEGIN SWITCH are also allowed
                            startToken = null;
                    break;

                case XSharpLexer.TRY:
                case XSharpLexer.IF:
                    type = xsharpKwOpenType;
                    if (startToken != null)
                        if (startToken.Type == XSharpLexer.END)         // END TRY or END IF
                        {
                            type = xsharpKwCloseType;
                            keywordContext = null;
                        }
                        else
                            startToken = null;
                    break;

                case XSharpLexer.WHILE:
                    type = xsharpKwOpenType;
                    if (startToken != null)
                        if (startToken.Type == XSharpLexer.END) // END WHILE
                            type = xsharpKwCloseType;
                        else if (startToken.Type != XSharpLexer.DO)
                            startToken = null;
                    break;


                case XSharpLexer.CASE:
                    if (startToken != null)
                    {
                        if (startToken.Type == XSharpLexer.DO)  // DO CASE
                            type = xsharpKwOpenType;
                        else if (startToken.Type == XSharpLexer.END) // END CASE
                        {
                            type = xsharpKwCloseType;
                            keywordContext = null;
                        }
                    }
                    else
                    {
                        type = xsharpKwCloseType;       // CASE inside, so close and open
                        type2 = xsharpKwOpenType;
                    }
                    break;

                case XSharpLexer.FOR:
                case XSharpLexer.FOREACH:
                case XSharpLexer.REPEAT:
                case XSharpLexer.TEXT:
                    startToken = null;
                    type = xsharpKwOpenType;            // Simple open
                    break;

                case XSharpLexer.NEXT:
                case XSharpLexer.UNTIL:
                case XSharpLexer.ENDDO:
                case XSharpLexer.ENDIF:
                case XSharpLexer.ENDCASE:
                case XSharpLexer.ENDTEXT:
                case XSharpLexer.ENDDEFINE:             // FoxPro end of class definition
                case XSharpLexer.ENDCLASS:              // XPP end of class definition
                    startToken = null;
                    type = xsharpKwCloseType;           // Simple close
                    break;

                case XSharpLexer.END:                   // followed by other keyword
                    keywordContext = token;
                    //type = xsharpKwCloseType;
                    break;
                case XSharpLexer.ELSE:
                case XSharpLexer.ELSEIF:
                case XSharpLexer.OTHERWISE:
                case XSharpLexer.RECOVER:
                case XSharpLexer.CATCH:
                case XSharpLexer.FINALLY:
                    startToken = null;                  // inside other block, so close and open
                    type = xsharpKwCloseType;
                    type2 = xsharpKwOpenType;
                    break;

                // begin .. end
                case XSharpLexer.SEQUENCE:
                case XSharpLexer.NAMESPACE:
                case XSharpLexer.LOCK:
                case XSharpLexer.SCOPE:
                case XSharpLexer.FIXED:
                case XSharpLexer.UNSAFE:
                case XSharpLexer.USING:
                case XSharpLexer.CHECKED:
                case XSharpLexer.UNCHECKED:
                    if (startToken != null)
                    {
                        if (startToken.Type == XSharpLexer.BEGIN)           // prefixed by BEGIN
                        {
                            type = xsharpKwOpenType;
                            keywordContext = null;
                        }
                        else if (startToken.Type == XSharpLexer.END)        // prefixed by END
                        {
                            type = xsharpKwCloseType;
                            keywordContext = null;
                        }
                    }
                    break;
                case XSharpLexer.PROPERTY:
                case XSharpLexer.SET:
                case XSharpLexer.INIT:
                case XSharpLexer.GET:
                case XSharpLexer.ADD:
                case XSharpLexer.REMOVE:
                    type = xsharpKwOpenType;
                    if (startToken != null && startToken.Type == XSharpLexer.END)
                    {
                        keywordContext = null;
                        type = xsharpKwCloseType;
                    }
                    break;
                // some entities also have an END marker
                case XSharpLexer.CLASS:
                case XSharpLexer.INTERFACE:
                case XSharpLexer.STRUCTURE:
                case XSharpLexer.ENUM:
                    type = xsharpKwOpenType;
                    if (startToken != null && startToken.Type == XSharpLexer.END)
                    {
                        type = xsharpKwCloseType;
                        keywordContext = null;
                    }
                    break;

            }
            //
            if (type != null)
            {
                if (startToken != null)
                    result.Add(Token2ClassificationSpan(startToken, token, snapshot, type));
                else
                    result.Add(Token2ClassificationSpan(token, snapshot, type));
            }
            if (type2 != null)
            {
                if (startToken != null)
                    result.Add(Token2ClassificationSpan(startToken, token, snapshot, type2));
                else
                    result.Add(Token2ClassificationSpan(token, snapshot, type2));
            }
            return result;
        }



        private void ScanForRegion(IToken token, int iToken, ITokenStream TokenStream,
            ref int iLast, ITextSnapshot snapshot, IList<ClassificationSpan> regionTags)
        {
            if (iToken > iLast)
            {
                var lastToken = ScanForLastToken(token.Type, iToken, TokenStream, out iLast);
                if (token != lastToken)
                {
                    regionTags.Add(Token2ClassificationSpan(token, snapshot, xsharpRegionStart));
                    regionTags.Add(Token2ClassificationSpan(lastToken, snapshot, xsharpRegionStop));
                }
            }
        }

        private void BuildColorClassifications()
        {
            var tokens = GetTokens();
            if (tokens == null)
                return;
            var tokenStream = tokens.TokenStream;
            var snapshot = tokens.SnapShot;

            Debug("Start building Classifications at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            XClassificationSpans newtags;
            var lines = new Dictionary<int, IList<XSharpToken>> (snapshot.LineCount);
            var regionTags = new List<ClassificationSpan>();
            if (tokenStream != null)
            {
                int iLastInclude = -1;
                int iLastPPDefine = -1;
                int iLastDefine = -1;
                int iLastSLComment = -1;
                int iLastDocComment = -1;
                int iLastUsing = -1;
                newtags = new XClassificationSpans();
                //texttags = new XClassificationSpans();
                keywordContext = null;
                IToken lastToken = null;
                for (var iToken = 0; iToken < tokenStream.Size; iToken++)
                {
                    var token = (XSharpToken) tokenStream.Get(iToken);
                    // store the tokens per line in a dictionary so we can quickly look them up
                    if ( (token.Channel == XSharpLexer.DefaultTokenChannel) ||( token.Channel == XSharpLexer.DEFOUTCHANNEL ) )
                    {
                        var line = token.Line - 1;  // VS Has 0 based line numbers
                        if (!lines.ContainsKey(line))
                        {
                            lines.Add(line, new List<XSharpToken>());
                        }
                        lines[line].Add(token);
                    }
                    // Orphan End ?
                    if ((keywordContext != null) && (keywordContext.Line != token.Line) && (keywordContext.Type == XSharpLexer.END))
                    {
                        newtags.Add(Token2ClassificationSpan(keywordContext, snapshot, xsharpKwCloseType));
                        keywordContext = null;
                    }
                    if (token.Type == XSharpParser.PRAGMA)
                    {
                        var start = token;
                        var stop = token;
                        while (true)
                        {
                            iToken++;
                            token = (XSharpToken) tokenStream.Get(iToken);
                            if (token.Type == XSharpParser.EOS || token.Type == XSharpParser.Eof)
                                break;
                            stop = token;
                        }
                        TextSpan tokenSpan = new TextSpan(start.StartIndex, stop.StopIndex - start.StartIndex + 1);
                        XsClassificationSpan span1 = tokenSpan.ToClassificationSpan(snapshot, xsharpPPType);
                        span1.startTokenType = start.Type;
                        span1.endTokenType = stop.Type;
                        newtags.Add(span1);
                        iToken--;
                        continue;
                    }

                    var span = ClassifyToken(token, regionTags, snapshot,lastToken);
                    if ((span != null) )
                    {
                        // don't forget the current one
                        newtags.Add(span);
                        // We can have some Open/Close keyword ( FOR..NEXT; WHILE...ENDDO; IF...ENDIF)
                        if (span.ClassificationType == xsharpKeywordType)
                        {
                            var list = ClassifyKeyword(token, snapshot);
                            foreach (var item in list)
                                newtags.Add(item);
                        }

                        if (!XSettings.DisableRegions)
                        {
                            // now look for Regions of similar code lines
                            switch (token.Type)
                            {
                                case XSharpLexer.PP_INCLUDE:
                                    ScanForRegion(token, iToken, tokenStream, ref iLastInclude, snapshot, regionTags);
                                    break;
                                case XSharpLexer.PP_DEFINE:
                                    ScanForRegion(token, iToken, tokenStream, ref iLastPPDefine, snapshot, regionTags);
                                    break;
                                case XSharpLexer.DEFINE:
                                    ScanForRegion(token, iToken, tokenStream, ref iLastDefine, snapshot, regionTags);
                                    break;
                                case XSharpLexer.SL_COMMENT:
                                    ScanForRegion(token, iToken, tokenStream, ref iLastSLComment, snapshot, regionTags);
                                    break;
                                case XSharpLexer.DOC_COMMENT:
                                    ScanForRegion(token, iToken, tokenStream, ref iLastDocComment, snapshot, regionTags);
                                    break;
                                case XSharpLexer.USING:
                                    ScanForRegion(token, iToken, tokenStream, ref iLastUsing, snapshot, regionTags);
                                    break;
                                default:
                                    break;
                            }
                        }
                    }
                    if (token.Channel != XSharpLexer.Hidden)
                    {
                        lastToken = token;
                    }
                }
                // Orphan End ?
                tokens.Lines = lines;
                if ((keywordContext != null) && (keywordContext.Type == XSharpLexer.END))
                {
                    newtags.Add(Token2ClassificationSpan(keywordContext, snapshot, xsharpKwCloseType));
                    keywordContext = null;
                }
            }
            else
            {
                newtags = _colorTags;
            }
            XSettings.LogMessage("-->> XSharpClassifier.BuildColorClassifications()");
            lock (gate)
            {
                _colorTags = newtags;
                _lexerRegions = regionTags;
            }
            XSettings.LogMessage("<<-- XSharpClassifier.BuildColorClassifications()");
            Debug("End building Classifications at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            TriggerRepaint(snapshot);
        }

        IToken ScanForLastToken(int type, int start, ITokenStream TokenStream, out int iLast)
        {
            var lastFound = TokenStream.Get(start);
            int iLine = lastFound.Line;
            iLast = start;
            IToken nextToken;
            for (int i = start + 1; i < TokenStream.Size - 2; i++)
            {
                nextToken = TokenStream.Get(i);
                IToken nextToken2 = TokenStream.Get(i + 2);
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

            for (int i = iLast; i < TokenStream.Size; i++)
            {
                nextToken = TokenStream.Get(i);
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
            XSettings.LogMessage("-->> XSharpClassifier.GetRegionTags()");
            IList<ClassificationSpan> result;
            lock (gate)
            {
                if (_parserRegions != null)
                {
                    var list = _parserRegions.ToList();
                    if (_lexerRegions != null)
                        list.AddRange(_lexerRegions);
                    result = list;
                }
                else if (_lexerRegions != null)
                {
                    result = _lexerRegions;
                }
                else
                {
                    result = new List<ClassificationSpan>();
                }
            }
            XSettings.LogMessage("<<-- XSharpClassifier.GetRegionTags()");
            return result;
        }

        public IList<ClassificationSpan> GetTags()
        {
            XSettings.LogMessage("-->> XSharpClassifier.GetTags()");
            IList<ClassificationSpan> ret;
            lock (gate)
            {
                ret = _colorTags.Tags;
            }
            XSettings.LogMessage("<<-- XSharpClassifier.GetTags()");
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
            // and directly create the necessary tags here from the List<XSharpToken>
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

        static internal XSharpClassifier GetColorizer(ITextBuffer buffer, IClassificationTypeRegistryService registry, ITextDocumentFactoryService factory)
        {
            XSharpClassifier colorizer = buffer.Properties.GetOrCreateSingletonProperty(
                () => new XSharpClassifier(buffer, registry, factory));
            return colorizer;
        }

        internal static void Debug(string msg, params object[] o)
        {
            XSettings.LogMessage(string.Format("XSharpClassifier: " + msg, o));
        }
    }


    [DebuggerDisplay("{Span} {ClassificationType.Classification,nq} ")]
    public class XsClassificationSpan : ClassificationSpan
    {

        public int startTokenType;
        public int endTokenType;

        public XsClassificationSpan(SnapshotSpan span, IClassificationType classification) : base(span, classification)
        {
            startTokenType = -1;
            endTokenType = -1;
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
                if (end > start + 1)
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

