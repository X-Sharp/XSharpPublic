//
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
using System.Linq;
using System.Threading;
namespace XSharpColorizer
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
        static private IClassificationType xsharpPunctuationType;
        static private IClassificationType xsharpStringType;
        static private IClassificationType xsharpNumberType;
        static private IClassificationType xsharpPPType;
        static private IClassificationType xsharpBraceOpenType;
        static private IClassificationType xsharpBraceCloseType;
        static private IClassificationType xsharpRegionStart;
        static private IClassificationType xsharpRegionStop;
        static private IClassificationType xsharpInactiveType;
        static private IClassificationType xsharpLiteralType;
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
        private ITextDocumentFactoryService _txtdocfactory;
        private bool _first = true;
        private XSharpModel.ParseResult _info = null;
        private IToken keywordContext;
        private ITokenStream _tokens;
        private ITextSnapshot _snapshot;
        private XFile _file;
        #endregion

        #region Properties
        public ITextSnapshot Snapshot => _snapshot;

        private bool disableEntityParsing => _file.Project.ProjectNode.DisableParsing;
        private bool disableSyntaxHighlighting => _file.Project.ProjectNode.DisableLexing;
        private bool disableRegions => _file.Project.ProjectNode.DisableRegions;
        #endregion


        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpClassifier"/> class.
        /// </summary>
        /// <param name="registry">Classification registry.</param>

        internal XSharpClassifier(ITextBuffer buffer, IClassificationTypeRegistryService registry, ITextDocumentFactoryService factory)
        {
            XFile file = null;
            this._buffer = buffer;
            if (buffer.Properties.ContainsProperty(typeof(XFile)))
            {
                file = buffer.GetFile();
                if (file == null)
                {
                    return;
                }
            }
            _file = file;
            // Initialize our background workers
            this._buffer.Changed += Buffer_Changed;
            _bwClassify = new BackgroundWorker();
            _bwClassify.RunWorkerCompleted += ClassifyCompleted;
            _bwClassify.DoWork += DoClassify;

            _bwBuildModel = new BackgroundWorker();
            _bwBuildModel.DoWork += BuildModelDoWork;

            _txtdocfactory = factory;
            if (xsharpKeywordType == null)
            {
                // These fields are static so only initialize the first time
                xsharpKeywordType = registry.GetClassificationType("keyword");
                xsharpIdentifierType = registry.GetClassificationType("identifier");
                xsharpCommentType = registry.GetClassificationType("comment");
                xsharpOperatorType = registry.GetClassificationType("operator");
                xsharpPunctuationType = registry.GetClassificationType("punctuation");
                xsharpPPType = registry.GetClassificationType("preprocessor keyword");
                xsharpNumberType = registry.GetClassificationType("number");
                xsharpStringType = registry.GetClassificationType("string");
                xsharpInactiveType = registry.GetClassificationType("excluded code");
                xsharpBraceOpenType = registry.GetClassificationType("punctuation");
                xsharpBraceCloseType = registry.GetClassificationType("punctuation");
                xsharpLiteralType = registry.GetClassificationType("literal");
                xsharpRegionStart = registry.GetClassificationType(ColorizerConstants.XSharpRegionStartFormat);
                xsharpRegionStop = registry.GetClassificationType(ColorizerConstants.XSharpRegionStopFormat);
                xsharpKwOpenType = registry.GetClassificationType(ColorizerConstants.XSharpBraceOpenFormat);
                xsharpKwCloseType = registry.GetClassificationType(ColorizerConstants.XSharpBraceCloseFormat);
            }
            // Run a synchronous scan to set the initial buffer colors
            _snapshot = buffer.CurrentSnapshot;
            _sourceWalker = new SourceWalker(file);
            ClassifyBuffer(_snapshot);
            _first = false;
            // start the model builder to do build a code model and the regions asynchronously
            try
            {
                _bwBuildModel.RunWorkerAsync();
            }
            catch  { }

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
                catch  { }
            }

        }

        private void ClassifyBuffer(ITextSnapshot snapshot)
        {
            if (disableSyntaxHighlighting)
                return;
            Debug("Starting classify at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            ITokenStream tokens;
            tokens = _sourceWalker.Lex(snapshot.GetText());
            lock (gate)
            {
                _snapshot = snapshot;
                _tokens = tokens;
                XSharpTokens xTokens = new XSharpTokens((BufferedTokenStream)tokens, snapshot);
                if (_buffer.Properties.ContainsProperty(typeof(XSharpTokens)))
                {
                    _buffer.Properties.RemoveProperty(typeof(XSharpTokens));
                }
                _buffer.Properties.AddProperty(typeof(XSharpTokens), xTokens);

            }
            BuildColorClassifications(tokens, snapshot);
            Debug("Ending classify at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            return;
        }

        private void DoClassify(object sender, DoWorkEventArgs e)
        {
            // Note this runs in the background
            // Wait a little and then get the current snapshot. They may have typed fast or the buffer may have been updated by the formatter
            // wait a second before actually starting to lex the buffer
            if (disableSyntaxHighlighting)
                return;
            System.Threading.Thread.Sleep(1000);
            // and then take the current snapshot because it may have changed in the meantime
            var snapshot = _buffer.CurrentSnapshot;
            ClassifyBuffer(snapshot);
            e.Result = snapshot; // so we know the version of the snapshot that was used in this classifier
        }
        private void triggerRepaint(ITextSnapshot snapshot)
        {
            if (ClassificationChanged != null)
            {
                System.Diagnostics.Trace.WriteLine("-->> XSharpClassifier.triggerRepaint()");
                if (snapshot != null && _buffer?.CurrentSnapshot != null)
                {
                    // tell the editor that we have new info
                    if (!_first)
                    {
                        ClassificationChanged(this, new ClassificationChangedEventArgs(
                                new SnapshotSpan(snapshot, Span.FromBounds(0, snapshot.Length))));
                    }
                }
                System.Diagnostics.Trace.WriteLine("<<-- XSharpClassifier.triggerRepaint()");
            }
        }
        private void ClassifyCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            System.Diagnostics.Trace.WriteLine("-->> XSharpClassifier.ClassifyCompleted()");
            if (e.Cancelled)
            {
                return;
            }
            if (e.Error == null)
            {
                var snapshot = e.Result as ITextSnapshot;
                if (snapshot != null)
                {
                    triggerRepaint(snapshot);
                    // if the buffer has changed in the mean time then restart the classification
                    // because we have 'missed' the buffer_changed event because we were busy
                    var newSnapshot = _buffer.CurrentSnapshot;
                    if (newSnapshot.Version != snapshot.Version)
                    {
                        // buffer was changed, so restart
                        try
                        {
                            _bwClassify.RunWorkerAsync();
                        }
                        catch
                        {

                        }
                    }
                    else
                    {
                        triggerRepaint(snapshot);
                        if (!_bwBuildModel.IsBusy)
                        {
                            _bwBuildModel.RunWorkerAsync();
                        }
                    }
                }
            }
            System.Diagnostics.Trace.WriteLine("<<-- XSharpClassifier.ClassifyCompleted()");
        }

        #endregion

        #region Parser Methods
        private void BuildModelDoWork(object sender, DoWorkEventArgs e)
        {
            if (disableEntityParsing)
                return;
            System.Diagnostics.Trace.WriteLine("-->> XSharpClassifier.BuildModelDoWork()");
            // Note this runs in the background
            // parse for positional keywords that change the colors
            // and get a reference to the tokenstream
            // do we need to create a new tree
            // this happens the first time in the buffer only
            var snapshot = _buffer.CurrentSnapshot;
            ITokenStream tokens = null;
            ParseResult info;
            {
                Debug("Starting parse at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
                var lines = new List<String>();
                foreach (var line in snapshot.Lines)
                {
                    lines.Add(line.GetText());
                }

                info = _sourceWalker.Parse(lines, false);
                lock (gate)
                {
                    _info = info;
                }
                Debug("Ending parse at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            }
            tokens = _tokens;
            if (info != null && tokens != null)
            {
                Debug("Starting model build  at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
                _sourceWalker.BuildModel(info);
                var regionTags = BuildRegionTags(info, snapshot, xsharpRegionStart, xsharpRegionStop);
                lock (gate)
                {
                    _parserRegions = regionTags.ToArray();
                }
                DoRepaintRegions();
                Debug("Ending model build  at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            }
            System.Diagnostics.Trace.WriteLine("<<-- XSharpClassifier.BuildModelDoWork()");
        }
        #endregion

        private void DoRepaintRegions()
        {
            if (_buffer.Properties.ContainsProperty(typeof(XSharpOutliningTagger)))
            {
                var tagger = _buffer.Properties[typeof(XSharpOutliningTagger)] as XSharpOutliningTagger;
                tagger.Update();
            }

        }
        public IList<ClassificationSpan> BuildRegionTags(XSharpModel.ParseResult info, ITextSnapshot snapshot, IClassificationType start, IClassificationType stop)
        {
            if (disableRegions)
            {
                return new List<ClassificationSpan>();
            }
            System.Diagnostics.Trace.WriteLine("-->> XSharpClassifier.BuildRegionTags()");
            var regions = new List<ClassificationSpan>();
            var classList = new List<EntityObject>();
            var propertyList = new List<EntityObject>();
            if (info != null && snapshot != null)
            {

                // walk list of entities
                foreach (var oElement in info.Entities)
                {
                    if (oElement.eType.NeedsEndKeyword())
                    {
                        classList.Add(oElement);
                    }
                    if (oElement.eType == EntityType._Property)
                    {
                        if (oElement.oParent?.eType != EntityType._Interface)
                        {
                            // make sure we only push multi line properties
                            var text = snapshot.GetLineFromPosition(oElement.nOffSet).GetText();
                            var substatements = text.Split(new char[] { ';' }, StringSplitOptions.RemoveEmptyEntries);
                            var words = substatements[0].Split(new char[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
                            var singleLine = false;
                            foreach (var word in words)
                            {
                                switch (word.ToLower())
                                {
                                    case "auto":
                                    case "get":
                                    case "set":
                                        singleLine = true;
                                        break;
                                    default:
                                        break;
                                }
                            }
                            if (!singleLine)
                            {
                                propertyList.Add(oElement);
                            }
                        }
                    }
                    //else if (oElement.eType == EntityType._Property)
                    //{
                    //    classStack.Push(oElement);
                    //}
                    else if (oElement.eType.HasBody()
                            || oElement.eType == EntityType._VOStruct
                            || oElement.eType == EntityType._Union)
                    {
                        int nStart, nEnd;
                        nStart = oElement.nOffSet;
                        nEnd = oElement.nOffSet;
                        var oNext = oElement.oNext;
                        if (oElement.eType == EntityType._VOStruct
                            || oElement.eType == EntityType._Union)
                        {
                            while (oNext != null && oNext.eType == EntityType._Field)
                            {
                                oNext = oNext.oNext;
                            }
                        }
                        if (oNext != null)
                        {
                            var nLine = oNext.nStartLine;
                            // our lines are 1 based and we want the line before, so -2
                            nEnd = snapshot.GetLineFromLineNumber(nLine - 2).Start;
                        }
                        else
                        {
                            if (oElement.oParent.cName != XElement.GlobalName)
                            {
                                // find the endclass line after this element
                                foreach (var oLine in info.SpecialLines)
                                {
                                    if (oLine.eType == LineType.EndClass && oLine.Line > oElement.nStartLine)
                                    {
                                        var nLine = oLine.Line;
                                        // our lines are 1 based and we want the line before, so -2
                                        nEnd = snapshot.GetLineFromLineNumber(nLine - 2).Start;
                                        break;
                                    }
                                }
                            }
                            if (nEnd == nStart)
                            {
                                var nEndLine = snapshot.LineCount;
                                // walk the special lines collection to see if there are any 'end' lines at the end of the file
                                for (int i = info.SpecialLines.Count - 1; i >= 0; i--)
                                {
                                    var oLine = info.SpecialLines[i];
                                    switch (oLine.eType)
                                    {
                                        case LineType.EndNamespace:
                                        case LineType.EndClass:
                                            nEndLine = oLine.Line - 1;
                                            break;
                                        default:
                                            i = -1; // exit the loop
                                            break;
                                    }
                                }
                                // Our lines are 1 based, so subtract 1
                                nEnd = snapshot.GetLineFromLineNumber(nEndLine - 1).Start;
                            }
                        }
                        if (nEnd > snapshot.Length)
                        {
                            nEnd = snapshot.Length;
                        }
                        AddRegionSpan(regions, snapshot, nStart, nEnd);

                    }

                }

            }
            var blockStack = new Stack<LineObject>();
            var nsStack = new Stack<LineObject>();
            foreach (var oLine in info.SpecialLines)
            {
                int nStart = 0, nEnd = 0;
                switch (oLine.eType)
                {
                    case LineType.BeginNamespace:
                        nsStack.Push(oLine);
                        break;
                    case LineType.EndNamespace:
                        if (nsStack.Count > 0)
                        {
                            var nsStart = nsStack.Pop();
                            nStart = nsStart.OffSet;
                            nEnd = oLine.OffSet;
                            AddRegionSpan(regions, snapshot, nStart, nEnd);
                        }
                        break;
                    case LineType.EndClass:
                        if (classList.Count > 0)
                        {
                            var cls = classList[0];
                            classList.RemoveAt(0);
                            nStart = cls.nOffSet;
                            nEnd = oLine.OffSet;
                            AddRegionSpan(regions, snapshot, nStart, nEnd);
                        }
                        break;
                    case LineType.EndProperty:
                        if (propertyList.Count > 0)
                        {
                            var prop = propertyList[0];
                            propertyList.RemoveAt(0);
                            nStart = prop.nOffSet;
                            nEnd = oLine.OffSet;
                            AddRegionSpan(regions, snapshot, nStart, nEnd);
                        }
                        break;
                    case LineType.TokenIn:
                        blockStack.Push(oLine);
                        break;
                    case LineType.TokenInOut:
                        if (blockStack.Count > 0)
                        {
                            var blStart = blockStack.Peek();
                            // remove previous ELSEIF but not the IF
                            if (blStart.eType == LineType.TokenInOut)
                            {
                                blockStack.Pop();
                                //}
                                if (blStart.cArgument == "DO" || blStart.cArgument == "SWITCH")
                                {
                                    // no contents before the first case
                                    ;
                                }
                                else
                                {
                                    nStart = blStart.OffSet;
                                    // our lines are 1 based.
                                    // we do not want to include the next case line in the block from the previous one
                                    nEnd = snapshot.GetLineFromLineNumber(oLine.Line - 2).Start;
                                    AddRegionSpan(regions, snapshot, nStart, nEnd);
                                }
                            }
                        }
                        blockStack.Push(oLine);
                        break;
                    case LineType.TokenOut:
                        if (blockStack.Count > 0)
                        {
                            // bop back to first token of the if .. endif or do case .. endcase
                            while (blockStack.Count > 0 && blockStack.Peek().eType == LineType.TokenInOut)
                            {
                                var blStart = blockStack.Pop();
                                nStart = blStart.OffSet;
                                nEnd = snapshot.GetLineFromLineNumber(oLine.Line - 2).Start;
                                AddRegionSpan(regions, snapshot, nStart, nEnd);
                            }
                            if (blockStack.Count > 0)
                            {
                                var blStart = blockStack.Pop();
                                nStart = blStart.OffSet;
                                // get position of the line based on the line number
                                nEnd = snapshot.GetLineFromLineNumber(oLine.Line - 1).Start;
                                AddRegionSpan(regions, snapshot, nStart, nEnd);
                            }
                        }
                        break;
                    default:
                        break;
                }
            }
            System.Diagnostics.Trace.WriteLine("<<-- XSharpClassifier.BuildRegionTags()");
            return regions;
        }
        private void AddRegionSpan(List<ClassificationSpan> regions, ITextSnapshot snapshot, int nStart, int nEnd)
        {
            try
            {
                TextSpan tokenSpan;
                ClassificationSpan span;
                int nLineLength = snapshot.GetLineFromPosition(nStart).Length;
                tokenSpan = new TextSpan(nStart, nLineLength);
                span = tokenSpan.ToClassificationSpan(snapshot, xsharpRegionStart);
                regions.Add(span);
                nLineLength = snapshot.GetLineFromPosition(nEnd).Length;
                if (nEnd + nLineLength >= snapshot.Length)
                {
                    nLineLength = snapshot.Length - nEnd - 1;
                    if (nLineLength < 0)
                        nLineLength = 0;
                }
                tokenSpan = new TextSpan(nEnd, nLineLength);
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
        private ClassificationSpan Token2ClassificationSpan(IToken token, ITextSnapshot snapshot, IClassificationType type)
        {
            TextSpan tokenSpan = new TextSpan(token.StartIndex, token.StopIndex - token.StartIndex + 1);
            ClassificationSpan span = tokenSpan.ToClassificationSpan(snapshot, type);
            return span;
        }


        private ClassificationSpan ClassifyToken(IToken token, IList<ClassificationSpan> regionTags, ITextSnapshot snapshot)
        {
            var tokenType = token.Type;
            ClassificationSpan result = null;
            switch (token.Channel)
            {
                case XSharpLexer.PRAGMACHANNEL:         // #pragma
                case XSharpLexer.PREPROCESSORCHANNEL:
                    // #define, #ifdef etc
                    result = Token2ClassificationSpan(token, snapshot, xsharpPPType);
                    switch (token.Type)
                    {
                        case XSharpLexer.PP_REGION:
                        case XSharpLexer.PP_IFDEF:
                        case XSharpLexer.PP_IFNDEF:
                            regionTags.Add(Token2ClassificationSpan(token, snapshot, xsharpRegionStart));
                            break;
                        case XSharpLexer.PP_ENDREGION:
                        case XSharpLexer.PP_ENDIF:
                            regionTags.Add(Token2ClassificationSpan(token, snapshot, xsharpRegionStop));
                            break;
                        default:
                            break;
                    }
                    break;
                case XSharpLexer.DEFOUTCHANNEL:                // code in an inactive #ifdef
                    result = Token2ClassificationSpan(token, snapshot, xsharpInactiveType);
                    break;
                case XSharpLexer.XMLDOCCHANNEL:
                case XSharpLexer.Hidden:
                    if (XSharpLexer.IsComment(token.Type))
                    {
                        result = Token2ClassificationSpan(token, snapshot, xsharpCommentType);
                        if (token.Type == XSharpLexer.ML_COMMENT && token.Text.IndexOf("\r") >= 0)
                        {
                            regionTags.Add(Token2ClassificationSpan(token, snapshot, xsharpRegionStart));
                            regionTags.Add(Token2ClassificationSpan(token, snapshot, xsharpRegionStop));
                        }
                    }
                    break;
                default: // Normal channel
                    IClassificationType type = null;
                    if (XSharpLexer.IsIdentifier(tokenType))
                    {
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
                                type = xsharpStringType;
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
                        switch (tokenType)
                        {
                            case XSharpLexer.LPAREN:
                            case XSharpLexer.LCURLY:
                            case XSharpLexer.LBRKT:
                                type = xsharpBraceOpenType;
                                break;

                            case XSharpLexer.RPAREN:
                            case XSharpLexer.RCURLY:
                            case XSharpLexer.RBRKT:
                                type = xsharpBraceCloseType;
                                break;
                            default:
                                type = xsharpOperatorType;
                                break;
                        }
                    }
                    if (type != null)
                    {
                        result = Token2ClassificationSpan(token, snapshot, type);
                    }
                    break;
            }
            return result;
        }


        private ClassificationSpan ClassifyKeyword(IToken token, ITextSnapshot snapshot)
        {
            var tokenType = token.Type;
            ClassificationSpan result = null;
            IClassificationType type = null;
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
                        if (startToken.Type == XSharpLexer.END)
                            type = xsharpKwCloseType;
                        else
                            startToken = null;
                    }
                    else
                        keywordContext = token;
                    break;

                case XSharpLexer.BEGIN:
                    keywordContext = token;
                    //type = xsharpKwOpenType;
                    break;

                case XSharpLexer.SWITCH:
                    type = xsharpKwOpenType;
                    if (startToken != null)
                        if (startToken.Type == XSharpLexer.END)
                            type = xsharpKwCloseType;
                        else if ((startToken.Type != XSharpLexer.DO) || (startToken.Type != XSharpLexer.BEGIN))
                            startToken = null;
                    break;

                case XSharpLexer.TRY:
                case XSharpLexer.IF:
                    type = xsharpKwOpenType;
                    if (startToken != null)
                        if (startToken.Type == XSharpLexer.END)
                            type = xsharpKwCloseType;
                        else
                            startToken = null;
                    break;

                case XSharpLexer.WHILE:
                    type = xsharpKwOpenType;
                    if (startToken != null)
                        if (startToken.Type == XSharpLexer.END)
                            type = xsharpKwCloseType;
                        else if (startToken.Type != XSharpLexer.DO)
                            startToken = null;
                    break;


                case XSharpLexer.CASE:
                    if (startToken != null)
                        if (startToken.Type == XSharpLexer.DO)
                            type = xsharpKwOpenType;
                        else if (startToken.Type == XSharpLexer.END)
                            type = xsharpKwCloseType;
                    break;


                case XSharpLexer.FOR:
                case XSharpLexer.FOREACH:
                case XSharpLexer.REPEAT:
                    startToken = null;
                    type = xsharpKwOpenType;
                    break;

                case XSharpLexer.NEXT:
                case XSharpLexer.UNTIL:
                case XSharpLexer.ENDDO:
                case XSharpLexer.ENDIF:
                case XSharpLexer.ENDCASE:
                    startToken = null;
                    type = xsharpKwCloseType;
                    break;

                case XSharpLexer.END:
                    keywordContext = token;
                    //type = xsharpKwCloseType;
                    break;

            }
            //
            if (type != null)
            {
                if (startToken != null)
                    result = Token2ClassificationSpan(startToken, token, snapshot, type);
                else
                    result = Token2ClassificationSpan(token, snapshot, type);
            }
            return result;
        }



        private void scanForRegion(IToken token, int iToken, ITokenStream TokenStream,
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

        private void BuildColorClassifications(ITokenStream tokenStream, ITextSnapshot snapshot)
        {
            Debug("Start building Classifications at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            XClassificationSpans newtags;
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
                keywordContext = null;
                for (var iToken = 0; iToken < tokenStream.Size; iToken++)
                {
                    var token = tokenStream.Get(iToken);
                    // Orphan End ?
                    if ((keywordContext != null) && (keywordContext.Line != token.Line) && (keywordContext.Type == XSharpLexer.END))
                    {
                        newtags.Add(Token2ClassificationSpan(keywordContext, snapshot, xsharpKwCloseType));
                        keywordContext = null;
                    }
                    var span = ClassifyToken(token, regionTags, snapshot);
                    if (span != null)
                    {
                        newtags.Add(span);
                        // We can have some Open/Close keyword ( FOR..NEXT; WHILE...ENDDO; IF...ENDIF)
                        if (span.ClassificationType == xsharpKeywordType)
                        {
                            span = ClassifyKeyword(token, snapshot);
                            if (span != null)
                            {
                                newtags.Add(span);
                            }
                        }
                        if (!disableRegions)
                        {
                            // now look for Regions of similar code lines
                            switch (token.Type)
                            {
                                case XSharpLexer.PP_INCLUDE:
                                    scanForRegion(token, iToken, tokenStream, ref iLastInclude, snapshot, regionTags);
                                    break;
                                case XSharpLexer.PP_DEFINE:
                                    scanForRegion(token, iToken, tokenStream, ref iLastPPDefine, snapshot, regionTags);
                                    break;
                                case XSharpLexer.DEFINE:
                                    scanForRegion(token, iToken, tokenStream, ref iLastDefine, snapshot, regionTags);
                                    break;
                                case XSharpLexer.SL_COMMENT:
                                    scanForRegion(token, iToken, tokenStream, ref iLastSLComment, snapshot, regionTags);
                                    break;
                                case XSharpLexer.DOC_COMMENT:
                                    scanForRegion(token, iToken, tokenStream, ref iLastDocComment, snapshot, regionTags);
                                    break;
                                case XSharpLexer.USING:
                                    scanForRegion(token, iToken, tokenStream, ref iLastUsing, snapshot, regionTags);
                                    break;
                                default:
                                    break;
                            }
                        }
                    }
                }
                // Orphan End ?
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
            System.Diagnostics.Trace.WriteLine("-->> XSharpClassifier.BuildColorClassifications()");
            lock (gate)
            {
                _snapshot = snapshot;
                _colorTags = newtags;
                _lexerRegions = regionTags;
            }
            System.Diagnostics.Trace.WriteLine("<<-- XSharpClassifier.BuildColorClassifications()");
            Debug("End building Classifications at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            triggerRepaint(snapshot);
        }

        IToken ScanForLastToken(int type, int start, ITokenStream TokenStream, out int iLast)
        {
            var lastFound = TokenStream.Get(start);
            int iLine = lastFound.Line;
            iLast = start;
            IToken nextToken = lastFound;
            IToken nextToken2 = lastFound;
            for (int i = start + 1; i < TokenStream.Size - 2; i++)
            {
                nextToken = TokenStream.Get(i);
                nextToken2 = TokenStream.Get(i + 2);  // STATIC <WS> DEFINE for example.
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
            nextToken = lastFound;
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
            System.Diagnostics.Trace.WriteLine("-->> XSharpClassifier.GetRegionTags()");
            IList<ClassificationSpan> result;
            lock (gate)
            {
                if (_parserRegions != null)
                {
                    var list = _parserRegions.ToList();
                    if (_lexerRegions != null)
                        list.AddRange(_lexerRegions);
                    result = list; ;
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
            System.Diagnostics.Trace.WriteLine("<<-- XSharpClassifier.GetRegionTags()");
            return result;
        }

        public IList<ClassificationSpan> GetTags()
        {
            System.Diagnostics.Trace.WriteLine("-->> XSharpClassifier.GetTags()");
            IList<ClassificationSpan> ret;
            lock (gate)
            {
                ret = _colorTags.Tags;
            }
            System.Diagnostics.Trace.WriteLine("<<-- XSharpClassifier.GetTags()");
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
#if DEBUG
            if (System.Diagnostics.Debugger.IsAttached)
                System.Diagnostics.Debug.WriteLine(String.Format("XColorizer: " + msg, o));
#endif
        }
    }

    internal class XClassificationSpans
    {
        private IList<ClassificationSpan> _tags;
        private readonly object gate = new object();
        private IDictionary<int, List<ClassificationSpan>> _hash;
        private IList<ClassificationSpan> _multilineTokens;
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
        internal void Clear()
        {
            lock (gate)
            {
                _tags.Clear();
                _hash.Clear();
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
