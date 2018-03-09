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
using System.Collections.Immutable;
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

        private XClassificationSpans _tags = new XClassificationSpans();
        private IImmutableList<ClassificationSpan> _tagsRegion = ImmutableList<ClassificationSpan>.Empty;
        private ITextDocumentFactoryService _txtdocfactory;
        private bool _hasParserErrors = false;
        private bool _first = true;
        private XSharpParser.SourceContext _tree = null;
        #endregion

        public ITextSnapshot Snapshot => _sourceWalker.Snapshot;

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
            var snapshot = buffer.CurrentSnapshot;
            _sourceWalker = new SourceWalker(file, snapshot);
            ITokenStream tokens = ClassifyBuffer(snapshot);
            BuildColorClassifications(tokens, snapshot);
            _first = false;
            // start the model builder to do build a code model and the regions asynchronously
            _bwBuildModel.RunWorkerAsync(snapshot);

        }
        #region Lexer Methods

        private void Buffer_Changed(object sender, TextContentChangedEventArgs e)
        {
            if (!_bwClassify.IsBusy && !_bwBuildModel.IsBusy)
            {
                var snapshot = e.After;
                _bwClassify.RunWorkerAsync(snapshot);
            }
        }

        private ITokenStream ClassifyBuffer(ITextSnapshot snapshot)
        {
            _sourceWalker.Snapshot = snapshot;
            Debug("Starting classify at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            ITokenStream tokens;
            if (_first)
            {
                tokens = _sourceWalker.Lex();
                _tree = null;
            }
            else
            {
                _tree = _sourceWalker.Parse();
                tokens = _sourceWalker.TokenStream;
            }
            _hasParserErrors = _sourceWalker.HasParseErrors;
            BuildColorClassifications(tokens, snapshot);
            Debug("Ending classify at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            return tokens;
        }

        private void DoClassify(object sender, DoWorkEventArgs e)
        {
            // Note this runs in the background
            // Wait a little and then get the current snapshot. They may have typed fast or the buffer may have been updated by the formatter
            var snapshot = (ITextSnapshot)e.Argument;
            ClassifyBuffer(snapshot);
            e.Result = snapshot;
        }
        private void triggerRepaint(ITextSnapshot snapshot)
        {
            System.Diagnostics.Trace.WriteLine("-->> XSharpClassifier.triggerRepaint()");
            if (snapshot != null)
            {
                if (_buffer.CurrentSnapshot.Version == snapshot.Version && !_first)
                {
                    ClassificationChanged(this, new ClassificationChangedEventArgs(
                            new SnapshotSpan(snapshot, Span.FromBounds(0, snapshot.Length))));
                }
            }
            System.Diagnostics.Trace.WriteLine("<<-- XSharpClassifier.triggerRepaint()");
        }
        private void ClassifyCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            System.Diagnostics.Trace.WriteLine("-->> XSharpClassifier.ClassifyCompleted()");
            if (e.Cancelled)
            {
                return;
            }
            if (e.Error == null )
            {
                var snapshot = e.Result as ITextSnapshot;
                if (snapshot != null)
                {
                    var newSnapshot = _buffer.CurrentSnapshot;
                    if (newSnapshot.Version != snapshot.Version)
                    {
                        // buffer was changed, so restart
                        _bwClassify.RunWorkerAsync(newSnapshot);
                    }
                    else
                    {
                        triggerRepaint(snapshot);
                        if (!_bwBuildModel.IsBusy)
                        {
                            _bwBuildModel.RunWorkerAsync(snapshot);
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
            System.Diagnostics.Trace.WriteLine("-->> XSharpClassifier.BuildModelDoWork()");
            // Note this runs in the background
            // parse for positional keywords that change the colors
            // and get a reference to the tokenstream
            // do we need to create a new tree 
            // this happens the first time in the buffer only
            var snapshot = e.Argument as ITextSnapshot;
            ITokenStream tokens = null;
            if (_tree == null || _sourceWalker.Snapshot.Version != snapshot.Version)
            {
                Debug("Starting parse at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
                _sourceWalker.Snapshot = snapshot;
                var tree = _sourceWalker.Parse();
                lock (gate)
                {
                    _tree = tree;
                }
                Debug("Ending parse at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
                _hasParserErrors = _sourceWalker.HasParseErrors;
            }
            tokens = _sourceWalker.TokenStream;
            if (_tree != null && tokens != null)
            {
                Debug("Starting model build  at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
                _sourceWalker.BuildModel(_tree, true);
                var regionTags = BuildRegionTags(_tree, snapshot, xsharpRegionStart, xsharpRegionStop);
                BuildColorClassifications(tokens, snapshot, regionTags);
                DoRepaintRegions();
                Debug("Ending model build  at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            }
            System.Diagnostics.Trace.WriteLine("<<-- XSharpClassifier.BuildModelDoWork()");
        }
        #endregion

        private void DoRepaintRegions()
        {
            if (!_hasParserErrors)
            {
                if (_buffer.Properties.ContainsProperty(typeof(XSharpOutliningTagger)))
                {
                    var tagger = _buffer.Properties[typeof(XSharpOutliningTagger)] as XSharpOutliningTagger;
                    tagger.Update();
                }
            }

        }
        public IImmutableList<ClassificationSpan> BuildRegionTags(XSharpParser.SourceContext xTree, ITextSnapshot snapshot, IClassificationType start, IClassificationType stop)
        {
            System.Diagnostics.Trace.WriteLine("-->> XSharpClassifier.BuildRegionTags()");
            IImmutableList<ClassificationSpan> regions = null;
            if (xTree != null && snapshot != null && !_hasParserErrors)
            {
                var rdiscover = new XSharpRegionDiscover(snapshot);
                //
                try
                {
                    var treeWalker = new LanguageService.SyntaxTree.Tree.ParseTreeWalker();
                    //
                    rdiscover.xsharpRegionStartType = start;
                    rdiscover.xsharpRegionStopType = stop;
                    // Walk the tree. The XSharpRegionDiscover class will collect the tags.
                    treeWalker.Walk(rdiscover, xTree);
                    regions = rdiscover.GetRegionTags();
                }
                catch (Exception e)
                {
                    Debug("BuildRegionTags failed: " + e.Message);
                }
            }
            System.Diagnostics.Trace.WriteLine("<<-- XSharpClassifier.BuildRegionTags()");
            return regions;
        }




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
            //
            switch (tokenType)
            {
                case XSharpLexer.FOR:
                case XSharpLexer.FOREACH:
                case XSharpLexer.REPEAT:
                case XSharpLexer.DO:
                case XSharpLexer.IF:
                case XSharpLexer.TRY:
                case XSharpLexer.BEGIN:
                case XSharpLexer.SWITCH:
                    type = xsharpKwOpenType;
                    break;

                case XSharpLexer.NEXT:
                case XSharpLexer.UNTIL:
                case XSharpLexer.END:
                case XSharpLexer.ENDDO:
                case XSharpLexer.ENDIF:
                case XSharpLexer.ENDCASE:
                    type = xsharpKwCloseType;
                    break;
            }
            //
            if (type != null)
            {
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
            if (tokenStream != null && snapshot != null)
            {
                BuildColorClassifications(tokenStream, snapshot, null);
            }
        }

        private void BuildColorClassifications(ITokenStream tokenStream, ITextSnapshot snapshot,
            IImmutableList<ClassificationSpan> parserRegionTags)
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
                for (var iToken = 0; iToken < tokenStream.Size; iToken++)
                {
                    var token = tokenStream.Get(iToken);
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
            else
            {
                newtags = _tags;
            }
            System.Diagnostics.Trace.WriteLine("-->> XSharpClassifier.BuildColorClassifications()");
            if (parserRegionTags != null)
            {
                regionTags.AddRange(parserRegionTags);
            }
            lock (gate)
            {
                _tags = newtags;
            }
            if (!_hasParserErrors && parserRegionTags != null)
            {
                var list = regionTags.ToImmutableList();
                lock (gate)
                {
                    _tagsRegion = list ;
                }
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
            for (int i = start + 1; i < TokenStream.Size; i++)
            {
                nextToken = TokenStream.Get(i);
                if (nextToken.Line > iLine)
                {
                    if (nextToken.Type == type)
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

        public IImmutableList<ClassificationSpan> GetRegionTags()
        {
            System.Diagnostics.Trace.WriteLine("-->> XSharpClassifier.GetRegionTags()");
            IImmutableList<ClassificationSpan> ret;
            lock (gate)
            {
                ret = _tagsRegion;
            }
            System.Diagnostics.Trace.WriteLine("<<-- XSharpClassifier.GetRegionTags()");
            return ret;
        }

        public IImmutableList<ClassificationSpan> GetTags()
        {
            System.Diagnostics.Trace.WriteLine("-->> XSharpClassifier.GetTags()");
            IImmutableList<ClassificationSpan> ret;
            lock (gate)
            {
                ret = _tags.Tags;
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
            var tags = _tags;
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
        private IDictionary<int, List<ClassificationSpan>> _hash;
        private IList<ClassificationSpan> _multilineTokens;
        internal XClassificationSpans()
        {
            _tags = new List<ClassificationSpan>();
            _hash = new Dictionary<int, List<ClassificationSpan>>();
            _multilineTokens = new List<ClassificationSpan>();
        }
        internal void Add(ClassificationSpan span)
        {
            _tags.Add(span);
            int start = span.Span.Start.GetContainingLine().LineNumber;
            int end = span.Span.End.GetContainingLine().LineNumber;
            if (end > start + 1)
                _multilineTokens.Add(span);
            else
            {
                if (!_hash.ContainsKey(start))
                {
                    _hash.Add(start, new List<ClassificationSpan>());
                }
                _hash[start].Add(span);
            }
        }
        internal List<ClassificationSpan> GetItemsForLine(int line)
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
        internal int Count => _tags.Count;
        internal void Clear()
        {
            _tags.Clear();
            _hash.Clear();
        }
        internal ImmutableList<ClassificationSpan> Tags => _tags.ToImmutableList();
    }
}

