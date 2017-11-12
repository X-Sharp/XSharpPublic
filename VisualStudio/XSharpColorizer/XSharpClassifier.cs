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
using System.Collections.Concurrent;

namespace XSharpColorizer
{
    /// <summary>
    /// Classifier that classifies all text as an instance of the "XSharpClassifier" classification type.
    /// </summary>
    public class XSharpClassifier : IClassifier
    {
        private ITextBuffer buffer;
        public ITextSnapshot Snapshot => buffer.CurrentSnapshot;
        private readonly IClassificationType xsharpKeywordType;
        private readonly IClassificationType xsharpIdentifierType;
        private readonly IClassificationType xsharpCommentType;
        private readonly IClassificationType xsharpOperatorType;
        private readonly IClassificationType xsharpPunctuationType;
        private readonly IClassificationType xsharpStringType;
        private readonly IClassificationType xsharpNumberType;
        private readonly IClassificationType xsharpPPType;
        private readonly IClassificationType xsharpBraceOpenType;
        private readonly IClassificationType xsharpBraceCloseType;
        private readonly IClassificationType xsharpRegionStart;
        private readonly IClassificationType xsharpRegionStop;
        private readonly IClassificationType xsharpInactiveType;
        private readonly IClassificationType xsharpLiteralType;
        private readonly BackgroundWorker _bwLex = null;
        private readonly BackgroundWorker _bwParse = null;
        private readonly BackgroundWorker _bwRegions = null;

        private IImmutableList<ClassificationSpan> _tags;
        private IImmutableList<ClassificationSpan> _tagsRegion;
        private ITextDocumentFactoryService txtdocfactory;
        private bool _hasPositionalKeywords;
        private bool _hasParserErrors;
        XFile _file;
        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpClassifier"/> class.
        /// </summary>
        /// <param name="registry">Classification registry.</param>

        internal XSharpClassifier(ITextBuffer buffer, IClassificationTypeRegistryService registry, ITextDocumentFactoryService factory)
        {
            this.buffer = buffer;
            if (buffer.Properties.ContainsProperty(typeof(XFile)))
            {
                var file = (XFile)buffer.Properties.GetProperty(typeof(XFile));
                if (file == null)
                {
                    return;
                }
                _file = file;
            }
            this.buffer.Properties.AddProperty(typeof(XSharpClassifier), this);
            this.buffer.Changed += Buffer_Changed;
            _bwLex = new BackgroundWorker();
            _bwLex.WorkerSupportsCancellation = true;
            _bwLex.RunWorkerCompleted += LexCompleted;
            _bwLex.DoWork += DoLex;
            _bwParse = new BackgroundWorker();
            _bwParse.WorkerSupportsCancellation = true;
            _bwParse.RunWorkerCompleted += ParseCompleted;
            _bwParse.DoWork += DoParse;
            _bwRegions = new BackgroundWorker();
            _bwRegions.WorkerSupportsCancellation = true;
            _bwRegions.DoWork += DoRepaintRegions;

            txtdocfactory = factory;
            lock (this)
            {
                _tags = ImmutableList.CreateBuilder<ClassificationSpan>().ToImmutableList();
                _tagsRegion = ImmutableList.CreateBuilder<ClassificationSpan>().ToImmutableList();
            }
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
            //
            _bwLex.RunWorkerAsync(buffer.CurrentSnapshot);
        }
        private void Buffer_Changed(object sender, TextContentChangedEventArgs e)
        {
            var snapshot = e.After;
            if (!_bwLex.IsBusy)
            {
                _bwLex.RunWorkerAsync(snapshot);
            }
            // when busy then the parser run after the lexer will detect a new version and will take care of repainting
        }

        private void DoLex(object sender, DoWorkEventArgs e)
        {
            // Note this runs in the background
            var snapshot = (ITextSnapshot)e.Argument;
            Debug("Starting lex at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            var file = _file;
            if (file != null)
            {
                file.Tree = null;
                var xsWalker = new SourceWalker(file, snapshot);
                var TokenStream = xsWalker.LexFile();
                Debug("Ending lex at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
                BuildColorClassifications(TokenStream, snapshot);
                e.Result = snapshot;
            }
            else
            {
                _bwLex.CancelAsync();
                e.Cancel = true;
            }
        }

        private void triggerRepaint(ITextSnapshot snapshot)
        {
            if (snapshot.Version == buffer.CurrentSnapshot.Version)
            {
                ClassificationChanged(this, new ClassificationChangedEventArgs(
                       new SnapshotSpan(snapshot, Span.FromBounds(0, snapshot.Length))));
            }

        }
        private void LexCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            if (e.Error == null && !e.Cancelled)
            {
                triggerRepaint((ITextSnapshot)e.Result);
                if (!_bwParse.IsBusy)
                {
                    _bwParse.RunWorkerAsync(buffer.CurrentSnapshot);
                }
            }
        }

        private void DoParse(object sender, DoWorkEventArgs e)
        {
            // Note this runs in the background
            var snapshot = (ITextSnapshot)e.Argument;
            // parse for positional keywords that change the colors
            // and get a reference to the tokenstream
            Debug("Starting parse at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            var file = _file;
            if (file != null)
            {
                var xsWalker = new SourceWalker(file, snapshot);
                var xTree = xsWalker.Parse();
                _hasParserErrors = xsWalker.HasParseErrors;
                var tokenStream = xsWalker.TokenStream;
                xsWalker.BuildModel(xTree, true);
                var regionTags = BuildRegionTags(xTree, snapshot, xsharpRegionStart, xsharpRegionStop);
                BuildColorClassifications(tokenStream, snapshot, regionTags);
                e.Result = xsWalker;
            }
            else
            {
                _bwParse.CancelAsync();
                e.Cancel = true;
            }
            Debug("Ending parse, modelbuild and regionTags builder at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
        }

        private void DoRepaintRegions(object sender, DoWorkEventArgs e)
        {
            System.Threading.Thread.Sleep(1000);
            if (!_hasParserErrors)
            {
                if (buffer.Properties.ContainsProperty(typeof(XSharpOutliningTagger)))
                {
                    var tagger = buffer.Properties[typeof(XSharpOutliningTagger)] as XSharpOutliningTagger;
                    tagger.Update();
                }
            }

        }
        public IImmutableList<ClassificationSpan> BuildRegionTags(XSharpParser.SourceContext xTree, ITextSnapshot snapshot, IClassificationType start, IClassificationType stop)
        {
            IImmutableList<ClassificationSpan> regions = null;
            if (xTree != null && snapshot != null)
            {
                var rdiscover = new XSharpRegionDiscover(snapshot);
                //
                try
                {
                    var walker = new LanguageService.SyntaxTree.Tree.ParseTreeWalker();
                    //
                    rdiscover.xsharpRegionStartType = start;
                    rdiscover.xsharpRegionStopType = stop;
                    // Walk the tree. The XSharpRegionDiscover class will collect the tags.
                    walker.Walk(rdiscover, xTree);
                    _hasPositionalKeywords = rdiscover.HasPositionalKeyword;
                    regions = rdiscover.GetRegionTags();
                }
                catch (Exception e)
                {
                    Debug("BuildRegionTags failed: " + e.Message);
                }
            }
            return regions;
        }


        private void ParseCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            if (e.Error == null && !e.Cancelled)
            {
                var xsWalker = (SourceWalker)e.Result;
                if (xsWalker.Snapshot != null && xsWalker.Snapshot.Version == buffer.CurrentSnapshot.Version)
                {
                    if (!_bwRegions.IsBusy)
                    {
                        _bwRegions.RunWorkerAsync(buffer.CurrentSnapshot);
                    }
                }
                else
                {
                    // trigger another parse because buffer was changed while we were busy
                    _bwParse.RunWorkerAsync(buffer.CurrentSnapshot);
                }
            }
        }


        private TextSpan Token2TextSpan(IToken token)
        {
            TextSpan tokenSpan = new TextSpan(token.StartIndex, token.StopIndex - token.StartIndex + 1);
            return tokenSpan;
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
            BuildColorClassifications(tokenStream, snapshot, null);
        }

        private void BuildColorClassifications(ITokenStream tokenStream, ITextSnapshot snapshot,
            IImmutableList<ClassificationSpan> parserRegionTags)
        {
            Debug("Starting colorize at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            List<ClassificationSpan> newtags;
            var regionTags = new List<ClassificationSpan>();
            if (tokenStream != null)
            {
                int iLastInclude = -1;
                int iLastPPDefine = -1;
                int iLastDefine = -1;
                int iLastSLComment = -1;
                int iLastDocComment = -1;
                int iLastUsing = -1;
                newtags = new List<ClassificationSpan>();
                for (var iToken = 0; iToken < tokenStream.Size; iToken++)
                {
                    var token = tokenStream.Get(iToken);
                    var span = ClassifyToken(token, regionTags, snapshot);
                    if (span != null)
                    {
                        newtags.Add(span);
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
                            //case XSharpLexer.GLOBAL:
                            //    scanForRegion(token, iToken, tokenStream, ref iLastGlobal, snapshot, regionBuilder);
                            //    break;
                            //case XSharpLexer.LOCAL:
                            //    scanForRegion(token, iToken, tokenStream, ref iLastLocal, snapshot, regionBuilder);
                            //    break;
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
                newtags = _tags.ToList();
            }
            lock (this)
            {
                if (parserRegionTags != null)
                {
                    regionTags.AddRange(parserRegionTags);
                }
                _tags = newtags.ToImmutableList();
                if (!_hasParserErrors && parserRegionTags != null)
                {
                    _tagsRegion = regionTags.ToImmutableList();
                }
            }
            Debug("Ending colorize at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
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
            lock (this)
            {
                return _tagsRegion;
            }
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
            IList<ClassificationSpan> originaltags;
            lock (this)
            {
                // No need to copy. tags is only assigned to and never directly modified
                originaltags = _tags.ToImmutableList();
            }
            for (int i = 0; i < originaltags.Count; i++)
            {
                var tag = originaltags[i];
                // Use the Span.Span property to avoid the check for the same Snapshot
                if (tag.Span.Span.OverlapsWith(span.Span))
                {
                    result.Add(tag);
                }
                if (tag.Span.Start > span.Span.End)
                    break;
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
            System.Diagnostics.Debug.WriteLine(String.Format("XColorizer: " + msg, o));
#endif
        }
    }
}

