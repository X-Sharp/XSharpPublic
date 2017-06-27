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
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using System.Windows.Threading;
using System.Threading.Tasks;
using System.ComponentModel;
using XSharpModel;
using System.Linq;
namespace XSharpColorizer
{
    /// <summary>
    /// Classifier that classifies all text as an instance of the "XSharpClassifier" classification type.
    /// </summary>
    internal class XSharpClassifier : IClassifier
    {
        private ITextBuffer buffer;
        public ITextSnapshot Snapshot => buffer.CurrentSnapshot;
        private IClassificationType xsharpKeywordType;
        private IClassificationType xsharpIdentifierType;
        private IClassificationType xsharpCommentType;
        private IClassificationType xsharpOperatorType;
        private IClassificationType xsharpPunctuationType;
        private IClassificationType xsharpStringType;
        private IClassificationType xsharpNumberType;
        private IClassificationType xsharpPPType;
        private IClassificationType xsharpBraceOpenType;
        private IClassificationType xsharpBraceCloseType;
        private IClassificationType xsharpRegionStart;
        private IClassificationType xsharpRegionStop;
        private IClassificationType xsharpInactiveType;
        private SourceWalker xsWalker;
        private List<ClassificationSpan> tags;
        private List<ClassificationSpan> tagsRegion;
        private object regionLock = new object();
        private ITextDocumentFactoryService txtdocfactory;
        private BackgroundWorker _bwLex = null;
        private BackgroundWorker _bwParse = null;
        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpClassifier"/> class.
        /// </summary>
        /// <param name="registry">Classification registry.</param>
        internal XSharpClassifier(ITextBuffer buffer, IClassificationTypeRegistryService registry, ITextDocumentFactoryService factory)
        {
            this.buffer = buffer;
            this.buffer.Properties.AddProperty(typeof(XSharpClassifier), this);

            this.buffer.Changed += Buffer_Changed;
            _bwLex = new BackgroundWorker();
            _bwLex.RunWorkerCompleted += LexCompleted;
            _bwLex.DoWork += DoLex;
            _bwParse = new BackgroundWorker();
            _bwParse.RunWorkerCompleted += ParseCompleted;
            _bwParse.DoWork += DoParse;
            txtdocfactory = factory;
            //xsTagger = new XSharpTagger(registry);
            xsWalker = new SourceWalker(registry);
            tags = new List<ClassificationSpan>();
            lock (regionLock)
            {
                tagsRegion = new List<ClassificationSpan>();
            }
            //
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


            xsharpRegionStart = registry.GetClassificationType(ColorizerConstants.XSharpRegionStartFormat);
            xsharpRegionStop = registry.GetClassificationType(ColorizerConstants.XSharpRegionStopFormat);
            //
            _bwLex.RunWorkerAsync(buffer.CurrentSnapshot);
        }
        private void Buffer_Changed(object sender, TextContentChangedEventArgs e)
        {
            var snapshot = e.After;
            if (! _bwLex.IsBusy)
            {
                _bwLex.RunWorkerAsync(snapshot);
            }
            // when busy then the parser run after the lexer will detect a new version and will take care of repainting
        }

        private void DoLex(object sender, DoWorkEventArgs e)
        {
            // Note this runs in the background
            var snapshot = (ITextSnapshot)e.Argument;
            System.Diagnostics.Debug.WriteLine("Starting lex at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            xsWalker.FullPath = GetFileName();
            xsWalker.Snapshot = snapshot;
            var TokenStream = xsWalker.LexFile();
            System.Diagnostics.Debug.WriteLine("Ending lex at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            BuildColorClassifications(TokenStream, snapshot);
            e.Result = snapshot;
        }

        private void triggerRepaint(ITextSnapshot snapshot)
        {
            if (snapshot.Version == buffer.CurrentSnapshot.Version)
            {
                if (this.ClassificationChanged != null )
                {
                    this.ClassificationChanged(this, new ClassificationChangedEventArgs(
                        new SnapshotSpan(snapshot, Span.FromBounds(0, snapshot.Length))));
                }
            }

        }
        private void LexCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            triggerRepaint((ITextSnapshot)e.Result);
            if (!_bwParse.IsBusy)
            {
                _bwParse.RunWorkerAsync(buffer.CurrentSnapshot);
            }
        }

        private string GetFileName()
        {
            string path = String.Empty;
            if (txtdocfactory != null)
            {
                ITextDocument doc = null;
                if (txtdocfactory.TryGetTextDocument(this.buffer, out doc))
                {
                    path = doc.FilePath;
                }
            }
            return path;            
        }
        private void DoParse(object sender, DoWorkEventArgs e)
        {
            // Note this runs in the background
            var snapshot = (ITextSnapshot)e.Argument;
            // parse for positional keywords that change the colors
            // and get a reference to the tokenstream
            System.Diagnostics.Debug.WriteLine("Starting parse at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            xsWalker.FullPath = GetFileName();
            xsWalker.Snapshot = snapshot;
            xsWalker.InitParse();
            xsWalker.BuildModelAndRegionTags();
            lock (regionLock)
            {
                this.tagsRegion = xsWalker.RegionTags;
            }
            var TokenStream = xsWalker.TokenStream;
            System.Diagnostics.Debug.WriteLine("Ending parse at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            BuildColorClassifications(TokenStream, snapshot);
            e.Result = snapshot;
        }

        private void ParseCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            var snapshot = (ITextSnapshot)e.Result;
            if (snapshot.Version == buffer.CurrentSnapshot.Version)
            {
                if (!xsWalker.HasParseErrors)
                {
                    if (buffer.Properties.ContainsProperty(typeof(XSharpOutliningTagger)))
                    {
                        var tagger = buffer.Properties[typeof(XSharpOutliningTagger)] as XSharpOutliningTagger;
                        tagger.Update();
                    }
                }
            }
            else
            {
                // trigger another parse because buffer was changed while we were busy
                _bwParse.RunWorkerAsync(buffer.CurrentSnapshot);
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


        private ClassificationSpan ClassifyToken(IToken token, IList<ClassificationSpan> tagsRegion, ITextSnapshot snapshot)
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
                            lock (regionLock)
                            {
                                tagsRegion.Add(Token2ClassificationSpan(token, snapshot, xsharpRegionStart));
                            }
                            break;
                        case XSharpLexer.PP_ENDREGION:
                        case XSharpLexer.PP_ENDIF:
                            lock (regionLock)
                            {
                                tagsRegion.Add(Token2ClassificationSpan(token, snapshot, xsharpRegionStop));
                            }
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
                        if (token.Type == XSharpLexer.ML_COMMENT)
                        {
                            lock (regionLock)
                            {
                                tagsRegion.Add(Token2ClassificationSpan(token, snapshot, xsharpRegionStart));
                                tagsRegion.Add(Token2ClassificationSpan(token, snapshot, xsharpRegionStop));
                            }
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
                            default:
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
        private void scanForRegion(IToken token, int iToken, ITokenStream TokenStream, ref int iLast, ITextSnapshot snapshot, List<ClassificationSpan> tagsRegion)
        {
            if (iToken > iLast)
            {
                var lastToken = ScanForLastToken(token.Type, iToken, TokenStream, out iLast);
                if (token != lastToken)
                {
                    lock (regionLock)
                    {
                        tagsRegion.Add(Token2ClassificationSpan(token, snapshot, xsharpRegionStart));
                        tagsRegion.Add(Token2ClassificationSpan(lastToken, snapshot, xsharpRegionStop));
                    }
                }
            }
        }

        private void BuildColorClassifications(ITokenStream TokenStream, ITextSnapshot snapshot)
        {
            System.Diagnostics.Debug.WriteLine("Starting colorize at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            if (TokenStream != null)
            {
                int iLastInclude = -1;
                int iLastPPDefine = -1;
                int iLastDefine = -1;
                int iLastSLComment = -1;
                int iLastDocComment = -1;
                int iLastUsing = -1;
                //int iLastGlobal = -1;
                //int iLastLocal = -1;
                List<ClassificationSpan> newtags = new List<ClassificationSpan>();
                for (var iToken = 0; iToken < TokenStream.Size; iToken++)
                {
                    var token = TokenStream.Get(iToken);
                    var span = ClassifyToken(token, tagsRegion, snapshot);
                    if (span != null)
                    {
                        newtags.Add(span);
                        // now look for Regions of similar code lines
                        switch (token.Type)
                        {
                            case XSharpLexer.PP_INCLUDE:
                                scanForRegion(token, iToken, TokenStream, ref iLastInclude, snapshot, tagsRegion);
                                break;
                            case XSharpLexer.PP_DEFINE:
                                scanForRegion(token, iToken, TokenStream, ref iLastPPDefine, snapshot, tagsRegion);
                                break;
                            case XSharpLexer.DEFINE:
                                scanForRegion(token, iToken, TokenStream, ref iLastDefine, snapshot, tagsRegion);
                                break;
                            //case XSharpLexer.GLOBAL:
                            //    scanForRegion(token, iToken, TokenStream, ref iLastGlobal, snapshot, tagsRegion);
                            //    break;
                            //case XSharpLexer.LOCAL:
                            //    scanForRegion(token, iToken, TokenStream, ref iLastLocal, snapshot, tagsRegion);
                            //    break;
                            case XSharpLexer.SL_COMMENT:
                                scanForRegion(token, iToken, TokenStream, ref iLastSLComment, snapshot, tagsRegion);
                                break;
                            case XSharpLexer.DOC_COMMENT:
                                scanForRegion(token, iToken, TokenStream, ref iLastDocComment, snapshot, tagsRegion);
                                break;
                            case XSharpLexer.USING:
                                scanForRegion(token, iToken, TokenStream, ref iLastUsing, snapshot, tagsRegion);
                                break;
                            default:
                                break;
                        }
                    }
                }
                // Make local copy
                ClassificationSpan[] regionTags = xsWalker.RegionTags.ToArray();
                newtags.AddRange(regionTags);
                tags = newtags;
            }
            System.Diagnostics.Debug.WriteLine("Ending colorize at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
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

        internal IList<ClassificationSpan> GetRegionTags()
        {
            // return an array so we will not have locking issues
            lock (regionLock)
            {
                return tagsRegion.ToArray();
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
            var originaltags = tags;        // create copy in case the tags property gets changed in the background
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
            XSharpClassifier colorizer = buffer.Properties.GetOrCreateSingletonProperty<XSharpClassifier>(
                () => new XSharpClassifier(buffer, registry, factory));
            return colorizer;
        }

    }
    internal class ClassificationSpanComparer : IComparer<ClassificationSpan>
    {
        public int Compare(ClassificationSpan x, ClassificationSpan y)
        {
            if (x.Span.Start < y.Span.Start)
                return -1;
            if (x.Span.Start > y.Span.Start)
                return 1;
            return x.Span.Length - y.Span.Length;
            

        }
    }
}

