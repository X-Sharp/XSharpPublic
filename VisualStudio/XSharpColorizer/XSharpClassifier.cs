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
        internal List<ClassificationSpan> tagsRegion;
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
            tagsRegion = new List<ClassificationSpan>();
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
                if (this.ClassificationChanged != null)
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
            this.tagsRegion = xsWalker.RegionTags;
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
                if (buffer.Properties.ContainsProperty(typeof(XSharpOutliningTagger)))
                {
                    var tagger = buffer.Properties[typeof(XSharpOutliningTagger)] as XSharpOutliningTagger;
                    tagger.Update();
                }
            }
            else
            {
                // trigger another parse because buffer was changed while we were busy
                _bwParse.RunWorkerAsync(buffer.CurrentSnapshot);
            }
        }

        private void BuildColorClassifications(ITokenStream TokenStream, ITextSnapshot snapshot)
        {
            System.Diagnostics.Debug.WriteLine("Starting colorize at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            if (TokenStream != null)
            {
                IToken token = null;
                int iLastInclude = -1;
                int iLastDefine = -1;
                int iLastSLComment = -1;
                int iLastDocComment = -1;
                List<ClassificationSpan> newtags = new List<ClassificationSpan>();
                for (var iToken = 0; iToken < TokenStream.Size; iToken++)
                {
                    token = TokenStream.Get(iToken);
                    var tokenType = token.Type;
                    TextSpan tokenSpan = new TextSpan(token.StartIndex, token.StopIndex - token.StartIndex + 1);
                    //
                    if (token.Channel != 0)
                    {
                        switch (token.Channel)
                        {
                            case XSharpLexer.PREPROCESSORCHANNEL:          // #define, #ifdef etc
                                newtags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpPPType));
                                switch (token.Type)
                                {
                                    //case XSharpLexer.PP_ELSE:
                                    //    tagsRegion.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpRegionStop));
                                    //    tagsRegion.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpRegionStart));
                                    //    break;
                                    case XSharpLexer.PP_REGION:
                                    case XSharpLexer.PP_IFDEF:
                                    case XSharpLexer.PP_IFNDEF:
                                        tagsRegion.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpRegionStart));
                                        break;
                                    case XSharpLexer.PP_ENDREGION:
                                    case XSharpLexer.PP_ENDIF:
                                        tagsRegion.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpRegionStop));
                                        break;
                                    case XSharpLexer.PP_INCLUDE:
                                        // scan for list of #includes and create a block
                                        if (iToken > iLastInclude)
                                        {
                                            var lastToken = ScanForLastToken(XSharpLexer.PP_INCLUDE, iToken, TokenStream, out iLastInclude);
                                            if (token != lastToken)
                                            {
                                                tagsRegion.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpRegionStart));
                                                var endSpan = new TextSpan(lastToken.StartIndex, lastToken.StopIndex - lastToken.StartIndex + 1);
                                                tagsRegion.Add(endSpan.ToClassificationSpan(snapshot, xsharpRegionStop));

                                            }
                                        }
                                        break;
                                    case XSharpLexer.PP_DEFINE:
                                        // scan for list of #includes and create a block
                                        if (iToken > iLastDefine)
                                        {
                                            var lastToken = ScanForLastToken(XSharpLexer.PP_DEFINE, iToken, TokenStream, out iLastDefine);
                                            if (token != lastToken)
                                            {
                                                tagsRegion.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpRegionStart));
                                                var endSpan = new TextSpan(lastToken.StartIndex, lastToken.StopIndex - lastToken.StartIndex + 1);
                                                tagsRegion.Add(endSpan.ToClassificationSpan(snapshot, xsharpRegionStop));

                                            }
                                        }
                                        break;
                                    default:
                                        break;
                                }
                                break;
                            case XSharpLexer.PRAGMACHANNEL:         // #pragma
                                newtags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpPPType));
                                break;
                            case XSharpLexer.DEFOUTCHANNEL:                // code in an inactive #ifdef
                                newtags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpInactiveType));
                                break;
                            case XSharpLexer.XMLDOCCHANNEL:
                            case XSharpLexer.Hidden:
                                if (XSharpLexer.IsComment(token.Type))
                                {
                                    newtags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpCommentType));
                                    if (token.Type == XSharpLexer.ML_COMMENT)
                                    {
                                        tagsRegion.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpRegionStart));
                                        tagsRegion.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpRegionStop));
                                    }
                                    else if (token.Type == XSharpLexer.SL_COMMENT)
                                    {
                                        if (iToken > iLastSLComment)
                                        {
                                            var lastToken = ScanForLastToken(XSharpLexer.SL_COMMENT, iToken, TokenStream, out iLastSLComment);
                                            if (token != lastToken)
                                            {
                                                tagsRegion.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpRegionStart));
                                                var endSpan = new TextSpan(lastToken.StartIndex, lastToken.StopIndex - lastToken.StartIndex + 1);
                                                tagsRegion.Add(endSpan.ToClassificationSpan(snapshot, xsharpRegionStop));

                                            }
                                        }
                                    }
                                    else if (token.Type == XSharpLexer.DOC_COMMENT)
                                    {
                                        if (iToken > iLastDocComment)
                                        {
                                            var lastToken = ScanForLastToken(XSharpLexer.DOC_COMMENT, iToken, TokenStream, out iLastDocComment);
                                            if (token != lastToken)
                                            {
                                                tagsRegion.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpRegionStart));
                                                var endSpan = new TextSpan(lastToken.StartIndex, lastToken.StopIndex - lastToken.StartIndex + 1);
                                                tagsRegion.Add(endSpan.ToClassificationSpan(snapshot, xsharpRegionStop));

                                            }
                                        }

                                    }
                                }
                                //
                                // add code to create a region for a group of
                                // SL_COMMENT and/or DOC_COMMENT lines
                                // detect when we are on the last line of a comment block and then
                                // find the first line of the block by scanning backwards
                                // and when the line numbers are different then create region

                                else if (token.Type == XSharpLexer.LINE_CONT ||
                                        token.Type == XSharpLexer.LINE_CONT_OLD)
                                {
                                    // Semi colon followed by optional comment and whitespace
                                    // if there is an embedded comment then mark that as comment
                                    if (token.Text.Trim().Length > 1)
                                    {
                                        // Contains embedded comment
                                        tokenSpan = new TextSpan(token.StartIndex + 1, token.StopIndex - token.StartIndex);
                                        newtags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpCommentType));
                                        // The semi colon
                                        tokenSpan = new TextSpan(token.StartIndex, 1);
                                        newtags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpPunctuationType));
                                        continue;
                                    }

                                }
                                break;
                        }
                        continue;
                    }
                    else if (XSharpLexer.IsIdentifier(tokenType))
                    {
                        newtags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpIdentifierType));
                    }
                    else if (XSharpLexer.IsConstant(tokenType))
                    {
                        switch (tokenType)
                        {
                            case XSharpLexer.STRING_CONST:
                            case XSharpLexer.CHAR_CONST:
                            case XSharpLexer.ESCAPED_STRING_CONST:
                            case XSharpLexer.INTERPOLATED_STRING_CONST:
                                newtags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpStringType));
                                break;
                            default:
                                newtags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpNumberType));
                                break;
                        }

                    }
                    else if (XSharpLexer.IsKeyword(tokenType))
                    {
                        newtags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpKeywordType));
                    }
                    else if (XSharpLexer.IsOperator(tokenType))
                    {
                        switch (tokenType)
                        {
                            case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LPAREN:
                            case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LCURLY:
                            case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LBRKT:
                                newtags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpBraceOpenType));
                                break;

                            case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.RPAREN:
                            case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.RCURLY:
                            case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.RBRKT:
                                newtags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpBraceCloseType));
                                break;
                            default:
                                newtags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpOperatorType));
                                break;
                        }
                    }
                }
                // Add Region Tags
                foreach (var tag in xsWalker.RegionTags)
                {
                    newtags.Add(tag);
                }
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
            var result = new List<ClassificationSpan>();
            var originaltags = tags;        // create copy in case the tags property gets changed in the background
            foreach (var tag in originaltags)
            {
                if (tag.Span.End.Position < span.Start.Position || tag.Span.Start.Position >= span.End.Position)
                {
                    // skip tags that are completely before or after the selected span;
                }
                else
                {
                    result.Add(tag);
                }
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

}

