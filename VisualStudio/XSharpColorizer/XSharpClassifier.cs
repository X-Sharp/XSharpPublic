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
    internal class XSharpClassifier : IClassifier, IDisposable
    {
        private ITextBuffer buffer;
        public ITextSnapshot Snapshot { get; set; }
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
        //private XSharpTagger xsTagger;
        private SourceWalker xsWalker;
        private List<ClassificationSpan> tags;
        internal List<ClassificationSpan> tagsRegion;
        private ITextDocumentFactoryService txtdocfactory;
        private int versionNumber = 0;
        private BackgroundWorker _bw = null;


        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpClassifier"/> class.
        /// </summary>
        /// <param name="registry">Classification registry.</param>
        internal XSharpClassifier(ITextBuffer buffer, IClassificationTypeRegistryService registry, ITextDocumentFactoryService factory)
        {
            this.buffer = buffer;
            this.buffer.Changed += Buffer_Changed;
            _bw = new BackgroundWorker();
            _bw.RunWorkerCompleted += bw_RunWorkerCompleted;
            _bw.DoWork += bw_DoWork;
            _bw.WorkerSupportsCancellation = true;
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
            _snapshot = buffer.CurrentSnapshot;
            _bw.RunWorkerAsync();
        }
        private ITextSnapshot _snapshot;
        private void Buffer_Changed(object sender, TextContentChangedEventArgs e)
        {
            var snapshot = e.After;
            bool mustParse = snapshot.Version.VersionNumber != this.versionNumber;

            if ( mustParse )
            {
                lock (this)
                {
                    if (!_bw.IsBusy)
                    {
                        _snapshot = snapshot;
                        _bw.RunWorkerAsync();
                    }
                }
            }
        }

        private void bw_DoWork(object sender, DoWorkEventArgs e)
        {
            this.versionNumber = _snapshot.Version.VersionNumber;
            System.Diagnostics.Debug.WriteLine("Starting walk at {0}, version {1}", DateTime.Now, this.versionNumber.ToString());
            Parse(_snapshot);
        }

        private void bw_RunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            if (this.ClassificationChanged != null)
                this.ClassificationChanged(this, new ClassificationChangedEventArgs(
                    new SnapshotSpan(_snapshot, Span.FromBounds(0, _snapshot.Length))));
            System.Diagnostics.Debug.WriteLine("Ending walk at {0}, version {1}", DateTime.Now, this.versionNumber.ToString());
            if (buffer.CurrentSnapshot.Version != _snapshot.Version)
            {
                // Start again because they have continued typing while we were busy
                _snapshot = buffer.CurrentSnapshot;
                _bw.RunWorkerAsync();
            }
        }

        private void Parse(ITextSnapshot snapshot)
        {
            Snapshot = snapshot;
            ITokenStream TokenStream = null;
            // parse for positional keywords that change the colors
            // and get a reference to the tokenstream
            System.Diagnostics.Debug.WriteLine("Starting parse at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            string path = String.Empty;
            if (txtdocfactory != null)
            {
                ITextDocument doc = null;
                if (txtdocfactory.TryGetTextDocument(this.buffer, out doc))
                {
                    path = doc.FilePath;
                }
            }
            // Parse the source and get the (Lexer) Tokenstream to locate comments, keywords and other tokens.
            // The parser will identify (positional) keywords that are used as identifier
            //xsTagger.Parse(snapshot, out TokenStream, path);
            // By setting the FullPath the system will also try to locate the project and its compiler options.
            // when no project is found then the default parse options will be used
            xsWalker.FullPath = path;
            xsWalker.Snapshot = snapshot;
            xsWalker.InitParse();
            xsWalker.BuildModelAndRegionTags();
            this.tagsRegion = xsWalker.Tags;
            TokenStream = xsWalker.TokenStream;
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
                    if (_bw.CancellationPending)
                       break;
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
                foreach (var tag in xsWalker.Tags)
                {
                    newtags.Add(tag);
                }
                tags = newtags;
                System.Diagnostics.Debug.WriteLine("Ending parse at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
                if (ClassificationChanged != null)
                {
                    ClassificationChanged(this, new ClassificationChangedEventArgs(new SnapshotSpan(snapshot, 0, snapshot.Length)));
                }
            }
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

        static System.Collections.Generic.Dictionary<ITextBuffer, XSharpClassifier> hashtable;
        static internal XSharpClassifier GetColorizer(ITextBuffer buffer, IClassificationTypeRegistryService registry, ITextDocumentFactoryService factory)
        {
            if (hashtable == null)
                hashtable = new System.Collections.Generic.Dictionary<ITextBuffer, XSharpClassifier>();
            if (hashtable.ContainsKey(buffer))
                return hashtable[buffer];
            var colorizer = new XSharpClassifier(buffer, registry, factory);
            hashtable.Add(buffer, colorizer);
            return colorizer;
        }

        static internal XSharpClassifier GetColorizer(ITextBuffer buffer )
        {
            if (hashtable != null)
            {
                if (hashtable.ContainsKey(buffer))
                    return hashtable[buffer];
            }
            return null;
        }

        #region IDisposable Support
        private bool disposedValue = false; // To detect redundant calls

        protected virtual void Dispose(bool disposing)
        {
            if (!disposedValue)
            {
                if (disposing)
                {
                    if (hashtable.ContainsKey(this.buffer))
                    {
                        hashtable.Remove(this.buffer);
                    }
                }

                disposedValue = true;
            }
        }

        ~XSharpClassifier()
        {
            // Do not change this code. Put cleanup code in Dispose(bool disposing) above.
            Dispose(false);
        }

        // This code added to correctly implement the disposable pattern.
        void IDisposable.Dispose()
        {
            // Do not change this code. Put cleanup code in Dispose(bool disposing) above.
            Dispose(true);
        }
        #endregion
    }
}

