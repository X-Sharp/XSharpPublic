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
        private IClassificationType xsharpConstantType;
        private IClassificationType xsharpBraceOpenType;
        private IClassificationType xsharpBraceCloseType;
        private IClassificationType xsharpRegionStart;
        private IClassificationType xsharpRegionStop;
        private XSharpTagger xsTagger;
        private List<ClassificationSpan> tags;
        private ITextDocumentFactoryService txtdocfactory;
        private int versionNumber = 0;
        private bool isBusy = false;


        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpClassifier"/> class.
        /// </summary>
        /// <param name="registry">Classification registry.</param>
        internal XSharpClassifier(ITextBuffer buffer, IClassificationTypeRegistryService registry, ITextDocumentFactoryService factory)
        {
            this.buffer = buffer;
            
            txtdocfactory = factory;
            xsTagger = new XSharpTagger(registry);
            tags = new List<ClassificationSpan>();
            //
            xsharpKeywordType = registry.GetClassificationType(ColorizerConstants.XSharpKeywordFormat);
            xsharpIdentifierType = registry.GetClassificationType(ColorizerConstants.XSharpIdentifierFormat);
            xsharpCommentType = registry.GetClassificationType(ColorizerConstants.XSharpCommentFormat);
            xsharpOperatorType = registry.GetClassificationType(ColorizerConstants.XSharpOperatorFormat);
            xsharpConstantType = registry.GetClassificationType(ColorizerConstants.XSharpConstantFormat);
            xsharpBraceOpenType = registry.GetClassificationType(ColorizerConstants.XSharpBraceOpenFormat);
            xsharpBraceCloseType = registry.GetClassificationType(ColorizerConstants.XSharpBraceCloseFormat);
            xsharpRegionStart = registry.GetClassificationType(ColorizerConstants.XSharpRegionStartFormat);
            xsharpRegionStop = registry.GetClassificationType(ColorizerConstants.XSharpRegionStopFormat);
            //
            Parse(buffer.CurrentSnapshot);
            isBusy = false;
        }

        private void Parse( ITextSnapshot snapshot)
        {
            Snapshot = snapshot;
            ITokenStream TokenStream = null;
            // parse for positional keywords that change the colors
            // and get a reference to the tokenstream
            System.Diagnostics.Debug.WriteLine("Starting parse at {0}, version {1}", DateTime.Now, snapshot.Version.ToString());
            string path = String.Empty;
            isBusy = true;
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
            xsTagger.Parse(snapshot, out TokenStream, path);
            if (TokenStream != null)
            {
                List<ClassificationSpan> newtags = new List<ClassificationSpan>();
                for (var iToken = 0; iToken < TokenStream.Size; iToken++)
                {
                    var token = TokenStream.Get(iToken);
                    var tokenType = token.Type;
                    TextSpan tokenSpan = new TextSpan(token.StartIndex, token.StopIndex - token.StartIndex + 1);
                    if (XSharpLexer.IsKeyword(tokenType))
                    {
                        newtags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpKeywordType));
                    }
                    else if (XSharpLexer.IsConstant(tokenType))
                    {
                        newtags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpConstantType));

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
                    else if (XSharpLexer.IsIdentifier(tokenType))
                    {
                        newtags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpIdentifierType));
                    }
                    else if (XSharpLexer.IsComment(tokenType))
                    {
                        newtags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpCommentType));
                    }
                }
                foreach (var tag in xsTagger.Tags)
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
            var snapshot = buffer.CurrentSnapshot;
            if (snapshot.Version.VersionNumber != this.versionNumber  && ! isBusy )
            {
                isBusy = true;
                this.versionNumber = snapshot.Version.VersionNumber;
                var bw = new BackgroundWorker();
                bw.DoWork += Bw_DoWork;
                bw.RunWorkerCompleted += Bw_RunWorkerCompleted;
                bw.RunWorkerAsync(span.Snapshot);
            }
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

        private void Bw_RunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            isBusy = false;
        }

        private void Bw_DoWork(object sender, DoWorkEventArgs e)
        {
            Parse((ITextSnapshot)e.Argument);
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
