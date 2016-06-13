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
using Microsoft.VisualStudio.Text.Tagging;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

namespace XSharpColorizer
{
    /// <summary>
    /// Classifier that classifies all text as an instance of the "XSharpClassifier" classification type.
    /// </summary>
    internal class XSharpClassifier : IClassifier, IDisposable
    {
        private ITextBuffer Buffer;
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

        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpClassifier"/> class.
        /// </summary>
        /// <param name="registry">Classification registry.</param>
        internal XSharpClassifier(ITextBuffer buffer, IClassificationTypeRegistryService registry, ITextDocumentFactoryService factory)
        {
            this.Buffer = buffer;
            //this.Snapshot = buffer.CurrentSnapshot;
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
            this.Buffer.Changed += OnBufferChanged;
            //
            Colorize();
        }

        void OnBufferChanged(object sender, TextContentChangedEventArgs e)
        {
            // If this isn't the most up-to-date version of the buffer, then ignore it for now (we'll eventually get another change event).
            if (this.versionNumber == e.After.Version.VersionNumber)
                return;
            Colorize();
            this.versionNumber = e.After.Version.VersionNumber;


        }

        private void Colorize()
        {
            var snapshot = this.Buffer.CurrentSnapshot;
            Snapshot = snapshot;
            ITokenStream TokenStream = null;
            // parse for positional keywords that change the colors
            // and get a reference to the tokenstream
            string path = String.Empty;
            if (txtdocfactory != null)
            {
                ITextDocument doc = null;
                if (txtdocfactory.TryGetTextDocument(this.Buffer, out doc))
                {
                    path = doc.FilePath;

                }
            }
            // Parse the source and get the (Lexer) Tokenstream to locate comments, keywords and other tokens.
            // The parser will identify (positional) keywords that are used as identifier
            xsTagger.Parse(snapshot, out TokenStream, path);
            if (TokenStream != null)
            {
                tags.Clear();
                for (var iToken = 0; iToken < TokenStream.Size; iToken++)
                {
                    var token = TokenStream.Get(iToken);
                    var tokenType = token.Type;
                    TextSpan tokenSpan = new TextSpan(token.StartIndex, token.StopIndex - token.StartIndex + 1);
                    if (XSharpLexer.IsKeyword(tokenType))
                    {
                        tags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpKeywordType));
                    }
                    else if (XSharpLexer.IsConstant(tokenType))
                    {
                        tags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpConstantType));

                    }
                    else if (XSharpLexer.IsOperator(tokenType))
                    {
                        switch (tokenType)
                        {
                            case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LPAREN:
                            case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LCURLY:
                            case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LBRKT:
                                tags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpBraceOpenType));
                                break;

                            case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.RPAREN:
                            case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.RCURLY:
                            case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.RBRKT:
                                tags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpBraceCloseType));
                                break;
                            default:
                                tags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpOperatorType));
                                break;
                        }
                    }
                    else if (XSharpLexer.IsIdentifier(tokenType))
                    {
                        tags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpIdentifierType));
                    }
                    else if (XSharpLexer.IsComment(tokenType))
                    {
                        tags.Add(tokenSpan.ToClassificationSpan(snapshot, xsharpCommentType));
                    }
                }
                foreach (var tag in xsTagger.Tags)
                {
                    tags.Add(tag);
                }
                //
                if (ClassificationChanged != null)
                {
                    ClassificationChanged(this, new ClassificationChangedEventArgs(new SnapshotSpan(Buffer.CurrentSnapshot,
                        0, this.Buffer.CurrentSnapshot.Length)));
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
            /*
            var result = new List<ClassificationSpan>()
            {
                new ClassificationSpan(new SnapshotSpan(span.Snapshot, new Span(span.Start, span.Length)), this.classificationType)
            };

            return result;

            if ((spans.Count == 0) || (this.tags == null) || (this.tags.Count == 0))
            {
                //return Enumerable.Empty<ITagSpan<IClassificationTag>>();
                yield break;
            }
            //
            SnapshotSpan entire = span
            //
            foreach (var tag in this.tags)
            {
                if (tag.Span.End.Position >= entire.Start.Position &&
                     tag.Span.Start.Position <= entire.End.Position)
                {
                    yield return tag;
                }
            }*/

            return this.tags;
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
                    if (hashtable.ContainsKey(this.Buffer))
                    {
                        hashtable.Remove(this.Buffer);
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
