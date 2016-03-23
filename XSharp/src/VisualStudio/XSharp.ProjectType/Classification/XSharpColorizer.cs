using LanguageService.CodeAnalysis;
using LanguageService.CodeAnalysis.Text;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Threading.Tasks;
using LanguageService.CodeAnalysis.XSharp;
using Microsoft.VisualStudio.ComponentModelHost;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;

namespace XSharpColorizer
{

    [Export(typeof(ITaggerProvider))]
    [ContentType("XSharp")]
    [TagType(typeof(IClassificationTag))]
    internal class XSharpColorizerProvider : ITaggerProvider
    {

        [Export]
        [Name("XSharp")]
        [BaseDefinition("code")]
        internal static ContentTypeDefinition XSharpContentType = null;

        [Export]
        [FileExtension(".prg")]
        [ContentType("XSharp")]
        internal static FileExtensionToContentTypeDefinition XSharpPrgFileType = null;

        [Export]
        [FileExtension(".vh")]
        [ContentType("XSharp")]
        internal static FileExtensionToContentTypeDefinition XSharpVhHeader = null;

        [Export]
        [FileExtension(".xs")]
        [ContentType("XSharp")]
        internal static FileExtensionToContentTypeDefinition XSharpXsFileType = null;

        [Export]
        [FileExtension(".xh")]
        [ContentType("XSharp")]
        internal static FileExtensionToContentTypeDefinition XSharpXhHeader = null;

        [Import]
        ITextDocumentFactoryService factory = null;

        [Import]
        internal IClassificationTypeRegistryService ClassificationRegistry = null; // Set via MEF

        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            return (ITagger<T>) XSharpColorizer.GetColorizer(buffer, ClassificationRegistry, factory);
        }
    }

    class XSharpColorizer : ITagger<IClassificationTag>, IDisposable
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
#pragma warning disable CS0067
        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;
#pragma warning restore CS0067
        private List<ITagSpan<IClassificationTag>> tags;

        private ITextDocumentFactoryService txtdocfactory;
        private int versionNumber = 0;
        private  XSharpColorizer(ITextBuffer buffer, IClassificationTypeRegistryService registry, ITextDocumentFactoryService factory)
        {
            this.Buffer = buffer;
            //this.Snapshot = buffer.CurrentSnapshot;
            txtdocfactory = factory;
            xsTagger = new XSharpTagger(registry);

            tags = new List<ITagSpan<IClassificationTag>>();
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


        /// <summary>
        /// Parse the current Snapshot, and build the Tag List
        /// </summary>
        private void Colorize()
        {
            // RvdH 2016-02-23
            // This method now uses the Lexer to get the tokens in the buffer and uses some static methods of the lexer to 
            // distinguish between different types of tokens.
            // because the (current version of) the parser does not have info about comments anymore as well 
            // some other tokens, such as preprocessor tokens
            // Since some keywords can also be used as identifiers we will also parse the source in the parser to 
            // change some keywords to identifiers.
            // We can probably also get the list of tokens from the parser but I have no idea how to do that at this moment

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
                        tags.Add(tokenSpan.ToTagSpan(snapshot, xsharpKeywordType));
                    }
                    else if (XSharpLexer.IsConstant(tokenType))
                    {
                        tags.Add(tokenSpan.ToTagSpan(snapshot, xsharpConstantType));

                    }
                    else if (XSharpLexer.IsOperator(tokenType))
                    {
                        switch (tokenType)
                        {
                            case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LPAREN:
                            case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LCURLY:
                            case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LBRKT:
                                tags.Add(tokenSpan.ToTagSpan(snapshot, xsharpBraceOpenType));
                                break;

                            case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.RPAREN:
                            case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.RCURLY:
                            case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.RBRKT:
                                tags.Add(tokenSpan.ToTagSpan(snapshot, xsharpBraceCloseType));
                                break;
                             default:
                                tags.Add(tokenSpan.ToTagSpan(snapshot, xsharpOperatorType));
                                break;
                        }
                    }
                    else if (XSharpLexer.IsIdentifier(tokenType))
                    {
                        tags.Add(tokenSpan.ToTagSpan(snapshot, xsharpIdentifierType));
                    }
                    else if (XSharpLexer.IsComment(tokenType))
                    {
                        tags.Add(tokenSpan.ToTagSpan(snapshot, xsharpCommentType));
                    }
                }
                foreach (var tag in xsTagger.Tags)
                {
                    tags.Add(tag);
                }
                if (TagsChanged != null)
                {
                    TagsChanged(this, new SnapshotSpanEventArgs(new SnapshotSpan(Buffer.CurrentSnapshot,
                        0, this.Buffer.CurrentSnapshot.Length)));
                }

            }
        }

        public IEnumerable<ITagSpan<IClassificationTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            if ((spans.Count == 0) || (this.tags == null) || (this.tags.Count == 0) )
            {
                //return Enumerable.Empty<ITagSpan<IClassificationTag>>();
                yield break;
            }
            //
            SnapshotSpan entire = new SnapshotSpan(spans[0].Start, spans[spans.Count - 1].End).TranslateTo(this.Snapshot, SpanTrackingMode.EdgeExclusive);
            //
            foreach ( var tag in this.tags )
            {
                if ( tag.Span.End.Position >= entire.Start.Position &&
                     tag.Span.Start.Position <= entire.End.Position )
                {
                    yield return tag;
                }
            }
        }


        static System.Collections.Generic.Dictionary<ITextBuffer, XSharpColorizer> hashtable;
        static internal XSharpColorizer GetColorizer(ITextBuffer buffer, IClassificationTypeRegistryService registry, ITextDocumentFactoryService factory)
        {
            if (hashtable == null)
                hashtable = new System.Collections.Generic.Dictionary<ITextBuffer, XSharpColorizer> ();
            if (hashtable.ContainsKey(buffer))
                return hashtable[buffer];
            var colorizer = new XSharpColorizer(buffer, registry, factory);
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

         ~XSharpColorizer()
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
