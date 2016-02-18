using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Classification;
using Microsoft.CodeAnalysis.Text;
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
        internal static FileExtensionToContentTypeDefinition XSharpFileType = null;

        [Import]
        internal IClassificationTypeRegistryService ClassificationRegistry = null; // Set via MEF

        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            return (ITagger<T>)new XSharpColorizer(buffer, ClassificationRegistry);
        }
    }

    class XSharpColorizer : ITagger<IClassificationTag>
    {
        private ITextBuffer Buffer;
        public ITextSnapshot Snapshot { get; set; }
        //private IClassificationType xsharpKeywordType;
        //private IClassificationType xsharpValueType;
        //private IClassificationType xsharpBraceOpenType;
        //private IClassificationType xsharpBraceCloseType;
        //private IClassificationType xsharpRegionStartType;
        //private IClassificationType xsharpRegionStopType;

        private XSharpTagger xsTagger;


#pragma warning disable CS0067
        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;
#pragma warning restore CS0067
        //private List<ITagSpan<IClassificationTag>> tags;

        internal XSharpColorizer(ITextBuffer buffer, IClassificationTypeRegistryService registry)
        {
            this.Buffer = buffer;
            this.Snapshot = buffer.CurrentSnapshot;
            //
            xsTagger = new XSharpTagger(registry);
            xsTagger.Parse(buffer.CurrentSnapshot);
            //
            //xsharpKeywordType = registry.GetClassificationType(Constants.XSharpKeywordFormat);
            //xsharpValueType = registry.GetClassificationType(Constants.XSharpValueFormat);
            //xsharpBraceOpenType = registry.GetClassificationType(Constants.XSharpBraceOpenFormat);
            //xsharpBraceCloseType = registry.GetClassificationType(Constants.XSharpBraceCloseFormat);
            //xsharpRegionStartType = registry.GetClassificationType(Constants.XSharpRegionStartFormat);
            //xsharpRegionStopType = registry.GetClassificationType(Constants.XSharpRegionStopFormat);
            //
            //this.ReParse(buffer.CurrentSnapshot);
            this.Buffer.Changed += OnBufferChanged;
            //
        }


        void OnBufferChanged(object sender, TextContentChangedEventArgs e)
        {
            // If this isn't the most up-to-date version of the buffer, then ignore it for now (we'll eventually get another change event).
            if (e.After != Buffer.CurrentSnapshot)
                return;
            //this.ReParse(e.After);
            xsTagger.Parse(e.After);
        }


        /// <summary>
        /// Parse the current Snapshot, and build the Tag List
        /// </summary>
        //void ReParse( ITextSnapshot snapshot )
        //{
        //    this.Snapshot = snapshot;
        //    string source = this.Snapshot.GetText();
        //    // Currently we "eat" all Exception that might be raised
        //    // by XSharpSyntaxTree.ParseText
        //    try
        //    {
        //        LanguageService.CodeAnalysis.SyntaxTree tree = XSharpSyntaxTree.ParseText(source);
        //        var syntaxRoot = tree.GetRoot();
        //        // Get the antlr4 parse tree root
        //        var xtree = ((LanguageService.CodeAnalysis.XSharp.Syntax.CompilationUnitSyntax)syntaxRoot).XSource;

        //        //
        //        var walker = new LanguageService.SyntaxTree.Tree.ParseTreeWalker();
        //        var discover = new XSharpTreeDiscover();
        //        discover.Snapshot = this.Snapshot;
        //        discover.xsharpBraceCloseType = xsharpBraceCloseType;
        //        discover.xsharpBraceOpenType = xsharpBraceOpenType;
        //        discover.xsharpValueType = xsharpValueType;
        //        discover.xsharpKeywordType = xsharpKeywordType;
        //        discover.xsharpRegionStartType = xsharpRegionStartType;
        //        discover.xsharpRegionStopType = xsharpRegionStopType;
        //        walker.Walk(discover, xtree);
        //        //
        //        this.tags = discover.tags;
        //        //
        //    }
        //    catch
        //    {

        //    }
        //    //
        //}

        public IEnumerable<ITagSpan<IClassificationTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            if ((spans.Count == 0) || (this.xsTagger.Tags == null) || (this.xsTagger.Tags.Count == 0) )
            {
                //return Enumerable.Empty<ITagSpan<IClassificationTag>>();
                yield break;
            }
            //
            SnapshotSpan entire = new SnapshotSpan(spans[0].Start, spans[spans.Count - 1].End).TranslateTo(this.Snapshot, SpanTrackingMode.EdgeExclusive);
            //
            foreach ( var tag in this.xsTagger.Tags )
            {
                //if ( tag.Span.Start.Position >= entire.Start.Position &&
                //     tag.Span.End.Position <= entire.End.Position )
                //{
                    yield return tag;
                //}
            }
            //return this.tags; // GetTagsXS(this.cache, spans);
        }

        #region old code - bad code
        /*
        public IEnumerable<ITagSpan<IClassificationTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            if (spans.Count == 0)
            {
                return Enumerable.Empty<ITagSpan<IClassificationTag>>();
            }
            if (this.cache == null || this.cache.Snapshot != spans[0].Snapshot)
            {
                // 
                string source = spans[0].Snapshot.GetText();
                try
                {
                    LanguageService.CodeAnalysis.SyntaxTree tree = XSharpSyntaxTree.ParseText(source);
                    var syntaxRoot = tree.GetRoot();
                    // Get the antlr4 parse tree root
                    var xtree = ((LanguageService.CodeAnalysis.XSharp.Syntax.CompilationUnitSyntax)syntaxRoot).XSource;
                    //
                    cache = new XSharpDocument();
                    cache.Snapshot = spans[0].Snapshot;
                    cache.Tree = xtree;
                    //

                    var walker = new LanguageService.SyntaxTree.Tree.ParseTreeWalker();
                    var discover = new XSharpTreeDiscover();
                    discover.Document = this.cache;
                    discover.xsharpBraceCloseType = xsharpBraceCloseType;
                    discover.xsharpBraceOpenType = xsharpBraceOpenType;
                    discover.xsharpValueType = xsharpValueType;
                    discover.xsharpKeywordType = xsharpKeywordType;
                    discover.xsharpRegionType = xsharpRegionType;
                    walker.Walk(discover, xtree);
                    //
                    this.tags = discover.tags;
                }
                catch
                {
                    
                }
            }
            return this.tags; // GetTagsXS(this.cache, spans);
        }

        private IEnumerable<ITagSpan<IClassificationTag>> GetTagsXS(
                    XSharpDocument doc,
                    NormalizedSnapshotSpanCollection spans)
        {
            //
            foreach (var item in doc.Tree.children)
            {
                //
                string text = item.GetText();
                if (item is LanguageService.SyntaxTree.Tree.ErrorNodeImpl)
                {
                    LanguageService.SyntaxTree.IToken sym = ((LanguageService.SyntaxTree.Tree.ErrorNodeImpl)item).Symbol;
                    //
                    var tokenSpan = new TextSpan(sym.StartIndex, sym.StopIndex - sym.StartIndex+1);
                    //
                    switch (sym.Type)
                    {
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.ID:
                            break;

                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.DOT:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.COLON:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.MINUS:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.PLUS:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.MULT:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.DIV:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.ASSIGN:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.NL:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.WS:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.UNRECOGNIZED:
                            break;

                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.ARRAY:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.BYTE:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.CHAR:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.CODEBLOCK:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.DATE:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.DWORD:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.FLOAT:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.INT:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.INT64:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LOGIC:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LONGINT:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.PSZ:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.PTR:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.REAL4:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.REAL8:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.REF:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.SHORTINT:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.STRING:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.SYMBOL:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.USUAL:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.UINT64:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.VOID:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.WORD:
                            break;

                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LPAREN:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LCURLY:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LBRKT:
                            yield return tokenSpan.ToTagSpan(doc.Snapshot, xsharpBraceOpenType);
                            break;

                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.RPAREN:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.RCURLY:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.RBRKT:
                            yield return tokenSpan.ToTagSpan(doc.Snapshot, xsharpBraceCloseType);
                            break;

                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.HEX_CONST:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.BIN_CONST:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.INT_CONST:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.DATE_CONST:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.REAL_CONST:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.STRING_CONST:
                            yield return tokenSpan.ToTagSpan(doc.Snapshot, xsharpValueType);
                            break;

                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.SYMBOL_CONST:
                            yield return tokenSpan.ToTagSpan(doc.Snapshot, xsharpKeywordType);
                            break;

                        default:
                            yield return tokenSpan.ToTagSpan(doc.Snapshot, xsharpKeywordType);
                            break;
                    }
                }
            }
        }

        public class XSharpDocument
        {
            public XSharpParser.SourceContext Tree;

            public ITextSnapshot Snapshot { get; set; }
        }
        */
        #endregion
    }
}
