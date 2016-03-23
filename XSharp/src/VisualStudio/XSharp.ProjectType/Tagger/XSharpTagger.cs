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

namespace XSharpColorizer
{
    internal class XSharpTagger
    {
        private IClassificationType xsharpIdentifierType;
        private IClassificationType xsharpBraceOpenType;
        private IClassificationType xsharpBraceCloseType;
        private IClassificationType xsharpRegionStartType;
        private IClassificationType xsharpRegionStopType;

        private List<ITagSpan<IClassificationTag>> tags;

        internal XSharpTagger(IClassificationTypeRegistryService registry)
        {
            xsharpIdentifierType = registry.GetClassificationType(ColorizerConstants.XSharpIdentifierFormat);
            xsharpBraceOpenType = registry.GetClassificationType(ColorizerConstants.XSharpBraceOpenFormat);
            xsharpBraceCloseType = registry.GetClassificationType(ColorizerConstants.XSharpBraceCloseFormat);
            xsharpRegionStartType = registry.GetClassificationType(ColorizerConstants.XSharpRegionStartFormat);
            xsharpRegionStopType = registry.GetClassificationType(ColorizerConstants.XSharpRegionStopFormat);
            //
            tags = new List<ITagSpan<IClassificationTag>>();
        }

        internal List<ITagSpan<IClassificationTag>> Tags
        {
            get
            {
                return this.tags;
            }
        }

        internal List<ClassificationSpan> GetClassifications()
        {
            List<ClassificationSpan> classifications = new List<ClassificationSpan>();
            //
            foreach (var tag in Tags)
            {
                classifications.Add(new ClassificationSpan(tag.Span, tag.Tag.ClassificationType));
            }
            return classifications;
        }


        internal void Parse(ITextSnapshot snapshot, out LanguageService.SyntaxTree.ITokenStream TokenStream, string path)
        {
            string source = snapshot.GetText();
            // Currently we "eat" all Exception that might be raised
            // by XSharpSyntaxTree.ParseText
            TokenStream = null;
            try
            {
                LanguageService.CodeAnalysis.SyntaxTree tree = XSharpSyntaxTree.ParseText(source, null, path);
                var syntaxRoot = tree.GetRoot();
                // Get the antlr4 parse tree root
                var xtree = ((LanguageService.CodeAnalysis.XSharp.Syntax.CompilationUnitSyntax)syntaxRoot).XSource;
                TokenStream = ((LanguageService.CodeAnalysis.XSharp.Syntax.CompilationUnitSyntax)syntaxRoot).XTokenStream;
                //
                var walker = new LanguageService.SyntaxTree.Tree.ParseTreeWalker();
                var discover = new XSharpTreeDiscover();
                discover.Snapshot = snapshot;
                discover.xsharpBraceCloseType = xsharpBraceCloseType;
                discover.xsharpBraceOpenType = xsharpBraceOpenType;
                discover.xsharpIdentifierType = xsharpIdentifierType;
                discover.xsharpRegionStartType = xsharpRegionStartType;
                discover.xsharpRegionStopType = xsharpRegionStopType;
                // Walk the tree. The TreeDiscover class will collect the tags.
                walker.Walk(discover, xtree);
                this.tags = discover.tags;
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine(e.Message);
            }
        }
    }
}
