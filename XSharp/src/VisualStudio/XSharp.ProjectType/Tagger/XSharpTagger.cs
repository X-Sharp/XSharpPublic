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
    internal class XSharpTagger
    {
        private IClassificationType xsharpKeywordType;
        private IClassificationType xsharpValueType;
        private IClassificationType xsharpBraceOpenType;
        private IClassificationType xsharpBraceCloseType;
        private IClassificationType xsharpRegionStartType;
        private IClassificationType xsharpRegionStopType;

        private List<ITagSpan<IClassificationTag>> tags;

        internal XSharpTagger( IClassificationTypeRegistryService registry)
        {
            xsharpKeywordType = registry.GetClassificationType(Constants.XSharpKeywordFormat);
            xsharpValueType = registry.GetClassificationType(Constants.XSharpValueFormat);
            xsharpBraceOpenType = registry.GetClassificationType(Constants.XSharpBraceOpenFormat);
            xsharpBraceCloseType = registry.GetClassificationType(Constants.XSharpBraceCloseFormat);
            xsharpRegionStartType = registry.GetClassificationType(Constants.XSharpRegionStartFormat);
            xsharpRegionStopType = registry.GetClassificationType(Constants.XSharpRegionStopFormat);
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
            foreach( var tag in Tags )
            {
                classifications.Add(new ClassificationSpan(tag.Span, tag.Tag.ClassificationType));
            }
            //
            return classifications;
        }

        internal void Parse(ITextSnapshot snapshot)
        {
            string source = snapshot.GetText();
            // Currently we "eat" all Exception that might be raised
            // by XSharpSyntaxTree.ParseText
            try
            {
                LanguageService.CodeAnalysis.SyntaxTree tree = XSharpSyntaxTree.ParseText(source);
                var syntaxRoot = tree.GetRoot();
                // Get the antlr4 parse tree root
                var xtree = ((LanguageService.CodeAnalysis.XSharp.Syntax.CompilationUnitSyntax)syntaxRoot).XSource;

                //
                var walker = new LanguageService.SyntaxTree.Tree.ParseTreeWalker();
                var discover = new XSharpTreeDiscover();
                discover.Snapshot = snapshot;
                discover.xsharpBraceCloseType = xsharpBraceCloseType;
                discover.xsharpBraceOpenType = xsharpBraceOpenType;
                discover.xsharpValueType = xsharpValueType;
                discover.xsharpKeywordType = xsharpKeywordType;
                discover.xsharpRegionStartType = xsharpRegionStartType;
                discover.xsharpRegionStopType = xsharpRegionStopType;
                walker.Walk(discover, xtree);
                //
                this.tags = discover.tags;
                //
            }
            catch
            {

            }
            //
        }
    }
}
