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
        private IClassificationType xsharpIdentifierType;
         private IClassificationType xsharpBraceOpenType;
        private IClassificationType xsharpBraceCloseType;
        private IClassificationType xsharpRegionStartType;
        private IClassificationType xsharpRegionStopType;

        private List<ITagSpan<IClassificationTag>> tags;

        internal XSharpTagger( IClassificationTypeRegistryService registry)
        {
            xsharpIdentifierType = registry.GetClassificationType(Constants.XSharpIdentifierFormat);
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

        /*
                    this.Snapshot = this.Buffer.CurrentSnapshot;
            var stream = new AntlrInputStream(this.Snapshot.GetText());
            var lexer = new XSharpLexer(stream);
            var token = lexer.NextToken();
            tags.Clear();
            
            while (token.Type != XSharpLexer.Eof)
            {
                var tokenType = token.Type;
                TextSpan tokenSpan = new TextSpan(token.StartIndex, token.StopIndex - token.StartIndex + 1);
                if (XSharpLexer.IsKeyword(tokenType))
                {
                    tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpKeywordType));
                }
                else if (XSharpLexer.IsConstant(tokenType))
                {
                    if (tokenType == XSharpLexer.STRING_CONST || tokenType == XSharpLexer.ESCAPED_STRING_CONST)
                        tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpStringType));
                    else
                        tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpConstantType));

                }
                else if (XSharpLexer.IsOperator(tokenType))
                {
                    switch (tokenType)
                    {
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LPAREN:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LCURLY:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LBRKT:
                            tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpBraceOpenType));
                            break;

                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.RPAREN:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.RCURLY:
                        case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.RBRKT:
                            tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpBraceCloseType));
                            break;
                        default:
                            tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpOperatorType));
                            break;
                    }
                }
                else if (XSharpLexer.IsIdentifier(tokenType))
                {
                    tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpIdentifierType));
                }
                else if (XSharpLexer.IsComment(tokenType))
                {
                    tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpCommentType));
                    if (tokenType == XSharpLexer.ML_COMMENT)
                    {
                        tokenSpan = new TextSpan(token.StartIndex, 1);
                        tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpRegionStartType));
                        tokenSpan = new TextSpan(token.StopIndex, 1);
                        tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpRegionStopType));
                    }
                }
                token = lexer.NextToken();
            }

        */
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
                discover.xsharpIdentifierType = xsharpIdentifierType;
                discover.xsharpRegionStartType = xsharpRegionStartType;
                discover.xsharpRegionStopType = xsharpRegionStopType;
                walker.Walk(discover, xtree);
                //

                this.tags = discover.tags;
                //
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine(e.Message);
            }
            //
        }
    }
}
