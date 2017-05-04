using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using LanguageService.SyntaxTree.Misc;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Tagging;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LanguageService.SyntaxTree.Tree;
using Microsoft.VisualStudio.Text;

namespace XSharpColorizer
{
    class XSharpTreeDiscover : XSharpBaseListener
    {
        internal List<ITagSpan<IClassificationTag>> tags;
        public ITextSnapshot Snapshot { get; set; }
        internal IClassificationType xsharpIdentifierType;
        internal IClassificationType xsharpBraceOpenType;
        internal IClassificationType xsharpBraceCloseType;
        internal IClassificationType xsharpRegionStartType;
        internal IClassificationType xsharpRegionStopType;

        public XSharpTreeDiscover()
        {
            tags = new List<ITagSpan<IClassificationTag>>();
        }


        public override void ExitEveryRule([NotNull] ParserRuleContext context)
        {
            base.ExitEveryRule(context);
            if ((context is XSharpParser.Namespace_Context) ||
                (context is XSharpParser.Class_Context) ||
                (context is XSharpParser.PropertyContext) ||
                (context is XSharpParser.PropertyAccessorContext))
            {
                // already done
                // BEGIN         NAMESPACE .... END NAMESPACE 
                //
                TagRegion(context, context.ChildCount - 2 );
            }
            else if ((context is XSharpParser.FunctionContext) ||
                    (context is XSharpParser.ProcedureContext) ||
                    (context is XSharpParser.MethodContext) ||
                    (context is XSharpParser.ConstructorContext) ||
                    (context is XSharpParser.DestructorContext))
            {
                // Put a region up to the end of the Entity
                TagRegion(context, context.ChildCount - 1);
            }
			else if (context is XSharpParser.IdentifierContext)
			{
				LanguageService.SyntaxTree.IToken sym = context.Start;
	            TextSpan tokenSpan;
		        tokenSpan = new TextSpan(sym.StartIndex, sym.StopIndex - sym.StartIndex + 1);
        	    tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpIdentifierType));
			}
        }

        private void TagRegion(ParserRuleContext context, int endChild)
        {
            var endToken = context.GetChild(endChild);
            if (endToken is LanguageService.SyntaxTree.Tree.TerminalNodeImpl)
            {
                LanguageService.SyntaxTree.IToken sym = ((LanguageService.SyntaxTree.Tree.TerminalNodeImpl)endToken).Symbol;
                var tokenSpan = new TextSpan(context.Start.StartIndex, 1);
                tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpRegionStartType));
                tokenSpan = new TextSpan( sym.StartIndex, sym.StopIndex - sym.StartIndex + 1);
                tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpRegionStopType));
            }
            else if (endToken is LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.StatementBlockContext)
            {
                XSharpParser.StatementBlockContext lastTokenInContext = endToken as LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.StatementBlockContext;
                var tokenSpan = new TextSpan(context.Start.StartIndex, 1);
                tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpRegionStartType));
                tokenSpan = new TextSpan(lastTokenInContext.Stop.StartIndex - 1, 1);
                tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpRegionStopType));
            }
        }

    }
}
