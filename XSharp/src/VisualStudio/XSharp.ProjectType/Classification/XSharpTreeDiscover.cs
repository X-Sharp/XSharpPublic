using Microsoft.CodeAnalysis.Text;
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
        internal IClassificationType xsharpKeywordType;
        internal IClassificationType xsharpValueType;
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
            //
            if ((context is XSharpParser.Using_Context) ||
                (context is XSharpParser.Namespace_Context) ||
                (context is XSharpParser.Class_Context) ||
                (context is XSharpParser.PropertyContext) ||
                (context is XSharpParser.NativeTypeContext) ||
                (context is XSharpParser.ClassvarModifiersContext) ||
                (context is XSharpParser.MethodtypeContext) ||
                (context is XSharpParser.MemberModifiersContext) ||
                (context is XSharpParser.ConstructorModifiersContext) ||
                (context is XSharpParser.PropertyAutoAccessorContext) ||
                (context is XSharpParser.ForeachStmtContext) ||
                (context is XSharpParser.ForStmtContext) ||
                (context is XSharpParser.RepeatStmtContext) ||
                (context is XSharpParser.WhileStmtContext) ||
                (context is XSharpParser.VarLocalDeclContext) ||
                (context is XSharpParser.LocaldeclContext) ||
                (context is XSharpParser.FunctionContext) ||
                (context is XSharpParser.ProcedureContext) ||
                (context is XSharpParser.MethodContext) ||
                (context is XSharpParser.PropertyContext) ||
                (context is XSharpParser.PropertyAccessorContext)||
                (context is XSharpParser.ClsctorContext) 
                )
            {
                var tokenSpan = new TextSpan(context.Start.StartIndex, context.Start.StopIndex - context.Start.StartIndex + 1);
                tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpKeywordType));
                //
                if ((context is XSharpParser.Namespace_Context) ||
                    (context is XSharpParser.Class_Context) ||
                    (context is XSharpParser.PropertyContext) ||
                    (context is XSharpParser.PropertyAccessorContext))
                {
                    // already done
                    // BEGIN         NAMESPACE .... END NAMESPACE 
                    TagSubTokens(context, new List<int>() { 1, context.ChildCount - 3, context.ChildCount - 2 });
                    //
                    TagRegion(context, context.ChildCount - 2 );
                }
                else if ((context is XSharpParser.ForeachStmtContext) ||
                        (context is XSharpParser.WhileStmtContext) ||
                        (context is XSharpParser.ForStmtContext))
                {
                    // already done
                    // WHILE          END 
                    TagSubTokens(context, new List<int>() { 1, context.ChildCount - 2 });
                    //
                }
                else if (context is XSharpParser.RepeatStmtContext)
                {
                    TagSubTokens(context, new List<int>() { 1, context.ChildCount - 3 });
                }
                else if ((context is XSharpParser.FunctionContext) ||
                        (context is XSharpParser.ProcedureContext) ||
                        (context is XSharpParser.MethodContext) ||
                        (context is XSharpParser.ClsctorContext))
                {
                    // Put a region up to the end of the Entity
                    TagRegion(context, context.ChildCount - 1);
                }
            }
            else
            {
                if ((context is XSharpParser.LiteralValueContext))
                {
                    var tokenSpan = new TextSpan(context.Start.StartIndex, context.Start.StopIndex - context.Start.StartIndex + 1);
                    tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpValueType));
                    //
                }
                //
            }
        }

        private void TagSubTokens(ParserRuleContext context, List<int> positions)
        {
            foreach (int i in positions)
            {
                var otherTokens = context.GetChild(i);
                if (otherTokens is LanguageService.SyntaxTree.Tree.TerminalNodeImpl)
                {
                    LanguageService.SyntaxTree.IToken sym = ((LanguageService.SyntaxTree.Tree.TerminalNodeImpl)otherTokens).Symbol;
                    var tokenSpan = new TextSpan(sym.StartIndex, sym.StopIndex - sym.StartIndex + 1);
                    tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpKeywordType));
                }
            }
            return;
        }


        public override void ExitEntity([NotNull] XSharpParser.EntityContext context)
        {
            base.ExitEntity(context);
            //
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

        public override void VisitTerminal([NotNull] ITerminalNode node)
        {
            LanguageService.SyntaxTree.IToken sym = node.Symbol;
            TextSpan tokenSpan;

            //
            switch (sym.Type)
            {
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.AS:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.IN:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.AUTO:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.GET:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.SET:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.IF:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.ELSE:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.ELSEIF:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.ENDIF:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.END:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.DO:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.CASE:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.ENDCASE:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.RETURN:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.STATIC:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.VIRTUAL:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.INHERIT:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.IMPLEMENTS:
                    tokenSpan = new TextSpan(sym.StartIndex, sym.StopIndex - sym.StartIndex + 1);
                    tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpKeywordType));
                    break;

                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LPAREN:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LCURLY:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.LBRKT:
                    tokenSpan = new TextSpan(sym.StartIndex, sym.StopIndex - sym.StartIndex + 1);
                    tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpBraceOpenType));
                    break;

                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.RPAREN:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.RCURLY:
                case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpLexer.RBRKT:
                    tokenSpan = new TextSpan(sym.StartIndex, sym.StopIndex - sym.StartIndex + 1);
                    tags.Add(tokenSpan.ToTagSpan(Snapshot, xsharpBraceCloseType));
                    break;
            }
        }

        public override void VisitErrorNode([NotNull] LanguageService.SyntaxTree.Tree.IErrorNode node)
        {

        }

    }
}
