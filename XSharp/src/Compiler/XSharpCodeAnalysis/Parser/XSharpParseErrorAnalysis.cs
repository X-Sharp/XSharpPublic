using System;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using InternalSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal class XSharpParseErrorAnalysis : XSharpBaseListener
    {
        private XSharpParser _parser;

        public XSharpParseErrorAnalysis(XSharpParser parser)
        {
            _parser = parser;
        }

        internal void CheckMissingToken(ref IToken t, int tId, string tText = "")
        {
            if (t == null)
            {
                t = _parser.TokenFactory.Create(XSharpParser.ID, "");
            }
        }

        public override void VisitErrorNode([NotNull] IErrorNode node)
        {
            if (node.Symbol.TokenIndex == -1)
            {
                node.Parent.RuleContext.AddError(new ParseErrorData(node, ErrorCode.ERR_SyntaxError, node));
            }
            else
            {
                //node.Parent.RuleContext.AddError(new ParseErrorData(node, ErrorCode.ERR_SyntaxError, node));
            }
        }

        public override void VisitTerminal(ITerminalNode node)
        {
        }

        public override void ExitEveryRule([NotNull] ParserRuleContext context)
        {
            if (context.exception != null)
                context.AddError(new ParseErrorData(context, ErrorCode.ERR_SyntaxError, context));
        }

        public override void ExitEntity([NotNull] XSharpParser.EntityContext context)
        {
        }

        public override void ExitEof([NotNull] XSharpParser.EofContext context)
        {
        }

        public override void ExitEos([NotNull] XSharpParser.EosContext context)
        {
        }

        public override void ExitSource([NotNull] XSharpParser.SourceContext context)
        {
        }

        public override void ExitFunction([NotNull] XSharpParser.FunctionContext context)
        {
            if (context.Type == null)
            {
/*                var t = new XSharpParser.SimpleDatatypeContext(new XSharpParser.DatatypeContext(context, 0));
                t.TypeName = new XSharpParser.TypeNameContext(t, 0);
                t.AddChild(t.TypeName);
                t.TypeName.NativeType = new XSharpParser.NativeTypeContext(t, 0);
                t.TypeName.AddChild(t.TypeName.NativeType);
                t.TypeName.NativeType.Token = _parser.TokenFactory.Create(XSharpParser.VOID, "");
                t.TypeName.NativeType.AddChild(t.TypeName.NativeType.Token);
                context.Type = t;
                context.AddChild(t);*/
                context.Type = new XSharpParser.DatatypeContext(context, 0);
                context.Type.Put(SyntaxFactory.PredefinedType(SyntaxFactory.MissingToken(SyntaxKind.VoidKeyword)));
                //context.AddError(new ParseErrorData(context, ErrorCode.ERR_SyntaxError, context));
            }
            if (context.StmtBlk == null)
            {
                context.StmtBlk = new XSharpParser.StatementBlockContext(context, 0);
                context.AddChild(context.StmtBlk);
            }
        }

        public override void ExitProcedure([NotNull] XSharpParser.ProcedureContext context)
        {
        }

        public override void ExitParameterList([NotNull] XSharpParser.ParameterListContext context)
        {
        }

        public override void ExitParameter([NotNull] XSharpParser.ParameterContext context)
        {
        }

        public override void ExitStatementBlock([NotNull] XSharpParser.StatementBlockContext context)
        {
        }

        public override void ExitReturnStmt([NotNull] XSharpParser.ReturnStmtContext context)
        {
        }

        public override void ExitExpressionStmt([NotNull] XSharpParser.ExpressionStmtContext context)
        {
        }

        public override void ExitAccessMember([NotNull] XSharpParser.AccessMemberContext context)
        {
        }

        public override void ExitPostfixExpression([NotNull] XSharpParser.PostfixExpressionContext context)
        {
        }

        public override void ExitPrefixExpression([NotNull] XSharpParser.PrefixExpressionContext context)
        {
        }

        public override void ExitBinaryExpression([NotNull] XSharpParser.BinaryExpressionContext context)
        {
        }

        public override void ExitAssignmentExpression([NotNull] XSharpParser.AssignmentExpressionContext context)
        {
        }

        public override void ExitMethodCall([NotNull] XSharpParser.MethodCallContext context)
        {
        }

        public override void ExitCtorCall([NotNull] XSharpParser.CtorCallContext context)
        {
        }

        public override void ExitArrayAccess([NotNull] XSharpParser.ArrayAccessContext context)
        {
        }

        public override void ExitNameExpression([NotNull] XSharpParser.NameExpressionContext context)
        {
        }

        public override void ExitTypeExpression([NotNull] XSharpParser.TypeExpressionContext context)
        {
        }

        public override void ExitIifExpression([NotNull] XSharpParser.IifExpressionContext context)
        {
        }

        public override void ExitParenExpression([NotNull] XSharpParser.ParenExpressionContext context)
        {
        }

        public override void ExitTypeCast([NotNull] XSharpParser.TypeCastContext context)
        {
        }

        public override void ExitSizeOfExpression([NotNull] XSharpParser.SizeOfExpressionContext context)
        {
        }

        public override void ExitTypeOfExpression([NotNull] XSharpParser.TypeOfExpressionContext context)
        {
        }

        public override void ExitArgumentList([NotNull] XSharpParser.ArgumentListContext context)
        {
        }

        public override void ExitArgument([NotNull] XSharpParser.ArgumentContext context)
        {
        }

        public override void ExitQualifiedName([NotNull] XSharpParser.QualifiedNameContext context)
        {
        }

        public override void ExitSimpleName([NotNull] XSharpParser.SimpleNameContext context)
        {
        }

        public override void ExitGenericName([NotNull] XSharpParser.GenericNameContext context)
        {
        }

        public override void ExitGenericArgumentList([NotNull] XSharpParser.GenericArgumentListContext context)
        {
        }

        public override void ExitIdentifierName([NotNull] XSharpParser.IdentifierNameContext context)
        {
        }

        public override void ExitPtrDatatype([NotNull] XSharpParser.PtrDatatypeContext context)
        {
        }

        public override void ExitArrayDatatype([NotNull] XSharpParser.ArrayDatatypeContext context)
        {
        }

        public override void ExitArrayRank([NotNull] XSharpParser.ArrayRankContext context)
        {
        }

        public override void ExitSimpleDatatype([NotNull] XSharpParser.SimpleDatatypeContext context)
        {
        }

        public override void ExitTypeName([NotNull] XSharpParser.TypeNameContext context)
        {
        }

        public override void ExitLiteralExpression([NotNull] XSharpParser.LiteralExpressionContext context)
        {
        }

        public override void ExitLiteralArrayExpression([NotNull] XSharpParser.LiteralArrayExpressionContext context)
        {
        }

        public override void ExitIif([NotNull] XSharpParser.IifContext context)
        {
        }

        public override void ExitLiteralArray([NotNull] XSharpParser.LiteralArrayContext context)
        {
        }

        public override void ExitCodeblockExpression([NotNull] XSharpParser.CodeblockExpressionContext context)
        {
        }

        public override void ExitCodeblock([NotNull] XSharpParser.CodeblockContext context)
        {
        }

        public override void ExitCodeblockParamList([NotNull] XSharpParser.CodeblockParamListContext context)
        {
        }

        public override void ExitLiteralValue([NotNull] XSharpParser.LiteralValueContext context)
        {
        }

        public override void ExitIdentifier([NotNull] XSharpParser.IdentifierContext context)
        {
            CheckMissingToken(ref context.Token, XSharpParser.ID, "");
        }

        public override void ExitNativeType([NotNull] XSharpParser.NativeTypeContext context)
        {
        }

    }
}
