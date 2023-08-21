/*
using System;
using System.Collections.Generic;
using System.Text;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal static class PPExpressionEvaluator
    {
        internal static bool Evaluate(XSharpParser.ExpressionContext expression, XSharpPreprocessor preprocessor)
        {
            var walker = new ParseTreeWalker();
            var evaluator = new ExpressionEvaluator(preprocessor);
            walker.Walk(evaluator, expression);
            return evaluator.Result;
        }
    }
    internal class ExpressionEvaluator : XSharpBaseListener
    {
        private readonly Stack<object> _evalStack;
        private readonly XSharpPreprocessor _preprocessor;
        internal ExpressionEvaluator(XSharpPreprocessor preprocessor)
        {
            _preprocessor = preprocessor;
            _evalStack = new Stack<object>();
        }
        internal bool Result
        {
            get
            {
                if (_evalStack.Count > 0)
                {
                    return AsLogic();
                }
                return false;
            }
        }

        private long AsInt()
        {
            if (_evalStack.Count > 0)
                return AsInt(_evalStack.Pop());
            return 0;
        }

        private long AsInt(object value)
        {
            switch (value)
            {
                case bool l:
                    return l ? 1 : 0;
                case int i:
                    return i;
                case long i64:
                    return i64;
                case string s:
                    if (long.TryParse(s, out var result))
                        return result;
                    return 0;
                case double d:
                    return (long)d;
                case decimal dec:
                    return (long)dec;
                case char c:
                    return c;
            }
            return 0;

        }

        private double AsDouble(object value)
        {
            switch (value)
            {
                case bool l:
                    return l ? 1.0 : 0.0;
                case int i:
                    return i;
                case long i64:
                    return i64;
                case string s:
                    if (double.TryParse(s, out var result))
                        return result;
                    return 0;
                case double d:
                    return d;
                case decimal dec:
                    return (double)dec;
                case char c:
                    return (double)c;
            }
            return 0;

        }

        private bool AsLogic()
        {
            if (_evalStack.Count > 0)
                return AsLogic(_evalStack.Pop());
            return false;

        }
        private bool AsLogic(object value)
        {
            switch (value)
            {
                case bool l:
                    return l;
                case int i:
                    return i != 0;
                case long i64:
                    return i64 != 0;
                case string s:
                    return !string.IsNullOrEmpty(s);
                case double d:
                    return d != 0.0;
                case decimal dec:
                    return dec != 0.0m;
                case char c:
                    return c != '\0';
            }
            return false;
        }

        public override void ExitEveryRule([NotNull] ParserRuleContext context)
        {
            base.ExitEveryRule(context);
            if (context is XSharpParser.ExpressionContext expr)
            {
                switch (context)
                {
                    case XSharpParser.BinaryExpressionContext:
                    case XSharpParser.PrimaryExpressionContext:
                    case XSharpParser.PrefixExpressionContext:
                        break; // ok
                    default:
                        _preprocessor.Error((XSharpToken)context.Start, ErrorCode.ERR_PreProcessorError, $"Unexpected expression type '{expr}' ( {expr.SourceText} )" );
                        break;
                }
            }
        }

        public void doCompare(XSharpParser.BinaryExpressionContext context, object lhs, object rhs)
        {
            bool handled = true;
            if (lhs is char cl)
            {
                lhs = cl.ToString();
            }
            if (rhs is char cr)
            {
                rhs = cr.ToString();
            }
            if (lhs is string || rhs is string)
            {
                var strlhs = lhs?.ToString() ?? "";
                var strrhs = rhs?.ToString() ?? "";
                switch (context.Op.Type)
                {
                    case XSharpLexer.GT:
                        _evalStack.Push(string.Compare(strlhs, strrhs,StringComparison.Ordinal) > 0);
                        break;
                    case XSharpLexer.GTE:
                        _evalStack.Push(string.Compare(strlhs, strrhs, StringComparison.Ordinal) >= 0);
                        break;
                    case XSharpLexer.LT:
                        _evalStack.Push(string.Compare(strlhs, strrhs, StringComparison.Ordinal) < 0);
                        break;
                    case XSharpLexer.LTE:
                        _evalStack.Push(string.Compare(strlhs, strrhs, StringComparison.Ordinal) <= 0);
                        break;
                    case XSharpLexer.EEQ:
                        _evalStack.Push(string.Compare(strlhs, strrhs, StringComparison.Ordinal) == 0);
                        break;
                    case XSharpLexer.NEQ:
                    case XSharpLexer.NEQ2:
                        _evalStack.Push(string.Compare(strlhs, strrhs, StringComparison.Ordinal) != 0);
                        break;
                    default:
                        handled = false;
                        break;
                }
            }
            else if (lhs is double || rhs is double || lhs is decimal || rhs is decimal)
            {
                var dblLhs = AsDouble(lhs);
                var dblRhs = AsDouble(rhs);
                switch (context.Op.Type)
                {
                    case XSharpLexer.GT:
                        _evalStack.Push(dblLhs > dblRhs);
                        break;
                    case XSharpLexer.GTE:
                        _evalStack.Push(dblLhs >= dblRhs);
                        break;
                    case XSharpLexer.LT:
                        _evalStack.Push(dblLhs < dblRhs);
                        break;
                    case XSharpLexer.LTE:
                        _evalStack.Push(dblLhs <= dblRhs);
                        break;
                    case XSharpLexer.EEQ:
                    case XSharpLexer.EQ:
                        _evalStack.Push(dblLhs == dblRhs);
                        break;
                    case XSharpLexer.NEQ:
                    case XSharpLexer.NEQ2:
                        _evalStack.Push(dblLhs != dblRhs);
                        break;
                    default:
                        handled = false;
                        break;

                }
            }
            else if (lhs is int || rhs is int || lhs is long || rhs is long)
            {
                var intLhs = AsInt(lhs);
                var intRhs = AsInt(rhs);
                switch (context.Op.Type)
                {
                    case XSharpLexer.GT:
                        _evalStack.Push(intLhs > intRhs);
                        break;
                    case XSharpLexer.GTE:
                        _evalStack.Push(intLhs >= intRhs);
                        break;
                    case XSharpLexer.LT:
                        _evalStack.Push(intLhs < intRhs);
                        break;
                    case XSharpLexer.LTE:
                        _evalStack.Push(intLhs <= intRhs);
                        break;
                    case XSharpLexer.EEQ:
                    case XSharpLexer.EQ:
                        _evalStack.Push(intLhs == intRhs);
                        break;
                    case XSharpLexer.NEQ:
                    case XSharpLexer.NEQ2:
                        _evalStack.Push(intLhs != intRhs);
                        break;
                    default:
                        handled = false;
                        break;

                }
            }
            else
            {
                var bLhs = AsLogic(lhs);
                var bRhs = AsLogic(rhs);
                switch (context.Op.Type)
                {
                    case XSharpLexer.EEQ:
                    case XSharpLexer.EQ:
                        _evalStack.Push(bLhs == bRhs);
                        break;
                    case XSharpLexer.NEQ:
                    case XSharpLexer.NEQ2:
                        _evalStack.Push(bLhs != bRhs);
                        break;
                    default:
                        handled = false;
                        break;
                }
            }
            if (!handled)
            {
                _preprocessor.Error((XSharpToken)context.Start, ErrorCode.ERR_PreProcessorError,
                    $"Binary Expression does not support operator {context.Op.Text} for values {lhs} and {rhs}");
                _evalStack.Push(false);
            }
        }
        public void doCalc(XSharpParser.BinaryExpressionContext context, object lhs, object rhs)
        {
            bool handled = true;
            if (lhs is char cl)
            {
                lhs = cl.ToString();
            }
            if (rhs is char cr)
            {
                rhs = cr.ToString();
            }

            if (lhs is string || rhs is string)
            {
                var strlhs = lhs?.ToString() ?? "";
                var strrhs = rhs?.ToString() ?? "";
                switch (context.Op.Type)
                {
                    case XSharpLexer.PLUS:
                        _evalStack.Push(string.Concat(strlhs, strrhs));
                        break;
                    case XSharpLexer.MINUS:
                        var length = strlhs.Length + strrhs.Length;
                        var result = strlhs.Trim() + strrhs;
                        result = result.PadRight(length);
                        _evalStack.Push(result);
                        break;
                    default:
                        handled = false;
                        break;
                }
            }
            else if (lhs is double || rhs is double)
            {
                var dblLhs = AsDouble(lhs);
                var dblRhs = AsDouble(rhs);
                switch (context.Op.Type)
                {
                    case XSharpLexer.PLUS:
                        _evalStack.Push(dblLhs + dblRhs);
                        break;
                    case XSharpLexer.MINUS:
                        _evalStack.Push(dblLhs - dblRhs);
                        break;
                    case XSharpLexer.MULT:
                        _evalStack.Push(dblLhs * dblRhs);
                        break;
                    case XSharpLexer.DIV:
                        _evalStack.Push(dblLhs / dblRhs);
                        break;
                    case XSharpLexer.MOD:
                        _evalStack.Push(dblLhs % dblRhs);
                        break;
                    case XSharpLexer.EXP:
                        _evalStack.Push(Math.Pow(dblLhs, dblRhs));
                        break;

                    default:
                        handled = false;
                        break;
                }
            }
            else if (lhs is int || rhs is int || lhs is long || rhs is long)
            {
                var intLhs = AsInt(lhs);
                var intRhs = AsInt(rhs);
                switch (context.Op.Type)
                {
                    case XSharpLexer.PLUS:
                        _evalStack.Push(intLhs + intRhs);
                        break;
                    case XSharpLexer.MINUS:
                        _evalStack.Push(intLhs - intRhs);
                        break;
                    case XSharpLexer.MULT:
                        _evalStack.Push(intLhs * intRhs);
                        break;
                    case XSharpLexer.DIV:
                        _evalStack.Push(intLhs / intRhs);
                        break;
                    case XSharpLexer.MOD:
                        _evalStack.Push(intLhs % intRhs);
                        break;
                    case XSharpLexer.EXP:
                        _evalStack.Push((int)Math.Pow(intLhs, intRhs));
                        break;
                    case XSharpLexer.PIPE:
                        _evalStack.Push(intLhs | intRhs);
                        break;
                    case XSharpLexer.AMP:
                        _evalStack.Push(intLhs & intRhs);
                        break;
                    case XSharpLexer.LSHIFT:
                        if (intRhs < int.MaxValue)
                            _evalStack.Push(intLhs << (int)intRhs);
                        else
                            handled = false;
                        break;
                    case XSharpLexer.RSHIFT:
                        if (intRhs < int.MaxValue)
                            _evalStack.Push(intLhs >> (int) intRhs);
                        else
                            handled = false;
                        break;
                    default:
                        handled = false;
                        break;
                }
            }
            else
            {
                handled = false;
            }
            if (!handled)
            {
                _evalStack.Push(0);
                _preprocessor.Error((XSharpToken)context.Start, ErrorCode.ERR_PreProcessorError,
                    $"Binary Expression does not support operator {context.Op.Text} for values {lhs} and {rhs}");
            }

        }

        public void doLogical(XSharpParser.BinaryExpressionContext context, object lhs, object rhs)
        {
            var blhs = AsLogic(lhs);
            var brhs = AsLogic(rhs);
            switch (context.Op.Type)
            {
                case XSharpLexer.AMP:
                case XSharpLexer.LOGIC_AND:
                case XSharpLexer.AND:
                case XSharpLexer.FOX_AND:
                    _evalStack.Push(blhs && brhs);
                    break;
                case XSharpLexer.LOGIC_OR:
                case XSharpLexer.OR:
                case XSharpLexer.FOX_OR:
                    _evalStack.Push(blhs || brhs);
                    break;
                case XSharpLexer.LOGIC_XOR:
                case XSharpLexer.FOX_XOR:
                    _evalStack.Push(blhs ^ brhs);
                    break;
                default:
                    _evalStack.Push(false);
                    _preprocessor.Error((XSharpToken)context.Start, ErrorCode.ERR_PreProcessorError,
                        $"Binary Expression does support operator {context.Op.Text} for values {lhs} and {rhs}");
                    break;
            }
        }

        public override void ExitBinaryExpression([NotNull] XSharpParser.BinaryExpressionContext context)
        {
            if (_evalStack.Count < 2)
            {
                _preprocessor.Error((XSharpToken)context.Start, ErrorCode.ERR_PreProcessorError, "Binary Expression expects 2 operands: " + context.SourceText );
            }
            else
            {
                // values are pushed from left to right
                var rhs = _evalStack.Pop();
                var lhs = _evalStack.Pop();
                var type = context.Op.Type;
                if (type == XSharpLexer.GT && context.Gt != null)
                {
                    var token = new XSharpToken(context.Op);
                    token.Type = XSharpLexer.RSHIFT;
                    token.Text = ">>";
                    context.Op = token;
                }

                switch (context.Op.Type)
                {
                    case XSharpLexer.LT:
                    case XSharpLexer.LTE:
                    case XSharpLexer.GT:
                    case XSharpLexer.GTE:
                    case XSharpLexer.EQ:
                    case XSharpLexer.EEQ:
                    case XSharpLexer.NEQ:
                    case XSharpLexer.NEQ2:
                        doCompare(context, lhs, rhs);
                        return;
                    case XSharpLexer.PLUS:
                    case XSharpLexer.MINUS:
                    case XSharpLexer.EXP:
                    case XSharpLexer.MULT:
                    case XSharpLexer.DIV:
                    case XSharpLexer.MOD:
                    case XSharpLexer.LSHIFT:
                    case XSharpLexer.RSHIFT:
                    case XSharpLexer.PIPE:
                    case XSharpLexer.AMP:
                        doCalc(context, lhs, rhs);
                        break;

                    case XSharpLexer.LOGIC_AND:
                    case XSharpLexer.AND:
                    case XSharpLexer.FOX_AND:
                    case XSharpLexer.LOGIC_XOR:
                    case XSharpLexer.FOX_XOR:
                    case XSharpLexer.LOGIC_OR:
                    case XSharpLexer.OR:
                    case XSharpLexer.FOX_OR:
                        doLogical(context, lhs, rhs);
                        break;
                    default:
                        _preprocessor.Error((XSharpToken)context.Start, ErrorCode.ERR_PreProcessorError, "Unexpected operator " + context.Op.Text);
                        break;

                }
            }
        }

        public override void ExitPrefixExpression([NotNull] XSharpParser.PrefixExpressionContext context)
        {
            switch (context.Op.Type)
            {
                case XSharpLexer.PLUS:
                case XSharpLexer.MINUS:
                case XSharpLexer.INC:
                case XSharpLexer.TILDE:
                case XSharpLexer.DEC:
                    if (_evalStack.Count > 0)
                    {
                        var i = AsInt();
                        switch (context.Op.Type)
                        {
                            case XSharpLexer.PLUS:
                                _evalStack.Push(i);
                                break;
                            case XSharpLexer.MINUS:
                                _evalStack.Push(i);
                                break;
                            case XSharpLexer.INC:
                                _evalStack.Push(++i);
                                break;
                            case XSharpLexer.DEC:
                                _evalStack.Push(--i);
                                break;
                            case XSharpLexer.TILDE:
                                _evalStack.Push(~i);
                                break;
                        }

                    }
                    break;
                case XSharpLexer.NOT:
                case XSharpLexer.LOGIC_NOT:
                case XSharpLexer.FOX_NOT:
                    if (_evalStack.Count > 0)
                    {
                        var b = AsLogic();
                        _evalStack.Push(!b);

                    }
                    break;
                default:
                    _preprocessor.Error((XSharpToken)context.Start, ErrorCode.ERR_PreProcessorError, "Parenthesized expression should only have one element");
                    break;
            }
        }

        public override void ExitPrimaryExpression([NotNull] XSharpParser.PrimaryExpressionContext context)
        {
            switch (context.Expr)
            {
                case XSharpParser.LiteralExpressionContext:
                case XSharpParser.ParenExpressionContext:
                    break;
                default:
                    _preprocessor.Error((XSharpToken)context.Start, ErrorCode.ERR_PreProcessorError, $"Unexpected expression type '{context.Expr}' ( {context.Expr.SourceText} )");
                    break;
            }
        }
        public override void ExitLiteralValue([NotNull] XSharpParser.LiteralValueContext context)
        {
            SyntaxToken val = context.Token.SyntaxLiteralValue(_preprocessor.Options);
            _evalStack.Push(val.Value);
        }
        public override void ExitParenExpression([NotNull] XSharpParser.ParenExpressionContext context)
        {
            if (context._Exprs.Count > 1)
            {
                _preprocessor.Error((XSharpToken)context.Start, ErrorCode.ERR_PreProcessorError, "Parenthesized expression should only have one element");
            }

        }
    }
}
*/
