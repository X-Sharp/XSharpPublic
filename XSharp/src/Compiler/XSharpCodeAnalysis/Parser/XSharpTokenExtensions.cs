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
    internal static class TokenExtensions
    {
        private static string StringValue(IToken token)
        {
            return token.Text.Substring(1, token.Text.Length - 2);
        }

        private static int HexValue(IToken token)
        {
            int r = 0;
            foreach (char c in token.Text.Substring(2))
            {
                char cu = char.ToUpper(c);
                if (cu != 'U' && cu != 'L')
                {
                    r <<= 4;
                    if (cu >= '0' && cu <= '9')
                        r |= cu - '0';
                    else
                        r |= (char.ToUpper(cu) - 'A') + 10;
                }
            }
            return r;
        }

        private static int BinValue(IToken token)
        {
            int r = 0;
            foreach (char c in token.Text.Substring(2))
            {
                char cu = char.ToUpper(c);
                if (cu != 'U')
                {
                    r <<= 1;
                    if (cu == '1')
                        r |= 1;
                }
            }
            return r;
        }

        private static double RealValue(IToken token)
        {
            return double.Parse(token.Text);
        }

        private static int IntValue(IToken token)
        {
            return int.Parse(token.Text);
        }

        public static SyntaxToken Syntax(this IToken token)
        {
            SyntaxToken r;
            switch (token.Type)
            {
                case XSharpParser.ID:
                    r = SyntaxFactory.Identifier(token.Text);
                    break;

                //                case XSharpParser.ARRAY:
                //                    r = SyntaxFactory.MissingToken(SyntaxKind.);
                //                    break;
                case XSharpParser.BYTE:
                    r = SyntaxFactory.MissingToken(SyntaxKind.ByteKeyword);
                    break;
                //                case XSharpParser.CODEBLOCK:
                //                    r = SyntaxFactory.MissingToken(SyntaxKind.VoidKeyword);
                //                    break;
                //                case XSharpParser.DATE:
                //                    r = SyntaxFactory.MissingToken(SyntaxKind.VoidKeyword);
                //                    break;
                case XSharpParser.DWORD:
                    r = SyntaxFactory.MissingToken(SyntaxKind.UIntKeyword);
                    break;
                case XSharpParser.FLOAT:
                    r = SyntaxFactory.MissingToken(SyntaxKind.DoubleKeyword);
                    break;
                case XSharpParser.SHORTINT:
                    r = SyntaxFactory.MissingToken(SyntaxKind.ShortKeyword);
                    break;
                case XSharpParser.INT:
                    r = SyntaxFactory.MissingToken(SyntaxKind.IntKeyword);
                    break;
                case XSharpParser.INT64:
                    r = SyntaxFactory.MissingToken(SyntaxKind.LongKeyword);
                    break;
                case XSharpParser.LOGIC:
                    r = SyntaxFactory.MissingToken(SyntaxKind.BoolKeyword);
                    break;
                case XSharpParser.LONGINT:
                    r = SyntaxFactory.MissingToken(SyntaxKind.IntKeyword);
                    break;
                case XSharpParser.OBJECT:
                    r = SyntaxFactory.MissingToken(SyntaxKind.ObjectKeyword);
                    break;
                //                case XSharpParser.PSZ:
                //                    r = SyntaxFactory.MissingToken(SyntaxKind.VoidKeyword);
                //                    break;
                //                case XSharpParser.PTR:
                //                    r = SyntaxFactory.MissingToken(SyntaxKind.);
                //                    break;
                case XSharpParser.REAL4:
                    r = SyntaxFactory.MissingToken(SyntaxKind.FloatKeyword);
                    break;
                case XSharpParser.REAL8:
                    r = SyntaxFactory.MissingToken(SyntaxKind.DoubleKeyword);
                    break;
                case XSharpParser.STRING:
                    r = SyntaxFactory.MissingToken(SyntaxKind.StringKeyword);
                    break;
                case XSharpParser.SYMBOL:
                    r = SyntaxFactory.MissingToken(SyntaxKind.StringKeyword);
                    break;
                //                case XSharpParser.USUAL:
                //                    r = SyntaxFactory.MissingToken(SyntaxKind.VoidKeyword);
                //                    break;
                case XSharpParser.UINT64:
                    r = SyntaxFactory.MissingToken(SyntaxKind.ULongKeyword);
                    break;
                case XSharpParser.WORD:
                    r = SyntaxFactory.MissingToken(SyntaxKind.UShortKeyword);
                    break;
                case XSharpParser.VOID:
                    r = SyntaxFactory.MissingToken(SyntaxKind.VoidKeyword);
                    break;

                //                case XSharpParser.EXP:
                //                    r = SyntaxKind.None;
                //                    break;
                case XSharpParser.PLUS:
                    r = SyntaxFactory.MissingToken(SyntaxKind.PlusToken);
                    break;
                case XSharpParser.MINUS:
                    r = SyntaxFactory.MissingToken(SyntaxKind.MinusToken);
                    break;
                case XSharpParser.MULT:
                    r = SyntaxFactory.MissingToken(SyntaxKind.AsteriskToken);
                    break;
                case XSharpParser.DIV:
                    r = SyntaxFactory.MissingToken(SyntaxKind.SlashToken);
                    break;
                case XSharpParser.MOD:
                    r = SyntaxFactory.MissingToken(SyntaxKind.PercentToken);
                    break;
                case XSharpParser.LSHIFT:
                    r = SyntaxFactory.MissingToken(SyntaxKind.LessThanLessThanToken);
                    break;
                case XSharpParser.RSHIFT:
                    r = SyntaxFactory.MissingToken(SyntaxKind.GreaterThanGreaterThanToken);
                    break;
                case XSharpParser.LT:
                    r = SyntaxFactory.MissingToken(SyntaxKind.LessThanToken);
                    break;
                case XSharpParser.LTE:
                    r = SyntaxFactory.MissingToken(SyntaxKind.LessThanEqualsToken);
                    break;
                case XSharpParser.GT:
                    r = SyntaxFactory.MissingToken(SyntaxKind.GreaterThanToken);
                    break;
                case XSharpParser.GTE:
                    r = SyntaxFactory.MissingToken(SyntaxKind.GreaterThanEqualsToken);
                    break;
                case XSharpParser.EQ:
                    r = SyntaxFactory.MissingToken(SyntaxKind.EqualsEqualsToken);
                    break;
                case XSharpParser.EEQ:
                    r = SyntaxFactory.MissingToken(SyntaxKind.EqualsEqualsToken);
                    break;
                case XSharpParser.NEQ:
                    r = SyntaxFactory.MissingToken(SyntaxKind.ExclamationEqualsToken);
                    break;
                //                case XSharpParser.SUBSTR:
                //                    r = SyntaxKind.None;
                //                    break;
                case XSharpParser.AMP:
                    r = SyntaxFactory.MissingToken(SyntaxKind.AmpersandToken);
                    break;
                case XSharpParser.TILDE:
                    r = SyntaxFactory.MissingToken(SyntaxKind.CaretToken);
                    break;
                case XSharpParser.PIPE:
                    r = SyntaxFactory.MissingToken(SyntaxKind.BarToken);
                    break;
                case XSharpParser.LOGIC_AND:
                    r = SyntaxFactory.MissingToken(SyntaxKind.AmpersandAmpersandToken);
                    break;
                case XSharpParser.LOGIC_OR:
                    r = SyntaxFactory.MissingToken(SyntaxKind.BarBarToken);
                    break;

                case XSharpParser.ASSIGN_OP:
                    r = SyntaxFactory.MissingToken(SyntaxKind.EqualsToken);
                    break;
                case XSharpParser.ASSIGN_ADD:
                    r = SyntaxFactory.MissingToken(SyntaxKind.PlusEqualsToken);
                    break;
                //                case XSharpParser.ASSIGN_EXP:
                //                    kind = SyntaxKind.None;
                //                    break;
                case XSharpParser.ASSIGN_MUL:
                    r = SyntaxFactory.MissingToken(SyntaxKind.AsteriskEqualsToken);
                    break;
                case XSharpParser.ASSIGN_DIV:
                    r = SyntaxFactory.MissingToken(SyntaxKind.SlashEqualsToken);
                    break;
                case XSharpParser.ASSIGN_MOD:
                    r = SyntaxFactory.MissingToken(SyntaxKind.PercentEqualsToken);
                    break;
                case XSharpParser.ASSIGN_BITAND:
                    r = SyntaxFactory.MissingToken(SyntaxKind.AmpersandToken);
                    break;
                case XSharpParser.ASSIGN_BITOR:
                    r = SyntaxFactory.MissingToken(SyntaxKind.BarEqualsToken);
                    break;
                case XSharpParser.ASSIGN_LSHIFT:
                    r = SyntaxFactory.MissingToken(SyntaxKind.LessThanLessThanEqualsToken);
                    break;
                case XSharpParser.ASSIGN_RSHIFT:
                    r = SyntaxFactory.MissingToken(SyntaxKind.GreaterThanGreaterThanEqualsToken);
                    break;
                case XSharpParser.ASSIGN_XOR:
                    r = SyntaxFactory.MissingToken(SyntaxKind.CaretEqualsToken);
                    break;

                case XSharpParser.TRUE_CONST:
                    r = SyntaxFactory.MissingToken(SyntaxKind.TrueKeyword);
                    break;
                case XSharpParser.FALSE_CONST:
                    r = SyntaxFactory.MissingToken(SyntaxKind.FalseKeyword);
                    break;
                case XSharpParser.STRING_CONST:
                    r = SyntaxFactory.Literal(null, token.Text, StringValue(token), null);
                    break;
                case XSharpParser.SYMBOL_CONST:
                    r = SyntaxFactory.Literal(null, token.Text, token.Text, null);
                    break;
                case XSharpParser.HEX_CONST:
                    r = SyntaxFactory.Literal(null, token.Text, HexValue(token), null);
                    break;
                case XSharpParser.BIN_CONST:
                    r = SyntaxFactory.Literal(null, token.Text, BinValue(token), null);
                    break;
                case XSharpParser.REAL_CONST:
                    r = SyntaxFactory.Literal(null, token.Text, RealValue(token), null);
                    break;
                case XSharpParser.INT_CONST:
                    r = SyntaxFactory.Literal(null, token.Text, IntValue(token), null);
                    break;
                case XSharpParser.DATE_CONST:
                    r = SyntaxFactory.Literal(null, token.Text, token.Text, null);
                    break;
                case XSharpParser.NIL:
                case XSharpParser.NULL:
                case XSharpParser.NULL_ARRAY:
                case XSharpParser.NULL_CODEBLOCK:
                case XSharpParser.NULL_DATE:
                case XSharpParser.NULL_OBJECT:
                case XSharpParser.NULL_PSZ:
                case XSharpParser.NULL_PTR:
                case XSharpParser.NULL_STRING:
                case XSharpParser.NULL_SYMBOL:
                    r = SyntaxFactory.MissingToken(SyntaxKind.NullKeyword);
                    break;
                default:
                    throw new InvalidOperationException();
            }
            return r;
        }

        public static SyntaxKind ExpressionKind(this IToken token)
        {
            SyntaxKind r;
            switch (token.Type)
            {
                case XSharpParser.ID:
                    r = SyntaxKind.IdentifierName;
                    break;

                case XSharpParser.ARRAY:
                case XSharpParser.BYTE:
                case XSharpParser.CODEBLOCK:
                case XSharpParser.DATE:
                case XSharpParser.DWORD:
                case XSharpParser.FLOAT:
                case XSharpParser.SHORTINT:
                case XSharpParser.INT:
                case XSharpParser.INT64:
                case XSharpParser.LOGIC:
                case XSharpParser.LONGINT:
                case XSharpParser.OBJECT:
                case XSharpParser.PSZ:
                case XSharpParser.PTR:
                case XSharpParser.REAL4:
                case XSharpParser.REAL8:
                case XSharpParser.STRING:
                case XSharpParser.SYMBOL:
                case XSharpParser.USUAL:
                case XSharpParser.UINT64:
                case XSharpParser.WORD:
                case XSharpParser.VOID:
                    r = SyntaxKind.None;
                    break;

                //                case XSharpParser.EXP:
                //                    r = SyntaxKind.None;
                //                    break;
                case XSharpParser.PLUS:
                    r = SyntaxKind.AddExpression;
                    break;
                case XSharpParser.MINUS:
                    r = SyntaxKind.SubtractExpression;
                    break;
                case XSharpParser.MULT:
                    r = SyntaxKind.MultiplyExpression;
                    break;
                case XSharpParser.DIV:
                    r = SyntaxKind.DivideExpression;
                    break;
                case XSharpParser.MOD:
                    r = SyntaxKind.ModuloExpression;
                    break;
                case XSharpParser.LSHIFT:
                    r = SyntaxKind.LeftShiftExpression;
                    break;
                case XSharpParser.RSHIFT:
                    r = SyntaxKind.RightShiftExpression;
                    break;
                case XSharpParser.LT:
                    r = SyntaxKind.LessThanExpression;
                    break;
                case XSharpParser.LTE:
                    r = SyntaxKind.LessThanOrEqualExpression;
                    break;
                case XSharpParser.GT:
                    r = SyntaxKind.GreaterThanExpression;
                    break;
                case XSharpParser.GTE:
                    r = SyntaxKind.GreaterThanOrEqualExpression;
                    break;
                case XSharpParser.EQ:
                    r = SyntaxKind.EqualsExpression;
                    break;
                case XSharpParser.EEQ:
                    r = SyntaxKind.EqualsExpression;
                    break;
                case XSharpParser.NEQ:
                    r = SyntaxKind.NotEqualsExpression;
                    break;
                //                case XSharpParser.SUBSTR:
                //                    r = SyntaxKind.None;
                //                    break;
                case XSharpParser.AMP:
                    r = SyntaxKind.BitwiseAndExpression;
                    break;
                case XSharpParser.TILDE:
                    r = SyntaxKind.ExclusiveOrExpression;
                    break;
                case XSharpParser.PIPE:
                    r = SyntaxKind.BitwiseOrExpression;
                    break;
                case XSharpParser.LOGIC_AND:
                    r = SyntaxKind.LogicalAndExpression;
                    break;
                case XSharpParser.LOGIC_OR:
                    r = SyntaxKind.LogicalOrExpression;
                    break;

                case XSharpParser.ASSIGN_OP:
                    r = SyntaxKind.SimpleAssignmentExpression;
                    break;
                case XSharpParser.ASSIGN_ADD:
                    r = SyntaxKind.AddAssignmentExpression;
                    break;
                //                case XSharpParser.ASSIGN_EXP:
                //                    r = SyntaxKind.None;
                //                    break;
                case XSharpParser.ASSIGN_MUL:
                    r = SyntaxKind.MultiplyAssignmentExpression;
                    break;
                case XSharpParser.ASSIGN_DIV:
                    r = SyntaxKind.DivideAssignmentExpression;
                    break;
                case XSharpParser.ASSIGN_MOD:
                    r = SyntaxKind.ModuloAssignmentExpression;
                    break;
                case XSharpParser.ASSIGN_BITAND:
                    r = SyntaxKind.AndAssignmentExpression;
                    break;
                case XSharpParser.ASSIGN_BITOR:
                    r = SyntaxKind.OrAssignmentExpression;
                    break;
                case XSharpParser.ASSIGN_LSHIFT:
                    r = SyntaxKind.LeftShiftAssignmentExpression;
                    break;
                case XSharpParser.ASSIGN_RSHIFT:
                    r = SyntaxKind.RightShiftAssignmentExpression;
                    break;
                case XSharpParser.ASSIGN_XOR:
                    r = SyntaxKind.ExclusiveOrAssignmentExpression;
                    break;

                case XSharpParser.TRUE_CONST:
                    r = SyntaxKind.TrueLiteralExpression;
                    break;
                case XSharpParser.FALSE_CONST:
                    r = SyntaxKind.FalseLiteralExpression;
                    break;
                case XSharpParser.STRING_CONST:
                    r = SyntaxKind.StringLiteralExpression;
                    break;
                case XSharpParser.SYMBOL_CONST:
                    r = SyntaxKind.StringLiteralExpression;
                    break;
                case XSharpParser.HEX_CONST:
                    r = SyntaxKind.NumericLiteralExpression;
                    break;
                case XSharpParser.BIN_CONST:
                    r = SyntaxKind.NumericLiteralExpression;
                    break;
                case XSharpParser.REAL_CONST:
                    r = SyntaxKind.NumericLiteralExpression;
                    break;
                case XSharpParser.INT_CONST:
                    r = SyntaxKind.NumericLiteralExpression;
                    break;
                case XSharpParser.DATE_CONST:
                    r = SyntaxKind.NumericLiteralExpression;
                    break;
                case XSharpParser.NIL:
                case XSharpParser.NULL:
                case XSharpParser.NULL_ARRAY:
                case XSharpParser.NULL_CODEBLOCK:
                case XSharpParser.NULL_DATE:
                case XSharpParser.NULL_OBJECT:
                case XSharpParser.NULL_PSZ:
                case XSharpParser.NULL_PTR:
                case XSharpParser.NULL_STRING:
                case XSharpParser.NULL_SYMBOL:
                    r = SyntaxKind.NullLiteralExpression;
                    break;
                default:
                    throw new InvalidOperationException();
            }
            return r;
        }
    }
}