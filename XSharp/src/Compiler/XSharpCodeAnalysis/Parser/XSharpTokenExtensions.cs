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

        public static SyntaxToken SyntaxIdentifier(this IToken token)
        {
            (token as CommonToken).Type = XSharpParser.ID;
            var r = SyntaxFactory.Identifier(token.Text);
            r.XNode = new TerminalNodeImpl(token);
            return r;
        }

        public static SyntaxToken SyntaxKeywordIdentifier(this IToken token)
        {
            var r = SyntaxFactory.Identifier(token.Text);
            r.XNode = new TerminalNodeImpl(token);
            return r;
        }

        public static SyntaxToken SyntaxNativeType(this IToken token)
        {
            SyntaxToken r;
            switch (token.Type)
            {
                case XSharpParser.BYTE:
                    r = SyntaxFactory.MissingToken(SyntaxKind.ByteKeyword);
                    break;
                //case XSharpParser.CODEBLOCK:
                //    r = SyntaxFactory.MissingToken(SyntaxKind.VoidKeyword);
                //    break;
                //case XSharpParser.DATE:
                //    r = SyntaxFactory.MissingToken(SyntaxKind.VoidKeyword);
                //    break;
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
                //case XSharpParser.PSZ:
                //    r = SyntaxFactory.MissingToken(SyntaxKind.VoidKeyword);
                //    break;
                //case XSharpParser.PTR:
                //    r = SyntaxFactory.MissingToken(SyntaxKind.);
                //    break;
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
                //case XSharpParser.USUAL:
                //    r = SyntaxFactory.MissingToken(SyntaxKind.VoidKeyword);
                //    break;
                case XSharpParser.UINT64:
                    r = SyntaxFactory.MissingToken(SyntaxKind.ULongKeyword);
                    break;
                case XSharpParser.WORD:
                    r = SyntaxFactory.MissingToken(SyntaxKind.UShortKeyword);
                    break;
                case XSharpParser.VOID:
                    r = SyntaxFactory.MissingToken(SyntaxKind.VoidKeyword);
                    break;
                //case XSharpParser.ARRAY:
                //    r = SyntaxFactory.MissingToken(SyntaxKind.);
                //    break;
                default:
                    r = SyntaxFactory.MissingToken(SyntaxKind.BadToken).WithAdditionalDiagnostics(
                        new SyntaxDiagnosticInfo(0, token.Text.Length, ErrorCode.ERR_SyntaxError, token));
                    break;
            }
            r.XNode = new TerminalNodeImpl(token);
            return r;
        }

        public static SyntaxToken SyntaxLiteralValue(this IToken token)
        {
            SyntaxToken r;
            switch (token.Type)
            {
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
                    r = SyntaxFactory.MissingToken(SyntaxKind.BadToken).WithAdditionalDiagnostics(
                        new SyntaxDiagnosticInfo(0, token.Text.Length, ErrorCode.ERR_SyntaxError, token));
                    break;
            }
            r.XNode = new TerminalNodeImpl(token);
            return r;
        }

        public static SyntaxToken SyntaxOp(this IToken token)
        {
            SyntaxToken r;
            switch (token.Type)
            {
                //case XSharpParser.EXP:
                //    r = SyntaxKind.None;
                //    break;
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
                //case XSharpParser.SUBSTR:
                //    r = SyntaxKind.None;
                //    break;
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
                //case XSharpParser.ASSIGN_EXP:
                //    kind = SyntaxKind.None;
                //    break;
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
                default:
                    r = SyntaxFactory.MissingToken(SyntaxKind.BadToken).WithAdditionalDiagnostics(
                        new SyntaxDiagnosticInfo(0, token.Text.Length, ErrorCode.ERR_SyntaxError, token));
                    break;
            }
            r.XNode = new TerminalNodeImpl(token);
            return r;
        }

        public static SyntaxToken SyntaxKeyword(this IToken token)
        {
            SyntaxToken r;
            switch (token.Type)
            {
                case XSharpParser.ABSTRACT:
                    r = SyntaxFactory.MissingToken(SyntaxKind.AbstractKeyword);
                    break;
                case XSharpParser.STATIC:
                    r = SyntaxFactory.MissingToken(SyntaxKind.StaticKeyword);
                    break;
                case XSharpParser.INTERNAL:
                    r = SyntaxFactory.MissingToken(SyntaxKind.InternalKeyword);
                    break;
                case XSharpParser.PUBLIC:
                    r = SyntaxFactory.MissingToken(SyntaxKind.PublicKeyword);
                    break;
                case XSharpParser.EXPORT:
                    r = SyntaxFactory.MissingToken(SyntaxKind.PublicKeyword);
                    break;
                case XSharpParser.PRIVATE:
                    r = SyntaxFactory.MissingToken(SyntaxKind.PrivateKeyword);
                    break;
                case XSharpParser.HIDDEN:
                    r = SyntaxFactory.MissingToken(SyntaxKind.PrivateKeyword);
                    break;
                case XSharpParser.NEW:
                    r = SyntaxFactory.MissingToken(SyntaxKind.NewKeyword);
                    break;
                case XSharpParser.PROTECTED:
                    r = SyntaxFactory.MissingToken(SyntaxKind.ProtectedKeyword);
                    break;
                case XSharpParser.PARTIAL:
                    r = SyntaxFactory.MissingToken(SyntaxKind.PartialKeyword);
                    break;
                case XSharpParser.EXTERN:
                    r = SyntaxFactory.MissingToken(SyntaxKind.ExternKeyword);
                    break;
                case XSharpParser.UNSAFE:
                    r = SyntaxFactory.MissingToken(SyntaxKind.UnsafeKeyword);
                    break;
                case XSharpParser.CHECKED:
                    r = SyntaxFactory.MissingToken(SyntaxKind.CheckedKeyword);
                    break;
                case XSharpParser.UNCHECKED:
                    r = SyntaxFactory.MissingToken(SyntaxKind.UncheckedKeyword);
                    break;
                case XSharpParser.ASYNC:
                    r = SyntaxFactory.MissingToken(SyntaxKind.AsyncKeyword);
                    break;
                case XSharpParser.AWAIT:
                    r = SyntaxFactory.MissingToken(SyntaxKind.AwaitKeyword);
                    break;
                case XSharpParser.CASE:
                    r = SyntaxFactory.MissingToken(SyntaxKind.CaseKeyword);
                    break;
                case XSharpParser.DEFAULT:
                    r = SyntaxFactory.MissingToken(SyntaxKind.DefaultKeyword);
                    break;
                case XSharpParser.OTHERWISE:
                    r = SyntaxFactory.MissingToken(SyntaxKind.DefaultKeyword);
                    break;
                case XSharpParser.REF:
                    r = SyntaxFactory.MissingToken(SyntaxKind.RefKeyword);
                    break;
                case XSharpParser.OUT:
                    r = SyntaxFactory.MissingToken(SyntaxKind.OutKeyword);
                    break;
                case XSharpParser.CONST:
                    r = SyntaxFactory.MissingToken(SyntaxKind.ConstKeyword);
                    break;
                case XSharpParser.CLASS:
                    r = SyntaxFactory.MissingToken(SyntaxKind.ClassKeyword);
                    break;
                case XSharpParser.STRUCTURE:
                    r = SyntaxFactory.MissingToken(SyntaxKind.StructKeyword);
                    break;
                case XSharpParser.SEALED:
                    r = SyntaxFactory.MissingToken(SyntaxKind.SealedKeyword);
                    break;
                case XSharpParser.VIRTUAL:
                    r = SyntaxFactory.MissingToken(SyntaxKind.VoidKeyword);
                    break;
                case XSharpParser.SELF:
                    r = SyntaxFactory.MissingToken(SyntaxKind.ThisKeyword);
                    break;
                case XSharpParser.USING:
                    r = SyntaxFactory.MissingToken(SyntaxKind.UsingKeyword);
                    break;
                case XSharpParser.SUPER:
                    r = SyntaxFactory.MissingToken(SyntaxKind.BaseKeyword);
                    break;
                case XSharpParser.VAR:
                    r = SyntaxFactory.Identifier("Xs$var");
                    break;
                case XSharpParser.THROW:
                    r = SyntaxFactory.MissingToken(SyntaxKind.ThrowKeyword);
                    break;
                case XSharpParser.TRY:
                    r = SyntaxFactory.MissingToken(SyntaxKind.TryKeyword);
                    break;
                case XSharpParser.CATCH:
                    r = SyntaxFactory.MissingToken(SyntaxKind.CatchKeyword);
                    break;
                case XSharpParser.FINALLY:
                    r = SyntaxFactory.MissingToken(SyntaxKind.FinallyKeyword);
                    break;
                case XSharpParser.YIELD:
                    r = SyntaxFactory.MissingToken(SyntaxKind.YieldKeyword);
                    break;
                case XSharpParser.VOLATILE:
                    r = SyntaxFactory.MissingToken(SyntaxKind.VolatileKeyword);
                    break;
                case XSharpParser.INITONLY:
                    r = SyntaxFactory.MissingToken(SyntaxKind.ReadOnlyKeyword);
                    break;
                case XSharpParser.IMPLICIT:
                    r = SyntaxFactory.MissingToken(SyntaxKind.ImplicitKeyword);
                    break;
                case XSharpParser.EXPLICIT:
                    r = SyntaxFactory.MissingToken(SyntaxKind.ExplicitKeyword);
                    break;
                case XSharpParser.INSTANCE:
                    r = SyntaxFactory.MissingToken(SyntaxKind.None);
                    break;
                case XSharpParser.ACCESS:
                case XSharpParser.ALIGN:
                case XSharpParser.AS:
                case XSharpParser.ASSIGN:
                case XSharpParser.BEGIN:
                case XSharpParser.BREAK:
                //case XSharpParser.CASE:
                case XSharpParser.CAST:
                case XSharpParser.CLIPPER:
                case XSharpParser.DEFINE:
                case XSharpParser.DIM:
                case XSharpParser.DLL:
                case XSharpParser.DO:
                case XSharpParser.DOWNTO:
                case XSharpParser.ELSE:
                case XSharpParser.ELSEIF:
                case XSharpParser.END:
                case XSharpParser.ENDCASE:
                case XSharpParser.ENDDO:
                case XSharpParser.ENDIF:
                case XSharpParser.EXIT:
                //case XSharpParser.EXPORT:
                case XSharpParser.FASTCALL:
                case XSharpParser.FIELD:
                case XSharpParser.FOR:
                case XSharpParser.FUNCTION:
                case XSharpParser.GLOBAL:
                //case XSharpParser.HIDDEN:
                case XSharpParser.IF:
                case XSharpParser.IIF:
                case XSharpParser.INHERIT:
                case XSharpParser.IN:
                case XSharpParser.IS:
                case XSharpParser.LOCAL:
                case XSharpParser.LOOP:
                case XSharpParser.MEMBER:
                case XSharpParser.METHOD:
                case XSharpParser.NEXT:
                //case XSharpParser.OTHERWISE:
                case XSharpParser.PASCAL:
                //case XSharpParser.PRIVATE:
                case XSharpParser.PROCEDURE:
                //case XSharpParser.PROTECTED:
                //case XSharpParser.PUBLIC:
                case XSharpParser.RECOVER:
                case XSharpParser.RETURN:
                case XSharpParser.SEQUENCE:
                case XSharpParser.SIZEOF:
                case XSharpParser.STEP:
                case XSharpParser.STRICT:
                case XSharpParser.THISCALL:
                case XSharpParser.TO:
                case XSharpParser.TYPEOF:
                case XSharpParser.UNION:
                case XSharpParser.UPTO:
                case XSharpParser.WHILE:
                case XSharpParser.AUTO:
                case XSharpParser.CONSTRUCTOR:
                //case XSharpParser.CONST:
                case XSharpParser.DELEGATE:
                case XSharpParser.DESTRUCTOR:
                case XSharpParser.ENUM:
                case XSharpParser.EVENT:
                case XSharpParser.FOREACH:
                case XSharpParser.GET:
                case XSharpParser.IMPLEMENTS:
                case XSharpParser.IMPLIED:
                case XSharpParser.INTERFACE:
                //case XSharpParser.INTERNAL:
                case XSharpParser.LOCK:
                case XSharpParser.NAMESPACE:
                //case XSharpParser.NEW:
                case XSharpParser.OPERATOR:
                //case XSharpParser.OUT:
                //case XSharpParser.PARTIAL:
                case XSharpParser.PROPERTY:
                case XSharpParser.REPEAT:
                case XSharpParser.SCOPE:
                case XSharpParser.SET:
                case XSharpParser.UNTIL:
                case XSharpParser.VALUE:
                case XSharpParser.VOSTRUCT:
                case XSharpParser.ASSEMBLY:
                //case XSharpParser.ASYNC:
                //case XSharpParser.AWAIT:
                //case XSharpParser.CHECKED:
                //case XSharpParser.DEFAULT:
                //case XSharpParser.EXTERN:
                case XSharpParser.MODULE:
                case XSharpParser.SWITCH:
                //case XSharpParser.UNCHECKED:
                //case XSharpParser.UNSAFE:
                case XSharpParser.WHERE:
                    r = SyntaxFactory.Identifier(token.Text);
                    break;
                default:
                    r = SyntaxFactory.MissingToken(SyntaxKind.BadToken).WithAdditionalDiagnostics(
                        new SyntaxDiagnosticInfo(0, token.Text.Length, ErrorCode.ERR_SyntaxError, token));
                    break;
            }
            r.XNode = new TerminalNodeImpl(token);
            return r;
        }

        public static SyntaxKind SwitchLabelKind(this IToken token)
        {
            SyntaxKind r;
            switch (token.Type)
            {
                case XSharpParser.CASE:
                    r = SyntaxKind.CaseSwitchLabel;
                    break;
                case XSharpParser.DEFAULT:
                    r = SyntaxKind.DefaultSwitchLabel;
                    break;
                case XSharpParser.OTHERWISE:
                    r = SyntaxKind.DefaultSwitchLabel;
                    break;
                default:
                    throw new InvalidOperationException();
            }
            return r;
        }

        public static SyntaxKind ConstraintKind(this IToken token)
        {
            SyntaxKind r;
            switch (token.Type)
            {
                case XSharpParser.CLASS:
                    r = SyntaxKind.ClassConstraint;
                    break;
                case XSharpParser.STRUCTURE:
                    r = SyntaxKind.StructConstraint;
                    break;
                default:
                    throw new InvalidOperationException();
            }
            return r;
        }

        public static SyntaxKind AccessorKind(this IToken token)
        {
            SyntaxKind r;
            switch (token.Type)
            {
                case XSharpParser.GET:
                    r = SyntaxKind.GetAccessorDeclaration;
                    break;
                case XSharpParser.SET:
                    r = SyntaxKind.SetAccessorDeclaration;
                    break;
                default:
                    throw new InvalidOperationException();
            }
            return r;
        }

        public static SyntaxKind StatementKind(this IToken token)
        {
            SyntaxKind r;
            switch (token.Type)
            {
                case XSharpParser.UNSAFE:
                    r = SyntaxKind.UnsafeStatement;
                    break;
                case XSharpParser.CHECKED:
                    r = SyntaxKind.CheckedStatement;
                    break;
                case XSharpParser.UNCHECKED:
                    r = SyntaxKind.UncheckedStatement;
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
                case XSharpParser.CHECKED:
                    r = SyntaxKind.CheckedExpression;
                    break;
                case XSharpParser.UNCHECKED:
                    r = SyntaxKind.UncheckedExpression;
                    break;
                default:
                    throw new InvalidOperationException();
            }
            return r;
        }

        public static SyntaxKind ExpressionKindLiteral(this IToken token)
        {
            SyntaxKind r;
            switch (token.Type)
            {
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

        public static SyntaxKind ExpressionKindBinaryOp(this IToken token)
        {
            SyntaxKind r;
            switch (token.Type)
            {
                //case XSharpParser.EXP:
                //    r = SyntaxKind.None;
                //    break;
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
                //case XSharpParser.SUBSTR:
                //    r = SyntaxKind.None;
                //    break;
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
                //case XSharpParser.ASSIGN_EXP:
                //    r = SyntaxKind.None;
                //    break;
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
                default:
                    throw new InvalidOperationException();
            }
            return r;
        }

        public static SyntaxKind ExpressionKindPrefixOp(this IToken token)
        {
            SyntaxKind r;
            switch (token.Type)
            {
                case XSharpParser.PLUS:
                    r = SyntaxKind.UnaryPlusExpression;
                    break;
                case XSharpParser.MINUS:
                    r = SyntaxKind.UnaryMinusExpression;
                    break;
                case XSharpParser.TILDE:
                    r = SyntaxKind.BitwiseNotExpression;
                    break;
                case XSharpParser.ADDROF:
                    r = SyntaxKind.AddressOfExpression;
                    break;
                case XSharpParser.INC:
                    r = SyntaxKind.PreIncrementExpression;
                    break;
                case XSharpParser.DEC:
                    r = SyntaxKind.PreDecrementExpression;
                    break;
                case XSharpParser.LOGIC_NOT:
                    r = SyntaxKind.LogicalNotExpression;
                    break;
                case XSharpParser.LOGIC_XOR:
                    r = SyntaxKind.BitwiseNotExpression;
                    break;
                case XSharpParser.NOT:
                    r = SyntaxKind.LogicalNotExpression;
                    break;
                default:
                    throw new InvalidOperationException();
            }
            return r;
        }

        public static SyntaxKind ExpressionKindPostfixOp(this IToken token)
        {
            SyntaxKind r;
            switch (token.Type)
            {
                case XSharpParser.INC:
                    r = SyntaxKind.PostIncrementExpression;
                    break;
                case XSharpParser.DEC:
                    r = SyntaxKind.PostDecrementExpression;
                    break;
                default:
                    throw new InvalidOperationException();
            }
            return r;
        }

        public static void AddCheckUnique(this SyntaxListBuilder list, SyntaxToken t)
        {
            if (t.Kind != SyntaxKind.None) {
                if(list.Any(t.Kind)) {
                    t = t.WithAdditionalDiagnostics(
                        new SyntaxDiagnosticInfo(t.GetLeadingTriviaWidth(), t.Width, ErrorCode.ERR_DuplicateModifier, t));
                }
                list.Add(t);
            }
        }
    }
}