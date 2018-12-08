using System;
using System.Collections.Generic;
using System.Linq;
using System.Diagnostics;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.MacroCompiler
{
    using Syntax;
    using static Compilation;

    partial class Parser
    {
        // Input source
        IList<Token> _Input;

        // Configuration
        MacroOptions _options;
        bool AllowMissingSyntax { get { return _options.AllowMissingSyntax; } }

        // State
        int _index = 0;

        internal Parser(IList<Token> input, MacroOptions options)
        {
            _Input = input;
            _options = options;
        }

        Token Lt()
        {
            return _index < _Input.Count ? _Input[_index] : null;
        }

        TokenType La()
        {
            return _index < _Input.Count ? _Input[_index].type : TokenType.EOF;
        }

        TokenType La(int n)
        {
            return (_index + n - 1) < _Input.Count ? _Input[_index + n - 1].type : TokenType.EOF;
        }

        bool Eoi()
        {
            return _index >= _Input.Count;
        }

        void Consume()
        {
            _index++;
        }

        void Consume(int n)
        {
            _index += n;
        }

        int Mark()
        {
            return _index;
        }

        void Rewind(int pos)
        {
            _index = pos;
        }

        Token ConsumeAndGet()
        {
            var t = _Input[_index];
            _index++;
            return t;
        }

        bool Expect(TokenType c)
        {
            if (La() == c)
            {
                Consume();
                return true;
            }
            return false;
        }

        bool ExpectAndGet(TokenType c, out Token t)
        {
            if (La() == c)
            {
                t = ConsumeAndGet();
                return true;
            }
            t = null;
            return false;
        }

        bool Require(bool p, ErrorCode error, params object[] args)
        {
            if (!p)
            {
                throw Error(Lt(), error, args);
            }
            return p;
        }

        T Require<T>(T n, ErrorCode error, params object[] args)
        {
            if (n == null)
            {
                throw Error(Lt(), error, args);
            }
            return n;
        }

        T RequireEnd<T>(T n, ErrorCode error, params object[] args)
        {
            Expect(TokenType.EOS);
            if (La() != TokenType.EOF)
            {
                throw Error(Lt(), error, args);
            }
            return n;
        }

        internal bool CanParseTerm()
        {
            switch (La())
            {
                case TokenType.ID:
                case TokenType.SELF:
                case TokenType.SUPER:
                case TokenType.CHECKED:
                case TokenType.UNCHECKED:
                case TokenType.TYPEOF:
                case TokenType.SIZEOF:
                case TokenType.DEFAULT:
                case TokenType.TRUE_CONST:
                case TokenType.FALSE_CONST:
                case TokenType.CHAR_CONST:
                case TokenType.STRING_CONST:
                case TokenType.ESCAPED_STRING_CONST:
                case TokenType.INTERPOLATED_STRING_CONST:
                case TokenType.SYMBOL_CONST:
                case TokenType.HEX_CONST:
                case TokenType.BIN_CONST:
                case TokenType.REAL_CONST:
                case TokenType.INT_CONST:
                case TokenType.DATE_CONST:
                case TokenType.NIL:
                case TokenType.NULL:
                case TokenType.NULL_ARRAY:
                case TokenType.NULL_CODEBLOCK:
                case TokenType.NULL_DATE:
                case TokenType.NULL_OBJECT:
                case TokenType.NULL_PSZ:
                case TokenType.NULL_PTR:
                case TokenType.NULL_STRING:
                case TokenType.NULL_SYMBOL:
                case TokenType.INCOMPLETE_STRING_CONST:
                case TokenType.ARRAY:
                case TokenType.CODEBLOCK:
                case TokenType.DATE:
                case TokenType.FLOAT:
                case TokenType.PSZ:
                case TokenType.SYMBOL:
                case TokenType.USUAL:
                case TokenType.BYTE:
                case TokenType.CHAR:
                case TokenType.DATETIME:
                case TokenType.DECIMAL:
                case TokenType.DWORD:
                case TokenType.DYNAMIC:
                case TokenType.INT:
                case TokenType.INT64:
                case TokenType.LOGIC:
                case TokenType.LONGINT:
                case TokenType.OBJECT:
                case TokenType.PTR:
                case TokenType.REAL4:
                case TokenType.REAL8:
                case TokenType.SHORTINT:
                case TokenType.STRING:
                case TokenType.UINT64:
                case TokenType.VOID:
                case TokenType.WORD:
                case TokenType.LPAREN:
                case TokenType.LCURLY:
                case TokenType.IIF:
                case TokenType.FIELD_:
                    return true;
                case TokenType.LT:
                    {
                        var p = Mark();
                        var la = ParseTypedLiteralArray();
                        Rewind(p);
                        return la != null;
                    }
                default:
                    return TokenAttr.IsSoftKeyword(La());
            }
        }

        internal Expr ParseTerm()
        {
#if DEBUG
            if (!CanParseTerm())
                return null;
#endif
            var t = La();
            switch (t)
            {
                case TokenType.ID:
                    return ParseFieldAlias() ?? ParseNameOrCtorCallOrSpecialFunc(ParseTypeSuffix(ParseQualifiedName()));
                case TokenType.SELF:
                    return new SelfExpr(ConsumeAndGet());
                case TokenType.SUPER:
                    return new SuperExpr(ConsumeAndGet());
                case TokenType.CHECKED:
                    {
                        var o = ConsumeAndGet();
                        return new CheckedExpr(ParseParenExpr(), o);
                    }
                case TokenType.UNCHECKED:
                    {
                        var o = ConsumeAndGet();
                        return new UncheckedExpr(ParseParenExpr(), o);
                    }
                case TokenType.TYPEOF:
                    {
                        var o = ConsumeAndGet();
                        return new TypeOfExpr(ParseParenType(), o);
                    }
                case TokenType.SIZEOF:
                    {
                        var o = ConsumeAndGet();
                        return new SizeOfExpr(ParseParenType(), o);
                    }
                case TokenType.DEFAULT:
                    {
                        var o = ConsumeAndGet();
                        return new DefaultExpr(ParseParenType(), o);
                    }
                case TokenType.TRUE_CONST:
                case TokenType.FALSE_CONST:
                case TokenType.CHAR_CONST:
                case TokenType.STRING_CONST:
                case TokenType.ESCAPED_STRING_CONST:
                case TokenType.INTERPOLATED_STRING_CONST:
                case TokenType.SYMBOL_CONST:
                case TokenType.HEX_CONST:
                case TokenType.BIN_CONST:
                case TokenType.REAL_CONST:
                case TokenType.INT_CONST:
                case TokenType.DATE_CONST:
                case TokenType.NIL:
                case TokenType.NULL:
                case TokenType.NULL_ARRAY:
                case TokenType.NULL_CODEBLOCK:
                case TokenType.NULL_DATE:
                case TokenType.NULL_OBJECT:
                case TokenType.NULL_PSZ:
                case TokenType.NULL_PTR:
                case TokenType.NULL_STRING:
                case TokenType.NULL_SYMBOL:
                    return new LiteralExpr(ConsumeAndGet());
                case TokenType.INCOMPLETE_STRING_CONST:
                    throw Error(Lt(), ErrorCode.UnterminatedString);
                case TokenType.ARRAY:
                case TokenType.CODEBLOCK:
                case TokenType.DATE:
                case TokenType.FLOAT:
                case TokenType.PSZ:
                case TokenType.SYMBOL:
                case TokenType.USUAL:
                    return ParseNativeTypeOrCast(new NativeTypeExpr(ConsumeAndGet()));
                case TokenType.BYTE:
                case TokenType.CHAR:
                case TokenType.DATETIME:
                case TokenType.DECIMAL:
                case TokenType.DWORD:
                case TokenType.DYNAMIC:
                case TokenType.INT:
                case TokenType.INT64:
                case TokenType.LOGIC:
                case TokenType.LONGINT:
                case TokenType.OBJECT:
                case TokenType.PTR:
                case TokenType.REAL4:
                case TokenType.REAL8:
                case TokenType.SHORTINT:
                case TokenType.STRING:
                case TokenType.UINT64:
                case TokenType.VOID:
                case TokenType.WORD:
                    return ParseNativeTypeOrCast(new NativeTypeExpr(ConsumeAndGet()));
                case TokenType.LPAREN:
                    return ParseParenExpr();
                case TokenType.LCURLY:
                    return ParseLiteralArray();
                case TokenType.LT:
                    return ParseTypedLiteralArray();
                case TokenType.IF:
                case TokenType.IIF:
                    return ParseIif();
                case TokenType.FIELD_:
                    return ParseFieldAlias();
                // TODO nvk: PTR LPAREN Type=datatype COMMA Expr=expression RPAREN		#voCastPtrExpression	// PTR( typeName, expr )
                // TODO nvk: Op=(VO_AND | VO_OR | VO_XOR | VO_NOT) LPAREN Exprs+=expression (COMMA Exprs+=expression)* RPAREN							#intrinsicExpression	// _Or(expr, expr, expr)
                // TODO nvk: AMP LPAREN Expr=expression RPAREN							#macro					// &( expr )
                // TODO nvk: AMP Id=identifierName										#macro					// &id
                case TokenType.ARGLIST:
                    throw Error(Lt(), ErrorCode.NotSupported, Lt()?.value);
                default:
                    if (TokenAttr.IsSoftKeyword(La()))
                        return ParseFieldAlias() ?? ParseNameOrCtorCallOrSpecialFunc(ParseTypeSuffix(ParseQualifiedName()));
                    return null;
            }
        }

        internal TypeExpr ParseType()
        {
            var t = La();
            switch (t)
            {
                case TokenType.ID:
                    return ParseTypeSuffix(ParseQualifiedName());
                case TokenType.ARRAY:
                case TokenType.CODEBLOCK:
                case TokenType.DATE:
                case TokenType.FLOAT:
                case TokenType.PSZ:
                case TokenType.SYMBOL:
                case TokenType.USUAL:
                    return ParseTypeSuffix(new NativeTypeExpr(ConsumeAndGet()));
                case TokenType.BYTE:
                case TokenType.CHAR:
                case TokenType.DATETIME:
                case TokenType.DECIMAL:
                case TokenType.DWORD:
                case TokenType.DYNAMIC:
                case TokenType.INT:
                case TokenType.INT64:
                case TokenType.LOGIC:
                case TokenType.LONGINT:
                case TokenType.OBJECT:
                case TokenType.PTR:
                case TokenType.REAL4:
                case TokenType.REAL8:
                case TokenType.SHORTINT:
                case TokenType.STRING:
                case TokenType.UINT64:
                case TokenType.VOID:
                case TokenType.WORD:
                    return ParseTypeSuffix(new NativeTypeExpr(ConsumeAndGet()));
                default:
                    if (TokenAttr.IsSoftKeyword(La()))
                        return ParseTypeSuffix(ParseQualifiedName());
                    return null;
            }
        }

        internal IdExpr ParseId()
        {
            if (La() == TokenType.ID || TokenAttr.IsSoftKeyword(La()))
                return new IdExpr(ConsumeAndGet());
            return null;
        }

        internal Expr ParseNativeTypeOrCast(NativeTypeExpr nt)
        {
            var e = ParseNameOrCtorCallOrSpecialFunc(ParseTypeSuffix(nt));

            if ((e as NativeTypeExpr) == nt && La() == TokenType.LPAREN)
            {
                bool cast = false;

                Require(Expect(TokenType.LPAREN), ErrorCode.Expected, "'('");

                if (Expect(TokenType.CAST))
                {
                    Require(Expect(TokenType.COMMA), ErrorCode.Expected, "','");
                    cast = true;
                }

                var expr = ParseExpression();

                Require(Expect(TokenType.RPAREN) || AllowMissingSyntax, ErrorCode.Expected, "')'");

                return cast ? new TypeCast((TypeExpr)e, expr) : new TypeConversion((TypeExpr)e, expr);
            }

            return e;
        }

        internal NameExpr ParseName()
        {
            NameExpr n = ParseId();

            if (n != null)
            {
                // TODO nvk: Parse generic arguments
            }

            return n;
        }

        internal TypeExpr ParseTypeSuffix(TypeExpr t)
        {
            if (t != null)
            {
                // TODO nvk: parse PTR, array specifiers
            }

            return t;
        }

        internal NameExpr ParseQualifiedName()
        {
            NameExpr n = ParseName();
            while (La() == TokenType.DOT && (La(2) == TokenType.ID || TokenAttr.IsSoftKeyword(La(2))))
            {
                Consume();
                n = new QualifiedNameExpr(n, ParseName());
            }
            return n;
        }

        internal Expr ParseNameOrCtorCallOrSpecialFunc(TypeExpr t)
        {
            if (t != null && La() == TokenType.LCURLY)
            {
                var args = ParseCurlyArgList();

                // TODO nvk: Parse property initializers { name := expr }

                return new CtorCallExpr(t, args);
            }

            if (t is IdExpr && La() == TokenType.LPAREN)
            {
                switch ((t as IdExpr).Name.ToUpperInvariant())
                {
                    case "ALTD":
                        throw Error(Lt(), ErrorCode.NotSupported, (t as IdExpr).Name);
                    case "_GETINST":
                        throw Error(Lt(), ErrorCode.NotSupported, (t as IdExpr).Name);
                    case "CHR":
                    case "_CHR":
                        {
                            var args = ParseParenArgList();
                            Require(args.Args.Count == 1, ErrorCode.BadNumArgs, 1);
                            return new TypeCast(new NativeTypeExpr(t.Token, TokenType.CHAR), args.Args.First().Expr);
                        }
                    case "PCALL":
                    case "CCALL":
                        break;
                    case "PCALLNATIVE":
                    case "CCALLNATIVE":
                        break;
                    case "SLEN":
                        break;
                    case "STRING2PSZ":
                    case "CAST2PSZ":
                        break;
                    case "PCOUNT":
                    case "ARGCOUNT":
                    case "_GETMPARAM":
                    case "_GETFPARAM":
                        break;
                }
            }

            return t;
        }

        Oper ParseOper(out Node n)
        {
            return Opers[(int)La()].Parse(this, out n);
        }

        Oper ParsePrefixOper(out Node n)
        {
            return PrefixOpers[(int)La()].Parse(this, out n);
        }

        bool CanParsePrefixOper()
        {
            return PrefixOpers[(int)La()] != Oper.Empty;
        }

        internal Expr ParseExpression()
        {
            var exprs = new Stack<Tuple<Expr, Oper, Node>>();

            Expr e;

            do
            {
                Node n;
                Oper op;

                bool isCast = La() == TokenType.LPAREN;

                e = ParseTerm();

                if (isCast && e is TypeExpr && (CanParsePrefixOper() || CanParseTerm()))
                {
                    var p = Mark();
                    while (ParsePrefixOper(out n) != null) { }
                    isCast = CanParseTerm();
                    Rewind(p);
                }
                else
                    isCast = false;

                if (isCast)
                {
                    n = e;
                    e = null;
                    op = Opers[(int)TokenType.TYPECAST];
                }
                else
                    op = e == null ? ParsePrefixOper(out n) : ParseOper(out n);

                AssocType at;

                do
                {
                    at = op?.assoc ?? AssocType.None;
                    if (e == null && op != null && at != AssocType.Prefix)
                        throw Error(Lt(), ErrorCode.Expected, "expression");
                    if (e != null && at == AssocType.Prefix)
                        throw Error(Lt(), ErrorCode.Unexpected, "prefix operator");
                    while (exprs.Count > 0 && e != null && (op == null || exprs.Peek().Item2 < op))
                    {
                        var s = exprs.Pop();
                        e = s.Item2.Combine(this, s.Item1, s.Item3, e);
                    }
                    if (at == AssocType.Postfix)
                    {
                        e = op.Combine(this, e, n, null);
                        op = ParseOper(out n);
                    }
                } while (at == AssocType.Postfix || (e != null && op == null && exprs.Count > 0));

                if (op == null)
                    break;

                exprs.Push(new Tuple<Expr, Oper, Node>(e, op, n));
            } while (true);

            return e;
        }

        internal ExprList ParseExprList(bool allowEmpty = false)
        {
            IList<Expr> l = new List<Expr>();

            var e = ParseExpression();
            if (e == null && La() == TokenType.COMMA)
            {
                if (allowEmpty) e = new EmptyExpr(Lt());
                else Require(e != null, ErrorCode.Expected, "expression");
            }
            while (Expect(TokenType.COMMA))
            {
                l.Add(e);
                e = ParseExpression();
                if (e == null && allowEmpty) e = new EmptyExpr(Lt());
                else Require(e != null, ErrorCode.Expected, "expression");
            }
            if (e != null)
            {
                l.Add(e);
            }

            return new ExprList(l);
        }

        internal Codeblock ParseCodeblock()
        {
            Require(Expect(TokenType.LCURLY), ErrorCode.Expected, "'{'");

            List<IdExpr> p = null;

            if (Expect(TokenType.PIPE))
            {
                var a = ParseId();
                if (a != null)
                {
                    p = new List<IdExpr>();
                    do
                    {
                        p.Add(a);
                        if (!Expect(TokenType.COMMA))
                            break;
                        a = Require(ParseId(), ErrorCode.Expected, "identifier");
                    } while (true);
                }

                Require(Expect(TokenType.PIPE), ErrorCode.Expected, "'|'");
            }
            else Require(Expect(TokenType.OR), ErrorCode.Expected, "'|'");

            var l = ParseExprList();

            Require(Expect(TokenType.RCURLY) || AllowMissingSyntax, ErrorCode.Expected, "'}'");

            return new Codeblock(p,l);
        }

        internal Codeblock ParseMacro()
        {
            var p = new List<IdExpr>();
            if (La() == TokenType.LCURLY && (La(2) == TokenType.PIPE || La(2) == TokenType.OR))
                return RequireEnd(ParseCodeblock(), ErrorCode.Unexpected, "token");

            var l = ParseExprList();
            if (l != null)
                return RequireEnd(new Codeblock(null,l), ErrorCode.Unexpected, "token");

            return null;
        }

        internal TypeExpr ParseParenType()
        {
            Require(Expect(TokenType.LPAREN), ErrorCode.Expected, "'('");

            var t = Require(ParseType(), ErrorCode.Expected, "type");

            Require(Expect(TokenType.RPAREN) || AllowMissingSyntax, ErrorCode.Expected, "')'");

            return t;
        }

        internal Expr ParseParenExpr()
        {
            Require(Expect(TokenType.LPAREN), ErrorCode.Expected, "'('");

            var e = Require(ParseExpression(), ErrorCode.Expected, "expression");

            Require(Expect(TokenType.RPAREN) || AllowMissingSyntax, ErrorCode.Expected, "')'");

            return e;
        }

        internal Expr ParseLiteralArray(TypeExpr t = null)
        {
            Require(Expect(TokenType.LCURLY), ErrorCode.Expected, "'{'");

            var e = ParseExprList(true);

            Require(Expect(TokenType.RCURLY) || AllowMissingSyntax, ErrorCode.Expected, "'}'");

            return new LiteralArray(e, t);
        }

        internal Expr ParseTypedLiteralArray()
        {
            var p = Mark();

            TypeExpr t = null;

            if (Expect(TokenType.LT))
            {
                t = ParseType();

                if (!Expect(TokenType.GT))
                    t = null;
            }

            if (t != null && La() == TokenType.LCURLY)
            {
                var la = ParseLiteralArray(t);

                return la;
            }

            Rewind(p);

            return null;
        }

        internal Expr ParseIif()
        {
            Token o;
            if (ExpectAndGet(TokenType.IIF, out o) || Expect(TokenType.IF))
            {
                Require(Expect(TokenType.LPAREN), ErrorCode.Expected, "'('");

                var c = Require(ParseExpression(), ErrorCode.Expected, "expression");

                Expect(TokenType.COMMA);

                var et = ParseExpression() ?? new EmptyExpr(Lt());

                Expect(TokenType.COMMA);

                var ef = ParseExpression() ?? new EmptyExpr(Lt());

                Require(Expect(TokenType.RPAREN) || AllowMissingSyntax, ErrorCode.Expected, "')'");

                return new IifExpr(c, et, ef, o);
            }
            return null;
        }

        internal Expr ParseFieldAlias()
        {
            if (La(2) == TokenType.ALIAS)
            {
                if (Expect(TokenType.FIELD_))
                    Require(Expect(TokenType.ALIAS), ErrorCode.Expected, "'->'");
                else if (La(4) == TokenType.LPAREN)
                    return null;
                var alias = Require(ParseId(), ErrorCode.Expected, "name");
                if (La() == TokenType.ALIAS)
                {
                    Token o = ConsumeAndGet();
                    var field = Require(ParseId(), ErrorCode.Expected, "name");
                    return new AliasExpr(new LiteralExpr(alias.Token, TokenType.SYMBOL_CONST), new LiteralExpr(field.Token, TokenType.SYMBOL_CONST), o);
                }
                else
                {
                    return new AliasExpr(null, new LiteralExpr(alias.Token, TokenType.SYMBOL_CONST), alias.Token);
                }
            }
            return null;
        }

        internal Arg ParseArg()
        {
            var e = ParseExpression();
            if (e == null)
                return null;
            return new Arg(e);
        }

        internal ArgList ParseArgList()
        {
            IList<Arg> l = new List<Arg>();

            var a = ParseArg();
            if (a == null && La() == TokenType.COMMA)
                a = new Arg(new EmptyExpr(Lt()));
            while (Expect(TokenType.COMMA))
            {
                l.Add(a);
                a = ParseArg();
                if (a == null)
                    a = new Arg(new EmptyExpr(Lt()));
            }
            if (a != null)
            {
                l.Add(a);
            }

            return new ArgList(l);
        }

        internal ArgList ParseParenArgList()
        {
            Require(Expect(TokenType.LPAREN), ErrorCode.Expected, "'('");

            var l = ParseArgList();

            Require(Expect(TokenType.RPAREN) || AllowMissingSyntax, ErrorCode.Expected, "')'");

            return l;
        }

        internal ArgList ParseBrktArgList()
        {
            Require(Expect(TokenType.LBRKT), ErrorCode.Expected, "'['");

            var l = ParseArgList();

            Require(Expect(TokenType.RBRKT), ErrorCode.Expected, "']'");

            return l;
        }

        internal ArgList ParseCurlyArgList()
        {
            Require(Expect(TokenType.LCURLY), ErrorCode.Expected, "'{'");

            var l = ParseArgList();

            Require(Expect(TokenType.RCURLY) || AllowMissingSyntax, ErrorCode.Expected, "'}'");

            return l;
        }

        enum AssocType
        {
            None,
            BinaryLeft,
            BinaryRight,
            Prefix,
            Postfix,

            PostfixDot,
            PostfixColon,
            PostfixCall,
            PostfixIndex,
            BinaryAssign,
            BinaryAssignOp,
            BinaryLogic,
            PrefixAssign,
            PostfixAssign,
            PostfixIs,
            PostfixAsType,
            PrefixCast,
            BinaryAlias,
        }

        class Oper
        {
            internal static readonly Oper Empty = new Oper(AssocType.None, TokenType.UNRECOGNIZED, int.MaxValue);
            internal delegate Oper ParseDelegate(Parser p, out Node n);
            internal delegate Expr CombineDelegate(Parser p, Expr l, Node o, Expr r);
            internal readonly AssocType assoc;
            internal readonly TokenType type;
            internal readonly int level;
            internal readonly ParseDelegate Parse;
            internal readonly CombineDelegate Combine;
            internal Oper(AssocType assoc, TokenType type, int level)
            {
                this.assoc = assoc;
                this.type = type;
                this.level = level;
                switch (assoc)
                {
                    case AssocType.BinaryLeft:
                    case AssocType.BinaryRight:
                        switch (type)
                        {
                            case TokenType.GT:
                                Parse = _parse_gt;
                                break;
                            case TokenType.LT:
                                Parse = _parse_lt;
                                break;
                            default:
                                Parse = _parse;
                                break;
                        }
                        Combine = _combine_binary;
                        break;
                    case AssocType.PostfixDot:
                        this.assoc = AssocType.Postfix;
                        Parse = _parse_postfix_dot;
                        Combine = _combine_postfix_dot;
                        break;
                    case AssocType.PostfixColon:
                        this.assoc = AssocType.Postfix;
                        Parse = _parse_postfix_colon;
                        Combine = _combine_postfix_colon;
                        break;
                    case AssocType.PostfixCall:
                        this.assoc = AssocType.Postfix;
                        Parse = _parse_postfix_call;
                        Combine = _combine_postfix_call;
                        break;
                    case AssocType.PostfixIndex:
                        this.assoc = AssocType.Postfix;
                        Parse = _parse_postfix_index;
                        Combine = _combine_postfix_index;
                        break;
                    case AssocType.BinaryAssign:
                        this.assoc = AssocType.BinaryRight;
                        Parse = _parse;
                        Combine = _combine_binary_assign;
                        break;
                    case AssocType.BinaryAssignOp:
                        this.assoc = AssocType.BinaryRight;
                        Parse = _parse;
                        Combine = _combine_binary_assign_op;
                        break;
                    case AssocType.BinaryLogic:
                        this.assoc = AssocType.BinaryLeft;
                        Parse = _parse;
                        Combine = _combine_binary_logic;
                        break;
                    case AssocType.Postfix:
                        Parse = _parse;
                        Combine = _combine_postfix;
                        break;
                    case AssocType.Prefix:
                        Parse = _parse;
                        Combine = _combine_prefix;
                        break;
                    case AssocType.PostfixAssign:
                        this.assoc = AssocType.Postfix;
                        Parse = _parse;
                        Combine = _combine_postfix_assign;
                        break;
                    case AssocType.PrefixAssign:
                        this.assoc = AssocType.Prefix;
                        Parse = _parse;
                        Combine = _combine_prefix_assign;
                        break;
                    case AssocType.PostfixIs:
                        this.assoc = AssocType.Postfix;
                        Parse = _parse_is_as;
                        Combine = _combine_postfix_is;
                        break;
                    case AssocType.PostfixAsType:
                        this.assoc = AssocType.Postfix;
                        Parse = _parse_is_as;
                        Combine = _combine_postfix_as_type;
                        break;
                    case AssocType.PrefixCast:
                        this.assoc = AssocType.Prefix;
                        Parse = _parse;
                        Combine = _combine_prefix_cast;
                        break;
                    case AssocType.BinaryAlias:
                        this.assoc = AssocType.BinaryLeft;
                        Parse = _parse;
                        Combine = _combine_binary_alias;
                        break;
                    case AssocType.None:
                        Parse = _parse_empty;
                        Combine = null;
                        break;
                }
            }
            internal Oper(AssocType assoc, TokenType type, int level, ParseDelegate parseFunc = null, CombineDelegate combineFunc = null) : this(assoc, type, level)
            {
                if (parseFunc != null) Parse = parseFunc;
                if (combineFunc != null) Combine = combineFunc;
            }
            Oper _parse(Parser p, out Node n)
            {
                n = null;
                if (assoc != AssocType.None)
                {
                    n = new SyntaxToken(p.ConsumeAndGet());
                    Debug.Assert(n.Token.type == type);
                    return this;
                }
                return null;
            }
            Oper _parse_gt(Parser p, out Node n)
            {
                n = new SyntaxToken(p.ConsumeAndGet());
                if (p.Expect(TokenType.GT))
                {
                    n.Token.type = TokenType.RSHIFT;
                    Debug.Assert(n.Token.type == TokenType.RSHIFT);
                    return Opers[(int)TokenType.RSHIFT];
                }
                Debug.Assert(n.Token.type == type);
                return this;
            }
            Oper _parse_lt(Parser p, out Node n)
            {
                n = new SyntaxToken(p.ConsumeAndGet());
                if (p.Expect(TokenType.LT))
                {
                    n.Token.type = TokenType.LSHIFT;
                    Debug.Assert(n.Token.type == TokenType.LSHIFT);
                    return Opers[(int)TokenType.LSHIFT];
                }
                Debug.Assert(n.Token.type == type);
                return this;
            }
            Oper _parse_is_as(Parser p, out Node n)
            {
                p.Consume();
                n = p.ParseQualifiedName();
                return this;
            }
            Oper _parse_postfix_dot(Parser p, out Node n)
            {
                p.Consume();
                n = p.Require(p.ParseName(), ErrorCode.Expected, "name");
                return this;
            }
            Oper _parse_postfix_colon(Parser p, out Node n)
            {
                p.Consume();
                n = p.Require(p.ParseName(), ErrorCode.Expected, "name");
                return this;
            }
            Oper _parse_postfix_call(Parser p, out Node n)
            {
                n = p.ParseParenArgList();
                return this;
            }
            Oper _parse_postfix_index(Parser p, out Node n)
            {
                n = p.ParseBrktArgList();
                return this;
            }
            Oper _parse_empty(Parser p, out Node n)
            {
                n = null;
                return null;
            }
            Expr _combine_prefix(Parser p, Expr l, Node o, Expr r) => new UnaryExpr(r, o.Token);
            Expr _combine_postfix(Parser p, Expr l, Node o, Expr r) => new UnaryExpr(l, o.Token);
            Expr _combine_prefix_assign(Parser p, Expr l, Node o, Expr r) => new PrefixExpr(r, o.Token);
            Expr _combine_postfix_assign(Parser p, Expr l, Node o, Expr r) => new PostfixExpr(l, o.Token);
            Expr _combine_binary(Parser p, Expr l, Node o, Expr r) => new BinaryExpr(l, o.Token, r);
            Expr _combine_binary_assign(Parser p, Expr l, Node o, Expr r) => new AssignExpr(l, o.Token, r);
            Expr _combine_binary_assign_op(Parser p, Expr l, Node o, Expr r) => new AssignOpExpr(l, o.Token, r);
            Expr _combine_binary_logic(Parser p, Expr l, Node o, Expr r) => new BinaryLogicExpr(l, o.Token, r);
            Expr _combine_postfix_is(Parser p, Expr l, Node o, Expr r) => new IsExpr(l, (TypeExpr)o, o.Token);
            Expr _combine_postfix_as_type(Parser p, Expr l, Node o, Expr r) => new AsTypeExpr(l, (TypeExpr)o, o.Token);
            Expr _combine_prefix_cast(Parser p, Expr l, Node o, Expr r) => new TypeCast((TypeExpr)o, r);
            Expr _combine_binary_alias(Parser p, Expr l, Node o, Expr r) => new AliasExpr(l, r, o.Token);
            Expr _combine_postfix_dot(Parser p, Expr l, Node o, Expr r)
            {
                if (!(l is TypeExpr)) throw Compilation.Error(p.Lt(), ErrorCode.Expected, "name");
                return new QualifiedNameExpr((TypeExpr)l, (NameExpr)o);
            }
            Expr _combine_postfix_colon(Parser p, Expr l, Node o, Expr r) => new MemberAccessExpr(l, (NameExpr)o);
            Expr _combine_postfix_call(Parser p, Expr l, Node o, Expr r) => new MethodCallExpr(l, (ArgList)o);
            Expr _combine_postfix_index(Parser p, Expr l, Node o, Expr r) => new ArrayAccessExpr(l, (ArgList)o);
            public static bool operator <(Oper a, Oper b)
            {
                return a.level < b.level || (a.level == b.level && a.assoc != AssocType.BinaryRight);
            }
            public static bool operator >(Oper a, Oper b)
            {
                return a.level > b.level || (a.level == b.level && a.assoc == AssocType.BinaryRight);
            }
        }

        static readonly Oper[] Opers;
        static readonly Oper[] PrefixOpers;

        static Parser()
        {
            Opers = new Oper[(int)TokenType.LAST];
            PrefixOpers = new Oper[(int)TokenType.LAST];

            for (var i = 0; i < Opers.Length; i++)
            {
                Opers[i] = Oper.Empty;
                PrefixOpers[i] = Oper.Empty;
            }

            Opers[(int)TokenType.DOT] = new Oper(AssocType.PostfixDot, TokenType.DOT, 1);
            Opers[(int)TokenType.COLON] = new Oper(AssocType.PostfixColon, TokenType.COLON, 1);
            Opers[(int)TokenType.LPAREN] = new Oper(AssocType.PostfixCall, TokenType.LPAREN, 2);
            Opers[(int)TokenType.LBRKT] = new Oper(AssocType.PostfixIndex, TokenType.LBRKT, 2);
            Opers[(int)TokenType.QMARK] = new Oper(AssocType.Postfix, TokenType.QMARK, 3);
            Opers[(int)TokenType.TYPECAST] = new Oper(AssocType.PrefixCast, TokenType.TYPECAST, 4);
            Opers[(int)TokenType.INC] = new Oper(AssocType.PostfixAssign, TokenType.INC, 5);
            Opers[(int)TokenType.DEC] = new Oper(AssocType.PostfixAssign, TokenType.DEC, 5);
            PrefixOpers[(int)TokenType.AWAIT] = new Oper(AssocType.Prefix, TokenType.AWAIT, 6);
            PrefixOpers[(int)TokenType.INC] = new Oper(AssocType.PrefixAssign, TokenType.INC, 6);
            PrefixOpers[(int)TokenType.DEC] = new Oper(AssocType.PrefixAssign, TokenType.DEC, 6);
            PrefixOpers[(int)TokenType.PLUS] = new Oper(AssocType.Prefix, TokenType.PLUS, 6);
            PrefixOpers[(int)TokenType.MINUS] = new Oper(AssocType.Prefix, TokenType.MINUS, 6);
            PrefixOpers[(int)TokenType.TILDE] = new Oper(AssocType.Prefix, TokenType.TILDE, 6);
            PrefixOpers[(int)TokenType.ADDROF] = new Oper(AssocType.Prefix, TokenType.ADDROF, 6);
            Opers[(int)TokenType.IS] = new Oper(AssocType.PostfixIs, TokenType.IS, 7);
            Opers[(int)TokenType.ASTYPE] = new Oper(AssocType.PostfixAsType, TokenType.AS, 7);
            Opers[(int)TokenType.ALIAS] = new Oper(AssocType.BinaryAlias, TokenType.ALIAS, 8);
            Opers[(int)TokenType.EXP] = new Oper(AssocType.BinaryLeft, TokenType.EXP, 9);
            Opers[(int)TokenType.MULT] = new Oper(AssocType.BinaryLeft, TokenType.MULT, 10);
            Opers[(int)TokenType.DIV] = new Oper(AssocType.BinaryLeft, TokenType.DIV, 10);
            Opers[(int)TokenType.MOD] = new Oper(AssocType.BinaryLeft, TokenType.MOD, 10);
            Opers[(int)TokenType.PLUS] = new Oper(AssocType.BinaryLeft, TokenType.PLUS,11);
            Opers[(int)TokenType.MINUS] = new Oper(AssocType.BinaryLeft, TokenType.MINUS, 11);
            Opers[(int)TokenType.LSHIFT] = new Oper(AssocType.BinaryLeft, TokenType.LSHIFT, 12);
            Opers[(int)TokenType.RSHIFT] = new Oper(AssocType.BinaryLeft, TokenType.RSHIFT, 12);
            Opers[(int)TokenType.GT] = new Oper(AssocType.BinaryLeft, TokenType.GT, 13);
            Opers[(int)TokenType.LT] = new Oper(AssocType.BinaryLeft, TokenType.LT, 13);
            Opers[(int)TokenType.GTE] = new Oper(AssocType.BinaryLeft, TokenType.GTE, 13);
            Opers[(int)TokenType.LTE] = new Oper(AssocType.BinaryLeft, TokenType.LTE, 13);
            Opers[(int)TokenType.EQ] = new Oper(AssocType.BinaryLeft, TokenType.EQ, 13);
            Opers[(int)TokenType.EEQ] = new Oper(AssocType.BinaryLeft, TokenType.EEQ, 13);
            Opers[(int)TokenType.SUBSTR] = new Oper(AssocType.BinaryLeft, TokenType.SUBSTR, 13);
            Opers[(int)TokenType.NEQ] = new Oper(AssocType.BinaryLeft, TokenType.NEQ, 13);
            Opers[(int)TokenType.NEQ2] = new Oper(AssocType.BinaryLeft, TokenType.NEQ2, 13);
            Opers[(int)TokenType.AMP] = new Oper(AssocType.BinaryLeft, TokenType.AMP, 14);
            Opers[(int)TokenType.TILDE] = new Oper(AssocType.BinaryLeft, TokenType.TILDE, 15);
            Opers[(int)TokenType.PIPE] = new Oper(AssocType.BinaryLeft, TokenType.PIPE, 16);
            PrefixOpers[(int)TokenType.NOT] = new Oper(AssocType.Prefix, TokenType.NOT, 6);
            PrefixOpers[(int)TokenType.LOGIC_NOT] = new Oper(AssocType.Prefix, TokenType.LOGIC_NOT, 6);
            Opers[(int)TokenType.AND] = new Oper(AssocType.BinaryLogic, TokenType.AND, 18);
            Opers[(int)TokenType.LOGIC_AND] = new Oper(AssocType.BinaryLogic, TokenType.LOGIC_AND, 18);
            Opers[(int)TokenType.LOGIC_XOR] = new Oper(AssocType.BinaryLeft, TokenType.LOGIC_XOR, 19);
            Opers[(int)TokenType.OR] = new Oper(AssocType.BinaryLogic, TokenType.OR, 20);
            Opers[(int)TokenType.LOGIC_OR] = new Oper(AssocType.BinaryLogic, TokenType.LOGIC_OR, 20);
            Opers[(int)TokenType.DEFAULT] = new Oper(AssocType.BinaryLeft, TokenType.DEFAULT, 21);
            Opers[(int)TokenType.ASSIGN_OP] = new Oper(AssocType.BinaryAssign, TokenType.ASSIGN_OP, 22);
            Opers[(int)TokenType.ASSIGN_ADD] = new Oper(AssocType.BinaryAssignOp, TokenType.ASSIGN_ADD, 22);
            Opers[(int)TokenType.ASSIGN_SUB] = new Oper(AssocType.BinaryAssignOp, TokenType.ASSIGN_SUB, 22);
            Opers[(int)TokenType.ASSIGN_EXP] = new Oper(AssocType.BinaryAssignOp, TokenType.ASSIGN_EXP, 22);
            Opers[(int)TokenType.ASSIGN_MUL] = new Oper(AssocType.BinaryAssignOp, TokenType.ASSIGN_MUL, 22);
            Opers[(int)TokenType.ASSIGN_DIV] = new Oper(AssocType.BinaryAssignOp, TokenType.ASSIGN_DIV, 22);
            Opers[(int)TokenType.ASSIGN_MOD] = new Oper(AssocType.BinaryAssignOp, TokenType.ASSIGN_MOD, 22);
            Opers[(int)TokenType.ASSIGN_BITAND] = new Oper(AssocType.BinaryAssignOp, TokenType.ASSIGN_BITAND, 22);
            Opers[(int)TokenType.ASSIGN_BITOR] = new Oper(AssocType.BinaryAssignOp, TokenType.ASSIGN_BITOR, 22);
            Opers[(int)TokenType.ASSIGN_LSHIFT] = new Oper(AssocType.BinaryAssignOp, TokenType.ASSIGN_LSHIFT, 22);
            Opers[(int)TokenType.ASSIGN_RSHIFT] = new Oper(AssocType.BinaryAssignOp, TokenType.ASSIGN_RSHIFT, 22);
            Opers[(int)TokenType.ASSIGN_XOR] = new Oper(AssocType.BinaryAssignOp, TokenType.ASSIGN_XOR, 22);
        }
    }
}
