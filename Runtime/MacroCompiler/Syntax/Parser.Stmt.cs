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
        internal StmtBlock ParseStatementBlock()
        {
            var t = Lt();
            List<Stmt> l = new List<Stmt>();
            while (ParseStatement() is Stmt s)
                l.Add(s);
            return new StmtBlock(t, l.ToArray());
        }

        internal Stmt ParseStatement()
        {
            switch (La())
            {
                case TokenType.MEMVAR:
                    return ParseXBaseDeclarationStmt(TokenType.MEMVAR);
                case TokenType.PARAMETERS:
                    return ParseXBaseDeclarationStmt(TokenType.PARAMETERS, parseType: true);
                case TokenType.LPARAMETERS:
                    return ParseXBaseDeclarationStmt(TokenType.LPARAMETERS, parseType: true);
                case TokenType.PRIVATE:
                    return ParseXBaseDeclarationStmt(TokenType.PRIVATE, parseArraySub: true, parseInit: true);
                case TokenType.PUBLIC:
                    return ParseXBaseDeclarationStmt(TokenType.PUBLIC, parseArraySub: true, parseInit: true, parseType: true);
                case TokenType.LOCAL:
                case TokenType.STATIC:
                case TokenType.VAR:
                    return ParseLocalDeclarationStmt();
                case TokenType.DECLARE:
                case TokenType.DIMENSION:
                    return ParseFoxDimensionStmt();
                case TokenType.FIELD:
                    return ParseFieldDeclStmt();
                case TokenType.NOP:
                    return ParseNopStmt();
                case TokenType.WHILE:
                    return ParseWhileStmt();
                case TokenType.FOR:
                    return ParseForStmt();
                case TokenType.IF:
                    return ParseIfStmt();
                case TokenType.DO:
                    switch(La(2))
                    {
                        case TokenType.WHILE:
                            return ParseWhileStmt();
                        case TokenType.CASE:
                            return ParseDoCaseStmt();
                        default:
                            throw Error(Lt(), ErrorCode.Expected, "WHILE or CASE");
                    }
                case TokenType.EXIT:
                case TokenType.LOOP:
                case TokenType.BREAK:
                case TokenType.RETURN:
                case TokenType.QMARK:
                case TokenType.QQMARK:
                case TokenType.BEGIN:
                case TokenType.REPEAT:
                case TokenType.FOREACH:
                case TokenType.THROW:
                case TokenType.TRY:
                case TokenType.YIELD:
                case TokenType.WITH:
                    return null;
                default:
                    var l = ParseExprList();
                    if (l?.Exprs.Count > 0)
                    {
                        Require(TokenType.EOS);
                        return new ExprStmt(l);
                    }
                    break;
            }
            return null;
        }

        internal DeclStmt ParseXBaseDeclarationStmt(TokenType kind, bool parseArraySub = false, bool parseInit = false, bool parseType = false)
        {
            Token t;
            Require(ExpectAndGet(kind, out t), ErrorCode.Expected, kind);
            var vl = new List<VarDecl>();
            do
            {
                if (ParseXBaseVarDecl(parseArraySub: parseArraySub, parseInit: parseInit, parseType: parseType) is VarDecl v)
                    vl.Add(v);
            } while (Expect(TokenType.COMMA));
            Require(vl.Count > 0, ErrorCode.Expected, "variable declaration");
            Require(TokenType.EOS);
            return new DeclStmt(t, vl.ToArray());
        }

        internal VarDecl ParseXBaseVarDecl(bool parseArraySub = false, bool parseInit = false, bool parseType = false)
        {
            // Parse variable name
            Token n = null;
            if (La() == TokenType.ID || TokenAttr.IsSoftKeyword(La()))
                n = ConsumeAndGet();
            else if (La() == TokenType.M && La(2) == TokenType.DOT && (La(3) == TokenType.ID || TokenAttr.IsSoftKeyword(La(3))))
            {
                Consume(2);
                n = ConsumeAndGet();
            }
            else
                return null;

            // Parse array sub indices
            List<Expr> asub = null;
            if (parseArraySub && Expect(TokenType.LBRKT))
            {
                asub = new List<Expr>();
                do
                {
                    asub.Add(Require(ParseExpression(), ErrorCode.Expected, "expression"));
                    if (Expect(TokenType.COMMA))
                        continue;
                    if (La() == TokenType.RBRKT && La(2) == TokenType.LBRKT)
                    {
                        Consume(2);
                        continue;
                    }
                    break;
                } while (true);
                Require(Expect(TokenType.RBRKT), ErrorCode.Expected, TokenType.RBRKT);
            }

            // Parse initializer
            Expr init = null;
            if (parseInit && Expect(TokenType.ASSIGN_OP))
            {
                init = ParseExpression();
            }

            // Parse type
            TypeExpr type = null;
            if (parseType && Expect(TokenType.AS)) // type parsed but ignored
            {
                Require(ParseType(), ErrorCode.Expected, "type");
                if (Expect(TokenType.OF))
                {
                    Require(ParseId(), ErrorCode.Expected, "identifier");
                }
            }

            return new VarDecl(n, asub?.ToArray(), type, init);
        }

        internal DeclStmt ParseLocalDeclarationStmt()
        {
            Token t = null;
            if (ExpectAndGet(TokenType.LOCAL, out t))
            {
                if (La() == TokenType.STATIC)
                    t = ConsumeAndGet();
                if (t.type == TokenType.STATIC)
                    Require(La() != TokenType.VAR && La() != TokenType.IMPLIED, ErrorCode.NotSupported, "static implied variable");
                if (La() == TokenType.IMPLIED)
                    t = ConsumeAndGet();
            }
            else if (ExpectAndGet(TokenType.STATIC, out t))
            {
                if (La() == TokenType.LOCAL)
                    Consume();
                Require(La() != TokenType.VAR && La() != TokenType.IMPLIED, ErrorCode.NotSupported, "static implied variable");
            }
            else if (ExpectAndGet(TokenType.VAR, out t))
            {
                Require(La() != TokenType.STATIC, ErrorCode.NotSupported, "static implied variable");
            }
            Require(t, ErrorCode.Expected, TokenType.LOCAL);
            bool implied = t.type == TokenType.VAR || t.type == TokenType.IMPLIED;


            var vl = new List<VarDecl>();
            do
            {
                if (ParseVarDecl(implied: implied) is VarDecl v)
                    vl.Add(v);
            } while (Expect(TokenType.COMMA));
            Require(vl.Count > 0, ErrorCode.Expected, "variable declaration");

            Require(TokenType.EOS);
            return new DeclStmt(t, vl.ToArray());
        }
        internal VarDecl ParseVarDecl(bool implied = false)
        {
            // Parse modifiers
            bool isConst = Expect(TokenType.CONST);
            bool isDim = !implied && Expect(TokenType.DIM);

            // Parse variable name
            Token n = null;
            if (La() == TokenType.ID || TokenAttr.IsSoftKeyword(La()))
                n = ConsumeAndGet();
            else if (La() == TokenType.M && La(2) == TokenType.DOT && (La(3) == TokenType.ID || TokenAttr.IsSoftKeyword(La(3))))
            {
                Consume(2);
                n = ConsumeAndGet();
            }
            else
                return null;

            // Parse array sub indices
            List<Expr> asub = null;
            if (!implied && Expect(TokenType.LBRKT))
            {
                asub = new List<Expr>();
                do
                {
                    asub.Add(Require(ParseExpression(), ErrorCode.Expected, "expression"));
                    if (Expect(TokenType.COMMA))
                        continue;
                    if (La() == TokenType.RBRKT && La(2) == TokenType.LBRKT)
                    {
                        Consume(2);
                        continue;
                    }
                    break;
                } while (true);
                Require(Expect(TokenType.RBRKT), ErrorCode.Expected, TokenType.RBRKT);
            }

            // Parse initializer
            Expr init = null;
            if (Expect(TokenType.ASSIGN_OP))
            {
                init = ParseExpression();
            }
            if (implied) Require(init, ErrorCode.Expected, "initializer");

            // Parse type
            TypeExpr type = null;
            bool isIsType = false;
            if (!implied && ExpectAndGetAny(TokenType.AS, TokenType.IS) is Token t)
            {
                isIsType = t.type == TokenType.IS;
                type = Require(ParseType(), ErrorCode.Expected, "type");
            }

            return implied ? new ImpliedVarDecl(n, init) { IsConst = isConst }
                : new VarDecl(n, asub?.ToArray(), type, init) { IsConst = isConst, IsDim = isDim, IsIsType = isIsType };
        }

        internal DeclStmt ParseFoxDimensionStmt()
        {
            Token t = Require(ExpectAndGetAny(TokenType.DIMENSION, TokenType.DECLARE), ErrorCode.Expected, TokenType.DIMENSION);
            var vl = new List<VarDecl>();
            do
            {
                if (ParseFoxDimVarDecl() is VarDecl v)
                    vl.Add(v);
            } while (Expect(TokenType.COMMA));
            Require(vl.Count > 0, ErrorCode.Expected, "variable declaration");
            Require(TokenType.EOS);
            return new DeclStmt(t, vl.ToArray());
        }
        internal VarDecl ParseFoxDimVarDecl()
        {
            // Parse variable name
            Token n = null;
            if (La() == TokenType.ID || TokenAttr.IsSoftKeyword(La()))
                n = ConsumeAndGet();
            else
                return null;

            // Parse array sub indices
            List<Expr> asub = null;
            if (ExpectAndGetAny(TokenType.LBRKT,TokenType.LPAREN) is Token lp)
            {
                asub = new List<Expr>();
                do
                {
                    asub.Add(Require(ParseExpression(), ErrorCode.Expected, "expression"));
                    if (Expect(TokenType.COMMA))
                        continue;
                    if (La() == TokenType.RBRKT && La(2) == TokenType.LBRKT)
                    {
                        Consume(2);
                        continue;
                    }
                    break;
                } while (true);
                var rp = Expect(lp.type == TokenType.LBRKT ? TokenType.RBRKT : TokenType.RPAREN);
                Require(rp, ErrorCode.Expected, rp);
            }

            // Parse type
            TypeExpr type = null;
            if (Expect(TokenType.AS)) // type parsed byt ignored
            {
                Require(ParseType(), ErrorCode.Expected, "type");
                if (Expect(TokenType.OF))
                {
                    Require(ParseId(), ErrorCode.Expected, "identifier");
                }
            }

            return new VarDecl(n, asub?.ToArray(), type, null);
        }

        internal FieldDeclStmt ParseFieldDeclStmt()
        {
            Token t;
            Require(ExpectAndGet(TokenType.FIELD, out t), ErrorCode.Expected, TokenType.FIELD);
            var fl = new List<Token>();
            do
            {
                fl.Add(RequireId());
            } while (Expect(TokenType.COMMA));

            Token alias = null;
            if (Expect(TokenType.IN))
            {
                alias = RequireId();
            }

            Require(TokenType.EOS);
            return new FieldDeclStmt(fl.ToArray(), alias);
        }

        internal EmptyStmt ParseNopStmt()
        {
            Token t;
            ExpectAndGet(TokenType.NOP, out t);
            Require(TokenType.EOS);
            if (Expect(TokenType.LPAREN))
                Require(TokenType.RPAREN);
            return new EmptyStmt(t);
        }

        internal WhileStmt ParseWhileStmt()
        {
            Expect(TokenType.DO);
            Token w;
            Require(ExpectAndGet(TokenType.WHILE, out w),ErrorCode.Expected, TokenType.WHILE);
            var cond = Require(ParseExpression(), ErrorCode.Expected, "expression");
            Require(TokenType.EOS);
            var s = ParseStatementBlock();
            if (!Expect(TokenType.ENDDO))
            {
                Require(TokenType.END);
                ExpectAny(TokenType.DO, TokenType.WHILE);
            }
            Require(TokenType.EOS);
            return new WhileStmt(w, cond, s);
        }

        internal ForStmt ParseForStmt()
        {
            Token t;
            ExpectAndGet(TokenType.FOR, out t);
            VarDecl d = null;
            AssignExpr a = null;
            if (ExpectAny(TokenType.IMPLIED, TokenType.VAR) || (La(2) == TokenType.IMPLIED && Expect(TokenType.LOCAL) && Expect(TokenType.IMPLIED)))
            {
                d = ParseForDecl(true);
            }
            else if (Expect(TokenType.LOCAL))
            {
                d = ParseForDecl(false);
            }
            else
            {
                a = ParseExpression() as AssignExpr;
                Require(a?.Left is IdExpr, ErrorCode.Expected, "id assignemnt expression");
            }
            var dir = Require(ExpectAndGetAny(TokenType.UPTO, TokenType.DOWNTO, TokenType.TO), ErrorCode.Expected, TokenType.TO);
            Expr final = Require(ParseExpression(), ErrorCode.Expected, "expression");
            Expr step = null;
            if (Expect(TokenType.STEP))
            {
                step = Require(ParseExpression(), ErrorCode.Expected, "expression");
            }
            Require(TokenType.EOS);
            var s = ParseStatementBlock();
            if (Expect(TokenType.END))
            {
                Expect(TokenType.FOR);
            }
            else
                Require(Expect(TokenType.NEXT), ErrorCode.Expected, "END FOR");
            Require(TokenType.EOS);
            return a != null ? new ForStmt(t, a, dir, final, step, s)
                : new ForStmt(t, d, dir, final, step, s);
        }
        internal VarDecl ParseForDecl(bool implied)
        {
            Token n = null;
            if (La() == TokenType.ID || TokenAttr.IsSoftKeyword(La()))
                n = ConsumeAndGet();
            Require(n, ErrorCode.Expected, "identifier");

            Expr init = null;
            if (Require(TokenType.ASSIGN_OP))
            {
                init = ParseExpression();
            }

            TypeExpr type = null;
            if (!implied && Expect(TokenType.AS))
            {
                type = Require(ParseType(), ErrorCode.Expected, "type");
            }

            return implied ? new ImpliedVarDecl(n, init)
                : new VarDecl(n, null, type, init);
        }

        internal IfStmt ParseIfStmt()
        {
            var t = RequireAndGet(TokenType.IF);
            var cond = Require(ParseExpression(), ErrorCode.Expected, "expression");
            Expect(TokenType.THEN);
            Require(TokenType.EOS);
            var si = ParseStatementBlock();
            Stmt se = null;
            if (Expect(TokenType.ELSE))
            {
                Require(TokenType.EOS);
                se = ParseStatementBlock();
            }
            if (Expect(TokenType.END))
            {
                Expect(TokenType.IF);
            }
            else
                Require(Expect(TokenType.ENDIF), ErrorCode.Expected, "END IF");
            Require(TokenType.EOS);
            return new IfStmt(t, cond, si, se);
        }

        internal DoCaseStmt ParseDoCaseStmt()
        {
            var t = RequireAndGet(TokenType.DO);
            Require(TokenType.CASE);
            Require(TokenType.EOS);

            var cases = new List<CaseBlock>();
            Token c;
            while (ExpectAndGet(TokenType.CASE, out c))
            {
                var cond = ParseExpression();
                Require(TokenType.EOS);
                var s = ParseStatementBlock();
                cases.Add(new CaseBlock(c, cond, s));
            }

            Stmt o = null;
            if (Expect(TokenType.OTHERWISE))
            {
                Require(TokenType.EOS);
                o = ParseStatementBlock();
            }

            Require(cases.Count > 0 || o != null, ErrorCode.Expected, "CASE or OTHERWISE");

            if (Expect(TokenType.END))
                Expect(TokenType.CASE);
            else
                Require(TokenType.ENDCASE);
            Require(TokenType.EOS);

            return new DoCaseStmt(t, cases.ToArray(), o);
        }
    }
}
