//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Collections.Generic;
using System.Linq;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal class XSharpParseErrorAnalysis : XSharpBaseListener
    {
        readonly XSharpParser _parser;
        readonly IList<ParseErrorData> _parseErrors;
        readonly CSharpParseOptions _options;
        readonly List<PragmaOption> _pragmas;

        public XSharpParseErrorAnalysis(XSharpParser parser, IList<ParseErrorData> parseErrors,
            CSharpParseOptions options, List<PragmaOption> pragmas)
        {
            _parser = parser;
            _parseErrors = parseErrors;
            _options = options;
            _pragmas = pragmas;
        }

        private void NotInDialect(XSharpParserRuleContext context, string msg)
        {
            _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_FeatureNotAvailableInDialect, msg, _options.Dialect.ToString()));
        }
        private void NotInCore(XSharpParserRuleContext context, string msg)
        {
            if (_options.Dialect == XSharpDialect.Core)
            {
                NotInDialect(context, msg);
            }
        }
        void CheckExpressionBodyStatements(XSharpParser.ExpressionContext body, XSharpParser.StatementBlockContext statements)
        {
            if (body != null && statements?._Stmts.Count > 0)
            {
                _parseErrors.Add(new ParseErrorData(body, ErrorCode.ERR_BlockBodyAndExpressionBody));
            }
        }
        void CheckEmptyBlock(XSharpParserRuleContext context, XSharpParser.StatementBlockContext statements, string blockType)
        {
            if (statements != null && statements._Stmts.Count == 0)
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.WRN_EmptyStatement, blockType));
            }
        }
        private void checkMissingKeyword(object endToken, ParserRuleContext context, string msg)
        {
            if (endToken == null)
            {
                // if there is an error inside the start .. end then there is no need to report
                // a missing end
                int start = context.Start.StartIndex;
                int end = context.Stop.StopIndex;
                bool haserrorinblock = false;
                foreach (var error in _parseErrors)
                {
                    if (!ErrorFacts.IsWarning(error.Code))
                    {
                        if (error.Node.Position > start && error.Node.Position < end)
                        {

                            haserrorinblock = true;
                            break;
                        }
                    }
                }
                if (!haserrorinblock)
                {
                    var err = ErrorCode.ERR_SyntaxError;
                    IToken anchor = context.Stop;
                    if (anchor == null)
                        anchor = context.Start;
                    var errdata = new ParseErrorData(anchor, err, msg);
                    _parseErrors.Add(errdata);
                }
            }
            return;
        }

        public override void VisitErrorNode([NotNull] IErrorNode node)
        {
            if (node.Symbol.Type == XSharpLexer.INCOMPLETE_STRING_CONST)
            {
                var err = ErrorCode.ERR_UnterminatedStringLit;
                IToken anchor = node.Symbol;
                var errdata = new ParseErrorData(anchor, err);
                _parseErrors.Add(errdata);
            }
            else if (node.Symbol.Type == XSharpLexer.INVALID_NUMBER)
            {
                var err = ErrorCode.ERR_InvalidNumber;
                IToken anchor = node.Symbol;
                var errdata = new ParseErrorData(anchor, err);
                _parseErrors.Add(errdata);
            }
            //else
            //{
            //    _parseErrors.Add(new ParseErrorData(node, ErrorCode.ERR_SyntaxError, node));
            //}
        }

        // Check for incorrect operators
        public override void ExitAssignoperator([NotNull] XSharpParser.AssignoperatorContext context)
        {
            if (context.Op.Type != XSharpParser.ASSIGN_OP && !_options.HasOption(CompilerOption.AllowOldStyleAssignments, context, _pragmas))
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_AssignmentOperatorExpected));
            }
        }
        public override void ExitWhileStmt([NotNull] XSharpParser.WhileStmtContext context)
        {
            CheckEmptyBlock(context, context.StmtBlk, "[DO] WHILE statement");
        }
        public override void ExitWithBlock([NotNull] XSharpParser.WithBlockContext context)
        {
            CheckEmptyBlock(context, context.StmtBlk, "WITH block");
        }
        public override void ExitRepeatStmt([NotNull] XSharpParser.RepeatStmtContext context)
        {
            CheckEmptyBlock(context, context.StmtBlk, "REPEAT statement");
        }

        public override void ExitForeachStmt([NotNull] XSharpParser.ForeachStmtContext context)
        {
            CheckEmptyBlock(context, context.StmtBlk, "FOREACH statement");
        }

        public override void ExitDoStmt([NotNull] XSharpParser.DoStmtContext context)
        {
            if (context.Amp != null)
            {
                NotInCore(context, "DO Statement with procedure name inside variable");
            }
        }

        public override void ExitLocalfuncproc([NotNull] XSharpParser.LocalfuncprocContext context)
        {
            if (context.T2 != null && context.T2.Token.Type != context.T.Token.Type)
            {
                _parseErrors.Add(new ParseErrorData(context.T2, ErrorCode.ERR_UnExpectedExpected, context.T2.Token.Text, context.T.Token.Text));
            }
            if (context.T.Token.Type == XSharpParser.PROCEDURE)
            {
                if (context.Sig.Type != null && context.Sig.Type.Start.Type != XSharpLexer.VOID)
                {
                    _parseErrors.Add(new ParseErrorData(context.Sig.Type.Start, ErrorCode.ERR_UnexpectedToken, context.Sig.Type.GetText()));
                }
            }
            else
            {
                // local function should have type
                if (context.Sig.Type == null)
                {
                    _parseErrors.Add(new ParseErrorData(context.Sig.Stop, ErrorCode.ERR_TypeExpected));
                }
            }
            if (context.Sig.ParamList != null)
            {
                foreach (var param in context.Sig.ParamList._Params)
                {
                    // parameters for local functions should have type
                    if (param.Type == null)
                    {
                        _parseErrors.Add(new ParseErrorData(param.Stop, ErrorCode.ERR_TypeExpected));

                    }
                    if (param.Attributes != null)
                    {
                        _parseErrors.Add(new ParseErrorData(param.Stop, ErrorCode.ERR_AttributesInLocalFuncDecl));
                    }
                }
            }
        }

        public override void ExitSignature([NotNull] XSharpParser.SignatureContext context)
        {
            if (context.ExpressionBody != null)
            {
                var parent = context.Parent as XSharpParser.IMemberWithBodyContext;
                if (parent != null && parent.Statements != null)
                {
                    CheckExpressionBodyStatements(context.ExpressionBody, parent.Statements);
                }
                // Clipper calling convention is not supported
                if (context.ParamList?._Params?.Count > 0)
                {
                    bool hasClipper = false;
                    foreach (XSharpParser.ParameterContext param in context.ParamList._Params)
                    {
                        if (param.Type == null)
                        {
                            hasClipper = true;
                            break;
                        }
                    }
                    if (hasClipper)
                    {
                        _parseErrors.Add(new ParseErrorData(context.ExpressionBody, ErrorCode.ERR_ExpressionBodyClipperCallingConvention));
                    }
                }
            }
        }

        public override void ExitFuncproc([NotNull] XSharpParser.FuncprocContext context)
        {
            if (context.InitExit != null)
            {
                if (!_options.HasRuntime)
                {
                    _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_FeatureNotAvailableInDialect, "Init/Exit procedure", _options.Dialect.ToString()));
                }

                if (context.T.Token.Type != XSharpParser.PROCEDURE)
                {
                    _parseErrors.Add(new ParseErrorData(context.InitExit, ErrorCode.ERR_FunctionsCannotHaveInitExit));
                }
                else
                {
                    if (context.Sig.ParamList?._Params.Count > 0)
                    {
                        _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_InitProceduresCannotDefineParameters));
                    }
                }
            }

            if (context.T2 != null && context.T2.Token.Type != context.T.Token.Type)
            {
                _parseErrors.Add(new ParseErrorData(context.T2, ErrorCode.ERR_UnExpectedExpected, context.T2.Token.Text, context.T.Token.Text));
            }

        }

        public override void ExitLocalvar([NotNull] XSharpParser.LocalvarContext context)
        {
            bool isDim = context.Dim != null;
            bool hasArraySub = context.ArraySub != null;
            if (isDim && !hasArraySub)
            {
                _parseErrors.Add(new ParseErrorData(context.DIM(), ErrorCode.ERR_ArrayInitializerExpected));
            }
            if (!isDim && hasArraySub && _options.Dialect == XSharpDialect.Core)
            {
                _parseErrors.Add(new ParseErrorData(context.ArraySub, ErrorCode.ERR_FeatureNotAvailableInDialect, "Indexed Local", _options.Dialect.ToString()));
            }

        }

        public override void ExitFoxdimvar([NotNull] XSharpParser.FoxdimvarContext context)
        {
            // only comes here in the Fox dialect
            if (context._Dims.Count > 2)
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_FoxDimensionDeclaration));
            }
        }

        public override void ExitForStmt([NotNull] XSharpParser.ForStmtContext context)
        {
            IToken Op = null;
            if (context.AssignExpr is XSharpParser.BinaryExpressionContext)
            {
                var bin = context.AssignExpr as XSharpParser.BinaryExpressionContext;
                Op = bin.Op;
            }
            else if (context.AssignExpr is XSharpParser.AssignmentExpressionContext)
            {
                var ass = context.AssignExpr as XSharpParser.AssignmentExpressionContext;
                Op = ass.Op;
            }
            if (Op != null && Op.Type != XSharpParser.ASSIGN_OP && !_options.HasOption(CompilerOption.AllowOldStyleAssignments, context, _pragmas))
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_AssignmentOperatorExpected));
            }
            CheckEmptyBlock(context, context.StmtBlk, "FOR statement");
        }
        public override void ExitIfStmt([NotNull] XSharpParser.IfStmtContext context)
        {
            // empty IF and ELSEIF are handled in the condblock
            CheckEmptyBlock(context.ElseStmtBlk, context.ElseStmtBlk, "ELSE block");
        }
        public override void ExitCaseStmt([NotNull] XSharpParser.CaseStmtContext context)
        {
            // empty CASE blocks are handled in the Exitcondblock method
            if (context._CaseBlocks.Count == 0 && context.OtherwiseStmtBlk == null)
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.WRN_EmptyStatement, "DO CASE statement"));
            }
            CheckEmptyBlock(context.OtherwiseStmtBlk, context.OtherwiseStmtBlk, "OTHERWISE block");
        }
        public override void ExitCondBlock([NotNull] XSharpParser.CondBlockContext context)
        {
            if (context.Then != null && context.st != null && context.st.Type == XSharpLexer.CASE)
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_UnexpectedToken, context.Then));
            }
            CheckEmptyBlock(context, context.StmtBlk, context.st.Text + " block");
        }
        public override void ExitTryStmt([NotNull] XSharpParser.TryStmtContext context)
        {
            if (context._CatchBlock.Count == 0 && context.FinBlock == null)
            {
                var errdata = new ParseErrorData(context, ErrorCode.WRN_TryWithoutCatch);
                _parseErrors.Add(errdata);
            }
            CheckEmptyBlock(context, context.StmtBlk, "TRY block");

            if (context.FinBlock != null )
            {
                CheckEmptyBlock(context.FinBlock, context.FinBlock, "FINALLY block");
            }
        }
        public override void ExitCatchBlock([NotNull] XSharpParser.CatchBlockContext context)
        {
            CheckEmptyBlock(context, context.StmtBlk, "CATCH block");
        }
        public override void ExitSwitchStmt([NotNull] XSharpParser.SwitchStmtContext context)
        {
            if (context._SwitchBlock.Count == 0)
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.WRN_EmptyStatement, "SWITCH statement"));
            }
        }
        public override void ExitSwitchBlock([NotNull] XSharpParser.SwitchBlockContext context)
        {
            // Note that switch blocks MAY be empty!
        }
        public override void ExitSeqStmt([NotNull] XSharpParser.SeqStmtContext context)
        {
            NotInCore(context, "BEGIN SEQUENCE statement");
            CheckEmptyBlock(context, context.StmtBlk, "BEGIN SEQUENCE statement");
            if (context.FinBlock != null)
            {
                CheckEmptyBlock(context.FinBlock, context.FinBlock, "FINALLY block");
            }
        }

        public override void ExitBlockStmt([NotNull] XSharpParser.BlockStmtContext context)
        {
            if (context.Key2 != null)
            {
                if (context.Key1.Token.Type != context.Key2.Token.Type)
                {
                    _parseErrors.Add(new ParseErrorData(context.Key2, ErrorCode.ERR_UnexpectedToken, context.Key2.Token.Text));
                }
            }
            // some rules have the token in the Key. Others in Key1.
            var token = context.Key;
            if (context.Key1 != null)
            {
                token = context.Key1.Token;
            }
            CheckEmptyBlock(context, context.StmtBlk, token.Text + " block");
        }

        public override void ExitBinaryExpression([NotNull] XSharpParser.BinaryExpressionContext context)
        {
            if (context.Left != null && context.Right == null)
            {
                var err = ErrorCode.ERR_SyntaxError;
                IToken anchor = context.Stop;
                if (anchor == null)
                    anchor = context.Start;
                var errdata = new ParseErrorData(anchor, err, "Expression after '" + context.Op.Text + "' operator");
                _parseErrors.Add(errdata);
            }
        }

        public override void ExitLiteralValue([NotNull] XSharpParser.LiteralValueContext context)
        {
            if (context.Token.Type == XSharpLexer.INCOMPLETE_STRING_CONST)
            {
                var err = ErrorCode.ERR_UnterminatedStringLit;
                IToken anchor = context.Stop;
                if (anchor == null)
                    anchor = context.Start;
                var errdata = new ParseErrorData(anchor, err);
                _parseErrors.Add(errdata);
            }
            else if (context.Token.Type == XSharpLexer.INVALID_NUMBER)
            {
                var err = ErrorCode.ERR_InvalidNumber;
                IToken anchor = context.Stop;
                if (anchor == null)
                    anchor = context.Start;
                var errdata = new ParseErrorData(anchor, err);
                _parseErrors.Add(errdata);
            }
        }
        public override void ExitFoxtextoutStmt(XSharpParser.FoxtextoutStmtContext context)
        {
            if (_options.Dialect != XSharpDialect.FoxPro)
            {
                NotInDialect(context, "TextMerge output statement ('\\' or '\\\\')");
            }
        }

        public override void ExitFoxdll(XSharpParser.FoxdllContext context)
        {
            if (_options.Dialect != XSharpDialect.FoxPro)
            {
                NotInDialect(context, "DECLARE - DLL statement");
            }
            if (context.Attributes != null)
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.WRN_Unsupported, "Adding Attributes to a DECLARE - DLL declaration"));
            }

        }

        public override void ExitFielddecl(XSharpParser.FielddeclContext context)
        {
            NotInCore(context, "FIELD statement");
        }
        public override void EnterFoxfield([NotNull] XSharpParser.FoxfieldContext context)
        {
            string name = context.F.Name.GetText();
            if (name.EndsWith("_COMATTRIB", System.StringComparison.OrdinalIgnoreCase))
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.WRN_FoxUnsupportedClause, "PEMName_COMATTRIB"));
            }
        }

        public override void ExitXFunctionExpression([NotNull] XSharpParser.XFunctionExpressionContext context)
        {
            NotInCore(context, context.XFunc.GetText().ToUpper() + " function");
        }

        public override void ExitFoxclsctor([NotNull] XSharpParser.FoxclsctorContext context)
        {
            if (_options.fox1)
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_FoxCtorDtor));
            }
        }
        public override void ExitFoxclsdtor([NotNull] XSharpParser.FoxclsdtorContext context)
        {
            if (_options.fox1)
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_FoxCtorDtor));
            }
        }
        public override void ExitFoxclass([NotNull] XSharpParser.FoxclassContext context)
        {
            if (context.BaseType == null && _options.fox1)
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_FoxAsClauseMandatory));
            }
            if (context.OLEPUBLIC() != null)
            {
                _parseErrors.Add(new ParseErrorData(context.OLEPUBLIC(), ErrorCode.WRN_FoxUnsupportedClause, "OLEPUBLIC"));
            }
        }

        public override void ExitFoxpemcomattrib([NotNull] XSharpParser.FoxpemcomattribContext context)
        {
            _parseErrors.Add(new ParseErrorData(context, ErrorCode.WRN_FoxUnsupportedClause, "PEMName_COMATTRIB"));
        }
        public override void ExitFoximplementsclause([NotNull] XSharpParser.FoximplementsclauseContext context)
        {
            if (context.Excl != null)
            {
                _parseErrors.Add(new ParseErrorData(context.Excl, ErrorCode.WRN_FoxUnsupportedClause, "EXCLUDED"));
            }
            if (context.Library != null)
            {
                _parseErrors.Add(new ParseErrorData(context.Library, ErrorCode.WRN_FoxUnsupportedClause, "IN <classlibrary>"));
            }
        }

        public override void ExitConstructor([NotNull] XSharpParser.ConstructorContext context)
        {
            if (context.Modifiers?.EXTERN().Length > 0)
            {
                if (context.StmtBlk._Stmts?.Count > 0)
                {
                    _parseErrors.Add(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_ExternHasBody, "Constructor"));
                }
            }
            if (context.isInInterface())
            {
                _parseErrors.Add(new ParseErrorData(context.c1, ErrorCode.ERR_InterfacesCantContainConstructors));
            }
            CheckExpressionBodyStatements(context.ExpressionBody, context.Statements);
        }
        public override void ExitOperator_([NotNull] XSharpParser.Operator_Context context)
        {
            if (context.Modifiers?.EXTERN().Length > 0)
            {
                if (context.StmtBlk?._Stmts?.Count > 0)
                {
                    _parseErrors.Add(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_ExternHasBody, "Operator"));
                }
            }
            CheckExpressionBodyStatements(context.ExpressionBody, context.Statements);
        }
        public override void ExitClsvars([NotNull] XSharpParser.ClsvarsContext context)
        {
            if (context.isInInterface())
            {
                _parseErrors.Add(new ParseErrorData(context.Member, ErrorCode.ERR_InterfacesCantContainFields));
            }

        }

        private void interfacesCannotHaveTypes([NotNull] XSharpParser.ClassmemberContext context)
        {
            //if (context.isInInterface())
            //{
            //    _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_InterfacesCannotContainTypes));
            //}
        }

        public override void ExitVodllmethod([NotNull] XSharpParser.VodllmethodContext context)
        {
            _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_DLLMethodNotSupported));
        }

        public override void ExitVodll([NotNull] XSharpParser.VodllContext context)
        {
            if (context.Attributes != null)
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.WRN_Unsupported, "Adding Attributes to a _DLL declaration"));
            }

            if (context.Ordinal != null)
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_InvalidDLLEntryPoint,
                    "A numeric entrypoint (" + context.Ordinal.Text.Substring(1) + ") is not supported in .Net"));
            }
            if (context.Address != null || context.Number != null)
            {
                if (context.Address == null || context.Number == null)
                {
                    _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_InvalidDLLEntryPoint, "Both the @ sign and the number must be specified"));
                }
                else if (context.Address.StartIndex > context.Entrypoint.stop.StopIndex + 1
                    || context.Number.StartIndex > context.Address.StopIndex + 1)
                {
                    _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_InvalidDLLEntryPoint, "No spaces allowed in entrypoint name"));
                }
            }
            if (context.CharSet != null)
            {
                var text = context.CharSet.Text.ToUpper();
                if (text != "AUTO" && text != "ANSI" && text != "UNICODE")
                {
                    _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_UnExpectedExpected, context.CharSet.Text, "AUTO', 'ANSI' or 'UNICODE"));
                }
            }
        }
        public override void ExitNestedClass([NotNull] XSharpParser.NestedClassContext context)
        {
            interfacesCannotHaveTypes(context);
        }
        public override void ExitNestedDelegate([NotNull] XSharpParser.NestedDelegateContext context)
        {
            interfacesCannotHaveTypes(context);
        }

        public override void ExitNestedEnum([NotNull] XSharpParser.NestedEnumContext context)
        {
            interfacesCannotHaveTypes(context);
        }

        public override void ExitNestedInterface([NotNull] XSharpParser.NestedInterfaceContext context)
        {
            interfacesCannotHaveTypes(context);
        }

        public override void ExitNestedStructure([NotNull] XSharpParser.NestedStructureContext context)
        {
            interfacesCannotHaveTypes(context);
        }
        public override void ExitDestructor([NotNull] XSharpParser.DestructorContext context)
        {
            if (context.Modifiers?.EXTERN().Length > 0)
            {
                if (context.StmtBlk?._Stmts?.Count > 0)
                {
                    _parseErrors.Add(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_ExternHasBody, "Destructor"));
                }
            }
            if (context.isInInterface())
            {
                _parseErrors.Add(new ParseErrorData(context.d1, ErrorCode.ERR_InterfacesCantContainConstructors));
            }
            CheckExpressionBodyStatements(context.ExpressionBody, context.Statements);
        }


        public override void ExitParameter([NotNull] XSharpParser.ParameterContext context)
        {
            if (context.Ellipsis != null)
            {
                var parlist = context.Parent as XSharpParser.ParameterListContext;
                if (parlist._Params.Last() != context)
                {
                    _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_VarargsLast));
                }
            }
        }

        public override void ExitFoxmemvar([NotNull] XSharpParser.FoxmemvarContext context)
        {
            if (!_options.HasOption(CompilerOption.MemVars, context, _pragmas))
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_DynamicVariablesNotAllowed));
            }
            if ((context.T.Type == XSharpParser.PARAMETERS || context.T.Type == XSharpParser.MEMVAR))
            {
                if (context.Amp != null)
                {
                    _parseErrors.Add(new ParseErrorData(context.Amp, ErrorCode.ERR_IdentifierExpected));
                }
                if (context.Expression != null)
                {
                    var type = context.T.Type == XSharpParser.PARAMETERS ? "Parameters" : "Memory Variables";
                    _parseErrors.Add(new ParseErrorData(context.Expression, ErrorCode.ERR_ParameterInit, type));
                }
            }
        }
        public override void ExitFoxmethod([NotNull] XSharpParser.FoxmethodContext context)
        {
            if (context.HelpString != null)
            {
                _parseErrors.Add(new ParseErrorData(context.HelpString, ErrorCode.WRN_FoxUnsupportedClause, "HELPSTRING"));
            }
            if (context.ThisAccess != null)
            {
                _parseErrors.Add(new ParseErrorData(context.ThisAccess, ErrorCode.WRN_FoxUnsupportedClause, "THISACCESS"));
            }
            if (context.T2 != null && context.T2.Token.Type != context.T.Token.Type)
            {
                _parseErrors.Add(new ParseErrorData(context.T2, ErrorCode.ERR_UnExpectedExpected, context.T2.Token.Text, context.T.Token.Text));
            }
        }
        public override void ExitMethod([NotNull] XSharpParser.MethodContext context)
        {
            var t = context.T.Token as XSharpToken;

            var isInInterface = context.isInInterface();
            var isExtern = context.Modifiers?.EXTERN().Length > 0;
            var isAbstract = context.Modifiers?.ABSTRACT().Length > 0;
            var hasbody = context.StmtBlk != null && context.StmtBlk._Stmts.Count > 0;
            if (context.T2 != null && context.T2.Token.Type != t.Type)
            {
                _parseErrors.Add(new ParseErrorData(context.T2, ErrorCode.ERR_SyntaxError, t.Text));

            }
            //if (isInInterface && hasbody)
            // {
            //     _parseErrors.Add(new ParseErrorData(context.Sig.Id, ErrorCode.ERR_InterfaceMemberHasBody));
            // }
            //if (isInInterface && context.ClassId != null)
            //{
            //    _parseErrors.Add(new ParseErrorData(context.ClassId, ErrorCode.ERR_InterfacesCannotContainTypes));
            //}
            if (isInInterface && _options.VoInitAxitMethods)
            {
                var name = context.Sig.Id.GetText().ToLower();
                if (name == "init" || name == "axit")
                {
                    _parseErrors.Add(new ParseErrorData(context.Start, ErrorCode.ERR_InterfacesCantContainConstructors));
                }
            }

            if (isAbstract)
            {
                if (isExtern)
                {
                    _parseErrors.Add(new ParseErrorData(context.Modifiers, ErrorCode.ERR_AbstractAndExtern, "Method"));
                }
                if (hasbody)
                {
                    _parseErrors.Add(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_AbstractHasBody, "Method"));
                }
            }
            else if (isExtern)
            {
                if (hasbody)
                {
                    _parseErrors.Add(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_ExternHasBody, "Method"));
                }
            }
            if (context.T.Token.Type == XSharpParser.ASSIGN || context.T.Token.Type == XSharpParser.ACCESS)
            {
                // no type parameters on access and assign
                if (context.Sig != null && (context.Sig.TypeParameters != null || context.Sig._ConstraintsClauses?.Count > 0))
                {
                    _parseErrors.Add(new ParseErrorData(context, ErrorCode.Err_TypeParametersAccessAssign));
                }
            }

        }

        public override void ExitXppaccessors([NotNull] XSharpParser.XppaccessorsContext context)
        {
            if (_options.Dialect == XSharpDialect.XPP)
            {
                var tokens = context._Tokens.Where((t) => t.Type == XSharpParser.ACCESS);
                if (tokens.Count() > 1)
                {
                    _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_XPPAccessorCount, tokens.First().Text));
                }
                tokens = context._Tokens.Where((t) => t.Type == XSharpParser.ASSIGN);
                if (tokens.Count() > 1)
                {
                    _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_XPPAccessorCount, tokens.First().Text));
                }
            }
        }

        public override void ExitXppclass([NotNull] XSharpParser.XppclassContext context)
        {
            if (_options.Dialect == XSharpDialect.XPP)
            {
                if (context._BaseTypes.Count == 1 && context.From.Type != XSharpParser.SHARING)
                {
                    _parseErrors.Add(new ParseErrorData(context, ErrorCode.WRN_XPPSuperIVarsAlwaysShared));
                }
                if (context._BaseTypes.Count > 1)
                {
                    _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_XPPMultipleInheritance));
                }
                if (context.TypeParameters != null)
                {
                    NotInDialect(context.TypeParameters, "Type parameters for Classes in XPP Syntax");
                }
                if (context._ConstraintsClauses?.Count > 0)
                {
                    NotInDialect(context._ConstraintsClauses.First(), "Type parameter constraints for Classes in XPP Syntax");
                }
            }
            else
            {
                // Should not happen, the XPP Class syntax only exists in the Xbase++ dialect
                NotInDialect(context, "Xbase++ CLASS Syntax");
            }
        }
        public override void ExitXppmethod([NotNull] XSharpParser.XppmethodContext context)
        {
            if (_options.Dialect != XSharpDialect.XPP)
            {
                NotInDialect(context, "Xbase++ METHOD Syntax");
            }
            CheckExpressionBodyStatements(context.Sig.ExpressionBody, context.Statements);
        }
        public override void ExitXppinlineMethod([NotNull] XSharpParser.XppinlineMethodContext context)
        {
            if (_options.Dialect != XSharpDialect.XPP)
            {
                NotInDialect(context, "Xbase++ METHOD Syntax");
            }
            CheckExpressionBodyStatements(context.Sig.ExpressionBody, context.Statements);
        }

        public override void ExitXppdeclareMethod([NotNull] XSharpParser.XppdeclareMethodContext context)
        {
            if (_options.Dialect == XSharpDialect.XPP)
            {
                if (context.Is?.ChildCount > 0)
                {
                    _parseErrors.Add(new ParseErrorData(context.Is, ErrorCode.WRN_XPPVarIsInNotSupported));
                }
            }
        }
        public override void ExitXppclassvars([NotNull] XSharpParser.XppclassvarsContext context)
        {
            if (_options.Dialect == XSharpDialect.XPP)
            {
                if (context.Is?.ChildCount > 0)
                {
                    _parseErrors.Add(new ParseErrorData(context.Is, ErrorCode.WRN_XPPVarIsInNotSupported));
                }
                if (context.Shared != null)
                {
                    _parseErrors.Add(new ParseErrorData(context.Shared, ErrorCode.WRN_XPPSharedIsDefault));
                }
            }
        }
        public override void ExitXppclassModifiers([NotNull] XSharpParser.XppclassModifiersContext context)
        {
            if (_options.Dialect == XSharpDialect.XPP)
            {
                foreach (var m in context._Tokens)
                {
                    if (m.Type == XSharpParser.FREEZE)
                    {
                        _parseErrors.Add(new ParseErrorData(m, ErrorCode.WRN_XPPFreezeNotSupported));
                    }
                }
            }
        }

        public override void ExitRecoverBlock([NotNull] XSharpParser.RecoverBlockContext context)
        {
            NotInCore(context, "RECOVER USING block");
            CheckEmptyBlock(context, context.StmtBlock, "RECOVER block");
            return;
        }

        public override void ExitVostruct([NotNull] XSharpParser.VostructContext context)
        {
            if (_options.Dialect != XSharpDialect.VO && _options.Dialect != XSharpDialect.Vulcan)
            {
                NotInDialect(context, "VOSTRUCT");
            }
        }

        public override void ExitVounion([NotNull] XSharpParser.VounionContext context)
        {
            if (_options.Dialect != XSharpDialect.VO && _options.Dialect != XSharpDialect.Vulcan)
            {
                NotInDialect(context, "UNION");
            }
        }
        public override void ExitArrayOfType([NotNull] XSharpParser.ArrayOfTypeContext context)
        {
            if (!_options.XSharpRuntime)
            {
                NotInDialect(context, "ARRAY OF <type>");
            }
        }
        public override void ExitXbaseType([NotNull] XSharpParser.XbaseTypeContext context)
        {
            NotInCore(context, context.Token.Text);
            if (context.Token.Type == XSharpParser.CURRENCY && _options.Dialect != XSharpDialect.Core && !_options.XSharpRuntime)
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_CurrencyTypeNotSupported));
            }
            return;
        }
        public override void ExitFoxtypedecl([NotNull] XSharpParser.FoxtypedeclContext context)
        {
            if (context.Type != null && !(context.Parent is XSharpParser.FoxlparameterContext))
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.WRN_FoxUnsupportedClause, context.As.Text + context.Type.SourceText));
            }
        }

        public override void ExitFoxclasslib([NotNull] XSharpParser.FoxclasslibContext context)
        {
            _parseErrors.Add(new ParseErrorData(context, ErrorCode.WRN_FoxUnsupportedClause, context.Of.Text + context.ClassLib.SourceText));
        }

        public override void ExitAliasedExpression([NotNull] XSharpParser.AliasedExpressionContext context)
        {
            // This rule, part of primary handles all aliased expressions
            NotInCore(context, "ALIAS(->) operator");
            return;
        }
        public override void ExitMacro([NotNull] XSharpParser.MacroContext context)
        {
            NotInCore(context, "MACRO compiler");
            return;
        }
        public override void ExitMacroName([NotNull] XSharpParser.MacroNameContext context)
        {
            NotInCore(context, "MACRO compiler");
            return;
        }
        public override void ExitAccessMemberLate([NotNull] XSharpParser.AccessMemberLateContext context)
        {
            NotInCore(context, "Late bound member access");
            return;
        }

        public override void ExitMemvardecl([NotNull] XSharpParser.MemvardeclContext context)
        {
            if (_options.Dialect == XSharpDialect.Core)
            {
                NotInCore(context, "Dynamic Memory Variables");
                return;
            }
            if (context.T.Type == XSharpParser.LPARAMETERS || context.T.Type == XSharpParser.FIELD)
            {
                // this declares local vars or fields, so always allowed outside of the Core dialect
                return;
            }
            if (!_options.HasOption(CompilerOption.MemVars, context, _pragmas))
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_DynamicVariablesNotAllowed));
            }
        }
        public override void ExitMemvar([NotNull] XSharpParser.MemvarContext context)
        {
            if (!_options.HasOption(CompilerOption.MemVars, context, _pragmas))
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_DynamicVariablesNotAllowed));
            }
            if (context.Expression != null && context.ArraySub != null)
            {
                // can't have both an array specification and a initialization value
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_MemvarInit));
            }
            if (context.T.Type == XSharpParser.PARAMETERS || context.T.Type == XSharpParser.MEMVAR)
            {
                if (context.Amp != null)
                {
                    _parseErrors.Add(new ParseErrorData(context.Amp, ErrorCode.ERR_IdentifierExpected));
                }
                if (context.Expression != null)
                {
                    var type = context.T.Type == XSharpParser.PARAMETERS ? "Parameters" : "Memory Variables";
                    _parseErrors.Add(new ParseErrorData(context.Expression, ErrorCode.ERR_ParameterInit, type));
                }
            }
        }

        public override void ExitFilewidememvar([NotNull] XSharpParser.FilewidememvarContext context)
        {
            if (_options.Dialect == XSharpDialect.Core)
            {
                NotInCore(context, "Dynamic Memory Variables");
                return;
            }
            if (!_options.HasOption(CompilerOption.MemVars, context, _pragmas))
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_DynamicVariablesNotAllowed));
            }
            else
            {
                foreach (var memvar in context._XVars)
                {
                    if (memvar.Amp != null)
                    {
                        _parseErrors.Add(new ParseErrorData(memvar, ErrorCode.ERR_UnexpectedCharacter, memvar.Amp.Text));
                    }
                }
                foreach (var memvar in context._FoxVars)
                {
                    if (memvar.Amp != null)
                    {
                        _parseErrors.Add(new ParseErrorData(memvar, ErrorCode.ERR_UnexpectedCharacter, memvar.Amp.Text));
                    }
                }
            }
        }

        public override void ExitClass_([NotNull] XSharpParser.Class_Context context)
        {
            checkMissingKeyword(context.End, context, "END CLASS");
        }
        public override void ExitInterface_([NotNull] XSharpParser.Interface_Context context)
        {
            checkMissingKeyword(context.End, context, "END INTERFACE");
        }
        public override void ExitStructure_([NotNull] XSharpParser.Structure_Context context)
        {
            checkMissingKeyword(context.End, context, "END STRUCTURE");
        }
        public override void ExitEnum_([NotNull] XSharpParser.Enum_Context context)
        {
            checkMissingKeyword(context.End, context, "END [ENUM]");
        }

        public override void ExitNamespace_([NotNull] XSharpParser.Namespace_Context context)
        {
            checkMissingKeyword(context.End, context, "END NAMESPACE");
        }


        public override void ExitClassvar([NotNull] XSharpParser.ClassvarContext context)
        {
            bool isDim = context.Dim != null;
            bool hasArraySub = context.ArraySub != null;
            bool isFixed = (context.Parent as XSharpParser.ClassvarsContext)?.Modifiers?._FIXED != null;
            if (isDim && !hasArraySub)
            {
                _parseErrors.Add(new ParseErrorData(context.DIM(), ErrorCode.ERR_ArrayInitializerExpected));
            }
            if (!isDim && hasArraySub && _options.Dialect == XSharpDialect.Core)
            {
                _parseErrors.Add(new ParseErrorData(context.ArraySub, ErrorCode.ERR_FeatureNotAvailableInDialect, "Indexed Class variable", _options.Dialect.ToString()));
            }
            if (!isDim && isFixed)
            {
                _parseErrors.Add(new ParseErrorData(context.Id, ErrorCode.ERR_SyntaxError, "DIM"));
            }
        }

        // we may want to produce an error here in the future
        public override void ExitPropertyParameterList([NotNull] XSharpParser.PropertyParameterListContext context)
        {
            if (context.L.Type == XSharpLexer.LPAREN && context._Params.Count > 0)
            {
                _parseErrors.Add(new ParseErrorData(context.L, ErrorCode.WRN_PropertyParametersBrackets));
            }
        }

        public override void ExitProperty([NotNull] XSharpParser.PropertyContext context)
        {
            //var isInInterface = context.isInInterface();
            var isExtern = context.Modifiers?.EXTERN().Length > 0;
            var isAbstract = context.Modifiers?.ABSTRACT().Length > 0;
            bool HasBody = (context.Auto != null || context.Multi != null);
            if (!HasBody)
            {
                foreach (var aCtx in context._LineAccessors)
                {
                    if (aCtx.Expr != null && aCtx.ExprList != null)
                    {
                        HasBody = true;
                    }
                }
            }
            if (HasBody)
            {
                //if (isInInterface)
                //{
                //    _parseErrors.Add(new ParseErrorData(context.Start, ErrorCode.ERR_InterfaceMemberHasBody, "Property"));
                //}
                if (isExtern)
                {
                    _parseErrors.Add(new ParseErrorData(context.Start, ErrorCode.ERR_ExternHasBody, "Property"));
                }
                if (isAbstract)
                {
                    _parseErrors.Add(new ParseErrorData(context.Start, ErrorCode.ERR_AbstractHasBody, "Property"));
                }
            }
            if (isAbstract && isExtern)
            {
                _parseErrors.Add(new ParseErrorData(context.Modifiers, ErrorCode.ERR_AbstractAndExtern));
            }
        }

        public override void ExitPropertyAccessor([NotNull] XSharpParser.PropertyAccessorContext context)
        {
            CheckExpressionBodyStatements(context.ExpressionBody, context.StmtBlk);
            if (context.Key2 != null && context.Key2.Type != context.Key.Type)
            {
                _parseErrors.Add(new ParseErrorData(context.Key2, ErrorCode.ERR_UnExpectedExpected, context.Key2.Text, context.Key.Text));

            }
        }
        public override void ExitEventAccessor([NotNull] XSharpParser.EventAccessorContext context)
        {
            CheckExpressionBodyStatements(context.ExpressionBody, context.StmtBlk);
        }
        public override void ExitJumpStmt([NotNull] XSharpParser.JumpStmtContext context)
        {
            if (context.Key.Type == XSharpParser.BREAK)
            {
                NotInCore(context, "BREAK statement");
            }
        }
        public override void ExitFoxexpressionStmt([NotNull] XSharpParser.FoxexpressionStmtContext context)
        {
            if (context.Eq != null && _options.Dialect != XSharpDialect.FoxPro)
            {
                NotInDialect(context, "= Command");
            }
        }
    }

}

