/*
   Copyright 2016-2017 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Collections.Generic;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal class XSharpParseErrorAnalysis : XSharpBaseListener
    {
        private XSharpParser _parser;
        private IList<ParseErrorData> _parseErrors;
        CSharpParseOptions _options;

        public XSharpParseErrorAnalysis(XSharpParser parser, IList<ParseErrorData> parseErrors, CSharpParseOptions options)
        {
            _parser = parser;
            _parseErrors = parseErrors;
            _options = options;
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
                    if (error.Node.Position > start && error.Node.Position < end)
                    {
                        haserrorinblock = true;
                        break;

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

        private void checkMissingToken(IToken l, IToken r, ParserRuleContext context)
        {
            if (l != null && r == null)
            {
                ErrorCode err = ErrorCode.ERR_SyntaxError;
                object par = null;
                switch (l.Type)
                {
                    case XSharpLexer.LPAREN:
                        err = ErrorCode.ERR_CloseParenExpected;
                        break;
                    case XSharpLexer.LCURLY:
                        err = ErrorCode.ERR_RbraceExpected;
                        break;
                    case XSharpLexer.LBRKT:
                        err = ErrorCode.ERR_SyntaxError;
                        par = ']';
                        break;
                }
                IToken anchor = context.Stop;
                if (anchor == null)
                    anchor = l;
                ParseErrorData errdata;
                if (par != null)
                    errdata = new ParseErrorData(anchor, err, par);
                else
                    errdata = new ParseErrorData(anchor, err);
                _parseErrors.Add(errdata);
            }
        }

        private void checkForGarbage(XSharpParserRuleContext context, string msg)
        {
            if (context != null && !_options.Dialect.AllowGarbage())
            {
                NotInDialect(context, msg);
            }
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
            if (context.Op.Type != XSharpParser.ASSIGN_OP && _options.Dialect != XSharpDialect.FoxPro)
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.WRN_AssignmentOperatorExpected));
            }
        }

        // Check for missing end keywords for statement blocks

        public override void ExitWhileStmt([NotNull] XSharpParser.WhileStmtContext context)
        {
            checkMissingKeyword(context.e, context, "END[DO]");
            checkForGarbage(context.Ignored, "Expression after END [DO]");
        }

        public override void ExitWithBlock([NotNull] XSharpParser.WithBlockContext context)
        {
            checkMissingKeyword(context.e, context, "END [WITH]");
        }

        public override void ExitFuncproc([NotNull] XSharpParser.FuncprocContext context)
        {
            if (context.T.Type != XSharpParser.PROCEDURE)
            {
                if (context.InitExit != null)
                {
                    _parseErrors.Add(new ParseErrorData(context.InitExit, ErrorCode.ERR_FunctionsCannotHaveInitExit));
                }
            }
            if (context.T2 != null)
            {
                XSharpToken endToken = (XSharpToken)context.T2;
                if (endToken != null && endToken.Type != context.T.Type)
                {
                    _parseErrors.Add(new ParseErrorData(endToken, ErrorCode.ERR_UnexpectedToken, endToken.SourceSymbol.Text));
                }
            }
        }

        public override void ExitLocalvar([NotNull] XSharpParser.LocalvarContext context)
        {
            bool isDim = context.Dim != null;
            bool hasArraySub = context.ArraySub != null;
            context.SetSequencePoint();
            if (isDim && !hasArraySub)
            {
                _parseErrors.Add(new ParseErrorData(context.DIM(), ErrorCode.ERR_ArrayInitializerExpected)); 
            }
            if (!isDim && hasArraySub && _options.Dialect == XSharpDialect.Core)
            {
                _parseErrors.Add(new ParseErrorData(context.ArraySub, ErrorCode.ERR_FeatureNotAvailableInDialect, "Indexed Local", _options.Dialect.ToString()));
            }
        }

        public override void ExitForStmt([NotNull] XSharpParser.ForStmtContext context)
        {
            checkMissingKeyword(context.e, context, "NEXT");
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
            if (Op != null && Op.Type != XSharpParser.ASSIGN_OP && _options.Dialect != XSharpDialect.FoxPro)
            {
                context.AddError(new ParseErrorData(context, ErrorCode.WRN_AssignmentOperatorExpected));
            }
            checkForGarbage(context.Ignored, "Identifier after NEXT");
        }
        public override void ExitForeachStmt([NotNull] XSharpParser.ForeachStmtContext context)
        {
            checkMissingKeyword(context.e, context, "NEXT");
            checkForGarbage(context.Ignored, "Identifier after NEXT");
        }
        public override void ExitIfStmt([NotNull] XSharpParser.IfStmtContext context)
        {
            checkMissingKeyword(context.e, context, "END[IF]");
            checkForGarbage(context.Ignored, "Identifier after ENDIF");
        }
        public override void ExitCaseStmt([NotNull] XSharpParser.CaseStmtContext context)
        {
            checkMissingKeyword(context.CaseStmt?.Start, context, "CASE or OTHERWISE");
            checkMissingKeyword(context.e, context, "END[CASE]");
        } 
        public override void ExitTryStmt([NotNull] XSharpParser.TryStmtContext context)
        {
            checkMissingKeyword(context.e, context, "END [TRY]");
        }
        public override void ExitSwitchStmt([NotNull] XSharpParser.SwitchStmtContext context)
        {
            checkMissingKeyword(context.e, context, "END [SWITCH]");
        }
        public override void ExitSeqStmt([NotNull] XSharpParser.SeqStmtContext context)
        {
            NotInCore(context, "BEGIN SEQUENCE statement");
            checkMissingKeyword(context.e, context, "END SEQUENCE");
        }

        public override void ExitBlockStmt([NotNull] XSharpParser.BlockStmtContext context)
        {
            checkMissingKeyword(context.e, context, "END [" + context.Key.Text + "]");
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



        public override void ExitTextStmt(XSharpParser.TextStmtContext context)
        {
            if (_options.Dialect != XSharpDialect.FoxPro)
            {
                NotInDialect(context, "TEXT .. ENDTEXT statement");
            }
            return;
        }
        public override void ExitTextoutStmt(XSharpParser.TextoutStmtContext context)
        {
            if (_options.Dialect != XSharpDialect.FoxPro)
            {
                NotInDialect(context, "TextMerge output statement ('\\' or '\\\\')");
            }
        }

        public override void ExitFielddecl(XSharpParser.FielddeclContext context)
        {
           NotInCore(context, "FIELD statement");
        }

        public override void EnterFoxfield([NotNull] XSharpParser.FoxfieldContext context)
        {
            if (_options.Dialect == XSharpDialect.FoxPro)
            {
                string name = context.F.Name.GetText().ToUpper();
                if (name.EndsWith("_COMATTRIB"))
                {
                    _parseErrors.Add(new ParseErrorData(context, ErrorCode.WRN_FoxPEMName_COMATTRIBClause));
                }
            }
        }
        public override void ExitFoxclass([NotNull] XSharpParser.FoxclassContext context)
        {
            if (_options.Dialect != XSharpDialect.FoxPro)
            {
                NotInDialect(context, "FOXPRO Class syntax");
            }
            else
            {
                if (context.Classlib != null)
                {
                    _parseErrors.Add(new ParseErrorData(context, ErrorCode.WRN_FoxOfClasslibClause));
                }
                if (context.OLEPUBLIC() != null)
                {
                    _parseErrors.Add(new ParseErrorData(context, ErrorCode.WRN_FoxOlePublicClause));
                }
            }
        }

        public override void ExitFoxpemcomattrib([NotNull] XSharpParser.FoxpemcomattribContext context)
        {
            if (_options.Dialect == XSharpDialect.FoxPro)
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.WRN_FoxPEMName_COMATTRIBClause));
            }
        }
        public override void ExitFoximplementsclause([NotNull] XSharpParser.FoximplementsclauseContext context)
        {
            if (_options.Dialect == XSharpDialect.FoxPro)
            {
                if (context.Excl != null)
                {
                    _parseErrors.Add(new ParseErrorData(context.Excl, ErrorCode.WRN_FoxImplementsExcludeClause));
                }
                if (context.Library != null)
                {
                    _parseErrors.Add(new ParseErrorData(context.Library, ErrorCode.WRN_FoxImplementsLibraryClause));
                }
            }
        }
        public override void ExitConstructor([NotNull] XSharpParser.ConstructorContext context)
        {
            if (context.Modifiers?.EXTERN().Length > 0)
            {
                if (context.StmtBlk?._Stmts?.Count > 0)
                {
                    _parseErrors.Add(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_ExternHasBody, "Constructor"));
                }
            }
            if (context.isInInterface())
            {
                _parseErrors.Add(new ParseErrorData(context.c1, ErrorCode.ERR_InterfacesCantContainConstructors));
            }
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
            if (context.isInInterface())
            {
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_InterfacesCannotContainTypes));
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
        }

        

        public override void ExitMethod([NotNull] XSharpParser.MethodContext context)
        {
            var isInInterface = context.isInInterface();
            var isExtern = context.Modifiers?.EXTERN().Length > 0;
            var isAbstract = context.Modifiers?.ABSTRACT().Length > 0;
            if (context.T2 != null)
            {
                XSharpToken endToken = (XSharpToken)context.T2.Token;
                if (endToken != null && endToken.Type != context.T.Token.Type)
                {
                    _parseErrors.Add(new ParseErrorData(endToken, ErrorCode.ERR_UnexpectedToken, endToken.SourceSymbol.Text));
                }
            }
            if (context.HelpString != null)
            {
                _parseErrors.Add(new ParseErrorData(context.HelpString, ErrorCode.WRN_FoxHelpStringClause));
            }
            if (context.ThisAccess != null)
            {
                _parseErrors.Add(new ParseErrorData(context.ThisAccess, ErrorCode.WRN_FoxThisAccessClause));
            }
            if (isInInterface && context.StmtBlk != null && context.StmtBlk._Stmts.Count > 0)
            {
                _parseErrors.Add(new ParseErrorData(context.Id, ErrorCode.ERR_InterfaceMemberHasBody));
            }
            if (isInInterface && context.ClassId != null)
            {
                _parseErrors.Add(new ParseErrorData(context.ClassId, ErrorCode.ERR_InterfacesCannotContainTypes));
            }
            if (isInInterface && _options.VoInitAxitMethods)
            {
                var name = context.Id.GetText().ToLower();
                if (name == "init" || name == "axit")
                {
                    _parseErrors.Add(new ParseErrorData(context.Start, ErrorCode.ERR_InterfacesCantContainConstructors));
                }
            }

            if (isAbstract)
            {
                if (isExtern)
                {
                    _parseErrors.Add(new ParseErrorData(context.Modifiers, ErrorCode.ERR_AbstractAndExtern));
                }
                if (context.StmtBlk?._Stmts?.Count > 0)
                {
                    _parseErrors.Add(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_AbstractHasBody));
                }
            }
            else if (isExtern)
            {
                if (context.StmtBlk?._Stmts?.Count > 0)
                {
                    _parseErrors.Add(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_ExternHasBody, "Method"));
                }
            }
            if (context.T.Token.Type == XSharpParser.ASSIGN || context.T.Token.Type == XSharpParser.ACCESS)
            {
                // no type parameters on access and assign
                if (context.TypeParameters != null || context._ConstraintsClauses.Count > 0)
                {
                    context.AddError(new ParseErrorData(context, ErrorCode.Err_TypeParametersAccessAssign));
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
                    context.AddError(new ParseErrorData(context, ErrorCode.ERR_XPPMultipleInheritance));
                }

            }
            else
            {
                NotInDialect(context, "Xbase++ CLASS Syntax");
            }
        }
        public override void ExitXppmethod([NotNull] XSharpParser.XppmethodContext context)
        {
            if (_options.Dialect != XSharpDialect.XPP)
            {
                NotInDialect(context, "Xbase++ METHOD Syntax");
            }
        }

        public override void ExitXppclassvars([NotNull] XSharpParser.XppclassvarsContext context)
        {
            if (_options.Dialect == XSharpDialect.XPP)
            {
                if (context.Is != null)
                {
                    _parseErrors.Add(new ParseErrorData(context.Is, ErrorCode.WRN_XPPVarIsInNotSupported));
                }
                if (context.Shared != null)
                {
                    _parseErrors.Add(new ParseErrorData(context.Shared, ErrorCode.WRN_XPPSharedIsDefault));
                }
                if (context.ReadOnly != null)
                {
                    _parseErrors.Add(new ParseErrorData(context.ReadOnly, ErrorCode.WRN_XPPReadonlyClause));
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
                        _parseErrors.Add(new ParseErrorData(m, ErrorCode.WRN_XPPFrozedNotSupported));
                    }
                }
            }
        }

        public override void ExitRecoverBlock([NotNull] XSharpParser.RecoverBlockContext context)
        {
            NotInCore(context, "RECOVER USING block");
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
            return;
        }

        public override void ExitAliasedMemvar([NotNull] XSharpParser.AliasedMemvarContext context)
        {
            NotInCore(context, "ALIAS(->) operator");
            return;
        }
        public override void ExitAliasedField([NotNull] XSharpParser.AliasedFieldContext context)
        {
            NotInCore(context, "ALIAS(->) operator");
            return;
        }

        public override void ExitAliasedFieldLate([NotNull] XSharpParser.AliasedFieldLateContext context)
        {
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
        public override void ExitAccessMemberLateName([NotNull] XSharpParser.AccessMemberLateNameContext context)
        {
            NotInCore(context, "Late bound member access");
            return;
        }
        public override void ExitXbasedecl([NotNull] XSharpParser.XbasedeclContext context)
        {
            if (_options.Dialect == XSharpDialect.FoxPro && context.T.Type == XSharpParser.LPARAMETERS)
                return;
            if (_options.Dialect != XSharpDialect.Core && context.T.Type == XSharpParser.FIELD  )
                return;
            if (!_options.Dialect.AllowXBaseVariables() || !_options.SupportsMemvars)
            {
                NotInDialect(context, context.T.Text + " statement");
            }
        }
        public override void ExitFilewidememvar([NotNull] XSharpParser.FilewidememvarContext context)
        {
            if (context._Vars.Count > 0)
            {
                NotInCore(context, "File Dynamic Memory Variables");
            }

        }

        public override void ExitNamespace_([NotNull] XSharpParser.Namespace_Context context)
        {
            checkForGarbage(context.Ignored, "Expression after END NAMESPACE");
        }
        public override void ExitInterface_([NotNull] XSharpParser.Interface_Context context)
        {
            checkForGarbage(context.Ignored, "Expression after END INTERFACE");
        }
        public override void ExitClass_([NotNull] XSharpParser.Class_Context context)
        {
            checkForGarbage(context.Ignored, "Expression after END CLASS");
        }

        public override void ExitClassvar([NotNull] XSharpParser.ClassvarContext context)
        {
            bool isDim = context.Dim != null;
            bool hasArraySub = context.ArraySub != null;
            bool isFixed = (context.Parent.Parent as XSharpParser.ClassvarsContext)?.Modifiers?._FIXED != null;
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

        public override void ExitStructure_([NotNull] XSharpParser.Structure_Context context)
        {
            checkForGarbage(context.Ignored, "Expression after END STRUCTURE");
        }
        public override void ExitEnum_([NotNull] XSharpParser.Enum_Context context)
        {
            checkForGarbage(context.Ignored, "Expression after END ENUM");
        }
        public override void ExitEvent_([NotNull] XSharpParser.Event_Context context)
        {
            checkForGarbage(context.Ignored, "Expression after END EVENT");
        }
        public override void ExitProperty([NotNull] XSharpParser.PropertyContext context)
        {
            var isInInterface = context.isInInterface();
            var isExtern = context.Modifiers?.EXTERN().Length > 0;
            var isAbstract = context.Modifiers?.ABSTRACT().Length > 0;
            checkForGarbage(context.Ignored, "Expression after END PROPERTY");
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
                if (isInInterface)
                {
                    _parseErrors.Add(new ParseErrorData(context.Start, ErrorCode.ERR_InterfaceMemberHasBody));
                }
                if (isExtern)
                {
                    _parseErrors.Add(new ParseErrorData(context.Start, ErrorCode.ERR_ExternHasBody, "Property"));
                }
                if (isAbstract)
                {
                    _parseErrors.Add(new ParseErrorData(context.Start, ErrorCode.ERR_AbstractHasBody));
                }
            }
            if (isAbstract && context.Modifiers?.EXTERN().Length > 0)
            {
                _parseErrors.Add(new ParseErrorData(context.Modifiers, ErrorCode.ERR_AbstractAndExtern));
            }
        }
        public override void ExitJumpStmt([NotNull] XSharpParser.JumpStmtContext context)
        {
            if (context.Key.Type == XSharpParser.BREAK)
            {
                NotInCore(context, "BREAK statement");
            }
        }
        public override void ExitExpressionStmt([NotNull] XSharpParser.ExpressionStmtContext context)
        {
            if (context.eq != null && _options.Dialect != XSharpDialect.FoxPro)
            {
                NotInDialect(context, "= Command");
            }
        }
        public override void ExitSource([NotNull] XSharpParser.SourceContext context)
        {
            if (context.StmtBlk != null && context.StmtBlk._Stmts.Count > 0)
            {
                if (_options.Dialect != XSharpDialect.FoxPro)
                {
                    _parseErrors.Add(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_UnexpectedCommand));
                }
            }
        }
    }

}
