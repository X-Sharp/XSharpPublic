//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable

using InternalSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;

using Microsoft.CodeAnalysis.CSharp;
using System.Linq;
using System.Collections.Generic;
using System;
using Antlr4.Runtime.Tree;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Microsoft.CodeAnalysis;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Syntax;

#if !VSPARSER
using MCT = Microsoft.CodeAnalysis.Text;
using CoreInternalSyntax = Microsoft.CodeAnalysis.Syntax.InternalSyntax;

#endif
namespace LanguageService.CodeAnalysis.XSharp.SyntaxParser
{
    internal class XSharpErrorListener : IAntlrErrorListener<IToken>
    {
        readonly String _fileName;
        readonly IList<ParseErrorData> _parseErrors;
        internal XSharpErrorListener(String FileName, IList<ParseErrorData> parseErrors) : base()
        {
            _fileName = FileName;
            _parseErrors = parseErrors;
        }

        public void SyntaxError(IRecognizer recognizer, IToken offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
        {
            if (e?.OffendingToken != null)
            {
                //Debug.WriteLine(_fileName+"(" + e.OffendingToken.Line + "," + e.OffendingToken.Column + "): error: " + msg);
                _parseErrors.Add(new ParseErrorData(e.OffendingToken, ErrorCode.ERR_ParserError, msg));
            }
            else if (offendingSymbol != null)
            {
                //Debug.WriteLine(_fileName + "(" + offendingSymbol.Line + "," + offendingSymbol.Column + "): error: " + msg);
                _parseErrors.Add(new ParseErrorData(offendingSymbol, ErrorCode.ERR_ParserError, msg));
            }
            else
            {
                //Debug.WriteLine(_fileName + "(" + line + 1 + "," + charPositionInLine + 1 + "): error: " + msg);
                _parseErrors.Add(new ParseErrorData(_fileName, ErrorCode.ERR_ParserError, msg));
            }
        }
    }

    public partial class XSharpParser
    {
        public CSharpParseOptions Options { get; set; }
        public bool AllowNamedArgs => Options.AllowNamedArguments;
        public bool IsXPP => Options.Dialect == XSharpDialect.XPP;
        public bool IsFox => Options.Dialect == XSharpDialect.FoxPro;
        public bool IsVO => Options.Dialect switch { XSharpDialect.VO => true, XSharpDialect.Vulcan => true, _ => false };
        public bool IsCoreVO => Options.Dialect switch { XSharpDialect.Core => true, XSharpDialect.VO => true, XSharpDialect.Vulcan => true, _ => false };
        public bool ModernSyntax => Options.ModernSyntax;
        public bool HasMemVars => Options.SupportsMemvars;
        bool ExpectToken(int type)
        {
            int icurrent = 1;
            var la = InputStream.La(icurrent);
            while (la != type && la != EOS && la != Eof)
            {
                icurrent += 1;
                la = InputStream.La(icurrent);
            }
            return la == type;
        }

        bool IsTypeCastAllowed()
        {
            // after we added support for the WITH statement the following code was incorrectly recognized as typecast
            // ? (n):ToString()
            // we don't worry here about the correct order of () and []. The parser rule takes care of that.
            if (InputStream.La(1) != LPAREN)
                return false;
            // Quick check for (DWORD) or similar with type keyword
            if (InputStream.La(3) == RPAREN && XSharpLexer.IsType(InputStream.La(2)))
            {
                return true;
            }
            int nestedlevel = 1;
            int la = 2;
            while (true)
            {
                var c = InputStream.La(la);
                switch (c)
                {
                    case LPAREN:
                        nestedlevel += 1;
                        break;
                    case RPAREN:
                        nestedlevel -= 1;
                        break;
                    case EOS:
                    case Eof:
                        // EOS so no valid typecast
                        return false;
                    default:
                        break;

                }
                if (nestedlevel == 0)
                    break;
                la += 1;
            }
            if (InputStream.La(la) == RPAREN)
            {
                var c = InputStream.La(la + 1);
                return CanFollowCast(c);
            }
            return true;
        }

        private static bool CanFollowCast(int c)
        {
            // This code is derived from the Roslyn code for CanFollowCast() In LanguageParser.Cs
            switch (c)
            {
                // the case order is the same as that method
                case ASTYPE:                          // SyntaxKind.AsKeyword:
                case IS:                              // SyntaxKind.IsKeyword:
                case EOS:                             // SyntaxKind.SemicolonToken:
                case RPAREN:                          // SyntaxKind.CloseParenToken:
                case RBRKT:                           // SyntaxKind.CloseBracketToken:
                //case LCURLY:                          // SyntaxKind.OpenBraceToken:    Disabled because this should be allowed (TYPE) { => dosomething() }
                case RCURLY:                          // SyntaxKind.CloseBraceToken:
                case COMMA:                           // SyntaxKind.CommaToken:
                case ASSIGN:                          // SyntaxKind.EqualsToken:
                case ASSIGN_ADD:                      // SyntaxKind.PlusEqualsToken:
                case ASSIGN_SUB:                      // SyntaxKind.MinusEqualsToken:
                case ASSIGN_MUL:                      // SyntaxKind.AsteriskEqualsToken:
                case ASSIGN_DIV:                      // SyntaxKind.SlashEqualsToken:
                case ASSIGN_MOD:                      // SyntaxKind.PercentEqualsToken:
                case ASSIGN_BITAND:                   // SyntaxKind.AmpersandEqualsToken:
                case ASSIGN_XOR:                      // SyntaxKind.CaretEqualsToken:
                case ASSIGN_BITOR:                    // SyntaxKind.BarEqualsToken:
                case ASSIGN_LSHIFT:                   // SyntaxKind.LessThanLessThanEqualsToken:
                case ASSIGN_RSHIFT:                   // SyntaxKind.GreaterThanGreaterThanEqualsToken:
                case QMARK:                           // SyntaxKind.QuestionToken:
                case COLON:                           // SyntaxKind.ColonToken:
                case OR:                              // SyntaxKind.BarBarToken:
                case AND:                             // SyntaxKind.AmpersandAmpersandToken:
                case PIPE:                            // SyntaxKind.BarToken
                case TILDE:                           // SyntaxKind.CaretToken:
                case AMP:                             // SyntaxKind.AmpersandToken:
                case EQ:                              // Unique in VO
                case EEQ:                             // SyntaxKind.EqualsEqualsToken:
                case NEQ:                             // SyntaxKind.ExclamationEqualsToken:
                case LT:                              // SyntaxKind.LessThanToken:
                case LTE:                             // SyntaxKind.LessThanEqualsToken:
                case GT:                              // SyntaxKind.GreaterThanToken:
                case GTE:                             // SyntaxKind.GreaterThanEqualsToken:
                case LSHIFT:                          // SyntaxKind.LessThanLessThanToken:
                case RSHIFT:                          // SyntaxKind.GreaterThanGreaterThanToken:
                case PLUS:                            // SyntaxKind.PlusToken:
                case MINUS:                           // SyntaxKind.MinusToken:
                case MULT:                            // SyntaxKind.AsteriskToken:
                case DIV:                             // SyntaxKind.SlashToken:
                case MOD:                             // SyntaxKind.PercentToken:
                case DEC:                             // SyntaxKind.PlusPlusToken:
                case INC:                             // SyntaxKind.MinusMinusToken:
                case LBRKT:                           // SyntaxKind.OpenBracketToken:
                case DOT:                             // SyntaxKind.DotToken:
                // case ALIAS                         // SyntaxKind.MinusGreaterThanToken     Disabled we allow (ABC)->FieldName
                case DEFAULT:                         // SyntaxKind.QuestionQuestionToken:
                case Eof:                             // SyntaxKind.EndOfFileToken:
                case UDCSEP:                          // SyntaxKind.EqualsGreaterThanToken: for lambda expressions
                                                      // SyntaxKind.DotDotToken:  // used for ranges. We do not have that
                    return false;
            }
            return true;
        }

        bool validExpressionStmt()
        {
            var la = InputStream.La(2);
            if (la != LPAREN)
                return true;
            la = InputStream.La(1);
            return la != CONSTRUCTOR && la != DESTRUCTOR;
        }
        public partial class ParenExpressionContext
        {
            public ExpressionContext Expr => _Exprs[_Exprs.Count - 1];
        }
        public partial class PragmaContext
        {
            public PragmaBase Pragma;
        }

        #region Interfaces
        public interface IPartialPropertyContext : ITypeContext
        {
#if !VSPARSER
            List<IMethodContext> PartialProperties { get; set; }
#endif
        }

        public interface IGlobalEntityContext : IMemberContext
        {
            FuncprocModifiersContext FuncProcModifiers { get; }
        }
        public interface ILoopStmtContext : IBlockStmtContext
        {
        }
        public interface IBlockStmtContext
        {
            StatementBlockContext Statements { get; }
        }

        public interface IFinallyBlockStmtContext : IBlockStmtContext
        {
            StatementBlockContext FinallyBlockStatements { get; }
        }

        public interface ITypeParametersContext
        {
            public TypeparametersContext TypeParameters { get; }
            public IList<TypeparameterconstraintsclauseContext> _ConstraintsClauses { get; }
        }
        public interface IMemberWithBodyContext : IMemberContext, IBlockStmtContext
        {
        }

        public interface IBodyWithLocalFunctions
        {
            IList<object> LocalFunctions { get; set; }
        }

        public interface ISourceContext
        {
            IList<PragmaOption> PragmaOptions { get; set; }
            IList<EntityContext> Entities { get; }
        }

        public interface ITypeContext : IEntityContext
        {
#if !VSPARSER
            TypeData TypeData { get; }
#endif
        }
        public interface IMemberContext : IEntityContext
        {
#if !VSPARSER
            MemberData Data { get; }
#endif
            DatatypeContext ReturnType { get; }
            ParameterListContext Params { get; }
        }

        public interface ICallContext
        {
            public bool HasRefArguments { get; set; }
            public ArgumentListContext Arguments { get; }
        }
        public interface IMemberWithCC
        {
            public CallingconventionContext CC { get; }
        }
        public interface IEntityContext : IRuleNode, IXParseTree
        {
            string Name { get; }
            string ShortName { get; }
        }

        public interface IMultiElementContext
        {
            int Count { get; }
        }
        public interface IXPPMemberContext : IMemberWithBodyContext, IBodyWithLocalFunctions, ITypeParametersContext
        {
            bool IsStatic { get; }
#if !VSPARSER
            InternalSyntax.XppDeclaredMethodInfo Info { get; }
#endif
            AttributesContext Atts { get; }
            XppmemberModifiersContext Mods { get; }
            void SetStatements(StatementBlockContext stmts);
            ExpressionContext ExpressionBody { get; }
        }
        #endregion
#if !VSPARSER
        #region Flags
        [Flags]
        enum TypeFlags : int
        {
            None = 0,
            HasInstanceCtor = 1 << 1,
            Partial = 1 << 2,
            HasStatic = 1 << 3,        // XPP Class property
            PartialProps = 1 << 4,
            HasInit = 1 << 5,
        }
        [Flags]
        enum MemberFlags : int
        {
            None = 0,
            ClipperCallingConvention = 1 << 0,
            MissingReturnType = 1 << 1,
            UsesPSZ = 1 << 2,
            MustBeUnsafe = 1 << 3,
            HasTypedParameter = 1 << 4,
            HasLParametersStmt = 1 << 5,
            HasParametersStmt = 1 << 6,
            MustBeVoid = 1 << 7,
            IsInitAxit = 1 << 8,
            HasDimVar = 1 << 9,
            HasSync = 1 << 10,
            HasAddressOf = 1 << 11,
            IsInitProcedure = 1 << 12,
            HasMemVars = 1 << 13,
            HasYield = 1 << 14,
            HasFormalParameters = 1 << 15,
            IsEntryPoint = 1 << 16,
            HasUndeclared = 1 << 17,
            HasMemVarLevel = 1 << 18,
            UsesPCount = 1 << 19,
            ParameterAssign = 1 << 20,
            HasExplicitVirtual = 1 << 21,
            HasExplicitOverride = 1 << 22,
            IsProperty = 1 << 23,
        }
        #endregion

        public class TypeData
        {
            void setFlags(TypeFlags newFlag, bool set)
            {
                if (set)
                    flags |= newFlag;
                else
                    flags &= ~newFlag;
                return;
            }
            TypeFlags flags;
            public bool HasInstanceCtor
            {
                get { return flags.HasFlag(TypeFlags.HasInstanceCtor); }
                set { setFlags(TypeFlags.HasInstanceCtor, value); }
            }
            public bool Partial
            {
                get { return flags.HasFlag(TypeFlags.Partial); }
                set { setFlags(TypeFlags.Partial, value); }
            }
            public bool PartialProps
            {
                get { return flags.HasFlag(TypeFlags.PartialProps); }
                set { setFlags(TypeFlags.PartialProps, value); }
            }
            public bool HasStatic
            {
                get { return flags.HasFlag(TypeFlags.HasStatic); }
                set { setFlags(TypeFlags.HasStatic, value); }
            }
        }

        public class MemberData
        {
            MemberFlags flags;
            #region Getters/Setters
            void setFlags(MemberFlags newFlag, bool set)
            {
                if (set)
                    flags |= newFlag;
                else
                    flags &= ~newFlag;
                return;
            }
            public bool HasClipperCallingConvention
            {
                get { return flags.HasFlag(MemberFlags.ClipperCallingConvention); }
                set { setFlags(MemberFlags.ClipperCallingConvention, value); }
            }
            public bool HasDeclaredParameters => HasParametersStmt || HasLParametersStmt || HasFormalParameters;
            public bool HasParametersStmt
            {
                get { return flags.HasFlag(MemberFlags.HasParametersStmt); }
                set { setFlags(MemberFlags.HasParametersStmt, value); }
            }
            public bool HasLParametersStmt
            {
                get { return flags.HasFlag(MemberFlags.HasLParametersStmt); }
                set { setFlags(MemberFlags.HasLParametersStmt, value); }
            }
            public bool HasFormalParameters
            {
                get { return flags.HasFlag(MemberFlags.HasFormalParameters); }
                set { setFlags(MemberFlags.HasFormalParameters, value); }
            }
            public bool HasMissingReturnType
            {
                get { return flags.HasFlag(MemberFlags.MissingReturnType); }
                set { setFlags(MemberFlags.MissingReturnType, value); }
            }
            public bool HasTypedParameter
            {
                get { return flags.HasFlag(MemberFlags.HasTypedParameter); }
                set { setFlags(MemberFlags.HasTypedParameter, value); }
            }
            public bool UsesPSZ
            {
                get { return flags.HasFlag(MemberFlags.UsesPSZ); }
                set { setFlags(MemberFlags.UsesPSZ, value); }
            }
            public bool MustBeUnsafe
            {
                get { return flags.HasFlag(MemberFlags.MustBeUnsafe); }
                set { setFlags(MemberFlags.MustBeUnsafe, value); }
            }

            public bool MustBeVoid            // Assign, SET, Event Accessor
            {
                get { return flags.HasFlag(MemberFlags.MustBeVoid); }
                set { setFlags(MemberFlags.MustBeVoid, value); }
            }
            public bool IsInitAxit            // init or axit with /vo1
            {
                get { return flags.HasFlag(MemberFlags.IsInitAxit); }
                set { setFlags(MemberFlags.IsInitAxit, value); }
            }

            public bool HasDimVar
            {
                get { return flags.HasFlag(MemberFlags.HasDimVar); }
                set { setFlags(MemberFlags.HasDimVar, value); }
            }
            public bool HasSync
            {
                get { return flags.HasFlag(MemberFlags.HasSync); }
                set { setFlags(MemberFlags.HasSync, value); }
            }
            public bool HasAddressOf
            {
                get { return flags.HasFlag(MemberFlags.HasAddressOf); }
                set { setFlags(MemberFlags.HasAddressOf, value); }
            }
            public bool HasMemVars
            {
                get { return flags.HasFlag(MemberFlags.HasMemVars); }
                set { setFlags(MemberFlags.HasMemVars, value); }
            }
            public bool HasUndeclared
            {
                get { return flags.HasFlag(MemberFlags.HasUndeclared); }
                set { setFlags(MemberFlags.HasUndeclared, value); }
            }
            public bool HasMemVarLevel
            {
                get { return flags.HasFlag(MemberFlags.HasMemVarLevel); }
                set { setFlags(MemberFlags.HasMemVarLevel, value); }
            }
            public bool UsesPCount
            {
                get { return flags.HasFlag(MemberFlags.UsesPCount); }
                set { setFlags(MemberFlags.UsesPCount, value); }
            }
            public bool HasYield
            {
                get { return flags.HasFlag(MemberFlags.HasYield); }
                set { setFlags(MemberFlags.HasYield, value); }
            }
            public bool IsInitProcedure
            {
                get { return flags.HasFlag(MemberFlags.IsInitProcedure); }
                set { setFlags(MemberFlags.IsInitProcedure, value); }
            }
            public bool IsEntryPoint
            {
                get { return flags.HasFlag(MemberFlags.IsEntryPoint); }
                set { setFlags(MemberFlags.IsEntryPoint, value); }
            }
            public bool IsProperty
            {
                get { return flags.HasFlag(MemberFlags.IsProperty); }
                set { setFlags(MemberFlags.IsProperty, value); }
            }
            public bool ParameterAssign
            {
                get { return flags.HasFlag(MemberFlags.ParameterAssign); }
                set { setFlags(MemberFlags.ParameterAssign, value); }
            }
            public bool HasExplicitOverride
            {
                get { return flags.HasFlag(MemberFlags.HasExplicitOverride); }
                set { setFlags(MemberFlags.HasExplicitOverride, value); }
            }
            public bool HasExplicitVirtual
            {
                get { return flags.HasFlag(MemberFlags.HasExplicitVirtual); }
                set { setFlags(MemberFlags.HasExplicitVirtual, value); }
            }
            #endregion
            internal Dictionary<string, MemVarFieldInfo> Fields = null;
            internal MemVarFieldInfo AddField(string name, string Alias, XSharpParserRuleContext context)
            {
                if (Fields == null)
                {
                    Fields = new Dictionary<string, MemVarFieldInfo>(XSharpString.Comparer);
                }
                MemVarFieldInfo info;
                if (Fields.ContainsKey(name))
                {
                    info = Fields[name];
                }
                else
                {
                    info = new MemVarFieldInfo(name, Alias, context);
                    Fields.Add(info.Name, info);
                }
                if (!info.IsMacroMemvar)
                {
                    if (info.Name != info.FullName && !Fields.ContainsKey(info.FullName))
                        Fields.Add(info.FullName, info);
                }
                return info;
            }
            internal MemVarFieldInfo GetField(string name)
            {
                if (Fields != null && Fields.ContainsKey(name))
                {
                    return Fields[name];
                }
                return null;
            }
        }
#endif
        public partial class ScriptContext : IEntityContext
        {
            public string Name => null;
            public string ShortName => null;
        }

        public partial class MethodCallContext : ICallContext
        {
            public bool HasRefArguments { get; set; }
            public ArgumentListContext Arguments => ArgList;

        }
        public partial class BoundMethodCallContext : ICallContext
        {
            public bool HasRefArguments { get; set; }
            public ArgumentListContext Arguments => ArgList;
        }
        public partial class CtorCallContext : ICallContext
        {
            public bool HasRefArguments { get; set; }
            public ArgumentListContext Arguments => ArgList;
        }

        public partial class DoStmtContext : ICallContext
        {
            public bool HasRefArguments { get; set; }
            public ArgumentListContext Arguments => ArgList;
        }
        public partial class SourceContext : ISourceContext
        {
            public IList<PragmaOption> PragmaOptions { get; set; }
            public IList<EntityContext> Entities => _Entities;
        }

        public partial class FoxsourceContext : ISourceContext
        {
            public IList<PragmaOption> PragmaOptions { get; set; }
            public IList<EntityContext> Entities => _Entities;
        }

        public partial class BlockStmtContext : IBlockStmtContext
        {
            public StatementBlockContext Statements { get { return StmtBlk; } }
        }
        public partial class WithBlockContext : IBlockStmtContext
        {
            public StatementBlockContext Statements { get { return StmtBlk; } }
        }
        public partial class CondBlockContext : IBlockStmtContext
        {
            public StatementBlockContext Statements { get { return StmtBlk; } }
        }
        public partial class SeqStmtContext : IFinallyBlockStmtContext
        {
            public StatementBlockContext Statements { get { return StmtBlk; } }
            public StatementBlockContext FinallyBlockStatements { get { return FinBlock; } }
        }
        public partial class TryStmtContext : IFinallyBlockStmtContext
        {
            public StatementBlockContext Statements { get { return StmtBlk; } }
            public StatementBlockContext FinallyBlockStatements { get { return FinBlock; } }
        }

        public partial class CatchBlockContext : IBlockStmtContext
        {
            public StatementBlockContext Statements { get { return StmtBlk; } }
        }
        public partial class RecoverBlockContext : IBlockStmtContext
        {
            public StatementBlockContext Statements { get { return StmtBlock; } }
        }

        public partial class SwitchBlockContext : IBlockStmtContext
        {
            public StatementBlockContext Statements { get { return StmtBlk; } }
        }

        public partial class RepeatStmtContext : ILoopStmtContext
        {
            public StatementBlockContext Statements { get { return StmtBlk; } }
        }
        public partial class WhileStmtContext : ILoopStmtContext
        {
            public StatementBlockContext Statements { get { return StmtBlk; } }
        }
        public partial class ForeachStmtContext : ILoopStmtContext
        {
            public StatementBlockContext Statements { get { return StmtBlk; } }
        }
        public partial class ForStmtContext : ILoopStmtContext
        {
            public StatementBlockContext Statements { get { return StmtBlk; } }
        }

        public partial class LocalfuncprocContext : IMemberWithBodyContext, IBodyWithLocalFunctions, ITypeParametersContext
        {
            public IdentifierContext Id => this.Sig.Id;
            public TypeparametersContext TypeParameters => this.Sig.TypeParameters;
            public IList<TypeparameterconstraintsclauseContext> _ConstraintsClauses => this.Sig._ConstraintsClauses;
            public ParameterListContext ParamList => this.Sig.ParamList;
            public DatatypeContext ReturnType => this.Sig.ReturnType;
#if !VSPARSER

            readonly MemberData _data = new();
            public MemberData Data => _data;
#endif
            public ParameterListContext Params => this.ParamList;
            public string Name => ParentName + ShortName;
            public string ShortName => this.Id.GetText();
            public LocalfuncprocModifiersContext LocalFuncProcModifiers => Modifiers;
            public StatementBlockContext Statements => StmtBlk;
            public IList<object> LocalFunctions { get; set; } = null;
        }

        public partial class FuncprocContext : IMemberWithBodyContext, IGlobalEntityContext, IBodyWithLocalFunctions, ITypeParametersContext, IMemberWithCC
        {

            public IdentifierContext Id => this.Sig.Id;
            public TypeparametersContext TypeParameters => this.Sig.TypeParameters;
            public IList<TypeparameterconstraintsclauseContext> _ConstraintsClauses => this.Sig._ConstraintsClauses;
            public ParameterListContext ParamList => this.Sig.ParamList;
            public CallingconventionContext CC => this.Sig.CallingConvention;
            public DatatypeContext ReturnType => this.Sig.ReturnType;

#if !VSPARSER
            readonly MemberData _data = new();
            public MemberData Data => _data;
#endif
            public ParameterListContext Params => this.ParamList;
            public string Name => ParentName + ShortName;
            public string ShortName => this.Id.GetText();
            public FuncprocModifiersContext FuncProcModifiers => Modifiers;
            public StatementBlockContext Statements => StmtBlk;
            public int RealType { get; set; } // fox FoxPro Function and Procedure will be come method, access or assign
            public IList<object> LocalFunctions { get; set; } = null;
        }

        public interface IMethodContext : IMemberWithBodyContext, ITypeParametersContext
        {
            IdentifierContext Id { get; }
            ParameterListContext ParamList { get; }
            MemberModifiersContext Mods { get; }
            ExpressionContext ExpressionBody { get; }

            bool IsInInterface { get; }
            bool IsInStructure { get; }
            int RealType { get; }
        }
        public partial class MethodContext : IMethodContext, IBodyWithLocalFunctions, ITypeParametersContext, IMemberWithCC
        {
            public IdentifierContext Id => this.Sig.Id;
            public TypeparametersContext TypeParameters => this.Sig.TypeParameters;
            public IList<TypeparameterconstraintsclauseContext> _ConstraintsClauses => this.Sig._ConstraintsClauses;
            public ParameterListContext ParamList => this.Sig.ParamList;
            public DatatypeContext ReturnType => this.Sig.ReturnType;
            public CallingconventionContext CC => this.Sig.CallingConvention;
            public ExpressionContext ExpressionBody => this.Sig.ExpressionBody;

            public bool IsInInterface => this.isInInterface();
            public bool IsInStructure => this.isInStructure();
#if !VSPARSER
            readonly MemberData _data = new();
            public MemberData Data => _data;
#endif
            public ParameterListContext Params => this.ParamList;
            public MemberModifiersContext Mods => this.Modifiers;
            public string ShortName => this.Id.GetText();
            public string Name
            {
                get
                {
                    string name = this.Id.GetText();
                    if (this.T.Token.Type == XSharpParser.ACCESS)
                        name += ":Access";
                    else if (this.T.Token.Type == XSharpParser.ASSIGN)
                        name += ":Assign";
                    else
                        name += "()";
                    return ParentName + name;
                }
            }
            public StatementBlockContext Statements => StmtBlk;
            public int RealType { get; set; } // fox FoxPro Function and Procedure will be come method, access or assign
            public IList<object> LocalFunctions { get; set; } = null;

        }

        public partial class FoxmethodContext : IMethodContext, IBodyWithLocalFunctions, ITypeParametersContext, IMemberWithCC
        {
            public IdentifierContext Id => this.Sig.Id;
            public TypeparametersContext TypeParameters => this.Sig.TypeParameters;
            public IList<TypeparameterconstraintsclauseContext> _ConstraintsClauses => this.Sig._ConstraintsClauses;
            public ParameterListContext ParamList => this.Sig.ParamList;
            public DatatypeContext ReturnType => this.Sig.ReturnType;
            public CallingconventionContext CC => this.Sig.CallingConvention;
            public MemberModifiersContext Mods => this.Modifiers;
            public string ShortName => this.Sig.Id.GetText();
            public ParameterListContext Params => this.Sig.ParamList;
            public ExpressionContext ExpressionBody => this.Sig.ExpressionBody;

            public bool IsInInterface => false;
            public bool IsInStructure => false;

#if !VSPARSER
            readonly MemberData _data = new();
            public MemberData Data => _data;
#endif
            public string Name
            {
                get
                {
                    string name = this.Id.GetText();
                    name += "()";
                    return ParentName + name;
                }
            }
            public StatementBlockContext Statements => StmtBlk;
            public int RealType { get; set; } // fox FoxPro Function and Procedure will be come method, access or assign
            public IList<object> LocalFunctions { get; set; } = null;
        }

        public partial class EventAccessorContext : IMemberWithBodyContext, IBodyWithLocalFunctions
        {
#if !VSPARSER
            readonly MemberData _data = new();
            public MemberData Data => _data;
#endif
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => null;
            public string Name => ParentName + Key.Text;
            public string ShortName => ParentName + Key.Text;
            public StatementBlockContext Statements => StmtBlk;
            public bool HasReturnValue => false;
            public IList<object> LocalFunctions { get; set; } = null;
        }

        public partial class PropertyAccessorContext : IMemberWithBodyContext, IBodyWithLocalFunctions
        {
#if !VSPARSER
            readonly MemberData _data = new();
            public MemberData Data => _data;
#endif
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => null;
            public string Name => ParentName + Key.Text;
            public string ShortName => ParentName + Key.Text;
            public StatementBlockContext Statements => StmtBlk;
            public IList<object> LocalFunctions { get; set; } = null;
        }
        public partial class PropertyLineAccessorContext : IMemberContext
        {
#if !VSPARSER
            readonly MemberData _data = new();
            public MemberData Data => _data;
#endif
            public CallingconventionContext CC => null;
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => null;
            public string Name => ParentName + Key.Text;
            public string ShortName => ParentName + Key.Text;
        }
        public partial class ConstructorContext : IMemberWithBodyContext, IBodyWithLocalFunctions, IMemberWithCC
        {
#if !VSPARSER
            readonly MemberData _data = new();
            public MemberData Data => _data;
#endif
            public CallingconventionContext CC => CallingConvention;
            public ParameterListContext Params => this.ParamList;
            public DatatypeContext ReturnType => null;
            public string Name => ParentName + ShortName;
            public string ShortName => "ctor";
            public StatementBlockContext Statements => StmtBlk;
            public IList<object> LocalFunctions { get; set; } = null;
        }
        public partial class DestructorContext : IMemberWithBodyContext, IBodyWithLocalFunctions
        {
#if !VSPARSER
            readonly MemberData _data = new();
            public MemberData Data => _data;
#endif
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => null;
            public string Name => ParentName + ShortName;
            public string ShortName => "Finalize";
            public StatementBlockContext Statements => StmtBlk;
            public IList<object> LocalFunctions { get; set; } = null;
        }
        public partial class Event_Context : IMemberContext
        {
#if !VSPARSER
            readonly MemberData _data = new();
            public MemberData Data => _data;
#endif
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => this.Type;
            public string Name => ParentName + ShortName;
            public string ShortName => Id.GetText();
            public IList<object> LocalFunctions { get; set; } = null;
        }
        public partial class VodefineContext : IMemberContext, IGlobalEntityContext
        {
#if !VSPARSER
            readonly MemberData _data = new();
            public MemberData Data => _data;
#endif
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => null;
            public string Name => ParentName + ShortName;
            public string ShortName => Id.GetText();
            public FuncprocModifiersContext FuncProcModifiers => Modifiers;
        }
        public partial class PropertyContext : IMemberContext
        {
#if !VSPARSER
            readonly MemberData _data = new();
            public MemberData Data => _data;
#endif
            public CallingconventionContext CC => null;
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => this.Type;
            public string Name => ParentName + ShortName;
            public string ShortName
            {
                get
                {
                    return Id != null ? Id.GetText() : "Item";
                }
            }
        }
        public partial class Operator_Context : IMemberWithBodyContext, IBodyWithLocalFunctions
        {
#if !VSPARSER
            readonly MemberData _data = new();
            public MemberData Data => _data;
#endif
            public CallingconventionContext CC => null;
            public ParameterListContext Params => this.ParamList;
            public DatatypeContext ReturnType => this.Type;
            public string Name => ParentName + ShortName;
            public string ShortName
            {
                get
                {
                    string name;
                    if (Operation != null)
                        name = Operation.GetText() + Gt?.Text;
                    else
                        name = Conversion.GetText();
                    return name;
                }
            }
            public StatementBlockContext Statements => StmtBlk;
            public IList<object> LocalFunctions { get; set; } = null;
        }
        public partial class Delegate_Context : ITypeContext, IMemberWithBodyContext
        {
#if !VSPARSER
            readonly TypeData _tdata = new();
            readonly MemberData _mdata = new();
            public TypeData TypeData => _tdata;
            public MemberData Data => _mdata;
#endif
            public CallingconventionContext CC => null;
            public ParameterListContext Params => this.ParamList;
            public DatatypeContext ReturnType => this.Type;
            public string Name => ParentName + ShortName;
            public string ShortName => this.Id.GetText();
            public StatementBlockContext Statements => null;
        }
        public partial class Interface_Context : IPartialPropertyContext, ITypeContext
        {
#if !VSPARSER
            readonly TypeData _data = new();
            public TypeData TypeData => _data;
            List<IMethodContext> partialProperties = null;
            public List<IMethodContext> PartialProperties
            {
                get { return partialProperties; }
                set { partialProperties = value; }
            }
#endif
            public string Name => ParentName + ShortName;
            public string ShortName => this.Id.GetText();

        }
        public partial class Class_Context : IPartialPropertyContext, ITypeContext
        {
#if !VSPARSER
            readonly TypeData _data = new();
            public TypeData TypeData => _data;

            List<IMethodContext> partialProperties = null;
            public List<IMethodContext> PartialProperties
            {
                get { return partialProperties; }
                set { partialProperties = value; }
            }
#endif
            public string Name => ParentName + ShortName;
            public string ShortName => Id.GetText();
        }
        public partial class Structure_Context : IPartialPropertyContext, ITypeContext
        {
#if !VSPARSER
            readonly TypeData _data = new();
            public TypeData TypeData => _data;
            List<IMethodContext> partialProperties = null;
            public List<IMethodContext> PartialProperties
            {
                get { return partialProperties; }
                set { partialProperties = value; }
            }
#endif
            public string Name => ParentName + ShortName;
            public string ShortName => Id.GetText();
        }
        public partial class VodllContext : IMemberContext, IGlobalEntityContext
        {
#if !VSPARSER
            readonly MemberData data = new();
            public MemberData Data => data;
#endif
            public CallingconventionContext CC => null;
            public ParameterListContext Params => this.ParamList;
            public DatatypeContext ReturnType => this.Type;
            public string Name => this.Id.GetText();
            public string ShortName => this.Id.GetText();
            public FuncprocModifiersContext FuncProcModifiers => Modifiers;
        }

        public partial class VoglobalContext : IMemberContext, IGlobalEntityContext
        {
#if !VSPARSER
            readonly MemberData data = new();
            public MemberData Data => data;
#endif
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => this._Vars.First().DataType;
            public string Name => this._Vars.First().Id.GetText();
            public string ShortName => this._Vars.First().Id.GetText();
            public FuncprocModifiersContext FuncProcModifiers => Modifiers;
        }
        public partial class FoxdllContext : IMemberContext, IGlobalEntityContext
        {
#if !VSPARSER
            readonly MemberData data = new();
            public MemberData Data => data;
#endif
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => this.Type;
            public string Name => this.Id.GetText();
            public string ShortName => this.Id.GetText();
            public FuncprocModifiersContext FuncProcModifiers => Modifiers;
        }

        public partial class FuncprocModifiersContext
        {
            public bool IsStaticVisible { get; set; }
        }

        public partial class VounionContext : ITypeContext
        {
#if !VSPARSER
            readonly TypeData _data = new();
            public TypeData TypeData => _data;
#endif
            public string Name => this.Id.GetText();
            public string ShortName => this.Id.GetText();

        }
        public partial class VostructContext : ITypeContext
        {
#if !VSPARSER
            readonly TypeData _data = new();
            public TypeData TypeData => _data;
#endif
            public string Name => this.Id.GetText();
            public string ShortName => this.Id.GetText();

        }
        public partial class XppclassContext : ITypeContext
        {
#if !VSPARSER
            readonly TypeData _data = new();
            public TypeData TypeData => _data;
#endif
            public string Name => ParentName + ShortName;
            public string ShortName => Id.GetText();
        }
        public partial class XppclassvarsContext
        {
            public int Visibility { get; set; }
        }

        public partial class XppmethodContext : IXPPMemberContext, IMemberWithCC, ITypeParametersContext
        {
            public bool IsStatic => this.Modifiers != null && this.Modifiers._Tokens.Any(t => t.Type == XSharpLexer.CLASS || t.Type == XSharpLexer.CLASS);
#if !VSPARSER
            readonly MemberData data = new();
            public MemberData Data => data;
            public InternalSyntax.XppDeclaredMethodInfo Info { get; set; }
#endif
            public AttributesContext Atts => this.Attributes;
            public XppmemberModifiersContext Mods => this.Modifiers;
            public CallingconventionContext CC => this.Sig.CallingConvention;
            public ParameterListContext Params => this.Sig.ParamList;
            public DatatypeContext ReturnType => this.Sig.ReturnType;
            public TypeparametersContext TypeParameters => this.Sig.TypeParameters;
            public IList<TypeparameterconstraintsclauseContext> _ConstraintsClauses => this.Sig._ConstraintsClauses;
            public ParameterListContext ParamList => this.Sig.ParamList;
            public string ShortName => this.Sig.Id.GetText();
            public string Name
            {
                get
                {
                    string name = Sig.Id.GetText() + "()";
                    return ParentName + name;
                }
            }
            public StatementBlockContext Statements => StmtBlk;
            public void SetStatements(StatementBlockContext stmts) => this.StmtBlk = stmts;
            public IList<object> LocalFunctions { get; set; } = null;
            public ExpressionContext ExpressionBody => this.Sig.ExpressionBody;
        }
        public partial class XppinlineMethodContext : IXPPMemberContext, IBodyWithLocalFunctions, IMemberWithCC, ITypeParametersContext
        {
            public bool IsStatic => this.Modifiers != null && this.Modifiers._Tokens.Any(t => t.Type == XSharpLexer.CLASS || t.Type == XSharpLexer.CLASS);
#if !VSPARSER
            readonly MemberData data = new();
            public MemberData Data => data;
            public InternalSyntax.XppDeclaredMethodInfo Info { get; set; }
#endif
            public AttributesContext Atts => this.Attributes;
            public XppmemberModifiersContext Mods => this.Modifiers;

            public ParameterListContext Params => this.Sig.ParamList;
            public DatatypeContext ReturnType => this.Sig.ReturnType;
            public string ShortName => this.Sig.Id.GetText();
            public ExpressionContext ExpressionBody => this.Sig.ExpressionBody;
            public CallingconventionContext CC => this.Sig.CallingConvention;
            public TypeparametersContext TypeParameters => this.Sig.TypeParameters;
            public IList<TypeparameterconstraintsclauseContext> _ConstraintsClauses => this.Sig._ConstraintsClauses;
            public ParameterListContext ParamList => this.Sig.ParamList;
            public string Name
            {
                get
                {
                    string name = Sig.Id.GetText() + "()";
                    return ParentName + name;
                }
            }
            public StatementBlockContext Statements => StmtBlk;
            public void SetStatements(StatementBlockContext stmts) => this.StmtBlk = stmts;
            public IList<object> LocalFunctions { get; set; } = null;
        }

        public partial class WithBlockContext
        {
            public string VarName;
        }
        public partial class AliasedExpressionContext
        {
            public bool XSharpRuntime;
        }
        public partial class XppdeclarepropertyContext : IMemberContext
        {
            public bool IsStatic => this.Modifiers != null && this.Modifiers._Tokens.Any(t => t.Type == XSharpLexer.CLASS || t.Type == XSharpLexer.CLASS);
#if !VSPARSER
            readonly MemberData data = new();
            public MemberData Data => data;
#endif
            public AttributesContext Atts => Attributes;
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => this.Type;
            public string ShortName => this.Id.GetText();
            public string Name
            {
                get
                {
                    string name = this.Id.GetText();
                    return ParentName + name;
                }
            }
        }
        public partial class SignatureContext
        {
            public DatatypeContext ReturnType => this.Type;
        }
        public partial class FoxclassContext : IPartialPropertyContext, ITypeContext
        {
#if !VSPARSER
            readonly TypeData _data = new();
            public TypeData TypeData => _data;
            List<IMethodContext> partialProperties = null;

            public List<IMethodContext> PartialProperties
            {
                get { return partialProperties; }
                set { partialProperties = value; }
            }
#endif
            public string Name => ParentName + ShortName;
            public string ShortName => Id.GetText();

        }
        public partial class NamedArgumentContext
        {
            internal bool IsMissing => this.ChildCount == 0;
        }
        public partial class UnnamedArgumentContext
        {
            internal bool IsMissing => Expr == null;
        }

        [Flags]
        internal enum FoxFlags : byte
        {
            None = 0,
            MemberAccess = 1,
            MPrefix = 2,
        }
        public partial class AccessMemberContext
        {
            internal FoxFlags foxFlags = FoxFlags.None;
            internal bool IsFox => foxFlags != FoxFlags.None;
            internal bool HasMPrefix => foxFlags.HasFlag(FoxFlags.MPrefix);
            internal string AreaName => Expr == null ? "" : Expr.GetText().ToUpper();
            internal string FieldName => Name.GetText().ToUpper();
        }
        #region Ruleš with multiple vars or multiple expressions The Count determines how breakpoints are set

        public partial class FoxexpressionStmtContext : IMultiElementContext
        {
            public int Count => _Exprs.Count;
        }
        public partial class ExpressionStmtContext : IMultiElementContext
        {
            public int Count => _Exprs.Count;
        }

        public partial class CommonLocalDeclContext : IMultiElementContext
        {
            public int Count => _LocalVars.Count;
        }

        public partial class VarLocalDeclContext : IMultiElementContext
        {
            public int Count => _ImpliedVars.Count;
        }
        public partial class FoxlparametersContext : IMultiElementContext
        {
            public int Count => this._LParameters.Count;
        }
        public partial class MemvardeclContext : IMultiElementContext
        {
            public int Count => this._Vars.Count;
        }
        public partial class FoxmemvardeclContext : IMultiElementContext
        {
            public int Count => this._FoxVars.Count;
        }
        public partial class ClassvarsContext : IMultiElementContext
        {
            public int Count => this._Vars.Count;
        }
        public partial class VoglobalContext : IMultiElementContext
        {
            public int Count => this._Vars.Count;
        }
        public partial class FilewidevarContext : IMultiElementContext
        {
            public int Count => this._Vars.Count + this._XVars.Count + this._FoxVars.Count;
        }
        public partial class ExpressionListContext : IMultiElementContext
        {
            public int Count => this._Exprs.Count;
        }
        public partial class ParenExpressionContext : IMultiElementContext
        {
            public int Count => this._Exprs.Count;
        }
        public partial class VariableDeclarationContext : IMultiElementContext
        {
            public int Count => this._Decl.Count;
        }
        #endregion
    }

    [DebuggerDisplay("{DebuggerDisplay()}")]
    internal class ParseErrorData
    {
        internal readonly IXParseTree Node;
        internal readonly ErrorCode Code;
        internal readonly object[] Args;
        internal ParseErrorData(IErrorNode enode, ErrorCode code, params object[] args) :
            this(token: enode.Symbol, code: code, args: args)
        {
        }

        internal string DebuggerDisplay()
        {
            if (Node is XTerminalNodeImpl xterm)
                return Code.ToString() + " " + xterm.Symbol.Line.ToString() + " " + xterm.Symbol.Text;
            if (Node.SourceSymbol != null)
                return Code.ToString() + " " + Node.SourceSymbol.Line.ToString() + " " + Node.SourceSymbol.Text;
            return Code.ToString();
        }
        //internal ParseErrorData(ErrorCode code) :
        //    this(node: null, code: code, args: Array.Empty<object>())
        //{ }
        internal ParseErrorData(string fileName, ErrorCode code, params object[] args) :
            this(node: null, code: code, args: args)
        {
            var token = new XSharpToken(0);
            var node = new XTerminalNodeImpl(token)
            {
                SourceFileName = fileName
            };
            this.Node = node;
        }
        internal ParseErrorData(IXParseTree node, ErrorCode code) :
            this(node, code, Array.Empty<object>())
        { }
        internal ParseErrorData(IXParseTree node, ErrorCode code, params object[] args)
        {
            this.Node = node;
            this.Code = code;
            this.Args = args;
        }
        internal ParseErrorData(IToken token, ErrorCode code, params object[] args)
        {
            if (token == null)
                token = new XSharpToken(0, "");
            else if (!(token is XSharpToken))
            {
                token = new XSharpToken(token);
            }
            this.Node = new XTerminalNodeImpl(token);
            this.Code = code;
            this.Args = args;
        }
        internal ParseErrorData(ITerminalNode tnode, ErrorCode code, params object[] args)
        {
            this.Node = new XTerminalNodeImpl(tnode.Symbol);
            this.Code = code;
            this.Args = args;
        }

        /*protected static SyntaxDiagnosticInfo MakeError(CSharpSyntaxNode node, ErrorCode code, params object[] args)
        {
            return new SyntaxDiagnosticInfo(node.GetLeadingTriviaWidth(), node.Width, code, args);
        }*/
        internal static List<ParseErrorData> NewBag()
        {
            return new List<ParseErrorData>();
        }
    }

    public interface IXParseTree : IParseTree
    {
        object CsNode { get; set; }
        int Position { get; }
        int FullWidth { get; }
        string SourceFileName { get; }
        int Line { get; }
        int Column { get; }

        /// <summary>
        /// When the token is coming from an UDC then this is the UDC anchor
        /// </summary>
        XSharpToken SourceSymbol { get; }
#if !VSPARSER
        Microsoft.CodeAnalysis.Location GetLocation(SyntaxTree syntaxTree);
#endif
    }
    [Serializable]
    public class XTerminalNodeImpl : Antlr4.Runtime.Tree.TerminalNodeImpl,
        IFormattable,
        IXParseTree,
        IErrorNode
    {
        private string fileName = null;
        public XTerminalNodeImpl(IToken symbol) : base(symbol)
        { }
        public object CsNode { get; set; }
        public int Column => Symbol.Column;
        public int Line => Symbol.Line;
        public int Position => Symbol.StartIndex;
        public int FullWidth => Symbol.StopIndex - Symbol.StartIndex + 1;
        public string ToString(string format, IFormatProvider formatProvider)
        {
            return ToString();
        }
        public string SourceFileName
        {
            get
            {
                if (fileName != null)
                    return fileName;
                var ct = (Symbol as XSharpToken);
                if (ct != null)
                {
                    if (ct.TokenSource != null && !String.IsNullOrEmpty(ct.TokenSource.SourceName))
                        return ct.TokenSource.SourceName;
                    return ct.SourceName;
                }
                return "<unknown>";
            }
            set => fileName = value;
        }

        public XSharpToken SourceSymbol
        {
            get
            {
                return ((XSharpToken)Symbol).SourceSymbol;
            }
        }
        public override string ToString() { return this.GetText(); }
#if !VSPARSER
        public Microsoft.CodeAnalysis.Location GetLocation(SyntaxTree syntaxTree)
        {
            var token = this.Symbol;
            return new XSharpSourceLocation(token, syntaxTree);
        }
#endif
    }

#if !VSPARSER
    [DebuggerDisplay("{_fieldType} {FullName}")]
    public class MemVarFieldInfo
    {
        private enum MemvarType : byte
        {
            Memvar,
            Field,
            ClipperParameter,
            MacroMemvar,
            Local,
        }
        public bool IsMemvar => _fieldType == MemvarType.Memvar;
        public bool IsField => _fieldType == MemvarType.Field;
        public bool IsClipperParameter => _fieldType == MemvarType.ClipperParameter;
        public bool IsMacroMemvar => _fieldType == MemvarType.MacroMemvar;
        public bool IsLocal => _fieldType == MemvarType.Local;
        [Flags]
        enum FieldFlags : byte
        {
            IsFileWidePublic,
            IsCreated,
            IsParameter,
            IsWritten,
            IsPublic
        }
        private readonly MemvarType _fieldType;
        public string Name { get; private set; }
        public string Alias { get; private set; }
        public string FullName
        {
            get
            {
                if (_fieldType == MemvarType.MacroMemvar)
                {
                    var name = Name.Substring(0, Name.IndexOf(":"));
                    return Alias + "->" + name;
                }
                if (Alias != null)
                {
                    return Alias + "->" + Name;
                }
                else
                {
                    return Name;
                }
            }
        }

        private FieldFlags _flags;
        static FieldFlags setFlag(FieldFlags oldFlag, FieldFlags newFlag, bool set)
        {
            if (set)
                oldFlag |= newFlag;
            else
                oldFlag &= ~newFlag;
            return oldFlag;
        }
        public bool IsFileWidePublic
        {
            get { return _flags.HasFlag(FieldFlags.IsFileWidePublic); }
            set { _flags = setFlag(_flags, FieldFlags.IsFileWidePublic, value); }
        }
        public bool IsParameter
        {
            get { return _flags.HasFlag(FieldFlags.IsParameter); }
            set { _flags = setFlag(_flags, FieldFlags.IsParameter, value); }
        }
        public bool IsWritten
        {
            get { return _flags.HasFlag(FieldFlags.IsWritten); }
            set { _flags = setFlag(_flags, FieldFlags.IsWritten, value); }
        }
        public bool IsCreated
        {
            get { return _flags.HasFlag(FieldFlags.IsCreated); }
            set { _flags = setFlag(_flags, FieldFlags.IsCreated, value); }
        }
        public bool IsPublic
        {
            get { return _flags.HasFlag(FieldFlags.IsPublic); }
            set { _flags = setFlag(_flags, FieldFlags.IsPublic, value); }
        }
        public XSharpParserRuleContext Context { get; private set; }
        internal MemVarFieldInfo(string name, string alias, XSharpParserRuleContext context, bool filewidepublic = false)
        {
            if (name.StartsWith("@@"))
                name = name.Substring(2);
            Name = name;
            Alias = alias;
            Context = context;
            if (!string.IsNullOrEmpty(alias))
            {
                if (alias.StartsWith("@@"))
                    alias = alias.Substring(2);
                switch (alias.ToUpper())
                {
                    case "&":
                        _fieldType = MemvarType.MacroMemvar;
                        Alias = XSharpSpecialNames.MemVarPrefix;
                        break;
                    case "M":
                    case "MEMV":
                    case "MEMVA":
                    case "MEMVAR":
                        _fieldType = MemvarType.Memvar;
                        Alias = XSharpSpecialNames.MemVarPrefix;
                        break;
                    case "FIELD":
                    case "_FIELD":
                        Alias = XSharpSpecialNames.FieldPrefix;
                        _fieldType = MemvarType.Field;
                        break;
                    default:
                        switch (alias)
                        {
                            case XSharpSpecialNames.ClipperParamPrefix:
                                _fieldType = MemvarType.ClipperParameter;
                                IsParameter = true;
                                break;
                            case XSharpSpecialNames.MemVarPrefix:
                                _fieldType = MemvarType.Memvar;
                                break;
                            case XSharpSpecialNames.LocalPrefix:
                                _fieldType = MemvarType.Local;
                                Alias = null;
                                break;
                            case XSharpSpecialNames.FieldPrefix:
                            default:
                                _fieldType = MemvarType.Field;
                                break;
                        }
                        break;
                }
            }
            else
            {
                Alias = XSharpSpecialNames.FieldPrefix;
                _fieldType = MemvarType.Field;
            }
            IsFileWidePublic = filewidepublic;
        }
    }
#endif
    internal static class RuleExtensions
    {
        internal static XSharpParserRuleContext Context([NotNull] this XSharpParser.IEntityContext entity) => (XSharpParserRuleContext)entity;
        internal static bool isScript([NotNull] this XSharpParser.IEntityContext entity) => entity is XSharpParser.ScriptContext;
#if !VSPARSER
        internal static bool IsStatic(this InternalSyntax.ClassDeclarationSyntax classdecl)
        {
            return classdecl.Modifiers.Any((int)SyntaxKind.StaticKeyword);
        }
        internal static bool IsOfType(this InternalSyntax.TypeSyntax type, string typeName)
        {
            return type is InternalSyntax.QualifiedNameSyntax ns &&
                ns.Right is InternalSyntax.IdentifierNameSyntax ins &&
                ins.Identifier.Text == typeName;
        }
        internal static bool IsUsualType(this InternalSyntax.TypeSyntax type)
        {
            return type.IsOfType(OurTypeNames.UsualType);
        }
        internal static bool IsArrayType(this InternalSyntax.TypeSyntax type)
        {
            return type.IsOfType(OurTypeNames.ArrayType);
        }
        internal static bool IsPszType(this InternalSyntax.TypeSyntax type)
        {
            return type.IsOfType(OurTypeNames.PszType);
        }
        internal static bool IsSymbolType(this InternalSyntax.TypeSyntax type)
        {
            return type.IsOfType(OurTypeNames.SymbolType);
        }
        internal static bool IsPtrType(this InternalSyntax.TypeSyntax type)
        {
            return type.IsOfType("IntPtr");
        }
        internal static bool IsStatic(this InternalSyntax.ConstructorDeclarationSyntax ctordecl)
        {
            return ctordecl.Modifiers.Any((int)SyntaxKind.StaticKeyword);
        }

        internal static void Put<T>([NotNull] this IXParseTree t, T node)
            where T : InternalSyntax.CSharpSyntaxNode
        {
            if (node != null)
            {
                node.XNode = t;
                t.CsNode = node;
            }
        }

        internal static T Get<T>([NotNull] this IXParseTree t)
            where T : InternalSyntax.CSharpSyntaxNode
        {
            if (t == null || t.CsNode == null)
                return null;

            return (T)t.CsNode;
        }

        internal static void PutList<T>([NotNull] this IXParseTree t, CoreInternalSyntax.SyntaxList<T> node)
            where T : InternalSyntax.CSharpSyntaxNode
        {
            //node.XNode = t;
            t.CsNode = node;
        }

        internal static CoreInternalSyntax.SyntaxList<T> GetList<T>([NotNull] this IXParseTree t)
            where T : InternalSyntax.CSharpSyntaxNode
        {
            if (t.CsNode == null)
                return default(CoreInternalSyntax.SyntaxList<T>);

            return (CoreInternalSyntax.SyntaxList<T>)t.CsNode;
        }

        internal static TNode WithAdditionalDiagnostics<TNode>([NotNull] this TNode node, params DiagnosticInfo[] diagnostics)
            where TNode : InternalSyntax.CSharpSyntaxNode
        {
            DiagnosticInfo[] existingDiags = node.GetDiagnostics();
            int existingLength = existingDiags.Length;
            if (existingLength == 0)
            {
                return node.WithDiagnosticsGreen(diagnostics);
            }
            else
            {
                DiagnosticInfo[] result = new DiagnosticInfo[existingDiags.Length + diagnostics.Length];
                existingDiags.CopyTo(result, 0);
                diagnostics.CopyTo(result, existingLength);
                return node.WithDiagnosticsGreen(result);
            }
        }
#endif
        internal static bool IsRealCodeBlock([NotNull] this IXParseTree context)
        {

            if (context is XSharpParser.ArrayElementContext aelc)
                return aelc.Expr.IsRealCodeBlock();
            if (context is XSharpParser.PrimaryExpressionContext pec)
                return pec.Expr.IsRealCodeBlock();
            if (context is XSharpParser.CodeblockExpressionContext cec)
                return cec.CbExpr.IsRealCodeBlock();
            if (context is XSharpParser.AliasedExpressionContext aexc)
            {
                if (aexc.XSharpRuntime)
                {
                    return false;
                }
            }
            if (context is XSharpParser.CodeblockCodeContext)
                return ((IXParseTree)context.Parent).IsRealCodeBlock();
            if (context is XSharpParser.CodeblockContext cbc)
            {
                if (cbc.lambda != null)
                    return false;
                // when no => operator and no explicit parameters
                // then this is a true codeblock
                return cbc.LambdaParamList == null || cbc.LambdaParamList.ImplicitParams != null;
            }
            return false;
        }
        internal static string CodeBlockSource([NotNull] this IXParseTree context)
        {

            if (context is XSharpParser.ArrayElementContext aelc)
                return aelc.Expr.CodeBlockSource();
            if (context is XSharpParser.PrimaryExpressionContext pec)
                return pec.Expr.CodeBlockSource();
            if (context is XSharpParser.CodeblockExpressionContext cec)
                return cec.CbExpr.CodeBlockSource();
            if (context is XSharpParser.AliasedExpressionContext aexc)
            {
                if (aexc.XSharpRuntime)
                {
                    return null;
                }
            }
            if (context is XSharpParser.CodeblockCodeContext)
                return ((IXParseTree)context.Parent).CodeBlockSource();
            if (context is XSharpParser.CodeblockContext cbc)
            {
                if (cbc.lambda != null)
                    return null;
                // when no => operator and no explicit parameters
                // then this is a true codeblock
                if (context is XSharpParserRuleContext rule)
                    return rule.SourceText;
            }
            return null;
        }
        internal static bool isInInterface([NotNull] this RuleContext context)
        {
            var parent = context.Parent;
            if (parent == null)
                return false;
            if (parent is XSharpParser.ClassmemberContext)
                return parent.Parent is XSharpParser.Interface_Context;
            else
                return parent.isInInterface();
        }

        internal static XSharpParser.CodeblockContext GetParentCodeBlock([NotNull] this RuleContext context)
        {
            var parent = context.Parent;
            if (parent == null)
                return null;
            if (parent is XSharpParser.CodeblockContext cbc)
            {
                if (cbc.lambda != null || cbc.Or != null || cbc.P1 != null)
                    return cbc;
            }
            return parent.GetParentCodeBlock();

        }
        internal static bool IsInLambdaOrCodeBlock([NotNull] this RuleContext context)
        {
            return context.GetParentCodeBlock() != null;
        }
        internal static bool isInClass([NotNull] this RuleContext context)
        {
            var parent = context.Parent;
            if (parent == null)
                return false;
            if (parent is XSharpParser.ClassmemberContext)
            {
                if (parent.Parent is XSharpParser.Class_Context)
                    return true;
                if (parent.Parent is XSharpParser.FoxclassContext)
                    return true;
                return false;
            }
            else if (parent is XSharpParser.XppclassMemberContext)
            {
                return parent.Parent is XSharpParser.XppclassContext;
            }
            return parent.isInClass();
        }
        internal static bool isInStructure([NotNull] this RuleContext context)
        {
            var parent = context.Parent;
            if (parent == null)
                return false;
            if (parent is XSharpParser.ClassmemberContext)
                return parent.Parent is XSharpParser.Structure_Context;
            else
                return parent.isInStructure();
        }
    }
}
