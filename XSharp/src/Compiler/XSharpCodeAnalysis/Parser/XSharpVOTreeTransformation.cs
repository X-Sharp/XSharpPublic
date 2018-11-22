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
// Uncomment this define to dump the AST to the debug console.
//#define DUMP_TREE

using System;
using System.Linq;
using System.Collections.Generic;
using Roslyn.Utilities; 
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    using Microsoft.CodeAnalysis.Syntax.InternalSyntax;

    internal class XSharpVOTreeTransformation : XSharpTreeTransformation
    {
        // XBase Type Names
        #region Fields
        private readonly TypeSyntax _usualType;
        private readonly TypeSyntax _floatType;
        private readonly TypeSyntax _arrayType;
        private readonly TypeSyntax _dateType;
        private readonly TypeSyntax _symbolType;
        private readonly TypeSyntax _pszType;
        private readonly TypeSyntax _codeblockType;
        private readonly TypeSyntax _stringType;
        private readonly TypeSyntax _intType;
        private readonly string _errorType;
        private readonly string _classLibraryType;
        private readonly string _wrappedExceptionType;
        private readonly string _compilerVersionType;
        private readonly string _runtimeStateType;
        private readonly string _defaultParameterType;
        private readonly string _actualType;
        private readonly string _clipperCallingConvention;
        private readonly string _winBoolType;

        private ArrayTypeSyntax arrayOfUsual = null;
        private ArrayTypeSyntax arrayOfString = null;
        private bool voStructHasDim;

        private Dictionary<string, FieldDeclarationSyntax> _literalSymbols;
        private Dictionary<string, Tuple<string, FieldDeclarationSyntax>> _literalPSZs;
        #endregion

        #region Static Fields
        private static SyntaxList<AttributeListSyntax> _voClassAttribs = null;
        private ParameterListSyntax _clipperParams = null;
        private AttributeListSyntax _actualArgs = null;

        #endregion


        #region Constructors and destructors
        public XSharpVOTreeTransformation(XSharpParser parser, CSharpParseOptions options, SyntaxListPool pool,
            ContextAwareSyntax syntaxFactory, string fileName) :
            base(parser, options, pool, syntaxFactory, fileName)
        {
            if (options.XSharpRuntime)
            {
                _usualType = GenerateQualifiedName(XSharpQualifiedTypeNames.Usual);
                _floatType = GenerateQualifiedName(XSharpQualifiedTypeNames.Float);
                _dateType = GenerateQualifiedName(XSharpQualifiedTypeNames.Date);
                _arrayType = GenerateQualifiedName(XSharpQualifiedTypeNames.Array);
                _symbolType = GenerateQualifiedName(XSharpQualifiedTypeNames.Symbol);
                _pszType = GenerateQualifiedName(XSharpQualifiedTypeNames.Psz);
                _codeblockType = GenerateQualifiedName(XSharpQualifiedTypeNames.Codeblock);
                _errorType = XSharpQualifiedTypeNames.Error;
                _wrappedExceptionType = XSharpQualifiedTypeNames.WrappedException;
                _classLibraryType = XSharpQualifiedTypeNames.ClassLibrary;
                _compilerVersionType = XSharpQualifiedTypeNames.CompilerVersion;
                _runtimeStateType = XSharpQualifiedTypeNames.RuntimeState;
                _defaultParameterType = XSharpQualifiedTypeNames.DefaultParameter;
                _actualType = XSharpQualifiedTypeNames.ActualType;
                _clipperCallingConvention = XSharpQualifiedTypeNames.ClipperCallingConvention;
                _winBoolType = XSharpQualifiedTypeNames.WinBool;
            }
            else
            {
                _usualType = GenerateQualifiedName(VulcanQualifiedTypeNames.Usual);
                _floatType = GenerateQualifiedName(VulcanQualifiedTypeNames.Float);
                _dateType = GenerateQualifiedName(VulcanQualifiedTypeNames.Date);
                _arrayType = GenerateQualifiedName(VulcanQualifiedTypeNames.Array);
                _symbolType = GenerateQualifiedName(VulcanQualifiedTypeNames.Symbol);
                _pszType = GenerateQualifiedName(VulcanQualifiedTypeNames.Psz);
                _codeblockType = GenerateQualifiedName(VulcanQualifiedTypeNames.Codeblock);
                _errorType = VulcanQualifiedTypeNames.Error;
                _wrappedExceptionType = VulcanQualifiedTypeNames.WrappedException;
                _classLibraryType = VulcanQualifiedTypeNames.ClassLibrary;
                _compilerVersionType = VulcanQualifiedTypeNames.CompilerVersion;
                _runtimeStateType = VulcanQualifiedTypeNames.RuntimeState;
                _defaultParameterType = VulcanQualifiedTypeNames.DefaultParameter;
                _actualType = VulcanQualifiedTypeNames.ActualType;
                _clipperCallingConvention = VulcanQualifiedTypeNames.ClipperCallingConvention;
                _winBoolType = VulcanQualifiedTypeNames.WinBool;
            }
            _stringType = _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.StringKeyword));
            _intType = _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.IntKeyword));
            _literalSymbols = new Dictionary<string, FieldDeclarationSyntax>();
            _literalPSZs = new Dictionary<string, Tuple<string, FieldDeclarationSyntax>>();
            // calculate the global class name;
            GlobalClassName = GetGlobalClassName(_options.TargetDLL);

            // calculate the default vo class attributes
            GetVOClassAttributes();
        }

        internal Dictionary<string, FieldDeclarationSyntax> LiteralSymbols => _literalSymbols;
        internal Dictionary<string, Tuple<string, FieldDeclarationSyntax>> LiteralPSZs => _literalPSZs;
        internal static SyntaxList<AttributeListSyntax> VOClassAttribs { get { return _voClassAttribs; } }

        public override string GetGlobalClassName(XSharpTargetDLL targetDLL)
        {
            // our runtime DLLs have a fixed Globals Class name
            if (targetDLL != XSharpTargetDLL.Other)
            {
                    return base.GetGlobalClassName(targetDLL);
            }
            string name = _options.CommandLineArguments?.CompilationOptions.ModuleName;
            string firstSource = _options.CommandLineArguments?.SourceFiles.FirstOrDefault().Path;
            if (String.IsNullOrEmpty(name))
            {
                name = firstSource;
            }

            if (!String.IsNullOrEmpty(name))
            {
                string filename = PathUtilities.GetFileName(name);
                filename = PathUtilities.RemoveExtension(filename);
                filename = filename.Replace('.', '_');
                if (_options.CommandLineArguments?.CompilationOptions.OutputKind.IsApplication() == true)
                    name = filename + XSharpSpecialNames.VOExeFunctionsClass;
                else
                    name = filename + XSharpSpecialNames.VODllFunctionsClass;
            }
            else
            {
                name = XSharpSpecialNames.FunctionsClass;
            }
            return name;
        }
        internal SyntaxList<AttributeListSyntax> GetVOClassAttributes()
        {
            if (_voClassAttribs == null)
            {
                lock (gate)
                {
                    if (_voClassAttribs == null)
                    {
                        var attlist = _pool.Allocate<AttributeListSyntax>();
                        var attargs = ArrayBuilder<AttributeArgumentSyntax>.GetInstance();
                        attargs.Add(_syntaxFactory.AttributeArgument(null, null, GenerateQualifiedName(SystemQualifiedNames.LayoutSequential)));
                        attargs.Add(_syntaxFactory.AttributeArgument(GenerateNameEquals("Charset"), null,
                                        MakeSimpleMemberAccess(GenerateQualifiedName(SystemQualifiedNames.CharSet),
                                             _syntaxFactory.IdentifierName(SyntaxFactory.Identifier("Auto")))));
                        var attrib = _syntaxFactory.Attribute(
                                        name: GenerateQualifiedName(SystemQualifiedNames.StructLayout),
                                        argumentList: MakeAttributeArgumentList(MakeSeparatedList(attargs.ToArrayAndFree()))
                                        );
                        attlist.Add(MakeAttributeList(null, MakeSeparatedList(attrib)));
                        _voClassAttribs = attlist.ToList();
                        _pool.Free(attlist);
                    }
                }
            }
            return _voClassAttribs;
        }


        private void InitializeArrayTypes()
        {
            if (arrayOfUsual == null || arrayOfString == null)
            {
                var emptysizes = _pool.AllocateSeparated<ExpressionSyntax>();
                emptysizes.Add(_syntaxFactory.OmittedArraySizeExpression(SyntaxFactory.MakeToken(SyntaxKind.OmittedArraySizeExpressionToken)));
                var emptyrank = _syntaxFactory.ArrayRankSpecifier(
                                SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                                emptysizes,
                                SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken));
                _pool.Free(emptysizes);
                arrayOfUsual = _syntaxFactory.ArrayType(_usualType, emptyrank);
                arrayOfString = _syntaxFactory.ArrayType(_stringType, emptyrank);
            }
        }
        private static XSharpVOTreeTransformation getTransform(CSharpParseOptions options)
        {
            return new XSharpVOTreeTransformation(null, options, new SyntaxListPool(), new ContextAwareSyntax(new SyntaxFactoryContext()), "");
        }

        #endregion

        #region SyntaxTree
        private SyntaxTree GenerateDefaultSyntaxTree(List<Tuple<int, String>> initprocs, bool isApp, bool hasPCall)
        {

            // Create Global Functions class with the Members to call the Init procedures
            // Vulcan only does this for DLLs. We do it for EXE too to make things more consistent
            // Methods $Init1() and $Exit() are always created.
            var members = CreateInitMembers(initprocs, isApp, hasPCall);
            var modulemembers = new List<MemberDeclarationSyntax>();
            if (isApp)
            {
                modulemembers.Add(CreateAppInit());
                modulemembers.Add(CreateAppExit());
            }
            else
            {
                modulemembers.Add(CreateRunInitProcs());
            }
            GlobalEntities.Members.Add(GenerateGlobalClass(GlobalClassName, false, true, members.ToArray()));
            // Add global attributes
            GlobalEntities.Members.Add(GenerateGlobalClass(XSharpSpecialNames.ModuleName, true, false, modulemembers.ToArray()));
            var arguments = _pool.AllocateSeparated<AttributeArgumentSyntax>();
            var attributes = _pool.AllocateSeparated<AttributeSyntax>();
            // VulcanClassLibrary
            arguments.Add(_syntaxFactory.AttributeArgument(null, null, GenerateLiteral(GlobalClassName)));
            arguments.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
            arguments.Add(_syntaxFactory.AttributeArgument(null, null, GenerateLiteral(_options.DefaultNamespace)));
            attributes.Add(_syntaxFactory.Attribute(
                name: GenerateQualifiedName(_classLibraryType), argumentList: MakeAttributeArgumentList(arguments)));
            arguments.Clear();
            // VulcanVersion
            arguments.Add(_syntaxFactory.AttributeArgument(null, null, GenerateLiteral("X# " + global::XSharp.Constants.Version + " - dialect:" + _options.Dialect.ToString())));
            attributes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
            attributes.Add(_syntaxFactory.Attribute(
                name: GenerateQualifiedName(_compilerVersionType),argumentList: MakeAttributeArgumentList(arguments)));

            var target = _syntaxFactory.AttributeTargetSpecifier(SyntaxFactory.Identifier("assembly"), SyntaxFactory.MakeToken(SyntaxKind.ColonToken));
            var attrlist = MakeAttributeList(
                target,
                attributes);
            GlobalEntities.Attributes.Add(attrlist);
            _pool.Free(arguments);
            _pool.Free(attributes);
            var eof = SyntaxFactory.Token(SyntaxKind.EndOfFileToken);
            return CSharpSyntaxTree.Create(
                (Syntax.CompilationUnitSyntax)_syntaxFactory.CompilationUnit(
                    GlobalEntities.Externs,
                    GlobalEntities.Usings,
                    GlobalEntities.Attributes,
                    GlobalEntities.Members, eof).
                    CreateRed());
        }
        public static SyntaxTree DefaultVOSyntaxTree(IEnumerable<SyntaxTree> trees, bool isApp)
        {
            // Trees is NEVER empty !
            CSharpParseOptions options = (CSharpParseOptions)trees.First().Options;
            // Collect Init procedures in all trees
            var initprocs = new List<Tuple<int, string>>();
            bool hasPCall = false;
            foreach (var tree in trees)
            {
                var root = tree.GetRoot();
                if (root != null)
                {
                    if (root.Green is CompilationUnitSyntax unit)
                    {
                        if (unit.InitProcedures != null)
                        {
                            initprocs.AddRange(unit.InitProcedures);
                        }
                        hasPCall = hasPCall || unit.HasPCall;
                    }

                }
            }

            var t = getTransform(options);
            return t.GenerateDefaultSyntaxTree(initprocs, isApp, hasPCall);
        }
        
        public static string VOGlobalClassName(CSharpParseOptions options)
        {
            var t = getTransform(options);
            return t.GlobalClassName;
        }
        #endregion

        #region Special app methods
        protected MethodDeclarationSyntax CreateInitFunction(IList<String> procnames, string functionName, bool isApp)
        {
            // create body for new Init procedure
            var stmts = _pool.Allocate<StatementSyntax>();
            foreach (var name in procnames)
            {
                var invoke = GenerateMethodCall(name, true);
                stmts.Add(GenerateExpressionStatement(invoke,true));
            }
            var mods = TokenList(isApp ? SyntaxKind.InternalKeyword : SyntaxKind.PublicKeyword, SyntaxKind.StaticKeyword);
            var pars = EmptyParameterList();
            var m = SyntaxFactory.MethodDeclaration(MakeCompilerGeneratedAttribute(), mods,
                _voidType, /*explicitif*/null,
                SyntaxFactory.Identifier(functionName), /*typeparams*/null, pars,/* constraints*/null, MakeBlock(stmts),/*exprbody*/null,
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));

            _pool.Free(stmts);
            m.XGenerated = true;
            return m;
        }

        private List<MemberDeclarationSyntax> CreateInitMembers(List<Tuple<int, String>> initprocs, bool isApp, bool hasPCall)
        {
            var members = new List<MemberDeclarationSyntax>();
            var init1 = new List<string>();
            var init2 = new List<string>();
            var init3 = new List<string>();
            var exit = new List<string>();
            foreach (var element in initprocs)
            {
                switch (element.Item1)
                {
                    case 1:
                        init1.Add(element.Item2);
                        break;
                    case 2:
                        init2.Add(element.Item2);
                        break;
                    case 3:
                        init3.Add(element.Item2);
                        break;
                    case -1:        // Exit procedures
                        exit.Add(element.Item2);
                        break;
                }
            }

            // Put Everything in separate methods $Init1 .. $Init3
            // Always generate $Init1
            members.Add(CreateInitFunction(init1, XSharpSpecialNames.InitProc1, isApp));
            if (init2.Count > 0)
            {
                members.Add(CreateInitFunction(init2, XSharpSpecialNames.InitProc2, isApp));
            }
            if (init3.Count > 0)
            {
                members.Add(CreateInitFunction(init3, XSharpSpecialNames.InitProc3, isApp));
            }
            members.Add(CreateInitFunction(exit, XSharpSpecialNames.ExitProc, isApp));
            if (hasPCall)
            {
                members.Add(CreatePCallFunction());
            }
            return members;
        }


        private MethodDeclarationSyntax CreateAppExit()
        {
            // Creates an empty $AppExit method.
            // The contents will be created in the LocalRewriter
            // This will contain the code to clear globals
            var stmts = _pool.Allocate<StatementSyntax>();
            var body = MakeBlock(stmts);
            var appId = SyntaxFactory.Identifier(XSharpSpecialNames.AppExit);
            var modifiers = TokenList(SyntaxKind.InternalKeyword, SyntaxKind.StaticKeyword);

            var appExit = _syntaxFactory.MethodDeclaration(
                MakeCompilerGeneratedAttribute(), modifiers,
                _voidType, null, appId, null, EmptyParameterList(),
                null, body, null, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            _pool.Free(stmts);
            appExit.XGenerated = true;
            return appExit;
        }

        private MethodDeclarationSyntax CreateRunInitProcs()
        {
            var stmts = _pool.Allocate<StatementSyntax>();
            var body = MakeBlock(stmts);
            var appId = SyntaxFactory.Identifier(XSharpFunctionNames.RunInitProcs);
            var modifiers = TokenList(SyntaxKind.PublicKeyword, SyntaxKind.StaticKeyword);
            var initProcs = _syntaxFactory.MethodDeclaration(
                MakeCompilerGeneratedAttribute(), modifiers,
                _voidType, null, appId, null, EmptyParameterList(),
                null, body, null, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            _pool.Free(stmts);
            initProcs.XGenerated = true;
            return initProcs;

        }
        private MethodDeclarationSyntax CreateAppInit()
        {
            // Creates a skeleton $AppInit method.
            // The rest of the contents will be created in the LocalRewriter
            var stmts = _pool.Allocate<StatementSyntax>();
            var appId = SyntaxFactory.Identifier(XSharpSpecialNames.AppInit);
            // try
            // {
            //      State.AppModule = typeof(Functions).Module          // stmt 1
            //      State.CompilerOptionVO11 = <value of VO11>          // optional stmt 2, generated in the LocalRewriter
            //      State.CompilerOptionOvf  = <value of OVF>           // optional stmt 3, generated in the LocalRewriter
            //      State.CompilerOptionFOvf = <value of OVF>           // optional stmt 4, generated in the LocalRewriter
            //      <Call Init procedures>                              // optional block is generated in the LocalRewriter
            // }
            // catch (Exception exception)
            // {
            //    throw new Exception("Error when executing code in INIT procedure", exception);
            // }

            var lhs = GenerateQualifiedName(_runtimeStateType + ".AppModule");

            ExpressionSyntax rhs = MakeTypeOf(GenerateQualifiedName(GlobalClassName));

            rhs = MakeSimpleMemberAccess(rhs, GenerateSimpleName("Module"));
            stmts.Add(GenerateExpressionStatement(MakeSimpleAssignment(lhs, rhs),true));
            // rest of the statements is generated in the LocalRewriter with a check for the existence of the fields in VulcanRT.
            // in Vulcan.Runtime.State
            var body = MakeBlock(stmts);
            stmts.Clear();

            // Create Exception
            var arg1 = MakeArgument(GenerateLiteral("Error when executing code in Vulcan INIT procedure(s)"));
            var arg2 = MakeArgument(GenerateSimpleName(XSharpSpecialNames.ExVarName));
            var excType = GenerateQualifiedName(SystemQualifiedNames.Exception);
            var Exception = CreateObject(excType, MakeArgumentList(arg1, arg2));
            var throwstmt = _syntaxFactory.ThrowStatement(
                SyntaxFactory.MakeToken(SyntaxKind.ThrowKeyword),
                Exception, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            stmts.Add(throwstmt);
            // Catch Clause
            var catchDecl = _syntaxFactory.CatchDeclaration(
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                excType, SyntaxFactory.Identifier(XSharpSpecialNames.ExVarName),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
            var catchClause = _syntaxFactory.CatchClause(
                SyntaxFactory.MakeToken(SyntaxKind.CatchKeyword), catchDecl, null, MakeBlock(stmts));

            var tryStmt = _syntaxFactory.TryStatement(
                    SyntaxFactory.MakeToken(SyntaxKind.TryKeyword),
                    body, catchClause, null);
            tryStmt.XGenerated = true;
            stmts.Clear();
            stmts.Add(tryStmt);
            body = MakeBlock(stmts);
            body.XGenerated = true;
            // Body is ready now. Now create the method as a private method
            var modifiers = TokenList(SyntaxKind.InternalKeyword, SyntaxKind.StaticKeyword);

            var appInit = _syntaxFactory.MethodDeclaration(
                MakeCompilerGeneratedAttribute(), modifiers,
                _voidType, null, appId, null, EmptyParameterList(),
                null, body, null, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            _pool.Free(stmts);
            appInit.XGenerated = true;
            return appInit;

        }

        private LocalDeclarationStatementSyntax GenerateReturnVar(TypeSyntax type, ExpressionSyntax expr= null)
        {
            var localdecl = GenerateLocalDecl(XSharpSpecialNames.ReturnName, type,expr);
            localdecl.Declaration.XGenerated = true;
            localdecl.XGenerated = true;
            return localdecl;

        }
        protected override BlockSyntax CreateEntryPoint(BlockSyntax originalbody, [NotNull] XP.FunctionContext context)
        {
            // This method changes the body of the entry point for the VO Syntax
            // it calls $AppInit()
            // when there is a return statement in the main statement list then the 
            // value of this return statement is stored in a local Xs$Return
            // At the end of the main body variables are cleared
            // And $AppExit() is called to clear the globals in referenced Vulcan Libs
            // and GC routines are called
            // and when needed it returns with X$Return
            // Not that the code only checks for locals in the main body statement list
            // and not for locals hidden inside blocks inside the main body
            // When the main body has a PSZ Try - Finally then our cleanup code and init code will be inserted before and after 
            // the try finally
            bool needsReturnValue = false;
            var lastStmt = (StatementSyntax) originalbody ;
            if (originalbody.Statements.Count > 0)
                lastStmt = originalbody.Statements.Last;
            var lastXnode = lastStmt.XNode;
            var newbody = new List<StatementSyntax>();     // contains the copied and adjusted statements
            var endbody = new List<StatementSyntax>();     // contains the cleanup code
            TryStatementSyntax trystmt = null;
            StatementSyntax pszdecl = null;
            var body = originalbody;                    //body where we check for LOCAL and RETURN statements
            if (context.Data.UsesPSZ && originalbody.Statements.Count == 2 && originalbody.Statements[1] is TryStatementSyntax)
            {
                // local declaration followed by try .. finally
                pszdecl = originalbody.Statements[0];
                trystmt = originalbody.Statements[1] as TryStatementSyntax;
                body = trystmt.Block;
            }
            else
            {
                var call = GenerateMethodCall(XSharpSpecialNames.ModuleName + "." + XSharpSpecialNames.AppInit, true);
                newbody.Add(GenerateExpressionStatement(call,true));
            }
            if (context.Type != null && context.Type.GetText().ToLower() != "void")
            {
                needsReturnValue = true;
            }
            foreach (var stmt in body.Statements)
            {
                if (stmt is ReturnStatementSyntax && stmt == body.Statements[body.Statements.Count - 1])
                {
                    var retStmt = stmt as ReturnStatementSyntax;
                    var retExpr = retStmt.Expression;
                    if (retExpr != null && ! retExpr.XNode.IsLiteral())
                    {
                        needsReturnValue = true;
                        var assignStmt = GenerateExpressionStatement(MakeSimpleAssignment(GenerateSimpleName(XSharpSpecialNames.ReturnName), retExpr), true);
                        assignStmt.XNode = lastXnode;
                        newbody.Add(assignStmt);
                    }
                }
                else
                {
                    newbody.Add(stmt);
                    if (stmt is LocalDeclarationStatementSyntax)
                    {
                        var locdecl = stmt as LocalDeclarationStatementSyntax;
                        var localvar = locdecl.XNode as XP.LocalvarContext;
                        bool useNull = true;
                        bool mustclear = true;
                        if (localvar != null)
                        {
                            var name = localvar.Id.GetText();
                            TypeSyntax type;
                            if (localvar.DataType == null)
                            {
                                type = _usualType;
                                mustclear = true;
                                useNull = false;
                            }
                            else
                            {
                                type = locdecl.Declaration.Type;

                                if (localvar.DataType is XP.ArrayDatatypeContext ||
                                    localvar.DataType is XP.NullableDatatypeContext ||
                                    localvar.DataType is XP.ArrayOfTypeContext ||
                                    localvar.DataType is XP.PtrDatatypeContext)
                                {
                                    useNull = true;
                                }
                                else
                                {
                                    var sdc = localvar.DataType as XP.SimpleDatatypeContext;
                                    var tn = sdc.TypeName;
                                    if (tn.XType != null)
                                    {
                                        useNull = mustclear = tn.XType.Token.MustBeCleared();
                                        if (tn.XType.Token.Type == XP.USUAL ||
                                            tn.XType.Token.Type == XP.PSZ)
                                        {
                                            useNull = false;
                                        }
                                    }
                                    else if (tn.NativeType != null)
                                    {
                                        useNull = mustclear = tn.NativeType.Token.MustBeCleared();
                                    }
                                    else if (tn.Name != null)
                                    {
                                        useNull = false;
                                        mustclear = true;
                                    }
                                }
                            }
                            if (mustclear)
                            {
                                ExpressionSyntax clearExpr = null;

                                if (useNull)
                                    clearExpr = GenerateLiteralNull();
                                else
                                    clearExpr = MakeDefault(type);
                                var expr = MakeSimpleAssignment(GenerateSimpleName(name), clearExpr);
                                var estmt = GenerateExpressionStatement(expr, true);
                                estmt.XNode = lastXnode;
                                endbody.Add(estmt);
                            }
                        }
                    }
                }
            }
            newbody.AddRange(endbody);
            if (trystmt != null)
            {
                trystmt = trystmt.Update(trystmt.TryKeyword, MakeBlock(newbody), trystmt.Catches, trystmt.Finally);
                trystmt.XGenerated = true;
                newbody.Clear();
                newbody.Add(pszdecl);
                var call = GenerateMethodCall(XSharpSpecialNames.ModuleName + "." + XSharpSpecialNames.AppInit, true);
                newbody.Add(GenerateExpressionStatement(call, true));
                newbody.Add(trystmt);
            }
            // the next statements should all be linked to the last line of code in the start function
            // so we do not skip back to the start line in the debugger
            StatementSyntax newStmt = GenerateExpressionStatement(GenerateMethodCall(XSharpSpecialNames.ModuleName + "." + XSharpSpecialNames.AppExit, true), true);
            newStmt.XNode = lastXnode;
            newbody.Add(newStmt);
            newStmt = GenerateExpressionStatement(GenerateMethodCall(SystemQualifiedNames.GcCollect, true), true);
            newStmt.XNode = lastXnode;
            newbody.Add(newStmt);

            newStmt = GenerateExpressionStatement(GenerateMethodCall(SystemQualifiedNames.GcWait, true), true);
            newStmt.XNode = lastXnode;
            newbody.Add(newStmt);
            if (needsReturnValue)
            {
                newbody.Insert(0,GenerateReturnVar(context.Type.Get<TypeSyntax>()));
                newStmt = GenerateReturn(GenerateSimpleName(XSharpSpecialNames.ReturnName));
            }
            else
            {
                newStmt = GenerateReturn(null);
            }
            newStmt.XNode = lastXnode;
            newbody.Add(newStmt);

            return MakeBlock(newbody);
        }
        #endregion

        #region MemVar and Fields
        internal ExpressionSyntax GenerateMemVarPut(string memvar, ExpressionSyntax right)
        {
            var arg1 = MakeArgument(GenerateLiteral(memvar));
            var arg2 = MakeArgument(right);
            var args = MakeArgumentList(arg1, arg2);
            var expr = GenerateMethodCall(XSharpQualifiedFunctionNames.MemVarPut, args, true);
            expr = (ExpressionSyntax)NotInDialect(expr, "MEMVAR");
            //todo: Implement MemVarPut
            return expr;
        }

        internal ExpressionSyntax GenerateMemVarGet(string memvar)
        {
            var arg1 = MakeArgument(GenerateLiteral(memvar));
            var args = MakeArgumentList(arg1);
            var expr = GenerateMethodCall(XSharpQualifiedFunctionNames.MemVarGet, args, true);
            expr = (ExpressionSyntax)NotInDialect(expr, "MEMVAR");
            //todo: Implement MemVarGet
            return expr;
        }

        internal ExpressionSyntax GenerateFieldSet(string alias, string field, ExpressionSyntax right)
        {
            string method = "";
            ArgumentListSyntax args;
            var argField = MakeArgument(GenerateLiteral(field));
            var argValue = MakeArgument(right);
            if (!String.IsNullOrEmpty(alias))
            {
                method = _options.XSharpRuntime ? XSharpQualifiedFunctionNames.FieldSetWa : VulcanQualifiedFunctionNames.FieldSetWa; ;
                var argWA = MakeArgument(GenerateLiteral(alias));
                args = MakeArgumentList(argWA, argField, argValue);
            }
            else
            {
                method = _options.XSharpRuntime ? XSharpQualifiedFunctionNames.FieldSet : VulcanQualifiedFunctionNames.FieldSet;
                args = MakeArgumentList(argField, argValue);
            }
            var expr = GenerateMethodCall(method, args, true);
            return expr;
        }

        internal ExpressionSyntax GenerateFieldGet(string alias, string field)
        {
            string method;
            ArgumentListSyntax args;
            var argField = MakeArgument(GenerateLiteral(field));
            if (string.IsNullOrEmpty(alias))
            {
                method = _options.XSharpRuntime ? XSharpQualifiedFunctionNames.FieldGet : VulcanQualifiedFunctionNames.FieldGet;
                args = MakeArgumentList(argField);
            }
            else
            {
                method = _options.XSharpRuntime ? XSharpQualifiedFunctionNames.FieldGetWa : VulcanQualifiedFunctionNames.FieldGetWa;
                var argWA = MakeArgument(GenerateLiteral(alias));
                args = MakeArgumentList(argWA, argField);
            }

            var expr = GenerateMethodCall(method, args, true);
            return expr;
        }

        public override void ExitNameExpression([NotNull] XP.NameExpressionContext context)
        {
            // Check to see if the name is a field or Memvar, registered with the FIELD or MemVar statement
            string Name = context.Name.GetText();
            ExpressionSyntax expr = context.Name.Get<NameSyntax>();
            MemVarFieldInfo fieldInfo = null;
            if (CurrentEntity != null)
            {
                fieldInfo = CurrentEntity.Data.GetField(Name);
            }
            if (fieldInfo != null)
            {
                if (fieldInfo.IsField)
                {
                    expr = GenerateFieldGet(fieldInfo.Alias, fieldInfo.Name);

                }
                else
                {
                    expr = GenerateMemVarGet(fieldInfo.Name);
                }
            }
            context.Put(expr);
        }


        #endregion

        #region Expressions

        private ExpressionSyntax GenerateNIL()
        {
            //if (_options.XSharpRuntime)
            //    return GenerateQualifiedName(XSharpQualifiedFunctionNames.UsualNIL);
            //else
            //    return GenerateQualifiedName(VulcanQualifiedFunctionNames.UsualNIL);
            return MakeDefault(_usualType);
        }
        protected override ExpressionSyntax GenerateMissingExpression(bool AddError)
        {
            var result = GenerateNIL();
            if (AddError)
                result = result.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_MissingArgument));
            return result;
        }
        #endregion

        #region ClassVars and LocalVars
        protected override void VisitClassvar([NotNull] XP.ClassvarContext context)
        {
            base.VisitClassvar(context);
            if (context.ArraySub != null && context.Dim == null)
            {
                var vd = context.Get<VariableDeclaratorSyntax>();
                var initializer = GenerateVOArrayInitializer(context.ArraySub);
                if (context.Initializer != null)
                {
                    // You cannot have both an  initializer initial Dimensions
                    initializer = initializer.WithAdditionalDiagnostics(
                            new SyntaxDiagnosticInfo(ErrorCode.ERR_VulcanArrayDimAndInit));
                }
                context.Put(GenerateVariable(vd.Identifier, initializer));
            }
        }

        protected override void VisitLocalvar([NotNull] XP.LocalvarContext context)
        {
            if (context.ArraySub != null && context.Dim == null && 
                (context.DataType == null || 
                    context.DataType.Get<TypeSyntax>() == _arrayType ||
                    context.DataType.Get<TypeSyntax>() == _usualType))
            {
                // Change LOCAL a[20]   to LOCAL a[20] AS ARRAY
                // Build the whole tree because when this is used in Start() then we try to determine if
                // the locals needs to be assigned with NULL and that code expects the complete tree
                if (context.DataType == null )
                {
                    var dataType = FixPosition(new XP.DatatypeContext(context, 0), context.Stop);
                    var sdt = new XP.SimpleDatatypeContext(dataType);
                    sdt.Put(_arrayType);
                    context.DataType = sdt;
                    context.AddChild(sdt);
                    var typeName = FixPosition(new XP.TypeNameContext(sdt, 0), context.Stop);
                    dataType.AddChild(typeName);
                    sdt.TypeName = typeName;
                    var xBaseType = FixPosition(new XP.XbaseTypeContext(typeName, 0), context.Stop);
                    typeName.AddChild(xBaseType);
                    var token = new XSharpToken(XP.ARRAY, "ARRAY");
                    xBaseType.Start = token;
                }
                if (context.DataType.Get<TypeSyntax>() == _usualType)
                {
                    context.DataType.CsNode = _arrayType;
                }
                var initializer = GenerateVOArrayInitializer(context.ArraySub);
                if (context.Expression != null)
                {
                    // You cannot have both an  initializer initial Dimensions
                    initializer = initializer.WithAdditionalDiagnostics(
                        new SyntaxDiagnosticInfo(ErrorCode.ERR_VulcanArrayDimAndInit));
                }
                else
                {
                    context.Expression = FixPosition(new XP.ExpressionContext(context, 0), context.Stop);
                }
                context.Expression.Put<ExpressionSyntax>(initializer);
            }
            base.VisitLocalvar(context);
        }

        public override void ExitArrayOfType([NotNull] XP.ArrayOfTypeContext context)
        {
            if (!_options.XSharpRuntime)
            {
                context.Put(NotInDialect(_objectType, "ARRAY OF <type>"));
            }
            else
            {
                var type = MakeGenericName(OurTypeNames.ArrayBase, context.TypeName.Get<TypeSyntax>());
                var qtype = _syntaxFactory.QualifiedName(GenerateSimpleName("XSharp"),
                        SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                        type);
                context.Put(qtype);
            }
        }


        public override void ExitXbaseType([NotNull] XP.XbaseTypeContext context)
        {
            TypeSyntax type = null;
            switch (context.Token.Type)
            {
                case XP.ARRAY:
                    type = _arrayType;
                    break;
                case XP.CODEBLOCK:
                    type = _codeblockType;
                    break;
                case XP.DATE:
                    type = _dateType;
                    break;
                case XP.FLOAT:
                    type = _floatType;
                    break;
                case XP.PSZ:
                    type = _pszType;
                    break;
                case XP.USUAL:
                    type = _usualType;
                    break;
                case XP.SYMBOL:
                    type = _symbolType;
                    break;
                default:
                    type = null;
                    break;
            }
            if (type == null)
            {
                type = (TypeSyntax)NotInDialect(_objectType, context.Token.Text);
            }
            context.Put(type);
        }

        #endregion

        #region Helpers for Method Init and Method Axit (/vo1)
        protected ConstructorDeclarationSyntax createConstructor(
            XP.IEntityContext context,
            SyntaxList<SyntaxToken> modifiers,
            XP.AttributesContext atts,
            XP.ParameterListContext paramlist,
            XP.StatementBlockContext stmtblock,
            XSharpParserRuleContext errorcontext,
            IToken chain = null,
            XP.ArgumentListContext args = null,
            bool isInInterface = false)
        {
            if (modifiers.Any((int)SyntaxKind.ExternKeyword))
            {
                if (stmtblock?._Stmts?.Count > 0)
                {
                    errorcontext.AddError(new ParseErrorData(stmtblock, ErrorCode.ERR_ExternHasBody));
                }
                stmtblock = null;
            }
            if (isInInterface)
            {
                errorcontext.AddError(new ParseErrorData(errorcontext.Start, ErrorCode.ERR_InterfacesCantContainConstructors));
                return null;
            }
            else
            {
                var attributes = atts?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>();
                var parameters = paramlist?.Get<ParameterListSyntax>() ?? EmptyParameterList();
                var body = stmtblock?.Get<BlockSyntax>();
                TypeSyntax returntype = null;
                if (chain != null && context.Data.HasClipperCallingConvention)
                {
                    var chainArgs = args?.Get<ArgumentListSyntax>() ?? EmptyArgumentList();
                    var chainExpr = MakeSimpleMemberAccess(
                        chain.Type == XP.SELF ? (ExpressionSyntax)GenerateSelf() : GenerateSuper(),
                        GenerateSimpleName(".ctor"));
                    body = MakeBlock(MakeList<StatementSyntax>(
                        GenerateExpressionStatement(_syntaxFactory.InvocationExpression(chainExpr, chainArgs)),
                        body));
                    chain = null;
                }
                ImplementClipperAndPSZ(context, ref attributes, ref parameters, ref body, ref returntype);
                SyntaxToken typeId = null;
                if (context is XP.MethodContext)
                {
                    var clsm = context as XP.MethodContext;
                    if (clsm.ClassId != null)
                    {
                        // Method Init Class Foo
                        typeId = clsm.ClassId.Get<SyntaxToken>();
                    }
                }
                else if (context is XP.ConstructorContext)
                {
                    var clsc = context as XP.ConstructorContext;
                    if (clsc.ClassId != null)
                    {
                        // Method Init Class Foo
                        typeId = clsc.ClassId.Get<SyntaxToken>();
                    }
                }
                else if (context is XP.XppmethodContext)
                {
                    var clsm = context as XP.XppmethodContext;
                    if (clsm.ClassId != null)
                    {
                        // Method Init Class Foo
                        typeId = clsm.ClassId.Get<SyntaxToken>();
                    }
                }

                if (typeId == null)
                {
                    if (context.Parent is XP.ClsmethodContext |
                        context.Parent is XP.ClsctorContext |
                        context.Parent is XP.ClsdtorContext )
                    {
                        {
                            typeId = (context.Parent.Parent as XP.Class_Context)?.Id.Get<SyntaxToken>()
                                ?? (context.Parent.Parent as XP.Structure_Context)?.Id.Get<SyntaxToken>()
                                ?? (context.Parent.Parent as XP.Interface_Context)?.Id.Get<SyntaxToken>();
                        }
                    }
                    else if (context.Parent is XP.XppclassMemberContext)
                    {
                        typeId = (context.Parent.Parent as XP.XppclassContext)?.Id.Get<SyntaxToken>();
                    }
                    else
                    {
                        typeId = (context.Parent as XP.Class_Context)?.Id.Get<SyntaxToken>()
                            ?? (context.Parent as XP.Structure_Context)?.Id.Get<SyntaxToken>()
                            ?? (context.Parent as XP.Interface_Context)?.Id.Get<SyntaxToken>();
                    }
                }
                if (typeId == null)
                {
                    return null;
                }
                return _syntaxFactory.ConstructorDeclaration(
                    attributeLists: attributes,
                    modifiers: modifiers,
                    identifier: typeId,
                    parameterList: parameters,
                    initializer: createInitializer(chain, args), 
                    body: body,
                    expressionBody: null,
                    semicolonToken: (stmtblock?._Stmts?.Count > 0) ? null :
                    SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            }

        }

        protected DestructorDeclarationSyntax createDestructor(
            XP.IEntityContext context,
            SyntaxList<SyntaxToken> modifiers,
            XP.AttributesContext atts,
            XP.StatementBlockContext stmtblock,
            XSharpParserRuleContext errorcontext,
            bool isInInterface = false)
        {
            if (modifiers.Any((int)SyntaxKind.ExternKeyword))
            {
                if (stmtblock?._Stmts?.Count > 0)
                {
                    errorcontext.AddError(new ParseErrorData(stmtblock, ErrorCode.ERR_ExternHasBody, "Destructor"));
                }
                stmtblock = null;
            }
            if (isInInterface)
            {
                errorcontext.AddError(new ParseErrorData(errorcontext.Start, ErrorCode.ERR_InterfacesCantContainConstructors));
            }
            else
            {
                // no return statement needed in DESTRUCTOR
                // body = AddMissingReturnStatement(body, context.StmtBlk, null);
                SyntaxToken parentId = null;
                if (context is XP.MethodContext)
                {
                    var clsm = context as XP.MethodContext;
                    if (clsm.ClassId != null)
                    {
                        // Method Axit Class Foo
                        parentId = clsm.ClassId.Get<SyntaxToken>();
                    }
                }
                if (parentId == null)
                {
                    if (context.Parent is XP.ClsmethodContext)
                    {
                        parentId = (context.Parent.Parent as XP.Class_Context)?.Id.Get<SyntaxToken>()
                            ?? (context.Parent.Parent as XP.Structure_Context)?.Id.Get<SyntaxToken>()
                            ?? (context.Parent.Parent as XP.Interface_Context)?.Id.Get<SyntaxToken>();
                    }
                    else
                    {
                        parentId = (context.Parent as XP.Class_Context)?.Id.Get<SyntaxToken>()
                            ?? (context.Parent as XP.Structure_Context)?.Id.Get<SyntaxToken>()
                            ?? (context.Parent as XP.Interface_Context)?.Id.Get<SyntaxToken>();
                    }
                }
                if (parentId == null)
                    return null;
                return _syntaxFactory.DestructorDeclaration(
                    attributeLists: atts?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: modifiers,
                    tildeToken: SyntaxFactory.MakeToken(SyntaxKind.TildeToken),
                    identifier: parentId,
                    parameterList: EmptyParameterList(),
                    body: stmtblock.Get<BlockSyntax>(),
                    expressionBody: null,
                    semicolonToken: (stmtblock != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            }
            return null;
        }
  
            
        #endregion

        #region Helpers
        protected override TypeSyntax _getParameterType([NotNull] XP.ParameterContext context)
        {
            TypeSyntax type = context.Type?.Get<TypeSyntax>();
            if (type == null)
            {
                if (!_options.VOUntypedAllowed)
                    type = _getMissingType();
                else if (CurrentEntity != null && CurrentEntity.Data.HasTypedParameter )
                    type = _usualType;
                else
                    type = _getMissingType();
            }
            return type;
        }

        protected override TypeSyntax _getMissingType()
        {
            TypeSyntax varType;
            if (_options.VOUntypedAllowed)
                varType = _usualType;
            else
                varType = MissingType();
            return varType;
        }

        public override ConstructorDeclarationSyntax GenerateDefaultCtor(SyntaxToken id, XP.Class_Context classctx)
        {
            ParameterListSyntax pars = GetClipperParameters();

            var arg = MakeArgument(GenerateSimpleName(XSharpSpecialNames.ClipperArgs));
            ArgumentListSyntax args = MakeArgumentList(arg);

            var chain = _syntaxFactory.ConstructorInitializer(SyntaxKind.BaseConstructorInitializer,
                                                                SyntaxFactory.MakeToken(SyntaxKind.ColonToken),
                                                                SyntaxFactory.MakeToken(SyntaxKind.BaseKeyword),
                                                                args
                                                                );
            var stmts = new List<StatementSyntax>();
            var body = MakeBlock(stmts);
            SyntaxListBuilder<AttributeListSyntax> attributeLists = _pool.Allocate<AttributeListSyntax>();
            GenerateAttributeList(attributeLists, SystemQualifiedNames.CompilerGenerated);
            attributeLists.Add(MakeClipperCallingConventionAttribute(new List<ExpressionSyntax>()));
            var mods = TokenList(SyntaxKind.PublicKeyword);
            var ctor = _syntaxFactory.ConstructorDeclaration(attributeLists, mods, id, pars, chain, body, null, null);
            ctor.XGenerated = true;
            _pool.Free(attributeLists);
            return ctor;
        }

        #endregion

        #region Listener Methods
        public override void ExitClass_([NotNull] XP.Class_Context context)
        {
            base.ExitClass_(context);
            var csNode = context.Get<CSharpSyntaxNode>();
            ClassDeclarationSyntax classdecl = null;
            if (csNode is ClassDeclarationSyntax)
                classdecl = csNode as ClassDeclarationSyntax;
            else if (csNode is NamespaceDeclarationSyntax)
            {
                var ns = csNode as NamespaceDeclarationSyntax;
                classdecl = ns.Members[0] as ClassDeclarationSyntax;
            }
            else
            {
                return;
            }
            var members = classdecl.Members;
            // No need to create an instance constructor for a static class
            if (!classdecl.IsStatic())
            {
                context.Data.HasInstanceCtor = false;
                foreach (var m in members)
                {
                    if (m is ConstructorDeclarationSyntax && !((ConstructorDeclarationSyntax)m).IsStatic())
                    {
                        context.Data.HasInstanceCtor = true;
                        break;
                    }
                }

                if (!context.Data.Partial && !context.Data.HasInstanceCtor && _options.VOClipperConstructors)
                {
                    //
                    // generate new class constructor
                    // 
                    var ctor = GenerateDefaultCtor(classdecl.Identifier, context);
                    var newmembers = _pool.Allocate<MemberDeclarationSyntax>();
                    newmembers.AddRange(classdecl.Members);
                    if (ctor != null)
                    {
                        newmembers.Add(ctor);
                    }
                    classdecl = classdecl.Update(
                        classdecl.AttributeLists,
                        classdecl.Modifiers,
                        classdecl.Keyword,
                        classdecl.Identifier,
                        classdecl.TypeParameterList,
                        classdecl.BaseList,
                        classdecl.ConstraintClauses,
                        classdecl.OpenBraceToken,
                        newmembers,
                        classdecl.CloseBraceToken,
                        classdecl.SemicolonToken);
                    _pool.Free(newmembers);
                    context.Put(classdecl);
                    context.Data.HasInstanceCtor = true;
                }
            }
        }
        public override void ExitConstructor([NotNull] XP.ConstructorContext context)
        {
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility();
            if (mods.Any((int)SyntaxKind.StaticKeyword))
            {
                context.Data.HasClipperCallingConvention = false;
            }
            var ctor = createConstructor(
                context, mods, context.Attributes, context.ParamList, context.StmtBlk, context,
                context.Chain, context.ArgList, context.isInInterface());
            if (ctor != null)
            {
                context.Put(ctor);
            }
            else
            {
                context.StmtBlk = null;
            }
            return;
        }

        public override void ExitDestructor([NotNull] XP.DestructorContext context)
        {
            var modifiers = context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList<SyntaxToken>();
            var dtor = createDestructor(context, modifiers,
                context.Attributes, context.StmtBlk, context, context.isInInterface());
            if (dtor != null)
            {
                context.Put(dtor);
            }
            else
            {
                context.StmtBlk = null;
            }
            return;
        }

        public override void ExitCallingconvention([NotNull] XP.CallingconventionContext context)
        {
            // TODO nvk (calling convention is silently ignored for now)
            if (CurrentEntity != null)
            {
                if (context.Convention.Type == XP.CLIPPER && context.Parent == CurrentEntity)
                {
                    CurrentEntity.Data.HasClipperCallingConvention = true;
                }
            }
            base.ExitCallingconvention(context);
        }

        public override void ExitMethod([NotNull] XP.MethodContext context)
        {
            if (context.Data.IsInitAxit)
            {
                var idName = context.Id.GetText();
                if (String.Equals(idName, "init", StringComparison.OrdinalIgnoreCase))
                {
                    // Convert method to constructor
                    var mods = context.Modifiers?.GetList<SyntaxToken>() ?? MakeList<SyntaxToken>(SyntaxFactory.MakeToken(SyntaxKind.PublicKeyword));
                    var ctor = createConstructor(context,
                        mods, context.Attributes, context.ParamList, context.StmtBlk, context);
                    if (ctor != null)
                    {
                        context.Put(ctor);
                    }
                    else
                    {
                        context.StmtBlk = null;
                    }
                    return;
                }
                else if (String.Equals(idName, "axit", StringComparison.OrdinalIgnoreCase))
                {
                    // Convert method to destructor
                    var mods = context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList<SyntaxToken>();
                    var dtor = createDestructor(context,
                        mods, context.Attributes, context.StmtBlk, context);
                    if (dtor != null)
                    {
                        context.Put(dtor);
                    }
                    else
                    {
                        context.StmtBlk = null;
                    }
                    return;
                }
            }
            base.ExitMethod(context);
        }

 
        public override void ExitBinaryExpression([NotNull] XP.BinaryExpressionContext context)
        {
            if (context.Op.Type == XP.SUBSTR)
            {
                string method = _options.XSharpRuntime ? XSharpQualifiedFunctionNames.InStr : VulcanQualifiedFunctionNames.InStr; ;
                var argLeft = context.Left.Get<ExpressionSyntax>();
                var argRight = context.Right.Get<ExpressionSyntax>();
                var args = MakeArgumentList(MakeArgument(argLeft), MakeArgument(argRight));
                var expr = GenerateMethodCall(method, args, true);
                context.Put(expr);

                return;
            }
            else if (context.Op.Type == XP.DIV && _options.VOClipperIntegerDivisions)
            {
                var lhs = MakeCastTo(_usualType, context.Left.Get<ExpressionSyntax>());
                var rhs = MakeCastTo(_usualType, context.Right.Get<ExpressionSyntax>());
                context.Put(_syntaxFactory.BinaryExpression(
                    context.Op.ExpressionKindBinaryOp(),
                    lhs,
                    context.Op.SyntaxOp(),
                    rhs));
                return;
            }
            base.ExitBinaryExpression(context);
        }

        public override void ExitIntrinsicExpression([NotNull] XP.IntrinsicExpressionContext context)
        {
            // _And , _OR, _XOR, _NOT should be unchecked to be VO/Vulcan compatible
            base.ExitIntrinsicExpression(context);
            var expr = MakeChecked(context.Get<ExpressionSyntax>(), false);
            context.Put(expr);
        }
        public override void ExitJumpStmt([NotNull] XP.JumpStmtContext context)
        {
            if (context.Key.Type == XP.BREAK)
            {
                ArgumentListSyntax args;
                context.SetSequencePoint(context.end);
                if (context.Expr != null)
                    args = MakeArgumentList(MakeArgument(context.Expr.Get<ExpressionSyntax>()));
                else
                    args = MakeArgumentList(MakeArgument(GenerateNIL()));
                var expr = CreateObject(GenerateQualifiedName(_wrappedExceptionType), args);
                context.Put(_syntaxFactory.ThrowStatement(SyntaxFactory.MakeToken(SyntaxKind.ThrowKeyword),
                    expr,
                        SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            }
            else
            {
                base.ExitJumpStmt(context);
            }
        }

        public override void ExitQoutStmt([NotNull] XP.QoutStmtContext context)
        {
            // Simply generate call to VulcanRTFuncs.Functions.QOut or QQOut
            // and pass list of expressions as argument
            ArgumentSyntax arg;
            string methodName;
            context.SetSequencePoint(context.end);
            if (context.Q.Type == XP.QQMARK)
                methodName = _options.XSharpRuntime ? XSharpQualifiedFunctionNames.QQout : VulcanQualifiedFunctionNames.QQout;
            else
                methodName = _options.XSharpRuntime ? XSharpQualifiedFunctionNames.Qout : VulcanQualifiedFunctionNames.Qout;
            ArgumentListSyntax args;
            if (context._Exprs != null && context._Exprs.Count > 0)
            {
                var al = new List<ArgumentSyntax>();
                foreach (var eCtx in context._Exprs)
                {
                    arg = MakeArgument(eCtx.Get<ExpressionSyntax>());
                    al.Add(arg);
                }
                args = MakeArgumentList(al.ToArray());
            }
            else
            {
                args = EmptyArgumentList();
            }
            var call = GenerateMethodCall(methodName, args, true);
            context.Put(GenerateExpressionStatement(call));
            return;
        }


        public override void ExitAccessMember([NotNull] XP.AccessMemberContext context)
        {
            if (context.Op.Type == XP.COLONCOLON)
            {
                context.Put(MakeSimpleMemberAccess(
                    GenerateSelf(),
                    context.Name.Get<SimpleNameSyntax>()));
                return;

            }
            else if (context.Op.Type == XP.DOT)
            {
                if (context.Expr.Get<ExpressionSyntax>() is NameSyntax)
                {
                    context.Put(_syntaxFactory.QualifiedName(
                        context.Expr.Get<NameSyntax>(),
                        SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                        context.Name.Get<SimpleNameSyntax>()));
                    return;
                }
                else // if (context.Expr is XP.ArrayAccessContext)
                {
                    context.Put(MakeSimpleMemberAccess(
                        context.Expr.Get<ExpressionSyntax>(),
                        context.Name.Get<SimpleNameSyntax>()));
                }
                //else
                //{
                //    context.Put(MakeSimpleMemberAccess(
                //        context.Expr.Get<ExpressionSyntax>(),
                //        (SimpleNameSyntax) NotInDialect(context.Name.Get<SimpleNameSyntax>(),"equivalency of : and . member access operators"))
                //        );
                //    return;
                //}
            }
            context.Put(MakeSimpleMemberAccess(
                context.Expr.Get<ExpressionSyntax>(),
                context.Name.Get<SimpleNameSyntax>()));
        }

        public override void ExitPrefixExpression([NotNull] XP.PrefixExpressionContext context)
        {
            ExpressionSyntax expr = context.Expr.Get<ExpressionSyntax>();
            var Op = context.Op;
            // 
            if (expr.XNode is XP.PrimaryExpressionContext && expr.XNode.GetChild(0) is XP.AliasedFieldContext)
            {
                // prefix on an aliased field
                // ++Customer->CustNo
                // translate to 
                // Functions.__FieldSetWa("Customer", "CustNo", Functions.__FieldGetWa("Customer", "CustNo") + 1);
                XP.AliasedFieldContext fieldNode = expr.XNode.GetChild(0) as XP.AliasedFieldContext;
                var alias = fieldNode.Alias?.GetText();
                var field = fieldNode.Field.GetText();
                expr = GenerateFieldGet(alias, field);
                var lit = GenerateLiteral(1);
                switch (Op.Type)
                {
                    case XSharpLexer.INC:
                        expr = _syntaxFactory.BinaryExpression(SyntaxKind.AddExpression, expr, SyntaxFactory.MakeToken(SyntaxKind.PlusToken), lit);
                        expr = GenerateFieldSet(alias, field, expr);
                        context.Put(expr);
                        return;
                    case XSharpLexer.DEC:
                        expr = _syntaxFactory.BinaryExpression(SyntaxKind.SubtractExpression, expr, SyntaxFactory.MakeToken(SyntaxKind.MinusToken), lit);
                        expr = GenerateFieldSet(alias, field, expr);
                        context.Put(expr);
                        return;
                    case XSharpLexer.TILDE:
                    case XSharpLexer.PLUS:
                    case XSharpLexer.MINUS:
                    case XSharpLexer.ADDROF:
                        // these all are normal operators and do not change the field
                        break;
                }
            }
            else if (expr.XNode is XP.PrimaryExpressionContext && expr.XNode.GetChild(0) is XP.NameExpressionContext)
            {
                // (Expr) ->++FIELD 
                // FIELD Is a NameExpression here
                // we need to redo this here
                var nameExpr = expr.XNode.GetChild(0) as XP.NameExpressionContext;
                expr = GeneratePrePostFixField(nameExpr, context.Op);
                if (expr != null)
                {
                    context.Put(expr);
                    return;
                }
            }
            base.ExitPrefixExpression(context);

        }

        private ExpressionSyntax GeneratePrePostFixField(XP.NameExpressionContext nameExpr, IToken Op)
        {
            ExpressionSyntax expr = null;
            var lit = GenerateLiteral(1);
            string field = nameExpr.GetText();
            MemVarFieldInfo fieldInfo = null;
            if (CurrentEntity != null)
            {
                fieldInfo = CurrentEntity.Data.GetField(field);
            }
            if (fieldInfo != null)
            {
                if (fieldInfo.IsField)
                {
                    expr = GenerateFieldGet(fieldInfo.Alias, fieldInfo.Name);
                }
                else
                {
                    expr = GenerateMemVarGet(fieldInfo.Name);
                }

                if (Op.Type == XSharpLexer.INC)
                {
                    expr = _syntaxFactory.BinaryExpression(SyntaxKind.AddExpression, expr, SyntaxFactory.MakeToken(SyntaxKind.PlusToken), lit);
                }
                else
                {
                    expr = _syntaxFactory.BinaryExpression(SyntaxKind.SubtractExpression, expr, SyntaxFactory.MakeToken(SyntaxKind.MinusToken), lit);
                }
                if (fieldInfo.IsField)
                {
                    expr = GenerateFieldSet(null, field, expr);
                }
                else
                {
                    expr = GenerateMemVarPut(field, expr);
                }
            }
            return expr;
        }

        public override void ExitPostfixExpression([NotNull] XP.PostfixExpressionContext context)
        {
            ExpressionSyntax expr = context.Expr.Get<ExpressionSyntax>();
            var Op = context.Op;
            // 
            if (expr.XNode is XP.PrimaryExpressionContext && expr.XNode.GetChild(0)  is XP.AliasedFieldContext)
            {
                // postfix on an aliased field
                // Customer->CustNo ++
                // translate to 
                // Functions.__FieldSetWa("Customer", "CustNo", Functions.__FieldGetWa("Customer", "CustNo") + 1);

                XP.AliasedFieldContext fieldNode = expr.XNode.GetChild(0) as XP.AliasedFieldContext;
                var alias = fieldNode.Alias?.GetText();
                var field = fieldNode.Field.GetText();
                expr = GenerateFieldGet(alias, field);
                var lit = GenerateLiteral(1);
                if (Op.Type == XSharpLexer.INC)
                    expr = _syntaxFactory.BinaryExpression(SyntaxKind.AddExpression, expr, SyntaxFactory.MakeToken(SyntaxKind.PlusToken), lit);
                else
                    expr = _syntaxFactory.BinaryExpression(SyntaxKind.SubtractExpression, expr, SyntaxFactory.MakeToken(SyntaxKind.MinusToken), lit);
                    expr = GenerateFieldSet(alias, field, expr);
                context.Put(expr);
                return;
                
            }
            else if (expr.XNode is XP.PrimaryExpressionContext && expr.XNode.GetChild(0) is XP.NameExpressionContext)
            {
                // (Expr) ->FIELD ++
                // FIELD gets converted to an InvocationSyntax of FieldGet or MemVarGet (depending on FIELD or MEMVAR statement)
                // we need to redo this here
                var nameExpr = expr.XNode.GetChild(0)  as XP.NameExpressionContext;
                expr = GeneratePrePostFixField(nameExpr, context.Op);
                if (expr != null)
                { 
                    context.Put(expr);
                    return;
                }
            }
            base.ExitPostfixExpression(context);

        }
        public override void ExitAssignmentExpression([NotNull] XP.AssignmentExpressionContext context)
        {
            // when /vo12 is used then for the types .ASSIGN_DIV add conversion for the LHS and RHS to Double
            // Check for Field or MemVar assignments
            ExpressionSyntax left = context.Left.Get<ExpressionSyntax>();
            ExpressionSyntax right = context.Right.Get<ExpressionSyntax>();
            if (left.XNode is XP.PrimaryExpressionContext && left.XNode.GetChild(0) is XP.AliasedFieldContext)
            {
                XP.AliasedFieldContext fieldNode = left.XNode.GetChild(0) as XP.AliasedFieldContext;
                //ToDo
                // Convert _FIELD->NAME += 1 to _FIELD->NAME := _FIELD->NAME + 1

                ExpressionSyntax expr;
                if (context.Op.Type == XP.ASSIGN_OP)
                {
                    expr = GenerateFieldSet(fieldNode.Alias?.GetText(), fieldNode.Field.GetText(), right);
                }
                else
                {
                    // AREA->NAME+= 1 gets converted to
                    // __FieldSet("Area", "Name", __FieldGet("Area","Name") + 1)
                    // left already has the FieldGet
                    var op = context.Op.ComplexToSimpleBinaryOp();
                    var token = context.Op.ComplexToSimpleToken();
                    if (op == SyntaxKind.EmptyStatement)
                    {
                        expr = GenerateFieldSet(fieldNode.Alias?.GetText(), fieldNode.Field.GetText(), right);
                        expr = (ExpressionSyntax)NotInDialect(expr, "Complex operation: " + context.Op.Text);
                    }
                    else
                    {
                        expr = _syntaxFactory.BinaryExpression(op, left, token, right);
                        expr = GenerateFieldSet(fieldNode.Alias?.GetText(), fieldNode.Field.GetText(), expr);
                    }

                }
                context.Put(expr);
                return;

            }
            else if (left.XNode is XP.PrimaryExpressionContext && left.XNode.GetChild(0) is XP.NameExpressionContext)
            {
                XP.NameExpressionContext namecontext = left.XNode.GetChild(0) as XP.NameExpressionContext;
                string name = namecontext.Name.GetText();
                MemVarFieldInfo fieldInfo = null;
                if (CurrentEntity != null)
                {
                    fieldInfo = CurrentEntity.Data.GetField(name);
                }
                if (fieldInfo != null)
                {
                    ExpressionSyntax expr;
                    if (fieldInfo.IsField)
                    {
                        if (context.Op.Type == XP.ASSIGN_OP)
                        {
                            expr = GenerateFieldSet(fieldInfo.Alias, fieldInfo.Name, right);
                        }
                        else
                        {
                            var op = context.Op.ComplexToSimpleBinaryOp();
                            var token = context.Op.ComplexToSimpleToken();
                            if (op == SyntaxKind.EmptyStatement)
                            {
                                expr = GenerateFieldSet(fieldInfo.Alias, fieldInfo.Name, right);
                                expr = (ExpressionSyntax)NotInDialect(expr, "Complex operation: " + context.Op.Text);
                            }
                            else
                            {

                                // AREA->NAME+= 1 gets converted to
                                // __FieldSet("Area", "Name", __FieldGet("Area","Name") + 1)
                                // left already has the FieldGet
                                expr = _syntaxFactory.BinaryExpression(op, left, token, right);
                                expr = GenerateFieldSet(fieldInfo.Alias, fieldInfo.Name, expr);
                            }
                        }
                    }
                    else
                    {
                        if (context.Op.Type == XP.ASSIGN_OP)
                        {
                            expr = GenerateMemVarPut(fieldInfo.Name, right);
                        }
                        else
                        {
                            var op = context.Op.ComplexToSimpleBinaryOp();
                            var token = context.Op.ComplexToSimpleToken();
                            if (op == SyntaxKind.EmptyStatement)
                            {
                                expr = GenerateMemVarPut(fieldInfo.Name, right);
                                expr = (ExpressionSyntax)NotInDialect(expr, "Complex operation: " + context.Op.Text);
                            }
                            else
                            {

                                // NAME+= 1 gets converted to MemVarPut("Name", MemVarGet("Name") + 1)
                                // left already has the MemVarGet
                                // MemVarPut( "Name", __MemVarGet("Name") + 1)
                                expr = _syntaxFactory.BinaryExpression(op, left, token, right);
                                expr = GenerateMemVarPut(fieldInfo.Name, expr);
                            }
                        }
                    }
                    context.Put(expr);
                    return;
                }
            }
            base.ExitAssignmentExpression(context);
        }
        public override void ExitSizeOfExpression([NotNull] XP.SizeOfExpressionContext context)
        {
            // cast result of sizeof to DWORD to be compatible
            base.ExitSizeOfExpression(context);
            context.Put(MakeCastTo(_syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.UIntKeyword)),
                context.Get<ExpressionSyntax>()));
        }

        #endregion

        #region Error Handling
        public override void ExitSeqStmt([NotNull] XP.SeqStmtContext context)
        {
            // This generates 2 try statements:
            // 1) an inner try that controls the calls to CompilerServices.EnterBeginSequence();
            //    and CompilerServices.ExitBeginSequence();
            // 2) an outer try that has the try - catch - finally from the seq statement itself

            context.SetSequencePoint(context.end);
            var stmts = _pool.Allocate<StatementSyntax>();
            stmts.Add(GenerateExpressionStatement(GenerateMethodCall(
                _options.XSharpRuntime ? XSharpQualifiedFunctionNames.EnterSequence : VulcanQualifiedFunctionNames.EnterSequence,true)));
            stmts.Add(MakeBlock(context.StmtBlk.Get<BlockSyntax>()));
            var tryBlock = MakeBlock(stmts);
            stmts.Clear();
            stmts.Add(GenerateExpressionStatement(GenerateMethodCall(
                _options.XSharpRuntime ? XSharpQualifiedFunctionNames.ExitSequence : VulcanQualifiedFunctionNames.ExitSequence, true)));
            var innerTry = _syntaxFactory.TryStatement(SyntaxFactory.MakeToken(SyntaxKind.TryKeyword),
                 tryBlock,
                 null,
                 _syntaxFactory.FinallyClause(SyntaxFactory.MakeToken(SyntaxKind.FinallyKeyword),
                     MakeBlock(stmts)));
            stmts.Clear();
            stmts.Add(innerTry);
            tryBlock = MakeBlock(stmts);
            stmts.Clear();
            CatchClauseSyntax catchClause = null;
            FinallyClauseSyntax finallyClause = null;
            if (context.RecoverBlock != null)
            {
                catchClause = context.RecoverBlock.Get<CatchClauseSyntax>();
            }
            else
            {
                // generate a default catch block if there is no finally block
                if (context.FinBlock == null)
                {
                    var cb = FixPosition(new XP.CatchBlockContext(context, 0), context.Stop);
                    cb.StmtBlk = FixPosition(new XP.StatementBlockContext(cb, 0), context.Stop);
                    this.ExitStatementBlock(cb.StmtBlk);
                    this.ExitCatchBlock(cb);
                    catchClause = cb.Get<CatchClauseSyntax>();

                }
            }
            if (context.FinBlock != null)
            {
                finallyClause = _syntaxFactory.FinallyClause(
                 SyntaxFactory.MakeToken(SyntaxKind.FinallyKeyword),
                 context.FinBlock.Get<BlockSyntax>());
            }
            StatementSyntax outerTry = _syntaxFactory.TryStatement(SyntaxFactory.MakeToken(SyntaxKind.TryKeyword),
                  tryBlock,
                  catchClause,
                  finallyClause);
            context.Put(outerTry);
            _pool.Free(stmts);

        }

        public override void ExitRecoverBlock([NotNull] XP.RecoverBlockContext context)
        {
            // The recover block has source code:
            //RECOVER USING uValue
            //  statements
            //
            // and gets converted to
            //
            // Catch obj as Exception
            // if obj is VulcanWrappedException                                        // condition 1
            //   uValue := ((VulcanWrappedException) obj).Value                        // assign 1
            //
            // else if obj is Vulcan.Error                                             // condition 2
            //   uValue :=  obj1                                                       // assign 2
            //
            // else if obj is Exception                                                // condition 3
            //   // wraps Exception in Vulcan.Error Object                             // Always true unless obj = NULL
            //   uValue := (USUAL)  Error._WrapRawException((Exception) obj1))         // assign 3
            //
            // else
            //   uValue := obj1                                                        // assign 4
            // endif
            var stmts = _pool.Allocate<StatementSyntax>();
            var catchVar = SyntaxFactory.Identifier(XSharpSpecialNames.RecoverVarName);
            context.SetSequencePoint(context.end);
            if (context.Id != null)
            {
                var objName = GenerateSimpleName(XSharpSpecialNames.RecoverVarName);
                var idName = GenerateSimpleName(context.Id.GetText());

                var condition1 = _syntaxFactory.BinaryExpression(
                      SyntaxKind.IsExpression,
                      objName,
                      SyntaxFactory.MakeToken(SyntaxKind.IsKeyword),
                      GenerateQualifiedName(_wrappedExceptionType));

                var condition2 = _syntaxFactory.BinaryExpression(
                    SyntaxKind.IsExpression,
                    objName,
                    SyntaxFactory.MakeToken(SyntaxKind.IsKeyword),
                    GenerateQualifiedName(_errorType));

                var condition3 = _syntaxFactory.BinaryExpression(
                    SyntaxKind.IsExpression,
                    objName,
                    SyntaxFactory.MakeToken(SyntaxKind.IsKeyword),
                    GenerateQualifiedName(SystemQualifiedNames.Exception));

                var assign1 = GenerateExpressionStatement(
                    MakeSimpleAssignment(idName,
                       MakeSimpleMemberAccess(
                       MakeCastTo(GenerateQualifiedName(_wrappedExceptionType), objName),
                         GenerateSimpleName("Value"))));

                var assign2 = GenerateExpressionStatement(MakeSimpleAssignment(idName, objName));

                var assign3 = GenerateExpressionStatement(MakeSimpleAssignment(
                    idName, MakeCastTo(_usualType, GenerateMethodCall(
                        _options.XSharpRuntime ? XSharpQualifiedFunctionNames.WrapException : VulcanQualifiedFunctionNames.WrapException,
                    MakeArgumentList(MakeArgument(objName)),true))));

                var assign4 = GenerateExpressionStatement(MakeSimpleAssignment(idName, objName));

                var elseClause = _syntaxFactory.ElseClause(
                   SyntaxFactory.MakeToken(SyntaxKind.ElseKeyword),
                   MakeBlock(assign4));

                // if 3
                var ifstmt = _syntaxFactory.IfStatement(
                            SyntaxFactory.MakeToken(SyntaxKind.IfKeyword),
                            SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                            condition3,
                            SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                            MakeBlock(assign3), elseClause);

                // if 3 is assigned to the else block of if 2
                elseClause = _syntaxFactory.ElseClause(
                    SyntaxFactory.MakeToken(SyntaxKind.ElseKeyword),
                    MakeBlock(ifstmt));

                // if 2
                ifstmt = _syntaxFactory.IfStatement(
                            SyntaxFactory.MakeToken(SyntaxKind.IfKeyword),
                            SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                            condition2,
                            SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                            MakeBlock(assign2), elseClause);

                // if 2 is assigned to the else block of if 1
                elseClause = _syntaxFactory.ElseClause(
                    SyntaxFactory.MakeToken(SyntaxKind.ElseKeyword),
                    MakeBlock(ifstmt));
                // if 1
                ifstmt = _syntaxFactory.IfStatement(
                            SyntaxFactory.MakeToken(SyntaxKind.IfKeyword),
                            SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                            condition1,
                            SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                            MakeBlock(assign1), elseClause);

                stmts.Add(ifstmt);
            }
            else
            {
                catchVar = null;
            }
            stmts.Add(context.StmtBlock.Get<BlockSyntax>());
            var catchClause = _syntaxFactory.CatchClause(
                SyntaxFactory.MakeToken(SyntaxKind.CatchKeyword),
                _syntaxFactory.CatchDeclaration(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    GenerateQualifiedName(SystemQualifiedNames.Exception),
                    catchVar,
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)),
                    null,
                    MakeBlock(stmts));

            context.Put(catchClause);
            _pool.Free(stmts);
        }
        #endregion

        #region Return statements

        private bool NeedsReturn(IList<XP.StatementContext> stmts)
        {
            // This code checks only the last statement. When there is a return or throw
            // on another line then the system will report 'Unreachable code' anyway.
            if (stmts.Count == 0)
                return true;
            var stmt = stmts.Last();
            if (stmt is XP.ReturnStmtContext)
            {
                return false;
            }
            if (stmt is XP.JumpStmtContext)
            {
                var jmpstmt = stmt as XP.JumpStmtContext;
                if (jmpstmt.Key.Type == XP.THROW)
                    return false;
                if (jmpstmt.Key.Type == XP.BREAK)
                    return false;
            }
            if (stmt is XP.IfStmtContext)
            {
                var ifstmt = stmt as XP.IfStmtContext;
                var ifelsestmt = ifstmt.IfStmt;
                var elsestmt = ifelsestmt?.ElseBlock;
                // The first ifelsestmt should always have a value, but better safe than sorry
                // process to the end of the list
                // when there is no else, then we need a break
                // otherwise process every statement list
                while (ifelsestmt != null)                     //
                {
                    if (NeedsReturn(ifelsestmt.StmtBlk._Stmts))
                    {
                        return true;
                    }
                    elsestmt = ifelsestmt.ElseBlock;
                    ifelsestmt = ifelsestmt.ElseIfBlock;
                }
                // No Else, so there is at least one block that does not end with a RETURN etc
                if (elsestmt == null || elsestmt._Stmts?.Count == 0)
                {
                    return true;
                }
                else
                {
                    return NeedsReturn(elsestmt._Stmts);
                }
            }
            if (stmt is XP.CaseStmtContext)
            {
                var docasestmt = stmt as XP.CaseStmtContext;
                var casestmt = docasestmt.CaseStmt;     // CaseBlock, there may be no blocks at all.
                int lastkey = XP.CASE;
                while (casestmt != null)                // otherwise is also a CaseBlock stored in NextCase
                {
                    if (NeedsReturn(casestmt.StmtBlk._Stmts))
                        return true;
                    lastkey = casestmt.Key.Type;
                    casestmt = casestmt.NextCase;
                }
                if (lastkey == XP.CASE) // There is no otherwise
                    return true;
                return false;           // all branches end with a return  statement
            }
            if (stmt is XP.SwitchStmtContext)
            {
                var swstmt = stmt as XP.SwitchStmtContext;
                bool hasdefault = false;
                foreach (var swBlock in swstmt._SwitchBlock)
                {
                    if (swBlock.StmtBlk._Stmts.Count > 0 && NeedsReturn(swBlock.StmtBlk._Stmts))
                        return true;
                    if (swBlock.Key.Type != XP.CASE)
                        hasdefault = true;
                }
                if (!hasdefault)                    return true;
                return false;           // all branches end with a return statement
            }

            if (stmt is XP.BlockStmtContext)
            {
                var blockstmt = stmt as XP.BlockStmtContext;
                return NeedsReturn(blockstmt.StmtBlk._Stmts);
            }
            if (stmt is XP.ILoopStmtContext)        // For, Foreach, While, Repeat
            {
                var blockstmt = stmt as XP.ILoopStmtContext;
                return NeedsReturn(blockstmt.Statements._Stmts);
            }
            if (stmt is XP.TryStmtContext)
            {
                var trystmt = stmt as XP.TryStmtContext;
                // no finally check each of the blocks
                if (NeedsReturn(trystmt.StmtBlk._Stmts))
                    return true;
                if (trystmt._CatchBlock?.Count == 0)
                    return true;
                foreach (var cb in trystmt._CatchBlock)
                {
                    // if one of the catches has no return then we need to add a return
                    if (NeedsReturn(cb.StmtBlk._Stmts))
                        return true;
                }
                // all catch blocks are terminated
                return false;
            }
            if (stmt is XP.SeqStmtContext)
            {
                var seqstmt = stmt as XP.SeqStmtContext;
                if (NeedsReturn(seqstmt.StmtBlk._Stmts))
                    return true;
                if (seqstmt.RecoverBlock == null)
                    return true;
                return NeedsReturn(seqstmt.RecoverBlock.StmtBlock._Stmts);
            }
            return true;
        }
        private ExpressionSyntax GetReturnExpression(TypeSyntax returnType)
        {
            ExpressionSyntax result = null;
            if (returnType is PredefinedTypeSyntax)
            {
                var pretype = returnType as PredefinedTypeSyntax;
                switch (pretype.keyword.Kind)
                {
                    case SyntaxKind.VoidKeyword:
                        return null;
                    default:
                        result = MakeDefault(returnType);
                        break;
                }
            }
            else
            {
                if (returnType is QualifiedNameSyntax)
                {
                    var qns = returnType as QualifiedNameSyntax;
                    // System.Void
                    var sName  = qns.ToFullString().Replace(" ", "");
                    var v1 = GenerateQualifiedName(SystemQualifiedNames.Void1).ToFullString().Replace(" ", "");
                    var v2 = GenerateQualifiedName(SystemQualifiedNames.Void2).ToFullString().Replace(" ", "");
                    if (sName.Equals(v1, StringComparison.OrdinalIgnoreCase))
                    {
                        return null;
                    }
                    // global::System.Void
                    if (sName.Equals(v2, StringComparison.OrdinalIgnoreCase))
                    {
                        return null;
                    }
                }
                if (returnType == _pszType || returnType == _symbolType)
                {
                    result = CreateObject(returnType, MakeArgumentList(MakeArgument(GenerateLiteral(""))));
                }
                else
                {
                    // other types all return a default expression
                    // This includes USUAL, DATE, ARRAY, STRING, FLOAT etc
                    result = MakeDefault(returnType);
                }
            }
            return result;
        }

        protected override BlockSyntax AddMissingReturnStatement(BlockSyntax body, XP.StatementBlockContext stmtBlock, TypeSyntax returnType)
        {
            if (_options.VOAllowMissingReturns && stmtBlock != null && NeedsReturn(stmtBlock._Stmts))
            {
                var result = GetReturnExpression(returnType);
                if (result != null) // this happens for the Void Type
                {
                    var statements = _pool.Allocate<StatementSyntax>();
                    statements.AddRange(body.Statements);
                    statements.Add(GenerateReturn(result));

                    body = MakeBlock(statements).WithAdditionalDiagnostics(
                                new SyntaxDiagnosticInfo(ErrorCode.WRN_MissingReturnStatement));
                    _pool.Free(statements);

                }
            }
            return body;
        }


        public override void ExitReturnStmt([NotNull] XP.ReturnStmtContext context)
        {
            context.SetSequencePoint(context.end);
            var expr = context.Expr?.Get<ExpressionSyntax>();
            // when / vo9 is enabled then add missing Expression
            var ent = CurrentEntity;
            ErrorCode errcode = (ErrorCode)0;
            
            if (ent != null)
            {
                // INIT and AXIT methods can not return values
                // Allow RETURN VOID
                if (context.Void != null && !ent.Data.MustBeVoid)
                {
                    expr = GenerateLiteral(0);
                    errcode = ErrorCode.WRN_MissingReturnValue;
                }
                if (expr == null && _options.VOAllowMissingReturns && !ent.Data.MustBeVoid)
                {
                    errcode = ErrorCode.WRN_MissingReturnValue;
                    if (ent is XP.MethodContext || ent is XP.FunctionContext || ent is XP.PropertyAccessorContext)
                    {
                        TypeSyntax dataType;
                        if (ent.Data.HasMissingReturnType)
                        {
                            dataType = _getMissingType();
                        }
                        else
                        {
                            if (ent is XP.MethodContext)
                                dataType = ((XP.MethodContext)ent).Type.Get<TypeSyntax>();
                            else if (ent is XP.FunctionContext)
                                dataType = ((XP.FunctionContext)ent).Type.Get<TypeSyntax>();
                            else if (ent is XP.PropertyAccessorContext)
                                dataType = ((XP.PropertyContext)ent.Parent).Type.Get<TypeSyntax>();
                            else
                                dataType = _getMissingType();
                        }
                        // calculate a new return value with a warning
                        expr = GetReturnExpression(dataType);
                    }
                }
                if (ent.Data.MustBeVoid && expr != null)
                {
                    // we cannot simply create an expression statement. Some expressions are not allowed as statement
                    // for example
                    // RETURN SELF:Field
                    // We change that to
                    // VAR Xs$Return := SELF:Field
                    // RETURN

                    if (ent.Data.IsInitAxit && context.Expr is XP.PrimaryExpressionContext && context.Expr.GetChild(0) is XP.SelfExpressionContext)
                    {
                        // allow return SELF and ignore SELF
                        context.Put(GenerateReturn(null));
                    }
                    else
                    {
                        errcode = ErrorCode.WRN_NoReturnValueAllowed;
                        if (context.Expr.GetLiteralToken() == null) // no  literal so we must evaluate the expression
                        {
                            var declstmt = GenerateReturnVar( _impliedType, expr);
                            var retstmt = GenerateReturn(null);
                            var block = MakeBlock(MakeList<StatementSyntax>(declstmt, retstmt));
                            context.Put(block);
                        }
                        else
                        {
                            var stmt = GenerateReturn(null);
                            context.Put(stmt);
                        }
                    }
                }
                else
                {
                    var stmt = GenerateReturn(expr);
                    context.Put(stmt);
                }
                if ((int)errcode != 0)
                {
                    var stmt = context.CsNode as StatementSyntax;
                    stmt = stmt.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(errcode));
                    context.Put(stmt);
                }

            }

        }
        #endregion

        #region Parameters
        private AttributeSyntax MakeDefaultParameter(ExpressionSyntax arg1, ExpressionSyntax arg2)
        {
            var args = MakeSeparatedList(
                        _syntaxFactory.AttributeArgument(null, null, arg1),
                        _syntaxFactory.AttributeArgument(null, null, arg2)
                        );
            var arglist = MakeAttributeArgumentList(args);

            var attr = _syntaxFactory.Attribute(GenerateQualifiedName(_defaultParameterType), arglist);
            return attr;
        }
        protected override TypeSyntax GetExpressionType(XP.ExpressionContext expr, ref bool isConst)
        {
            var token = expr.GetLiteralToken();
            isConst = false;
            if (token != null)
            {
                // Try to imply the type from the expression
                switch (token.Type)
                {
                    case XP.NIL:
                        return _usualType;
                    case XP.NULL_ARRAY:
                        return _arrayType;
                    case XP.NULL_DATE:
                    case XP.DATE_CONST:
                        return _dateType;
                    case XP.NULL_SYMBOL:
                    case XP.SYMBOL_CONST:
                        return _symbolType;

                }

            }
            return base.GetExpressionType(expr, ref isConst);
        }
        private XP.LiteralExpressionContext GetLiteralExpression(XP.ExpressionContext expr)
        {
            if (expr is XP.PrimaryExpressionContext)
            {
                if (expr.GetChild(0) is XP.LiteralExpressionContext lit )
                {
                    return lit;
                }
                if (expr.GetChild(0) is XP.ParenExpressionContext paren)
                {
                    var parExpr = expr.GetChild(0) as XP.ParenExpressionContext;
                    return GetLiteralExpression(parExpr.Expr);
                }
            }
            return null;
        }
        private AttributeSyntax EncodeDefaultParameter(XP.ExpressionContext initexpr)
        {
            bool negative = false;
            if (initexpr is XP.PrefixExpressionContext)
            {
                var prefix = initexpr as XP.PrefixExpressionContext;
                if (prefix.Op.Type == XP.PLUS || prefix.Op.Type == XP.MINUS)
                {
                    initexpr = prefix.Expr;
                    negative = prefix.Op.Type == XP.MINUS;
                }
            }
            var token = initexpr.GetLiteralToken();
            if (token == null)
            {
                return MakeDefaultParameter(initexpr.Get<ExpressionSyntax>(), GenerateLiteral(0));
            }
            else
            
            {
                var nullExpr = GenerateLiteralNull();
                switch (token.Type)
                {
                    case XP.NIL:
                        return MakeDefaultParameter(GenerateLiteral(0L), GenerateLiteral(1));               // 1 = NIL
                    case XP.NULL_DATE:
                        return MakeDefaultParameter(GenerateLiteral(0L), GenerateLiteral(2));               // 2 = Date
                    case XP.DATE_CONST:
                        DateTime dt;
                        int[] elements = DecodeDateConst(token.Text);
                        if (elements != null)
                            dt = new DateTime(elements[0], elements[1], elements[2]);
                        else
                            dt = new DateTime(0L);
                        return MakeDefaultParameter(GenerateLiteral(dt.Ticks), GenerateLiteral(2));    // 2 = Date, value in ticks
                    case XP.NULL_SYMBOL:
                        return MakeDefaultParameter(nullExpr, GenerateLiteral(3));                      // 3 = Symbol, value is empty
                    case XP.SYMBOL_CONST:
                        var symvalue = token.Text.Substring(1);
                        return MakeDefaultParameter(GenerateLiteral(symvalue), GenerateLiteral(3));      // 3 = Symbol, value is a string
                    case XP.NULL_PSZ:
                        return MakeDefaultParameter(nullExpr, GenerateLiteral(4));                       // 4 = PSZ, null = empty
                    case XP.NULL_PTR:
                        return MakeDefaultParameter(GenerateLiteral(0L), GenerateLiteral(5));            // 5 = IntPtr
                    case XP.NULL_STRING:
                        if (_options.VONullStrings)
                        {
                            return MakeDefaultParameter(GenerateLiteral(""), GenerateLiteral(0));               // 0 = regular .Net Value
                        }
                        else
                        {
                            return MakeDefaultParameter(nullExpr, GenerateLiteral(0));                          // 0 = regular .Net Value
                        }
                    case XP.NULL_ARRAY:
                    case XP.NULL_OBJECT:
                    case XP.NULL_CODEBLOCK:
                        return MakeDefaultParameter(nullExpr, GenerateLiteral(0));                          // 0 = regular .Net Value
                    case XP.INT_CONST:
                        if (negative)
                        {
                            Int64 iValue = Int64.Parse(token.Text) * -1;
                            return MakeDefaultParameter(GenerateLiteral(iValue), GenerateLiteral(0));   // 0 = regular .Net Value
                        }
                        else
                            return MakeDefaultParameter(GenerateLiteral(token), GenerateLiteral(0));   // 0 = regular .Net Value
                    case XP.REAL_CONST:
                        double dValue;
                        switch (token.Text.Last())
                        {
                            case 'M':
                            case 'm':
                            case 'S':
                            case 's':
                            case 'D':
                            case 'd':
                                dValue = double.Parse(token.Text.Substring(0, token.Text.Length - 1), System.Globalization.CultureInfo.InvariantCulture);
                                break;
                            default:
                                dValue = double.Parse(token.Text, System.Globalization.CultureInfo.InvariantCulture);
                                break;
                        }
                        if (negative)
                            return MakeDefaultParameter(GenerateLiteral(dValue * -1), GenerateLiteral(0));   // 0 = regular .Net Value
                        else
                            return MakeDefaultParameter(GenerateLiteral(dValue), GenerateLiteral(0));   // 0 = regular .Net Value

                    default:
                        return MakeDefaultParameter(GenerateLiteral(token), GenerateLiteral(0));   // 0 = regular .Net Value
                }
            }
        }

        public override void ExitParameter([NotNull] XP.ParameterContext context)
        {
            base.ExitParameter(context);
            // Only apply the vulcan default parameter attribute when there
            // are no Attributes on the parameter, such as [CallerMember]
            if (context.Default != null && context.Attributes == null && ! _options.NoClipCall)
            {
                /*
                // only encode when there are parameters in this list after the current one
                // that have no default. Otherwise we let Roslyn take care of it.
                var ParamList = context.Parent as XP.ParameterListContext;
                var needed = false;
                var found = false;
                foreach (var param in ParamList._Params)
                {
                    if (param == context)
                    {
                        found = true;
                    }
                    else if (found && param.Default == null)
                    {
                        needed = true;
                    }
                }
                if (!needed)
                {
                    // Roslyn can handle it.
                    return;
                }
                }*/
                AttributeSyntax attr = EncodeDefaultParameter(context.Default);
                if (attr != null)
                {
                    ParameterSyntax par = context.Get<ParameterSyntax>();
                    var alist = par.AttributeLists;
                    var id = par.Identifier;
                    var type = par.Type;
                    var mod = par.Modifiers;
                    var attributeLists = _pool.Allocate<AttributeListSyntax>();
                    foreach (var attrib in alist)
                    {
                        attributeLists.Add(attrib);
                    }
                    var attrs = _pool.AllocateSeparated<AttributeSyntax>();
                    attrs.Add(attr);

                    var atlist = MakeAttributeList(
                        target: null,
                        attributes: attrs);
                    attributeLists.Add(atlist);
                    _pool.Free(attrs);
                    context.Put(_syntaxFactory.Parameter(attributeLists, mod, type, id, null));
                    _pool.Free(attributeLists);
                }
            }
        }
        protected AttributeListSyntax GetActualArgs()
        {
            if (_actualArgs == null)
            {
                lock (gate)
                {
                    if (_actualArgs == null)
                    {
                        var arguments = MakeSeparatedList(
                            _syntaxFactory.AttributeArgument(null, null, MakeTypeOf(_pszType)));

                        var attribute = _syntaxFactory.Attribute(
                                        name: GenerateQualifiedName(_actualType),argumentList: MakeAttributeArgumentList(arguments));
                        var attributes = MakeSeparatedList(attribute);

                        _actualArgs = MakeAttributeList(
                            target: null,
                            attributes: attributes);

                    }

                }
            }
            return _actualArgs;
        }
        internal override ParameterListSyntax UpdateVODLLParameters(ParameterListSyntax parameters)
        {
            // real work implemented in the subclass to check for PSZ parameters
            bool hasPsz = false;
            for (int i = 0; i < parameters.Parameters.Count; i++)
            {
                var p = parameters.Parameters[i];
                if (p.Type == _pszType)
                {
                    hasPsz = true;
                    break;
                }
            }
            if (hasPsz)
            {

                var @params = _pool.AllocateSeparated<ParameterSyntax>();
                for (int i = 0; i < parameters.Parameters.Count; i++)
                {
                    if (@params.Count > 0)
                        @params.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                    var p = parameters.Parameters[i];
                    if (p.Type != _pszType)
                    {
                        @params.Add(p);
                    }
                    else
                    {
                        var newParam = _syntaxFactory.Parameter(
                            attributeLists: GetActualArgs(),
                            modifiers: p.Modifiers,
                            type: _ptrType,
                            identifier: p.Identifier,
                            @default: p.Default
                            );
                        @params.Add(newParam);
                    }
                }
                parameters = _syntaxFactory.ParameterList(
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                @params,
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
                _pool.Free(@params);
            }
            return parameters;
        }

        #endregion

        #region PCALL
        protected MethodDeclarationSyntax CreatePCallFunction()
        {
            var id = SyntaxFactory.Identifier(XSharpSpecialNames.PCallProc);
            var p = SyntaxFactory.Identifier("p");
            var t = SyntaxFactory.Identifier("T");
            // body
            var tname = GenerateSimpleName("T");
            var arg1 = MakeArgument(GenerateSimpleName("p"));
            var arg2 = MakeArgument(MakeTypeOf(tname));
            var args = MakeArgumentList(arg1, arg2);
            ExpressionSyntax expr = GenerateMethodCall(SystemQualifiedNames.GetDelegate, args);
            expr = MakeCastTo(_objectType, expr);
            expr = MakeCastTo(tname, expr);
            var stmt = GenerateReturn(expr);
            var block = MakeBlock(stmt);
            var tparameters = _pool.AllocateSeparated<TypeParameterSyntax>();
            tparameters.Add(_syntaxFactory.TypeParameter(null, null, t));
            var typeparams = _syntaxFactory.TypeParameterList(SyntaxFactory.MakeToken(SyntaxKind.LessThanToken),
                tparameters,
                SyntaxFactory.MakeToken(SyntaxKind.GreaterThanToken));
            _pool.Free(tparameters);
            var mods = TokenList(SyntaxKind.InternalKeyword, SyntaxKind.StaticKeyword);
            var @params = _pool.AllocateSeparated<ParameterSyntax>();
            @params.Add(_syntaxFactory.Parameter(null, null, _ptrType, p, null));
            var pars = _syntaxFactory.ParameterList(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken), @params, SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
            _pool.Free(@params);
            var m = SyntaxFactory.MethodDeclaration(MakeCompilerGeneratedAttribute(), mods,
                tname, /*explicitif*/null,
                id, typeparams, pars,/* constraints*/null, block,/*exprbody*/null,
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            return m;
        }

        private bool AddPCallDelegate(XP.MethodCallContext context, string prefix, TypeSyntax type)
        {
            // This method generates a delegate and adds it to the current class
            // the XNode for the delegate points back to the PCALL or PCALLNATIVE in the ANtlr Parse Tree
            // At this stage the parameters are generated based on the # of parameters in the call
            // They are of type OBJECT for now
            // The first argument is not used since this is used to create the delegate
            var entity = CurrentEntity;
            var name = prefix;
            if (entity != null)
            {
                name += "$" + entity.ShortName;
            }
            name += UniqueNameSuffix;
            if (context.ArgList == null || context.ArgList?._Args.Count < 1)
            {
                context.Put(GenerateLiteral(0).WithAdditionalDiagnostics(
                    new SyntaxDiagnosticInfo(ErrorCode.ERR_BadArgCount, context.Expr.GetText(), 0)));
                return false;
            }
            // construct fake parameter list, all of type object
            var @params = _pool.AllocateSeparated<ParameterSyntax>();
            var atts = EmptyList<AttributeListSyntax>();
            var mods = EmptyList<SyntaxToken>();
            for (int i = 1; i < context.ArgList._Args.Count; i++)
            {
                var pname = SyntaxFactory.Identifier("$param" + i.ToString());
                var param = _syntaxFactory.Parameter(atts, mods, _objectType, pname, null);
                //param.XNode = context.ArgList._Args[i]; // link the parameter to the argument value
                if (i > 1)
                    @params.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                @params.Add(param);
            }
            var paramList = _syntaxFactory.ParameterList(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                                @params, SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
            _pool.Free(@params);
            var id = SyntaxFactory.Identifier(name);
            mods = TokenList(SyntaxKind.InternalKeyword);
            MemberDeclarationSyntax m = _syntaxFactory.DelegateDeclaration(
               MakeCompilerGeneratedAttribute(),
               mods,
               delegateKeyword: SyntaxFactory.MakeToken(SyntaxKind.DelegateKeyword),
               returnType: type,
               identifier: id,
               typeParameterList: null,
               parameterList: paramList,
               constraintClauses: null,
               semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            m.XNode = context; // link the Delegate to the calling code 
            ClassEntities.Peek().Members.Add(m);    // add to current class
            // Now change the context and create the call to the delegate
            return GeneratePCallDelegateCall(context, name);
        }
        private bool GeneratePCallDelegateCall(XP.MethodCallContext context, string name)
        {
            // This method changes the PCALL(hFunc, a,b,c) call
            // and converts it to a delegate call
            // hFunc = 1st argument
            // a,b,c etc are rest of the arguments
            // we assume that the # of parameters has been checked before
            GlobalEntities.HasPCall = true;
            var argname = GenerateQualifiedName(name);
            var methodName = MakeGenericName(XSharpSpecialNames.PCallProc, argname);
            var marshallargs = MakeArgumentList(context.ArgList._Args[0].Get<ArgumentSyntax>());
            // expr = Pcall$GetDelegate< <delegatename> >(hFunc)
            var expr = _syntaxFactory.InvocationExpression(methodName, marshallargs);
            expr.XPCall = true;         // This is used in the binder to locate the special generated invocation call
            //
            // Create the argumentlist that is passed to the delegate
            var delarglist = new List<ArgumentSyntax>();
            for (int i = 1; i < context.ArgList._Args.Count; i++)
            {
                delarglist.Add(context.ArgList._Args[i].Get<ArgumentSyntax>());
            }
            // expr = Delegate (a,b,c) 
            expr = _syntaxFactory.InvocationExpression(expr, MakeArgumentList(delarglist.ToArray()));
            context.Put(expr);
            return true;
        }
        private ExpressionSyntax GenerateLiteralSymbol(string symbol)
        {
            //remove the # from the string
            symbol = symbol.Substring(1);
            var expr = CreateObject(_symbolType, MakeArgumentList(MakeArgument(GenerateLiteral(symbol.ToUpper()))));
            if (_options.MacroScript)
                return expr;
            var lsym = "_"+symbol.ToLower();
            if (!_literalSymbols.ContainsKey(lsym))
            {
                // create field declarator with inline assignment
                // INTERNAL STATIC INITONLY _symbol := __Symbol{"SYMBOL"} AS __Symbol
                var init = _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken), expr);
                var vars = _syntaxFactory.VariableDeclarator(SyntaxFactory.MakeIdentifier(lsym), EmptyBracketedArgumentList(), init);
                var fielddecl = _syntaxFactory.FieldDeclaration(
                                            default(SyntaxList<AttributeListSyntax>),
                                            TokenList(SyntaxKind.InternalKeyword, SyntaxKind.StaticKeyword, SyntaxKind.ReadOnlyKeyword),
                                            _syntaxFactory.VariableDeclaration(_symbolType, MakeSeparatedList(vars)), 
                                            SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                _literalSymbols.Add(lsym, fielddecl);
            }
            var name = MakeSimpleMemberAccess(GenerateSimpleName(XSharpSpecialNames.SymbolTable), GenerateSimpleName(lsym));
            return name;
        }

        private bool GenerateLiteralPsz(XP.ExpressionContext context, out ExpressionSyntax expr)
        {
            //remove the quotes from the string
            expr = null;
            if (!context.IsLiteralExpression())
                return false;
            expr = context.Get<ExpressionSyntax>();
            var args = MakeArgumentList(MakeArgument(expr));
            expr = CreateObject(_pszType, args);
            expr.XGenerated = true;
            expr.XNode = context;
            if (_options.MacroScript)
                return false;
            var str = context.GetText();
            string fieldname = null;
            foreach (var pair in _literalPSZs)
            {
                if (String.Compare(pair.Value.Item1, str) == 0)
                {
                    fieldname = pair.Key;
                    break;
                }
            }
            if (String.IsNullOrEmpty(fieldname))
            {
                fieldname = "_$psz_" + UniqueNameSuffix;
                // create field declarator with inline assignment
                // INTERNAL STATIC INITONLY _psz := Psz{"value"} AS __PSZ
                var init = _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken), expr);
                init.XGenerated = true;
                init.XNode = context;
                var vars = _syntaxFactory.VariableDeclarator(SyntaxFactory.MakeIdentifier(fieldname), EmptyBracketedArgumentList(), init);
                vars.XGenerated = true;
                vars.XNode = context;

                var fielddecl = _syntaxFactory.FieldDeclaration(
                                            default(SyntaxList<AttributeListSyntax>),
                                            TokenList(SyntaxKind.InternalKeyword, SyntaxKind.StaticKeyword, SyntaxKind.ReadOnlyKeyword),
                                            _syntaxFactory.VariableDeclaration(_pszType, variables: MakeSeparatedList(vars)),
                                            SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                fielddecl.XNode = context;
                fielddecl.XGenerated = true;
                _literalPSZs.Add(fieldname, new Tuple<string, FieldDeclarationSyntax>(str, fielddecl));
            }
            expr = MakeSimpleMemberAccess(GenerateSimpleName(XSharpSpecialNames.PSZTable), GenerateSimpleName(fieldname));
            expr = expr.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.WRN_CompilerGeneratedPSZConversionGeneratesMemoryleak));
            return true;
        }

   
        private bool GeneratePCall(XP.MethodCallContext context)
        {
            // Return type and parameters should match the method prototype that the first parameter
            // For now we default to a return type of _objectType
            // points to. This is resolved in the binder and rewriter
            return AddPCallDelegate(context, XSharpSpecialNames.PCallPrefix, _objectType);
        }
        private bool GeneratePCallNative(XP.MethodCallContext context)
        {
            // return type is specified in 1st generic parameter
            // other parameters are derived from the types of the actual parameters
            // this is resolved in the binder and rewriter
            var expr = context.Expr.Get<ExpressionSyntax>();
            var gns = expr as GenericNameSyntax;
            // Check that # of generic parameters is exactly 1
            if (gns.TypeArgumentList.Arguments.Count != 1)
            {
                expr = GenerateLiteral(0).WithAdditionalDiagnostics(
                    new SyntaxDiagnosticInfo(ErrorCode.ERR_PCallNativeGenericType, gns.Identifier.Text));
                context.Put(expr);
                return true;
            }
            TypeSyntax arg = gns.TypeArgumentList.Arguments[0];
            return AddPCallDelegate(context, XSharpSpecialNames.PCallNativePrefix, arg);
        }
        #endregion
 
        #region Method Calls and special methods
        public override void ExitMethodCall([NotNull] XP.MethodCallContext context)
        {
            var expr = context.Expr.Get<ExpressionSyntax>();
            string name = null;
            if (expr is IdentifierNameSyntax)
            {
                // Intrinsic functions that depend on Vulcan types
                IdentifierNameSyntax ins = expr as IdentifierNameSyntax;
                name = ins.Identifier.Text.ToUpper();
                switch (name)
                {
                    case "PCALL":
                    case "CCALL":
                        if (GeneratePCall(context))
                            return;
                        break;
                    case "PCALLNATIVE":
                    case "CCALLNATIVE":
                        expr = GenerateLiteral(0).WithAdditionalDiagnostics(
                            new SyntaxDiagnosticInfo(ErrorCode.ERR_PCallNativeGenericType, ins.Identifier.Text));
                        context.Put(expr);
                        return;
                    case "SLEN":
                        if (GenerateSLen(context))
                            return;
                        break;
                    case "STRING2PSZ":
                    case "CAST2PSZ":
                        if (GenerateString2Psz(context, name))
                            return;
                        break;
                    case "PCOUNT":
                    case "ARGCOUNT":
                    case "_GETMPARAM":
                    case "_GETFPARAM":
                        if (CurrentEntity != null && CurrentEntity.Data.HasClipperCallingConvention)
                        {
                            if (GenerateClipCallFunc(context, name))
                                return;
                        }
                        expr = GenerateLiteral("", 0).WithAdditionalDiagnostics(
                            new SyntaxDiagnosticInfo(ErrorCode.ERR_OnlySupportedForClipperCallingConvention, ins.Identifier.Text));
                        context.Put(expr);
                        return;
                    default:
                        break;
                }
            }
            else if (expr is GenericNameSyntax)
            {
                var gns = expr as GenericNameSyntax;
                name = gns.Identifier.Text.ToUpper();
                switch (name)
                {
                    case "PCALLNATIVE":
                        if (GeneratePCallNative(context))
                            return;
                        break;
                }
            }
            else if (expr is ThisExpressionSyntax || expr is BaseExpressionSyntax)
            {
                // SUPER(..) and SELF(..)
                expr = MakeSimpleMemberAccess(expr, _syntaxFactory.IdentifierName(SyntaxFactory.Identifier(".ctor")));
                ArgumentListSyntax argList;
                if (context.ArgList != null)
                {
                    argList = context.ArgList.Get<ArgumentListSyntax>();
                }
                else
                {
                    argList = EmptyArgumentList();
                }
                context.Put(_syntaxFactory.InvocationExpression(expr, argList));
                return;
            }
            else if (_options.VoInitAxitMethods && expr is MemberAccessExpressionSyntax)
            {
                var mac = expr as MemberAccessExpressionSyntax;
                var mName = mac.Name as IdentifierNameSyntax;
                string methodName = mName?.Identifier.Text;
                if (string.Equals(methodName, "init", StringComparison.OrdinalIgnoreCase))
                {
                    var mExpr = mac.Expression;
                    if (mExpr is ThisExpressionSyntax || mExpr is BaseExpressionSyntax)
                    {
                        expr = MakeSimpleMemberAccess(mExpr, GenerateSimpleName(".ctor"));
                        ArgumentListSyntax argList;
                        if (context.ArgList != null)
                        {
                            argList = context.ArgList.Get<ArgumentListSyntax>();
                        }
                        else
                        {
                            argList = EmptyArgumentList();
                        }
                        context.Put(_syntaxFactory.InvocationExpression(expr, argList));
                        return;

                    }
                }
                else if (string.Equals(methodName, "axit", StringComparison.OrdinalIgnoreCase))
                {
                    var stmt = _syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                    stmt = stmt.WithAdditionalDiagnostics(
                        new SyntaxDiagnosticInfo(ErrorCode.WRN_SuppressAxitMethodCall));
                    context.Put(stmt);
                    return;
                }
            }
            // all other method names or syntaxes
            base.ExitMethodCall(context);
            return;
        }
        private bool GenerateSLen(XP.MethodCallContext context)
        {
            // Pseudo function SLen
            ArgumentListSyntax argList;
            ExpressionSyntax expr;
            if (context.ArgList != null)
            {
                argList = context.ArgList.Get<ArgumentListSyntax>();
            }
            else
            {
                return false;
            }

            expr = MakeCastTo(_stringType, argList.Arguments[0].Expression);
            expr = _syntaxFactory.ConditionalAccessExpression(expr,
                                    SyntaxFactory.MakeToken(SyntaxKind.QuestionToken),
                                    _syntaxFactory.MemberBindingExpression(
                                        SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                                        GenerateSimpleName("Length")
                                        ));
            expr = MakeSimpleMemberAccess(expr, GenerateSimpleName("Value"));
            expr = MakeCastTo(_syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.UIntKeyword)), expr);

            context.Put(expr);
            return true;
        }
        #endregion

        #region Entities and Clipper CC and PSZ support
        public AttributeListSyntax MakeClipperCallingConventionAttribute(List<ExpressionSyntax> names)
        {
            InitializeArrayTypes();
            return MakeAttributeList(
                                    target: null,
                                    attributes: MakeSeparatedList(_syntaxFactory.Attribute(
                                        name: GenerateQualifiedName(_clipperCallingConvention),
                                        argumentList: MakeAttributeArgumentList(
                                            MakeSeparatedList(
                                                _syntaxFactory.AttributeArgument(null, null,
                                                    _syntaxFactory.ArrayCreationExpression(
                                                        SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                                                        arrayOfString,
                                                        _syntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression,
                                                            SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                                                            MakeSeparatedList<ExpressionSyntax>(names.ToArray()),
                                                            SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken)))))
                                            ))
                                    ));
        }

        protected ParameterListSyntax GetClipperParameters()
        {
            if (_clipperParams == null)
            {
                lock (gate)
                {
                    if (_clipperParams == null)
                    {
                        InitializeArrayTypes();
                        SyntaxListBuilder modifiers = _pool.Allocate();
                        modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.ParamsKeyword));
                        var attrs = _pool.Allocate<AttributeListSyntax>();
                        var par = _syntaxFactory.Parameter(
                                        MakeCompilerGeneratedAttribute(),
                                        modifiers.ToList(),
                                        type: arrayOfUsual,
                                        identifier: SyntaxFactory.Identifier(XSharpSpecialNames.ClipperArgs),
                                        @default: null);
                        _clipperParams = _syntaxFactory.ParameterList(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                            MakeSeparatedList(par),
                            SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
                        _pool.Free(attrs);
                        _pool.Free(modifiers);
                    }
                }
            }
            return _clipperParams;
        }
        protected void Check4ClipperCC(XP.IEntityContext context, IList<XP.ParameterContext> parameters, IToken Convention, XP.DatatypeContext returnType)
        {
            bool isEntryPoint = false;
            bool hasConvention = false;
            if (context is XP.FunctionContext)
            {
                var fc = context as XP.FunctionContext;
                isEntryPoint = fc.Id.GetText().ToLower() == "start";
            }
            context.Data.HasMissingReturnType = (returnType == null);
            context.Data.HasTypedParameter = false;
            if (context is XP.ProcedureContext)
            {
                context.Data.MustBeVoid = true;
            }
            else if (!context.Data.HasMissingReturnType)
            {
                string rtype = returnType.GetText().ToLower();
                if (rtype == "void" || rtype == "system.void")
                {
                    context.Data.MustBeVoid = true;
                }

            }
            if (Convention != null)
            {
                context.Data.HasClipperCallingConvention = (Convention.Type == XP.CLIPPER);
                hasConvention = true;
            }
            int paramCount = 0;
            if (parameters != null && parameters != null)
                paramCount = parameters.Count;
            // Function Foo or Function Foo() without convention
            if (paramCount == 0 && !hasConvention)
            {
                context.Data.HasClipperCallingConvention = _options.VOClipperCallingConvention && !isEntryPoint ;
            }
            if (paramCount > 0)
            {
                bool bHasTypedParameter = false;
                foreach (XP.ParameterContext par in parameters)
                {
                    if (par.Type != null || par.Self != null)
                    {
                        bHasTypedParameter = true;
                        break;
                    }
                }
                context.Data.HasTypedParameter = bHasTypedParameter;
                if (!context.Data.HasClipperCallingConvention && !isEntryPoint && !hasConvention && _options.VOUntypedAllowed)
                    context.Data.HasClipperCallingConvention = !bHasTypedParameter;
            }
        }

        internal GenericNameSyntax MakeGenericName(string name, TypeSyntax type)
        {
            return _syntaxFactory.GenericName(SyntaxFactory.MakeIdentifier(name),
                           _syntaxFactory.TypeArgumentList(
                               SyntaxFactory.MakeToken(SyntaxKind.LessThanToken),
                               MakeSeparatedList(type),
                               SyntaxFactory.MakeToken(SyntaxKind.GreaterThanToken)
                               ));
        }

        private void implementNoClipCall(XP.IEntityContext context, ref ParameterListSyntax parameters, ref TypeSyntax dataType)
        {
            if (!context.Data.HasClipperCallingConvention)
                return;
            if (context.Data.HasMissingReturnType)
                dataType = _usualType;
            if (parameters.Parameters.Count > 0)
            {
                // generate default parameter attribute to make sure that calling code will compile
                //var attr = MakeDefaultParameter(GenerateLiteral(0), GenerateLiteral(1));
                var attrs = _pool.AllocateSeparated<AttributeSyntax>();
                //attrs.Add(attr);
                var defExpr = _syntaxFactory.EqualsValueClause(
                    SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                    MakeDefault(_usualType));
                var attrlist = MakeList(MakeAttributeList(null, attrs));
                var @params = _pool.AllocateSeparated<ParameterSyntax>();
                for (int i = 0; i < parameters.Parameters.Count; i++)
                {
                    ParameterSyntax parm = parameters.Parameters[i];
                    var par = _syntaxFactory.Parameter(
                          attributeLists: attrlist,
                          modifiers: null,
                          type: _usualType,
                          identifier: parm.Identifier,
                          @default: defExpr);
                    if (i > 0)
                        @params.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                    @params.Add(par);
                }
                parameters = _syntaxFactory.ParameterList(
                        SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                        @params,
                        SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
                _pool.Free(attrs);
                _pool.Free(@params);
            }
        }
        protected override void ImplementClipperAndPSZ(XP.IEntityContext context,
            ref SyntaxList<AttributeListSyntax> attributes, ref ParameterListSyntax parameters, ref BlockSyntax body,
            ref TypeSyntax dataType)
        {
            InitializeArrayTypes();
            if (context.Data.HasTypedParameter && context.Data.HasClipperCallingConvention)
            {
                parameters = parameters.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_TypedParametersWithClipperCallingConvention));
                return;
            }
            if (context.Data.HasClipperCallingConvention || context.Data.UsesPSZ)
            {
                var stmts = _pool.Allocate<StatementSyntax>();

                if (context.Data.HasClipperCallingConvention && _options.NoClipCall)
                {
                    implementNoClipCall(context, ref parameters, ref dataType);
                    context.Data.HasClipperCallingConvention = false;
                }
                if (context.Data.HasClipperCallingConvention && ! _options.NoClipCall)
                {

                    // Assuming the parameters are called oPar1 and oPar2 then the following code is generated
                    // LOCAL oPar1 := iif(Xs$Args:Length > 0,  Xs$Args[0], NIL) as USUAL
                    // LOCAL oPar2 := iif(Xs$Args:Length > 1,  Xs$Args[1], NIL) as USUAL
                    if (body != null)
                    {
                        // Create the ClipperCallingConventionAttribute for the method/function
                        // using the names from the paramNames list
                        // [ClipperCallingConvention(new string[] { "a", "b" })]
                        // make sure that existing attributes are not removed!
                        var attrs = _pool.Allocate<AttributeListSyntax>();
                        attrs.AddRange(attributes); // Copy existing attributes
                        var names = new List<ExpressionSyntax>();
                        for (int i = 0; i < parameters.Parameters.Count; i++)
                        {
                            var parm = parameters.Parameters[i];
                            string name = parm.Identifier.Text;
                            names.Add(GenerateLiteral(name));
                        }
                        attrs.Add(MakeClipperCallingConventionAttribute(names));
                        attributes = attrs;
                        _pool.Free(attrs);

                        // create PCount variable
                        var clipperArgs = GenerateSimpleName(XSharpSpecialNames.ClipperArgs);
                        var argLen = MakeSimpleMemberAccess(clipperArgs, GenerateSimpleName("Length"));
                        var notnull = _syntaxFactory.BinaryExpression(
                                           SyntaxKind.NotEqualsExpression,
                                           clipperArgs,
                                           SyntaxFactory.MakeToken(SyntaxKind.ExclamationEqualsToken),
                                           GenerateLiteralNull());
                        var len = _syntaxFactory.ConditionalExpression(
                                           notnull,
                                           SyntaxFactory.MakeToken(SyntaxKind.QuestionToken),
                                           argLen,
                                           SyntaxFactory.MakeToken(SyntaxKind.ColonToken),
                                           GenerateLiteral(0));

                        var decl = GenerateLocalDecl(XSharpSpecialNames.ClipperPCount, _intType, len);
                        stmts.Add(decl);
                        if (parameters.Parameters.Count > 0)
                        {

                            for (int i = 0; i < parameters.Parameters.Count; i++)
                            {
                                var parm = parameters.Parameters[i];
                                string name = parm.Identifier.Text;
                                var defexpr = GenerateGetClipperParam(GenerateLiteral(i+1));
                                decl = GenerateLocalDecl(name, _usualType, defexpr);
                                decl.XGenerated = true;
                                var variable = decl.Declaration.Variables[0];
                                variable.XGenerated = true;
                                stmts.Add(decl);
                            }
                        }
                    }
                    // Now Change argument to X$Args PARAMS USUAL[]
                    parameters = GetClipperParameters();
                }
                if (body != null)
                {
                    FinallyClauseSyntax finallyClause = null;
                    if (context.Data.UsesPSZ)
                    {
                        // VAR Xs$PszList := List<IntPtr>{}
                        var listOfIntPtr = _syntaxFactory.QualifiedName(GenerateQualifiedName(SystemQualifiedNames.CollectionsGeneric),
                            SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                            MakeGenericName("List", _ptrType));
                        var expr = CreateObject(listOfIntPtr, EmptyArgumentList());
                        stmts.Add(GenerateLocalDecl(XSharpSpecialNames.VoPszList, _impliedType, expr));
                        finallyClause = _syntaxFactory.FinallyClause(
                            SyntaxFactory.MakeToken(SyntaxKind.FinallyKeyword),
                            MakeBlock(MakeList<StatementSyntax>(
                                GenerateExpressionStatement(
                                    GenerateMethodCall(
                                        _options.XSharpRuntime ? XSharpQualifiedFunctionNames.PszRelease : VulcanQualifiedFunctionNames.PszRelease,
                                        MakeArgumentList(MakeArgument(GenerateSimpleName(XSharpSpecialNames.VoPszList))),true)))));
                    }
                    // TRY
                    //    original body
                    // FINALLY                                             < == Always
                    //    CompilerServices.String2PszRelease(Xs$PszList)   < == only for HasPsz(), otherwise empty
                    //    And assign Byref params back to Xs$Args
                    // END TRY

                    var tryStmt = finallyClause == null ? (StatementSyntax)body : // nvk: It doesn't hurt to optimize the tree (and avoid unnecessary diagnostics...)
                        _syntaxFactory.TryStatement(
                        SyntaxFactory.MakeToken(SyntaxKind.TryKeyword),
                        body,
                        null,
                        finallyClause);
                    stmts.Add(tryStmt);
                    body = MakeBlock(stmts);
                }
                _pool.Free(stmts);
            }
            // Add missing return type when needed. OBJECT or USUAL depending on the dialect.
            if (context.Data.HasMissingReturnType && !context.Data.MustBeVoid)
            {
                dataType = _getMissingType();
            }
        }

        private ExpressionSyntax _GenerateString2Psz(XSharpParserRuleContext context, ExpressionSyntax expr)
        {
            if (CurrentEntity != null )
            {
                CurrentEntity.Data.UsesPSZ = true;
                NameSyntax pszlist = GenerateSimpleName(XSharpSpecialNames.VoPszList);
                var argList = MakeArgumentList(MakeArgument(expr), MakeArgument(pszlist));
                expr = GenerateMethodCall(
                    _options.XSharpRuntime ? XSharpQualifiedFunctionNames.String2Psz : VulcanQualifiedFunctionNames.String2Psz, 
                    argList,true);
                var args = MakeArgumentList(MakeArgument(expr));
                expr = CreateObject(this._pszType, args);
                return expr;
            }
            return null;
        }

        private bool GenerateString2Psz(XP.MethodCallContext context, string name)
        {
            // this will only happen when the VO or Vulcan dialect is selected, so we can use the psz type here
            // and the reference to the String2Psz() in the Vulcan Runtime.
            ArgumentListSyntax argList;
            ExpressionSyntax expr;
            if (context.ArgList != null)
            {
                argList = context.ArgList.Get<ArgumentListSyntax>();
            }
            else
            {
                argList = EmptyArgumentList();
            }
            if (CurrentEntity != null)
            {
                // Add reference to compiler generated List<IntPtr> to the argList
                if (argList.Arguments.Count != 1)
                {
                    expr = GenerateNIL().WithAdditionalDiagnostics(
                        new SyntaxDiagnosticInfo(ErrorCode.ERR_BadArgCount, name, argList.Arguments.Count));
                    context.Put(expr);
                }
                else
                {
                    var result = _GenerateString2Psz(context, argList.Arguments[0].Expression);
                    context.Put(result);
                }
                return true;
            }
            else
            {
                return false;
            }
        }

        private ExpressionSyntax GenerateGetClipperParam(ExpressionSyntax expr)
        {
            // Note that the expr must result into a 1 based offset or (with /az) a 0 based offset
            // XS$PCount > ..
            BinaryExpressionSyntax cond;
            // no changes to expr for length comparison, even with /az
            cond = _syntaxFactory.BinaryExpression(
                                SyntaxKind.GreaterThanOrEqualExpression,
                                GenerateSimpleName(XSharpSpecialNames.ClipperPCount),
                                SyntaxFactory.MakeToken(SyntaxKind.GreaterThanToken),
                                expr);
            // XS$Args[..]
            if (_options.ArrayZero)
            {
                // adjust array offset when compiling with /az
                expr = _syntaxFactory.BinaryExpression(
                                SyntaxKind.SubtractExpression,
                                expr,
                                SyntaxFactory.MakeToken(SyntaxKind.MinusToken),
                                GenerateLiteral(1));
            }
            var indices = _pool.AllocateSeparated<ArgumentSyntax>();
            indices.Add(MakeArgument(expr));
            var left = _syntaxFactory.ElementAccessExpression(
                GenerateSimpleName(XSharpSpecialNames.ClipperArgs),
                _syntaxFactory.BracketedArgumentList(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                    indices,
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)));

            var right = GenerateNIL();
            var result = _syntaxFactory.ConditionalExpression(
                            cond,
                            SyntaxFactory.MakeToken(SyntaxKind.QuestionToken),
                            left,
                            SyntaxFactory.MakeToken(SyntaxKind.ColonToken),
                            right);
            _pool.Free(indices);
            return result;
        }

        private bool GenerateClipCallFunc(XP.MethodCallContext context, string name)
        {
            ArgumentListSyntax argList;
            ExpressionSyntax expr;
            if (context.ArgList != null)
            {
                argList = context.ArgList.Get<ArgumentListSyntax>();
            }
            else
            {
                argList = EmptyArgumentList();
            }
            if (name == "ARGCOUNT")
            {
                // Number of declared arguments in the function/methods
                if (CurrentEntity != null)
                {
                    var currEnt = this.CurrentEntity;
                    int argCount = currEnt != null ? currEnt.Params.Count : 0;
                    expr = GenerateLiteral(argCount);
                }
                else
                {
                    expr = GenerateLiteral(0);
                }
                if (argList.Arguments.Count != 0)
                {
                    expr = expr.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_BadArgCount, name, argList.Arguments.Count));
                }
                context.Put(expr);
                return true;

            }
            if (name == "PCOUNT")
            {
                if (_options.NoClipCall)
                {
                    var currEnt = this.CurrentEntity;
                    int argCount = currEnt != null ? currEnt.Params.Count : 0;
                    expr = GenerateLiteral(argCount);
                    context.Put(expr);
                }
                else
                {
                    expr = GenerateSimpleName(XSharpSpecialNames.ClipperPCount);
                    if (argList.Arguments.Count != 0)
                    {
                        expr = expr.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_BadArgCount, name, argList.Arguments.Count));
                    }
                    context.Put(expr);
                    if (CurrentEntity != null)
                        CurrentEntity.Data.UsesPCount = true;
                }
                return true;
            }
            else
            {
                if (argList.Arguments.Count != 1)
                {
                    expr = GenerateNIL().WithAdditionalDiagnostics(
                        new SyntaxDiagnosticInfo(ErrorCode.ERR_BadArgCount, name, argList.Arguments.Count));
                    context.Put(expr);
                    return true;
                }
                if (_options.NoClipCall)
                {
                    expr = GenerateNIL();
                    context.Put(expr);
                }
                else
                {
                    // _GETMPARAM or _GETFPARAM
                    if (CurrentEntity != null)
                        CurrentEntity.Data.UsesGetMParam = true;
                    context.Put(GenerateGetClipperParam(argList.Arguments[0].Expression));
                }
                return true;
            }
        }

        public override void EnterMethod([NotNull] XP.MethodContext context)
        {
            base.EnterMethod(context);
            Check4ClipperCC(context, context.ParamList?._Params, context.CallingConvention?.Convention, context.Type);
            if (context.T.Token.Type != XP.METHOD)
            {
                context.Data.HasClipperCallingConvention = false;
                context.Data.HasTypedParameter = true;          // this will set all missing types to USUAL
            }
            else
            {
                if (_options.VoInitAxitMethods && !context.isInInterface())
                {
                    var idName = context.Id.GetText();
                    if (String.Equals(idName, "init", StringComparison.OrdinalIgnoreCase)
                        || String.Equals(idName, "axit", StringComparison.OrdinalIgnoreCase))
                    {
                        context.Data.MustBeVoid = true;
                        context.Data.IsInitAxit = true;
                    }
                }

            }
        }
        public override void EnterFunction([NotNull] XP.FunctionContext context)
        {
            base.EnterFunction(context);
            Check4ClipperCC(context, context.ParamList?._Params, context.CallingConvention?.Convention, context.Type);
        }

        public override void EnterProcedure([NotNull] XP.ProcedureContext context)
        {
            base.EnterProcedure(context);
            Check4ClipperCC(context, context.ParamList?._Params, context.CallingConvention?.Convention, null);
        }

        public override void EnterConstructor([NotNull] XP.ConstructorContext context)
        {
            base.EnterConstructor(context);
            Check4ClipperCC(context, context.ParamList?._Params, context.CallingConvention?.Convention, null);
        }

        public override void EnterVodll([NotNull] XP.VodllContext context)
        {
            base.EnterVodll(context);
            Check4ClipperCC(context, context.ParamList?._Params, context.CallingConvention?.Cc, context.Type);
        }
        #endregion
        
        #region Literals

        private int[] DecodeDateConst(string dateliteral)
        {
            var args = dateliteral.Split('.');
            if (args.Length == 3)
            {
                int year, month, day;
                if (Int32.TryParse(args[0], out year) &&
                    Int32.TryParse(args[1], out month) &&
                    Int32.TryParse(args[2], out day))
                {
                    return new int[] { year, month, day };
                }
            }
            return null;
        }

        private ExpressionSyntax GenerateVOArrayInitializer([NotNull]XP.ArraysubContext arraysub)
        {
            var args = new List<ArgumentSyntax>();
            foreach (var index in arraysub._ArrayIndex)
            {
                args.Add(MakeArgument(index.Get<ExpressionSyntax>()));
            }
            var initializer = GenerateMethodCall(
                _options.XSharpRuntime ? XSharpQualifiedFunctionNames.ArrayNew : VulcanQualifiedFunctionNames.ArrayNew, 
                MakeArgumentList(args.ToArray()),true);
            initializer.XNode = arraysub;
            return initializer;
        }


        internal InitializerExpressionSyntax MakeArrayInitializer(SeparatedSyntaxList<ExpressionSyntax> exprs)
        {
            return _syntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression,
                                              SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                                              exprs,
                                              SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));
        }

        public override void ExitLiteralArray([NotNull] XP.LiteralArrayContext context)
        {
            ExpressionSyntax expr = null;
            // detect typed arrays.
            // <LONG> {...} indicates an array of type LONG -> Handled by base class
            if (context.Type != null)
            {
                base.ExitLiteralArray(context);
                return;
            }
            // when no type is specified and the dialect VO or Vulcan the type is USUAL
            TypeSyntax type = _usualType;
            SeparatedSyntaxList<ExpressionSyntax> exprs;
            if ((context._Elements?.Count ?? 0) > 0)
            {
                // Literal array with optional elements.
                // ExitArrayElement has left the CsNode empty for missing Expressions
                var l = _pool.AllocateSeparated<ExpressionSyntax>();
                foreach (var item in context._Elements)
                {
                    if (l.Count > 0)
                        l.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                    if (item.Expr != null)
                        l.Add(item.Expr.Get<ExpressionSyntax>());
                    else
                        l.Add(GenerateMissingExpression(false));

                }
                exprs = l.ToList();
                _pool.Free(l);
            }
            else
            {
                exprs = default(SeparatedSyntaxList<ExpressionSyntax>);
            }
            var initializer = MakeArrayInitializer(exprs);
            expr = _syntaxFactory.ArrayCreationExpression(SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                _syntaxFactory.ArrayType(type,
                MakeList(_syntaxFactory.ArrayRankSpecifier(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                    MakeSeparatedList<ExpressionSyntax>(
                        _syntaxFactory.OmittedArraySizeExpression(SyntaxFactory.MakeToken(SyntaxKind.OmittedArraySizeExpressionToken))),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)))),
                initializer);
            context.Put<ExpressionSyntax>(CreateObject(_arrayType, MakeArgumentList(MakeArgument(expr))));

        }
        public override void ExitLiteralValue([NotNull] XP.LiteralValueContext context)
        {
            string[] args;

            // Map some literals to static member access or static method calls
            // Or add a cast to help overload resolution
            ArgumentSyntax arg0, arg1, arg2;
            ExpressionSyntax expr = null;
            switch (context.Token.Type)
            {
                case XP.NIL:
                    expr = GenerateNIL();
                    break;
                case XP.NULL_PTR:
                    expr = MakeSimpleMemberAccess(_ptrType, GenerateSimpleName("Zero"));
                    break;
                case XP.NULL_PSZ:
                    expr = CreateObject(_pszType, MakeArgumentList(MakeArgument(GenerateLiteral(0))));
                    break;
                case XP.NULL_ARRAY:
                    expr = MakeCastTo(_arrayType, GenerateLiteralNull());
                    break;
                case XP.NULL_CODEBLOCK:
                    expr = MakeCastTo(_codeblockType, GenerateLiteralNull());
                    break;
                case XP.NULL_DATE:
                    expr = GenerateMethodCall(_options.XSharpRuntime ? XSharpQualifiedFunctionNames.NullDate : VulcanQualifiedFunctionNames.NullDate, EmptyArgumentList(),true);
                    break;
                case XP.NULL_SYMBOL:
                    arg0 = MakeArgument(GenerateLiteral(""));
                    expr = CreateObject(_symbolType, MakeArgumentList(arg0));
                    break;
                case XP.DATE_CONST:
                    int[] elements = DecodeDateConst(context.Token.Text);
                    if (elements != null)
                    {
                        arg0 = MakeArgument(GenerateLiteral(elements[0]));
                        arg1 = MakeArgument(GenerateLiteral(elements[1]));
                        arg2 = MakeArgument(GenerateLiteral(elements[2]));
                        expr = CreateObject(_dateType, MakeArgumentList(arg0, arg1, arg2));
                    }
                    break;
                case XP.SYMBOL_CONST:
                    // call helper method that will create a symbol for the symboltable
                    expr = GenerateLiteralSymbol(context.Token.Text);
                    break;
                case XP.REAL_CONST:
                    if (_options.VOFloatConstants && !(CurrentEntity is XP.VodefineContext))
                    {
                        // check to see if the token contains an 'S', 'D' or 'M'. In that case leave as is, since the user has specified
                        // single, double or decimal
                        var text = context.Token.Text;
                        if (text.IndexOfAny("sdmSDM".ToCharArray()) == -1)
                        {
                            args = text.Split('.');
                            if (args.Length == 2)
                            {
                                int len = context.Token.Text.Length;
                                int dec = args[1].Length;
                                arg0 = MakeArgument(GenerateLiteral(context.Token));
                                arg1 = MakeArgument(GenerateLiteral("0", 0));
                                arg2 = MakeArgument(GenerateLiteral(dec.ToString(), dec));
                                expr = CreateObject(_floatType, MakeArgumentList(arg0, arg1, arg2));
                            }
                        }
                    }
                    break;
            }
            if (expr != null)
            {
                context.Put(expr);
                return;
            }
            base.ExitLiteralValue(context);
        }

        #endregion
        #region Codeblocks
        public override void ExitCodeblock([NotNull] XP.CodeblockContext context)
        {
            base.ExitCodeblock(context);
            if (context.lambda == null)
            {
                var expr = context.Get<ExpressionSyntax>();
                expr = MakeCastTo(_codeblockType, expr);
                context.Put(expr);
            }
        }

        public override void ExitCodeblockCode([NotNull] XP.CodeblockCodeContext context)
        {
            // Convert everything to a stmt block when it is a real codeblock and "just" an expression
            // so it is easier to fix Void expressions as last expression in the list
            base.ExitCodeblockCode(context);
            var block = context.CsNode as BlockSyntax;
            if (block == null )
            {
                var cbc = context.Parent as XP.CodeblockContext;
                if (cbc?.lambda == null)
                {
                    if (cbc?.LambdaParamList == null || cbc?.LambdaParamList.ImplicitParams != null)
                    {
                        block = MakeBlock(GenerateReturn((ExpressionSyntax)context.CsNode));
                        context.Put<BlockSyntax>(block);
                    }
                }
            }
            if (context.Expr == null && context.ExprList == null && context.StmtBlk == null)
            {
                // empty codeblock ?
                var cbcontext = context.Parent as XP.CodeblockContext;
                if (cbcontext?.lambda == null)
                {
                    block = MakeBlock(GenerateReturn(GenerateNIL()));
                    context.Put<BlockSyntax>(block);
                }
            }
        }
        #endregion

        #region Workarea and Macro
        public override void ExitFielddecl([NotNull] XP.FielddeclContext context)
        {
            var stmt = _syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            context.SetSequencePoint();
            context.Put(stmt);
            return;
        }
        public override void EnterFielddecl([NotNull] XP.FielddeclContext context)
        {
            // register field names with current entity
            // so we can check for the field name in the ExitNameExpr method
            string Alias = "";
            if (context.Alias != null)
                Alias = context.Alias.GetText();
            if (CurrentEntity != null)
            {
                foreach (var field in context._Fields)
                {
                    CurrentEntity.Data.AddField(field.Id.GetText(), Alias, true);
                }
            }
        }


        public CSharpSyntaxNode GenerateAliasedExpression(
            [NotNull] XP.AliasedExprContext context,
            ExpressionSyntax wa, ExpressionSyntax expr)
        {
            // Adjust the expression that is evaluated in the other workarea
            if (expr is AssignmentExpressionSyntax)
            {
                // If the expression is a assignment expression 
                // then convert to FieldSet
                var ass = expr as AssignmentExpressionSyntax;
                var lhs = ass.Left;
                var rhs = ass.Right;
                expr = GenerateFieldSet(null, lhs.XNode.GetText(), rhs);
            }
            else if (expr is IdentifierNameSyntax)
            {
                // If it is an identifier name then convert
                // to FieldGet
                var ins = expr as IdentifierNameSyntax;
                expr = GenerateFieldGet(null, ins.Identifier.XNode.GetText());
            }
            else
            {
                ; // leave unchanged
            }
            var push = GenerateMethodCall(_options.XSharpRuntime ? XSharpQualifiedFunctionNames.PushWorkarea: VulcanQualifiedFunctionNames.PushWorkarea, MakeArgumentList(MakeArgument(wa)),true);
            var pop = GenerateMethodCall(_options.XSharpRuntime ? XSharpQualifiedFunctionNames.PopWorkarea : VulcanQualifiedFunctionNames.PopWorkarea, EmptyArgumentList(), true);
            var pushStmt = GenerateExpressionStatement(push, true);
            var popStmt = GenerateExpressionStatement(pop, true);

            if (context.Parent.Parent is XP.ExpressionStmtContext)
            {
                // context.Parent is always a primaryexpression
                // if context.Parent.Parent is a Expressionstatement then we do not have 
                // save the return value of the expression
                var list = new List<StatementSyntax>() { pushStmt, GenerateExpressionStatement(expr), popStmt };
                return MakeBlock(list);
            }
            else
            {
                // when not an expression statement, then we must save the result of expr
                // of the expression.
                // we do this by converting the expression to a anonymous method and returning the value from this expression
                // CUSTOMER->(<Expression>)
                //
                // translate to :
                //
                // GeneratedFunctionCall(CUSTOMER, expr)
                //   __pushWorkarea(CUSTOMER)
                //   try
                //     return expr
                //   finally
                //     __popWorkarea()
                //   end
                // }:Eval()
                return _syntaxFactory.InvocationExpression(
                MakeSimpleMemberAccess(
                    MakeCastTo(_codeblockType,
                        _syntaxFactory.ParenthesizedLambdaExpression(
                            asyncKeyword: null,
                            parameterList: EmptyParameterList(),
                            arrowToken: SyntaxFactory.MakeToken(SyntaxKind.EqualsGreaterThanToken),
                            body: MakeBlock(MakeList<StatementSyntax>(
                                pushStmt,
                                _syntaxFactory.TryStatement(SyntaxFactory.MakeToken(SyntaxKind.TryKeyword),
                                    MakeBlock(MakeList<StatementSyntax>(GenerateReturn(expr))),
                                    EmptyList<CatchClauseSyntax>(),
                                    _syntaxFactory.FinallyClause(SyntaxFactory.MakeToken(SyntaxKind.FinallyKeyword),
                                        MakeBlock(MakeList<StatementSyntax>(popStmt))
                                        )
                                    )
                                ))
                            )
                        ),
                    _syntaxFactory.IdentifierName(SyntaxFactory.MakeIdentifier(XSharpFunctionNames.Eval))
                    ),
                EmptyArgumentList());

            }

        }

        public override void ExitAliasedField([NotNull] XP.AliasedFieldContext context)
        {
            //_FIELD->NAME, CUSTOMER-NAME, _FIELD->CUSTOMER->NAME
            // translate to either __FieldGetWa(cArea,cField)
            // or __FieldGet(cField)
            ExpressionSyntax expr;
            string alias = context.Alias?.GetText();
            string field = context.Field.GetText();
            if (!String.IsNullOrEmpty(alias) && alias.ToUpper() == "M")
            {
                // M->FIELD
                expr = GenerateMemVarGet(field);
            }
            else
            {
                expr = GenerateFieldGet(alias, field);
            }
            context.Put(expr);
            return;
        }
        public override void ExitAliasedExpr([NotNull] XP.AliasedExprContext context)
        {
            ExpressionSyntax alias = context.Alias.Get<ExpressionSyntax>();
            if (context.Id != null)
            {
                if (context.Expr is XP.PrimaryExpressionContext && context.Expr.GetChild(0) is XP.NameExpressionContext)
                {
                    string field = context.Expr.GetText();
                    context.Put(GenerateFieldGet(context.Id.GetText(), field));
                    return;
                }
                alias = GenerateLiteral(context.Id.GetText());
            }
            var expr =
                GenerateAliasedExpression(
                        context,
                        alias,     // workarea
                        context.Expr.Get<ExpressionSyntax>() // expression
                    );
            context.Put(expr);
        }
        public override void ExitMacro([NotNull] XP.MacroContext context)
        {
            ExpressionSyntax expr;
            expr = context.Expr.Get<ExpressionSyntax>();
            var args = MakeArgumentList(MakeArgument(expr));
            context.SetSequencePoint();
            expr = GenerateMethodCall(_options.XSharpRuntime ? XSharpQualifiedFunctionNames.Evaluate : VulcanQualifiedFunctionNames.Evaluate, args, true);
            context.Put(expr);
            return;
        }

        #endregion

        #region Conversions and Typecasts
        public override void ExitVoConversionExpression([NotNull] XP.VoConversionExpressionContext context)
        {

            // Special case for PSZ(..) 
            // PSZ("String") becomes String2Psz("String")
            if (context.XType != null)
            {
                var xtype = context.XType as XP.XbaseTypeContext;
                if (xtype.Token.Type == XP.PSZ)
                {
                    ExpressionSyntax expr;
                    if (GenerateLiteralPsz(context.Expr, out expr))
                    {
                        context.Put(expr);
                        return;
                    }
                }
            }

            base.ExitVoConversionExpression(context);

        }

        public override void ExitTypeCast([NotNull] XP.TypeCastContext context)
        {
            // Special case for (PSZ) Expression, this becomes String2Psz(<Expression>)
            // but only when <Expression> is a literal string
            var dt = context.Type as XP.DatatypeContext;
            if (dt is XP.SimpleDatatypeContext)
            {
                var sdt = dt as XP.SimpleDatatypeContext;
                if (sdt.TypeName.XType != null && sdt.TypeName.XType.Token.Type == XP.PSZ)
                {
                    if (context.Expr.IsLiteralString())
                    {
                        var result = _GenerateString2Psz(context, context.Expr.Get<ExpressionSyntax>());
                        context.Put(result);
                        return;
                    }
                }
            }
            base.ExitTypeCast(context);
            return;
        }

        public override void ExitVoCastExpression([NotNull] XP.VoCastExpressionContext context)
        {
            // Special case for PSZ(_CAST 
            // PSZ(_CAST, "String") becomes String2Psz("String")
            if (context.XType != null)
            {
                var xtype = context.XType as XP.XbaseTypeContext;
                if (xtype.Token.Type == XP.PSZ)
                {
                    ExpressionSyntax expr;
                    if (GenerateLiteralPsz(context.Expr, out expr))
                    {
                        context.Put(expr);
                        return;
                    }
                }
            }
            base.ExitVoCastExpression(context);
        }

        #endregion

        #region VO Types

        public override void EnterVostruct([NotNull] XP.VostructContext context)
        {
            voStructHasDim = false;
        }

        public override void ExitVostruct([NotNull] XP.VostructContext context)
        {
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility();
            if (voStructHasDim)
            {
                var modBuilder = _pool.Allocate();
                modBuilder.AddRange(mods);
                modBuilder.Add(SyntaxFactory.MakeToken(SyntaxKind.UnsafeKeyword));
                mods = modBuilder.ToList<SyntaxToken>();
                _pool.Free(modBuilder);
            }
            var attargs = ArrayBuilder<AttributeArgumentSyntax>.GetInstance();
            attargs.Add(_syntaxFactory.AttributeArgument(null, null, GenerateQualifiedName(SystemQualifiedNames.LayoutSequential)));
            if (context.Alignment != null)
            {
                var lit = GenerateLiteral(context.Alignment);
                attargs.Add(_syntaxFactory.AttributeArgument(GenerateNameEquals("Pack"), null, lit));
            }

            MemberDeclarationSyntax m = _syntaxFactory.StructDeclaration(
                attributeLists: MakeList(
                    MakeAttributeList(
                        target: null,
                        attributes: MakeSeparatedList(
                            _syntaxFactory.Attribute(
                                name: GenerateQualifiedName(SystemQualifiedNames.StructLayout),
                                argumentList: MakeAttributeArgumentList(MakeSeparatedList(attargs.ToArrayAndFree()))
                                )
                            ))
                    ),
                modifiers: mods,
                keyword: SyntaxFactory.MakeToken(SyntaxKind.StructKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: null,
                baseList: null,
                constraintClauses: null,
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: (context._Members?.Count > 0) ? MakeList<MemberDeclarationSyntax>(context._Members) : EmptyList<MemberDeclarationSyntax>(),
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null);
            m.XVoDecl = true;
            if (context.Namespace != null)
            {
                m = AddNameSpaceToMember(context.Namespace, m);
            }
            else
            {
                m = (MemberDeclarationSyntax)CheckTypeName(context, "VOSTRUCT", m);
            }
            context.Put(m);
        }

        private TypeSyntax voStructMemberDataType(XP.VostructmemberContext context)
        {
            var varType = context.DataType?.Get<TypeSyntax>() ?? MissingType();
            if (context.DataType is XP.SimpleDatatypeContext)
            {
                var sdt = context.DataType as XP.SimpleDatatypeContext;
                if (sdt.TypeName.NativeType != null)
                {
                    if (sdt.TypeName.NativeType.Token.Type == XP.LOGIC)
                    {
                        varType = GenerateQualifiedName(_winBoolType);
                    }
                }
            }
            return varType;
        }

        public override void ExitVostructmember([NotNull] XP.VostructmemberContext context)
        {
            bool isDim = context.Dim != null;
            bool isUnionMember = (context.Parent is XP.VounionContext);
            var varType = voStructMemberDataType(context);

            varType.XVoDecl = true;
            if (context.As?.Type == XP.IS)
            {
                varType.XVoIsDecl = true;
            }
            if (isDim)
            {
                voStructHasDim = true;
            }

            SyntaxList<AttributeListSyntax> atts = null;
            if (isUnionMember)
            {
                var args = MakeSeparatedList(
                                        _syntaxFactory.AttributeArgument(null, null,
                                            GenerateLiteral("0", 0)));
                var arglist = MakeAttributeArgumentList(args);
                var att = MakeSeparatedList(
                            _syntaxFactory.Attribute(
                                name: GenerateQualifiedName(SystemQualifiedNames.FieldOffset),
                                argumentList: arglist));
                atts = MakeAttributeList(null, att);
            }
            else
            {
                atts = EmptyList<AttributeListSyntax>();
            }
            context.Put(_syntaxFactory.FieldDeclaration(
                atts,
                TokenList(SyntaxKind.PublicKeyword, isDim ? SyntaxKind.FixedKeyword : SyntaxKind.None),
                _syntaxFactory.VariableDeclaration(varType,
                    MakeSeparatedList(
                        isDim ? GenerateBuffer(context.Id.Get<SyntaxToken>(), MakeBracketedArgumentList(context.ArraySub._ArrayIndex.Select(e => _syntaxFactory.Argument(null, null, e.Get<ExpressionSyntax>())).ToArray()))
                        : GenerateVariable(context.Id.Get<SyntaxToken>()))),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void EnterVounion([NotNull] XP.VounionContext context)
        {
            voStructHasDim = false;
        }

        public override void ExitVounion([NotNull] XP.VounionContext context)
        {
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility();
            if (voStructHasDim)
            {
                var modBuilder = _pool.Allocate();
                modBuilder.AddRange(mods);
                modBuilder.Add(SyntaxFactory.MakeToken(SyntaxKind.UnsafeKeyword));
                mods = modBuilder.ToList<SyntaxToken>();
                _pool.Free(modBuilder);
            }

            MemberDeclarationSyntax m = _syntaxFactory.StructDeclaration(
                attributeLists: MakeList(
                    MakeAttributeList(
                        target: null,
                        attributes: MakeSeparatedList(
                            _syntaxFactory.Attribute(
                                name: GenerateQualifiedName(SystemQualifiedNames.StructLayout),
                                argumentList: MakeAttributeArgumentList(
                                    MakeSeparatedList(_syntaxFactory.AttributeArgument(null, null, GenerateQualifiedName(SystemQualifiedNames.LayoutExplicit)))
                                    )
                                )
                            ))
                    ),
                modifiers: mods,
                keyword: SyntaxFactory.MakeToken(SyntaxKind.StructKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: null,
                baseList: null,
                constraintClauses: null,
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: (context._Members?.Count > 0) ? MakeList<MemberDeclarationSyntax>(context._Members) : EmptyList<MemberDeclarationSyntax>(),
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null);
            m.XVoDecl = true;
            if (context.Namespace != null)
            {
                m = AddNameSpaceToMember(context.Namespace, m);
            }
            else
            {
                m = (MemberDeclarationSyntax)CheckTypeName(context, "UNION", m);
            }
            context.Put(m);
        }

        #endregion
 
    }
}
