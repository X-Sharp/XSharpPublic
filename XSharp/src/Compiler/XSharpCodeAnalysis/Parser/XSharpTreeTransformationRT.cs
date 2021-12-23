//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System;
using System.Linq;
using System.Collections.Generic;
using System.Diagnostics;
using Roslyn.Utilities;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    using Microsoft.CodeAnalysis.Syntax.InternalSyntax;

    internal class XSharpTreeTransformationRT : XSharpTreeTransformationCore
    {
        // XBase Type Names
        #region Fields
        protected readonly TypeSyntax _usualType;
        protected readonly TypeSyntax _floatType;
        protected readonly TypeSyntax _currencyType;
        protected readonly TypeSyntax _binaryType;
        protected readonly TypeSyntax _arrayType;
        protected readonly TypeSyntax _dateType;
        protected readonly TypeSyntax _symbolType;
        protected readonly TypeSyntax _pszType;
        protected readonly TypeSyntax _codeblockType;
        private readonly string _errorType;
        private readonly string _classLibraryType;
        private readonly string _wrappedExceptionType;
        private readonly string _compilerVersionType;
        private readonly string _runtimeStateType;
        private readonly string _defaultParameterType;
        private readonly string _actualType;
        private readonly string _clipperCallingConvention;

        private ArrayTypeSyntax arrayOfUsual = null;
        private ArrayTypeSyntax arrayOfString = null;
        private Dictionary<string, MemVarFieldInfo> _filewideMemvars = null;

        private Dictionary<string, FieldDeclarationSyntax> _literalSymbols;
        private Dictionary<string, Tuple<string, FieldDeclarationSyntax>> _literalPSZs;

        private SyntaxList<AttributeListSyntax> _voClassAttribs = null;
        private ParameterListSyntax _clipperParams = null;
        private AttributeListSyntax _actualArgs = null;

        #endregion

        #region Constructors and destructors
        protected override XSharpTreeTransformationCore CreateWalker(XSharpParser parser)
        {
            return new XSharpTreeTransformationRT(parser, _options, _pool, _syntaxFactory, _fileName);
        }
        public XSharpTreeTransformationRT(XSharpParser parser, CSharpParseOptions options, SyntaxListPool pool,
            ContextAwareSyntax syntaxFactory, string fileName) :
            base(parser, options, pool, syntaxFactory, fileName)
        {
            if (options.XSharpRuntime)
            {
                _usualType = GenerateQualifiedName(XSharpQualifiedTypeNames.Usual);
                _floatType = GenerateQualifiedName(XSharpQualifiedTypeNames.Float);
                _arrayType = GenerateQualifiedName(XSharpQualifiedTypeNames.Array);
                _binaryType = GenerateQualifiedName(XSharpQualifiedTypeNames.Binary);
                _codeblockType = GenerateQualifiedName(XSharpQualifiedTypeNames.Codeblock);
                _currencyType = GenerateQualifiedName(XSharpQualifiedTypeNames.Currency);
                _dateType = GenerateQualifiedName(XSharpQualifiedTypeNames.Date);
                _pszType = GenerateQualifiedName(XSharpQualifiedTypeNames.Psz);
                _symbolType = GenerateQualifiedName(XSharpQualifiedTypeNames.Symbol);
                _errorType = XSharpQualifiedTypeNames.Error;
                _wrappedExceptionType = XSharpQualifiedTypeNames.WrappedException;
                _classLibraryType = XSharpQualifiedTypeNames.ClassLibrary;
                _compilerVersionType = XSharpQualifiedTypeNames.CompilerVersion;
                _runtimeStateType = XSharpQualifiedTypeNames.RuntimeState;
                _defaultParameterType = XSharpQualifiedTypeNames.DefaultParameter;
                _actualType = XSharpQualifiedTypeNames.ActualType;
                _clipperCallingConvention = XSharpQualifiedTypeNames.ClipperCallingConvention;
            }
            else
            {
                _usualType = GenerateQualifiedName(VulcanQualifiedTypeNames.Usual);
                _floatType = GenerateQualifiedName(VulcanQualifiedTypeNames.Float);
                _currencyType = GenerateQualifiedName(VulcanQualifiedTypeNames.Usual);
                _binaryType = GenerateQualifiedName(VulcanQualifiedTypeNames.Usual);
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
            }
            if (_options.SupportsMemvars)
            {
                _filewideMemvars = new Dictionary<string, MemVarFieldInfo>(XSharpString.Comparer);
            }

            _literalSymbols = new Dictionary<string, FieldDeclarationSyntax>();
            _literalPSZs = new Dictionary<string, Tuple<string, FieldDeclarationSyntax>>();
            // calculate the global class name;
            GlobalClassName = GetGlobalClassName(_options.TargetDLL);

            // calculate the default vo class attributes
            GetVOClassAttributes();
        }

        internal Dictionary<string, FieldDeclarationSyntax> LiteralSymbols => _literalSymbols;
        internal Dictionary<string, Tuple<string, FieldDeclarationSyntax>> LiteralPSZs => _literalPSZs;
        internal SyntaxList<AttributeListSyntax> VOClassAttribs { get { return GetVOClassAttributes(); } }

        public override string GetGlobalClassName(XSharpTargetDLL targetDLL)
        {
            // our runtime DLLs have a fixed Globals Class name
            if (targetDLL != XSharpTargetDLL.Other && targetDLL < XSharpTargetDLL.VOWin32Api)
            {
                return base.GetGlobalClassName(targetDLL);
            }
            string name = _options.CommandLineArguments?.CompilationOptions.ModuleName;
            string firstSource = _options.CommandLineArguments?.SourceFiles.FirstOrDefault().Path;
            if (string.IsNullOrEmpty(name))
            {
                name = firstSource;
            }

            if (!string.IsNullOrEmpty(name))
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
                arrayOfString = _syntaxFactory.ArrayType(stringType, emptyrank);
            }
        }
        private static XSharpTreeTransformationRT getTransform(CSharpParseOptions options)
        {
            return new XSharpTreeTransformationRT(null, options, new SyntaxListPool(), new ContextAwareSyntax(new SyntaxFactoryContext()), "");
        }

        #endregion

        #region SyntaxTree
        private SyntaxTree GenerateDefaultSyntaxTree(List<Tuple<int, String>> initprocs, bool isApp, bool hasPCall, List<MemVarFieldInfo> filewidepublics)
        {

            // Create Global Functions class with the Members to call the Init procedures
            // Vulcan only does this for DLLs. We do it for EXE too to make things more consistent
            // Methods $Init1() and $Exit() are always created.
            var members = CreateInitMembers(initprocs, isApp, hasPCall, filewidepublics);
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
            arguments.Add(_syntaxFactory.AttributeArgument(null, null, GenerateLiteral("X# " + global::XSharp.Constants.FileVersion + " - dialect:" + _options.Dialect.ToString())));
            attributes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
            attributes.Add(_syntaxFactory.Attribute(
                name: GenerateQualifiedName(_compilerVersionType), argumentList: MakeAttributeArgumentList(arguments)));

            var target = _syntaxFactory.AttributeTargetSpecifier(SyntaxFactory.Identifier("assembly"), SyntaxFactory.MakeToken(SyntaxKind.ColonToken));
            var attrlist = MakeAttributeList(
                target,
                attributes);
            GlobalEntities.Attributes.Add(attrlist);
            _pool.Free(arguments);
            _pool.Free(attributes);
            var eof = SyntaxFactory.Token(SyntaxKind.EndOfFileToken);
            var cu = _syntaxFactory.CompilationUnit(
                    GlobalEntities.Externs,
                    GlobalEntities.Usings,
                    GlobalEntities.Attributes,
                    GlobalEntities.Members, eof);
            cu.XGenerated = true;
            var red = (Syntax.CompilationUnitSyntax)cu.CreateRed();
            return CSharpSyntaxTree.Create(red);
        }
        public static SyntaxTree DefaultRTSyntaxTree(IEnumerable<SyntaxTree> trees, bool isApp)
        {
            // Trees is NEVER empty !
            CSharpParseOptions options = (CSharpParseOptions)trees.First().Options;
            // Collect Init procedures in all trees
            var initprocs = new List<Tuple<int, string>>();
            var filewidepublics = new List<MemVarFieldInfo>();
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
                        if (unit.FileWidePublics != null)
                        {
                            filewidepublics.AddRange(unit.FileWidePublics);
                        }

                        hasPCall = hasPCall || unit.HasPCall;
                    }
                }
            }

            var t = getTransform(options);
            return t.GenerateDefaultSyntaxTree(initprocs, isApp, hasPCall, filewidepublics);
        }

        public static string VOGlobalClassName(CSharpParseOptions options)
        {
            var t = getTransform(options);
            return t.GlobalClassName;
        }
        #endregion

        #region Special app methods
        protected MethodDeclarationSyntax CreateInitFunction(IList<String> procnames, string functionName, bool isApp, List<MemVarFieldInfo> filewidepublics = null)
        {
            // create body for new Init procedure
            var stmts = _pool.Allocate<StatementSyntax>();
            foreach (var name in procnames)
            {
                var invoke = GenerateMethodCall(name, true);
                stmts.Add(GenerateExpressionStatement(invoke, null));
            }
            if (filewidepublics != null)
            {
                foreach (var memvar in filewidepublics)
                {
                    var name = memvar.Name;
                    var exp = GenerateMemVarDecl(memvar.Context, GenerateLiteral(name), false);
                    exp.XNode = memvar.Context;
                    stmts.Add(GenerateExpressionStatement(exp, memvar.Context)); ; ;
                    var context = memvar.Context as XSharpParser.XbasevarContext;
                    if (context.Expression != null)
                    {
                        exp = GenerateMemVarPut(context, GenerateLiteral(name), context.Expression.Get<ExpressionSyntax>());
                        stmts.Add(GenerateExpressionStatement(exp, context));
                    }
                    else if (context.ArraySub != null)
                    {
                        var args = new List<ArgumentSyntax>();
                        foreach (var index in context.ArraySub._ArrayIndex)
                        {
                            args.Add(MakeArgument(index.Get<ExpressionSyntax>()));
                        }
                        var initializer = GenerateMethodCall(XSharpQualifiedFunctionNames.ArrayNew, MakeArgumentList(args.ToArray()), true);
                        exp = GenerateMemVarPut(context, GenerateLiteral(name), initializer);
                        stmts.Add(GenerateExpressionStatement(exp, context));

                    }
                    else
                    {
                        // Assign FALSE to PUBLIC variables or TRUE when the name is CLIPPER
                        bool publicvalue = context.Id.GetText().ToUpper() == "CLIPPER";
                        exp = GenerateMemVarPut(context, GenerateLiteral(name), GenerateLiteral(publicvalue));
                        stmts.Add(GenerateExpressionStatement(exp, context));
                    }
                }
            }
            var mods = TokenList(isApp ? SyntaxKind.InternalKeyword : SyntaxKind.PublicKeyword, SyntaxKind.StaticKeyword);
            var pars = EmptyParameterList();
            var m = SyntaxFactory.MethodDeclaration(MakeCompilerGeneratedAttribute(), mods,
                voidType, /*explicitif*/null,
                SyntaxFactory.Identifier(functionName), /*typeparams*/null, pars,/* constraints*/null, MakeBlock(stmts),/*exprbody*/null,
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));

            _pool.Free(stmts);
            m.XGenerated = true;
            return m;
        }

        private List<MemberDeclarationSyntax> CreateInitMembers(List<Tuple<int, String>> initprocs, bool isApp, bool hasPCall, List<MemVarFieldInfo> filewidepublics)
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
            // Suppress generating $init1 when no methods are found and SuppressInit1 = true;
            if (!_options.SuppressInit1 || init1.Count > 0)
            {
                members.Add(CreateInitFunction(init1, XSharpSpecialNames.InitProc1, isApp));
            }
            if (init2.Count > 0)
            {
                members.Add(CreateInitFunction(init2, XSharpSpecialNames.InitProc2, isApp));
            }
            if (init3.Count > 0 || filewidepublics.Count > 0)
            {
                members.Add(CreateInitFunction(init3, XSharpSpecialNames.InitProc3, isApp, filewidepublics));
            }

            if (!_options.SuppressInit1 || exit.Count > 0)
            {
                members.Add(CreateInitFunction(exit, XSharpSpecialNames.ExitProc, isApp));
            }
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
            var body = MakeBlock();
            var appId = SyntaxFactory.Identifier(XSharpSpecialNames.AppExit);
            var modifiers = TokenList(SyntaxKind.InternalKeyword, SyntaxKind.StaticKeyword);

            var appExit = _syntaxFactory.MethodDeclaration(
                MakeCompilerGeneratedAttribute(), modifiers,
                voidType, null, appId, null, EmptyParameterList(),
                null, body, null, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            appExit.XGenerated = true;
            return appExit;
        }

        private MethodDeclarationSyntax CreateRunInitProcs()
        {
            var body = MakeBlock();
            var appId = SyntaxFactory.Identifier(ReservedNames.RunInitProcs);
            var modifiers = TokenList(SyntaxKind.PublicKeyword, SyntaxKind.StaticKeyword);
            var initProcs = _syntaxFactory.MethodDeclaration(
                MakeCompilerGeneratedAttribute(), modifiers,
                voidType, null, appId, null, EmptyParameterList(),
                null, body, null, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
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
            stmts.Add(GenerateExpressionStatement(MakeSimpleAssignment(lhs, rhs), null, true));
            // rest of the statements is generated in the LocalRewriter with a check for the existence of the fields in VulcanRT.
            // in Vulcan.Runtime.State
            var body = MakeBlock(stmts);
            stmts.Clear();

            // Create Exception
            var arg1 = MakeArgument(GenerateLiteral("Error when executing code in INIT procedure(s)"));
            var arg2 = MakeArgument(GenerateSimpleName(XSharpSpecialNames.ExVarName));
            var excType = GenerateQualifiedName(SystemQualifiedNames.Exception);
            var Exception = CreateObject(excType, MakeArgumentList(arg1, arg2));
            var throwstmt = _syntaxFactory.ThrowStatement(
                attributeLists: default,
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
                    attributeLists: default,
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
                voidType, null, appId, null, EmptyParameterList(),
                null, body, null, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            _pool.Free(stmts);
            appInit.XGenerated = true;
            return appInit;

        }

        private LocalDeclarationStatementSyntax GenerateReturnVar(TypeSyntax type, ExpressionSyntax expr = null)
        {
            var localdecl = GenerateLocalDecl(XSharpSpecialNames.ReturnName, type, expr);
            localdecl.Declaration.XGenerated = true;
            localdecl.XGenerated = true;
            return localdecl;

        }

        protected override BlockSyntax GenerateEntryPoint(SyntaxList<SyntaxToken> modifiers, [NotNull] XP.IMemberContext context, BlockSyntax body,
                     SyntaxList<AttributeListSyntax> attributeList, ParameterListSyntax parList)
        {
            // We handle the following:
            // 1) when the defined Start function has Clipper calling convention then we generate a new start function
            //    with string[] parameters and we call the defined start function
            // 2) we add a block

            var stmts = new List<StatementSyntax>();
            BlockSyntax epcall;
            var returntype = context.ReturnType.Get<TypeSyntax>() ?? voidType;
            // XPP dialect has a procedure main as entrypoint
            if (context.Data.HasClipperCallingConvention && _options.Dialect == XSharpDialect.XPP)
            {
                InitializeArrayTypes();
                // var Xs$Array := new List<USUAL>()
                var arrayId = SyntaxFactory.MakeIdentifier(XSharpSpecialNames.ArrayName);
                var arrayName = _syntaxFactory.IdentifierName(arrayId);
                var typeparam = _syntaxFactory.TypeArgumentList(SyntaxFactory.MakeToken(SyntaxKind.LessThanToken),
                    MakeSeparatedList(_usualType), SyntaxFactory.MakeToken(SyntaxKind.GreaterThanToken));
                AddUsingWhenMissing(GlobalEntities.Usings, GenerateQualifiedName("System.Collections.Generic"), false, null);

                var genlist = _syntaxFactory.GenericName(SyntaxFactory.Identifier("List"), typeparam);
                var createExpr = _syntaxFactory.ObjectCreationExpression(SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                         genlist, EmptyArgumentList(), null);
                var init = _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken), createExpr);
                var vd = _syntaxFactory.VariableDeclarator(arrayId, null, init);
                var decl = _syntaxFactory.VariableDeclaration(_impliedType, MakeSeparatedList(vd));
                stmts.Add(_syntaxFactory.LocalDeclarationStatement(
                    attributeLists: default,
                    awaitKeyword: null,
                    usingKeyword: null,
                    modifiers: default,
                    declaration: decl,
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                // create iterator that copies values to  the elements in the array
                var block = new List<StatementSyntax>();
                // iterator body : Xs$Array.Add(element)
                var addmethod = MakeSimpleMemberAccess(arrayName, GenerateSimpleName("Add"));
                var methcall = _syntaxFactory.InvocationExpression(addmethod, MakeArgumentList(MakeArgument(GenerateSimpleName("element"))));
                block.Add(GenerateExpressionStatement(methcall, context.Context(), true));
                StatementSyntax forStmt = _syntaxFactory.ForEachStatement(
                    attributeLists: default,
                    awaitKeyword: null,
                    SyntaxFactory.MakeToken(SyntaxKind.ForEachKeyword),
                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    _impliedType,
                    SyntaxFactory.Identifier("element"),
                    SyntaxFactory.MakeToken(SyntaxKind.InKeyword),
                    GenerateSimpleName(XSharpSpecialNames.ClipperArgs),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                    MakeBlock(block));
                stmts.Add(forStmt);
                // convert list to array
                // Start(Xs$Array.ToArray())
                var toarray = MakeSimpleMemberAccess(arrayName, GenerateSimpleName("ToArray"));
                var methodCall = _syntaxFactory.InvocationExpression(toarray, EmptyArgumentList());
                var arguments = MakeArgumentList(MakeArgument(methodCall));
                var methodcall = GenerateMethodCall(this._entryPoint, arguments, true);
                if (isVoidType(returntype))
                {
                    stmts.Add(GenerateExpressionStatement(methodcall, context.Context()));
                }
                else
                {
                    stmts.Add(GenerateReturn(methodcall));
                }
                epcall = MakeBlock(stmts);
                GenerateStartFunction(modifiers, context, epcall, attributeList, parList);
            }
            else if (XSharpString.Compare(this._entryPoint, WellKnownMemberNames.EntryPointMethodName) != 0)
            {
                var methodcall = GenerateMethodCall(this._entryPoint, EmptyArgumentList(), true);
                if (isVoidType(returntype))
                {
                    stmts.Add(GenerateExpressionStatement(methodcall, context.Context()));
                }
                else
                {
                    stmts.Add(GenerateReturn(methodcall));
                }
                epcall = MakeBlock(stmts);
                GenerateStartFunction(modifiers, context, epcall, attributeList, parList);
            }

            ExpressionSyntax call;
            stmts.Clear();
            call = GenerateMethodCall(XSharpSpecialNames.ModuleName + "." + XSharpSpecialNames.AppInit, true);
            stmts.Add(GenerateExpressionStatement(call, context.Context()));
            stmts.Add(body);
            //var ame = _syntaxFactory.AnonymousMethodExpression(
            //    null,
            //    SyntaxFactory.MakeToken(SyntaxKind.DelegateKeyword),
            //    EmptyParameterList(),
            //    body);
            //ame.XGenerated = true;
            //var variables = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            //var vardecl = _syntaxFactory.VariableDeclarator(SyntaxFactory.MakeIdentifier(XSharpSpecialNames.ActionVariable), null,
            //   _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken), ame));
            //variables.Add(vardecl);
            //vardecl.XNode = stmts[0].XNode;
            //vardecl.XGenerated = true;
            //var varType = GenerateQualifiedName("System.Action");
            //if (!isVoidType(returntype))
            //{
            //    var typeArgs = _syntaxFactory.TypeArgumentList(
            //        SyntaxFactory.MakeToken(SyntaxKind.LessThanToken),
            //        MakeSeparatedList<TypeSyntax>(returntype),
            //        SyntaxFactory.MakeToken(SyntaxKind.GreaterThanToken)
            //        );
            //    varType = _syntaxFactory.GenericName(SyntaxFactory.MakeIdentifier("Func"), typeArgs);
            //}
            //var localdecl = _syntaxFactory.LocalDeclarationStatement(
            //        EmptyList<SyntaxToken>(),
            //        _syntaxFactory.VariableDeclaration(varType, variables),
            //        SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            ////localdecl.XGenerated = true;
            //localdecl.XNode = stmts[0].XNode;
            //stmts.Add(localdecl);
            //var invoke = _syntaxFactory.InvocationExpression(GenerateSimpleName(XSharpSpecialNames.ActionVariable), EmptyArgumentList());
            //invoke.XGenerated = true;
            //invoke.XNode = stmts[0].XNode;
            //if (isVoidType(returntype))
            //    stmts.Add(GenerateExpressionStatement(invoke));
            //else
            //    stmts.Add(GenerateReturn(invoke));
            var tryblock = MakeBlock(stmts);
            stmts.Clear();
            call = GenerateMethodCall(XSharpSpecialNames.ModuleName + "." + XSharpSpecialNames.AppExit, true);
            stmts.Add(GenerateExpressionStatement(call, context.Context(), true));
            stmts.Add(GenerateExpressionStatement(GenerateMethodCall(SystemQualifiedNames.GcCollect, true), context.Context(), true));
            stmts.Add(GenerateExpressionStatement(GenerateMethodCall(SystemQualifiedNames.GcWait, true), context.Context(), true));
            var finallyblock = MakeBlock(stmts);
            var finallyclause = _syntaxFactory.FinallyClause(SyntaxFactory.MakeToken(SyntaxKind.FinallyKeyword),
                    finallyblock);
            var trystmt = _syntaxFactory.TryStatement(
                 attributeLists: default,
                 SyntaxFactory.MakeToken(SyntaxKind.TryKeyword),
                 tryblock,
                 catches: default,
                 finallyclause);
            return MakeBlock(trystmt);
        }

        protected void GenerateStartFunction(SyntaxList<SyntaxToken> modifiers, [NotNull] XP.IMemberContext context, BlockSyntax body,
                    SyntaxList<AttributeListSyntax> attributeList, ParameterListSyntax parList)
        {
            // only
            var returntype = context.ReturnType.Get<TypeSyntax>() ?? voidType;
            if (parList.Parameters.Count > 0)
            {
                var parameter = parList.Parameters[0];
                var atype = parameter.Type as ArrayTypeSyntax;
                if (atype != null)
                {
                    var stringtype = _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.StringKeyword));
                    if (atype.ElementType != stringtype)
                    {
                        // need to convert parameters to string[]
                        var emptysizes = _pool.AllocateSeparated<ExpressionSyntax>();
                        emptysizes.Add(_syntaxFactory.OmittedArraySizeExpression(SyntaxFactory.MakeToken(SyntaxKind.OmittedArraySizeExpressionToken)));
                        var emptyrank = _syntaxFactory.ArrayRankSpecifier(
                              SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                              emptysizes,
                              SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken));
                        atype = _syntaxFactory.ArrayType(stringtype, emptyrank);
                        parameter = parameter.Update(
                            default,
                            default,
                            atype, parameter.Identifier, null);
                        parList = _syntaxFactory.ParameterList(parList.OpenParenToken, MakeSeparatedList(parameter), parList.CloseParenToken);
                        _pool.Free(emptysizes);
                    }
                }
                else
                {
                    parList = EmptyParameterList();
                    parList = parList.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_NoEntryPoint));
                }
            }
            var attr = _pool.Allocate<AttributeListSyntax>();
            foreach (var att in attributeList.Nodes)
            {
                if (att.ToString().IndexOf("ClipperCallingConventionAttribute") == -1)
                {
                    attr.Add(att);
                }
            }
            GenerateAttributeList(attr, SystemQualifiedNames.CompilerGenerated);
            attributeList = attr.ToList();
            _pool.Free(attr);
            var id = SyntaxFactory.Identifier(WellKnownMemberNames.EntryPointMethodName);
            var ep = _syntaxFactory.MethodDeclaration(
                attributeLists: attributeList,
                modifiers: modifiers,
                returnType: returntype,
                explicitInterfaceSpecifier: null,
                identifier: id,
                typeParameterList: null,
                parameterList: parList,
                constraintClauses: null,
                body: body,
                expressionBody: null,
                semicolonToken: (body != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            ep.XGenerated = true;
            ep.XNode = context;
            GlobalEntities.GlobalClassMembers.Add(ep);
        }

        #endregion

        #region Expression Statement

        protected override StatementSyntax HandleExpressionStmt(IList<XP.ExpressionContext> expressions)
        {
            var statements = _pool.Allocate<StatementSyntax>();
            StatementSyntax result;
            foreach (var exprCtx in expressions)
            {
                // check because there may already be statements in here, such as the IF statement generated for AltD()
                var node = exprCtx.CsNode;
                if (node is StatementSyntax)
                {
                    statements.Add((StatementSyntax)node);
                }
                else
                {
                    // check for binary expression with '=' operator.
                    // convert to assignment and add a warning when not in FoxPro dialect
                    // please note that the expression
                    // x = y = z
                    // is represented as
                    //    Binary Expression
                    //       Left = Binary Expression x == y
                    //       Op   = ==
                    //       Right = Simple Name z
                    // so we need to 'rebuild' the expression
                    exprCtx.SetSequencePoint(exprCtx.Start, exprCtx.Stop);
                    var expr = exprCtx.Get<ExpressionSyntax>();
                    if (expr is BinaryExpressionSyntax bin)
                    {
                        bool nestedAssign = false;
                        var xNode1 = bin.XNode as XP.BinaryExpressionContext;
                        var oldStyleAssign = bin.OperatorToken.Kind == SyntaxKind.EqualsEqualsToken && xNode1 != null && xNode1.Op.Type == XP.EQ;
                        if (bin.Left is BinaryExpressionSyntax binLeft)
                        {
                            if (binLeft.OperatorToken.Kind == SyntaxKind.EqualsEqualsToken)
                            {
                                var xNode2 = binLeft.XNode as XP.BinaryExpressionContext;
                                nestedAssign = xNode2 != null && xNode2.Op.Type == XP.EQ;
                            }
                        }
                        if (oldStyleAssign || nestedAssign)
                        {
                            ExpressionSyntax RHS = bin.Right;
                            ExpressionSyntax LHS = bin.Left;
                            if (nestedAssign)
                            {
                                // check for x = y = z, but also x = y > z
                                var Left = (BinaryExpressionSyntax)LHS;
                                LHS = Left.Left;
                                RHS = _syntaxFactory.BinaryExpression(
                                                    bin.Kind,
                                                    Left.Right,
                                                    bin.OperatorToken,
                                                    bin.Right);
                            }
                            // check to see if the LHS is Late bound access
                            // because we need to generate a IVarPut() then
                            var left = LHS.XNode;
                            if (left is XP.AccessMemberLateContext || left is XP.AccessMemberLateNameContext)
                            {
                                // lhs is then an InvocationExpression
                                var invoke = LHS as InvocationExpressionSyntax;
                                string putMethod = _options.XSharpRuntime ? XSharpQualifiedFunctionNames.IVarPut : VulcanQualifiedFunctionNames.IVarPut;
                                var obj = invoke.ArgumentList.Arguments[0];
                                var varName = invoke.ArgumentList.Arguments[1];
                                var args = MakeArgumentList(obj, varName, MakeArgument(RHS));
                                expr = GenerateMethodCall(putMethod, args, true);
                            }
                            else if (left is XP.PrimaryExpressionContext pec && pec.Expr is XP.MacroNameContext macro)
                            {
                                expr = GenerateMemVarPut(exprCtx, getMacroNameExpression(macro.Name), RHS);
                            }
                            else
                            {
                                expr = MakeSimpleAssignment(bin.Left, bin.Right);
                                RegisterParamAssign(bin.Left.XNode.GetText());
                            }
                        }
                        if (_options.Dialect != XSharpDialect.FoxPro)
                        {
                            expr = expr.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.WRN_AssignmentOperatorExpected));
                        }
                    }
                    var stmt = GenerateExpressionStatement(expr, exprCtx);
                    statements.Add(stmt);
                }
            }

            if (statements.Count == 1)
            {
                result = statements[0];
            }
            else
            {
                result = MakeBlock(statements);
            }
            _pool.Free(statements);
            return result;
        }
        #endregion

        #region MemVar and Fields
        internal ExpressionSyntax GenerateMemVarPut(XSharpParserRuleContext context, ExpressionSyntax memvar, ExpressionSyntax right)
        {
            var arg1 = MakeArgument(memvar);
            var arg2 = MakeArgument(right);
            var args = MakeArgumentList(arg1, arg2);
            var name = XSharpQualifiedFunctionNames.MemVarPut;
            var expr = GenerateMethodCall(name, args, true);
            return expr;
        }

        internal ExpressionSyntax GenerateMemVarGet(XSharpParserRuleContext context, ExpressionSyntax memvar)
        {
            // this is now only used in the aliasedFieldLate rule.
            var arg1 = MakeArgument(memvar);
            var args = MakeArgumentList(arg1);
            var expr = GenerateMethodCall(XSharpQualifiedFunctionNames.MemVarGet, args, true);
            return expr;
        }
        internal ExpressionSyntax GenerateMemVarDecl(XSharpParserRuleContext context, ExpressionSyntax memvar, bool isprivate)
        {
            var arg1 = MakeArgument(memvar);
            var arg2 = MakeArgument(GenerateLiteral(isprivate));
            var args = MakeArgumentList(arg1, arg2);
            var expr = GenerateMethodCall(XSharpQualifiedFunctionNames.MemVarDecl, args, true);
            expr.XGenerated = true;
            return expr;
        }

        internal ExpressionSyntax GenerateFieldGet(XSharpParserRuleContext context, string alias, ExpressionSyntax field)
        {
            // this is now only used in the aliasedFieldLate rule.
            if (string.IsNullOrEmpty(alias))
            {
                var argField = MakeArgument(field);
                var method = _options.XSharpRuntime ? XSharpQualifiedFunctionNames.FieldGet : VulcanQualifiedFunctionNames.FieldGet;
                var args = MakeArgumentList(argField);
                return GenerateMethodCall(method, args, true);
            }
            else
            {
                if (alias.ToUpper() == "M")
                {
                    return GenerateMemVarGet(context, field);
                }

                return GenerateFieldGetWa(context, GenerateLiteral(alias), field);
            }
        }

        internal ExpressionSyntax GenerateFieldGetWa(XSharpParserRuleContext context, ExpressionSyntax area, ExpressionSyntax field)
        {
            ArgumentListSyntax args;
            var argField = MakeArgument(field);
            var argWA = MakeArgument(area);
            var method = _options.XSharpRuntime ? XSharpQualifiedFunctionNames.FieldGetWa : VulcanQualifiedFunctionNames.FieldGetWa;
            args = MakeArgumentList(argWA, argField);
            var expr = GenerateMethodCall(method, args, true);
            return expr;
        }

        internal ExpressionSyntax GenerateFieldSetWa(XSharpParserRuleContext context, ExpressionSyntax area, ExpressionSyntax field, ExpressionSyntax value)
        {
            ArgumentListSyntax args;
            var argField = MakeArgument(field);
            var argWA = MakeArgument(area);
            var argValue = MakeArgument(value);
            var method = _options.XSharpRuntime ? XSharpQualifiedFunctionNames.FieldSetWa : VulcanQualifiedFunctionNames.FieldSetWa;
            args = MakeArgumentList(argWA, argField, argValue);
            return GenerateMethodCall(method, args, true);

        }

        protected ExpressionSyntax MakeMemVarField(MemVarFieldInfo fieldInfo)
        {
            if (fieldInfo.IsClipperParameter)
                return GenerateSimpleName(fieldInfo.Name);
            return GenerateSimpleName(fieldInfo.FullName);
        }

        public override void ExitNameExpression([NotNull] XP.NameExpressionContext context)
        {
            // Check to see if the name is a field or Memvar, registered with the FIELD or MemVar statement
            string Name = context.Name.GetText();
            ExpressionSyntax expr = context.Name.Get<NameSyntax>();
            // SomeVar(1,2) Can also be a FoxPro array access
            if (context.Parent.Parent is not XP.MethodCallContext ||
                (_options.HasOption(CompilerOption.FoxArraySupport, context, PragmaOptions)))
            {
                MemVarFieldInfo fieldInfo = findMemVar(Name);
                if (fieldInfo != null)
                {
                    expr = MakeMemVarField(fieldInfo);
                }
            }
            context.Put(expr);
        }

        #endregion

        #region Expressions

        protected ExpressionSyntax GenerateNIL()
        {
            if (_options.NoClipCall)
                return MakeDefault(_usualType);
            if (_options.XSharpRuntime)
                return GenerateQualifiedName(XSharpQualifiedFunctionNames.UsualNIL);
            else
                return GenerateQualifiedName(VulcanQualifiedFunctionNames.UsualNIL);

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
        protected override void VisitClassvar([NotNull] XP.ClassvarContext context, bool isFixed)
        {
            base.VisitClassvar(context, isFixed);
            if (context.ArraySub != null && context.Dim == null)
            {
                var vd = context.Get<VariableDeclarationSyntax>();
                var initializer = GenerateVOArrayInitializer(context.ArraySub);
                if (context.Initializer != null)
                {
                    // You cannot have both an  initializer initial Dimensions
                    initializer = initializer.WithAdditionalDiagnostics(
                            new SyntaxDiagnosticInfo(ErrorCode.ERR_VulcanArrayDimAndInit));
                }
                var variable = GenerateVariable(context.Id.GetText(), initializer);
                vd = vd.Update(vd.Type, MakeSeparatedList(variable));
                context.Put(vd);
            }
        }

        protected override ExpressionSyntax GenerateInitializer(XP.DatatypeContext datatype)
        {
            ExpressionSyntax value;
            if (datatype == null || datatype.Get<TypeSyntax>() == _usualType)
            {
                value = GenerateNIL();
                value.XGenerated = true;
                return value;
            }
            return base.GenerateInitializer(datatype);
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
                if (context.DataType == null)
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
                context.Expression.Put(initializer);
            }
            base.VisitLocalvar(context);
        }

        public override void ExitArrayOfType([NotNull] XP.ArrayOfTypeContext context)
        {
            var type = MakeGenericName(OurTypeNames.ArrayBase, context.TypeName.Get<TypeSyntax>());
            var qtype = _syntaxFactory.QualifiedName(GenerateSimpleName("XSharp"),
                SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                type);
            context.Put(qtype);
        }
        protected ExpressionSyntax GetAmpBasedName(IToken Amp, XP.IdentifierNameContext id)
        {
            if (Amp != null) // PUBLIC &cVarName
            {
                return getMacroNameExpression(id);
            }
            else // PUBLIC Bar
            {
                return GenerateLiteral(id.GetText());
            }
        }

        protected void AddAmpbasedMemvar(XSharpParserRuleContext context, string name, string alias, IToken amp)
        {
            name = CleanVarName(name);
            if (amp == null) // normal DIMENSION foo (1,2)
            {
                addFieldOrMemvar(name, alias, context, false);
            }
            else // DIMENSION &foo(1,2)
            {
                // Make name unique because they can use &name multiple times
                name += ":" + context.Position.ToString();
                addFieldOrMemvar(name, "&", context, false);
            }
        }

        protected MemVarFieldInfo addFieldOrMemvar(string name, string prefix, XSharpParserRuleContext context, bool isParameter)
        {
            if (CurrentMember.Data.GetField(name) != null)
            {
                context.AddError(new ParseErrorData(context, ErrorCode.ERR_MemvarFieldWithSameName, name));
                return null;
            }
            else
            {

                var info = CurrentMember.Data.AddField(name, prefix, context);
                info.IsParameter = isParameter;
                return info;
            }
        }
        protected string CleanVarName(string name)
        {
            if (name.EndsWith("."))
            {
                name = name.Substring(0, name.Length - 1);
            }
            return name;
        }
        public override void EnterXbasedecl([NotNull] XP.XbasedeclContext context)
        {
            // declare memvars
            context.SetSequencePoint(context.end);
            if (CurrentMember == null)
            {
                return;
            }
            if (_options.SupportsMemvars)
            {
                CurrentMember.Data.HasMemVars = true;
                if (context.T.Type == XP.MEMVAR || context.T.Type == XP.PARAMETERS)
                {
                    foreach (var memvar in context._Vars)
                    {
                        var name = CleanVarName(memvar.Id.GetText());
                        addFieldOrMemvar(name, "M", memvar, context.T.Type == XP.PARAMETERS);
                    }
                }
                else if (context.T.Type == XP.PUBLIC || context.T.Type == XP.PRIVATE)
                {
                    foreach (var memvar in context._XVars)
                    {
                        var name = memvar.Id.GetText();
                        AddAmpbasedMemvar(memvar, name, "M", memvar.Amp);
                    }
                }
            }
            if (context.T.Type == XP.LPARAMETERS)
            {
                foreach (var memvar in context._Vars)
                {
                    var name = CleanVarName(memvar.Id.GetText());
                    addFieldOrMemvar(name, XSharpSpecialNames.ClipperParamPrefix, memvar, true);
                }
            }
            if (context.T.Type == XP.PARAMETERS || context.T.Type == XP.LPARAMETERS)
            {
                // parameters and lparameters assume CC
                var member = CurrentMember;
                member.Data.HasClipperCallingConvention = true;
                if (member.Data.HasParametersStmt || member.Data.HasLParametersStmt || member.Data.HasFormalParameters)
                {
                    // trigger error message by setting both
                    // that way 2 x PARAMETERS or 2x LPARAMETERS will also trigger an error
                    member.Data.HasParametersStmt = true;
                    member.Data.HasLParametersStmt = true;
                }
                else
                {
                    member.Data.HasParametersStmt = (context.T.Type == XP.PARAMETERS);
                    member.Data.HasLParametersStmt = (context.T.Type == XP.LPARAMETERS);
                }
            }
        }

        public override void EnterFilewidememvar([NotNull] XP.FilewidememvarContext context)
        {
            if (_options.SupportsMemvars)
            {
                if (context.Token.Type == XP.PUBLIC)
                {
                    // PUBLIC
                    foreach (var memvar in context._XVars)
                    {
                        if (memvar.Amp == null)
                        {
                            var name = CleanVarName(memvar.Id.GetText());
                            var mv = new MemVarFieldInfo(name, "M", memvar, true);
                            _filewideMemvars.Add(mv.Name, mv);
                            GlobalEntities.FileWidePublics.Add(mv);
                        }
                        // Code generation for initialization is done in CreateInitFunction()
                    }
                }
                else
                {
                    Debug.Assert(context.Token.Type == XP.MEMVAR);
                    // MEMVAR
                    foreach (var memvar in context._Vars)
                    {
                        var name = CleanVarName(memvar.Id.GetText());
                        var mv = new MemVarFieldInfo(name, "M", memvar, true);
                        _filewideMemvars.Add(mv.Name, mv);
                    }
                }
            }
        }

        public override void ExitFilewidememvar([NotNull] XP.FilewidememvarContext context)
        {
            // implemented in EnterFilewidememvar
            return;
        }


        protected MemVarFieldInfo findVar(string name)
        {

            MemVarFieldInfo memvar = null;
            // First look in the FileWide Memvars
            name = CleanVarName(name);
            if (_options.SupportsMemvars)
            {
                _filewideMemvars.TryGetValue(name, out memvar);
            }
            // the next is also needed when memvars are not supported, since name could be a field
            if (memvar == null && CurrentMember != null)
            {
                memvar = CurrentMember.Data.GetField(name);
            }
            return memvar;
        }
        protected MemVarFieldInfo findMemVar(string name)
        {

            MemVarFieldInfo memvar = findVar(name);
            if (memvar != null && !memvar.IsLocal)
                return memvar;
            return null;
        }

        public override void ExitXbasedeclStmt([NotNull] XP.XbasedeclStmtContext context)
        {
            context.CsNode = context.Decl.CsNode; // in the case of script LPARAMEWTERS this is a list
        }
        public override void ExitXbasedecl([NotNull] XP.XbasedeclContext context)
        {
            context.SetSequencePoint(context.end);
            var stmts = _pool.Allocate<StatementSyntax>();
            switch (context.T.Type)
            {
                case XP.PARAMETERS:
                    int i = 0;
                    foreach (var memvar in context._Vars)
                    {
                        var name = memvar.GetText();
                        ++i;
                        var exp = GenerateMemVarDecl(context, GenerateLiteral(name), true);
                        exp.XNode = context;
                        stmts.Add(GenerateExpressionStatement(exp, context, true));

                        var val = GenerateGetClipperParam(GenerateLiteral(i), context);
                        exp = GenerateMemVarPut(context, GenerateLiteral(name), val);
                        var stmt = GenerateExpressionStatement(exp, context);
                        memvar.Put(stmt);
                        stmts.Add(stmt);
                    }
                    context.Put(MakeBlock(stmts));
                    break;
                case XP.PRIVATE:
                case XP.PUBLIC:
                    bool isprivate = context.T.Type == XP.PRIVATE;
                    foreach (var memvar in context._XVars)
                    {
                        var varname = GetAmpBasedName(memvar.Amp, memvar.Id.Id);
                        var exp = GenerateMemVarDecl(memvar, varname, isprivate);
                        exp.XNode = memvar;
                        stmts.Add(GenerateExpressionStatement(exp, memvar, true));
                        ExpressionSyntax initializer = null;
                        if (memvar.Expression != null)
                        {
                            initializer = memvar.Expression.Get<ExpressionSyntax>();
                        }
                        else if (memvar.ArraySub != null)
                        {
                            var args = new List<ArgumentSyntax>();
                            foreach (var index in memvar.ArraySub._ArrayIndex)
                            {
                                args.Add(MakeArgument(index.Get<ExpressionSyntax>()));
                            }
                            initializer = GenerateMethodCall(XSharpQualifiedFunctionNames.ArrayNew, MakeArgumentList(args.ToArray()), true);
                        }
                        if (initializer != null)
                        {
                            exp = GenerateMemVarPut(context, varname, initializer);

                            var stmt = GenerateExpressionStatement(exp,memvar);
                            memvar.Put(stmt);
                            stmts.Add(stmt);

                        }
                        // no need to assign a default. The runtime takes care of that
                    }
                    context.Put(MakeBlock(stmts));
                    break;

                case XP.MEMVAR:
                    // handled in the Enter method
                    context.Put(_syntaxFactory.EmptyStatement(
                        attributeLists: default,
                        SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                    break;
                default:
                    Debug.Assert(false, "Unknown type in XbaseDecl", "Type = " + context.T.Text);
                    break;
            }
            _pool.Free(stmts);

        }

        public override void ExitXbaseType([NotNull] XP.XbaseTypeContext context)
        {
            TypeSyntax type = null;
            switch (context.Token.Type)
            {
                case XP.ARRAY:
                    type = _arrayType;
                    break;
                case XP.BINARY:
                    type = _binaryType;
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
                case XP.CURRENCY:
                    type = _currencyType;
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
                type = NotInDialect(objectType, context.Token.Text);
            }
            context.Put(type);
        }

        #endregion

        #region Helpers for Method Init and Method Axit (/vo1)
        protected ConstructorDeclarationSyntax createConstructor(
            XP.IMemberWithBodyContext context,
            SyntaxList<SyntaxToken> modifiers,
            XP.AttributesContext atts,
            XP.ParameterListContext paramlist,
            XP.StatementBlockContext stmtblock,
            XSharpParserRuleContext errorcontext,
            XP.ConstructorchainContext chain = null,
            XP.ArgumentListContext args = null,
            bool isInInterface = false,
            IList<object> localFunctions = null)
        {
            var attributes = getAttributes(atts);
            var parameters = getParameters(paramlist);
            var body = processEntityBody(context);
            TypeSyntax returntype = null;
            if (chain != null && context.Data.HasClipperCallingConvention)
            {
                var chainArgs = args?.Get<ArgumentListSyntax>() ?? EmptyArgumentList();
                var chainExpr = MakeSimpleMemberAccess(
                    chain.Start.Type == XP.SELF ? GenerateSelf() : GenerateSuper(),
                    GenerateSimpleName(".ctor"));
                body = MakeBlock(MakeList<StatementSyntax>(
                    GenerateExpressionStatement(_syntaxFactory.InvocationExpression(chainExpr, chainArgs), context.Context()),
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
                typeId = getParentId(context as XSharpParserRuleContext);
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
                initializer: createInitializer(chain),
                body: body,
                expressionBody: null,
                semicolonToken: (stmtblock?._Stmts?.Count > 0) ? null :
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));

        }

        protected DestructorDeclarationSyntax createDestructor(
            XP.IMemberWithBodyContext context,
            SyntaxList<SyntaxToken> modifiers,
            XP.AttributesContext atts,
            XP.StatementBlockContext stmtblock,
            XSharpParserRuleContext errorcontext,
            bool isInInterface = false,
            IList<object> localFunctions = null)
        {
            // this is not caught in the ParserErrorAnalysis because this is an Axit method
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
                parentId = getParentId(context as XSharpParserRuleContext);
            }
            if (parentId == null)
                return null;
            var body = processEntityBody(context);

            return _syntaxFactory.DestructorDeclaration(
                attributeLists: getAttributes(atts),
                modifiers: modifiers,
                tildeToken: SyntaxFactory.MakeToken(SyntaxKind.TildeToken),
                identifier: parentId,
                parameterList: EmptyParameterList(),
                body: body,
                expressionBody: null,
                semicolonToken: (stmtblock != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
        }

        #endregion

        #region Helpers
        protected override TypeSyntax _getParameterType([NotNull] XP.ParameterContext context)
        {
            TypeSyntax type = context.Type?.Get<TypeSyntax>();
            if (type == null)
            {
                if (!_options.HasOption(CompilerOption.UntypedAllowed, context, PragmaOptions))
                    type = _getMissingType();
                else if (CurrentEntity != null && CurrentMember.Data.HasTypedParameter)
                    type = _usualType;
                else
                    type = _getMissingType();
            }
            return type;
        }

        protected override TypeSyntax DefaultType()
        {
            return _usualType;
        }

        protected override TypeSyntax _getMissingType()
        {
            TypeSyntax varType;
            if (_options.HasOption(CompilerOption.UntypedAllowed, (XSharpParserRuleContext)CurrentEntity, PragmaOptions))
                varType = _usualType;
            else
                varType = MissingType();
            return varType;
        }

        public override ConstructorDeclarationSyntax GenerateDefaultCtor(SyntaxToken id, XP.Class_Context classctx,
            SyntaxListBuilder<UsingDirectiveSyntax> usingslist, List<XSharpLanguageParser.PartialPropertyElement> elements)
        {
            ParameterListSyntax pars = GetClipperParameters();

            var arg = MakeArgument(GenerateSimpleName(XSharpSpecialNames.ClipperArgs));
            ArgumentListSyntax args = MakeArgumentList(arg);

            // add using statements
            if (elements != null )
            {
                foreach (var element in elements)
                {
                    foreach (var u in element.Usings)
                    {
                        var green = u.Green as UsingDirectiveSyntax;
                        this.AddUsingWhenMissing(usingslist, green.Name, green.StaticKeyword != null, green.Alias);
                    }
                }
            }

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
            ctor.XNode = classctx;
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

                if (!context.Data.Partial && !context.Data.HasInstanceCtor && _options.HasOption(CompilerOption.DefaultClipperContructors, context, PragmaOptions))
                {

                    var hasComImport = false;
                    foreach (var attlist in classdecl.AttributeLists)
                    {
                        for (int iAtt = 0; iAtt < attlist.Attributes.Count && !hasComImport; iAtt++)
                        {
                            var att = attlist.Attributes[iAtt];
                            if (XSharpString.Compare(att.Name.ToString(), "ComImport") == 0)
                            {
                                hasComImport = true;
                            }
                        }
                    }
                    //
                    // generate new class constructor
                    //
                    if (!hasComImport)
                    {
                        var ctor = GenerateDefaultCtor(classdecl.Identifier, context, default, null);
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
        }
        public override void ExitConstructor([NotNull] XP.ConstructorContext context)
        {
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility();
            if (mods.Any((int)SyntaxKind.StaticKeyword))
            {
                context.Data.HasClipperCallingConvention = false;
            }
            var ctor = createConstructor(context, mods, context.Attributes, context.ParamList, context.StmtBlk, context,
                context.Chain, context.Chain?.ArgList, context.isInInterface(), context.LocalFunctions);
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

        public override void ExitDefaultExpression([NotNull] XP.DefaultExpressionContext context)
        {
            base.ExitDefaultExpression(context);
            // replace Default(USUAL) with NIL
            var type = context.Type.Get<TypeSyntax>();
            if (type == _usualType)
            {
                context.Put(GenerateNIL());
            }
        }

        public override void ExitDestructor([NotNull] XP.DestructorContext context)
        {
            var modifiers = context.Modifiers?.GetList<SyntaxToken>() ?? default;
            var dtor = createDestructor(context, modifiers,
                context.Attributes, context.StmtBlk, context, context.isInInterface(), context.LocalFunctions);
            if (dtor != null)
            {
                context.Put(dtor);
            }
            else
            {
                base.ExitDestructor(context);
            }
            return;
        }

        public override void ExitCallingconvention([NotNull] XP.CallingconventionContext context)
        {
            if (CurrentMember != null)
            {
                if (context.Convention.Type == XP.CLIPPER && context.Parent == CurrentEntity)
                {
                    CurrentMember.Data.HasClipperCallingConvention = true;
                }
            }
            base.ExitCallingconvention(context);
        }

        public override void ExitMethod([NotNull] XP.MethodContext context)
        {
            if (context.Data.IsInitAxit)
            {
                var idName = context.Id.GetText();
                if (XSharpString.Equals(idName, XSharpIntrinsicNames.InitMethod))
                {
                    // Convert method to constructor
                    var mods = context.Modifiers?.GetList<SyntaxToken>() ?? MakeList<SyntaxToken>(SyntaxFactory.MakeToken(SyntaxKind.PublicKeyword));
                    var ctor = createConstructor(context,
                        mods, context.Attributes, context.ParamList, context.StmtBlk, context, null, null, false, context.LocalFunctions);
                    if (ctor != null)
                    {
                        if (!context.isInClass() && context.ClassId != null)
                        {
                            var cls = GenerateClassWrapper(context.ClassId.Get<SyntaxToken>(), ctor);
                            context.Put(cls);
                        }
                        else
                        {
                            context.Put(ctor);
                        }
                    }
                    else
                    {
                        context.StmtBlk = null;
                    }
                    return;
                }
                else if (XSharpString.Equals(idName, XSharpIntrinsicNames.AxitMethod))
                {
                    // Convert method to destructor
                    var mods = context.Modifiers?.GetList<SyntaxToken>() ?? default;
                    var dtor = createDestructor(context,
                        mods, context.Attributes, context.StmtBlk, context, false, context.LocalFunctions);
                    if (dtor != null)
                    {
                        if (!context.isInClass() && context.ClassId != null)
                        {
                            var cls = GenerateClassWrapper(context.ClassId.Get<SyntaxToken>(), dtor);
                            context.Put(cls);
                        }
                        else
                        {
                            context.Put(dtor);
                        }
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
                string method = _options.XSharpRuntime ? XSharpQualifiedFunctionNames.InStr : VulcanQualifiedFunctionNames.InStr;
                var argLeft = context.Left.Get<ExpressionSyntax>();
                var argRight = context.Right.Get<ExpressionSyntax>();
                var args = MakeArgumentList(MakeArgument(argLeft), MakeArgument(argRight));
                var expr = GenerateMethodCall(method, args, true);
                context.Put(expr);

                return;
            }
            else if (context.Op.Type == XP.DIV && _options.HasOption(CompilerOption.ClipperIntegerDivisions, context, PragmaOptions))
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
                context.Put(_syntaxFactory.ThrowStatement(attributeLists: default,
                    SyntaxFactory.MakeToken(SyntaxKind.ThrowKeyword),
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
            context.Put(GenerateExpressionStatement(call, context));
            return;
        }

        public override void ExitAccessMember([NotNull] XP.AccessMemberContext context)
        {
            base.ExitAccessMember(context);
        }

        protected void RegisterParamAssign(string name)
        {
            if (CurrentEntity != null)
            {
                var info = CurrentMember.Data.GetField(name);
                if (info != null && info.IsParameter)
                {
                    CurrentMember.Data.ParameterAssign = true;
                    info.IsWritten = true;
                }
            }
        }

        public override void ExitPrefixExpression([NotNull] XP.PrefixExpressionContext context)
        {
            ExpressionSyntax expr = context.Expr.Get<ExpressionSyntax>();
            if (expr is IdentifierNameSyntax ins)
            {
                // if Left is a Clipper parameter then set the flag on the current entity
                if (CurrentEntity != null && (context.Op.Type == XP.INC || context.Op.Type == XP.DEC))
                {
                    RegisterParamAssign(ins.Identifier.Text);
                }
                base.ExitPrefixExpression(context);
                return;
            }

            var Op = context.Op;
            // Check to see if the expression is a aliased field in the form of (nArea)->FIELD
            var fieldNode = getAliasedSubNode<XP.AliasedFieldContext>(context.Expr);
            var fieldNodeLate = getAliasedSubNode<XP.AliasedFieldLateContext>(context.Expr);

            if (fieldNode != null || fieldNodeLate != null)
            {
                // prefix on an aliased field
                ExpressionSyntax area;
                ExpressionSyntax field;
                if (fieldNode != null)
                {
                    // we only get here for the Area variant.
                    area = fieldNode.Area.Get<ExpressionSyntax>();
                    field = GenerateLiteral(fieldNode.Field.GetText());
                }
                else
                {
                    if (fieldNodeLate.Area != null)
                    {
                        area = fieldNodeLate.Area.Get<ExpressionSyntax>();
                    }
                    else
                    {
                        area = GenerateLiteral(fieldNodeLate.Alias.GetText());
                    }
                    field = GenerateIdentifier(fieldNodeLate.Field);
                }
                expr = GenerateFieldGetWa(context, area, field);
                switch (Op.Type)
                {
                    case XSharpLexer.INC:
                        expr = GenerateAddOne(expr);
                        expr = GenerateFieldSetWa(context, area, field, expr);
                        context.Put(expr);
                        return;
                    case XSharpLexer.DEC:
                        expr = GenerateSubtractOne(expr);
                        expr = GenerateFieldSetWa(context, area, field, expr);
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
            base.ExitPrefixExpression(context);
        }

        public override void ExitPostfixExpression([NotNull] XP.PostfixExpressionContext context)
        {
            ExpressionSyntax expr = context.Expr.Get<ExpressionSyntax>();
            //
            if (expr is IdentifierNameSyntax ins)
            {
                // if Left is a Clipper parameter then set the flag on the current entity
                if (CurrentEntity != null && (context.Op.Type == XP.INC || context.Op.Type == XP.DEC))
                {
                    RegisterParamAssign(ins.Identifier.Text);
                }
                base.ExitPostfixExpression(context);
                return;
            }
            var Op = context.Op;
            var fieldNode = getAliasedSubNode<XP.AliasedFieldContext>(context.Expr);
            var fieldNodeLate = getAliasedSubNode<XP.AliasedFieldLateContext>(context.Expr);
            if (fieldNode != null || fieldNodeLate != null)
            {
                // postfix on an aliased field
                // but not of the simple alias->fieldName type
                ExpressionSyntax area;
                ExpressionSyntax field;
                if (fieldNode != null)
                {
                    // we only get here when the Alias is null and Area is set
                    area = fieldNode.Area.Get<ExpressionSyntax>();
                    field = GenerateLiteral(fieldNode.Field.GetText());
                }
                else
                {
                    if (fieldNodeLate.Area != null)
                    {
                        area = fieldNodeLate.Area.Get<ExpressionSyntax>();
                    }
                    else
                    {
                        area = GenerateLiteral(fieldNodeLate.Alias.GetText());
                    }

                    field = GenerateIdentifier(fieldNodeLate.Field);
                }
                expr = GenerateFieldGetWa(context, area, field);
                if (Op.Type == XSharpLexer.INC)
                    expr = GenerateAddOne(expr);
                else
                    expr = GenerateSubtractOne(expr);
                expr = GenerateFieldSetWa(context, area, field, expr);
                context.Put(expr);
                return;

            }
            base.ExitPostfixExpression(context);
        }
        private T getAliasedSubNode<T>(XSharpParserRuleContext xnode) where T : XSharpParserRuleContext
        {
            if (xnode is XP.PrimaryExpressionContext prim)
            {
                if (xnode.GetChild(0) is XP.AliasedExpressionContext aexp)
                {
                    if (aexp.GetChild(0) is T subnode)
                    {
                        return subnode;
                    }
                }
            }
            return null;
        }

        protected ExpressionSyntax getMacroNameExpression(XP.IdentifierNameContext nameContext)
        {
            string name = nameContext.GetText();
            int pos = name.IndexOf('.');
            if (pos == -1)
            {
                return nameContext.Get<IdentifierNameSyntax>();
            }
            var baseName = name.Substring(0, pos);
            var addition = name.Substring(pos + 1);
            MemVarFieldInfo fieldInfo = findMemVar(baseName);
            ExpressionSyntax lhs;
            if (fieldInfo != null)
            {
                lhs = MakeMemVarField(fieldInfo);
            }
            else
            {
                lhs = SyntaxFactory.IdentifierName(SyntaxFactory.Identifier(baseName));
            }
            // & var..Method() is also allowed.
            if (string.IsNullOrEmpty(addition))
            {
                return lhs;
            }
            var rhs = GenerateLiteral(addition);
            var expr = _syntaxFactory.BinaryExpression(SyntaxKind.AddExpression,
                lhs, SyntaxFactory.MakeToken(SyntaxKind.PlusToken), rhs);
            return expr;
        }

        public override void ExitAssignmentExpression([NotNull] XP.AssignmentExpressionContext context)
        {
            // when /vo12 is used then for the types .ASSIGN_DIV add conversion for the LHS and RHS to Double
            // Check for Field or MemVar assignments
            ExpressionSyntax left = context.Left.Get<ExpressionSyntax>();
            XP.MacroNameContext macro = null;

            if (context.Left is XP.PrimaryExpressionContext pe1 && pe1.Expr is XP.MacroNameContext mnc)
            {
                macro = mnc;
            }
            // if Left is a Clipper parameter then set the flag on the current entity
            else if (CurrentEntity != null && left is IdentifierNameSyntax ins)
            {
                RegisterParamAssign(ins.Identifier.Text);
            }
            // Aliased fields and fields and memvars that were resolved are all stored as IdentifierNameSyntax
            if (left is IdentifierNameSyntax && !(context.Parent is XP.AliasedExprContext))
            {
                base.ExitAssignmentExpression(context);
                return;
            }

            ExpressionSyntax right = context.Right.Get<ExpressionSyntax>();
            var op = context.Op.ComplexToSimpleBinaryOp();
            var token = context.Op.ComplexToSimpleToken();
            if (context.Op.Type != XP.ASSIGN_OP && op == SyntaxKind.EmptyStatement)
            {
                var expr = GenerateLiteral(0);
                expr = NotInDialect(expr, "Complex operation: " + context.Op.Text);
                context.Put(expr);
                return;
            }

            if (macro != null && _options.SupportsMemvars)
            {
                ExpressionSyntax expr;
                var id = macro.Name.Get<IdentifierNameSyntax>();
                if (context.Op.Type == XP.ASSIGN_OP)
                {
                    expr = GenerateMemVarPut(context, getMacroNameExpression(macro.Name), right);
                }
                else
                {
                    // left is already a MemVarGet
                    expr = _syntaxFactory.BinaryExpression(op, left, token, right);
                    expr = GenerateMemVarPut(context, id, expr);
                }
                context.Put(expr);
                return;
            }
            // check to see if LHS is ALIAS->FIELD or ALIAS->&FIELD
            var fieldNode = getAliasedSubNode<XP.AliasedFieldContext>(context.Left);
            var fieldNodeLate = getAliasedSubNode<XP.AliasedFieldLateContext>(context.Left);
            if (fieldNode != null || fieldNodeLate != null)
            {
                // for fieldNode we only get here when the Area is set. Simple CUSTOMER->LASTNAME expressions are converted to an identifier
                // for FieldNodeLate we always get here

                ExpressionSyntax expr;
                ExpressionSyntax field;
                ExpressionSyntax area;
                if (fieldNode != null)
                {
                    field = GenerateLiteral(fieldNode.Field.GetText());
                    if (fieldNode.Area != null)
                        area = fieldNode.Area.Get<ExpressionSyntax>();
                    else
                        area = GenerateLiteral(fieldNode.Alias.GetText());
                }
                else // fieldNodeLate
                {
                    field = GenerateLiteral(fieldNodeLate.Field.GetText());
                    if (fieldNodeLate.Area != null)
                        area = fieldNodeLate.Area.Get<ExpressionSyntax>();
                    else
                        area = GenerateLiteral(fieldNodeLate.Alias.GetText());
                }
                if (context.Op.Type == XP.ASSIGN_OP)
                {
                    expr = GenerateFieldSetWa(context, area, field, right);
                }
                else
                {
                    expr = _syntaxFactory.BinaryExpression(op, left, token, right);
                    expr = GenerateFieldSetWa(context, area, field, expr);
                }
                context.Put(expr);
                return;
            }
            // Handle (SomeExpression)->FieldName := value
            if (left is IdentifierNameSyntax && context.Parent is XP.AliasedExprContext aec)
            {
                var alias = aec.Alias.Get<ExpressionSyntax>();
                string name = left.XNode.GetText();
                MemVarFieldInfo fieldInfo = findMemVar(name);
                ExpressionSyntax field;
                if (fieldInfo != null)
                {
                    field = MakeMemVarField(fieldInfo);
                }
                else
                {
                    fieldInfo = new MemVarFieldInfo(name, "", context.Left);
                    field = MakeMemVarField(fieldInfo);
                }
                context.Left.Put(field);
            }
            if (left.XNode is XP.AccessMemberLateContext || left.XNode is XP.AccessMemberLateNameContext)
            {
                var mcall = left as InvocationExpressionSyntax;
                var obj = mcall.ArgumentList.Arguments[0].Expression;
                var varName = mcall.ArgumentList.Arguments[1].Expression;
                string putMethod = _options.XSharpRuntime ? XSharpQualifiedFunctionNames.IVarPut : VulcanQualifiedFunctionNames.IVarPut;
                if (context.Op.Type == XP.ASSIGN_OP)
                {

                    var args = MakeArgumentList(MakeArgument(obj), MakeArgument(varName), MakeArgument(right));
                    var ivarput = GenerateMethodCall(putMethod, args, true);
                    context.Put(ivarput);
                }
                else
                {
                    string getMethod = _options.XSharpRuntime ? XSharpQualifiedFunctionNames.IVarGet : VulcanQualifiedFunctionNames.IVarGet;
                    var args = MakeArgumentList(MakeArgument(obj), MakeArgument(varName));
                    left = GenerateMethodCall(getMethod, args, true);
                    right = _syntaxFactory.BinaryExpression(op, left, token, right);
                    args = MakeArgumentList(MakeArgument(obj), MakeArgument(varName), MakeArgument(right));
                    var ivarput = GenerateMethodCall(putMethod, args, true);
                    context.Put(ivarput);
                }
                return;
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
                _options.XSharpRuntime ? XSharpQualifiedFunctionNames.EnterSequence : VulcanQualifiedFunctionNames.EnterSequence, true), context));
            stmts.Add(MakeBlock(context.StmtBlk.Get<BlockSyntax>()));
            var tryBlock = MakeBlock(stmts);
            stmts.Clear();
            stmts.Add(GenerateExpressionStatement(GenerateMethodCall(
                _options.XSharpRuntime ? XSharpQualifiedFunctionNames.ExitSequence : VulcanQualifiedFunctionNames.ExitSequence, true),context));
            var innerTry = _syntaxFactory.TryStatement(
                attributeLists: default,
                SyntaxFactory.MakeToken(SyntaxKind.TryKeyword),
                 tryBlock,
                 catches: default,
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
            StatementSyntax outerTry = _syntaxFactory.TryStatement(
                  attributeLists: default,
                  SyntaxFactory.MakeToken(SyntaxKind.TryKeyword),
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
                         GenerateSimpleName("Value"))), context);
                assign1.XGenerated = true;

                var assign2 = GenerateExpressionStatement(MakeSimpleAssignment(idName, objName), context);
                assign2.XGenerated = true;

                var assign3 = GenerateExpressionStatement(MakeSimpleAssignment(
                    idName, MakeCastTo(_usualType, GenerateMethodCall(
                        _options.XSharpRuntime ? XSharpQualifiedFunctionNames.WrapException : VulcanQualifiedFunctionNames.WrapException,
                    MakeArgumentList(MakeArgument(objName)), true))), context);
                assign3.XGenerated = true;

                var assign4 = GenerateExpressionStatement(MakeSimpleAssignment(idName, objName), context);
                assign4.XGenerated = true;

                var elseClause = _syntaxFactory.ElseClause(
                   SyntaxFactory.MakeToken(SyntaxKind.ElseKeyword),
                   assign4);

                // if 3
                var ifstmt = _syntaxFactory.IfStatement(
                            attributeLists: default,
                            SyntaxFactory.MakeToken(SyntaxKind.IfKeyword),
                            SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                            condition3,
                            SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                            assign3, elseClause);
                ifstmt.XGenerated = true;
                // if 3 is assigned to the else block of if 2
                elseClause = _syntaxFactory.ElseClause(
                    SyntaxFactory.MakeToken(SyntaxKind.ElseKeyword),
                    ifstmt);

                // if 2
                ifstmt = _syntaxFactory.IfStatement(
                            attributeLists: default,
                            SyntaxFactory.MakeToken(SyntaxKind.IfKeyword),
                            SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                            condition2,
                            SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                            assign2, elseClause);
                ifstmt.XGenerated = true;
                // if 2 is assigned to the else block of if 1
                elseClause = _syntaxFactory.ElseClause(
                    SyntaxFactory.MakeToken(SyntaxKind.ElseKeyword),
                    ifstmt);
                // if 1
                ifstmt = _syntaxFactory.IfStatement(
                            attributeLists: default,
                            SyntaxFactory.MakeToken(SyntaxKind.IfKeyword),
                            SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                            condition1,
                            SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                            assign1, elseClause);
                ifstmt.XGenerated = true;
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
            switch (stmt)
            {
                case XP.ReturnStmtContext:
                case XP.YieldStmtContext:
                    return false;

                case XP.JumpStmtContext jmpstmt:
                    if (jmpstmt.Key.Type == XP.THROW)
                        return false;
                    if (jmpstmt.Key.Type == XP.BREAK)
                        return false;
                    break;
                case XP.IfStmtContext ifstmt:
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
                    return NeedsReturn(elsestmt._Stmts);

                case XP.CaseStmtContext docasestmt:
                    var casestmt = docasestmt.CaseStmt;     // CaseBlock, there may be no blocks at all.
                    int lastkey = XP.CASE;
                    while (casestmt != null)                // otherwise is also a CaseBlock stored in NextCase
                    {
                        if (NeedsReturn(casestmt.StmtBlk._Stmts))
                            return true;
                        lastkey = casestmt.Key.Type;
                        casestmt = casestmt.NextCase;
                    }
                    return lastkey == XP.CASE; // There is no otherwise
                                               // all branches end with a return  statement

                case XP.SwitchStmtContext swstmt:
                    bool hasdefault = false;
                    foreach (var swBlock in swstmt._SwitchBlock)
                    {
                        if (swBlock.StmtBlk._Stmts.Count > 0 && NeedsReturn(swBlock.StmtBlk._Stmts))
                            return true;
                        if (swBlock.Key.Type != XP.CASE)
                            hasdefault = true;
                    }
                    return !hasdefault;

                // make sure Try and Seq are before IBlock because they are also blocks
                case XP.TryStmtContext trystmt:
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

                case XP.SeqStmtContext seqstmt:
                    if (NeedsReturn(seqstmt.StmtBlk._Stmts))
                        return true;
                    if (seqstmt.RecoverBlock == null)
                        return true;
                    return NeedsReturn(seqstmt.RecoverBlock.StmtBlock._Stmts);

                case XP.IBlockStmtContext blockstmt:
                    // For, Foreach, While, Repeat, but also BEGIN .. END and WITH .. END WITH
                    return NeedsReturn(blockstmt.Statements._Stmts);


            }

            return true;
        }
        private ExpressionSyntax GetReturnExpression(TypeSyntax returnType)
        {
            ExpressionSyntax result = null;
            if (isVoidType(returnType))
            {
                return null;
            }
            if (returnType == _pszType || returnType == _symbolType)
            {
                result = CreateObject(returnType, MakeArgumentList(MakeArgument(GenerateLiteral(""))));
            }
            else
            {
                result = MakeDefault(returnType);
            }
            return result;
        }

        protected override BlockSyntax AddMissingReturnStatement(BlockSyntax body, XP.StatementBlockContext stmtBlock, TypeSyntax returnType)
        {
            if (CurrentMember != null && !CurrentMember.Data.HasYield)
            {
                if (_options.HasOption(CompilerOption.AllowMissingReturns, stmtBlock, PragmaOptions) && stmtBlock != null && NeedsReturn(stmtBlock._Stmts))
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
            }
            return body;
        }

        public override void ExitReturnStmt([NotNull] XP.ReturnStmtContext context)
        {
            context.SetSequencePoint(context.end);
            if (context.IsInLambdaOrCodeBlock())
            {
                base.ExitReturnStmt(context);
                return;
            }

            var expr = context.Expr?.CsNode as ExpressionSyntax;
            // when / vo9 is enabled then add missing Expression
            var ent = CurrentMember;
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
                if (expr == null && _options.HasOption(CompilerOption.AllowMissingReturns, context, PragmaOptions) && !ent.Data.MustBeVoid)
                {
                    if (_options.Dialect != XSharpDialect.FoxPro)
                    {
                        errcode = ErrorCode.WRN_MissingReturnValue;
                    }
                    if (ent is XP.IMemberWithBodyContext ientbody)
                    {
                        TypeSyntax dataType;
                        if (ent.Data.HasMissingReturnType)
                        {
                            dataType = _getMissingType();
                        }
                        else
                        {
                            if (ientbody.ReturnType != null)
                                dataType = ientbody.ReturnType.Get<TypeSyntax>();
                            else
                                dataType = _getMissingType();
                        }
                        // calculate a new return value with a warning
                        if (_options.Dialect == XSharpDialect.FoxPro && dataType == _usualType)
                        {
                            expr = GenerateLiteral(true);
                        }
                        else
                        {
                            expr = GetReturnExpression(dataType);
                        }
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
                            var declstmt = GenerateReturnVar(_impliedType, expr);
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
                    case XP.DATETIME_CONST:
                        return _dateTimeType;
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
                if (expr.GetChild(0) is XP.LiteralExpressionContext lit)
                {
                    return lit;
                }
                if (expr.GetChild(0) is XP.ParenExpressionContext paren)
                {
                    return GetLiteralExpression(paren.Expr);
                }
            }
            return null;
        }
        private AttributeSyntax EncodeDefaultParameter(XP.ExpressionContext initexpr, XP.DatatypeContext datatype)
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
                var zero = GenerateLiteral(0);
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
                        return MakeDefaultParameter(GenerateLiteralNull(), GenerateLiteral(3));        // 3 = Symbol, value is empty
                    case XP.SYMBOL_CONST:
                        var symvalue = token.Text.Substring(1);
                        return MakeDefaultParameter(GenerateLiteral(symvalue), GenerateLiteral(3));      // 3 = Symbol, value is a string
                    case XP.NULL_PSZ:
                        return MakeDefaultParameter(GenerateLiteralNull(), GenerateLiteral(4));                       // 4 = PSZ, null = empty
                    case XP.NULL_PTR:
                        return MakeDefaultParameter(GenerateLiteral(0L), GenerateLiteral(5));            // 5 = IntPtr
                    case XP.NULL_STRING:
                        if (_options.HasOption(CompilerOption.NullStrings, initexpr, PragmaOptions))
                        {
                            return MakeDefaultParameter(GenerateLiteral(""), zero);               // 0 = regular .Net Value
                        }
                        else
                        {
                            return MakeDefaultParameter(GenerateLiteralNull(), zero);                          // 0 = regular .Net Value
                        }
                    case XP.NULL_ARRAY:
                    case XP.NULL_OBJECT:
                    case XP.NULL_CODEBLOCK:
                        return MakeDefaultParameter(GenerateLiteralNull(), zero);                          // 0 = regular .Net Value
                    case XP.INT_CONST:
                        var ts = datatype.Get<TypeSyntax>();
                        if (negative)
                        {
                            string text = token.Text;
                            switch (text[text.Length - 1])
                            {
                                case 'L':
                                case 'l':
                                case 'U':
                                case 'u':
                                    text = text.Substring(0, text.Length - 1);
                                    break;
                            }
                            Int64 iValue = Int64.Parse(text) * -1;
                            ExpressionSyntax expr1 = GenerateLiteral(iValue);
                            if (datatype is XP.SimpleDatatypeContext sdtc && sdtc.TypeName.NativeType != null)
                            {
                                expr1 = MakeCastTo(ts, expr1);
                            }
                            return MakeDefaultParameter(expr1, zero);   // 0 = regular .Net Value
                        }
                        else
                        {
                            ExpressionSyntax expr1 = GenerateLiteral(token, initexpr);
                            if (datatype is XP.SimpleDatatypeContext sdtc && sdtc.TypeName.NativeType != null)
                            {
                                expr1 = MakeCastTo(ts, expr1);
                            }
                            return MakeDefaultParameter(expr1, zero);   // 0 = regular .Net Value
                        }
                    case XP.REAL_CONST:
                        double dValue;
                        if (token.Text[0] == '$')
                        {
                            dValue = double.Parse(token.Text.Substring(1, token.Text.Length - 1), System.Globalization.CultureInfo.InvariantCulture);
                        }
                        else
                        {
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
                        }
                        if (negative)
                        {
                            return MakeDefaultParameter(GenerateLiteral(dValue * -1), zero);   // 0 = regular .Net Value
                        }
                        else
                        {
                            return MakeDefaultParameter(GenerateLiteral(dValue), zero);   // 0 = regular .Net Value
                        }

                    default:
                        return MakeDefaultParameter(GenerateLiteral(token), zero);   // 0 = regular .Net Value
                }
            }
        }

        public override void ExitParameter([NotNull] XP.ParameterContext context)
        {
            base.ExitParameter(context);
            // Only apply the default parameter attribute when there
            // are no Attributes on the parameter, such as [CallerMember]
            var inLocalFuncProc = CurrentMember is XP.LocalfuncprocContext;
            // For local function we cannot use the VO compatible defaults and also no attributes
            if (!inLocalFuncProc && context.Default != null && context.Attributes == null)
            {
                if (CurrentMember.Data.HasClipperCallingConvention)
                {
                    var par = context.CsNode as ParameterSyntax;
                    par = par.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_DefaultParameterValueWithClipperCallingConvention, context.Start.StartIndex, context.FullWidth));
                    context.Put(par);
                    return;
                }
                AttributeSyntax attr = EncodeDefaultParameter(context.Default, context.Type);
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
                                        name: GenerateQualifiedName(_actualType), argumentList: MakeAttributeArgumentList(arguments));
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

                var @params = new List<ParameterSyntax>();
                for (int i = 0; i < parameters.Parameters.Count; i++)
                {
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
                parameters = MakeParameterList(@params);
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
            expr = MakeCastTo(objectType, expr);
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
            var @params = new List<ParameterSyntax>();
            @params.Add(_syntaxFactory.Parameter(
                default,
                default,
                _ptrType,
                p,
                null));
            var pars = MakeParameterList(@params);
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
            name += UniqueNameSuffix(context);
            if (context.ArgList == null || context.ArgList?._Args.Count < 1)
            {
                context.Put(GenerateLiteral(0).WithAdditionalDiagnostics(
                    new SyntaxDiagnosticInfo(ErrorCode.ERR_BadArgCount, context.Expr.GetText(), 0)));
                return false;
            }
            // construct fake parameter list, all of type object
            var @params = new List<ParameterSyntax>();
            for (int i = 1; i < context.ArgList._Args.Count; i++)
            {
                var param = MakeParameter("$param" + i.ToString(), objectType);
                @params.Add(param);
            }
            var paramList = MakeParameterList(@params);
            var id = SyntaxFactory.Identifier(name);
            var mods = TokenList(SyntaxKind.InternalKeyword);
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
            var lsym = "_" + symbol.ToLower();
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
            if (!context.IsLiteralString())
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
                // case sensitive always !
                if (String.Compare(pair.Value.Item1, str) == 0)
                {
                    fieldname = pair.Key;
                    break;
                }
            }
            if (String.IsNullOrEmpty(fieldname))
            {
                fieldname = "_$psz_" + UniqueNameSuffix(context);
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
            //expr = expr.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.WRN_CompilerGeneratedPSZConversionGeneratesMemoryleak));
            return true;
        }

        private bool GeneratePCall(XP.MethodCallContext context)
        {
            // Return type and parameters should match the method prototype that the first parameter
            // For now we default to a return type of _objectType
            // points to. This is resolved in the binder and rewriter
            return AddPCallDelegate(context, XSharpSpecialNames.PCallPrefix, objectType);
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
            if (context.ArgList != null)
            {
                var args = context.ArgList.CsNode as ArgumentListSyntax;
                if (HasRefArguments(args, context))
                {
                    foreach (var arg in context.ArgList._Args)
                    {
                        if (arg.RefOut != null && arg.Expr != null)
                        {
                            RegisterParamAssign(arg.Expr.GetText());
                        }
                        else if (arg.Expr is XP.PrefixExpressionContext pec && pec.Op.Type == XP.ADDROF)
                        {
                            RegisterParamAssign(pec.Expr.GetText());
                        }
                    }
                }
            }
            string name = null;
            if (expr is IdentifierNameSyntax ins)
            {
                // Intrinsic functions that depend on Vulcan types
                name = ins.Identifier.Text.ToUpper();
                switch (name)
                {
                    case XSharpIntrinsicNames.PCall:
                    case XSharpIntrinsicNames.CCall:
                        if (GeneratePCall(context))
                            return;
                        break;
                    case XSharpIntrinsicNames.PCallNative:
                    case XSharpIntrinsicNames.CCallNative:
                        expr = GenerateLiteral(0).WithAdditionalDiagnostics(
                            new SyntaxDiagnosticInfo(ErrorCode.ERR_PCallNativeGenericType, ins.Identifier.Text));
                        context.Put(expr);
                        return;
                    case XSharpIntrinsicNames.String2Psz:
                    case XSharpIntrinsicNames.Cast2Psz:
                        if (GenerateString2Psz(context, name))
                            return;
                        break;
                    case XSharpIntrinsicNames.PCount:
                    case XSharpIntrinsicNames.ArgCount:
                    case XSharpIntrinsicNames.ClipperArgs:
                    case XSharpIntrinsicNames.GetMParam:
                    case XSharpIntrinsicNames.GetFParam:
                        if (CurrentEntity != null && CurrentMember.Data.HasClipperCallingConvention)
                        {
                            if (GenerateClipCallFunc(context, name))
                                return;
                        }

                        if (CurrentEntity?.isScript() == true)
                        {
                            if (GenerateClipCallFunc(context, name))
                                return;
                        }

                        if (CurrentEntity is XP.MethodContext mc)
                        {
                            if (mc.T.Token.Type == XP.ACCESS || mc.T.Token.Type == XP.ASSIGN)
                            {
                                if (name == XSharpIntrinsicNames.PCount || name == XSharpIntrinsicNames.ArgCount)
                                {
                                    if (GenerateClipCallFunc(context, XSharpIntrinsicNames.ArgCount))
                                        return;
                                }
                            }
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
                    case XSharpIntrinsicNames.PCallNative:
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
                if (XSharpString.Equals(methodName, XSharpIntrinsicNames.InitMethod))
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
                else if (XSharpString.Equals(methodName, XSharpIntrinsicNames.AxitMethod))
                {
                    var result = GenerateNIL();
                    result = result.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_CallingFinalizeDeprecated));
                    context.Put(result);
                    return;
                }
            }
            // all other method names or syntaxes
            base.ExitMethodCall(context);
            return;
        }

        public override void ExitXFunctionExpression([NotNull] XP.XFunctionExpressionContext context)
        {
            // Date (aParam) will then be seen as a voconversion expression
            // Date() without and with 3 parameters exist in the Runtime
            // For(..), Array(..), DateTime(..) are detected in the Lexer rule handleSpecialFunctions()
            var token = context.XFunc.Token;
            var args = context.ArgList?.Get<ArgumentListSyntax>();
            if (args == null)
            {
                args = EmptyArgumentList();
            }
            var count = args.Arguments.Count;
            if (token.Type == XP.DATE && count == 1) // cast number to date
            {
                ExpressionSyntax expr = args.Arguments[0].Expression;
                expr = MakeCastTo(_dateType, expr);
                context.Put(expr);
            }
            else
            {
                var name = token.Text;
                context.Put(GenerateMethodCall(name, args, false));
            }
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
                        var par = _syntaxFactory.Parameter(
                                        MakeCompilerGeneratedAttribute(),
                                        modifiers.ToList(),
                                        type: arrayOfUsual,
                                        identifier: SyntaxFactory.Identifier(XSharpSpecialNames.ClipperArgs),
                                        @default: null);
                        var pars = new List<ParameterSyntax>();
                        pars.Add(par);
                        _clipperParams = MakeParameterList(pars);
                        _pool.Free(modifiers);
                    }
                }
            }
            return _clipperParams;
        }
        protected void Check4ClipperCC(XP.IMemberContext context, IList<XP.ParameterContext> parameters, IToken Convention, XP.DatatypeContext returnType)
        {
            bool isEntryPoint = false;
            bool hasConvention = false;
            if (context is XP.FuncprocContext fc)
            {
                isEntryPoint = fc.Id.GetText().ToLower() == "start";
            }

            context.Data.HasTypedParameter = false;
            context.Data.HasMissingReturnType = (returnType == null);
            if (!context.Data.HasMissingReturnType)
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
                context.Data.HasClipperCallingConvention = _options.HasOption(CompilerOption.ClipperCallingConvention, (XSharpParserRuleContext)context, PragmaOptions) && !isEntryPoint;
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
                if (!context.Data.HasClipperCallingConvention && !isEntryPoint && !hasConvention && _options.HasOption(CompilerOption.UntypedAllowed, (XSharpParserRuleContext)context, PragmaOptions))
                    context.Data.HasClipperCallingConvention = !bHasTypedParameter;
                if (!bHasTypedParameter)
                {
                    foreach (XP.ParameterContext par in parameters)
                    {
                        CurrentMember.Data.AddField(par.Id.GetText(), XSharpSpecialNames.ClipperParamPrefix, par);
                    }
                }
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

        private void implementNoClipCall(XP.IMemberContext context, ref ParameterListSyntax parameters, ref TypeSyntax dataType)
        {
            if (!context.Data.HasClipperCallingConvention)
                return;
            if (context.Data.HasMissingReturnType)
                dataType = _usualType;
            if (parameters.Parameters.Count > 0)
            {
                var defExpr = _syntaxFactory.EqualsValueClause(
                    SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                    MakeDefault(_usualType));

                var @params = new List<ParameterSyntax>();
                for (int i = 0; i < parameters.Parameters.Count; i++)
                {
                    var parm = parameters.Parameters[i];
                    var par = _syntaxFactory.Parameter(
                          attributeLists: default, //attrlist,
                          modifiers: default,
                          type: _usualType,
                          identifier: parm.Identifier, @default: defExpr);
                    @params.Add(par);
                }
                parameters = MakeParameterList(@params);
            }
        }
        protected override void ImplementClipperAndPSZ(XP.IMemberWithBodyContext context,
            ref SyntaxList<AttributeListSyntax> attributes, ref ParameterListSyntax parameters, ref BlockSyntax body,
            ref TypeSyntax dataType)
        {
            var prc = (XSharpParserRuleContext)context;
            var inLocalFuncProc = context is XP.LocalfuncprocContext;
            InitializeArrayTypes();
            if (context.Data.HasTypedParameter && context.Data.HasClipperCallingConvention)
            {
                parameters = parameters.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_TypedParametersWithClipperCallingConvention));
                return;
            }
            var count = 0;
            count += context.Data.HasFormalParameters ? 1 : 0;
            count += context.Data.HasParametersStmt ? 1 : 0;
            count += context.Data.HasLParametersStmt ? 1 : 0;
            if (count > 1)
            {
                body = body.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_ParametersWithDeclaredParameters));
                return;
            }
            List<string> parameternames = new List<String>();
            if (_options.NoClipCall && body != null && !(context is XP.PropertyAccessorContext))
            {
                // Bring body back to a simple "throw null" Assign xnode so we will have line numbers and a [Source] button in the docs.
                var stmts = new List<StatementSyntax>();
                var stmt = _syntaxFactory.ThrowStatement(attributeLists: default,
                    SyntaxFactory.MakeToken(SyntaxKind.ThrowKeyword),
                    GenerateLiteralNull(),
                    SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                stmt.XNode = context;
                stmts.Add(stmt);
                body = MakeBlock(stmts);
                context.Data.UsesPSZ = false;
            }
            if (context.Data.HasClipperCallingConvention || context.Data.UsesPSZ || context.Data.HasMemVars || _options.HasOption(CompilerOption.UndeclaredMemVars, prc, PragmaOptions))
            {
                var stmts = _pool.Allocate<StatementSyntax>();
                var finallystmts = _pool.Allocate<StatementSyntax>();
                if (context.Data.HasClipperCallingConvention && _options.NoClipCall)
                {
                    implementNoClipCall(context, ref parameters, ref dataType);
                    context.Data.HasClipperCallingConvention = false;
                }

                if (context.Data.HasClipperCallingConvention && !_options.NoClipCall)
                {

                    // Assuming the parameters are called oPar1 and oPar2 then the following code is generated
                    // LOCAL oPar1 := iif(Xs$Args:Length > 0,  Xs$Args[0], NIL) as USUAL
                    // LOCAL oPar2 := iif(Xs$Args:Length > 1,  Xs$Args[1], NIL) as USUAL
                    // Create the ClipperCallingConventionAttribute for the method/function
                    // using the names from the paramNames list
                    // [ClipperCallingConvention(new string[] { "a", "b" })]
                    // make sure that existing attributes are not removed!

                    if (context.Data.HasParametersStmt || context.Data.HasLParametersStmt)
                    {
                        foreach (var field in context.Data.Fields.Values)
                        {
                            if (field.IsParameter && !parameternames.Contains(field.Name))
                            {
                                parameternames.Add(field.Name);
                            }
                        }
                    }
                    else
                    {
                        for (int i = 0; i < parameters.Parameters.Count; i++)
                        {
                            var parm = parameters.Parameters[i];
                            string name = parm.Identifier.Text;
                            parameternames.Add(name);
                        }
                    }
                    // we create clipper calling convention attributes for methods
                    // we declared parameters, a parameters statement or a lparameters statement
                    var attrs = _pool.Allocate<AttributeListSyntax>();
                    attrs.AddRange(attributes); // Copy existing attributes
                    var names = new List<ExpressionSyntax>();
                    foreach (var name in parameternames)
                    {
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
                    var len = MakeConditional(notnull, argLen, GenerateLiteral(0));

                    var decl = GenerateLocalDecl(XSharpSpecialNames.ClipperPCount, _intType, len);
                    decl.XGenerated = true;
                    stmts.Add(decl);
                    // Now Change argument to X$Args PARAMS USUAL[]
                    var newparameters = GetClipperParameters();
                    // do not generate locals for parameters declared with the PARAMETERS statement
                    if (context.Data.HasFormalParameters || context.Data.HasLParametersStmt)
                    {
                        int i = 1;
                        foreach (var name in parameternames)
                        {
                            decl = GenerateLocalDecl(name, _usualType, GenerateGetClipperParam(GenerateLiteral(i), prc));
                            decl.XGenerated = true;
                            var variable = decl.Declaration.Variables[0];
                            variable.XGenerated = true;
                            stmts.Add(decl);
                            i++;
                        }
                    }
                    // Copy error messages from the declared parameters (for example when default values are defined)
                    if (context.Data.HasFormalParameters && parameters.Parameters.Count > 0)
                    {
                        for (int i = 0; i < parameters.Parameters.Count; i++)
                        {
                            var parm = parameters.Parameters[i];
                            var diag = parm.GetDiagnostics();
                            if (diag.Length > 0)
                                newparameters = newparameters.WithAdditionalDiagnostics(diag);
                        }
                    }
                    parameters = newparameters;
                }
                if (body != null)
                {
                    FinallyClauseSyntax finallyClause = null;

                    if (context.Data.UsesPSZ)
                    {
                        // VAR Xs$PszList := List<IntPtr>{}
                        // and in the finally
                        // String2PszRelease(Xs$PszList)
                        var listOfIntPtr = _syntaxFactory.QualifiedName(GenerateQualifiedName(SystemQualifiedNames.CollectionsGeneric),
                            SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                            MakeGenericName("List", _ptrType));
                        var expr = CreateObject(listOfIntPtr, EmptyArgumentList());
                        stmts.Add(GenerateLocalDecl(XSharpSpecialNames.VoPszList, _impliedType, expr));
                        finallystmts.Add(GenerateExpressionStatement(
                                    GenerateMethodCall(
                                        _options.XSharpRuntime ? XSharpQualifiedFunctionNames.PszRelease : VulcanQualifiedFunctionNames.PszRelease,
                                        MakeArgumentList(MakeArgument(GenerateSimpleName(XSharpSpecialNames.VoPszList))), true), null));
                    }
                    if (parameternames.Count > 0 && CurrentMember.Data.ParameterAssign)
                    {
                        var updatestmt = GenerateGetClipperByRefAssignParam(parameternames, context.Data, (XSharpParserRuleContext)context);
                        finallystmts.Add(updatestmt);

                    }
                    if (context.Data.HasMemVars || _options.HasOption(CompilerOption.UndeclaredMemVars, prc, PragmaOptions))
                    {
                        // VAR Xs$PrivatesLevel := XSharp.RT.Functions.__MemVarInit()
                        // in the finally
                        // XSharp.RT.Functions.__MemVarRelease(Xs$PrivatesLevel)
                        var expr = GenerateMethodCall(XSharpQualifiedFunctionNames.MemVarInit, EmptyArgumentList(), true);
                        var decl = GenerateLocalDecl(XSharpSpecialNames.PrivatesLevel, _impliedType, expr);
                        stmts.Add(decl);
                        var arg = MakeArgument(GenerateSimpleName(XSharpSpecialNames.PrivatesLevel));
                        expr = GenerateMethodCall(XSharpQualifiedFunctionNames.MemVarRelease, MakeArgumentList(arg), true);
                        finallystmts.Add(GenerateExpressionStatement(expr, (XSharpParserRuleContext) context, true));
                        context.Data.HasMemVarLevel = true;
                    }
                    if (finallystmts.Count > 0)
                    {
                        finallyClause = _syntaxFactory.FinallyClause(
                            SyntaxFactory.MakeToken(SyntaxKind.FinallyKeyword),
                            MakeBlock(finallystmts));
                    }
                    // TRY
                    //    original body
                    // FINALLY                                             < == Always
                    //    CompilerServices.String2PszRelease(Xs$PszList)   < == only for HasPsz(), otherwise empty
                    //    Assign Byref params back to Xs$Args
                    //    and XSharp.RT.Functions.__MemVarRelease(Xs$PrivatesLevel)
                    // END TRY

                    var tryStmt = finallyClause == null ? (StatementSyntax)body :
                        _syntaxFactory.TryStatement(
                            attributeLists: default,
                        SyntaxFactory.MakeToken(SyntaxKind.TryKeyword),
                        body,
                        null,
                        finallyClause);
                    stmts.Add(tryStmt);
                    body = MakeBlock(stmts);
                }
                _pool.Free(stmts);
                _pool.Free(finallystmts);
            }
            // Add missing return type when needed. OBJECT or USUAL depending on the dialect.
            if (context.Data.HasMissingReturnType && !context.Data.MustBeVoid)
            {
                dataType = _getMissingType();
            }
        }

        private ExpressionSyntax GenerateSubtractOne(ExpressionSyntax expr)
        {
            var lit = GenerateLiteral(1);
            expr = _syntaxFactory.BinaryExpression(SyntaxKind.SubtractExpression, expr, SyntaxFactory.MakeToken(SyntaxKind.MinusToken), lit);
            return expr;
        }
        private ExpressionSyntax GenerateAddOne(ExpressionSyntax expr)
        {
            var lit = GenerateLiteral(1);
            expr = _syntaxFactory.BinaryExpression(SyntaxKind.AddExpression, expr, SyntaxFactory.MakeToken(SyntaxKind.PlusToken), lit);
            return expr;
        }

        private ExpressionSyntax _GenerateString2Psz(XSharpParserRuleContext context, ExpressionSyntax expr)
        {
            if (CurrentEntity?.isScript() == true)
            {
                NameSyntax pszlist = GenerateSimpleName(XSharpSpecialNames.ScriptVoPszList);
                var argList = MakeArgumentList(MakeArgument(expr), MakeArgument(pszlist));
                expr = GenerateMethodCall(
                    _options.XSharpRuntime ? XSharpQualifiedFunctionNames.String2Psz : VulcanQualifiedFunctionNames.String2Psz,
                    argList, true);
                var args = MakeArgumentList(MakeArgument(expr));
                expr = CreateObject(this._pszType, args);
                return expr;
            }
            if (CurrentMember != null)
            {
                CurrentMember.Data.UsesPSZ = true;
                NameSyntax pszlist = GenerateSimpleName(XSharpSpecialNames.VoPszList);
                var argList = MakeArgumentList(MakeArgument(expr), MakeArgument(pszlist));
                expr = GenerateMethodCall(
                    _options.XSharpRuntime ? XSharpQualifiedFunctionNames.String2Psz : VulcanQualifiedFunctionNames.String2Psz,
                    argList, true);
                var args = MakeArgumentList(MakeArgument(expr));
                expr = CreateObject(this._pszType, args);
                expr.XIsString2Psz = true;
                return expr;
            }
            expr = CreateObject(this.objectType, EmptyArgumentList());
            expr.XIsString2Psz = true;
            expr = expr.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_String2PszMustBeAssignedToLocal));
            return expr;
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

        protected ExpressionSyntax GenerateGetClipperParam(ExpressionSyntax expr, XSharpParserRuleContext context)
        {
            // Note that the expr must result into a 1 based offset or (with /az) a 0 based offset
            // XS$PCount > ..
            BinaryExpressionSyntax cond;
            if (CurrentMember != null)
                CurrentMember.Data.UsesPCount = true;
            // no changes to expr for length comparison, even with /az
            cond = _syntaxFactory.BinaryExpression(
                                SyntaxKind.GreaterThanOrEqualExpression,
                                GenerateSimpleName(CurrentEntity?.isScript() != true ? XSharpSpecialNames.ClipperPCount : XSharpSpecialNames.ScriptClipperPCount),
                                SyntaxFactory.MakeToken(SyntaxKind.GreaterThanToken),
                                expr);
            // XS$Args[..]
            if (_options.HasOption(CompilerOption.ArrayZero, context, PragmaOptions))
            {
                // adjust array offset when compiling with /az
                expr = GenerateSubtractOne(expr);
            }
            var indices = _pool.AllocateSeparated<ArgumentSyntax>();
            indices.Add(MakeArgument(expr));
            var left = _syntaxFactory.ElementAccessExpression(
                GenerateSimpleName(CurrentEntity?.isScript() != true ? XSharpSpecialNames.ClipperArgs : XSharpSpecialNames.ScriptClipperArgs),
                _syntaxFactory.BracketedArgumentList(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                    indices,
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)));

            var result = MakeConditional(cond, left, GenerateNIL());
            _pool.Free(indices);
            return result;
        }
        private StatementSyntax GenerateGetClipperByRefAssignParam(List<string> paramNames, XP.MemberData data, XSharpParserRuleContext context)
        {
            // Note that the expr must result into a 1 based offset or (with /az) a 0 based offset
            // XS$PCount > ..
            var localnames = new List<string>();
            localnames.AddRange(paramNames);
            localnames.Reverse();
            var index = localnames.Count;
            StatementSyntax last = null;
            var indices = _pool.AllocateSeparated<ArgumentSyntax>();
            foreach (var paramName in localnames)
            {
                var info = data.GetField(paramName);
                if (info == null || info.IsWritten)
                {
                    ExpressionSyntax expr = GenerateLiteral(index);
                    BinaryExpressionSyntax cond;
                    // no changes to expr for length comparison, even with /az
                    cond = _syntaxFactory.BinaryExpression(
                                        SyntaxKind.GreaterThanOrEqualExpression,
                                        GenerateSimpleName(XSharpSpecialNames.ClipperPCount),
                                        SyntaxFactory.MakeToken(SyntaxKind.GreaterThanToken),
                                        expr);
                    // XS$Args[..]
                    if (_options.HasOption(CompilerOption.ArrayZero, context, PragmaOptions))
                    {
                        // adjust array offset when compiling with /az
                        expr = GenerateSubtractOne(expr);
                    }
                    indices.Clear();
                    indices.Add(MakeArgument(expr));
                    var left = _syntaxFactory.ElementAccessExpression(
                        GenerateSimpleName(XSharpSpecialNames.ClipperArgs),
                        _syntaxFactory.BracketedArgumentList(
                            SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                            indices,
                            SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)));
                    ExpressionSyntax right;

                    if (data.HasParametersStmt)
                    {
                        right = GenerateSimpleName(XSharpSpecialNames.MemVarPrefix + "->" + paramName);
                    }
                    else
                    {
                        right = GenerateSimpleName(paramName);
                    }
                    var assign = GenerateExpressionStatement(MakeSimpleAssignment(left, right), context);
                    if (last != null)
                    {
                        var list = new List<StatementSyntax>();
                        list.Add(assign);
                        list.Add(last);
                        var block = MakeBlock(list);
                        block.XGenerated = true;
                        last = GenerateIfStatement(cond, block, null);
                        last.XGenerated = true;
                    }
                    else
                    {
                        last = GenerateIfStatement(cond, assign, null);
                        last.XGenerated = true;
                    }
                }
                index -= 1;
            }
            _pool.Free(indices);
            return last;
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
            if (name == XSharpIntrinsicNames.ClipperArgs)
            {
                context.Put(GenerateSimpleName(CurrentEntity?.isScript() != true ? XSharpSpecialNames.ClipperArgs : XSharpSpecialNames.ScriptClipperArgs));
                return true;
            }
            if (name == XSharpIntrinsicNames.ArgCount)
            {
                // Number of declared arguments in the function/methods
                if (CurrentMember != null)
                {
                    var currEnt = this.CurrentMember;
                    int argCount = 0;
                    if (currEnt != null && currEnt.Params != null)
                    {
                        argCount = currEnt.Params._Params.Count;
                    }
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
            if (name == XSharpIntrinsicNames.PCount)
            {
                if (_options.NoClipCall)
                {
                    var currEnt = this.CurrentMember;
                    int argCount = 0;
                    if (currEnt != null && currEnt.Params != null)
                    {
                        argCount = currEnt.Params._Params.Count;
                    }
                    expr = GenerateLiteral(argCount);
                    context.Put(expr);
                }
                else
                {
                    if (CurrentMember != null)
                    {
                        CurrentMember.Data.UsesPCount = true;
                    }
                    expr = GenerateSimpleName(CurrentEntity?.isScript() != true ? XSharpSpecialNames.ClipperPCount : XSharpSpecialNames.ScriptClipperPCount);
                    if (argList.Arguments.Count != 0)
                    {
                        expr = expr.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_BadArgCount, name, argList.Arguments.Count));
                    }
                    context.Put(expr);
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
                    context.Put(GenerateGetClipperParam(argList.Arguments[0].Expression, context));
                }
                return true;
            }
        }

        public override void EnterMethod([NotNull] XP.MethodContext context)
        {
            base.EnterMethod(context);
            Check4ClipperCC(context, context.ParamList?._Params, context.CallingConvention?.Convention, context.ReturnType);
            switch (context.RealType)
            {
                case XP.ACCESS:
                case XP.ASSIGN:
                    context.Data.HasClipperCallingConvention = false;
                    context.Data.HasTypedParameter = true;          // this will set all missing types to USUAL
                    break;
                case XP.METHOD:
                    if (_options.VoInitAxitMethods && !context.isInInterface())
                    {
                        var idName = context.Id.GetText();
                        if (XSharpString.Equals(idName, XSharpIntrinsicNames.InitMethod)
                            || XSharpString.Equals(idName, XSharpIntrinsicNames.AxitMethod))
                        {
                            context.Data.MustBeVoid = true;
                            context.Data.IsInitAxit = true;
                        }
                    }
                    break;
                default:
                    break;
            }
        }
        public override void EnterFuncproc([NotNull] XP.FuncprocContext context)
        {
            base.EnterFuncproc(context);
            Check4ClipperCC(context, context.ParamList?._Params, context.CallingConvention?.Convention, context.ReturnType);
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

        protected ExpressionSyntax GenerateVOArrayInitializer([NotNull] XP.ArraysubContext arraysub)
        {
            var args = new List<ArgumentSyntax>();
            foreach (var index in arraysub._ArrayIndex)
            {
                args.Add(MakeArgument(index.Get<ExpressionSyntax>()));
            }
            var initializer = GenerateMethodCall(
                _options.XSharpRuntime ? XSharpQualifiedFunctionNames.ArrayNew : VulcanQualifiedFunctionNames.ArrayNew,
                MakeArgumentList(args.ToArray()), true);
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
                        l.Add(GenerateNIL());

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
            context.Put(CreateObject(_arrayType, MakeArgumentList(MakeArgument(expr))));

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
                    expr = MakeDefault(_pszType);
                    break;
                case XP.NULL_ARRAY:
                    expr = MakeDefault(_arrayType);
                    break;
                case XP.NULL_CODEBLOCK:
                    expr = MakeDefault(_codeblockType);
                    break;
                case XP.NULL_DATE:
                    expr = GenerateMethodCall(_options.XSharpRuntime ? XSharpQualifiedFunctionNames.NullDate : VulcanQualifiedFunctionNames.NullDate, EmptyArgumentList(), true);
                    break;
                case XP.NULL_SYMBOL:
                    expr = MakeDefault(_symbolType);
                    break;
                case XP.BINARY_CONST:
                    base.ExitLiteralValue(context);
                    expr = context.Get<ExpressionSyntax>();
                    expr = CreateObject(_binaryType, MakeArgumentList(MakeArgument(expr)));

                    break;
                case XP.DATE_CONST:
                    var elements = DecodeDateConst(context.Token.Text);
                    if (elements != null)
                    {
                        arg0 = MakeArgument(GenerateLiteral(elements[0]));
                        arg1 = MakeArgument(GenerateLiteral(elements[1]));
                        arg2 = MakeArgument(GenerateLiteral(elements[2]));
                        expr = CreateObject(_dateType, MakeArgumentList(arg0, arg1, arg2));
                    }
                    break;
                // handled in base class
                // case XP.DATETIME_CONST:
                case XP.SYMBOL_CONST:
                    // call helper method that will create a symbol for the symboltable
                    expr = GenerateLiteralSymbol(context.Token.Text);
                    break;
                case XP.REAL_CONST:
                    var text = context.Token.Text;
                    if (text[0] == '$')
                    {
                        var dec = GenerateLiteral(context.Start, context);
                        if (!(CurrentEntity is XP.VodefineContext))
                        {
                            arg0 = MakeArgument(dec);
                            expr = CreateObject(_currencyType, MakeArgumentList(arg0));
                        }
                        else
                        {
                            expr = dec;
                        }
                    }
                    else if (_options.HasOption(CompilerOption.FloatConstants, context, PragmaOptions) && !(CurrentEntity is XP.VodefineContext))
                    {
                        // check to see if the token contains an 'S', 'D' or 'M'. In that case leave as is, since the user has specified
                        // single, double or decimal or currency
                        if (text.IndexOfAny("sdmSDM".ToCharArray()) == -1)
                        {
                            args = text.Split('.');
                            if (args.Length == 2)
                            {
                                int dec = args[1].Length;
                                arg0 = MakeArgument(GenerateLiteral(context.Token, context));
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
            if (block == null)
            {
                // No need to do anything. We now support codeblocks with an expression body
                /*
                var cbc = context.Parent as XP.CodeblockContext;
                if (cbc?.lambda == null)
                {
                    if (cbc?.LambdaParamList == null || cbc?.LambdaParamList.ImplicitParams != null)
                    {
                        block = MakeBlock(GenerateReturn((ExpressionSyntax)context.CsNode));
                        context.Put(block);
                    }
                }
                */
            }
            if (context.Expr == null && context.ExprList == null && context.StmtBlk == null)
            {
                // empty codeblock ?
                var cbcontext = context.Parent as XP.CodeblockContext;
                if (cbcontext?.lambda == null)
                {
                    //block = MakeBlock(GenerateReturn(GenerateNIL()));
                    //context.Put(block);
                    // We now support codeblocks with an expression body
                    context.Put(GenerateNIL());
                }
            }
        }
        #endregion

        #region Workarea and Macro
        public override void ExitFielddecl([NotNull] XP.FielddeclContext context)
        {
            var stmt = _syntaxFactory.EmptyStatement(attributeLists: default, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            context.SetSequencePoint();
            context.Put(stmt);
            return;
        }
        public override void EnterFielddecl([NotNull] XP.FielddeclContext context)
        {
            // register field names with current entity
            // so we can check for the field name in the ExitNameExpr method
            string alias = "";
            if (context.Alias != null)
                alias = context.Alias.GetText();
            if (CurrentEntity != null)
            {
                foreach (var field in context._Fields)
                {
                    var name = field.Id.GetText();
                    addFieldOrMemvar(name, alias, field, false);
                }
            }
        }

        public CSharpSyntaxNode GenerateAliasedExpression(
            [NotNull] XSharpParserRuleContext context,
            ExpressionSyntax wa, ExpressionSyntax expr)
        {
            // Adjust the expression that is evaluated in the other workarea
            /*
              | ( Id=identifier | LPAREN Alias=expression RPAREN)
                       ALIAS ( (LPAREN Expr=expression RPAREN)
                      | Expr=expression )
            */
            // there are 4 variations:
            // workarea = Id or Alias
            // expression between parens or not
            //
            // assignments in the RHS are handled in the ExitAssignmentExpression

            var push = GenerateMethodCall(_options.XSharpRuntime ? XSharpQualifiedFunctionNames.PushWorkarea : VulcanQualifiedFunctionNames.PushWorkarea, MakeArgumentList(MakeArgument(wa)), true);
            var pop = GenerateMethodCall(_options.XSharpRuntime ? XSharpQualifiedFunctionNames.PopWorkarea : VulcanQualifiedFunctionNames.PopWorkarea, EmptyArgumentList(), true);
            var pushStmt = GenerateExpressionStatement(push, context, true);
            var popStmt = GenerateExpressionStatement(pop, context, true);
            // we mark the popStmt as generated so there is no extra stop in the debugger.
            // we do not mark the pushStmt as generated, so a select to an invalid workarea will show the right line number
            if (context.Parent.Parent.Parent is XP.ExpressionStmtContext)
            {
                // context.Parent is always a primaryexpression
                // if context.Parent.Parent is a Expressionstatement then we do not have
                // save the return value of the expression
                pushStmt.XNode = wa.XNode;
                popStmt.XNode = expr.XNode;
                var list = new List<StatementSyntax>() { pushStmt, GenerateExpressionStatement(expr, context), popStmt };
                return MakeBlock(list);
            }

            if (_options.XSharpRuntime)
            {
                // Convert to generic method call that takes care of switching workareas
                // alias can be a literal, or variable
                // __AreaEval ( alias, { => Expr })
                expr = _syntaxFactory.ParenthesizedLambdaExpression(
                        modifiers: default,
                        parameterList: EmptyParameterList(),
                        arrowToken: SyntaxFactory.MakeToken(SyntaxKind.EqualsGreaterThanToken),
                        block: null,
                        expressionBody: expr);
                var args = MakeArgumentList(MakeArgument(wa), MakeArgument(expr));
                var mcall = GenerateMethodCall(XSharpQualifiedFunctionNames.AreaEval, args);
                context.Put(mcall);
                return mcall;
            }

            // Vulcan does not have __AreaEval()
            // So we generate the following
            // CUSTOMER->(<Expression>)
            //
            // translate to a lambda with the following contents:
            //
            //  {  =>
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
                        modifiers: default,
                        parameterList: EmptyParameterList(),
                        arrowToken: SyntaxFactory.MakeToken(SyntaxKind.EqualsGreaterThanToken),
                        block: MakeBlock(MakeList<StatementSyntax>(
                            pushStmt,
                            _syntaxFactory.TryStatement(
                                attributeLists: default,
                                SyntaxFactory.MakeToken(SyntaxKind.TryKeyword),
                                MakeBlock(MakeList<StatementSyntax>(GenerateReturn(expr))),
                                catches: default,
                                _syntaxFactory.FinallyClause(SyntaxFactory.MakeToken(SyntaxKind.FinallyKeyword),
                                    MakeBlock(MakeList<StatementSyntax>(popStmt))
                                    )
                                )
                            )),
                        expressionBody: null
                        )
                    ),
                _syntaxFactory.IdentifierName(SyntaxFactory.MakeIdentifier(ReservedNames.Eval))
                ),
            EmptyArgumentList());

        }

        public override void ExitAliasedMemvar([NotNull] XP.AliasedMemvarContext context)
        {
            // | MEMVAR ALIAS VarName=identifier   #aliasedMemvar        // MEMVAR->Name
            string field = context.VarName.GetText();
            var name = GenerateSimpleName(XSharpSpecialNames.MemVarPrefix + "->" + field);
            context.Put(name);
            return;
        }

        public override void ExitAliasedExpression([NotNull] XP.AliasedExpressionContext context)
        {
            context.Put(context.Expr.Get<CSharpSyntaxNode>());
            context.XSharpRuntime = _options.XSharpRuntime;
        }

        public override void ExitAliasedField([NotNull] XP.AliasedFieldContext context)
        {
            /*
            | FIELD ALIAS (Alias=identifier ALIAS)? Field=identifier    #aliasedField		      // _FIELD->CUSTOMER->NAME is equal to CUSTOMER->NAME
            | Alias=identifier ALIAS Field=identifier                   #aliasedField		      // CUSTOMER->NAME,
            | LPAREN Area=identifier RPAREN ALIAS Field=identifier      #aliasedField		      // (nCust)->NAME
            */
            var fldName = context.Field.GetText();
            if (context.Area != null)
            {
                // 3rd syntax, Area is an identifier, (area)->Name
                var id = GenerateIdentifier(context.Area);
                context.Area.Put(id);
                context.Put(GenerateFieldGetWa(context, id, GenerateLiteral(fldName)));
            }
            else
            {
                //_FIELD->NAME, CUSTOMER-NAME, _FIELD->CUSTOMER->NAME
                if (context.Alias == null)
                {
                    var info = new MemVarFieldInfo(fldName, "", context);
                    context.Put(MakeMemVarField(info));
                }
                else
                {
                    var alias = context.Alias.GetText();
                    var info = new MemVarFieldInfo(fldName, alias, context);
                    context.Put(MakeMemVarField(info));
                }
            }
            return;
        }
        public override void ExitAliasedFieldLate([NotNull] XP.AliasedFieldLateContext context)
        {
            /*
                    | Alias=identifier              ALIAS AMP Field=expression  #aliasedFieldLate	    // CUSTOMER->&fldName
                    | FIELD ALIAS (Alias=identifier ALIAS)? AMP Field=expression #aliasedFieldLate	  // _FIELD->CUSTOMER->&fldName or _FIELD->&fldName
                    | LPAREN Area=identifier RPAREN ALIAS AMP Field=expression  #aliasedFieldLate	    // (nCust)->&fldName

            */

            var fieldName = context.Field.Get<ExpressionSyntax>();
            if (context.Area != null)
            {
                context.Put(GenerateFieldGetWa(context, GenerateIdentifier(context.Area), fieldName));
            }
            else
            {
                string alias = null;
                if (context.Alias != null)
                {
                    alias = context.Alias.GetText();
                }
                context.Put(GenerateFieldGet(context, alias, fieldName));
            }
        }

        public override void ExitAliasedExpr([NotNull] XP.AliasedExprContext context)
        {
            /*
            | ( Id=identifier | LPAREN Alias=expression RPAREN)
                ALIAS ( (LPAREN Expr=expression RPAREN) | Expr=expression )
           */
            ExpressionSyntax alias;
            // check if Expr is simple name

            if (context.Expr.IsIdentifier())
            {
                string field = context.Expr.GetText();
                var varName = field;
                // assume it is a field
                var info = new MemVarFieldInfo(varName, "", context);
                if (context.Id != null || context.Alias.IsIdentifier())
                {
                    string name;
                    if (context.Id != null)
                        name = context.Id.GetText();
                    else
                        name = context.Expr.GetText();
                    info = new MemVarFieldInfo(varName, name, context);
                    context.Put(MakeMemVarField(info));
                }
                else
                {
                    var fldGet = MakeMemVarField(info);
                    alias = context.Alias.Get<ExpressionSyntax>();
                    var aexpr = GenerateAliasedExpression(
                                context,
                                alias,     // workarea
                                fldGet // expression
                                );

                    context.Put(aexpr);
                }
                return;
            }

            else
            {
                if (context.Id != null)
                {
                    alias = GenerateLiteral(context.Id.GetText());
                }
                else if (context.Alias.IsIdentifier())
                {
                    alias = GenerateSimpleName(context.Alias.GetText());
                }
                else
                {
                    alias = context.Alias.Get<ExpressionSyntax>();
                }
                var expr = GenerateAliasedExpression(
                            context,
                            alias,     // workarea
                            context.Expr.Get<ExpressionSyntax>() // expression
                        );
                context.Put(expr);
            }
        }
        public override void ExitMacro([NotNull] XP.MacroContext context)
        {
            // & LPAREN expression RPAREN
            ExpressionSyntax expr;
            expr = context.Expr.Get<ExpressionSyntax>();
            var args = MakeArgumentList(MakeArgument(expr));
            context.SetSequencePoint();
            string methodName = _options.XSharpRuntime ? XSharpQualifiedFunctionNames.Evaluate : VulcanQualifiedFunctionNames.Evaluate;
            expr = GenerateMethodCall(methodName, args, true);
            context.Put(expr);
            return;
        }
        public override void ExitMacroName([NotNull] XP.MacroNameContext context)
        {
            // &identifierName
            context.SetSequencePoint();
            if (_options.SupportsMemvars && context.Name.GetText().IndexOf(".") > 0)
            {
                var id = getMacroNameExpression(context.Name);
                var expr = GenerateMemVarGet(context, id);
                context.Put(expr);
            }
            else
            {
                var name = context.Name.Get<IdentifierNameSyntax>();
                var args = MakeArgumentList(MakeArgument(name));
                string methodName = _options.XSharpRuntime ? XSharpQualifiedFunctionNames.Evaluate : VulcanQualifiedFunctionNames.Evaluate;
                var expr = GenerateMethodCall(methodName, args, true);
                context.Put(expr);
            }
            return;
        }
        public override void ExitAccessMemberLate([NotNull] XP.AccessMemberLateContext context)
        {
            // expression:&(expression)
            // needs to translate to either IVarGet() or IVarPut() when the parent is a assignment expression
            var left = context.Left.Get<ExpressionSyntax>();
            var right = context.Right.Get<ExpressionSyntax>();
            var args = MakeArgumentList(MakeArgument(left), MakeArgument(right));
            string methodName = _options.XSharpRuntime ? XSharpQualifiedFunctionNames.IVarGet : VulcanQualifiedFunctionNames.IVarGet;
            var ivarget = GenerateMethodCall(methodName, args, true);
            context.Put(ivarget);
        }
        public override void ExitAccessMemberLateName([NotNull] XP.AccessMemberLateNameContext context)
        {
            // expression:&identifierName
            // needs to translate to either IVarGet() or IVarPut() when the parent is a assignment expression
            /*
            | Left=expression Op=(DOT | COLON) AMP LPAREN Right=expression RPAREN  #accessMemberLate
                // aa:&(Expr). Expr must evaluate to a string which is the ivar name
                // can become IVarGet() or IVarPut when this expression is the LHS of an assignment
            | Left=expression Op=(DOT | COLON) AMP Name=identifierName  #accessMemberLateName
                // aa:&Name  Expr must evaluate to a string which is the ivar name
            */
            var left = context.Left.Get<ExpressionSyntax>();
            var right = getMacroNameExpression(context.Name);
            var args = MakeArgumentList(MakeArgument(left), MakeArgument(right));
            string methodName = _options.XSharpRuntime ? XSharpQualifiedFunctionNames.IVarGet : VulcanQualifiedFunctionNames.IVarGet;
            var ivarget = GenerateMethodCall(methodName, args, true);
            context.Put(ivarget);

        }
        #endregion

        #region Conversions and Typecasts
        public override void ExitVoConversionExpression([NotNull] XP.VoConversionExpressionContext context)
        {

            // Special case for PSZ(..)
            // PSZ("String") becomes String2Psz("String")
            // USUAL(<expr>) gets simplified to <expr>
            if (context.XType != null)
            {
                var xtype = context.XType as XP.XbaseTypeContext;
                switch (xtype.Token.Type)
                {
                    case XP.PSZ:
                        ExpressionSyntax expr;
                        if (GenerateLiteralPsz(context.Expr, out expr))
                        {
                            context.Put(expr);
                            return;
                        }
                        break;
                    case XP.USUAL:
                        // no typecast needed
                        context.Put(context.Expr.Get<ExpressionSyntax>());
                        return;
                }
            }
            base.ExitVoConversionExpression(context);
        }

        public override void ExitTypeCast([NotNull] XP.TypeCastContext context)
        {
            // Special case for (PSZ) Expression, is stored in the PSZ Table when expression is a literal
            // (USUAL) <expr> gets simplified to <expr>
            var dt = context.Type as XP.DatatypeContext;
            if (dt is XP.SimpleDatatypeContext sdt)
            {
                if (sdt.TypeName.XType != null)
                {
                    switch (sdt.TypeName.XType.Token.Type)
                    {
                        case XP.PSZ:
                            ExpressionSyntax expr;
                            if (GenerateLiteralPsz(context.Expr, out expr))
                            {
                                context.Put(expr);
                                return;
                            }
                            break;
                        //case XP.USUAL:
                        //    // no typecast needed
                        //    context.Put(context.Expr.Get<ExpressionSyntax>());
                        //    return;
                        default:
                            break;
                    }
                }
            }
            base.ExitTypeCast(context);
            return;
        }

        public override void ExitVoCastExpression([NotNull] XP.VoCastExpressionContext context)
        {
            // PSZ(_CAST, literal) is stored in the PSZ Table
            // USUAL(_CAST, <expr>) gets simplified to <expr>
            if (context.XType != null)
            {
                var xtype = context.XType as XP.XbaseTypeContext;
                switch (xtype.Token.Type)
                {
                    case XP.PSZ:
                        ExpressionSyntax expr;
                        if (GenerateLiteralPsz(context.Expr, out expr))
                        {
                            context.Put(expr);
                            return;
                        }
                        break;
                    case XP.USUAL:
                        // no typecast needed for CASTCLASS because that is handled later
                        if (context.Start.Type == XP.CASTCLASS)
                        {
                            context.Put(context.Expr.Get<ExpressionSyntax>());
                            return;
                        }
                        break;
                }
            }
            if (context.Type != null)
            {
                var type = context.Type as XP.NativeTypeContext;
                switch (type.Token.Type)
                {
                    case XP.PTR:
                        if (context.Expr is XP.PrimaryExpressionContext pe
                            && pe.Expr is XP.LiteralExpressionContext le
                            && le.Literal.Token.IsZeroLiteral()) // treat PTR(_CAST,0) as NULL_PTR
                        {
                            context.Put(MakeSimpleMemberAccess(_ptrType, GenerateSimpleName("Zero")));
                            return;
                        }
                        break;
                }
            }
            base.ExitVoCastExpression(context);
        }

        #endregion
    }
}

