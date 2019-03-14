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
using System.Text;
using System.Threading;
using Roslyn.Utilities;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Diagnostics; // PLEASE DO NOT REMOVE THIS!!!!
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;
using Microsoft.CodeAnalysis.PooledObjects;
namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    using Microsoft.CodeAnalysis.Syntax.InternalSyntax;

    internal class XSharpTreeTransformation : XSharpBaseListener
    {
        #region Nested classes
        internal class SyntaxEntities
        {
            internal SyntaxListPool _pool;
            public SyntaxListBuilder<ExternAliasDirectiveSyntax> Externs;
            public SyntaxListBuilder<UsingDirectiveSyntax> Usings;
            public SyntaxListBuilder<AttributeListSyntax> Attributes;
            public SyntaxListBuilder<MemberDeclarationSyntax> Members;
            public SyntaxListBuilder<MemberDeclarationSyntax> GlobalClassMembers;
            public SyntaxListBuilder<MemberDeclarationSyntax> StaticGlobalClassMembers;
            public object LastMember;
            public bool LastIsStatic;
            public List<Tuple<int, String>> InitProcedures;
            public List<FieldDeclarationSyntax> Globals;
            public bool HasPCall;
            public bool NeedsProcessing;

            internal SyntaxEntities(SyntaxListPool pool)
            {
                Externs = pool.Allocate<ExternAliasDirectiveSyntax>();
                Usings = pool.Allocate<UsingDirectiveSyntax>();
                Attributes = pool.Allocate<AttributeListSyntax>();
                Members = pool.Allocate<MemberDeclarationSyntax>();
                GlobalClassMembers = pool.Allocate<MemberDeclarationSyntax>();
                StaticGlobalClassMembers = pool.Allocate<MemberDeclarationSyntax>();
                Members = pool.Allocate<MemberDeclarationSyntax>();
                InitProcedures = new List<Tuple<int, String>>();
                Globals = new List<FieldDeclarationSyntax>();
                _pool = pool;
                HasPCall = false;
                NeedsProcessing = false;
                LastIsStatic = false;
                LastMember = null;
            }

            internal void Free()
            {
                _pool.Free(Members);
                _pool.Free(Attributes);
                _pool.Free(Usings);
                _pool.Free(Externs);
            }
        }

        internal class SyntaxClassEntities
        {
            internal class VoPropertyInfo
            {
                internal SyntaxToken idName;
                internal XP.MethodContext AccessMethodCtx = null;
                internal XP.MethodContext AssignMethodCtx = null;
                internal XP.MethodContext DupAccess = null;
                internal XP.MethodContext DupAssign = null;
            }

            internal SyntaxListPool _pool;
            public SyntaxListBuilder<MemberDeclarationSyntax> Members;
            public Dictionary<string, VoPropertyInfo> VoProperties;

            internal SyntaxClassEntities(SyntaxListPool pool)
            {
                Members = pool.Allocate<MemberDeclarationSyntax>();
                _pool = pool;
            }

            internal void Free()
            {
                _pool.Free(Members);
            }

            internal void AddVoPropertyAccessor(XP.MethodContext accessor)
            {
                if (VoProperties == null)
                    VoProperties = new Dictionary<string, VoPropertyInfo>(CaseInsensitiveComparison.Comparer);
                string name = accessor.Id.Get<SyntaxToken>().Text;
                VoPropertyInfo propertyInfo;
                if (!VoProperties.TryGetValue(name, out propertyInfo))
                {
                    propertyInfo = new VoPropertyInfo();
                    propertyInfo.idName = accessor.Id.Get<SyntaxToken>();
                    VoProperties.Add(name, propertyInfo);
                }
                switch (accessor.T.Token.Type)
                {
                    case XP.ACCESS:
                        if (propertyInfo.AccessMethodCtx != null)
                            propertyInfo.DupAccess = accessor;
                        else
                            propertyInfo.AccessMethodCtx = accessor;
                        break;
                    case XP.ASSIGN:
                        if (propertyInfo.AssignMethodCtx != null)
                            propertyInfo.DupAssign = accessor;
                        else
                            propertyInfo.AssignMethodCtx = accessor;
                        break;
                    default:
                        break;
                }
            }
        }
        #endregion

        #region Fields
        private static int _unique = 0;
        protected static object gate = new object();

        protected string GlobalClassName = XSharpSpecialNames.FunctionsClass;

        internal SyntaxListPool _pool;
        protected readonly ContextAwareSyntax _syntaxFactory; // Has context, the fields of which are resettable.
        protected readonly XSharpParser _parser;
        protected readonly CSharpParseOptions _options;
        protected readonly TypeSyntax _ptrType;
        protected readonly TypeSyntax _objectType;
        protected readonly TypeSyntax _voidType;
        protected readonly TypeSyntax _impliedType;
        protected readonly TypeSyntax _stringType;
        protected readonly TypeSyntax _intType;


        protected string _fileName;
        protected bool _isScript;
        protected string _entryPoint;

        internal SyntaxEntities GlobalEntities;
        internal SyntaxClassEntities GlobalClassEntities;
        internal Stack<SyntaxClassEntities> ClassEntities = new Stack<SyntaxClassEntities>();
        internal Stack<XP.IEntityContext> Entities = new Stack<XP.IEntityContext>();
        #endregion

        #region Static Fields
        static TypeSyntax intType = null;
        static TypeSyntax uintType = null;
        static TypeSyntax floatType = null;
        static TypeSyntax decimalType = null;
        static TypeSyntax doubleType = null;
        static TypeSyntax ulongType = null;
        static TypeSyntax longType = null;
        static TypeSyntax stringType = null;
        static readonly object oGate = new object();
        #endregion  


        #region Properties
        protected XP.IEntityContext CurrentEntity
        {
            get
            {
                if (Entities.Count > 0)
                    return Entities.Peek();
                return null;
            }
        }
        #endregion

        public static string GlobalFunctionClassName(XSharpTargetDLL targetDLL)
        {
            string className;
            switch (targetDLL)
            {
                case XSharpTargetDLL.Core:
                    className = XSharpSpecialNames.XSharpCoreFunctionsClass;
                    break;
                case XSharpTargetDLL.RDD:
                    className = XSharpSpecialNames.XSharpRDDFunctionsClass;
                    break;
                case XSharpTargetDLL.RT:
                    className = XSharpSpecialNames.XSharpRTFunctionsClass;
                    break;
                case XSharpTargetDLL.VO:
                    className = XSharpSpecialNames.XSharpVOFunctionsClass;
                    break;
                case XSharpTargetDLL.XPP:
                    className = XSharpSpecialNames.XSharpXPPFunctionsClass;
                    break;
                default:
                    className = XSharpSpecialNames.FunctionsClass;
                    break;
            }
            return className;

        }

        public virtual string GetGlobalClassName(XSharpTargetDLL targetDLL)
        {
            return GlobalFunctionClassName(targetDLL);
        }

        #region Construction and destruction
        public XSharpTreeTransformation(XSharpParser parser, CSharpParseOptions options, SyntaxListPool pool,
            ContextAwareSyntax syntaxFactory, string fileName)
        {
            _pool = pool;
            _syntaxFactory = syntaxFactory;
            _parser = parser;
            _options = options;
            _isScript = options.Kind == SourceCodeKind.Script;
            GlobalClassName = GetGlobalClassName(_options.TargetDLL);
            GlobalEntities = CreateEntities();
            _ptrType = GenerateQualifiedName(SystemQualifiedNames.IntPtr);
            _objectType = _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.ObjectKeyword));
            _voidType = _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.VoidKeyword));
            _stringType = _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.StringKeyword));
            _intType = _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.IntKeyword));
            _impliedType = GenerateSimpleName(XSharpSpecialNames.ImpliedTypeName);
            _fileName = fileName;
            _entryPoint = "Start";
            initTypes();
        }


        public static SyntaxTree DefaultXSharpSyntaxTree(IEnumerable<SyntaxTree> trees, bool isApp, XSharpTargetDLL targetDLL)
        {
            // trees is NOT used here, but it IS used in the VOTreeTransForm
            var opt = CSharpParseOptions.Default;
            XSharpSpecificCompilationOptions xopt = new XSharpSpecificCompilationOptions();
            xopt.TargetDLL = targetDLL;
            opt = opt.WithXSharpSpecificOptions(xopt);
            var t = new XSharpTreeTransformation(null, opt, new SyntaxListPool(), new ContextAwareSyntax(new SyntaxFactoryContext()), "");

            string globalClassName = t.GetGlobalClassName(targetDLL);

            t.GlobalEntities.Members.Add(t.GenerateGlobalClass(globalClassName, false, true));
            var eof = SyntaxFactory.Token(SyntaxKind.EndOfFileToken);
            var tree = CSharpSyntaxTree.Create(
                (Syntax.CompilationUnitSyntax)t._syntaxFactory.CompilationUnit(
                    t.GlobalEntities.Externs,
                    t.GlobalEntities.Usings,
                    t.GlobalEntities.Attributes,
                    t.GlobalEntities.Members, eof).CreateRed());
            return tree;
        }


        internal void Free()
        {
            GlobalEntities.Free();
        }

        internal SyntaxEntities CreateEntities()
        {
            return new SyntaxEntities(_pool);
        }

        internal SyntaxClassEntities CreateClassEntities()
        {
            return new SyntaxClassEntities(_pool);
        }
        #endregion

        #region Entitynames

        protected CSharpSyntaxNode CheckTypeName(XP.IEntityContext context, string typeKind, CSharpSyntaxNode node)
        {
            if (context.Parent.Parent is XP.Namespace_Context)
                return node;
            string name = context.Name;
            if (string.Compare(this.GlobalClassName, 0, name + ".", 0, name.Length + 1, StringComparison.OrdinalIgnoreCase) == 0)
            {
                node = node.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_TypeNameMatchesGlobalNamespace, typeKind, name, GlobalClassName));
            }
            return node;
        }
        protected string GetNestedName(IRuleNode ctx)
        {
            string name = "";
            int iNest = 0;
            while (ctx != null)
            {
                if (ctx is XP.Class_Context)
                {
                    name = ((XP.Class_Context)ctx).Id.GetText() + "." + name;
                    iNest++;
                }
                else if (ctx is XP.Structure_Context)
                {
                    name = ((XP.Structure_Context)ctx).Id.GetText() + "." + name;
                    iNest++;
                }
                else if (ctx is XP.Namespace_Context)
                {
                    name = ((XP.Namespace_Context)ctx).Name.GetText() + "." + name;
                    iNest++;
                }
                else if (ctx is XP.Interface_Context)
                {
                    name = ((XP.Interface_Context)ctx).Id.GetText() + "." + name;
                    iNest++;
                }
                else if (ctx is XP.PropertyContext)
                {
                    name = ((XP.PropertyContext)ctx).Id?.GetText() + "." + name;
                }
                else if (ctx is XP.Event_Context)
                {
                    name = ((XP.Event_Context)ctx).Id.GetText() + "." + name;
                }
                ctx = ctx.Parent;
            }
            if (iNest == 1 && !String.IsNullOrEmpty(_options.DefaultNamespace))
            {
                name = _options.DefaultNamespace + "." + name;
            }
            return name;

        }
        protected virtual BlockSyntax AddMissingReturnStatement(BlockSyntax body, XP.StatementBlockContext block, TypeSyntax returnType)
        {
            return body;
        }
        protected string GetEntityName(Boolean Full, Boolean funcNameOnly = false)
        {
            string name = "";
            string suffix = "";
            XP.IEntityContext context = Entities.Peek();
            if (context == null)
                return "";
            XP.DatatypeContext RetType = null;
            XP.ParameterListContext Params = null;
            XP.PropertyParameterListContext PParams = null;
            name = GetNestedName(context.Parent);
            if (context is XP.FunctionContext)
            {
                string modName = _options.CommandLineArguments?.CompilationOptions.ModuleName;
                if (modName == null)
                {
                    modName = _options.CommandLineArguments?.SourceFiles.FirstOrDefault().Path;
                    modName = PathUtilities.GetFileName(modName, false);
                }

                XP.FunctionContext fc = (XP.FunctionContext)context;
                if (name.Length == 0)
                    name = GlobalClassName + "." + fc.Id.GetText();
                else
                    name += fc.Id.GetText();
                if (name.Contains("."))
                {
                    name = modName + ":" + name.Substring(name.IndexOf(".") + 1);
                }

                RetType = fc.Type;
                Params = fc.ParamList;
            }
            else if (context is XP.ProcedureContext)
            {
                XP.ProcedureContext pc = (XP.ProcedureContext)context;
                if (name.Length == 0)
                    name = GlobalClassName + "." + pc.Id.GetText();
                else
                    name += pc.Id.GetText();
                Params = pc.ParamList;
            }
            else if (context is XP.ConstructorContext)
            {
                var cc = (XP.ConstructorContext)context;
                if (name.Length > 0) // Remove the dot
                    name = name.Substring(0, name.Length - 1);
                suffix = ".CTOR";
                Params = cc.ParamList;
            }
            else if (context is XP.DestructorContext)
            {
                var dc = (XP.DestructorContext)context;
                name += "Finalize()";
            }
            else if (context is XP.MethodContext)
            {
                XP.MethodContext mc = (XP.MethodContext)context;
                if (mc.ClassId != null)
                    name += mc.ClassId.GetText() + "." + mc.Id.GetText();
                else
                {
                    name += mc.Id.GetText();
                }
                RetType = mc.Type;
                Params = mc.ParamList;
                switch (mc.T.Token.Type)
                {
                    case XP.ACCESS:
                        suffix = ":Access";
                        break;
                    case XP.ASSIGN:
                        suffix = ":Assign";
                        break;
                }
            }
            else if (context is XP.PropertyContext)
            {
                XP.PropertyContext pc = (XP.PropertyContext)context;
                if (pc.Id != null)
                    name += pc.Id.GetText();
                if (pc.SELF() != null)
                    name += pc.SELF()?.GetText();
                PParams = pc.ParamList;
                RetType = pc.Type;
                suffix = ":Property";

            }
            else if (context is XP.Event_Context)
            {
                XP.Event_Context ec = (XP.Event_Context)context;
                name += ec.Id.GetText();
                RetType = ec.Type;
                suffix = ":Event";
            }
            else if (context is XP.VodllContext)
            {
                XP.VodllContext vdc = (XP.VodllContext)context;
                name += vdc.Id.GetText();
                RetType = vdc.Type;
                Params = vdc.ParamList;
                suffix = ":VoDll";
            }
            else if (context is XP.Delegate_Context)
            {
                XP.Delegate_Context dc = (XP.Delegate_Context)context;
                name += dc.Id.GetText();
                RetType = dc.Type;
                Params = dc.ParamList;
                suffix = ":Delegate";
            }
            else if (context is XP.Class_Context)
            {
                XP.Class_Context cc = (XP.Class_Context)context;
                name += cc.Id.GetText();
                suffix = ":Class";
            }
            else if (context is XP.Structure_Context)
            {
                XP.Structure_Context sc = (XP.Structure_Context)context;
                name += sc.Id.GetText();
                suffix = ":Structure";
            }
            else if (context is XP.EventAccessorContext)
            {
                XP.EventAccessorContext ec = (XP.EventAccessorContext)context;
                name += ec.Key.Text;
            }
            else if (context is XP.PropertyAccessorContext)
            {
                XP.PropertyAccessorContext pc = (XP.PropertyAccessorContext)context;
                name += pc.Key.Text;
            }
            if (Full)
            {
                if (RetType != null)
                {
                    name = RetType.GetText() + " " + name;
                }
                else
                {
                    name = "VOID " + name;
                }
                string strParams = "";
                if (Params != null)
                {
                    foreach (XP.ParameterContext _par in Params._Params)
                    {
                        if (strParams?.Length > 0)
                            strParams += ", ";
                        if (_par.Modifiers != null)
                        {
                            string mod = "";
                            foreach (var m in _par.Modifiers._Tokens)
                            {
                                if (m.Type != XP.AS)
                                    mod += m.Text + " ";
                            }
                            strParams += mod;
                        }
                        if (_par.Type != null)
                            strParams += _par.Type.GetText();
                        else
                            strParams += "USUAL";
                    }
                }
                if (PParams != null)
                {
                    foreach (XP.ParameterContext _par in PParams._Params)
                    {
                        if (strParams?.Length > 0)
                            strParams += ", ";
                        if (_par.Type != null)
                            strParams += _par.Type.GetText();
                        else
                            strParams += "USUAL";
                    }
                }
                if (String.Compare(suffix, ".CTOR", StringComparison.OrdinalIgnoreCase) == 0)
                {
                    name += "{ " + strParams + " }";
                    suffix = "";
                }
                else
                {
                    name += "( " + strParams + " )";
                    if (!string.IsNullOrEmpty(suffix))
                        name += suffix;
                }

            }
            else
            {
                if (funcNameOnly)
                {
                    int nPos = name.LastIndexOf('.');
                    name = name.Substring(nPos + 1);
                }
                else
                {
                    name = name.ToUpper() + suffix;
                }
            }
            return name;

        }
        #endregion

        #region Lists
        internal SyntaxList<SyntaxToken> TokenList(params SyntaxKind[] kinds)
        {
            var rb = _pool.Allocate();
            foreach (var k in kinds)
                if (k != SyntaxKind.None)
                {
                    rb.Add(SyntaxFactory.MakeToken(k));
                }
            var r = rb.ToList();
            _pool.Free(rb);
            return r;
        }

        protected SyntaxList<SyntaxToken> TokenListWithDefaultVisibility(Boolean inInterface = false, params SyntaxKind[] kinds)
        {
            var rb = _pool.Allocate();
            foreach (var k in kinds)
            {
                rb.Add(SyntaxFactory.MakeToken(k));
            }
            if (!inInterface)
                rb.FixDefaultVisibility();
            var r = rb.ToList<SyntaxToken>();
            _pool.Free(rb);
            return r;
        }

        protected SyntaxList<SyntaxToken> DefaultMethodModifiers(bool inInterface, bool inStructure, bool noOverride = false)
        {
            var rb = _pool.Allocate();
            if (!inInterface)
            {
                rb.FixDefaultVisibility();
                // structures do not get virtual or override modifiers
                if (!inStructure && !noOverride)
                {
                    if (_options.VirtualInstanceMethods)
                        rb.FixDefaultVirtual();
                    else
                        rb.FixDefaultMethod();
                }
            }
            var r = rb.ToList<SyntaxToken>();
            _pool.Free(rb);
            return r;
        }

        protected SyntaxList<SyntaxToken> EmptyList()
        {
            // cannot cache this. SyntaxList<T> is a struct
            SyntaxList<SyntaxToken> emptyList;
            var rb = _pool.Allocate();
            emptyList = rb.ToList<SyntaxToken>();
            _pool.Free(rb);
            return emptyList;
        }

        protected SyntaxList<T> EmptyList<T>() where T : CSharpSyntaxNode
        {
            var rb = _pool.Allocate<T>();
            var r = rb.ToList();
            _pool.Free(rb);
            return r;
        }

        protected SeparatedSyntaxList<T> EmptySeparatedList<T>() where T : CSharpSyntaxNode
        {
            var rb = _pool.AllocateSeparated<T>();
            var r = rb.ToList();
            _pool.Free(rb);
            return r;
        }

        protected SyntaxList<T> MakeList<T>(IEnumerable<IXParseTree> t)
            where T : InternalSyntax.CSharpSyntaxNode
        {
            if (t == null)
                return default(SyntaxList<T>);
            var l = _pool.Allocate<T>();
            foreach (var item in t)
            {
                if (item != null)
                {
                    if (item.CsNode is SyntaxList<T>)
                        l.AddRange(item.GetList<T>());
                    else
                        l.Add(item.Get<T>());
                }
            }
            var list = l.ToList();
            _pool.Free(l);
            return list;
        }

        protected SyntaxList<T> MakeList<T>(IEnumerable<T> t, params T[] items)
            where T : InternalSyntax.CSharpSyntaxNode
        {
            var l = _pool.Allocate<T>();
            if (t != null)
            {
                foreach (var item in t)
                {
                    if (item != null)
                        l.Add(item);
                }
            }
            foreach (var item in items)
            {
                if (item != null)
                    l.Add(item);
            }
            var list = l.ToList();
            _pool.Free(l);
            return list;
        }

        protected SyntaxList<T> MakeList<T>(params T[] items) where T : InternalSyntax.CSharpSyntaxNode
        {
            var l = _pool.Allocate<T>();
            foreach (var item in items)
            {
                if (item != null)
                    l.Add(item);
            }
            var list = l.ToList();
            _pool.Free(l);
            return list;
        }

        protected SeparatedSyntaxList<T> MakeSeparatedList<T>(System.Collections.IEnumerable t) where T : InternalSyntax.CSharpSyntaxNode
        {
            if (t == null)
                return default(SeparatedSyntaxList<T>);
            var l = _pool.AllocateSeparated<T>();
            foreach (var item in t)
            {
                if (item != null)
                {
                    if (l.Count > 0)
                        l.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                    l.Add(((IXParseTree)item).Get<T>());
                }
            }
            var list = l.ToList();
            _pool.Free(l);
            return list;
        }

        protected SeparatedSyntaxList<T> MakeSeparatedList<T>(params T[] items) where T : InternalSyntax.CSharpSyntaxNode
        {
            var l = _pool.AllocateSeparated<T>();
            foreach (var item in items)
            {
                if (item != null)
                {
                    if (l.Count > 0)
                        l.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                    l.Add(item);
                }
            }
            var list = l.ToList();
            _pool.Free(l);
            return list;
        }
        #endregion

        #region Helpers

        protected SyntaxToken GetRShiftToken(IToken firstGT, IToken secondGT)
        {
            if (secondGT == null)
                return SyntaxFactory.MakeToken(SyntaxKind.GreaterThanToken);
            int iFirst = firstGT.Column;
            int iSecond = secondGT.Column;
            SyntaxToken result;
            if (iSecond != iFirst + 1) // extra whitespace detected
                result = SyntaxFactory.MissingToken(SyntaxKind.GreaterThanGreaterThanToken).WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_UnexpectedToken, "> >"));
            else
                result = SyntaxFactory.MakeToken(SyntaxKind.GreaterThanGreaterThanToken);

            return result;
        }


        internal CSharpSyntaxNode NotInDialect(string feature)
        {
            CSharpSyntaxNode node = _syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            return NotInDialect(node, feature);
        }
        internal CSharpSyntaxNode NotInDialect(CSharpSyntaxNode node, string feature, string additional = "")
        {
            return node.WithAdditionalDiagnostics(
                new SyntaxDiagnosticInfo(ErrorCode.ERR_FeatureNotAvailableInDialect, feature, _options.Dialect.ToString(), additional));
        }

        internal string UniqueNameSuffix
        {
            get { return "$" + _unique++; }
        }

        internal T FixPosition<T>(T r, IToken t) where T : XSharpParserRuleContext
        {
            r.Start = r.Stop = t;
            return r;
        }


        protected TypeSyntax VoidType()
        {
            return _voidType;
        }

        protected TypeSyntax MissingType()
        {
            return _objectType
                .WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_TypeExpected));
        }

        protected void initTypes()
        {
            lock (oGate)
            {
                if (intType == null)
                {
                    intType = _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.IntKeyword));
                    uintType = _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.UIntKeyword));
                    floatType = _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.FloatKeyword));
                    doubleType = _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.DoubleKeyword));
                    decimalType = _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.DecimalKeyword));
                    stringType = _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.StringKeyword));
                    ulongType = _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.ULongKeyword));
                    longType = _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.LongKeyword));
                }


            }
        }
        protected virtual TypeSyntax GetExpressionType(XP.ExpressionContext expr, ref bool isConst)
        {

            TypeSyntax type = null;
            var token = expr.GetLiteralToken();
            isConst = false;
            if (token != null)
            {
                // Try to imply the type from the expression
                string text = token.Text;
                switch (token.Type)
                {
                    case XP.INT_CONST:
                    case XP.HEX_CONST:
                    case XP.REAL_CONST:
                        // call SyntaxLiteralValue because it inspects the size of the number
                        SyntaxToken val = token.SyntaxLiteralValue(_options);
                        if (val.Value is int)
                            type = intType;
                        else if (val.Value is uint)
                            type = uintType;
                        else if (val.Value is double)
                            type = doubleType;
                        else if (val.Value is float)
                            type = floatType;
                        else if (val.Value is decimal)
                            type = decimalType;
                        else if (val.Value is ulong)
                            type = ulongType;
                        else if (val.Value is long)
                            type = longType;
                        else
                            type = _objectType;
                        break;

                    case XP.INVALID_NUMBER:
                        type = _objectType;
                        break;

                    case XP.STRING_CONST:
                    case XP.ESCAPED_STRING_CONST:
                    case XP.INTERPOLATED_STRING_CONST:
                    case XP.INCOMPLETE_STRING_CONST:
                        type = stringType;
                        break;

                }
            }
            else if (expr is XP.PrimaryExpressionContext)
            {
                var prim = expr as XP.PrimaryExpressionContext;
                if (prim.Expr is XP.VoCastExpressionContext)
                {
                    var e = prim.Expr as XP.VoCastExpressionContext;
                    if (e.Type != null)
                    {
                        type = e.Type.Get<TypeSyntax>();
                    }
                    else if (e.XType != null)
                    {
                        type = e.XType.Get<TypeSyntax>();
                    }
                    isConst = e.Expr.GetLiteralToken() != null;
                }
                else if (prim.Expr is XP.VoCastPtrExpressionContext)
                {
                    var e = prim.Expr as XP.VoCastPtrExpressionContext;
                    var e2 = prim.Expr.Get<CastExpressionSyntax>();
                    type = e2.Type;
                    isConst = e.Expr.GetLiteralToken() != null;
                }
                else if (prim.Expr is XP.VoConversionExpressionContext)
                {
                    var e = prim.Expr as XP.VoConversionExpressionContext;
                    if (e.Type != null)
                    {
                        type = e.Type.Get<TypeSyntax>();
                    }
                    else if (e.XType != null)
                    {
                        type = e.XType.Get<TypeSyntax>();
                    }
                    isConst = e.Expr.GetLiteralToken() != null;
                }
                else if (prim.Expr is XP.DefaultExpressionContext)
                {
                    var e = prim.Expr as XP.DefaultExpressionContext;
                    type = e.Type.Get<TypeSyntax>();
                }
                else if (prim.Expr is XP.CtorCallContext)
                {
                    var e = prim.Expr as XP.CtorCallContext;
                    type = e.Type.Get<TypeSyntax>();
                }
                else if (prim.Expr is XP.CodeblockExpressionContext)
                {
                    if (_options.XSharpRuntime)
                        type = GenerateQualifiedName(XSharpQualifiedTypeNames.Codeblock);
                    else
                        type = GenerateQualifiedName(VulcanQualifiedTypeNames.Codeblock);
                }
                else if (prim.Expr is XP.LiteralArrayExpressionContext)
                {
                    if (_options.XSharpRuntime)
                        type = GenerateQualifiedName(XSharpQualifiedTypeNames.Array);
                    else
                        type = GenerateQualifiedName(VulcanQualifiedTypeNames.Array);
                }
                else if (prim.Expr is XP.UsualTypeNameExpressionContext)
                {
                    type = intType;
                    isConst = true;
                }
                else if (prim.Expr is XP.TypeExpressionContext)
                {
                    var e = prim.Expr as XP.TypeExpressionContext;
                    type = e.Type.Get<TypeSyntax>();
                }
                else if (prim.Expr is XP.ParenExpressionContext)
                {
                    var e = prim.Expr as XP.ParenExpressionContext;
                    type = GetExpressionType(e.Expr, ref isConst);
                    isConst = e.Expr.GetLiteralToken() != null;
                }
                else if (prim.Expr is XP.IifExpressionContext)
                {
                    var e = prim.Expr as XP.IifExpressionContext;
                    var i = e.Expr;
                    type = GetExpressionType(i.TrueExpr, ref isConst);
                }
            }
            else if (expr is XP.TypeCastContext)
            {
                var e = expr as XP.TypeCastContext;
                type = e.Type.Get<TypeSyntax>();
                isConst = e.Expr.GetLiteralToken() != null;

            }
            else if (expr is XP.TypeCheckExpressionContext)
            {
                var e = expr as XP.TypeCheckExpressionContext;
                if (e.Op.Type == XP.ASTYPE)
                {
                    type = e.Type.Get<TypeSyntax>();
                    isConst = e.Expr.GetLiteralToken() != null;
                }
            }
            else if (expr is XP.BinaryExpressionContext)
            {
                var e = expr as XP.BinaryExpressionContext;
                bool leftIsConst = false;
                bool rightIsConst = false;
                type = GetExpressionType(e.Left, ref leftIsConst);
                var type2 = GetExpressionType(e.Right, ref rightIsConst);
                isConst = leftIsConst && rightIsConst;
                if (type != type2 && type.ToFullString() != type2.ToFullString())
                {
                    if (type == _objectType)
                    {
                        type = type2;
                    }
                }
            }
            else if (expr is XP.AssignmentExpressionContext)
            {
                var e = expr as XP.AssignmentExpressionContext;
                bool leftIsConst = false;
                bool rightIsConst = false;
                type = GetExpressionType(e.Left, ref leftIsConst);
                var type2 = GetExpressionType(e.Right, ref rightIsConst);
                isConst = leftIsConst && rightIsConst;
                if (type != type2 && type.ToFullString() != type2.ToFullString())
                {
                    if (type == _objectType)
                    {
                        type = type2;
                    }
                }
            }
            else if (expr is XP.PrefixExpressionContext)
            {
                var e = expr as XP.PrefixExpressionContext;
                type = GetExpressionType(e.Expr, ref isConst);
                if (e.Op.Type == XP.ADDROF)
                    type = _syntaxFactory.PointerType(_voidType, SyntaxFactory.MakeToken(SyntaxKind.AmpersandToken));
                else
                    type = GetExpressionType(e.Expr, ref isConst);
            }
            if (type == null)
            {
                type = _objectType;
            }
            if (type is PredefinedTypeSyntax)
            {
                var pdt = type as PredefinedTypeSyntax;
                switch (pdt.Keyword.Kind)
                {
                    case SyntaxKind.IntKeyword:
                    case SyntaxKind.UIntKeyword:
                    case SyntaxKind.BoolKeyword:
                    case SyntaxKind.FloatKeyword:
                    case SyntaxKind.DecimalKeyword:
                    case SyntaxKind.DoubleKeyword:
                    case SyntaxKind.StringKeyword:
                    case SyntaxKind.CharKeyword:
                    case SyntaxKind.ByteKeyword:
                    case SyntaxKind.ShortKeyword:
                    case SyntaxKind.UShortKeyword:
                        isConst = true;
                        break;
                    default:
                        isConst = false;
                        break;

                }
            }
            else
            {
                isConst = false;
            }
            return type;
        }

        #endregion

        #region Code Generation Helpers

        StatementSyntax CheckForGarbage(StatementSyntax stmt, XSharpParserRuleContext ignored, string message)
        {
            if (ignored != null && !_options.Dialect.AllowGarbage())
            {
                stmt = (StatementSyntax)NotInDialect(stmt, message);
            }
            return stmt;
        }
        protected MemberDeclarationSyntax CheckForGarbage(MemberDeclarationSyntax member, XSharpParserRuleContext ignored, string message)
        {
            if (ignored != null && !_options.Dialect.AllowGarbage())
            {
                member = (MemberDeclarationSyntax)NotInDialect(member, message);
            }
            return member;
        }

        protected ArrayRankSpecifierSyntax MakeArrayRankSpecifier(int ranks)
        {
            var sizes = _pool.AllocateSeparated<ExpressionSyntax>();
            for (int i = 0; i < ranks; i++)
            {
                if (i > 0)
                    sizes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                sizes.Add(_syntaxFactory.OmittedArraySizeExpression(SyntaxFactory.MakeToken(SyntaxKind.OmittedArraySizeExpressionToken)));
            }
            var r = _syntaxFactory.ArrayRankSpecifier(SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                sizes,
                SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken));
            _pool.Free(sizes);
            return r;
        }

        protected ExpressionSyntax MakeCastTo(TypeSyntax type, ExpressionSyntax expr)
        {
            return _syntaxFactory.CastExpression(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                type, SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken), expr);
        }

        protected ExpressionSyntax MakeChecked(ExpressionSyntax expr, bool @checked)
        {
            if (@checked)
            {
                return _syntaxFactory.CheckedExpression(SyntaxKind.CheckedExpression,
                    SyntaxFactory.MakeToken(SyntaxKind.CheckedKeyword),
                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    expr,
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
            }
            else
            {
                return _syntaxFactory.CheckedExpression(SyntaxKind.UncheckedExpression,
                    SyntaxFactory.MakeToken(SyntaxKind.UncheckedKeyword),
                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    expr,
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
            }
        }
        protected LockStatementSyntax MakeLock(ExpressionSyntax expr, StatementSyntax statement)
        {
            return _syntaxFactory.LockStatement(SyntaxFactory.MakeToken(SyntaxKind.LockKeyword),
                                               SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                                                expr,
                                                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                                                statement);


        }
        protected AssignmentExpressionSyntax MakeSimpleAssignment(ExpressionSyntax lhs, ExpressionSyntax rhs)
        {
            return _syntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                     lhs, SyntaxFactory.MakeToken(SyntaxKind.EqualsToken), rhs);
        }

        protected MemberAccessExpressionSyntax MakeSimpleMemberAccess(ExpressionSyntax lhs, SimpleNameSyntax rhs)
        {
            return _syntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, lhs,
                                                        SyntaxFactory.MakeToken(SyntaxKind.DotToken), rhs);
        }
        protected TypeOfExpressionSyntax MakeTypeOf(TypeSyntax type)
        {
            return _syntaxFactory.TypeOfExpression(
                    SyntaxFactory.MakeToken(SyntaxKind.TypeOfKeyword),
                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    type,
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
        }


        protected IfStatementSyntax GenerateIfStatement(ExpressionSyntax condition, StatementSyntax statement, ElseClauseSyntax @else = null)
        {
            return _syntaxFactory.IfStatement(
                        SyntaxFactory.MakeToken(SyntaxKind.IfKeyword),
                        SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                        condition,
                        SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                        statement, @else);
        }

        protected virtual ExpressionSyntax GenerateMissingExpression()
        {
            return GenerateMissingExpression(true);
        }

        protected virtual ExpressionSyntax GenerateMissingExpression(bool withError)
        {
            var result = GenerateLiteralNull();
            if (withError)
            {
                result = result.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_MissingArgument));
            }
            return result;
        }

        protected LiteralExpressionSyntax GenerateLiteral(IToken token)
        {
            if (token != null)
                return SyntaxFactory.LiteralExpression(token.ExpressionKindLiteral(), token.SyntaxLiteralValue(_options));
            return null;
        }
        protected LiteralExpressionSyntax GenerateLiteral(bool value)
        {
            if (value)
                return _syntaxFactory.LiteralExpression(SyntaxKind.TrueLiteralExpression, SyntaxFactory.MakeToken(SyntaxKind.TrueKeyword));
            else
                return _syntaxFactory.LiteralExpression(SyntaxKind.FalseLiteralExpression, SyntaxFactory.MakeToken(SyntaxKind.FalseKeyword));
        }
        protected LiteralExpressionSyntax GenerateLiteral(string text)
        {
            if (text.StartsWith("@@"))
                text = text.Substring(2);
            return _syntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression,
                                                SyntaxFactory.Literal(null, text, text, null));
        }
        protected LiteralExpressionSyntax GenerateLiteral(int value)
        {
            return _syntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                                                SyntaxFactory.Literal(null, value.ToString(), value, null));
        }
        protected LiteralExpressionSyntax GenerateLiteral(long value)
        {
            return _syntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                                                SyntaxFactory.Literal(null, value.ToString(), value, null));
        }
        protected LiteralExpressionSyntax GenerateLiteral(double value)
        {
            return _syntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                                                SyntaxFactory.Literal(null, value.ToString(), value, null));
        }
        protected LiteralExpressionSyntax GenerateLiteral(string source, int value)
        {
            return _syntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                                                SyntaxFactory.Literal(null, source, value, null));
        }
        protected LiteralExpressionSyntax GenerateLiteralNull()
        {
            return _syntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression, SyntaxFactory.MakeToken(SyntaxKind.NullKeyword));
        }
        protected VariableDeclaratorSyntax GenerateVariable(string name, ExpressionSyntax initexpr = null)
        {
            return GenerateVariable(SyntaxFactory.Identifier(name), initexpr);
        }
        protected VariableDeclaratorSyntax GenerateVariable(SyntaxToken nameToken, ExpressionSyntax initexpr = null)
        {
            if (initexpr != null)
            {
                return _syntaxFactory.VariableDeclarator(
                    nameToken, null,
                    _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken), initexpr)
                    );
            }
            else
            {
                return _syntaxFactory.VariableDeclarator(
                    nameToken, null, null
                    );

            }
        }

        protected VariableDeclaratorSyntax GenerateBuffer(SyntaxToken nameToken, BracketedArgumentListSyntax dims)
        {
            return _syntaxFactory.VariableDeclarator(nameToken, dims, null);
        }

        protected LocalDeclarationStatementSyntax GenerateLocalDecl(string name, TypeSyntax type, ExpressionSyntax initexpr = null)
        {
            var decl = _syntaxFactory.VariableDeclaration(type,
                        MakeSeparatedList<VariableDeclaratorSyntax>(GenerateVariable(name, initexpr)));
            decl.XGenerated = true;
            var result = _syntaxFactory.LocalDeclarationStatement(
                        TokenList(), decl,
                        SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            result.XGenerated = true;
            return result;
        }

        protected ReturnStatementSyntax GenerateReturn(ExpressionSyntax result = null, bool markAsGenerated = false)
        {
            var stmt = _syntaxFactory.ReturnStatement(SyntaxFactory.MakeToken(SyntaxKind.ReturnKeyword),
                        result, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            stmt.XGenerated = markAsGenerated;
            return stmt;
        }
        protected IdentifierNameSyntax GenerateSimpleName(string name)
        {
            return _syntaxFactory.IdentifierName(SyntaxFactory.MakeIdentifier(name));
        }
        protected ExpressionSyntax GenerateSelf()
        {
            return _syntaxFactory.ThisExpression(SyntaxFactory.MakeToken(SyntaxKind.ThisKeyword));
        }
        protected ExpressionSyntax GenerateSuper()
        {
            return _syntaxFactory.BaseExpression(SyntaxFactory.MakeToken(SyntaxKind.BaseKeyword));
        }

        protected NameEqualsSyntax GenerateNameEquals(string name)
        {
            return _syntaxFactory.NameEquals(
                  GenerateSimpleName(name), SyntaxFactory.MakeToken(SyntaxKind.EqualsToken));
        }

        protected internal NameSyntax GenerateQualifiedName(string name)
        {
            string[] ids = name.Split('.');
            string idName = ids[0];
            string alias = null;
            int cc = idName.IndexOf("::");
            if (cc >= 0)
            {
                alias = idName.Substring(0, cc);
                idName = idName.Substring(cc + 2);
            }
            NameSyntax r = GenerateSimpleName(idName);
            if (alias != null)
            {
                if (string.Compare(alias, "global", StringComparison.OrdinalIgnoreCase) == 0)
                    r = _syntaxFactory.AliasQualifiedName(
                        _syntaxFactory.IdentifierName(SyntaxFactory.MakeToken(SyntaxKind.GlobalKeyword, alias)),
                        SyntaxFactory.MakeTokenNoWs(SyntaxKind.ColonColonToken),
                        (SimpleNameSyntax)r);
                else
                    r = _syntaxFactory.AliasQualifiedName(
                        GenerateSimpleName(alias),
                        SyntaxFactory.MakeTokenNoWs(SyntaxKind.ColonColonToken),
                        (SimpleNameSyntax)r);
            }
            for (int i = 1; i < ids.Length; i++)
            {
                r = _syntaxFactory.QualifiedName(
                    r,
                    SyntaxFactory.MakeTokenNoWs(SyntaxKind.DotToken),
                    GenerateSimpleName(ids[i]));
            }
            return r;
        }


        internal static Syntax.NameSyntax ExtGenerateQualifiedName(string name)
        {
            string[] ids = name.Split('.');
            string idName = ids[0];
            string alias = null;
            int cc = idName.IndexOf("::");
            if (cc >= 0)
            {
                alias = idName.Substring(0, cc);
                idName = idName.Substring(cc + 2);
            }
            Syntax.NameSyntax r = CSharp.SyntaxFactory.IdentifierName(idName);
            if (alias != null)
            {
                if (string.Compare(alias, "global", StringComparison.OrdinalIgnoreCase) == 0)
                    r = CSharp.SyntaxFactory.AliasQualifiedName(
                        CSharp.SyntaxFactory.IdentifierName(CSharp.SyntaxFactory.Token(SyntaxKind.GlobalKeyword)),
                        CSharp.SyntaxFactory.Token(SyntaxKind.ColonColonToken),
                        (Syntax.SimpleNameSyntax)r);
                else
                    r = CSharp.SyntaxFactory.AliasQualifiedName(
                        CSharp.SyntaxFactory.IdentifierName(alias),
                        CSharp.SyntaxFactory.Token(SyntaxKind.ColonColonToken),
                        (Syntax.SimpleNameSyntax)r);
            }
            for (int i = 1; i < ids.Length; i++)
            {
                r = CSharp.SyntaxFactory.QualifiedName(
                    r,
                    CSharp.SyntaxFactory.Token(SyntaxKind.DotToken),
                    CSharp.SyntaxFactory.IdentifierName(ids[i]));
            }
            return r;
        }

        protected NameSyntax GenerateGlobalQualifiedNameFromList(string name, params string[] dotNames)
        {
            NameSyntax r = GenerateSimpleName(name);
            r = _syntaxFactory.AliasQualifiedName(
                _syntaxFactory.IdentifierName(SyntaxFactory.MakeToken(SyntaxKind.GlobalKeyword, "global")),
                SyntaxFactory.MakeTokenNoWs(SyntaxKind.ColonColonToken),
                (SimpleNameSyntax)r);
            foreach (var dotName in dotNames)
            {
                r = _syntaxFactory.QualifiedName(
                    r,
                    SyntaxFactory.MakeTokenNoWs(SyntaxKind.DotToken),
                    GenerateSimpleName(dotName));
            }
            return r;
        }

        protected void GenerateAttributeList(SyntaxListBuilder<AttributeListSyntax> attributeLists, params string[] attributeNames)
        {
            SeparatedSyntaxListBuilder<AttributeSyntax> attributes = _pool.AllocateSeparated<AttributeSyntax>();
            foreach (var attributeName in attributeNames)
            {
                if (attributes.Count > 0)
                {
                    attributes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                }
                attributes.Add(_syntaxFactory.Attribute(
                    name: GenerateQualifiedName(attributeName),
                    argumentList: null));
            }
            attributeLists.Add(MakeAttributeList(null, attributes));
            _pool.Free(attributes);
        }

        protected ExpressionSyntax CreateObject(TypeSyntax type, ArgumentListSyntax args, InitializerExpressionSyntax init = null)
        {
            ExpressionSyntax expr;
            expr = _syntaxFactory.ObjectCreationExpression(
                SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                type, args, init);
            return expr;
        }

        protected ExpressionSyntax GenerateMethodCall(string MethodName, bool markAsGenerated = false)
        {
            return GenerateMethodCall(MethodName, EmptyArgumentList(), markAsGenerated);
        }

        protected ExpressionSyntax GenerateMethodCall(string MethodName, ArgumentListSyntax args, bool markAsGenerated = false)
        {
            ExpressionSyntax expr = _syntaxFactory.InvocationExpression(GenerateQualifiedName(MethodName), args);
            expr.XGenerated = markAsGenerated;
            return expr;
        }

        protected virtual void ImplementClipperAndPSZ(XP.IEntityWithBodyContext context,
            ref SyntaxList<AttributeListSyntax> attributes, ref ParameterListSyntax parameters, ref BlockSyntax body,
            ref TypeSyntax dataType)
        {
            return;
        }

        protected ExpressionStatementSyntax GenerateExpressionStatement(ExpressionSyntax expr, bool markAsGenerated = false)
        {
            expr.XGenerated = markAsGenerated;
            var stmt = _syntaxFactory.ExpressionStatement(expr, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            stmt.XGenerated = markAsGenerated;
            return stmt;
        }

        protected ArgumentSyntax MakeArgument(ExpressionSyntax expr, bool byref = false)
        {
            SyntaxToken byrefToken = null;
            if (byref)
                byrefToken = SyntaxFactory.MakeToken(SyntaxKind.RefKeyword);
            return _syntaxFactory.Argument(null, byrefToken, expr);
        }
        protected BlockSyntax MakeBlock(IList<StatementSyntax> statements)
        {
            var stmts = _pool.Allocate<StatementSyntax>();
            foreach (var stmt in statements)
                stmts.Add(stmt);
            var result = MakeBlock(stmts);
            _pool.Free(stmts);
            return result;
        }
        protected BlockSyntax MakeBlock(SyntaxList<StatementSyntax> statements)
        {
            var block = _syntaxFactory.Block(
                        SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                        statements,
                        SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));
            block.XGenerated = true;
            return block;
        }

        protected ExpressionSyntax MakeDefault(TypeSyntax type)
        {
            if (type is PredefinedTypeSyntax && ((PredefinedTypeSyntax)type).Keyword.Kind == SyntaxKind.StringKeyword)
            {
                return GenerateLiteral("");
            }
            else
            {
                return _syntaxFactory.DefaultExpression(
                    SyntaxFactory.MakeToken(SyntaxKind.DefaultKeyword),
                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    type,
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
            }
        }



        protected MemberDeclarationSyntax AddNameSpaceToMember(XP.NameDotContext ns, MemberDeclarationSyntax m)
        {
            if (ns != null)
            {
                m = _syntaxFactory.NamespaceDeclaration(SyntaxFactory.MakeToken(SyntaxKind.NamespaceKeyword),
                    name: ns.Get<NameSyntax>(),
                    openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    externs: EmptyList<ExternAliasDirectiveSyntax>(),
                    usings: EmptyList<UsingDirectiveSyntax>(),
                    members: MakeList<MemberDeclarationSyntax>(m),
                    closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));

            }
            return m;

        }


        protected internal void AddUsingWhenMissing(SyntaxListBuilder<UsingDirectiveSyntax> usings, NameSyntax usingName, bool bStatic, NameEqualsSyntax alias)
        {
            bool found = false;

            for (int i = 0; i < usings.Count; i++)
            {
                UsingDirectiveSyntax u = usings[i];
                if (alias != null)
                {
                    // match alias AND name
                    if (u.Alias != null && CaseInsensitiveComparison.Compare(u.Alias.Name.ToString(), alias.Name.ToString()) == 0
                        && CaseInsensitiveComparison.Compare(u.Name.ToString(), usingName.ToString()) == 0)
                    {
                        found = true;
                        break;
                    }

                }
                else if (CaseInsensitiveComparison.Compare(u.Name.ToString(), usingName.ToString()) == 0)
                {
                    found = true;
                    break;
                }
            }

            if (!found)
            {
                SyntaxToken tokenStatic = null;
                if (bStatic)
                    tokenStatic = SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword);

                usings.Add(_syntaxFactory.UsingDirective(SyntaxFactory.MakeToken(SyntaxKind.UsingKeyword),
                    tokenStatic,
                    alias,
                    usingName,
                    SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            }

        }
        protected void AddUsingWhenMissing(SyntaxListBuilder<UsingDirectiveSyntax> usings, string name, bool bStatic, NameEqualsSyntax alias)
        {
            NameSyntax usingName = GenerateQualifiedName(name);
            AddUsingWhenMissing(usings, usingName, bStatic, alias);
        }

        protected NamespaceDeclarationSyntax GenerateNamespace(string name, SyntaxList<MemberDeclarationSyntax> members)
        {
            var externs = _pool.Allocate<ExternAliasDirectiveSyntax>();
            var usings = _pool.Allocate<UsingDirectiveSyntax>();
            var r = _syntaxFactory.NamespaceDeclaration(SyntaxFactory.MakeToken(SyntaxKind.NamespaceKeyword),
                name: GenerateQualifiedName(name),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                externs: externs,
                usings: usings,
                members: members,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            _pool.Free(externs);
            _pool.Free(usings);
            return r;
        }

        private void splitClassNameAndNamespace(ref string className, out string nameSpace)
        {
            nameSpace = "";
            if (className.Contains("."))
            {
                nameSpace = className.Substring(0, className.LastIndexOf("."));
                className = className.Substring(className.LastIndexOf(".") + 1);
            }
        }

        protected MemberDeclarationSyntax GenerateGlobalClass(string className, bool bInternalClass, bool withAttribs, SyntaxList<MemberDeclarationSyntax> members)
        {
            string nameSpace;
            splitClassNameAndNamespace(ref className, out nameSpace);
            SyntaxListBuilder modifiers = _pool.Allocate();
            modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.PartialKeyword));
            if (bInternalClass)
                modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.InternalKeyword));
            else
                modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.PublicKeyword));
            modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword));
            MemberDeclarationSyntax r =
                _syntaxFactory.ClassDeclaration(
                attributeLists: withAttribs ? MakeCompilerGeneratedAttribute(true) : EmptyList<AttributeListSyntax>(),
                modifiers: modifiers.ToList<SyntaxToken>(),
                keyword: SyntaxFactory.MakeToken(SyntaxKind.ClassKeyword),
                identifier: SyntaxFactory.Identifier(className),
                typeParameterList: null,
                baseList: null, // BaseListSyntax baseList = _syntaxFactory.BaseList(colon, list)
                constraintClauses: default(SyntaxListBuilder<TypeParameterConstraintClauseSyntax>),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: members,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)) /*))*/;
            _pool.Free(modifiers);
            r.XGenerated = true;
            if (nameSpace.Length > 0)
            {
                r = GenerateNamespace(nameSpace, MakeList<MemberDeclarationSyntax>(r));
                r.XGenerated = true;
            }
            return r;
        }

        protected MemberDeclarationSyntax GenerateGlobalClass(string className, bool internalClass, bool withAttribs, params MemberDeclarationSyntax[] members)
        {
            SyntaxListBuilder<MemberDeclarationSyntax> globalClassMembers = _pool.Allocate<MemberDeclarationSyntax>();
            string nameSpace;
            splitClassNameAndNamespace(ref className, out nameSpace);
            if (members.Length > 0)
            {
                foreach (var m in members)
                    globalClassMembers.Add(m);
            }
            else
            {
                // When no members defined then we create an empty static constructor 
                var statements = _pool.Allocate<StatementSyntax>();
                statements.Add(_syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                var block = MakeBlock(statements);
                _pool.Free(statements);
                globalClassMembers.Add(
                        _syntaxFactory.ConstructorDeclaration(
                        attributeLists: MakeCompilerGeneratedAttribute(),
                        modifiers: TokenList(SyntaxKind.StaticKeyword),
                        identifier: SyntaxFactory.Identifier(className),
                        parameterList: EmptyParameterList(),
                        initializer: null,
                        body: block,
                        expressionBody: null,
                        semicolonToken: null)
                    );
            }
            SyntaxListBuilder modifiers = _pool.Allocate();
            modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.PartialKeyword));
            if (internalClass)
                modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.InternalKeyword));
            else
                modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.PublicKeyword));
            modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword));
            MemberDeclarationSyntax classdecl =
                _syntaxFactory.ClassDeclaration(
                attributeLists: withAttribs ? MakeCompilerGeneratedAttribute(true) : EmptyList<AttributeListSyntax>(),
                modifiers: modifiers.ToList<SyntaxToken>(),
                keyword: SyntaxFactory.MakeToken(SyntaxKind.ClassKeyword),
                identifier: SyntaxFactory.Identifier(className),
                typeParameterList: null,
                baseList: null, // BaseListSyntax baseList = _syntaxFactory.BaseList(colon, list)
                constraintClauses: default(SyntaxListBuilder<TypeParameterConstraintClauseSyntax>),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: globalClassMembers,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)) /*))*/;
            _pool.Free(modifiers);
            _pool.Free(globalClassMembers);
            classdecl.XGenerated = true;
            if (nameSpace.Length > 0)
            {
                classdecl = GenerateNamespace(nameSpace, MakeList<MemberDeclarationSyntax>(classdecl));
                classdecl.XGenerated = true;
            }
            return classdecl;
        }

        bool IsTypeEqual(TypeSyntax t1, TypeSyntax t2)
        {
            var id1 = t1.ToFullString().Replace(" ", "");
            var id2 = t2.ToFullString().Replace(" ", "");
            if (id1.IndexOf(".") > 0)
                id1 = id1.Substring(id1.LastIndexOf(".") + 1);
            if (id2.IndexOf(".") > 0)
                id2 = id2.Substring(id2.LastIndexOf(".") + 1);

            return string.Compare(id1, id2, StringComparison.OrdinalIgnoreCase) == 0;
        }

        internal MemberDeclarationSyntax GeneratePartialProperyMethod(XP.MethodContext context, bool Access, bool Static)
        {
            string suffix;
            TypeSyntax returntype;
            if (Access)
            {
                suffix = XSharpSpecialNames.AccessSuffix;
                returntype = context.Type?.Get<TypeSyntax>() ?? _getMissingType();
            }
            else
            {
                suffix = XSharpSpecialNames.AssignSuffix;
                returntype = _voidType;
            }
            var name = SyntaxFactory.Identifier(context.Id.GetText() + suffix);
            var parameters = context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList();
            SyntaxList<SyntaxToken> mods;
            if (Static)
                mods = TokenList(SyntaxKind.PrivateKeyword, SyntaxKind.StaticKeyword);
            else
                mods = TokenList(SyntaxKind.PrivateKeyword);


            var body = context.StmtBlk.Get<BlockSyntax>();
            MemberDeclarationSyntax m = _syntaxFactory.MethodDeclaration(
                 attributeLists: null,
                 modifiers: mods,
                 returnType: returntype,
                 explicitInterfaceSpecifier: null,
                 identifier: name,
                 typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                 parameterList: parameters,
                 constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                 body: body,
                 expressionBody: null, // TODO: (grammar) expressionBody methods
                 semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)
                 );

            context.Put(m);
            return m;
        }


        internal MemberDeclarationSyntax GenerateVoProperty(SyntaxClassEntities.VoPropertyInfo vop, XSharpParserRuleContext context)
        {
            var getMods = _pool.Allocate();
            var setMods = _pool.Allocate();
            bool mergePartialDeclarations = (context == null);
            var outerMods = _pool.Allocate();
            int getVisLvl;
            int setVisLvl;
            var AccMet = vop.DupAccess ?? vop.AccessMethodCtx;
            var AssMet = vop.DupAssign ?? vop.AssignMethodCtx;

            if ((AccMet == null || AssMet == null) && !mergePartialDeclarations)
            {
                // When one of the two is missing and we are in a partial class\
                // then we generate a normal method. Later from the LanguageParser
                // we will generate a property and in its body we will call the generated method
                var ent = context as XP.IEntityContext;
                if (ent != null && ent.Data.Partial)
                {
                    MemberDeclarationSyntax result;
                    ent.Data.PartialProps = true;
                    if (ent is XP.IPartialPropertyContext)
                    {
                        var cls = ent as XP.IPartialPropertyContext;
                        if (cls.PartialProperties == null)
                            cls.PartialProperties = new List<XSharpParser.MethodContext>();

                        var met = AccMet ?? AssMet;
                        cls.PartialProperties.Add(met);
                        if (met.CsNode == null)
                            met.CsNode = ((XP.ClsmethodContext)met.Parent).CsNode;
                        ((XP.ClsmethodContext)met.Parent).CsNode = null;

                    }
                    if (AccMet != null)
                    {
                        bool bStatic = false;
                        if (AccMet.Modifiers?._Tokens != null)
                        {
                            bStatic = AccMet.Modifiers._Tokens.Any(t => t.Type == XSharpLexer.STATIC);
                        }
                        result = GeneratePartialProperyMethod(AccMet, true, bStatic);
                    }
                    else
                    {
                        bool bStatic = false;
                        if (AssMet.Modifiers?._Tokens != null)
                        {
                            bStatic = AssMet.Modifiers._Tokens.Any(t => t.Type == XSharpLexer.STATIC);
                        }
                        result = GeneratePartialProperyMethod(AssMet, false, bStatic);
                    }
                    GlobalEntities.NeedsProcessing = true;
                    return result;
                }
            }


            #region modifiers and visibility
            if (AccMet != null)
            {
                if (AccMet.Modifiers != null)
                {
                    getMods.AddRange(AccMet.Modifiers.GetList<SyntaxToken>());
                }
                else if (!AccMet.isInInterface())
                {
                    getMods.FixDefaultVisibility();
                    if (_options.VirtualInstanceMethods && !AccMet.isInStructure())
                    {
                        getMods.FixDefaultVirtual();
                    }
                    else
                    {
                        getMods.FixDefaultMethod();
                    }
                }
                getVisLvl = getMods.GetVisibilityLevel();
            }
            else
                getVisLvl = 15;
            if (AssMet != null)
            {
                if (AssMet.Modifiers != null)
                {
                    setMods.AddRange(AssMet.Modifiers.GetList<SyntaxToken>());
                }
                else if (!AssMet.isInInterface())
                {
                    setMods.FixDefaultVisibility();
                    if (_options.VirtualInstanceMethods && !AssMet.isInStructure())
                    {
                        setMods.FixDefaultVirtual();
                    }
                    else
                    {
                        setMods.FixDefaultMethod();
                    }
                }
                setVisLvl = setMods.GetVisibilityLevel();
            }
            else
            {
                setVisLvl = 15;
            }
            if (getVisLvl <= setVisLvl)
            {
                outerMods.AddRange(getMods.ToList());
                getMods.Clear();
            }
            else
            {
                outerMods.AddRange(setMods.ToList());
                setMods.Clear();
            }
            var rawMods = getVisLvl <= setVisLvl ? setMods : getMods;
            var innerMods = _pool.Allocate();
            for (int i = 0; i < rawMods.Count; i++)
            {
                var t = rawMods[i];
                if (!outerMods.Any(t.RawKind))
                {
                    if (!SyntaxFacts.IsAccessibilityModifier((SyntaxKind)t.RawKind))
                    {
                        t = ((CSharpSyntaxNode)t).WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_BadMemberFlag, t));
                    }
                    innerMods.Add(t);
                }
            }
            _pool.Free(rawMods);
            if (getVisLvl <= setVisLvl)
            {
                setMods = innerMods;
            }
            else
            {
                getMods = innerMods;
            }
            #endregion
            #region Property Type
            TypeSyntax voPropType;
            var typeMatch = true;
            if (AssMet != null && AssMet.ParamList != null && AssMet.ParamList._Params?.Count > 0)
            {
                voPropType = AssMet.ParamList._Params[0].Type?.Get<TypeSyntax>() ?? _getMissingType();
                if (AccMet != null)
                {
                    var accType = AccMet.Type?.Get<TypeSyntax>() ?? _getMissingType();
                    typeMatch = IsTypeEqual(voPropType, accType);
                }
            }
            else if (AccMet != null)
            {
                voPropType = AccMet.Type?.Get<TypeSyntax>() ?? _getMissingType();
            }
            else
            {
                voPropType = _getMissingType();
            }
            voPropType.XVoDecl = true;
            #endregion
            #region Parameters
            BracketedParameterListSyntax voPropParams;
            ArgumentSyntax[] voPropArgs;
            int accParamCount = 0;
            int assParamCount = 0;
            if (AssMet != null && AssMet?.ParamList != null)
            {
                assParamCount = AssMet.ParamList._Params.Count;
            }
            if (AccMet != null && AccMet.ParamList != null)
            {
                accParamCount = AccMet.ParamList._Params.Count;
            }
            if (assParamCount > 1)
            {
                var @params = ArrayBuilder<XP.ParameterContext>.GetInstance();
                foreach (var p in AssMet.ParamList._Params.Skip(1))
                {
                    @params.Add(p);
                }
                voPropParams = _syntaxFactory.BracketedParameterList(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                    MakeSeparatedList<ParameterSyntax>(@params),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken));
                voPropArgs = @params.Select(pCtx => _syntaxFactory.Argument(null, null, GenerateSimpleName(pCtx.Id.Start.Text))).ToArray();
            }
            else if (accParamCount > 0)
            {
                voPropParams = _syntaxFactory.BracketedParameterList(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                    MakeSeparatedList<ParameterSyntax>(AccMet.ParamList._Params),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken));
                voPropArgs = AccMet.ParamList._Params.Select(pCtx => _syntaxFactory.Argument(null, null, GenerateSimpleName(pCtx.Id.Start.Text))).ToArray();
            }
            else
            {
                voPropParams = null;
                voPropArgs = new ArgumentSyntax[0];
            }
            #endregion
            var accessors = _pool.Allocate<AccessorDeclarationSyntax>();
            IXParseTree xnode = null;
            bool paramMatch = true;
            #region ACCESS = Get Accessor
            if (AccMet != null)
            {
                // Create the GET accessor.
                bool isInInterfaceOrAbstract = AccMet.isInInterface() ||
                    outerMods.Any((int)SyntaxKind.AbstractKeyword) ||
                    outerMods.Any((int)SyntaxKind.ExternKeyword);
                var m = AccMet.Get<MethodDeclarationSyntax>();
                var args = MakeArgumentList(voPropArgs);
                BlockSyntax block = isInInterfaceOrAbstract ? null : m.Body;
                if (AssMet != null)
                {
                    // When both Access and Assign exist, then check the parameter names and types
                    if (accParamCount != assParamCount - 1)
                    {
                        paramMatch = false;
                    }
                    else if (assParamCount > 1)
                    {
                        // Match the parameter names
                        // The correct order of the parameters in an assign is:
                        // value, [index1 [, index2 [, index3]]]
                        // and the matching Access parameters must be (of course)
                        // [index1 [, index2 [, index3]]]
                        for (int iParam = 0; iParam < assParamCount - 1 && paramMatch; iParam++)
                        {
                            var name1 = AccMet.ParamList._Params[iParam].Id.GetText();
                            var name2 = AssMet.ParamList._Params[iParam + 1].Id.GetText();
                            var type1 = AccMet.ParamList._Params[iParam].Type?.Get<TypeSyntax>() ?? _getMissingType();
                            var type2 = AssMet.ParamList._Params[iParam + 1].Type?.Get<TypeSyntax>() ?? _getMissingType();
                            if (String.Compare(name1, name2, StringComparison.OrdinalIgnoreCase) != 0
                                || !IsTypeEqual(type1, type2))
                            {
                                paramMatch = false;
                            }

                        }
                    }

                }
                if (mergePartialDeclarations)
                {
                    // change the body to call the generated access
                    ExpressionSyntax methodCall;
                    var name = AccMet.Id.GetText() + XSharpSpecialNames.AccessSuffix;
                    if (accParamCount == 0)
                    {
                        methodCall = GenerateMethodCall(name);
                    }
                    else
                    {
                        var a = new List<ArgumentSyntax>();
                        foreach (var p in AccMet.ParamList._Params)
                        {
                            a.Add(MakeArgument(GenerateSimpleName(p.Id.GetText())));
                        }
                        methodCall = GenerateMethodCall(name, MakeArgumentList(a.ToArray()));
                    }
                    block = MakeBlock(GenerateReturn(methodCall,true));
                    block.XGenerated = true;
                }

                var accessor = _syntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration,
                        EmptyList<AttributeListSyntax>(), getMods.ToList<SyntaxToken>(),
                        SyntaxFactory.MakeToken(SyntaxKind.GetKeyword),
                        isInInterfaceOrAbstract ? null : block,
                        null,
                        isInInterfaceOrAbstract ? null : m.SemicolonToken);
                accessor.XGenerated = mergePartialDeclarations;
                if (vop.DupAccess != null)
                {
                    accessor = accessor.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_DuplicateAccessor));
                }
                if (!paramMatch)
                {
                    var diag = new SyntaxDiagnosticInfo(ErrorCode.ERR_AccessAssignParametersMutchMatch);
                    accessor = accessor.WithAdditionalDiagnostics(diag);
                }
                if (!typeMatch)
                {
                    var diag = new SyntaxDiagnosticInfo(ErrorCode.ERR_AccessAssignTypesMutchMatch);
                    accessor = accessor.WithAdditionalDiagnostics(diag);
                }
                var node = AccMet.CsNode as CSharpSyntaxNode;
                if (node != null && node.ContainsDiagnostics)
                {
                    var diag = node.GetDiagnostics();
                    accessor = accessor.WithAdditionalDiagnostics(diag);
                }
                accessors.Add(accessor);
                AccMet.Put(accessor);
                if (AccMet.Parent is XP.ClsmethodContext)
                {
                    ((XP.ClsmethodContext)AccMet.Parent).CsNode = null;
                }
                xnode = AccMet;
            }
            #endregion
            #region ASSIGN = Set Accessor
            bool missingParam = false;
            if (AssMet != null)
            {
                bool isInInterfaceOrAbstract = AssMet.isInInterface() ||
                    outerMods.Any((int)SyntaxKind.AbstractKeyword) ||
                    outerMods.Any((int)SyntaxKind.ExternKeyword);
                var m = AssMet.Get<MethodDeclarationSyntax>();
                BlockSyntax block = null;
                if (!isInInterfaceOrAbstract)
                {
                    if (mergePartialDeclarations)
                    {
                        var a = new List<ArgumentSyntax>() { MakeArgument(GenerateSimpleName("value")) };
                        if (assParamCount > 1)
                        {
                            for (int i = 1; i < assParamCount; i++)
                            {
                                var paramName = AssMet.ParamList._Params[i].Id.GetText();
                                a.Add(MakeArgument(GenerateSimpleName(paramName)));
                            }
                        }
                        string mName = AssMet.Id.GetText() + XSharpSpecialNames.AssignSuffix;
                        var stmt = GenerateExpressionStatement(GenerateMethodCall(mName, MakeArgumentList(a.ToArray())));
                        block = MakeBlock(stmt);
                        block.XGenerated = true;
                    }
                    else
                    {
                        var paramName = "value";
                        if (assParamCount > 0)
                        {
                            paramName = AssMet.ParamList._Params[0].Id.GetText();

                        }
                        else
                        {
                            missingParam = true;
                        }
                        // when the name of the original parameter is value, then there is no need to change things
                        if (String.Compare(paramName, "value", StringComparison.OrdinalIgnoreCase) == 0)
                        {
                            block = m.Body;
                        }
                        else
                        {
                            // else create a local with the original parameter name and an assignment from the new value parameter
                            var stmts = new List<StatementSyntax>();
                            var locdecl = GenerateLocalDecl(paramName, _impliedType, GenerateSimpleName("value"));
                            stmts.Add(locdecl);
                            foreach (var stmt in m.Body.Statements)
                            {
                                stmts.Add(stmt);
                            }
                            block = MakeBlock(stmts);
                        }
                    }
                }
                var accessor = _syntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration,
                        EmptyList<AttributeListSyntax>(), setMods.ToList<SyntaxToken>(),
                        SyntaxFactory.MakeToken(SyntaxKind.SetKeyword),
                        isInInterfaceOrAbstract ? null : block,
                        null,
                        isInInterfaceOrAbstract ? null : m.SemicolonToken);
                accessor.XGenerated = mergePartialDeclarations;
                if (vop.DupAssign != null)
                {
                    accessor = accessor.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_DuplicateAccessor));
                }
                if (missingParam)
                {
                    accessor = accessor.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_AssignMethodsMustHaveAParameter));
                }
                if (!paramMatch)
                {
                    var diag = new SyntaxDiagnosticInfo(ErrorCode.ERR_AccessAssignParametersMutchMatch);
                    accessor = accessor.WithAdditionalDiagnostics(diag);
                }
                if (!typeMatch)
                {
                    var diag = new SyntaxDiagnosticInfo(ErrorCode.ERR_AccessAssignTypesMutchMatch);
                    accessor = accessor.WithAdditionalDiagnostics(diag);
                }
                if (AssMet.CsNode is CSharpSyntaxNode node && node.ContainsDiagnostics)
                {
                    var diag = node.GetDiagnostics();
                    accessor = accessor.WithAdditionalDiagnostics(diag);
                }
                accessors.Add(accessor);
                AssMet.Put(accessor);
                if (AssMet.Parent is XP.ClsmethodContext)
                {
                    ((XP.ClsmethodContext)AssMet.Parent).CsNode = null;
                }
                if (xnode == null)
                    xnode = AssMet;
            }
            #endregion
            BasePropertyDeclarationSyntax prop;
            var accessorList = _syntaxFactory.AccessorList(SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                        accessors, SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));
            // A Property in Roslyn is either an Indexer (when there are parameters)
            // or a property
            if (voPropParams != null)
            {
                prop = _syntaxFactory.IndexerDeclaration(
                    attributeLists: EmptyList<AttributeListSyntax>(),
                    modifiers: outerMods.ToList<SyntaxToken>(),
                    type: voPropType,
                    explicitInterfaceSpecifier: null,
                    thisKeyword: SyntaxFactory.MakeToken(SyntaxKind.ThisKeyword, vop.idName.Text),
                    parameterList: voPropParams,
                    accessorList: accessorList,
                    expressionBody: null,
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            }
            else
            {
                prop = _syntaxFactory.PropertyDeclaration(
                    attributeLists: EmptyList<AttributeListSyntax>(),
                    modifiers: outerMods.ToList<SyntaxToken>(),
                    type: voPropType,
                    explicitInterfaceSpecifier: null,
                    identifier: vop.idName,
                    accessorList: accessorList,
                    expressionBody: null,
                    initializer: null,
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            }

            _pool.Free(accessors);
            _pool.Free(getMods);
            _pool.Free(setMods);
            _pool.Free(outerMods);
            prop.XNode = xnode;
            return prop;
        }

        #endregion

        #region Generic Visitor methods
        public override void VisitErrorNode([NotNull] IErrorNode node)
        {
        }

        public override void VisitTerminal(ITerminalNode node)
        {
        }

        public override void EnterEveryRule([NotNull] ParserRuleContext context)
        {
#if DEBUG && DUMP_TREE
            var s = context.GetType().ToString();
            s = s.Substring(s.LastIndexOfAny(".+".ToCharArray())+1);
            s = s.Replace("Context","");
            Debug.WriteLine("{0}=> ({1},{2}) {3} [{4}] <{5}>",new string(' ',context.Depth()),context.Start.Line,context.Start.Column,s,context.Start.Text,XP.DefaultVocabulary.GetSymbolicName(context.Start.Type));
#endif

            if (context is XP.IEntityContext)
                Entities.Push((XP.IEntityContext)context);

        }

        public override void ExitEveryRule([NotNull] ParserRuleContext ctxt)
        {
            var context = ctxt as XSharpParserRuleContext;
            if (context.HasErrors() && context.CsNode != null && context.CsNode is CSharpSyntaxNode)
            {
                foreach (var e in context.ErrorData)
                {
                    var csNode = (CSharpSyntaxNode)context.CsNode;
                    context.Put(csNode.WithAdditionalDiagnostics(
                        new SyntaxDiagnosticInfo(csNode.GetLeadingTriviaWidth(), csNode.Width, e.Code, e.Args)));
                }
            }
#if DEBUG && DUMP_TREE
            var s = context.GetType().ToString();
            s = s.Substring(s.LastIndexOfAny(".+".ToCharArray())+1);
            s = s.Replace("Context","");
            Debug.WriteLine("{0}<= ({1},{2}) {3} '{4}'",new string(' ',context.Depth()),context.Start.Line,context.Start.Column,s,context.Start.Text);
#endif
            if (context is XP.IEntityContext)
                Entities.Pop();
        }
        #endregion

        #region Main Entrypoints

        public override void EnterScript([NotNull] XP.ScriptContext context)
        {
            GlobalClassEntities = CreateClassEntities();
            ClassEntities.Push(GlobalClassEntities);

            if (context._References?.Count > 0 || context._Includes?.Count > 0)
            {
                var dirs = _pool.Allocate();
                foreach (var @ref in context._References)
                {
                    var arg = @ref.SyntaxLiteralValue(_options);
                    dirs.Add(_syntaxFactory.ReferenceDirectiveTrivia(SyntaxFactory.MissingToken(SyntaxKind.HashToken),
                        SyntaxFactory.MissingToken(SyntaxKind.ReferenceKeyword),
                        arg,
                        SyntaxFactory.MissingToken(SyntaxKind.EndOfDirectiveToken), true));
                }
                foreach (var inc in context._Includes)
                {
                    var arg = inc.SyntaxLiteralValue(_options);
                    dirs.Add(_syntaxFactory.LoadDirectiveTrivia(SyntaxFactory.MissingToken(SyntaxKind.HashToken),
                        SyntaxFactory.MissingToken(SyntaxKind.LoadKeyword),
                        arg,
                        SyntaxFactory.MissingToken(SyntaxKind.EndOfDirectiveToken), true));
                }
                // HACK: Insert a dummy extern alias to ensure it is the first token in the compilation unit (it will be ignored during imports handling)
                GlobalEntities.Externs.Add(_syntaxFactory.ExternAliasDirective(
                    SyntaxFactory.MissingToken(dirs.ToListNode(), SyntaxKind.ExternKeyword, null),
                    SyntaxFactory.MissingToken(SyntaxKind.AliasKeyword),
                    SyntaxFactory.Identifier(XSharpSpecialNames.ScriptDummy),
                    SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)
                    ));
                _pool.Free(dirs);
            }
        }

        public override void ExitScript([NotNull] XP.ScriptContext context)
        {
            var generated = ClassEntities.Pop();
            GlobalEntities.Members.AddRange(generated.Members);
            generated.Free();

            // Add: using static Functions
            AddUsingWhenMissing(GlobalEntities.Usings, this.GlobalClassName, true, null);

            // Add: using System
            AddUsingWhenMissing(GlobalEntities.Usings, "System", false, null);
        }

        public override void ExitScriptEntity([NotNull] XP.ScriptEntityContext context)
        {
            if (context.Entity != null)
            {
                var s = context.Entity.CsNode;

                if (s is NamespaceDeclarationSyntax)
                {
                    context.AddError(new ParseErrorData(context.Entity, ErrorCode.ERR_NamespaceNotAllowedInScript));
                    GlobalEntities.Members.Add(s as MemberDeclarationSyntax);
                }
                else if (s is MemberDeclarationSyntax)
                    GlobalEntities.Members.Add(s as MemberDeclarationSyntax);
                else if (s is UsingDirectiveSyntax)
                {
                    var u = s as UsingDirectiveSyntax;
                    AddUsingWhenMissing(GlobalEntities.Usings, u.Name, u.StaticKeyword != null, u.Alias);
                }
                else if (s is AttributeListSyntax)
                {
                    context.AddError(new ParseErrorData(context.Entity, ErrorCode.ERR_AttributesNotAllowed));
                    GlobalEntities.Attributes.Add(s as AttributeListSyntax);
                }
                else if (s is ExternAliasDirectiveSyntax)
                {
                    GlobalEntities.Externs.Add(s as ExternAliasDirectiveSyntax);
                }
            }
            else if (context.Stmt != null)
            {
                var s = context.Stmt.CsNode;
                if (s is SyntaxList<StatementSyntax>)
                {
                    foreach (var stmt in context.Stmt.GetList<StatementSyntax>())
                    {
                        if ((stmt is LocalDeclarationStatementSyntax) && !stmt.ContainsDiagnostics)
                        {
                            var local = (LocalDeclarationStatementSyntax)stmt;
                            var decl = _syntaxFactory.FieldDeclaration(
                                EmptyList<AttributeListSyntax>(),
                                local.Modifiers,
                                local.Declaration,
                                local.SemicolonToken);
                            GlobalClassEntities.Members.Add(decl);
                        }
                        else
                        {
                            GlobalClassEntities.Members.Add(_syntaxFactory.GlobalStatement(stmt));
                        }
                    }
                }
                else if (context.Stmt is XP.ExpressionStmtContext)
                {
                    if (s is BlockSyntax)
                    {
                        var b = (BlockSyntax)s;
                        foreach (var stmt in b.Statements)
                        {
                            GlobalClassEntities.Members.Add(_syntaxFactory.GlobalStatement(
                                _syntaxFactory.ExpressionStatement(
                                    ((ExpressionStatementSyntax)stmt).Expression,
                                    SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken))
                                ));
                        }
                    }
                    else
                    {
                        GlobalClassEntities.Members.Add(_syntaxFactory.GlobalStatement(
                            _syntaxFactory.ExpressionStatement(
                                ((ExpressionStatementSyntax)s).Expression,
                                SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken))
                            ));

                    }
                }
                else
                {
                    GlobalClassEntities.Members.Add(_syntaxFactory.GlobalStatement(context.Stmt.Get<StatementSyntax>()));
                }
            }
            /*else if (context.Decl != null)
            {
                GlobalEntities.Members.Add(context.Decl.Get<MemberDeclarationSyntax>());
            }*/
        }

        public override void EnterMacroScript([NotNull] XP.MacroScriptContext context)
        {
            GlobalClassEntities = CreateClassEntities();
            ClassEntities.Push(GlobalClassEntities);
        }

        public override void ExitMacroScript([NotNull] XP.MacroScriptContext context)
        {
            ExpressionSyntax e;
            if (context.CbExpr != null)
            {
                e = context.CbExpr.Get<ExpressionSyntax>();
                e.XNode = null;
            }
            else if (context.Code != null)
            {
                e = _syntaxFactory.ParenthesizedLambdaExpression(
                    asyncKeyword: null,
                    parameterList: EmptyParameterList(),
                    arrowToken: SyntaxFactory.MakeToken(SyntaxKind.EqualsGreaterThanToken),
                    body: context.Code.Get<CSharpSyntaxNode>());
            }
            else /* should never happen! */
            {
                e = _syntaxFactory.ParenthesizedLambdaExpression(
                    asyncKeyword: null,
                    parameterList: EmptyParameterList(),
                    arrowToken: SyntaxFactory.MakeToken(SyntaxKind.EqualsGreaterThanToken),
                    body: MakeBlock(_syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))));
            }
            /*if (_options.HasRuntime)
            {
                var decl = GenerateLocalDecl("$result", _syntaxFactory.IdentifierName(SyntaxFactory.MakeIdentifier("CODEBLOCK")), e);
                GlobalClassEntities.Members.Add(_syntaxFactory.GlobalStatement(decl));
                e = _syntaxFactory.IdentifierName(SyntaxFactory.MakeIdentifier("$res"));
            }*/
            GlobalClassEntities.Members.Add(_syntaxFactory.GlobalStatement(
                _syntaxFactory.ExpressionStatement(
                    e,
                    SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken))
                ));

            var generated = ClassEntities.Pop();
            GlobalEntities.Members.AddRange(generated.Members);
            generated.Free();

            // Add: using static Functions
            AddUsingWhenMissing(GlobalEntities.Usings, this.GlobalClassName, true, null);

            // Add: using System
            AddUsingWhenMissing(GlobalEntities.Usings, "System", false, null);
        }

        protected void _enterSource()
        {
            //System.Diagnostics.Debug.WriteLine("Enter Source " + _fileName);
            GlobalClassEntities = CreateClassEntities();
            ClassEntities.Push(GlobalClassEntities);
        }
        public override void EnterSource([NotNull] XP.SourceContext context)
        {
            _enterSource();
        }

        protected void ProcessEntity(SyntaxListBuilder<MemberDeclarationSyntax> globalTypes, XSharpParserRuleContext context)
        {
            var s = context.CsNode;
            if (s is NamespaceDeclarationSyntax)
            {
                GlobalEntities.Members.Add(s as MemberDeclarationSyntax);
            }
            else if (s is MemberDeclarationSyntax)
            {
                globalTypes.Add(s as MemberDeclarationSyntax);
            }
            else if (s is UsingDirectiveSyntax)
            {
                var u = s as UsingDirectiveSyntax;
                AddUsingWhenMissing(GlobalEntities.Usings, u.Name, u.StaticKeyword != null, u.Alias);
            }
            else if (s is AttributeListSyntax)
            {
                GlobalEntities.Attributes.Add(s as AttributeListSyntax);
            }
            else if (s is ExternAliasDirectiveSyntax)
            {
                GlobalEntities.Externs.Add(s as ExternAliasDirectiveSyntax);
            }

        }
        protected void finishCompilationUnit(SyntaxListBuilder<MemberDeclarationSyntax> globalTypes)
        {
            FinalizeGlobalEntities();

            var generated = ClassEntities.Pop();
            if (generated.Members.Count > 0)
            {
                GlobalEntities.Members.Add(GenerateGlobalClass(GlobalClassName, false, false, generated.Members));
            }
            generated.Free();

            if (!string.IsNullOrEmpty(_options.DefaultNamespace))
            {
                GlobalEntities.Members.Add(_syntaxFactory.NamespaceDeclaration(SyntaxFactory.MakeToken(SyntaxKind.NamespaceKeyword),
                    name: GenerateQualifiedName(_options.DefaultNamespace),
                    openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    externs: MakeList<ExternAliasDirectiveSyntax>(),
                    usings: MakeList<UsingDirectiveSyntax>(),
                    members: globalTypes,
                    closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));

                AddUsingWhenMissing(GlobalEntities.Usings, _options.DefaultNamespace, false, null);
            }
            else
            {
                GlobalEntities.Members.AddRange(globalTypes);
            }
            // Add: using static Functions
            AddUsingWhenMissing(GlobalEntities.Usings, this.GlobalClassName, true, null);

            // Add: using System
            AddUsingWhenMissing(GlobalEntities.Usings, "System", false, null);
        }

        protected void _exitSource(XSharpParserRuleContext context, IList<XSharpParserRuleContext> entities)
        {
            // globaltypes are the types that are not embedded in a namespace
            // they will be embedded in the default namespace when the 
            // compiler option to do so is selected
            // GlobalEntities.Members will be added to the output without extra
            // namespace
            var globalTypes = _pool.Allocate<MemberDeclarationSyntax>();
            foreach (var entityCtx in entities)
            {
                ProcessEntity(globalTypes, entityCtx);
            }
            finishCompilationUnit(globalTypes);
            _pool.Free(globalTypes);

            //System.Diagnostics.Debug.WriteLine("Exit Source " + _fileName);
        }
        public override void ExitSource([NotNull] XP.SourceContext context)
        {
            var entities = new List<XSharpParserRuleContext>();
            entities.AddRange(context._Entities);

            _exitSource(context, entities);
        }
        private string RemoveUnwantedCharacters(string input)
        {
            StringBuilder result = new StringBuilder(input.Length);
            foreach (char ch in input)
            {
                if (Char.IsLetterOrDigit(ch))
                    result.Append(ch);
                else
                    result.Append('_');
            }
            return result.ToString();

        }

        public void FinalizeGlobalEntities()
        {
            if (GlobalEntities.GlobalClassMembers.Count > 0)
            {
                AddUsingWhenMissing(GlobalEntities.Usings, GlobalClassName, true, null);
                GlobalEntities.Members.Add(GenerateGlobalClass(GlobalClassName, false, false, GlobalEntities.GlobalClassMembers));
                GlobalEntities.GlobalClassMembers.Clear();

            }
            if (GlobalEntities.StaticGlobalClassMembers.Count > 0)
            {
                string filename = PathUtilities.GetFileName(_fileName);
                filename = PathUtilities.RemoveExtension(filename);
                filename = RemoveUnwantedCharacters(filename);
                string className = GlobalClassName + "$" + filename + "$";
                AddUsingWhenMissing(GlobalEntities.Usings, className, true, null);
                GlobalEntities.Members.Add(GenerateGlobalClass(className, false, false, GlobalEntities.StaticGlobalClassMembers));
                GlobalEntities.StaticGlobalClassMembers.Clear();
            }

        }

        public override void ExitUsing_([NotNull] XP.Using_Context context)
        {
            context.Put(_syntaxFactory.UsingDirective(SyntaxFactory.MakeToken(SyntaxKind.UsingKeyword),
                staticKeyword: context.Static?.SyntaxKeyword(),
                alias: context.Alias == null ? null : _syntaxFactory.NameEquals(context.Alias.Get<IdentifierNameSyntax>(), SyntaxFactory.MakeToken(SyntaxKind.EqualsToken)),
                name: context.Name.Get<NameSyntax>(),
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        protected void _exitNamespace(XSharpParserRuleContext context, string name, XSharpParserRuleContext ignored, IList<XSharpParserRuleContext> entities)
        {
            var externs = _pool.Allocate<ExternAliasDirectiveSyntax>();
            var usings = _pool.Allocate<UsingDirectiveSyntax>();
            var members = _pool.Allocate<MemberDeclarationSyntax>();
            foreach (var entityCtx in entities)
            {
                var s = entityCtx.CsNode;
                if (s is MemberDeclarationSyntax)
                    members.Add(s as MemberDeclarationSyntax);
                else if (s is UsingDirectiveSyntax)
                    usings.Add(s as UsingDirectiveSyntax);
                else if (s is AttributeListSyntax)
                    //Attributes.Add(s as AttributeListSyntax);
                    context.AddError(new ParseErrorData(entityCtx, ErrorCode.ERR_AttributesNotAllowed));
                else if (s is ExternAliasDirectiveSyntax)
                    externs.Add(s as ExternAliasDirectiveSyntax);
            }

            MemberDeclarationSyntax ns = _syntaxFactory.NamespaceDeclaration(SyntaxFactory.MakeToken(SyntaxKind.NamespaceKeyword),
                name: GenerateQualifiedName(name),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                externs: externs,
                usings: usings,
                members: members,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));

            _pool.Free(externs);
            _pool.Free(usings);
            _pool.Free(members);
            ns = CheckForGarbage(ns, ignored, "Name after END NAMESPACE");
            context.Put(ns);
            // Now add our namespace to the usings list so functions etc can find members 
            string ourname = name;
            var parent = context.Parent;
            while (parent is XP.EntityContext)
            {
                var parentns = parent.Parent as XP.Namespace_Context;
                if (parentns != null)
                {
                    ourname = parentns.Name.GetText() + "." + ourname;
                    parent = parentns.Parent;
                }
                else
                    break;
            }
            AddUsingWhenMissing(GlobalEntities.Usings, ourname, false, null);

        }
        public override void ExitNamespace_([NotNull] XP.Namespace_Context context)
        {
            var entities = new List<XSharpParserRuleContext>();
            entities.AddRange(context._Entities);
            _exitNamespace(context, context.Name.GetText(), context.Ignored, entities);
        }

        protected void addGlobalEntity(MemberDeclarationSyntax m, bool isStatic)
        {
            // make sure we do not add entities twice
            if (isStatic)
            {
                // When last entity did not go to the functions class or wasn't a static member
                if (GlobalEntities.LastMember is XP.IGlobalEntityContext && !GlobalEntities.LastIsStatic)
                {
                    FinalizeGlobalEntities();
                }
                GlobalEntities.StaticGlobalClassMembers.Add(m);
            }
            else
            {
                // When last entity did not go to the functions class or was a static member
                if (GlobalEntities.LastMember is XP.IGlobalEntityContext && GlobalEntities.LastIsStatic)
                {
                    FinalizeGlobalEntities();
                }
                GlobalEntities.GlobalClassMembers.Add(m);
            }
            GlobalEntities.LastMember = m;
            GlobalEntities.LastIsStatic = isStatic;

        }

        protected void ProcessGlobalEntityContext(XP.IGlobalEntityContext entity)
        {
            var modifiers = ((XP.IGlobalEntityContext)entity).FuncProcModifiers;
            if (entity is XP.ProcedureContext)
            {
                var proc = (XP.ProcedureContext)entity;
                if (proc.InitExit != null)  // Init & Exit procedures are never static
                {
                    modifiers = null;
                }
            }
            var bStaticVisibility = false;
            if (modifiers != null)
                bStaticVisibility = modifiers.IsStaticVisible;


            var m = entity.Get<MemberDeclarationSyntax>();
            addGlobalEntity(m, bStaticVisibility);
        }
        protected void ProcessNonGlobalEntity(XSharpParserRuleContext context, IXParseTree entity)
        {
            // When last entity has to go to the functions class 
            if (GlobalEntities.LastMember is XP.IGlobalEntityContext)
            {
                FinalizeGlobalEntities();
            }
            context.Put(entity.Get<CSharpSyntaxNode>());
            GlobalEntities.LastMember = entity;
        }

        protected void _exitEntity(XSharpParserRuleContext context)
        {
            var entity = context.children[0] as IXParseTree;
            if (_isScript)
            {
                context.Put(entity.Get<CSharpSyntaxNode>());
            }
            else if (entity is XP.IGlobalEntityContext) // procedure, function, DLL function, GLobal, Define
            {
                ProcessGlobalEntityContext(entity as XP.IGlobalEntityContext);
            }
            else
            {
                // When last entity has to go to the functions class 
                ProcessNonGlobalEntity(context, entity);
            }
        }

        public override void ExitEntity([NotNull] XP.EntityContext context)
        {
            _exitEntity(context);
        }
        #endregion
        #region Types

        #region User Defined Types
        public override void EnterInterface_([NotNull] XP.Interface_Context context)
        {
            ClassEntities.Push(CreateClassEntities());
        }

        public override void ExitInterface_([NotNull] XP.Interface_Context context)
        {
            var members = _pool.Allocate<MemberDeclarationSyntax>();
            var generated = ClassEntities.Pop();
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility();
            context.Data.Partial = mods.Any((int)SyntaxKind.PartialKeyword);
            if (generated.Members.Count > 0)
            {
                members.AddRange(generated.Members);
            }
            if (generated.VoProperties != null)
            {
                foreach (var vop in generated.VoProperties.Values)
                {
                    var prop = GenerateVoProperty(vop, context);
                    if (prop != null)
                        members.Add(prop);
                }
            }
            // Do this after VOProps generation because GenerateVOProperty sets the members
            // for Access & Assign to NULL
            foreach (var mCtx in context._Members)
            {
                if (mCtx.CsNode != null)
                    members.Add(mCtx.Get<MemberDeclarationSyntax>());
            }
            generated.Free();
            var baseTypes = _pool.AllocateSeparated<BaseTypeSyntax>();
            foreach (var pCtx in context._Parents)
            {
                if (baseTypes.Count > 0)
                    baseTypes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                baseTypes.Add(_syntaxFactory.SimpleBaseType(pCtx.Get<TypeSyntax>()));
            }
            MemberDeclarationSyntax m = _syntaxFactory.InterfaceDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: mods,
                keyword: SyntaxFactory.MakeToken(SyntaxKind.InterfaceKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                baseList: _syntaxFactory.BaseList(SyntaxFactory.MakeToken(SyntaxKind.ColonToken), baseTypes),
                constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: members,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null);
            _pool.Free(members);
            _pool.Free(baseTypes);
            if (context.Namespace != null)
            {
                m = AddNameSpaceToMember(context.Namespace, m);
            }
            else
            {
                m = (MemberDeclarationSyntax)CheckTypeName(context, "INTERFACE", m);
            }
            m = CheckForGarbage(m, context.Ignored, "Name after END INTERFACE");
            context.Put(m);
            if (context.Data.Partial)
            {
                GlobalEntities.NeedsProcessing = true;
            }
        }

        public override void EnterClass_([NotNull] XP.Class_Context context)
        {
            ClassEntities.Push(CreateClassEntities());
        }

        public override void ExitClass_([NotNull] XP.Class_Context context)
        {
            var members = _pool.Allocate<MemberDeclarationSyntax>();
            var generated = ClassEntities.Pop();
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility();
            context.Data.Partial = mods.Any((int)SyntaxKind.PartialKeyword);
            if (generated.Members.Count > 0)
            {
                members.AddRange(generated.Members);
            }
            if (generated.VoProperties != null)
            {
                foreach (var vop in generated.VoProperties.Values)
                {
                    var prop = GenerateVoProperty(vop, context);
                    if (prop != null)
                        members.Add(prop);
                }
            }
            // check if class has Ctor. 
            foreach (var mem in members.ToList())
            {
                // when an instant constructors then remember this
                if (mem is ConstructorDeclarationSyntax && !((ConstructorDeclarationSyntax)mem).IsStatic())
                {
                    context.Data.HasInstanceCtor = true;
                    break;
                }
            }

            // Do this after VOProps generation because GenerateVOProperty sets the members
            // for Access & Assign to NULL
            foreach (var mCtx in context._Members)
            {
                if (mCtx.CsNode != null)
                    members.Add(mCtx.Get<MemberDeclarationSyntax>());
            }
            generated.Free();
            var baseTypes = _pool.AllocateSeparated<BaseTypeSyntax>();
            var baseType = context.BaseType?.Get<TypeSyntax>();
            if (baseType != null)
            {
                baseTypes.Add(_syntaxFactory.SimpleBaseType(baseType));
            }
            foreach (var iCtx in context._Implements)
            {
                if (baseTypes.Count > 0)
                    baseTypes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                baseTypes.Add(_syntaxFactory.SimpleBaseType(iCtx.Get<TypeSyntax>()));
            }

            MemberDeclarationSyntax m = _syntaxFactory.ClassDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: mods,
                keyword: SyntaxFactory.MakeToken(SyntaxKind.ClassKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                baseList: _syntaxFactory.BaseList(SyntaxFactory.MakeToken(SyntaxKind.ColonToken), baseTypes),
                constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: members,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null);
            _pool.Free(members);
            _pool.Free(baseTypes);
            if (context.Namespace != null)
            {
                m = AddNameSpaceToMember(context.Namespace, m);

            }
            else
            {
                m = (MemberDeclarationSyntax)CheckTypeName(context, "CLASS", m);
            }
            m = CheckForGarbage(m, context.Ignored, "Name after END CLASS");
            context.Put(m);
            if (context.Data.Partial)
            {
                GlobalEntities.NeedsProcessing = true;
            }
        }

        public override void EnterStructure_([NotNull] XP.Structure_Context context)
        {
            ClassEntities.Push(CreateClassEntities());
        }

        public override void ExitStructure_([NotNull] XP.Structure_Context context)
        {
            var members = _pool.Allocate<MemberDeclarationSyntax>();
            var generated = ClassEntities.Pop();
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility();
            context.Data.Partial = mods.Any((int)SyntaxKind.PartialKeyword);
            if (generated.Members.Count > 0)
            {
                members.AddRange(generated.Members);
            }
            if (generated.VoProperties != null)
            {
                foreach (var vop in generated.VoProperties.Values)
                {
                    var prop = GenerateVoProperty(vop, context);
                    if (prop != null)
                        members.Add(prop);
                }
            }
            // Do this after VOProps generation because GenerateVOProperty sets the members
            // for Access & Assign to NULL
            foreach (var mCtx in context._Members)
            {
                if (mCtx.CsNode != null)
                    members.Add(mCtx.Get<MemberDeclarationSyntax>());
            }
            generated.Free();
            var baseTypes = _pool.AllocateSeparated<BaseTypeSyntax>();
            foreach (var iCtx in context._Implements)
            {
                if (baseTypes.Count > 0)
                    baseTypes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                baseTypes.Add(_syntaxFactory.SimpleBaseType(iCtx.Get<TypeSyntax>()));
            }

            MemberDeclarationSyntax m = _syntaxFactory.StructDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: mods,
                keyword: SyntaxFactory.MakeToken(SyntaxKind.StructKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                baseList: _syntaxFactory.BaseList(SyntaxFactory.MakeToken(SyntaxKind.ColonToken), baseTypes),
                constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: members,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null);
            _pool.Free(members);
            _pool.Free(baseTypes);
            if (context.Namespace != null)
            {
                m = AddNameSpaceToMember(context.Namespace, m);
            }
            else
            {
                m = (MemberDeclarationSyntax)CheckTypeName(context, "STRUCTURE", m);
            }
            m = CheckForGarbage(m, context.Ignored, "Name after END STRUCTURE");
            context.Put(m);
            if (context.Data.Partial)
            {
                GlobalEntities.NeedsProcessing = true;
            }
        }

        public override void ExitDelegate_([NotNull] XP.Delegate_Context context)
        {
            MemberDeclarationSyntax m = _syntaxFactory.DelegateDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(),
                delegateKeyword: SyntaxFactory.MakeToken(SyntaxKind.DelegateKeyword),
                returnType: context.Type?.Get<TypeSyntax>() ?? MissingType(),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                parameterList: context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
                constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            if (context.Namespace != null)
            {
                m = AddNameSpaceToMember(context.Namespace, m);
            }
            else
            {
                m = (MemberDeclarationSyntax)CheckTypeName(context, "STRUCTURE", m);
            }
            context.Put(m);
        }
        #endregion

        #region Enums
        public override void ExitEnum_([NotNull] XP.Enum_Context context)
        {
            BaseListSyntax baselist = default(BaseListSyntax);
            var baseTypes = _pool.AllocateSeparated<BaseTypeSyntax>();
            if (context.Type != null)
            {
                baseTypes.Add(_syntaxFactory.SimpleBaseType(context.Type.Get<TypeSyntax>()));
                baselist = _syntaxFactory.BaseList(
                    SyntaxFactory.MakeToken(SyntaxKind.ColonToken), baseTypes);

            }
            MemberDeclarationSyntax m = _syntaxFactory.EnumDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(),
                enumKeyword: SyntaxFactory.MakeToken(SyntaxKind.EnumKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                baseList: baselist,
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: MakeSeparatedList<EnumMemberDeclarationSyntax>(context._Members),
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            if (context.Namespace != null)
            {
                m = AddNameSpaceToMember(context.Namespace, m);
            }
            m = CheckForGarbage(m, context.Ignored, "Name after END ENUM");
            context.Put(m);
            _pool.Free(baseTypes);
        }

        public override void ExitEnummember([NotNull] XP.EnummemberContext context)
        {
            context.Put(_syntaxFactory.EnumMemberDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                identifier: context.Id.Get<SyntaxToken>(),
                equalsValue: context.Expr == null ? null : _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                    context.Expr.Get<ExpressionSyntax>())));
        }

        #endregion

        #region Events
        public override void ExitEvent_([NotNull] XP.Event_Context context)
        {
            if (context.Multi != null)
                context.SetSequencePoint(context.Multi.Start);
            else
                context.SetSequencePoint(context.end);

            var attrLists = context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>();
            var type_ = context.Type?.Get<TypeSyntax>() ?? MissingType();
            var singleLine = context._LineAccessors != null && context._LineAccessors.Count > 0;
            var multiLine = context.Multi != null && context._Accessors.Count > 0;
            ExplicitInterfaceSpecifierSyntax explif = null;
            if (context.ExplicitIface != null)
            {
                explif = _syntaxFactory.ExplicitInterfaceSpecifier(
                        name: context.ExplicitIface.Get<NameSyntax>(),
                        dotToken: SyntaxFactory.MakeToken(SyntaxKind.DotToken));
            }
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? DefaultMethodModifiers(context.isInInterface(), context.isInStructure());
            //if (context.ExplicitIface != null)
            {
                var m = _pool.Allocate();
                foreach (var mod in mods)
                {
                    if (singleLine || multiLine)
                    {
                        if (mod.Kind != SyntaxKind.VirtualKeyword && mod.Kind != SyntaxKind.OverrideKeyword)
                            m.Add(mod);
                    }
                    else
                    {
                        if (mod.Kind != SyntaxKind.VirtualKeyword && mod.Kind != SyntaxKind.OverrideKeyword && mod.Kind != SyntaxKind.PublicKeyword)
                            m.Add(mod);
                    }
                }
                mods = m.ToList<SyntaxToken>();
                _pool.Free(m);
            }

            if (singleLine)         // Single Line Syntax
            {
                var acclist = _syntaxFactory.AccessorList(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    MakeList<AccessorDeclarationSyntax>(context._LineAccessors),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));
                MemberDeclarationSyntax decl = _syntaxFactory.EventDeclaration(
                    attributeLists: attrLists,
                    modifiers: mods,
                    eventKeyword: SyntaxFactory.MakeToken(SyntaxKind.EventKeyword),
                    type: type_,
                    explicitInterfaceSpecifier: explif,
                    identifier: context.Id.Get<SyntaxToken>(),
                    accessorList: acclist);
                decl = CheckForGarbage(decl, context.Ignored, "Name after END EVENT");
                context.Put(decl);
            }
            else if (multiLine)        // Multi line Syntax
            {
                var acclist = _syntaxFactory.AccessorList(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    MakeList<AccessorDeclarationSyntax>(context._Accessors),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));
                MemberDeclarationSyntax decl = _syntaxFactory.EventDeclaration(
                    attributeLists: attrLists,
                    modifiers: mods,
                    eventKeyword: SyntaxFactory.MakeToken(SyntaxKind.EventKeyword),
                    type: type_,
                    explicitInterfaceSpecifier: explif,
                    identifier: context.Id.Get<SyntaxToken>(),
                    accessorList: acclist);
                decl = CheckForGarbage(decl, context.Ignored, "Name after END EVENT");
                context.Put(decl);
            }
            else // Old Syntax, auto generate accessors
            {
                if (context.ExplicitIface != null)
                {
                    string evtFldName = XSharpSpecialNames.EventFieldNamePrefix + context.Id.Get<SyntaxToken>();
                    ClassEntities.Peek().Members.Add(
                        _syntaxFactory.FieldDeclaration(
                            EmptyList<AttributeListSyntax>(),
                            TokenList(SyntaxKind.StaticKeyword, SyntaxKind.InternalKeyword),
                            _syntaxFactory.VariableDeclaration(context.Type?.Get<TypeSyntax>() ?? MissingType(),
                                MakeSeparatedList(GenerateVariable(evtFldName))),
                            SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))
                        );
                    var add_ = _syntaxFactory.AccessorDeclaration(SyntaxKind.AddAccessorDeclaration,
                            attributeLists: EmptyList<AttributeListSyntax>(),
                            modifiers: EmptyList(),
                            keyword: SyntaxFactory.MakeToken(SyntaxKind.AddKeyword),
                            body: MakeBlock(
                                MakeLock(GenerateSimpleName(evtFldName),
                                    GenerateExpressionStatement(
                                        _syntaxFactory.AssignmentExpression(SyntaxKind.AddAssignmentExpression,
                                            GenerateSimpleName(evtFldName),
                                            SyntaxFactory.MakeToken(SyntaxKind.PlusEqualsToken),
                                            GenerateSimpleName("value"))))
                                ),
                            expressionBody: null,
                            semicolonToken: null);
                    var remove_ = _syntaxFactory.AccessorDeclaration(SyntaxKind.RemoveAccessorDeclaration,
                                    attributeLists: EmptyList<AttributeListSyntax>(),
                                    modifiers: EmptyList(),
                                    keyword: SyntaxFactory.MakeToken(SyntaxKind.RemoveKeyword),
                                    body: MakeBlock(MakeLock(GenerateSimpleName(evtFldName),
                                            GenerateExpressionStatement(
                                                _syntaxFactory.AssignmentExpression(SyntaxKind.SubtractAssignmentExpression,
                                                    GenerateSimpleName(evtFldName),
                                                    SyntaxFactory.MakeToken(SyntaxKind.MinusEqualsToken),
                                                    GenerateSimpleName("value"))))
                                        ),
                                    expressionBody: null,
                                    semicolonToken: null);
                    var acclist = _syntaxFactory.AccessorList(
                        SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                        MakeList<AccessorDeclarationSyntax>(add_, remove_),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));
                    MemberDeclarationSyntax decl = _syntaxFactory.EventDeclaration(
                        attributeLists: attrLists,
                        modifiers: mods,
                        eventKeyword: SyntaxFactory.MakeToken(SyntaxKind.EventKeyword),
                        type: type_,
                        explicitInterfaceSpecifier: explif,
                        identifier: context.Id.Get<SyntaxToken>(),
                        accessorList: acclist);
                    decl = CheckForGarbage(decl, context.Ignored, "Name after END EVENT");
                    context.Put(decl);
                }
                else
                {
                    context.Put(_syntaxFactory.EventFieldDeclaration(
                        attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                        modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? DefaultMethodModifiers(context.isInInterface(), context.isInStructure()),
                        eventKeyword: SyntaxFactory.MakeToken(SyntaxKind.EventKeyword),
                        declaration: _syntaxFactory.VariableDeclaration(
                            context.Type?.Get<TypeSyntax>() ?? MissingType(),
                            MakeSeparatedList<VariableDeclaratorSyntax>(
                                GenerateVariable(context.Id.Get<SyntaxToken>()))),
                        semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                }
            }
        }

        public override void ExitEventLineAccessor([NotNull] XP.EventLineAccessorContext context)
        {
            context.ExprList.SetSequencePoint();
            context.Put(_syntaxFactory.AccessorDeclaration(context.Key.AccessorKind(),
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                keyword: context.Key.SyntaxKeyword(),
                body: MakeBlock(context.ExprList?.GetList<StatementSyntax>() ?? EmptyList<StatementSyntax>()),
                expressionBody: null,
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }
        public override void EnterEventAccessor([NotNull] XP.EventAccessorContext context)
        {
            context.Data.MustBeVoid = true;
        }
        public override void ExitEventAccessor([NotNull] XP.EventAccessorContext context)
        {
            context.StmtBlk.SetSequencePoint();
            context.Put(_syntaxFactory.AccessorDeclaration(context.Key.AccessorKind(),
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                keyword: context.Key.SyntaxKeyword(),
                body: context.StmtBlk.Get<BlockSyntax>(),
                expressionBody: null,
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }
        #endregion

        #region Class Vars

        public override void ExitArraysub([NotNull] XP.ArraysubContext context)
        {
            context.Put(_syntaxFactory.ArrayRankSpecifier(
                SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                MakeSeparatedList<ExpressionSyntax>(context._ArrayIndex),
                SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)));
        }



        public override void ExitClassvars([NotNull] XP.ClassvarsContext context)
        {
            var varList = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            var varType = context.Vars?.DataType?.Get<TypeSyntax>() ?? _getMissingType();
            bool isFixed = context.Modifiers?._FIXED != null;
            varType.XVoDecl = true;
            if (context.Vars?.As?.Type == XP.IS)
            {
                varType.XVoIsDecl = true;
            }
            foreach (var varCtx in context.Vars._Var)
            {
                bool isDim = varCtx.Dim != null && varCtx.ArraySub != null;
                if (isDim)
                {
                    if (isFixed)
                    {
                        ClassEntities.Peek().Members.Add(_syntaxFactory.FieldDeclaration(
                            attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                            modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(),
                            declaration: _syntaxFactory.VariableDeclaration(
                                type: varType,
                                variables: MakeSeparatedList(varCtx.Get<VariableDeclaratorSyntax>())),
                            semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                    }
                    else
                    {
                        ClassEntities.Peek().Members.Add(_syntaxFactory.FieldDeclaration(
                            attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                            modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(),
                            declaration: _syntaxFactory.VariableDeclaration(
                                type: _syntaxFactory.ArrayType(varType, MakeArrayRankSpecifier(varCtx.ArraySub._ArrayIndex.Count)),
                                variables: MakeSeparatedList(varCtx.Get<VariableDeclaratorSyntax>())),
                            semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                    }
                }
                else
                {
                    if (varList.Count > 0)
                        varList.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                    varList.Add(varCtx.Get<VariableDeclaratorSyntax>());
                }
            }
            if (varList.Count > 0)
            {
                var decl = _syntaxFactory.VariableDeclaration(
                        type: varType,
                        variables: varList);
                var attributeList = context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>();
                if (context.Modifiers == null)
                {
                    context.AddError(new ParseErrorData(context, ErrorCode.ERR_SyntaxError, "Classvar Modifier (EXPORT, PROTECTED, HIDDEN, PRIVATE, PUBLIC, INSTANCE, STATIC)  expected"));
                }
                else if (_options.HasRuntime)
                {

                    bool isInstance = context.Modifiers._Tokens.Any(t => t.Type == XSharpLexer.INSTANCE);
                    if (isInstance)
                    {
                        var attr = _pool.Allocate<AttributeListSyntax>();
                        attr.AddRange(attributeList);
                        GenerateAttributeList(attr, _options.XSharpRuntime ? XSharpQualifiedTypeNames.IsInstance : VulcanQualifiedTypeNames.IsInstance);
                        attributeList = attr.ToList();
                        _pool.Free(attr);
                    }
                }

                context.Put(_syntaxFactory.FieldDeclaration(
                    attributeLists: attributeList,
                    modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(),
                    declaration: decl,
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            }
            _pool.Free(varList);
        }

        public override void ExitClassVarList([NotNull] XP.ClassVarListContext context)
        {
            foreach (var cvCtx in context._Var)
                VisitClassvar(cvCtx);
        }

        public override void EnterClassvar([NotNull] XP.ClassvarContext context)
        {
            bool isDim = context.Dim != null;
            bool hasArraySub = context.ArraySub != null;
            bool isFixed = (context.Parent.Parent as XP.ClassvarsContext)?.Modifiers?._FIXED != null;
            if (isDim && !hasArraySub)
            {
                context.AddError(new ParseErrorData(context.DIM(), ErrorCode.ERR_ArrayInitializerExpected));
            }
            if (!isDim && hasArraySub)
            {
                context.ArraySub.AddError(new ParseErrorData(ErrorCode.ERR_FeatureNotAvailableInDialect, "Indexed Class variable", _options.Dialect.ToString()));
            }
            if (!isDim && isFixed)
            {
                context.AddError(new ParseErrorData(context.Id, ErrorCode.ERR_SyntaxError, "DIM"));
            }
        }

        public override void ExitClassvar([NotNull] XP.ClassvarContext context)
        {
            // nvk: Not handled here due to datatype, which is processed later
        }

        protected virtual void VisitClassvar([NotNull] XP.ClassvarContext context)
        {
            bool isDim = context.Dim != null && context.ArraySub != null;
            // make sure we do not initialize Interface and Structure members
            // context.Parent = classvarList
            // classvarList is used in VOGLobal, classvars
            // classvars is used in classmember, classmember is used in interface, class and structure
            var candefault = context.Parent.Parent is XP.VoglobalContext |
                context.Parent.Parent.Parent.isInClass();
            var initExpr = context.Initializer?.Get<ExpressionSyntax>();
            bool isFixed = (context.Parent.Parent as XP.ClassvarsContext)?.Modifiers?._FIXED != null;
            var varType = ((XP.ClassVarListContext)context.Parent).DataType?.Get<TypeSyntax>() ?? _getMissingType();

            if (isDim)
            {
                if (isFixed)
                {
                    if (initExpr != null)
                    {
                        initExpr = initExpr.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_SyntaxError, "AS"));
                        context.Initializer.Put(initExpr);
                        ((IXParseTree)context.ASSIGN_OP()).Put(initExpr);
                        context.Put(GenerateVariable(context.Id.Get<SyntaxToken>(), initExpr));
                    }
                    else
                    {
                        context.Put(GenerateBuffer(context.Id.Get<SyntaxToken>(),
                            MakeBracketedArgumentList(context.ArraySub._ArrayIndex.Select(e => _syntaxFactory.Argument(null, null, e.Get<ExpressionSyntax>())).ToArray())
                            ));
                    }
                    return;
                }
                else
                {
                    var cvl = context.Parent as XP.ClassVarListContext;
                    if (initExpr == null)
                    {
                        var arrayType = _syntaxFactory.ArrayType(varType, context.ArraySub.Get<ArrayRankSpecifierSyntax>());
                        initExpr = GenerateDimArrayInitExpression(arrayType, context.ArraySub );
                    }
                }
            }
            if (context.Initializer != null)
            {
                context.Initializer.SetSequencePoint();
            }
            context.Put(GenerateVariable(context.Id.Get<SyntaxToken>(), initExpr));
        }

        #endregion

        #region Properties
        public override void ExitProperty([NotNull] XP.PropertyContext context)
        {
            if (context.Multi != null)
                context.SetSequencePoint(context.Multi.Start);
            else
                context.SetSequencePoint(context.end);
            var isInInterface = context.isInInterface();
            var isExtern = context.Modifiers?.EXTERN().Count() > 0;
            var isAbstract = context.Modifiers?.ABSTRACT().Count() > 0;
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
                    context.AddError(new ParseErrorData(context.Start, ErrorCode.ERR_InterfaceMemberHasBody));
                }
                if (isExtern)
                {
                    context.AddError(new ParseErrorData(context.Start, ErrorCode.ERR_ExternHasBody, "Property"));
                }
                if (isAbstract)
                {
                    context.AddError(new ParseErrorData(context.Start, ErrorCode.ERR_AbstractHasBody));
                }
            }
            if (isAbstract && context.Modifiers?.EXTERN().Count() > 0)
            {
                context.AddError(new ParseErrorData(context.Modifiers, ErrorCode.ERR_AbstractAndExtern));
            }
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? DefaultMethodModifiers(isInInterface, context.isInStructure());
            if (context.ExplicitIface != null)
            {
                var m = _pool.Allocate();
                foreach (var mod in mods)
                {
                    if (mod.Kind != SyntaxKind.VirtualKeyword && mod.Kind != SyntaxKind.OverrideKeyword && mod.Kind != SyntaxKind.PublicKeyword)
                        m.Add(mod);
                }
                mods = m.ToList<SyntaxToken>();
                _pool.Free(m);
            }
            var type = context.Type?.Get<TypeSyntax>() ?? _getMissingType();
            type.XVoDecl = true;
            var atts = context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>();
            var id = context.Id.Get<SyntaxToken>();
            // check for a property with a just a set accessor and no get accessor
            // when its body is empty then we cannot generate a property so we have to add
            // the get accessor as well
            if (!isInInterface)
            {
                if (context._LineAccessors?.Count == 1)
                {
                    var accessor = context._LineAccessors[0];
                    if (accessor.Key.Type == XSharpLexer.SET && accessor.ExprList == null)
                    {
                        // create a Get Accessor
                        var newaccessor = new XP.PropertyLineAccessorContext(context, 0);
                        newaccessor.CopyFrom(accessor);
                        newaccessor.Key = new XSharpToken(XSharpLexer.GET, "GET");
                        var decl = _syntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration,
                            attributeLists: EmptyList<AttributeListSyntax>(),
                            modifiers: EmptyList(),
                            keyword: SyntaxFactory.Identifier("get"),
                            body: null,
                            expressionBody: null,
                            semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                        decl = decl.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.WRN_GeneratingGetAccessor));
                        newaccessor.Put(decl);
                        context._LineAccessors.Add(newaccessor);
                    }

                }
            }

            var accessorList = _syntaxFactory.AccessorList(SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                        (context.Auto != null) ?
                            ((context._AutoAccessors?.Count ?? 0) > 0) ? MakeList<AccessorDeclarationSyntax>(context._AutoAccessors) :
                            MakeList(_syntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration, EmptyList<AttributeListSyntax>(), EmptyList(),
                                    SyntaxFactory.MakeToken(SyntaxKind.GetKeyword), null, null, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)),
                                _syntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration, EmptyList<AttributeListSyntax>(), EmptyList(),
                                    SyntaxFactory.MakeToken(SyntaxKind.SetKeyword), null, null, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))) :
                        ((context._LineAccessors?.Count ?? 0) > 0) ? MakeList<AccessorDeclarationSyntax>(context._LineAccessors) :
                        MakeList<AccessorDeclarationSyntax>(context._Accessors),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));
            var explicitif = context.ExplicitIface == null ? null : _syntaxFactory.ExplicitInterfaceSpecifier(
                        name: context.ExplicitIface.Get<NameSyntax>(),
                        dotToken: SyntaxFactory.MakeToken(SyntaxKind.DotToken));

            // check the accessor list. if none of the Gets/Sets have a body then generate a warning
            if (context.Auto == null && !isInInterface && !mods.Any((int)SyntaxKind.AbstractKeyword))
            {
                var hasBody = false;
                foreach (var accessor in accessorList.Accessors)
                {
                    if (accessor.Body != null && accessor.Body.Statements.Count > 0)
                    {
                        hasBody = true;
                        break;
                    }
                }
                if (!hasBody)
                {
                    accessorList = accessorList.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.WRN_GetSetMustHaveBody));
                }
            }


            if (context.ParamList == null || context.ParamList._Params.Count == 0)
            {
                MemberDeclarationSyntax propertydecl = _syntaxFactory.PropertyDeclaration(
                     attributeLists: atts,
                     modifiers: mods,
                     type: type,
                     explicitInterfaceSpecifier: explicitif,
                     identifier: id,
                     accessorList: accessorList,
                     expressionBody: null,
                     initializer: context.Initializer != null ? _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                         context.Initializer.Get<ExpressionSyntax>()) : null,
                     semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                propertydecl = CheckForGarbage(propertydecl, context.Ignored, "Name after END PROPERTY");
                context.Put(propertydecl);
            }
            else
            {
                if (context.Auto != null)
                    context.AddError(new ParseErrorData(context.Auto, ErrorCode.ERR_SyntaxError, SyntaxFactory.MakeToken(SyntaxKind.GetKeyword)));
                MemberDeclarationSyntax indexer = _syntaxFactory.IndexerDeclaration(
                    attributeLists: atts,
                    modifiers: mods,
                    type: type,
                    explicitInterfaceSpecifier: explicitif,
                    thisKeyword: SyntaxFactory.MakeToken(SyntaxKind.ThisKeyword, context.Id?.Start.Text ?? ""),
                    parameterList: context.ParamList?.Get<BracketedParameterListSyntax>(),
                    accessorList: accessorList,
                    expressionBody: null, // TODO: (grammar) expressionBody methods
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                indexer = CheckForGarbage(indexer, context.Ignored, "Name after END PROPERTY");
                context.Put(indexer);
            }
        }

        public override void ExitPropertyParameterList([NotNull] XP.PropertyParameterListContext context)
        {
            var @params = _pool.AllocateSeparated<ParameterSyntax>();
            if (context._Params.Count > 0)
            {
                foreach (var paramCtx in context._Params)
                {
                    if (@params.Count > 0)
                        @params.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                    @params.Add(paramCtx.Get<ParameterSyntax>());
                }
                context.Put(_syntaxFactory.BracketedParameterList(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                    @params,
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)));
                _pool.Free(@params);
            }
        }

        public override void ExitPropertyAutoAccessor([NotNull] XP.PropertyAutoAccessorContext context)
        {
            context.Put(_syntaxFactory.AccessorDeclaration(context.Key.AccessorKind(),
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                keyword: context.Key.SyntaxKeyword(),
                body: null,
                expressionBody: null,
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitPropertyLineAccessor([NotNull] XP.PropertyLineAccessorContext context)
        {
            bool forceBody = false;
            var property = context.Parent as XP.PropertyContext;
            var isExtern = property.Modifiers?.EXTERN().Count() > 0;
            var isAbstract = property.Modifiers?.ABSTRACT().Count() > 0;
            if (context.Key.Type == XP.SET)
            {
                if (context.ExprList == null)
                {
                    if (!isExtern && !isAbstract && !property.isInInterface() && property._LineAccessors.Count > 1 &&
                        (property._LineAccessors[0].Expr != null || property._LineAccessors[1].Expr != null))
                    {
                        forceBody = true;
                    }
                }
                else
                {
                    context.ExprList.SetSequencePoint();
                }
            }

            if (context.Key.Type == XP.GET && context.Expr != null)
            {
                context.Expr.SetSequencePoint();
            }
            var decl = _syntaxFactory.AccessorDeclaration(context.Key.AccessorKind(),
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                keyword: context.Key.SyntaxKeyword(),
                body: context.Key.Type == XP.GET ?
                    (context.Expr == null ? null : MakeBlock(
                        MakeList<StatementSyntax>(GenerateReturn(context.Expr.Get<ExpressionSyntax>())
                        )))
                    : (context.ExprList == null && !forceBody) ? null
                    : MakeBlock(context.ExprList?.GetList<StatementSyntax>() ?? EmptyList<StatementSyntax>())
                    ,
                expressionBody: null,
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            context.Put(decl);
        }

        #endregion

        #region Class, Interface and Structure Members
        public virtual ConstructorDeclarationSyntax GenerateDefaultCtor(SyntaxToken id, XP.Class_Context classctx)
        {
            return null;
        }
        protected MemberDeclarationSyntax GenerateClassWrapper(SyntaxToken identifier, MemberDeclarationSyntax member, XP.NameDotContext namedot)
        {
            // This method generates a class wrapper for standalone Methods with a Class Clause
            MemberDeclarationSyntax cls = _syntaxFactory.ClassDeclaration(
                attributeLists: EmptyList<AttributeListSyntax>(),
                modifiers: TokenList(SyntaxKind.PartialKeyword, SyntaxKind.PublicKeyword),
                keyword: SyntaxFactory.MakeToken(SyntaxKind.ClassKeyword),
                identifier: identifier,
                typeParameterList: default(TypeParameterListSyntax),
                baseList: default(BaseListSyntax),
                constraintClauses: default(SyntaxList<TypeParameterConstraintClauseSyntax>),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: MakeList<MemberDeclarationSyntax>(member),
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null);
            if (namedot != null)
            {
                cls = AddNameSpaceToMember(namedot, cls);
            }
            return cls;
        }

        public override void EnterConstructor([NotNull] XP.ConstructorContext context)
        {
            context.Data.MustBeVoid = true;
        }


        protected ConstructorInitializerSyntax createInitializer(IToken chain, XP.ArgumentListContext args)
        {
            if (chain == null)
                return null;

            else
                return _syntaxFactory.ConstructorInitializer(chain.CtorInitializerKind(),
                                            SyntaxFactory.MakeToken(SyntaxKind.ColonToken),
                                            chain.SyntaxKeyword(),
                                            args?.Get<ArgumentListSyntax>() ?? EmptyArgumentList());
        }

        public override void ExitConstructor([NotNull] XP.ConstructorContext context)
        {
            context.SetSequencePoint(context.end);
            if (context.Modifiers?.EXTERN().Count() > 0)
            {
                if (context.StmtBlk?._Stmts?.Count > 0)
                {
                    context.AddError(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_ExternHasBody, "Constructor"));
                }
                context.StmtBlk = null;
            }
            if (context.isInInterface())
            {
                context.AddError(new ParseErrorData(context.CONSTRUCTOR(), ErrorCode.ERR_InterfacesCantContainConstructors));
            }
            else
            {
                var attributes = context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>();
                var parameters = context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList();
                var body = context.StmtBlk?.Get<BlockSyntax>();
                var mods = context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility();
                TypeSyntax returntype = null;
                ImplementClipperAndPSZ(context, ref attributes, ref parameters, ref body, ref returntype);
                // no return statement needed  in CONSTRUCTOR
                // body = AddMissingReturnStatement(body, context.StmtBlk, null);
                SyntaxToken parentId;
                bool missingParent = false;
                bool genClass = false;
                if (context.Parent is XP.ClassmemberContext)
                {
                    parentId = (context.Parent.Parent as XP.Class_Context)?.Id.Get<SyntaxToken>()
                        ?? (context.Parent.Parent as XP.Structure_Context)?.Id.Get<SyntaxToken>()
                        ?? (context.Parent.Parent as XP.Interface_Context)?.Id.Get<SyntaxToken>();
                }
                else if (context.ClassId != null)
                {
                    parentId = context.ClassId.Get<SyntaxToken>();
                    genClass = true;
                }
                else
                {
                    parentId = SyntaxFactory.MakeIdentifier("unknown");
                    missingParent = true;
                }
                var ctor = _syntaxFactory.ConstructorDeclaration(
                    attributeLists: attributes,
                    modifiers: mods,
                    identifier: parentId,
                    parameterList: parameters,
                    initializer: createInitializer(context.Chain, context.ArgList),
                    body: body,
                    expressionBody: null,
                    semicolonToken: (context.StmtBlk?._Stmts?.Count > 0) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                if (missingParent)
                {
                    ctor = ctor.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_ParserError, "Missing CLASS clause"));
                }
                if (!ctor.ContainsDiagnostics && genClass)
                {
                    var cls = GenerateClassWrapper(context.ClassId.Get<SyntaxToken>(), ctor, context.Namespace);
                    context.Put(cls);
                }
                else
                {
                    context.Put(ctor);
                }
            }
        }

        public override void EnterDestructor([NotNull] XP.DestructorContext context)
        {
            context.Data.MustBeVoid = true;
        }
        public override void ExitDestructor([NotNull] XP.DestructorContext context)
        {
            context.SetSequencePoint(context.end);
            if (context.Modifiers?.EXTERN().Count() > 0)
            {
                if (context.StmtBlk?._Stmts?.Count > 0)
                {
                    context.AddError(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_ExternHasBody, "Destructor"));
                }
                context.StmtBlk = null;
            }
            if (context.isInInterface())
            {
                context.AddError(new ParseErrorData(context.DESTRUCTOR(), ErrorCode.ERR_InterfacesCantContainConstructors));
            }
            else
            {
                // no return statement needed in DESTRUCTOR
                // body = AddMissingReturnStatement(body, context.StmtBlk, null);
                SyntaxToken parentId;
                bool missingParent = false;
                bool genClass = false;
                if (context.Parent is XP.ClassmemberContext)
                {
                    parentId = (context.Parent.Parent as XP.Class_Context)?.Id.Get<SyntaxToken>()
                        ?? (context.Parent.Parent as XP.Structure_Context)?.Id.Get<SyntaxToken>()
                        ?? (context.Parent.Parent as XP.Interface_Context)?.Id.Get<SyntaxToken>();
                }
                else if (context.ClassId != null)
                {
                    parentId = context.ClassId.Get<SyntaxToken>();
                    genClass = true;
                }
                else
                {
                    parentId = SyntaxFactory.MakeIdentifier("unknown");
                    missingParent = true;
                }
                var dtor = _syntaxFactory.DestructorDeclaration(
                    attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList<SyntaxToken>(),
                    tildeToken: SyntaxFactory.MakeToken(SyntaxKind.TildeToken),
                    identifier: parentId,
                    parameterList: EmptyParameterList(),
                    body: context.StmtBlk.Get<BlockSyntax>(),
                    expressionBody: null,
                    semicolonToken: (context.StmtBlk != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                if (missingParent)
                {
                    dtor = dtor.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_ParserError, "Missing CLASS clause"));
                }
                if (!dtor.ContainsDiagnostics && genClass)
                {
                    var cls = GenerateClassWrapper(context.ClassId.Get<SyntaxToken>(), dtor, context.Namespace);
                    context.Put(cls);
                }
                else
                {
                    context.Put(dtor);
                }


            }
        }



        public override void EnterPropertyAccessor([NotNull] XP.PropertyAccessorContext context)
        {
            context.Data.MustBeVoid = context.Key.Type == XP.SET;
        }

        public override void ExitPropertyAccessor([NotNull] XP.PropertyAccessorContext context)
        {
            context.SetSequencePoint(context.end);
            var attributes = context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>();
            ParameterListSyntax parameters = null;
            var body = context.StmtBlk.Get<BlockSyntax>();
            TypeSyntax returntype = null;
            ImplementClipperAndPSZ(context, ref attributes, ref parameters, ref body, ref returntype);

            context.Put(_syntaxFactory.AccessorDeclaration(context.Key.AccessorKind(),
                attributeLists: attributes,
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                keyword: context.Key.SyntaxKeyword(),
                body: body,
                expressionBody: null,
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }
        public override void EnterMethod([NotNull] XP.MethodContext context)
        {
            context.Data.MustBeVoid = context.T.Token.Type == XP.ASSIGN;
        }

        protected bool hasAttribute(SyntaxList<AttributeListSyntax> attributes, string attributeName)
        {
            foreach (AttributeListSyntax al in attributes)
            {
                for (int iAttr = 0; iAttr < al.Attributes.Count; iAttr++)
                {
                    var attr = al.Attributes[iAttr];
                    var name = attr.Name.ToFullString().Trim();
                    var pos = name.LastIndexOf(".");
                    if (pos > 0)
                    {
                        name = name.Substring(pos + 1).Trim();
                    }
                    if (String.Equals(name, attributeName, StringComparison.OrdinalIgnoreCase) ||
                        String.Equals(name, attributeName + "Attribute", StringComparison.OrdinalIgnoreCase))
                    {
                        return true;
                    }
                }
            }
            return false;
        }
        protected void removeAttribute(XP.AttributesContext attributes, string attributeName)
        {
            bool found = false;
            foreach (var attrblock in attributes._AttrBlk)
            {
                foreach (var attr in attrblock._Attributes)
                {
                    var name = attr.Name.GetText();
                    var pos = name.LastIndexOf(".");
                    if (pos > 0)
                        name = name.Substring(pos + 1).Trim();
                    if (string.Equals(name, attributeName, StringComparison.OrdinalIgnoreCase) ||
                        string.Equals(name, attributeName + "Attribute", StringComparison.OrdinalIgnoreCase))
                    {
                        attrblock._Attributes.Remove(attr);
                        found = true;
                        break;
                    }
                }
                ExitAttributeBlock(attrblock);
                if (found)
                    break;
            }
            ExitAttributes(attributes);
        }

        public override void ExitMethod([NotNull] XP.MethodContext context)
        {
            var idName = context.Id.Get<SyntaxToken>();
            var isInInterface = context.isInInterface();
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? DefaultMethodModifiers(isInInterface, context.isInStructure(), context.TypeParameters != null);
            var isExtern = mods.Any((int)SyntaxKind.ExternKeyword);
            var isAbstract = mods.Any((int)SyntaxKind.AbstractKeyword);
            var hasNoBody = isInInterface || isExtern || isAbstract;
            context.SetSequencePoint(context.end);
            if (context.T.Token.Type != XP.METHOD)
            {
                // no type parameters on access and assign
                if (context.TypeParameters != null || context._ConstraintsClauses.Count > 0)
                {
                    context.AddError(new ParseErrorData(ErrorCode.Err_TypeParametersAccessAssign));
                }
            }
            if (isInInterface && context.StmtBlk != null && context.StmtBlk._Stmts.Count > 0)
            {
                context.AddError(new ParseErrorData(context.Id, ErrorCode.ERR_InterfaceMemberHasBody));
            }
            if (isInInterface && context.ClassId != null)
            {
                context.AddError(new ParseErrorData(context.ClassId, ErrorCode.ERR_InterfacesCannotContainTypes));
            }
            if (isAbstract)
            {
                if (isExtern)
                {
                    context.AddError(new ParseErrorData(context.Modifiers, ErrorCode.ERR_AbstractAndExtern));
                }
                if (context.StmtBlk?._Stmts?.Count > 0)
                {
                    context.AddError(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_AbstractHasBody));
                }
                context.StmtBlk = null;
            }
            else if (isExtern)
            {
                if (context.StmtBlk?._Stmts?.Count > 0)
                {
                    context.AddError(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_ExternHasBody, "Method"));
                }
                context.StmtBlk = null;
            }
            var attributes = context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>();
            bool hasExtensionAttribute = false;
            if (context.T.Token.Type != XP.METHOD)
            {
                var vomods = _pool.Allocate();
                vomods.Add(SyntaxFactory.MakeToken(SyntaxKind.PrivateKeyword));
                if (mods.Any((int)SyntaxKind.StaticKeyword))
                    vomods.Add(SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword));
                if (mods.Any((int)SyntaxKind.UnsafeKeyword))
                    vomods.Add(SyntaxFactory.MakeToken(SyntaxKind.UnsafeKeyword));
                mods = vomods.ToList<SyntaxToken>();
                _pool.Free(vomods);
            }
            else
            {
                // Check for Extension Attribute. When found then set the self token for the first parameter
                // and generate the parameters again

                if (context.ParamList?._Params.Count > 0)
                {
                    bool hasSelf = context.ParamList?._Params[0].Self != null;
                    if (attributes.Count > 0 && !hasSelf)
                    {
                        if (hasAttribute(attributes, "Extension"))
                        {
                            hasExtensionAttribute = true;
                            var par = context.ParamList._Params[0];
                            par.Self = new XSharpToken(XSharpLexer.SELF, "SELF");
                            hasSelf = true;
                            par.Modifiers._Tokens.Add(par.Self);
                            ExitParameterDeclMods(par.Modifiers);
                            ExitParameter(par);
                            ExitParameterList(context.ParamList);
                            removeAttribute(context.Attributes, "Extension");
                            attributes = context.Attributes.GetList<AttributeListSyntax>();
                        }
                    }

                    if (hasSelf && !mods.Any((int)SyntaxKind.StaticKeyword))
                    {
                        var mTemp = _pool.Allocate();
                        mTemp.AddRange(mods);
                        mTemp.Add(SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword));
                        mods = mTemp.ToList<SyntaxToken>();
                        _pool.Free(mTemp);
                    }
                }

            }
            if (context.ExplicitIface != null)
            {
                var mTemp = _pool.Allocate();
                foreach (var mod in mods)
                {
                    if (mod.Kind != SyntaxKind.VirtualKeyword &&
                        mod.Kind != SyntaxKind.OverrideKeyword &&
                        mod.Kind != SyntaxKind.PublicKeyword)
                        mTemp.Add(mod);
                }
                mods = mTemp.ToList<SyntaxToken>();
                _pool.Free(mTemp);
            }
            if (!isExtern)
            {
                isExtern = hasDllImport(attributes);
                hasNoBody = hasNoBody || isExtern;
            }
            if (isExtern && !mods.Any((int)SyntaxKind.ExternKeyword))
            {
                // Add Extern Keyword to modifiers
                var m1 = _pool.Allocate();
                m1.AddRange(mods);
                if (!m1.Any((int)SyntaxKind.ExternKeyword))
                    m1.Add(SyntaxFactory.MakeToken(SyntaxKind.ExternKeyword));
                mods = m1.ToList<SyntaxToken>();
                _pool.Free(m1);
            }
            var parameters = context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList();
            if (isExtern)
            {
                parameters = UpdateVODLLParameters(parameters);
            }
            var body = hasNoBody ? null : context.StmtBlk.Get<BlockSyntax>();
            var returntype = context.Type?.Get<TypeSyntax>();
            if (returntype == null)
            {
                if (context.T.Token.Type == XP.ASSIGN)
                {
                    returntype = VoidType();
                }
                else  // method and access
                {
                    returntype = _getMissingType();
                    returntype.XNode = context;
                }
            }
            else
            {
                returntype.XVoDecl = true;
            }
            var oldbody = body;
            ImplementClipperAndPSZ(context, ref attributes, ref parameters, ref body, ref returntype);
            if (body != oldbody)
            {
                context.StmtBlk.Put(body);
            }
            if (context.T.Token.Type == XP.ASSIGN)
            {
                // Assign does not need a return. 
                // So do not add missing returns
                returntype = VoidType();
            }
            else if (context.StmtBlk != null && !hasNoBody)
            {
                body = AddMissingReturnStatement(body, context.StmtBlk, returntype);
            }
            MemberDeclarationSyntax m = _syntaxFactory.MethodDeclaration(
                attributeLists: attributes,
                modifiers: mods,
                returnType: returntype,
                explicitInterfaceSpecifier: context.ExplicitIface == null ? null : _syntaxFactory.ExplicitInterfaceSpecifier(
                    name: context.ExplicitIface.Get<NameSyntax>(),
                    dotToken: SyntaxFactory.MakeToken(SyntaxKind.DotToken)),
                identifier: idName,
                typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                parameterList: parameters,
                constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                body: body,
                expressionBody: null, // TODO: (grammar) expressionBody methods
                semicolonToken: (!hasNoBody && context.StmtBlk != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            if (hasExtensionAttribute)
            {
                m = m.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_ExplicitExtension));
            }

            if (context.ClassId != null)
            {
                bool GenClass = true;
                if (context.isInClass())
                {
                    string parentName;
                    XP.Class_Context parent = null;
                    if (context.Parent is XP.Class_Context)
                    {
                        parent = context.Parent as XP.Class_Context;
                    }
                    else if (context.Parent.Parent is XP.Class_Context)
                    {
                        parent = context.Parent.Parent as XP.Class_Context;
                    }
                    if (parent != null)
                    {
                        parentName = parent.Id.GetText();
                        if (parent.Namespace != null)
                            parentName = parent.Namespace.GetText() + parentName;
                        string className;
                        className = context.ClassId.GetText();
                        if (context.Namespace != null)
                            className = context.Namespace.GetText() + className;
                        if (String.Compare(parentName, className, StringComparison.OrdinalIgnoreCase) != 0)
                        {
                            m = m.WithAdditionalDiagnostics(
                            new SyntaxDiagnosticInfo(
                                    ErrorCode.ERR_NestedMethodMustHaveSameNameAsParentClass, className, parentName));
                        }
                        else
                            GenClass = false;


                    }
                }

                if (!m.ContainsDiagnostics && GenClass)
                {
                    m = GenerateClassWrapper(context.ClassId.Get<SyntaxToken>(), m, context.Namespace);
                }
            }
            context.Put(m);
            if (context.T.Token.Type != XP.METHOD)
            {
                if (context.Data.HasClipperCallingConvention && context.CallingConvention != null)
                {
                    m = m.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(
                                    ErrorCode.ERR_NoClipperCallingConventionForAccessAssign));
                }
                context.Put(m);
                ClassEntities.Peek().AddVoPropertyAccessor(context);
            }
        }

        public override void ExitMethodtype([NotNull] XP.MethodtypeContext context)
        {
            // nvk: Handled by the method rule
        }

        public override void ExitOperator_([NotNull] XP.Operator_Context context)
        {
            context.SetSequencePoint(context.end);
            if (context.Modifiers?.EXTERN().Count() > 0)
            {
                if (context.StmtBlk?._Stmts?.Count > 0)
                {
                    context.AddError(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_ExternHasBody, "Operator"));
                }
                context.StmtBlk = null;
            }
            if (context.Conversion != null)
                context.Put(_syntaxFactory.ConversionOperatorDeclaration(
                    attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(false, SyntaxKind.StaticKeyword),
                    implicitOrExplicitKeyword: context.Conversion.Get<SyntaxToken>(),
                    operatorKeyword: SyntaxFactory.MakeToken(SyntaxKind.OperatorKeyword),
                    type: context.Type?.Get<TypeSyntax>() ?? MissingType(),
                    parameterList: context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
                    body: context.StmtBlk.Get<BlockSyntax>(),
                    expressionBody: null, // TODO: (grammar) expressionBody methods
                    semicolonToken: (context.StmtBlk != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            else
            {
                SyntaxToken opToken;
                if (context.Operation.Token.Type == XP.GT && context.Gt != null) // right shift
                {
                    opToken = GetRShiftToken(context.Operation.Token, context.Gt);
                }
                else
                {
                    opToken = context.Operation.Get<SyntaxToken>();
                }
                // differentiate between unary and primary operators
                // overloadedops only handles binary operators
                // So for unary operator remap the token here.

                if (context.ParamList?._Params.Count == 1)
                {
                    opToken = context.Operation.Token.SyntaxPrefixOp();
                }
                context.Put(_syntaxFactory.OperatorDeclaration(
                    attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(false, SyntaxKind.StaticKeyword),
                    returnType: context.Type?.Get<TypeSyntax>() ?? MissingType(),
                    operatorKeyword: SyntaxFactory.MakeToken(SyntaxKind.OperatorKeyword),
                    operatorToken: opToken,
                    parameterList: context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
                    body: context.StmtBlk.Get<BlockSyntax>(),
                    expressionBody: null, // TODO: (grammar) expressionBody methods
                    semicolonToken: (context.StmtBlk != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            }
        }


        public override void ExitOverloadedOps([NotNull] XP.OverloadedOpsContext context)
        {
            switch (context.Token.Type)
            {
                case XP.OR:
                    // Please note that C# does not have an operator ||
                    context.Put(SyntaxFactory.MakeToken(SyntaxKind.BarToken));
                    break;
                case XP.AND:
                    // Please note that C# does not have an operator &&
                    context.Put(SyntaxFactory.MakeToken(SyntaxKind.AmpersandToken));
                    break;
                default:
                    context.Put(context.Token.SyntaxOp());
                    break;
            }
        }

        public override void ExitConversionOps([NotNull] XP.ConversionOpsContext context)
        {
            context.Put(context.Token.SyntaxKeyword());
        }

        #endregion

        #region Generics
        public override void ExitGenericArgumentList([NotNull] XP.GenericArgumentListContext context)
        {
            var types = _pool.AllocateSeparated<TypeSyntax>();
            foreach (var type in context._GenericArgs)
            {
                if (types.Count != 0)
                    types.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                types.Add(type.Get<TypeSyntax>());
            }
            context.Put(_syntaxFactory.TypeArgumentList(
                SyntaxFactory.MakeToken(SyntaxKind.LessThanToken),
                types.ToList(),
                SyntaxFactory.MakeToken(SyntaxKind.GreaterThanToken)
                ));
            _pool.Free(types);
        }

        public override void ExitTypeparameters([NotNull] XP.TypeparametersContext context)
        {
            var parameters = _pool.AllocateSeparated<TypeParameterSyntax>();
            foreach (var tpCtx in context._TypeParams)
            {
                if (parameters.Count > 0)
                    parameters.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                parameters.Add(tpCtx.Get<TypeParameterSyntax>());
            }
            context.Put(_syntaxFactory.TypeParameterList(SyntaxFactory.MakeToken(SyntaxKind.LessThanToken),
                parameters,
                SyntaxFactory.MakeToken(SyntaxKind.GreaterThanToken)));
            _pool.Free(parameters);
        }

        public override void ExitTypeparameter([NotNull] XP.TypeparameterContext context)
        {
            context.Put(_syntaxFactory.TypeParameter(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                varianceKeyword: context.VarianceKeyword?.SyntaxKeyword(),
                identifier: context.Id.Get<SyntaxToken>()));
        }

        public override void ExitTypeparameterconstraintsclause([NotNull] XP.TypeparameterconstraintsclauseContext context)
        {
            var constraints = _pool.AllocateSeparated<TypeParameterConstraintSyntax>();
            foreach (var cCtx in context._Constraints)
            {
                if (constraints.Count > 0)
                    constraints.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                constraints.Add(cCtx.Get<TypeParameterConstraintSyntax>());
            }
            context.Put(_syntaxFactory.TypeParameterConstraintClause(
                SyntaxFactory.MakeToken(SyntaxKind.WhereKeyword),
                context.Name.Get<IdentifierNameSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.ColonToken),
                constraints));
            _pool.Free(constraints);
        }

        public override void ExitTypeConstraint([NotNull] XP.TypeConstraintContext context)
        {
            context.Put(_syntaxFactory.TypeConstraint(context.Type?.Get<TypeSyntax>() ?? MissingType()));
        }

        public override void ExitClassOrStructConstraint([NotNull] XP.ClassOrStructConstraintContext context)
        {
            context.Put(_syntaxFactory.ClassOrStructConstraint(
                context.Key.ConstraintKind(),
                context.Key.SyntaxKeyword()));
        }

        public override void ExitConstructorConstraint([NotNull] XP.ConstructorConstraintContext context)
        {
            context.Put(_syntaxFactory.ConstructorConstraint(
                SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)));
        }

        #endregion

        #region ClassMember alternatives
        public override void ExitClsctor([NotNull] XP.ClsctorContext context)
        {
            context.SetSequencePoint(context.Member.end);
            if (context.isInInterface())
            {
                context.AddError(new ParseErrorData(context.Member, ErrorCode.ERR_InterfacesCantContainConstructors));
            }
            else
                context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }
        public override void ExitClsdtor([NotNull] XP.ClsdtorContext context)
        {
            context.SetSequencePoint(context.Member.end);
            context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }
        public override void ExitClsmethod([NotNull] XP.ClsmethodContext context)
        {
            context.SetSequencePoint(context.Member.end);
            context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void ExitClsoperator([NotNull] XP.ClsoperatorContext context)
        {
            context.SetSequencePoint(context.Member.end);
            if (context.isInInterface())
            {
                context.AddError(new ParseErrorData(context.Member, ErrorCode.ERR_InterfacesCantContainOperators));
            }
            else
                context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void ExitClsproperty([NotNull] XP.ClspropertyContext context)
        {
            context.SetSequencePoint(context.Member.end);
            context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void ExitClsvars([NotNull] XP.ClsvarsContext context)
        {
            if (context.isInInterface())
            {
                context.AddError(new ParseErrorData(context.Member, ErrorCode.ERR_InterfacesCantContainFields));
            }
            else if (context.Member.CsNode != null)
                context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }


        public override void ExitNestedClass([NotNull] XP.NestedClassContext context)
        {
            if (context.isInInterface())
            {
                context.AddError(new ParseErrorData(context.Member, ErrorCode.ERR_InterfacesCannotContainTypes));
            }
            else
                context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void ExitNestedDelegate([NotNull] XP.NestedDelegateContext context)
        {
            if (context.isInInterface())
            {
                context.AddError(new ParseErrorData(context.Member, ErrorCode.ERR_InterfacesCannotContainTypes));
            }
            else
                context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void ExitNestedEnum([NotNull] XP.NestedEnumContext context)
        {
            if (context.isInInterface())
            {
                context.AddError(new ParseErrorData(context.Member, ErrorCode.ERR_InterfacesCannotContainTypes));
            }
            else
                context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void ExitNestedEvent([NotNull] XP.NestedEventContext context)
        {
            context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void ExitNestedInterface([NotNull] XP.NestedInterfaceContext context)
        {
            if (context.isInInterface())
            {
                context.AddError(new ParseErrorData(context.Member, ErrorCode.ERR_InterfacesCannotContainTypes));
            }
            else
                context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void ExitNestedStructure([NotNull] XP.NestedStructureContext context)
        {
            if (context.isInInterface())
            {
                context.AddError(new ParseErrorData(context.Member, ErrorCode.ERR_InterfacesCannotContainTypes));
            }
            else
                context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        #endregion

        #region Attributes

        protected AttributeListSyntax MakeAttributeList(AttributeTargetSpecifierSyntax target, SeparatedSyntaxList<AttributeSyntax> attributes)
        {
            return _syntaxFactory.AttributeList(
                        openBracketToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                        target: target,
                        attributes: attributes,
                        closeBracketToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)
                        );
        }
        protected AttributeArgumentListSyntax MakeAttributeArgumentList(SeparatedSyntaxList<AttributeArgumentSyntax> args)
        {
            return _syntaxFactory.AttributeArgumentList(
                        SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                        args,
                        SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)
                        );
        }

        internal ExpressionSyntax MakeConditional(ExpressionSyntax condition, ExpressionSyntax left, ExpressionSyntax right)
        {
            return _syntaxFactory.ConditionalExpression(
                                       condition,
                                       SyntaxFactory.MakeToken(SyntaxKind.QuestionToken),
                                       left,
                                       SyntaxFactory.MakeToken(SyntaxKind.ColonToken),
                                       right);
        }

        internal SyntaxList<AttributeListSyntax> MakeCompilerGeneratedAttribute(bool lWithGlobalScope = false)
        {
            SyntaxListBuilder<AttributeListSyntax> attributeLists = _pool.Allocate<AttributeListSyntax>();
            GenerateAttributeList(attributeLists, SystemQualifiedNames.CompilerGenerated);
            //if (lWithGlobalScope)
            //{
            //    GenerateAttributeList(attributeLists, SystemQualifiedNames.CompilerGlobalScope);
            //}
            var compilerGenerated = attributeLists.ToList();
            _pool.Free(attributeLists);
            return compilerGenerated;
        }


        public override void ExitAttributes([NotNull] XP.AttributesContext context)
        {
            var attributeLists = _pool.Allocate<AttributeListSyntax>();
            foreach (var attrBlkCtx in context._AttrBlk)
            {
                attributeLists.Add(attrBlkCtx.Get<AttributeListSyntax>());
            }
            context.PutList(attributeLists.ToList());
            _pool.Free(attributeLists);
        }

        public override void ExitAttributeBlock([NotNull] XP.AttributeBlockContext context)
        {
            var attributes = _pool.AllocateSeparated<AttributeSyntax>();
            foreach (var attrCtx in context._Attributes)
            {
                if (attributes.Count > 0)
                {
                    attributes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                }
                attributes.Add(attrCtx.Get<AttributeSyntax>());
            }
            context.Put(MakeAttributeList(
                context.Target?.Get<AttributeTargetSpecifierSyntax>(),
                attributes));
            _pool.Free(attributes);
        }

        public override void ExitAttributeTarget([NotNull] XP.AttributeTargetContext context)
        {
            context.Put(_syntaxFactory.AttributeTargetSpecifier(
                context.Id?.Get<SyntaxToken>() ?? context.Kw.Get<SyntaxToken>(),
                SyntaxFactory.MakeToken(SyntaxKind.ColonToken)));
        }

        public override void ExitAttribute([NotNull] XP.AttributeContext context)
        {
            var arguments = _pool.AllocateSeparated<AttributeArgumentSyntax>();
            if (context._Params != null)
            {
                foreach (var paramCtx in context._Params)
                {
                    if (arguments.Count != 0)
                    {
                        arguments.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                    }
                    arguments.Add(paramCtx.Get<AttributeArgumentSyntax>());
                }
            }
            context.Put(_syntaxFactory.Attribute(
                name: context.Name.Get<NameSyntax>(),
                argumentList: MakeAttributeArgumentList(arguments)));
            _pool.Free(arguments);
        }

        public override void ExitPropertyAttributeParam([NotNull] XP.PropertyAttributeParamContext context)
        {
            context.Put(_syntaxFactory.AttributeArgument(
                _syntaxFactory.NameEquals(context.Name.Get<IdentifierNameSyntax>(),
                    SyntaxFactory.MakeToken(SyntaxKind.EqualsToken)),
                null, // TODO: (grammar) name: attr arg syntax?
                context.Expr.Get<ExpressionSyntax>()));
        }

        public override void ExitExprAttributeParam([NotNull] XP.ExprAttributeParamContext context)
        {
            context.Put(_syntaxFactory.AttributeArgument(null, null, context.Expr.Get<ExpressionSyntax>()));
        }

        public override void ExitGlobalAttributes([NotNull] XP.GlobalAttributesContext context)
        {
            var attributes = _pool.AllocateSeparated<AttributeSyntax>();
            // process wild cards in the AssemblyVersionAttribute, AssemblyFileVersionAttribute and AssemblyInformationalVersionAttribute
            // [assembly: AssemblyVersionAttribute("1.0.*")]
            // [assembly: AssemblyFileVersionAttribute("1.0.*")]
            // [assembly: AssemblyInformationalVersionAttribute("1.0.*")]
            foreach (var attrCtx in context._Attributes)
            {
                if (attributes.Count > 0)
                {
                    attributes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                }
                if (context.Target.Token.Type == XP.ASSEMBLY)
                {
                    string[] names = {  "AssemblyVersionAttribute","AssemblyVersion",
                                        "AssemblyFileVersionAttribute","AssemblyFileVersion",
                                        "AssemblyInformationalVersionAttribute","AssemblyInformationalVersion"};

                    string sSRef = "System.Reflection.";
                    foreach (var name in names)
                    {
                        string ctxName = attrCtx.Name.GetText();
                        if (String.Equals(ctxName, name, StringComparison.OrdinalIgnoreCase) ||
                            String.Equals(ctxName, sSRef + name, StringComparison.OrdinalIgnoreCase))
                        {
                            // check to see if the attribute has a wild card
                            if (attrCtx._Params.Count == 1 && attrCtx._Params[0].Start.Type == XP.STRING_CONST)
                            {
                                string version = attrCtx._Params[0].Start.Text;
                                if (version.StartsWith("\"") && version.EndsWith("\""))
                                    version = version.Substring(1, version.Length - 2);
                                System.Version vers;
                                if (VersionHelper.TryParseAssemblyVersion(version, allowWildcard: true, version: out vers))
                                {
                                    var arguments = _pool.AllocateSeparated<AttributeArgumentSyntax>();
                                    var newarg = _syntaxFactory.AttributeArgument(null, null, GenerateLiteral(vers.ToString()));
                                    arguments.Add(newarg);

                                    attrCtx.Put(_syntaxFactory.Attribute(attrCtx.Get<AttributeSyntax>().Name,
                                        MakeAttributeArgumentList(arguments)));
                                    _pool.Free(arguments);
                                }

                            }
                            break;
                        }

                    }
                }
                attributes.Add(attrCtx.Get<AttributeSyntax>());
            }
            context.Put(MakeAttributeList(
                context.Target.Get<AttributeTargetSpecifierSyntax>(),
                attributes));
            _pool.Free(attributes);
        }

        public override void ExitGlobalAttributeTarget([NotNull] XP.GlobalAttributeTargetContext context)
        {
            context.Put(_syntaxFactory.AttributeTargetSpecifier(
                context.Token.SyntaxKeywordIdentifier(),
                SyntaxFactory.MakeToken(SyntaxKind.ColonToken)));
        }

        #endregion  

        #region VO Compatible GLobal types and fields


        public override void ExitVodefine([NotNull] XP.VodefineContext context)
        {
            var variables = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            var expr = context.Expr.Get<ExpressionSyntax>();
            variables.Add(GenerateVariable(context.Id.Get<SyntaxToken>(), MakeChecked(expr,false)));
            var isConst = false;
            // always process the expression to determine if it is a const
            var type = GetExpressionType(context.Expr, ref isConst);
            if (context.DataType != null)
            {
                type = context.DataType?.Get<TypeSyntax>();
                isConst = false;
            }
            var list = _pool.Allocate();
            SyntaxList<SyntaxToken> modifiers = null;
            if (context.Modifiers != null)
            {
                // We are not including the "STATIC" modifier here. If there is any diagnostics attached
                // to this modifier, we will attach that to the Const Keyword that we are adding
                // This may happen if we have STATIC STATIC DEFINE Foo := 123 AS LONG
                DiagnosticInfo[] diags = null;
                foreach (var m in context.Modifiers.GetList<SyntaxToken>())
                {
                    if (m.Kind != SyntaxKind.StaticKeyword)
                        list.Add(m);
                    else if (m.ContainsDiagnostics)
                        diags = m.GetDiagnostics();
                }
                if (isConst)
                {
                    var token = SyntaxFactory.MakeToken(SyntaxKind.ConstKeyword);
                    if (diags != null)
                        token = token.WithAdditionalDiagnostics(diags);
                    list.Add(token);
                }
                else
                {
                    var token = SyntaxFactory.MakeToken(SyntaxKind.ReadOnlyKeyword);
                    if (diags != null)
                        token = token.WithAdditionalDiagnostics(diags);
                    list.Add(token);
                    token = SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword);
                    list.Add(token);
                }
                modifiers = list.ToList<SyntaxToken>();
                _pool.Free(list);
            }
            else
            {
                // Will be a public const
                if (isConst)
                {
                    modifiers = TokenList(SyntaxKind.PublicKeyword, SyntaxKind.ConstKeyword);
                }
                else
                {
                    // public readonly static.
                    // if we are lucky we may change this to a const inside SourceMemberSymbol.cs later if the 
                    // initializer expression gets folded to a const
                    // such as in
                    // DEFINE Foo := 0
                    // DEFINE Bar := Foo +1
                    modifiers = TokenList(SyntaxKind.PublicKeyword, SyntaxKind.ReadOnlyKeyword, SyntaxKind.StaticKeyword);
                }
            }
            var field = _syntaxFactory.FieldDeclaration(
                EmptyList<AttributeListSyntax>(),
                modifiers,
                _syntaxFactory.VariableDeclaration(type, variables),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            context.Put(field);
            _pool.Free(variables);
            GlobalEntities.Globals.Add(field);
        }


        public override void EnterVoglobal([NotNull] XP.VoglobalContext context)
        {
            if (context.Const != null)
            {
                if (context.Modifiers != null)
                    context.Modifiers._Tokens.Add(context.Const);
                else
                {
                    context.Modifiers = FixPosition(new XP.FuncprocModifiersContext(context, 0), context.Start);
                    context.Modifiers.PutList(TokenList(SyntaxKind.ConstKeyword, SyntaxKind.PublicKeyword));
                }
            }
        }

        public override void ExitVoglobal([NotNull] XP.VoglobalContext context)
        {
            var varList = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            var varType = context.Vars.DataType?.Get<TypeSyntax>() ?? _getMissingType();
            context.SetSequencePoint(context.end);

            varType.XVoDecl = true;
            if (context.Vars?.As?.Type == XP.IS)
            {
                varType.XVoIsDecl = true;
            }
            foreach (var varCtx in context.Vars._Var)
            {
                bool isDim = varCtx.Dim != null && varCtx.ArraySub != null;
                if (isDim)
                {
                    var global = _syntaxFactory.FieldDeclaration(
                        attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                        modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(false, SyntaxKind.StaticKeyword),
                        declaration: _syntaxFactory.VariableDeclaration(
                            type: _syntaxFactory.ArrayType(varType, MakeArrayRankSpecifier(varCtx.ArraySub._ArrayIndex.Count)),
                            variables: MakeSeparatedList(varCtx.Get<VariableDeclaratorSyntax>())),
                        semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                    context.Put(global);
                    GlobalEntities.Globals.Add(global);
                }
                else
                {
                    if (varList.Count > 0)
                        varList.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                    varList.Add(varCtx.Get<VariableDeclaratorSyntax>());
                }
            }
            if (varList.Count > 0)
            {
                var global = _syntaxFactory.FieldDeclaration(
                    attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(false, SyntaxKind.StaticKeyword),
                    declaration: _syntaxFactory.VariableDeclaration(
                        type: varType,
                        variables: varList),
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                context.Put(global);
                GlobalEntities.Globals.Add(global);
            }
            _pool.Free(varList);
        }


        public override void EnterVodll([NotNull] XP.VodllContext context)
        {
            // No need to set the MustBeVoid flag. _DLL PROCEDURE  has no statement list
            //context.Data.MustBeVoid = context.T.Type == XP.PROCEDURE;
            if (context.Modifiers != null)
            {
                context.Modifiers._Tokens.Add(_parser.TokenFactory.Create(XP.EXTERN, ""));
            }
        }


        internal virtual ParameterListSyntax UpdateVODLLParameters(ParameterListSyntax parameters)
        {
            // real work implemented in the subclass to check for PSZ parameters
            return parameters;
        }

        private AttributeSyntax _unmanagedCodeAttribute()
        {
            var attrargs = _pool.AllocateSeparated<AttributeArgumentSyntax>();
            var attrib = _syntaxFactory.Attribute(
                                name: GenerateQualifiedName("System.Security.SuppressUnmanagedCodeSecurityAttribute"),
                                argumentList: MakeAttributeArgumentList(attrargs));
            _pool.Free(attrargs);
            return attrib;

        }
        private AttributeSyntax _dllImportAttribute(XP.VodllContext context, ExpressionSyntax dllExpr, ExpressionSyntax entrypointExpr)
        {
            AttributeArgumentSyntax charset;
            SyntaxToken id;
            if (context.CharSet != null)
            {
                id = SyntaxFactory.Identifier(context.CharSet.Text);
            }
            else
            {
                id = SyntaxFactory.Identifier("Auto");
            }
            charset = _syntaxFactory.AttributeArgument(GenerateNameEquals("Charset"), null,
                             MakeSimpleMemberAccess(GenerateQualifiedName(SystemQualifiedNames.CharSet),
                                  _syntaxFactory.IdentifierName(id)));
            var attribs = new List<AttributeArgumentSyntax>() { _syntaxFactory.AttributeArgument(null, null, dllExpr), charset };
            if (entrypointExpr != null)
                attribs.Add(_syntaxFactory.AttributeArgument(GenerateNameEquals("EntryPoint"), null, entrypointExpr));
            attribs.Add(_syntaxFactory.AttributeArgument(GenerateNameEquals("SetLastError"), null, GenerateLiteral(true)));
            attribs.Add(_syntaxFactory.AttributeArgument(GenerateNameEquals("ExactSpelling"), null, GenerateLiteral(true)));
            if (context.CallingConvention != null)
                attribs.Add(context.CallingConvention.Get<AttributeArgumentSyntax>());
            return _syntaxFactory.Attribute(
                name: GenerateQualifiedName(SystemQualifiedNames.DllImport),
                argumentList: MakeAttributeArgumentList(MakeSeparatedList(attribs.ToArray())));

        }
        public override void ExitVodll([NotNull] XP.VodllContext context)
        {
            // The Parser Rule has attributes but these are ignored for now.
            string dllName = context.Dll.GetText();
            if (context.Extension != null)
            {
                dllName += "." + context.Extension.GetText();
            }
            ExpressionSyntax dllExpr = GenerateLiteral(dllName);
            ExpressionSyntax entrypointExpr;
            if (context.Ordinal != null)
            {
                entrypointExpr = GenerateLiteral(context.Ordinal.Text).WithAdditionalDiagnostics(
                    new SyntaxDiagnosticInfo(ErrorCode.ERR_InvalidDLLEntryPoint, "A numeric entrypoint (" + context.Ordinal.Text.Substring(1) + ") is not supported in .Net"));
            }
            else
            {
                string entrypoint = context.Entrypoint.GetText();
                entrypointExpr = GenerateLiteral(entrypoint);

                if (context.Address != null || context.Number != null)
                {
                    if (context.Address == null || context.Number == null)
                    {
                        entrypointExpr = entrypointExpr.WithAdditionalDiagnostics(
                            new SyntaxDiagnosticInfo(ErrorCode.ERR_InvalidDLLEntryPoint, "Both the @ sign and the number must be specified"));
                    }
                    else if (context.Address.StartIndex > context.Entrypoint.stop.StopIndex + 1
                        || context.Number.StartIndex > context.Address.StopIndex + 1)
                    {
                        entrypointExpr = entrypointExpr.WithAdditionalDiagnostics(
                            new SyntaxDiagnosticInfo(ErrorCode.ERR_InvalidDLLEntryPoint, "No spaces allowed in entrypoint name"));
                    }
                    else
                    {
                        // the whole string from entrypointExpr - @int is the entrypoint
                        entrypoint = entrypoint + context.Address.Text + context.Number.Text;
                        entrypointExpr = GenerateLiteral(entrypoint);
                    }
                }
            }

            var returnType = context.Type?.Get<TypeSyntax>() ?? (context.T.Type == XP.FUNCTION ? _getMissingType() : VoidType());
            returnType.XVoDecl = true;

            var parameters = context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList();
            parameters = UpdateVODLLParameters(parameters);
            var modifiers = context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(false, SyntaxKind.StaticKeyword, SyntaxKind.ExternKeyword);
            var attributes = MakeSeparatedList(_unmanagedCodeAttribute(),
                                                _dllImportAttribute(context, dllExpr, entrypointExpr)
                                              );
            var attList = MakeList(MakeAttributeList(target: null, attributes: attributes));

            context.Put(_syntaxFactory.MethodDeclaration(
                attributeLists: attList,
                modifiers: modifiers,
                returnType: returnType,
                explicitInterfaceSpecifier: null,
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: null,
                parameterList: parameters,
                constraintClauses: default(SyntaxList<TypeParameterConstraintClauseSyntax>),
                body: null,
                expressionBody: null,
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitDllcallconv([NotNull] XP.DllcallconvContext context)
        {
            string conv = null;
            switch (context.Cc.Type)
            {
                case XP.CLIPPER:
                case XP.STRICT:
                case XP.ASPEN:
                    conv = SystemQualifiedNames.Cdecl;
                    break;
                case XP.PASCAL:
                case XP.WINCALL:
                case XP.CALLBACK:
                    conv = null; // set no attribute at all.
                    break;
                case XP.THISCALL:
                    conv = SystemQualifiedNames.ThisCall; ;
                    break;
                case XP.FASTCALL:
                    conv = SystemQualifiedNames.Cdecl;
                    break;
            }
            if (!String.IsNullOrEmpty(conv))
            {
                context.Put(_syntaxFactory.AttributeArgument(
                    GenerateNameEquals("CallingConvention"),
                    null,
                    GenerateQualifiedName(conv)));
            }
        }

        public override void ExitVostruct([NotNull] XP.VostructContext context)
        {
            MemberDeclarationSyntax m = _syntaxFactory.StructDeclaration(
                attributeLists: EmptyList<AttributeListSyntax>(),
                modifiers: null,
                keyword: SyntaxFactory.MakeToken(SyntaxKind.StructKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: null,
                baseList: null,
                constraintClauses: null,
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: EmptyList<MemberDeclarationSyntax>(),
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null);

            context.Put(NotInDialect(m, "VOSTRUCT"));
        }

        public override void ExitVounion([NotNull] XP.VounionContext context)
        {
            MemberDeclarationSyntax m = _syntaxFactory.StructDeclaration(
                attributeLists: EmptyList<AttributeListSyntax>(),
                modifiers: null,
                keyword: SyntaxFactory.MakeToken(SyntaxKind.StructKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: null,
                baseList: null,
                constraintClauses: null,
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: EmptyList<MemberDeclarationSyntax>(),
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null);

            context.Put(NotInDialect(m, "UNION"));
        }

        protected bool IsStringType(TypeSyntax type)
        {
            if (type is PredefinedTypeSyntax)
            {
                var pretype = type as PredefinedTypeSyntax;
                switch (pretype.keyword.Kind)
                {
                    case SyntaxKind.StringKeyword:
                        return true;
                    default:
                        return false;
                }
            }
            else
            {
                if (type is QualifiedNameSyntax)
                {
                    var qns = type as QualifiedNameSyntax;
                    // System.Void
                    var sName = qns.ToFullString().Replace(" ", "");
                    if (sName.Equals("System.String", StringComparison.OrdinalIgnoreCase))
                    {
                        return true;
                    }
                    // global::System.Void
                    if (sName.Equals("global::System.String", StringComparison.OrdinalIgnoreCase))
                    {
                        return true;
                    }
                }
            }
            return false;
        }
        protected bool isVoidType(TypeSyntax type)
        {
            if (type is PredefinedTypeSyntax)
            {
                var pretype = type as PredefinedTypeSyntax;
                switch (pretype.keyword.Kind)
                {
                    case SyntaxKind.VoidKeyword:
                        return true;
                    default:
                        return false;
                }
            }
            else
            {
                if (type is QualifiedNameSyntax)
                {
                    var qns = type as QualifiedNameSyntax;
                    // System.Void
                    var sName = qns.ToFullString().Replace(" ", "");
                    var v1 = GenerateQualifiedName(SystemQualifiedNames.Void1).ToFullString().Replace(" ", "");
                    var v2 = GenerateQualifiedName(SystemQualifiedNames.Void2).ToFullString().Replace(" ", "");
                    if (sName.Equals(v1, StringComparison.OrdinalIgnoreCase))
                    {
                        return true;
                    }
                    // global::System.Void
                    if (sName.Equals(v2, StringComparison.OrdinalIgnoreCase))
                    {
                        return true;
                    }
                }

            }
            return false;
        }

        protected virtual BlockSyntax GenerateEntryPoint(SyntaxList<SyntaxToken> modifiers, [NotNull] XP.IEntityContext context, BlockSyntax body,
                    SyntaxList<AttributeListSyntax> attributeList, ParameterListSyntax parList)
        {
            // In the core dialect GenerateEntryPoint does nothing special
            // In other dialects the body of the start function will be changed
            // and an additional function may be generated
            return body;
        }


        bool hasDllImport(SyntaxList<AttributeListSyntax> attributes)
        {
            if (attributes != null && attributes.Count > 0)
            {
                foreach (AttributeListSyntax al in attributes)
                {
                    var ats = al.Attributes;
                    for (int i = 0; i < ats.Count; i++)
                    {
                        var at = ats[i] as AttributeSyntax;
                        if (at.ToString().ToLower().Contains("dllimport"))
                        {
                            return true;
                        }
                    }

                }
            }
            return false;
        }

        public override void ExitFunction([NotNull] XP.FunctionContext context)
        {
            context.SetSequencePoint(context.end);
            var isInInterface = context.isInInterface();
            if (isInInterface && context.StmtBlk != null && context.StmtBlk._Stmts.Count > 0)
            {
                context.AddError(new ParseErrorData(context.Id, ErrorCode.ERR_InterfaceMemberHasBody));
            }
            var attributes = context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>();
            bool isextern = hasDllImport(attributes);
            var modifiers = GetFuncProcModifiers(context.Modifiers, isextern, isInInterface);
            if (!isextern)
            {
                isextern = modifiers.Any((int)SyntaxKind.ExternKeyword);
            }
            var hasnobody = (isInInterface || isextern);
            var parameters = context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList();
            if (isextern)
            {
                parameters = UpdateVODLLParameters(parameters);
            }
            var body = hasnobody ? null : context.StmtBlk.Get<BlockSyntax>();
            var returntype = context.Type?.Get<TypeSyntax>() ?? _getMissingType();
            returntype.XVoDecl = true;
            var id = context.Id.Get<SyntaxToken>();
            if (!hasnobody)
            {
                ImplementClipperAndPSZ(context, ref attributes, ref parameters, ref body, ref returntype);
                body = AddMissingReturnStatement(body, context.StmtBlk, returntype);
                // Special Handling of EntryPoint
                if (string.Equals(id.Text, _entryPoint, StringComparison.OrdinalIgnoreCase)
                    && _options.CommandLineArguments.CompilationOptions.OutputKind.IsApplication())
                {
                    body = GenerateEntryPoint(modifiers, context, body, attributes, parameters);
                }
            }
            context.Put(_syntaxFactory.MethodDeclaration(
                attributeLists: attributes,
                modifiers: modifiers,
                returnType: returntype,
                explicitInterfaceSpecifier: null,
                identifier: id,
                typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                parameterList: parameters,
                constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                body: body,
                expressionBody: null,
                semicolonToken: (body != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));

        }
        public override void EnterProcedure([NotNull] XP.ProcedureContext context)
        {
            context.Data.MustBeVoid = true;
        }
        public override void ExitProcedure([NotNull] XP.ProcedureContext context)
        {
            var isInInterface = context.isInInterface();
            context.SetSequencePoint(context.end);
            if (isInInterface && context.StmtBlk != null && context.StmtBlk._Stmts.Count > 0)
            {
                context.AddError(new ParseErrorData(context.Id, ErrorCode.ERR_InterfaceMemberHasBody));
            }
            if (context.InitExit != null)
            {
                if (!_options.HasRuntime)
                {
                    context.AddError(new ParseErrorData(context.Id, ErrorCode.ERR_FeatureNotAvailableInDialect, "Init/Exit procedure", _options.Dialect.ToString()));
                }
                else
                {
                    if (context.ParamList?._Params.Count > 0)
                    {
                        context.AddError(new ParseErrorData(context.Id, ErrorCode.ERR_InitProceduresCannotDefineParameters));
                    }
                    else
                    {
                        int level = 0;
                        switch (context.InitExit.Type)
                        {
                            case XP.INIT1:
                                level = 1;
                                break;
                            case XP.INIT2:
                                level = 2;
                                break;
                            case XP.INIT3:
                                level = 3;
                                break;
                            case XP.EXIT:
                                level = -1;
                                break;
                        }
                        GlobalEntities.InitProcedures.Add(new Tuple<int, string>(level, context.Id.GetText()));
                        context.Data.IsInitProcedure = true;
                        context.Data.HasClipperCallingConvention = false;
                    }
                }
            }
            var attributes = context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>();
            bool isextern = hasDllImport(attributes);
            var parameters = context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList();
            if (isextern)
            {
                parameters = UpdateVODLLParameters(parameters);
            }
            var returntype = VoidType();
            var modifiers = GetFuncProcModifiers(context.Modifiers, isextern, isInInterface);
            if (!isextern)
            {
                isextern = modifiers.Any((int)SyntaxKind.ExternKeyword);
            }
            var hasnobody = isextern && isInInterface;
            var body = hasnobody ? null : context.StmtBlk.Get<BlockSyntax>();
            var id = context.Id.Get<SyntaxToken>();
            if (!hasnobody)
            {
                ImplementClipperAndPSZ(context, ref attributes, ref parameters, ref body, ref returntype);
                // no return statement needed in PROCEDURE
                // body = AddMissingReturnStatement(body, context.StmtBlk, null);
                // Special Handling of EntryPoint
                if (string.Equals(id.Text, _entryPoint, StringComparison.OrdinalIgnoreCase)
                    && _options.CommandLineArguments.CompilationOptions.OutputKind.IsApplication())
                {
                    // In the core dialect GenerateEntryPoint does nothing special
                    // In other dialects the body of the start function will be changed
                    // and an additional function may be generated
                    body = GenerateEntryPoint(modifiers, context, body, attributes, parameters);
                }
            }
            context.Put(_syntaxFactory.MethodDeclaration(
                attributeLists: attributes,
                modifiers: modifiers,
                returnType: returntype,
                explicitInterfaceSpecifier: null,
                identifier: id,
                typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                parameterList: parameters,
                constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                body: body,
                expressionBody: null,
                semicolonToken: (body != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitCallingconvention([NotNull] XP.CallingconventionContext context)
        {
            // TODO nvk (calling convention is silently ignored for now)
        }

        #endregion

        #region Parameters
        private static ParameterListSyntax _emptyParameterList = null;
        protected ParameterListSyntax EmptyParameterList()
        {
            if (_emptyParameterList == null)
            {
                lock (gate)
                {
                    if (_emptyParameterList == null)
                    {

                        _emptyParameterList = _syntaxFactory.ParameterList(
                            SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                            EmptySeparatedList<ParameterSyntax>(),
                            SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
                    }
                }

            }
            return _emptyParameterList;
        }


        public override void EnterParameterList([NotNull] XP.ParameterListContext context)
        {
            foreach (var par in context._Params)
            {
                if (par.Ellipsis != null)
                {
                    if (par != context._Params.Last())
                    {
                        par.AddError(new ParseErrorData(ErrorCode.ERR_VarargsLast, par));
                    }
                    //var parent = context.Parent;
                    //if (!(parent is XP.VodllContext))
                    //{
                    //    parent.AddError(new ParseErrorData(ErrorCode.ERR_VarargNotVODLL, parent));
                    //}
                    //else
                    //{
                    //    var callConv = ((XP.VodllContext)parent).CallingConvention;
                    //    if (callConv == null || callConv.Cc.Type != XP.STRICT)
                    //    {
                    //        parent.AddError(new ParseErrorData(ErrorCode.ERR_VarargNotVODLL, parent));
                    //    }
                    //}

                }
            }
        }
        public override void ExitParameterList([NotNull] XP.ParameterListContext context)
        {
            var @params = _pool.AllocateSeparated<ParameterSyntax>();
            foreach (var paramCtx in context._Params)
            {
                if (@params.Count > 0)
                    @params.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                @params.Add(paramCtx.Get<ParameterSyntax>());
            }
            context.Put(_syntaxFactory.ParameterList(
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                @params,
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)));
            _pool.Free(@params);
        }

        public override void EnterParameter([NotNull] XP.ParameterContext context)
        {
            if (context.Self != null)
            {
                context.Modifiers._Tokens.Add(context.Self);
            }
        }

        protected virtual TypeSyntax _getParameterType([NotNull] XP.ParameterContext context)
        {
            TypeSyntax type = context.Type?.Get<TypeSyntax>();
            if (type == null)
            {
                type = _getMissingType();
            }
            return type;
        }

        public override void ExitParameter([NotNull] XP.ParameterContext context)
        {
            bool hasParamArrayAttribute = false;
            var attributeList = context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>();

            if (hasAttribute(attributeList, "ParamArray"))
            {
                hasParamArrayAttribute = true;
                removeAttribute(context.Attributes, "ParamArray");
                attributeList = context.Attributes.GetList<AttributeListSyntax>();
                foreach (XSharpToken t in context.Modifiers._Tokens)
                {
                    // replace any AS, IS, REF or OUT with PARAMS. Leave CONST
                    if (t.Type != XP.CONST)
                    {
                        t.Type = XP.PARAMS;
                        t.Text = "PARAMS";
                    }
                }
                ExitParameterDeclMods(context.Modifiers);
            }

            if (context.Ellipsis != null)
            {
                context.Put(_syntaxFactory.Parameter(
                    EmptyList<AttributeListSyntax>(), EmptyList(), null, context.Ellipsis.SyntaxLiteralValue(_options), null));
                return;
            }
            TypeSyntax type = _getParameterType(context);
            type.XVoDecl = true;
            if (context.Modifiers != null && context.Modifiers._Tokens != null)
            {
                foreach (var token in context.Modifiers._Tokens)
                {
                    if (token.Type == XP.IS)
                    {
                        type.XVoIsDecl = true;
                        break;
                    }
                }
            }

            var par = _syntaxFactory.Parameter(
                attributeLists: attributeList,
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                type: type,
                identifier: context.Id.Get<SyntaxToken>(),
                @default: context.Default == null ? null : _syntaxFactory.EqualsValueClause(
                    SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                    context.Default.Get<ExpressionSyntax>()));

            if (hasParamArrayAttribute)
            {
                par = par.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_ExplicitParamArray));
            }
            context.Put(par);
        }

        #endregion

        #region Modifiers

        protected void HandleDefaultTypeModifiers(XSharpParserRuleContext context, IList<IToken> tokens, bool fixDefault = false)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            AddUniqueModifiers(modifiers, tokens, fixDefault, false);
            context.PutList(modifiers.ToList<SyntaxToken>());
            _pool.Free(modifiers);
        }

        private void AddUniqueModifiers(SyntaxListBuilder modifiers, IList<IToken> tokens, bool fixDefault, bool isInInterface)
        {
            foreach (var m in tokens)
            {
                var kw = m.SyntaxKeyword();
                if (isInInterface)
                {
                    if (kw.Kind != SyntaxKind.PublicKeyword)
                        modifiers.AddCheckUnique(kw);
                }
                else
                    modifiers.AddCheckUnique(kw);
            }
            if (fixDefault)
            {
                modifiers.FixDefaultVisibility();
            }
        }

        public override void ExitClassModifiers([NotNull] XP.ClassModifiersContext context)
        {
            HandleDefaultTypeModifiers(context, context._Tokens, true);
        }
        public override void ExitClassvarModifiers([NotNull] XP.ClassvarModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
                if (m.Type == XP.FIXED && context._FIXED == null)
                    context._FIXED = m;
            }
            modifiers.FixDefaultVisibility();
            context.PutList(modifiers.ToList<SyntaxToken>());
            _pool.Free(modifiers);
        }

        public override void ExitConstructorModifiers([NotNull] XP.ConstructorModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            AddUniqueModifiers(modifiers, context._Tokens, false, false);
            if (!modifiers.Any((int)SyntaxKind.StaticKeyword))
                modifiers.FixDefaultVisibility();
            context.PutList(modifiers.ToList<SyntaxToken>());
            _pool.Free(modifiers);
        }

        public override void ExitDelegateModifiers([NotNull] XP.DelegateModifiersContext context)
        {
            HandleDefaultTypeModifiers(context, context._Tokens, true);
        }
        public override void ExitEnumModifiers([NotNull] XP.EnumModifiersContext context)
        {
            HandleDefaultTypeModifiers(context, context._Tokens, true);
        }

        public override void ExitEventModifiers([NotNull] XP.EventModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            bool isInInterface = context.Parent.isInInterface();
            AddUniqueModifiers(modifiers, context._Tokens, false, isInInterface);
            if (!isInInterface)
            {
                modifiers.FixDefaultVisibility();
                if (_options.VirtualInstanceMethods && !context.Parent.isInStructure())
                    modifiers.FixDefaultVirtual();
                else
                    modifiers.FixDefaultMethod();
            }
            context.PutList(modifiers.ToList<SyntaxToken>());
            _pool.Free(modifiers);
        }


        public override void ExitDestructorModifiers([NotNull] XP.DestructorModifiersContext context)
        {
            HandleDefaultTypeModifiers(context, context._Tokens, true);
        }

        public override void ExitInterfaceModifiers([NotNull] XP.InterfaceModifiersContext context)
        {
            HandleDefaultTypeModifiers(context, context._Tokens, true);
        }

        public override void ExitAccessorModifiers([NotNull] XP.AccessorModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            bool isInInterface = context.Parent.isInInterface();
            AddUniqueModifiers(modifiers, context._Tokens, false, isInInterface);
            context.PutList(modifiers.ToList<SyntaxToken>());
            _pool.Free(modifiers);
        }

        public override void ExitMemberModifiers([NotNull] XP.MemberModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            bool genericParent = false;
            // Membermodifiers are used for Methods and Properties
            if (context.Parent is XP.MethodContext mc)
            {
                genericParent = mc.TypeParameters != null || mc._ConstraintsClauses.Count > 0;
            }
            bool isInInterface = context.Parent.isInInterface();
            AddUniqueModifiers(modifiers, context._Tokens, false, isInInterface);
            if (!isInInterface)
            {
                modifiers.FixDefaultVisibility();
                if (_options.VirtualInstanceMethods && !context.Parent.isInStructure())
                    modifiers.FixDefaultVirtual();
                else if (!genericParent)
                {
                    modifiers.FixDefaultMethod();
                }
            }
            context.PutList(modifiers.ToList<SyntaxToken>());
            _pool.Free(modifiers);
        }

        public override void ExitOperatorModifiers([NotNull] XP.OperatorModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            AddUniqueModifiers(modifiers, context._Tokens, false, false);
            if (!modifiers.Any((int)SyntaxKind.StaticKeyword))
                modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword));
            modifiers.FixDefaultVisibility();
            context.PutList(modifiers.ToList<SyntaxToken>());
            _pool.Free(modifiers);
        }

        public override void ExitParameterDeclMods([NotNull] XP.ParameterDeclModsContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                if (m.Type != XP.AS && m.Type != XP.IS)
                    modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            //modifiers.FixDefaultVisibility();
            context.PutList(modifiers.ToList<SyntaxToken>());
            _pool.Free(modifiers);
        }

        public override void ExitFuncprocModifiers([NotNull] XP.FuncprocModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            AddUniqueModifiers(modifiers, context._Tokens, false, false);
            // STATIC FUNCTION is implemented as INTERNAL and will be moved to a special class
            if (modifiers.Any((int)SyntaxKind.StaticKeyword))
            {
                // in this context static has to do with visibility
                modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.InternalKeyword));
                context.IsStaticVisible = true;
            }
            else
            {
                // in this context static means that .Net static = class method as opposed to instance method
                context.IsStaticVisible = false;
                modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword));
                modifiers.FixDefaultVisibility();
            }
            context.PutList(modifiers.ToList<SyntaxToken>());
            _pool.Free(modifiers);
        }


        public override void ExitStructureModifiers([NotNull] XP.StructureModifiersContext context)
        {
            HandleDefaultTypeModifiers(context, context._Tokens, true);
        }

        public override void ExitVotypeModifiers([NotNull] XP.VotypeModifiersContext context)
        {
            IList<IToken> tokens = context._Tokens;
            if (tokens != null)
            {
                foreach (XSharpToken t in tokens)
                {
                    if (t.Type == XSharpLexer.STATIC)
                    {
                        t.Type = XSharpLexer.INTERNAL;
                        t.Text = "INTERNAL";
                    }
                }
            }

            HandleDefaultTypeModifiers(context, tokens, true);
        }

        SyntaxList<SyntaxToken> GetFuncProcModifiers(XP.FuncprocModifiersContext context, bool isExtern, bool isInInterface)
        {
            if (isExtern)
                return context?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(isInInterface, SyntaxKind.StaticKeyword, SyntaxKind.ExternKeyword);
            else
                return context?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(isInInterface, SyntaxKind.StaticKeyword);

        }

        #endregion
        #endregion
        #region Statements

        #region Declaration Statements

        public override void ExitDeclarationStmt([NotNull] XP.DeclarationStmtContext context)
        {
            context.PutList(context.Decl.GetList<StatementSyntax>());
        }

        public override void EnterCommonLocalDecl([NotNull] XP.CommonLocalDeclContext context)
        {
            XP.DatatypeContext t = null;
            for (var i = context._LocalVars.Count - 1; i >= 0; i--)
            {
                var locCtx = context._LocalVars[i];
                if (locCtx.DataType != null)
                    t = locCtx.DataType;
                else if (t != null)
                    locCtx.DataType = t;
            }
        }

        public override void ExitCommonLocalDecl([NotNull] XP.CommonLocalDeclContext context)
        {
            context.SetSequencePoint();
            foreach (var lvCtx in context._LocalVars)
                VisitLocalvar(lvCtx);
            context.PutList(MakeList<StatementSyntax>(context._LocalVars));
        }

        public override void ExitVarLocalDecl([NotNull] XP.VarLocalDeclContext context)
        {
            context.SetSequencePoint();
            context.PutList(MakeList<StatementSyntax>(context._ImpliedVars));
        }

        public override void EnterLocalvar([NotNull] XP.LocalvarContext context)
        {
            bool isDim = context.Dim != null;
            bool hasArraySub = context.ArraySub != null;
            context.SetSequencePoint();
            if (isDim && !hasArraySub)
            {
                context.AddError(new ParseErrorData(context.DIM(), ErrorCode.ERR_ArrayInitializerExpected));
            }
            if (!isDim && hasArraySub)
            {
                context.ArraySub.AddError(new ParseErrorData(ErrorCode.ERR_FeatureNotAvailableInDialect, "Indexed Local", _options.Dialect.ToString()));
            }
        }

        public override void ExitLocalvar([NotNull] XP.LocalvarContext context)
        {
            // nvk: Do nothing here. It will be handled by the visitor after Datatype(s) are processed.
            context.SetSequencePoint();
        }

        protected virtual TypeSyntax _getMissingType()
        {
            return MissingType();
        }

        protected ExpressionSyntax GenerateDimArrayInitExpression(ArrayTypeSyntax arrayType, XP.ArraysubContext sub)
        {
            InitializerExpressionSyntax init = null;
            var dims = _syntaxFactory.ArrayCreationExpression(SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),arrayType, null);
            bool isstring = false;
            if (_options.VONullStrings && arrayType.ElementType is PredefinedTypeSyntax pdt)
            {
                isstring = pdt.Keyword.Kind == SyntaxKind.StringKeyword;

            }
            if (!isstring)
                return dims;
            if (sub._ArrayIndex.Count == 1)
            {
                // for single dim indexes we inline the initialization
                var l = _pool.AllocateSeparated<ExpressionSyntax>();
                int dimensions = Int32.Parse(sub._ArrayIndex[0].GetText());
                for (int i = 0; i < dimensions; i++)
                {
                    if (i > 0)
                    {
                        l.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                    }
                    l.Add(GenerateLiteral(""));
                }
                init = _syntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression,
                    SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    l.ToList(),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));
                _pool.Free(l);
                return _syntaxFactory.ArrayCreationExpression(SyntaxFactory.MakeToken(SyntaxKind.NewKeyword), arrayType, init);
            }
            // multi dim is not supported yet in X# runtime.
            if (_options.XSharpRuntime)
            {
                sub.AddError(new ParseErrorData(ErrorCode.ERR_ParserError, "DIM string arrays must have one dimension"));
                return null;
            }
            // Vulcan has StringArrayInit, but that does not return a string, so we have to generate a special function for this
            // this function creates the array, initializes it and returns it.
            // StringArrayInit has no return type because it can handle arrays of different dimensions
            // 
            var funcname = "$StringArrayInit" + UniqueNameSuffix;
            var stmts = _pool.Allocate<StatementSyntax>();
            StatementSyntax stmt = GenerateLocalDecl(XSharpSpecialNames.ArrayName, arrayType, dims);
            stmt.XNode = sub.Parent as XSharpParserRuleContext;
            stmt.XGenerated = true;
            stmts.Add(stmt);

            var varname = GenerateSimpleName(XSharpSpecialNames.ArrayName);
            var args = MakeArgumentList(MakeArgument(varname));
            stmt = GenerateExpressionStatement(GenerateMethodCall(VulcanQualifiedFunctionNames.StringArrayInit, args, true));
            stmt.XNode = sub.Parent as XSharpParserRuleContext;
            stmts.Add(stmt);

            stmt = GenerateReturn(varname,true);
            stmt.XNode = sub.Parent as XSharpParserRuleContext;
            stmts.Add(stmt);
            var body = MakeBlock(stmts);
            _pool.Free(stmts);
            var attributes = MakeCompilerGeneratedAttribute();
            var modifiers = MakeList<SyntaxToken>(SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword),
                                                    SyntaxFactory.MakeToken(SyntaxKind.InternalKeyword));
            var funcdecl = _syntaxFactory.MethodDeclaration(
                    attributeLists: attributes,
                    modifiers: modifiers,
                    returnType: arrayType,
                    explicitInterfaceSpecifier: null,
                    identifier: SyntaxFactory.MakeIdentifier(funcname),
                    typeParameterList: null,
                    parameterList: EmptyParameterList(),
                    constraintClauses: null,
                    body: body,
                    expressionBody: null,
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            funcdecl.XNode = sub.Parent as XSharpParserRuleContext;
            GlobalClassEntities.Members.Add(funcdecl);
            var expr = GenerateMethodCall(funcname);
            return expr;
        }
    

        protected virtual void VisitLocalvar([NotNull] XP.LocalvarContext context)
        {
            bool isConst = context.Const != null;
            bool isStatic = (context.Parent as XP.CommonLocalDeclContext).Static != null;
            bool isDim = context.Dim != null && context.ArraySub != null;
            string staticName = null;
            TypeSyntax varType;
            if (context.DataType != null)
                varType = context.DataType?.Get<TypeSyntax>();
            else
            {
                varType = _getMissingType();
            }
            var initExpr = context.Expression?.Get<ExpressionSyntax>();
            varType.XVoDecl = true;
            if (context.As?.Type == XP.IS)
            { 
                varType.XVoIsDecl = true;
            }
            if (isDim )
            {
                if (CurrentEntity != null)
                { 
                    CurrentEntity.Data.HasDimVar = true;
                }
                varType.XVoIsDim = true;
            }
            if (initExpr == null && isDim)
            {
                var arrayType = _syntaxFactory.ArrayType(varType, context.ArraySub.Get<ArrayRankSpecifierSyntax>());            // with dimensions      string[10,10]
                varType = _syntaxFactory.ArrayType(varType, MakeArrayRankSpecifier(context.ArraySub._ArrayIndex.Count));        // without dimensions   string[,]
                initExpr = GenerateDimArrayInitExpression(arrayType, context.ArraySub );
            }
            if (isStatic)
            {
                staticName = XSharpSpecialNames.StaticLocalFieldNamePrefix + context.Id.Get<SyntaxToken>().Text + UniqueNameSuffix;
                ClassEntities.Peek().Members.Add(
                    _syntaxFactory.FieldDeclaration(
                        EmptyList<AttributeListSyntax>(),
                        TokenList(SyntaxKind.StaticKeyword, SyntaxKind.InternalKeyword),
                        _syntaxFactory.VariableDeclaration(varType,
                            MakeSeparatedList(GenerateVariable(SyntaxFactory.Identifier(staticName)))),
                        SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))
                    );
                if (initExpr != null)
                {
                    ClassEntities.Peek().Members.Add(
                        _syntaxFactory.FieldDeclaration(
                            EmptyList<AttributeListSyntax>(),
                            TokenList(SyntaxKind.StaticKeyword, SyntaxKind.InternalKeyword),
                            _syntaxFactory.VariableDeclaration(_syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.BoolKeyword)),
                                MakeSeparatedList(
                                        GenerateVariable(SyntaxFactory.Identifier(staticName + XSharpSpecialNames.StaticLocalInitFieldNameSuffix),
                                        GenerateLiteral(true)))),
                            SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))
                        );
                    ClassEntities.Peek().Members.Add(
                        _syntaxFactory.FieldDeclaration(
                            EmptyList<AttributeListSyntax>(),
                            TokenList(SyntaxKind.StaticKeyword, SyntaxKind.InternalKeyword),
                            _syntaxFactory.VariableDeclaration(_objectType,
                                MakeSeparatedList(GenerateVariable(SyntaxFactory.Identifier(staticName + XSharpSpecialNames.StaticLocalLockFieldNameSuffix),
                                        CreateObject(_objectType, EmptyArgumentList())))),
                            SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))
                        );
                }
            }
            var variables = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            var vardecl = _syntaxFactory.VariableDeclarator(context.Id.Get<SyntaxToken>(), null,
                isStatic ? _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                    _syntaxFactory.RefExpression(SyntaxFactory.MakeToken(SyntaxKind.RefKeyword), GenerateSimpleName(staticName)))
                : (initExpr == null) ? null : _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken), initExpr));
            vardecl.XVoIsDim = isDim;
            var name = context.Id.GetText();
            if (CurrentEntity.Data.GetField(name) != null)
            {
                vardecl = vardecl.WithAdditionalDiagnostics( new SyntaxDiagnosticInfo(ErrorCode.ERR_MemvarFieldWithSameName, name));
            }
            variables.Add(vardecl);
            var modifiers = _pool.Allocate();
            if (isConst)
                modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.ConstKeyword));
                
            if (!isStatic)
            {
                var ldecl = _syntaxFactory.LocalDeclarationStatement(
                    modifiers.ToList<SyntaxToken>(),
                    _syntaxFactory.VariableDeclaration(varType, variables),
                    SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                ldecl.XVoIsDim = isDim;
                context.Put(ldecl);
            }
            else
            {
                var decl = _pool.Allocate<StatementSyntax>();
                var ldecl = _syntaxFactory.LocalDeclarationStatement(
                    modifiers.ToList<SyntaxToken>(),
                    _syntaxFactory.VariableDeclaration(
                        _syntaxFactory.RefType(SyntaxFactory.MakeToken(SyntaxKind.RefKeyword), null, varType), variables),
                    SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                decl.Add(ldecl);
                if (initExpr != null)
                {
                    decl.Add(GenerateIfStatement(
                        GenerateSimpleName(staticName + XSharpSpecialNames.StaticLocalInitFieldNameSuffix),

                        MakeLock(GenerateSimpleName(staticName + XSharpSpecialNames.StaticLocalLockFieldNameSuffix),
                            GenerateIfStatement(GenerateSimpleName(staticName + XSharpSpecialNames.StaticLocalInitFieldNameSuffix),

                                MakeBlock(MakeList<StatementSyntax>(
                                        GenerateExpressionStatement(MakeSimpleAssignment(GenerateSimpleName(staticName), initExpr)),
                                        GenerateExpressionStatement(
                                            MakeSimpleAssignment(
                                                GenerateSimpleName(staticName + XSharpSpecialNames.StaticLocalInitFieldNameSuffix),
                                                GenerateLiteral(false)))
                                        ))))));
                }
                context.PutList<StatementSyntax>(decl);
                _pool.Free(decl);
            }
            _pool.Free(variables);
            _pool.Free(modifiers);
        }

        public override void ExitImpliedvar([NotNull] XP.ImpliedvarContext context)
        {
            bool isConst = context.Const != null;
            bool isStatic = (context.Parent as XP.VarLocalDeclContext).Static != null;
            context.SetSequencePoint();
            var variables = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            var name = context.Id.GetText();
            if (CurrentEntity.Data.GetField(name) != null)
            {
                context.AddError(new ParseErrorData(ErrorCode.ERR_MemvarFieldWithSameName, name));
            }
            var variable = _syntaxFactory.VariableDeclarator(context.Id.Get<SyntaxToken>(), null,
                (context.Expression == null) ? null :
                _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken), context.Expression.Get<ExpressionSyntax>()));
            if (context.Op.Type != XP.ASSIGN_OP)
                variable = variable.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.WRN_AssignmentOperatorExpected));
            variables.Add(variable);
            var modifiers = _pool.Allocate();
            if (isConst)
                context.AddError(new ParseErrorData(ErrorCode.ERR_ImplicitlyTypedVariableCannotBeConst));
            if (isStatic)
                context.AddError(new ParseErrorData(ErrorCode.ERR_ImplicitlyTypedVariableCannotBeStatic));
            context.Put(_syntaxFactory.LocalDeclarationStatement(
                modifiers.ToList<SyntaxToken>(),
                _syntaxFactory.VariableDeclaration(_impliedType, variables),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            _pool.Free(variables);
            _pool.Free(modifiers);
        }

        public override void ExitXbasedeclStmt([NotNull] XP.XbasedeclStmtContext context)
        {
            context.Put(context.xbasedecl().Get<StatementSyntax>());
        }

        public override void ExitXbasedecl([NotNull] XP.XbasedeclContext context)
        {
            context.SetSequencePoint(context.end);
            context.Put(NotInDialect(context.T.Text + " statement"));
        }

 
        public override void ExitVariableDeclaration([NotNull] XP.VariableDeclarationContext context)
        {
            context.Put(_syntaxFactory.VariableDeclaration(
                context.Type?.Get<TypeSyntax>() ?? (context.Var != null ? _impliedType : MissingType()),
                MakeSeparatedList<VariableDeclaratorSyntax>(context._Decl)
                ));
        }

        public override void ExitVariableDeclarator([NotNull] XP.VariableDeclaratorContext context)
        {
            var decl = _syntaxFactory.VariableDeclarator(
                context.Id.Get<SyntaxToken>(),
                null,
                _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken), context.Expr.Get<ExpressionSyntax>()));
            if (context.Op.Type != XP.ASSIGN_OP)
            {
                decl = decl.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.WRN_AssignmentOperatorExpected));
            }
            context.Put(decl);
        }

        public override void ExitFieldStmt([NotNull] XP.FieldStmtContext context)
        {
            context.Put(context.Decl.Get<StatementSyntax>());
        }

        #endregion

        #region Loop Statements
        public override void ExitWhileStmt([NotNull] XP.WhileStmtContext context)
        {
            context.SetSequencePoint(context.Expr);
            StatementSyntax whileStmt = _syntaxFactory.WhileStatement(SyntaxFactory.MakeToken(SyntaxKind.WhileKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                context.StmtBlk.Get<BlockSyntax>());
            whileStmt = CheckForGarbage(whileStmt, context.Ignored, "Expression after END [DO]");
            context.Put(whileStmt);
        }

        public override void ExitRepeatStmt([NotNull] XP.RepeatStmtContext context)
        {
            context.SetSequencePoint(context.end);
            context.Expr.SetSequencePoint();
            var doStmt = _syntaxFactory.DoStatement(SyntaxFactory.MakeToken(SyntaxKind.DoKeyword),
                context.StmtBlk.Get<BlockSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.WhileKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                _syntaxFactory.PrefixUnaryExpression(SyntaxKind.LogicalNotExpression, SyntaxFactory.MakeToken(SyntaxKind.ExclamationToken),
                    context.Expr.Get<ExpressionSyntax>()),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            context.Put(doStmt);
        }

        public override void ExitForStmt([NotNull] XP.ForStmtContext context)
        {
            ExpressionSyntax assignExpr, whileExpr, incrExpr, iterExpr, initExpr;
            if (context.AssignExpr != null)
            {
                var assign = context.AssignExpr as XP.AssignmentExpressionContext;
                if (assign == null)
                {
                    context.Put(_syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                    context.AddError(new ParseErrorData(context.Dir, ErrorCode.ERR_SyntaxError, ":="));
                    return;
                }
                if (assign.Op.Type != XP.ASSIGN_OP)
                {
                    context.Put(_syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                    context.AddError(new ParseErrorData(assign.Op, ErrorCode.ERR_SyntaxError, ":="));
                    return;
                }
                context.AssignExpr.SetSequencePoint();
                iterExpr = assign.Left.Get<ExpressionSyntax>();
                initExpr = assign.Right.Get<ExpressionSyntax>();
                assignExpr = assign.Get<ExpressionSyntax>();
                iterExpr.XNode = assign;
                initExpr.XNode = assign;
            }
            else
            {
                iterExpr = _syntaxFactory.IdentifierName(context.ForIter.Get<SyntaxToken>());
                iterExpr.XNode = context.Expr;
                initExpr = context.Expr.Get<ExpressionSyntax>();
                assignExpr = MakeSimpleAssignment(iterExpr, initExpr);
                assignExpr.XNode = context.Expr;
                initExpr.XNode = context.Expr;
                context.Expr.SetSequencePoint();
            }
            if (context.Step == null)
            {
                context.Step = FixPosition(new XP.PrimaryExpressionContext(FixPosition(new XP.ExpressionContext(), context.Stop)), context.Stop);
                context.Step.Put(GenerateLiteral("1", 1));
            }
            else
            {
                context.Step.SetSequencePoint();
            }
            switch (context.Dir.Type)
            {
                case XP.DOWNTO:
                    whileExpr = _syntaxFactory.BinaryExpression(SyntaxKind.GreaterThanOrEqualExpression,
                        iterExpr,
                        SyntaxFactory.MakeToken(SyntaxKind.GreaterThanEqualsToken),
                        context.FinalExpr.Get<ExpressionSyntax>());
                    incrExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.SubtractAssignmentExpression,
                        iterExpr,
                        SyntaxFactory.MakeToken(SyntaxKind.MinusEqualsToken),
                        context.Step.Get<ExpressionSyntax>());
                    whileExpr.XNode = context.FinalExpr;
                    incrExpr.XNode = context.Step;
                    context.FinalExpr.SetSequencePoint();
                    context.Step.SetSequencePoint();
                    break;
                case XP.UPTO:
                    whileExpr = _syntaxFactory.BinaryExpression(SyntaxKind.LessThanOrEqualExpression,
                            iterExpr,
                            SyntaxFactory.MakeToken(SyntaxKind.LessThanEqualsToken),
                            context.FinalExpr.Get<ExpressionSyntax>());

                    incrExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.AddAssignmentExpression,
                                iterExpr,
                                SyntaxFactory.MakeToken(SyntaxKind.PlusEqualsToken),
                                context.Step.Get<ExpressionSyntax>());
                    whileExpr.XNode = context.FinalExpr;
                    incrExpr.XNode = context.Step;
                    context.FinalExpr.SetSequencePoint();
                    context.Step.SetSequencePoint();
                    break;
                case XP.TO:
                default:
                    // 
                    var compExpr = _syntaxFactory.BinaryExpression(SyntaxKind.GreaterThanExpression,
                        context.Step.Get<ExpressionSyntax>(), 
                        SyntaxFactory.MakeToken(SyntaxKind.GreaterThanToken),
                        GenerateLiteral(0));
                    var ltExpr = _syntaxFactory.BinaryExpression(SyntaxKind.LessThanOrEqualExpression,
                            iterExpr,
                            SyntaxFactory.MakeToken(SyntaxKind.LessThanEqualsToken),
                            context.FinalExpr.Get<ExpressionSyntax>());
                    var gtExpr = _syntaxFactory.BinaryExpression(SyntaxKind.GreaterThanOrEqualExpression,
                            iterExpr,
                            SyntaxFactory.MakeToken(SyntaxKind.GreaterThanEqualsToken),
                            context.FinalExpr.Get<ExpressionSyntax>());
                    whileExpr = MakeConditional(compExpr,ltExpr,gtExpr);

                    incrExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.AddAssignmentExpression,
                                iterExpr,
                                SyntaxFactory.MakeToken(SyntaxKind.PlusEqualsToken),
                                context.Step.Get<ExpressionSyntax>());
                    whileExpr.XNode = context.FinalExpr;
                    incrExpr.XNode = context.Step;
                    context.FinalExpr.SetSequencePoint();
                    context.Step.SetSequencePoint();
                    break;
            }
            var decl = default(VariableDeclarationSyntax);
            var init = _pool.AllocateSeparated<ExpressionSyntax>();
            if (context.ForDecl != null)
            {
                decl = _syntaxFactory.VariableDeclaration(
                    context.Type?.Get<TypeSyntax>() ?? _impliedType,
                    MakeSeparatedList(GenerateVariable(
                        context.ForIter.Get<SyntaxToken>(), initExpr)));
                decl.XNode = context.ForIter;
                context.ForIter.SetSequencePoint();
            }
            else
            {
                init.Add(assignExpr);
            }
            var incr = _pool.AllocateSeparated<ExpressionSyntax>();
            incr.Add(incrExpr);
            StatementSyntax forStmt = _syntaxFactory.ForStatement(SyntaxFactory.MakeToken(SyntaxKind.ForKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                decl,
                init,
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken),
                whileExpr,
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken),
                incr,
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                context.StmtBlk.Get<BlockSyntax>());
            _pool.Free(init);
            _pool.Free(incr);
            forStmt = CheckForGarbage(forStmt, context.Ignored, "Identifier after NEXT");
            context.Put(forStmt);
        }

        public override void ExitForeachStmt([NotNull] XP.ForeachStmtContext context)
        {
            context.SetSequencePoint(context.end);
            context.Id.SetSequencePoint();
            context.Container.SetSequencePoint();
            StatementSyntax forStmt = _syntaxFactory.ForEachStatement(SyntaxFactory.MakeToken(SyntaxKind.ForEachKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                context.Type?.Get<TypeSyntax>() ?? _impliedType,
                context.Id.Get<SyntaxToken>(),
                SyntaxFactory.MakeToken(SyntaxKind.InKeyword),
                context.Container.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                context.StmtBlk.Get<BlockSyntax>());
            forStmt = CheckForGarbage(forStmt, context.Ignored, "Identifier after NEXT");
            context.Put(forStmt);

        }

        #endregion

        #region Conditional Statements
        public override void ExitIfStmt([NotNull] XP.IfStmtContext context)
        {
            StatementSyntax ifStmt = context.IfStmt.Get<IfStatementSyntax>();
            context.SetSequencePoint(context.IfStmt.Cond);
            ifStmt = CheckForGarbage(ifStmt, context.Ignored, "Expression after END IF");
            if (context.IfStmt.Cond.CsNode is IsPatternExpressionSyntax)
            {
                // wrap the ifStmt in a block to make sure that variable introduced in IF x IS Foo oFoo is not visible after the block
                var stmts = new List<StatementSyntax>();
                stmts.Add(ifStmt);
                context.Put(MakeBlock(stmts));
            }
            else
            { 
                context.Put(ifStmt);
            }
        }

        public override void ExitIfElseBlock([NotNull] XP.IfElseBlockContext context)
        {
            context.SetSequencePoint(context.Cond);
            context.Put(GenerateIfStatement(
                context.Cond.Get<ExpressionSyntax>(),
                context.StmtBlk.Get<BlockSyntax>(),
                (context.ElseIfBlock != null) ?
                    _syntaxFactory.ElseClause(SyntaxFactory.MakeToken(SyntaxKind.ElseKeyword), context.ElseIfBlock.Get<IfStatementSyntax>())
                : (context.ElseBlock != null) ?
                    _syntaxFactory.ElseClause(SyntaxFactory.MakeToken(SyntaxKind.ElseKeyword), context.ElseBlock.Get<BlockSyntax>())
                : null));
        }

        public override void ExitCaseStmt([NotNull] XP.CaseStmtContext context)
        {
            if (context.CaseStmt != null)
            {
                context.SetSequencePoint(context.CaseStmt.Start, context.CaseStmt.end.Start);
            }
            else
                context.SetSequencePoint(context.end);
            if (context.CaseStmt.Key.Type == XP.OTHERWISE)
            {
                // DO case without case block, so we should always execute the otherwise block
                var block = context.CaseStmt.CsNode as StatementSyntax;
                block = block.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.WRN_EmptyCase));
                context.Put(block);
            }
            else
            { 
                StatementSyntax caseStmt = (StatementSyntax)context.CaseStmt?.Get<IfStatementSyntax>() ??
                    _syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                context.Put(caseStmt);
            }
        }

        public override void ExitCaseBlock([NotNull] XP.CaseBlockContext context)
        {
            if (context.Key.Type == XP.OTHERWISE)
            {
                context.SetSequencePoint(context.end);
                context.Put(context.StmtBlk.Get<StatementSyntax>());
            }
            else
            {
                context.SetSequencePoint(context.Cond);
                context.Put(GenerateIfStatement(
                    context.Cond.Get<ExpressionSyntax>(),
                    context.StmtBlk.Get<BlockSyntax>(),
                    (context.NextCase == null) ? null :
                        _syntaxFactory.ElseClause(SyntaxFactory.MakeToken(SyntaxKind.ElseKeyword),
                            context.NextCase.Get<StatementSyntax>())));
            }
        }


        public override void ExitSwitchStmt([NotNull] XP.SwitchStmtContext context)
        {
            context.SetSequencePoint(context.end);
            var sections = _pool.Allocate<SwitchSectionSyntax>();
            var emptyLabels = _pool.Allocate<SwitchLabelSyntax>();
            foreach (var switchBlkCtx in context._SwitchBlock)
            {
                // check for block with empty statement list
                // And concatenate it with the next switch block
                // but not the last block
                var sectionSyntax = switchBlkCtx.Get<SwitchSectionSyntax>();
                if (sectionSyntax.Statements.Count == 0 && switchBlkCtx != context._SwitchBlock.Last())
                {
                    emptyLabels.Add(sectionSyntax.Labels[0]);
                }
                else
                {
                    if (emptyLabels.Count != 0)
                    {
                        // create new labels for sectionSyntax that include the preceding labels
                        emptyLabels.Add(sectionSyntax.Labels[0]);
                        sectionSyntax = _syntaxFactory.SwitchSection(emptyLabels, sectionSyntax.Statements);
                    }
                    if (ContainsExitStatement(switchBlkCtx.StmtBlk._Stmts))
                    {
                        sectionSyntax = sectionSyntax.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_ExitInsideSwitchStatementNotAllowed));
                    }
                    sections.Add(sectionSyntax);
                    emptyLabels.Clear();
                }
            }
            StatementSyntax switchStmt = _syntaxFactory.SwitchStatement(SyntaxFactory.MakeToken(SyntaxKind.SwitchKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                sections,
                SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));

            context.Put(switchStmt);
            _pool.Free(sections);
            _pool.Free(emptyLabels);
        }

        public override void ExitSwitchBlock([NotNull] XP.SwitchBlockContext context)
        {
            var labels = _pool.Allocate<SwitchLabelSyntax>();
            var kw = context.Key.SyntaxKeyword();
            context.SetSequencePoint(context.end);
            if (kw.Kind == SyntaxKind.CaseKeyword)
            {
                labels.Add(_syntaxFactory.CaseSwitchLabel(kw, context.Const?.Get<ExpressionSyntax>(),
                    SyntaxFactory.MakeToken(SyntaxKind.ColonToken)));
            }
            else
            {
                labels.Add(_syntaxFactory.DefaultSwitchLabel(kw, SyntaxFactory.MakeToken(SyntaxKind.ColonToken)));
            }
            var stmts = _pool.Allocate<StatementSyntax>();
            if (context.StmtBlk._Stmts.Count > 0)
            {
                stmts.Add(context.StmtBlk.Get<BlockSyntax>());
                if (NeedsBreak(context.StmtBlk._Stmts))
                {
                    var brk = _syntaxFactory.BreakStatement(SyntaxFactory.MakeToken(SyntaxKind.BreakKeyword),
                        SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                    brk.XNode = context.StmtBlk._Stmts.Last();
                    stmts.Add(brk);
                }
            }
            context.Put(_syntaxFactory.SwitchSection(labels, stmts));
            _pool.Free(labels);
            _pool.Free(stmts);
        }

        #endregion

        #region Error Handling Statements

        public override void ExitTryStmt([NotNull] XP.TryStmtContext context)
        {
            // Add default Catch block for try stmt without catch blocks
            context.SetSequencePoint(context.end);
            if (context._CatchBlock?.Count == 0  && context.FinBlock == null)
            {
                var cb = FixPosition(new XP.CatchBlockContext(context, 0), context.Stop);
                cb.StmtBlk = FixPosition(new XP.StatementBlockContext(cb, 0), context.Stop);
                this.ExitStatementBlock(cb.StmtBlk);
                this.ExitCatchBlock(cb);
                context._CatchBlock.Add(cb);
            }
            var catches = _pool.Allocate<CatchClauseSyntax>();
            foreach (var catchCtx in context._CatchBlock)
            {
                catches.Add(catchCtx.Get<CatchClauseSyntax>());
            }
            StatementSyntax tryStmt =
            _syntaxFactory.TryStatement(SyntaxFactory.MakeToken(SyntaxKind.TryKeyword),
                context.StmtBlk.Get<BlockSyntax>(),
                catches,
                context.FinBlock == null ? null : _syntaxFactory.FinallyClause(SyntaxFactory.MakeToken(SyntaxKind.FinallyKeyword),
                    context.FinBlock.Get<BlockSyntax>()));
            _pool.Free(catches);
            context.Put(tryStmt);
        }

        public override void ExitCatchBlock([NotNull] XP.CatchBlockContext context)
        {
            context.SetSequencePoint(context.end);
            CatchDeclarationSyntax decl = null;
            if (context.Type != null || context.Id != null)
            {
                SyntaxToken id = null;
                TypeSyntax type = null;
                if (context.Id != null)
                {
                    id = context.Id.Get<SyntaxToken>();
                }
                if (context.Type != null)
                {
                    type = context.Type.Get<TypeSyntax>();
                }
                else
                {
                    type = GenerateQualifiedName("System.Exception");
                }
                decl = _syntaxFactory.CatchDeclaration(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    type, 
                    id,
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
            }

            context.Put(_syntaxFactory.CatchClause(SyntaxFactory.MakeToken(SyntaxKind.CatchKeyword),
                decl,
                null, // TODO: (grammar) catch filters?
                context.StmtBlk.Get<BlockSyntax>()));
        }

        public override void ExitSeqStmt([NotNull] XP.SeqStmtContext context)
        {
            context.Put(NotInDialect("BEGIN SEQUENCE statement"));
            return;
        }

        public override void ExitRecoverBlock([NotNull] XP.RecoverBlockContext context)
        {
            context.SetSequencePoint(context.end);
            context.Put(NotInDialect("RECOVER USING block"));
            return;
        }

        #endregion

        #region Other Statements

        public override void ExitStatementBlock([NotNull] XP.StatementBlockContext context)
        {
            List<StatementSyntax> statements = new List<StatementSyntax>();
            if (context._Stmts.Count > 0)
            {
                context.SetSequencePoint(context._Stmts[0].Stop);
            }
            foreach (var stmtCtx in context._Stmts)
            {
                // Sometimes we generate more than 1 C# statement for a xBase statement
                if (stmtCtx.CsNode is SyntaxList<StatementSyntax>)
                {
                    var list = (SyntaxList < StatementSyntax > ) stmtCtx.CsNode ;
                    for (int i = 0; i < list.Count; i++)
                    {
                        var stmt = list[i];
                        statements.Add(stmt);
                    }
                }
                else
                {
                    statements.Add(stmtCtx.Get<StatementSyntax>());
                }
            }
            if (CurrentEntity != null && CurrentEntity.Data.HasDimVar)
            {
                // Check for LOCAL DIM arrays and change them to Fixed statements
                statements = CheckForLocalDimArrays(statements);
            }
            context.Put(MakeBlock(statements));
        }

        private List<StatementSyntax> CheckForLocalDimArrays(List<StatementSyntax> statements)
        {
            if (!CurrentEntity.Data.HasAddressOf)
                return statements;

            foreach (var stmt in statements.ToList())
            {
                if (stmt is LocalDeclarationStatementSyntax)
                {
                    var localdecl = stmt as LocalDeclarationStatementSyntax;
                    if (localdecl.XVoIsDim)
                    {
                        // we only have one variable per localdecl
                        var vars = localdecl.Declaration.Variables;
                        var vardecl = vars[0];
                            
                        // create new declaration where the local is a ptr
                        // copy rest of statements to array
                        // remove rest of statements from the current array

                        int iPos = statements.IndexOf(stmt);
                        var substmts = new StatementSyntax[statements.Count - iPos - 1];
                        statements.CopyTo(iPos + 1, substmts, 0, substmts.Length);
                        statements.RemoveRange(iPos + 1, substmts.Length);
                        var newStmts = CheckForLocalDimArrays(substmts.ToList());
                        var newBlock = MakeBlock(newStmts);

                        // create new declaration for a pointer variable of the type 
                        // that the original declaration points to
                        // The new name will be the originalname + "$Dim"
                        var newId = SyntaxFactory.MakeIdentifier(vardecl.Identifier.Text + "$dim");
                        var atype = localdecl.Declaration.Type as ArrayTypeSyntax;
                        var element = atype.ElementType;
                        var ptype = _syntaxFactory.PointerType(atype.ElementType, SyntaxFactory.MakeToken(SyntaxKind.AsteriskToken));
                        var addressof = _syntaxFactory.PrefixUnaryExpression(SyntaxKind.AddressOfExpression,
                                        SyntaxFactory.MakeToken(SyntaxKind.AmpersandToken),
                                        GenerateSimpleName(vardecl.Identifier.Text));

                        var initexpr = _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                                        addressof);
                        var newvardecl = _syntaxFactory.VariableDeclarator(newId, null, initexpr);
                        var newvardecl2 = _syntaxFactory.VariableDeclaration(ptype, MakeSeparatedList(newvardecl));

                        var newStmt = _syntaxFactory.FixedStatement(
                                    SyntaxFactory.MakeToken(SyntaxKind.FixedKeyword),
                                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                                    newvardecl2,
                                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                                newBlock);
                        statements.Add(newStmt);
                        return statements;
                    }
                }
            }
            return statements;
        }

        private bool ContainsExitStatement(IList<XP.StatementContext> stmts)
        {
            // Checks for EXIT statements that are not inside a LoopStmt
            // Because these are not allowed inside a Switch
            foreach (var stmt in stmts)
            {
                if (stmt is XP.JumpStmtContext)
                {
                    var jmpstmt = stmt as XP.JumpStmtContext;
                    if (jmpstmt.Key.Type == XP.EXIT)
                        return true;
                }
                if (stmt is XP.ILoopStmtContext)    // // For, Foreach, While, Repeat may have private exits
                    continue;
                if (stmt is XP.BlockStmtContext)    // Non looping block
                {
                    var blockstmt = stmt as XP.BlockStmtContext;
                    if (ContainsExitStatement(blockstmt.StmtBlk._Stmts))
                        return true;
                }
                if (stmt is XP.IfStmtContext)
                {
                    var ifstmt = stmt as XP.IfStmtContext;
                    var ifelsestmt = ifstmt.IfStmt;
                    if (ContainsExitStatement(ifelsestmt.StmtBlk._Stmts))           // First IF Block
                        return true;
                    while (ifelsestmt.ElseIfBlock != null)                          // Subsequent elseif blocks
                    {
                        ifelsestmt = ifelsestmt.ElseIfBlock;
                        if (ContainsExitStatement(ifelsestmt.StmtBlk._Stmts))
                            return true;
                    }
                    if (ifelsestmt.ElseIfBlock != null &&                           // Else block
                         ContainsExitStatement(ifelsestmt.ElseBlock._Stmts))
                        return true;
                }
                if (stmt is XP.CaseStmtContext)
                {
                    var docasestmt = stmt as XP.CaseStmtContext;
                    var casestmt = docasestmt.CaseStmt;     // CaseBlock
                    while (casestmt != null)                // Handles the all case and otherwise blocks
                    {
                        if (ContainsExitStatement(casestmt.StmtBlk._Stmts))
                            return true;
                        casestmt = casestmt.NextCase;
                    }
                }
            }
            return false;
        }

        private bool NeedsBreak(IList<XP.StatementContext> stmts, bool inSideLoop = false)
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

                if ((jmpstmt.Key.Type == XP.EXIT || jmpstmt.Key.Type == XP.LOOP) && !inSideLoop)
                {
                    // LOOP or EXIT inside a nested Loop block
                    return false;
                }
            }
            if (stmt is XP.IfStmtContext)
            {
                var ifstmt = stmt as XP.IfStmtContext;
                var ifelsestmt = ifstmt.IfStmt;
                var elsestmt = ifelsestmt?.ElseBlock;           // The first ifelsestmt should always have a value, but better safe than sorry
                // process to the end of the list
                // when there is no else, then we need a break
                // otherwise process every statement list
                while (ifelsestmt != null)                     //
                {
                    if (NeedsBreak(ifelsestmt.StmtBlk._Stmts))
                    {
                        return true;
                    }
                    elsestmt = ifelsestmt.ElseBlock;
                    ifelsestmt = ifelsestmt.ElseIfBlock;
                }
                // No Else, so there is at least one block that does not end with a RETURN etc.
                if (elsestmt == null)
                {
                    return true;
                }
                else 
                {
                    return NeedsBreak(elsestmt._Stmts);
                }
            }
            if (stmt is XP.CaseStmtContext)
            {
                var docasestmt = stmt as XP.CaseStmtContext;
                var casestmt = docasestmt.CaseStmt;     // CaseBlock, there may be no blocks at all.
                int lastkey = XP.CASE;
                while (casestmt != null)                // otherwise is also a CaseBlock stored in NextCase
                {
                    if (NeedsBreak(casestmt.StmtBlk._Stmts))
                        return true;
                    lastkey = casestmt.Key.Type;
                    casestmt = casestmt.NextCase;
                }
                if (lastkey == XP.CASE) // There is no otherwise
                    return true;
                return false;           // all branches end with a breaking statement
            }
            if (stmt is XP.BlockStmtContext)
            {
                var blockstmt = stmt as XP.BlockStmtContext;
                return NeedsBreak(blockstmt.StmtBlk._Stmts);
            }
            if (stmt is XP.ILoopStmtContext)        // For, Foreach, While, Repeat
            {
                var blockstmt = stmt as XP.ILoopStmtContext;
                return NeedsBreak(blockstmt.Statements._Stmts, true);
            }
            if (stmt is XP.SwitchStmtContext)
            {
                var swstmt = stmt as XP.SwitchStmtContext;
                bool hasdefault = false;
                foreach (var swBlock in swstmt._SwitchBlock)
                {
                    if (swBlock.StmtBlk._Stmts.Count > 0 && NeedsBreak(swBlock.StmtBlk._Stmts))
                        return true;
                    if (swBlock.Key.Type != XP.CASE)
                        hasdefault = true;
                }
                if (!hasdefault)
                    return true;
                return false;           // all branches end with a breaking statement
            }

            return true;
        }

        public override void ExitJumpStmt([NotNull] XP.JumpStmtContext context)
        {
            context.SetSequencePoint(context.end);
            switch (context.Key.Type)
            {
                case XP.EXIT:
                    context.Put(_syntaxFactory.BreakStatement(SyntaxFactory.MakeToken(SyntaxKind.BreakKeyword),
                        SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                    break;
                case XP.LOOP:
                    context.Put(_syntaxFactory.ContinueStatement(SyntaxFactory.MakeToken(SyntaxKind.ContinueKeyword),
                        SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                    break;
                case XP.BREAK:
                    context.Put(NotInDialect("BREAK statement"));
                    break;
                case XP.THROW:
                    context.Put(_syntaxFactory.ThrowStatement(SyntaxFactory.MakeToken(SyntaxKind.ThrowKeyword),
                        context.Expr.Get<ExpressionSyntax>(),
                        SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                    break;

            }
        }


        public override void ExitExpressionStmt([NotNull] XP.ExpressionStmtContext context)
        {
            var statements = _pool.Allocate<StatementSyntax>();
            context.SetSequencePoint(context._Exprs[0].Start, context._Exprs[0].Stop);
            foreach (var exprCtx in context._Exprs)
            {
                // check because there may already be statements in here, such as the IF statement generated for AltD()
                var node = exprCtx.CsNode;
                if (node is StatementSyntax)
                {
                    statements.Add((StatementSyntax)node);
                }
                else
                {
                    var stmt = GenerateExpressionStatement(exprCtx.Get<ExpressionSyntax>());
                    stmt.XNode = exprCtx;
                    statements.Add(stmt);
                }
            }

            if (statements.Count == 1)
            {
                context.Put(statements[0]);
            }
            else
            {
                var block = MakeBlock(statements);
                context.Put(block);
            }

            _pool.Free(statements);
        }



        public override void ExitReturnStmt([NotNull] XP.ReturnStmtContext context)
        {
            context.SetSequencePoint(context.end);
            var expr = context.Expr?.Get<ExpressionSyntax>();
            var ent = CurrentEntity;
            if (context.Void != null && ent != null && !ent.Data.MustBeVoid)
            {
                expr = GenerateLiteral(0);
            }
            // / vo9 is handled in the Subclass
            context.Put(GenerateReturn(expr));
        }

        public override void ExitYieldStmt([NotNull] XP.YieldStmtContext context)
        {
            SyntaxKind kind;
            ExpressionSyntax arg;
            SyntaxToken token;
            context.SetSequencePoint(context.end);
            if (CurrentEntity != null)
                CurrentEntity.Data.HasYield = true;
            if (context.Break != null)  // yield exit or yield break
            {
                kind = SyntaxKind.YieldBreakStatement;
                arg = null;
                token = SyntaxFactory.MakeToken(SyntaxKind.BreakKeyword);
            }
            else                   // yield return
            {
                kind = SyntaxKind.YieldReturnStatement;
                arg = context.Expr?.Get<ExpressionSyntax>();
                token = SyntaxFactory.MakeToken(SyntaxKind.ReturnKeyword);
            }
            context.Put(_syntaxFactory.YieldStatement(kind, SyntaxFactory.MakeToken(SyntaxKind.YieldKeyword),
                token, arg,
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitQoutStmt([NotNull] XP.QoutStmtContext context)
        {
            ArgumentSyntax arg;
            ExpressionSyntax expr;
            // If dialect VO and VulcanRTFuncs included
            // Simply generate call to VulcanRTFuncs.Functions.QOut or QQOut
            // This is done in VOTreeTransForm
            context.SetSequencePoint(context.end);
            if (!(context._Exprs?.Count > 0))
            {
                if (context.Q.Type == XP.QMARK)
                {
                    expr = GenerateMethodCall(SystemQualifiedNames.WriteLine);
                    context.Put(GenerateExpressionStatement(expr));
                }
                else
                {
                    context.Put(_syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                }
            }
            else
            {
                // build list of arguments and a matching string.Format mask
                // when single question mark, then start with newline
                string mask = context.Q.Type == XP.QMARK ? "\r\n" : String.Empty;
                var args = new List<ArgumentSyntax>();
                foreach (var eCtx in context._Exprs)
                {
                    if (args.Count > 0)
                    {
                        mask += " ";
                    }
                    mask += "{" + args.Count.ToString() + "}";
                    expr = eCtx.Get<ExpressionSyntax>();
                    arg = MakeArgument(expr);
                    args.Add(arg);
                }
                args.Insert(0, MakeArgument(GenerateLiteral(mask)));
                // convert args to Array because the overload that receives a collection expects another kind of object
                var arglist = _syntaxFactory.ArgumentList(
                                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                                MakeSeparatedList<ArgumentSyntax>(args.ToArray()),
                                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));

                expr = GenerateMethodCall(SystemQualifiedNames.Write, arglist);
                context.Put(GenerateExpressionStatement(expr));
            }
        }

        public override void ExitBlockStmt([NotNull] XP.BlockStmtContext context)
        {
            context.SetSequencePoint(context.end);
            StatementSyntax node;
            switch (context.Key.Type)
            {
                case XP.SCOPE:
                    node = context.StmtBlk.Get<BlockSyntax>();
                    break;
                case XP.LOCK:
                    node = MakeLock(context.Expr.Get<ExpressionSyntax>(),
                        context.StmtBlk.Get<BlockSyntax>());
                    break;
                case XP.UNSAFE:
                    node = _syntaxFactory.UnsafeStatement(SyntaxFactory.MakeToken(SyntaxKind.UnsafeKeyword),
                        context.StmtBlk.Get<BlockSyntax>());
                    break;
                case XP.CHECKED:
                    node = _syntaxFactory.CheckedStatement(SyntaxKind.CheckedStatement,
                        SyntaxFactory.MakeToken(SyntaxKind.CheckedKeyword),
                        context.StmtBlk.Get<BlockSyntax>());
                    break;
                case XP.FIXED:
                    node = _syntaxFactory.FixedStatement(
                        SyntaxFactory.MakeToken(SyntaxKind.FixedKeyword),
                           SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                           context.VarDecl?.Get<VariableDeclarationSyntax>(),
                           SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                        context.StmtBlk.Get<BlockSyntax>());
                    break;
                case XP.UNCHECKED:
                    node = _syntaxFactory.CheckedStatement(SyntaxKind.UncheckedStatement,
                        SyntaxFactory.MakeToken(SyntaxKind.UncheckedKeyword),
                        context.StmtBlk.Get<BlockSyntax>());
                    break;
                case XP.USING:
                    node = _syntaxFactory.UsingStatement(SyntaxFactory.MakeToken(SyntaxKind.UsingKeyword),
                           SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                           context.VarDecl?.Get<VariableDeclarationSyntax>(),
                           context.Expr?.Get<ExpressionSyntax>(),
                           SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                           context.StmtBlk.Get<BlockSyntax>());
                    break;
                default:
                    // what else;
                    node = null;
                    break;
            }
            if (node != null)
            {
                context.Put(node);
            }

        }

        public override void ExitNopStmt([NotNull] XP.NopStmtContext context)
        {
            context.SetSequencePoint(context.end);
            context.Put(_syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        #endregion
        #region  Vulcan UDCs
        //public override void ExitDefaultStmt([NotNull] XP.DefaultStmtContext context)
        //{
        //    context.Put(NotInDialect("DEFAULT Statement"));
        //}

        //public override void ExitWaitAcceptStmt([NotNull] XP.WaitAcceptStmtContext context)
        //{
        //    context.Put(NotInDialect(context.Key.Text.ToUpper() + " Statement"));
        //}
        //public override void ExitCancelQuitStmt([NotNull] XP.CancelQuitStmtContext context)
        //{
        //    context.Put(NotInDialect(context.Key.Text.ToUpper() + " Statement"));
        //}
        #endregion
        #endregion
        #region Expressions
        #region Conditional Expressions
        public override void ExitCondAccessExpr([NotNull] XP.CondAccessExprContext context)
        {
#if false // nvk: check not needed because it is a separate rule now!
            switch (context.Right.Start.Type) {
                case XP.DOT:
                case XP.COLON:
                case XP.LBRKT:
                    break;
                default:
                    context.AddError(new ParseErrorData(context.Right.Start,ErrorCode.ERR_SyntaxError,"."));
                    break;
            }
#endif
            context.Put(_syntaxFactory.ConditionalAccessExpression(
                context.Left.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.QuestionToken),
                context.Right.Get<ExpressionSyntax>()
            ));
        }

        public override void ExitIif([NotNull] XP.IifContext context)
        {
            ExpressionSyntax left;
            ExpressionSyntax right;
            if (context.TrueExpr == null)
            {
                // Harbour does not warn when NIL is missing
                left = GenerateMissingExpression(_options.Dialect != XSharpDialect.Harbour);
            }
            else
            {
                left = context.TrueExpr.Get<ExpressionSyntax>();
            }
            if (context.FalseExpr == null)
            {
                // Harbour does not warn when NIL is missing
                right = GenerateMissingExpression(_options.Dialect != XSharpDialect.Harbour);
            }
            else
            {
                right = context.FalseExpr.Get<ExpressionSyntax>();
            }
            context.Put(MakeConditional(context.Cond.Get<ExpressionSyntax>(),left,right));
        }

        #endregion
        
        #region Bound Expressions
        public override void ExitBoundAccessMember([NotNull] XP.BoundAccessMemberContext context)
        {
            context.Put(MakeSimpleMemberAccess(
                context.Expr.Get<ExpressionSyntax>(),
                context.Name.Get<SimpleNameSyntax>()));
        }

        public override void ExitBoundArrayAccess([NotNull] XP.BoundArrayAccessContext context)
        {
            context.Put(_syntaxFactory.ElementAccessExpression(
                context.Expr.Get<ExpressionSyntax>(),
                context.ArgList?.Get<BracketedArgumentListSyntax>()
                    ?? _syntaxFactory.BracketedArgumentList(
                        SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                        default(SeparatedSyntaxList<ArgumentSyntax>),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken))));
        }

        public override void ExitBoundMethodCall([NotNull] XP.BoundMethodCallContext context)
        {
            context.Put(_syntaxFactory.InvocationExpression(
                context.Expr.Get<ExpressionSyntax>(),
                context.ArgList?.Get<ArgumentListSyntax>()
                    ?? _syntaxFactory.ArgumentList(
                        SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                        default(SeparatedSyntaxList<ArgumentSyntax>),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken))));
        }

        public override void ExitBoundCondAccessExpr([NotNull] XP.BoundCondAccessExprContext context)
        {
            context.Put(_syntaxFactory.ConditionalAccessExpression(
                context.Left.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.QuestionToken),
                context.Right.Get<ExpressionSyntax>()
            ));
        }

        public override void ExitBindMemberAccess([NotNull] XP.BindMemberAccessContext context)
        {
            context.Put(_syntaxFactory.MemberBindingExpression(
                SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                context.Name.Get<SimpleNameSyntax>()));
        }

        public override void ExitBindArrayAccess([NotNull] XP.BindArrayAccessContext context)
        {
            context.Put(_syntaxFactory.ElementBindingExpression(
                context.ArgList?.Get<BracketedArgumentListSyntax>() ?? EmptyBracketedArgumentList()
            ));
        }


        public override void ExitAccessMember([NotNull] XP.AccessMemberContext context)
        {
            if (context.Op.Type == XP.COLONCOLON)
            {
                context.Put(MakeSimpleMemberAccess(
                    GenerateSelf(),
                    context.Name.Get<SimpleNameSyntax>()));
            }
            else
            {
                context.Put(MakeSimpleMemberAccess(
                    context.Expr.Get<ExpressionSyntax>(),
                    context.Name.Get<SimpleNameSyntax>()));
            }
        }

        #endregion

        #region Common Expressions
        public override void ExitExpressionList([NotNull] XP.ExpressionListContext context)
        {
            var stmts = _pool.Allocate<StatementSyntax>();
            foreach (var eCtx in context._Exprs)
            {
                stmts.Add(GenerateExpressionStatement(eCtx.Get<ExpressionSyntax>()));
            }
            context.PutList(stmts.ToList());
            _pool.Free(stmts);
        }


        public override void ExitPostfixExpression([NotNull] XP.PostfixExpressionContext context)
        {
            context.Put(_syntaxFactory.PostfixUnaryExpression(
                context.Op.ExpressionKindPostfixOp(),
                context.Expr.Get<ExpressionSyntax>(),
                context.Op.SyntaxOp()));
        }

        public override void ExitPrefixExpression([NotNull] XP.PrefixExpressionContext context)
        {
            // Note
            // in VO ~is XOR for binary expressions and bitwise negation (ones complement) for unary expressions
            // in C# ^is XOR and ~is Bitwise negation (ones complement)
            // SyntaxPrefixOp() takes care of the Unary operators
            if (context.Op.Type == XP.ADDROF)
            {
                CurrentEntity.Data.HasAddressOf = true;
            }
            context.Put(_syntaxFactory.PrefixUnaryExpression(
                context.Op.ExpressionKindPrefixOp(),
                context.Op.SyntaxPrefixOp(),
                context.Expr.Get<ExpressionSyntax>()));
        }

        public override void ExitBinaryExpression([NotNull] XP.BinaryExpressionContext context)
        {
            // when /vo12 is used then for the types .DIV add conversion for the LHS and RHS to Double


            switch (context.Op.Type)
            {
                case XP.EXP:
                    context.Put(GenerateMethodCall(SystemQualifiedNames.Pow,
                        _syntaxFactory.ArgumentList(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                            MakeSeparatedList(MakeArgument(context.Left.Get<ExpressionSyntax>()),
                                MakeArgument(context.Right.Get<ExpressionSyntax>())),
                            SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken))));
                    break;
                case XP.SUBSTR:
                    // Convert LHS $ RHS to RHS:IndexOf(LHS) >= 0
                    // but since they both can be NULL add a condition:
                    // LHS == NULL ? FALSE: RHS:IndexOf(LHS)

                    var condition = _syntaxFactory.BinaryExpression(SyntaxKind.EqualsExpression, context.Left.Get<ExpressionSyntax>(),
                        SyntaxFactory.MakeToken(SyntaxKind.EqualsEqualsToken), GenerateLiteralNull());
                    var indexof = _syntaxFactory.ConditionalAccessExpression(
                                    context.Right.Get<ExpressionSyntax>(),
                                    SyntaxFactory.MakeToken(SyntaxKind.QuestionToken),
                                    _syntaxFactory.InvocationExpression(_syntaxFactory.MemberBindingExpression(SyntaxFactory.MakeToken(SyntaxKind.DotToken), GenerateSimpleName("IndexOf")),
                                     MakeArgumentList(MakeArgument(context.Left.Get<ExpressionSyntax>()))));
                    var rhsExp = _syntaxFactory.BinaryExpression(SyntaxKind.GreaterThanExpression, indexof,
                                SyntaxFactory.MakeToken(SyntaxKind.GreaterThanToken),
                                GenerateLiteral("-1", -1));

                    var exp = MakeConditional(condition, GenerateLiteral(false), rhsExp);

                    context.Put(exp);
                    break;
                case XP.ASSIGN_EXP:
                    context.Put(MakeSimpleAssignment(
                        context.Left.Get<ExpressionSyntax>(),
                        GenerateMethodCall(SystemQualifiedNames.Pow,
                            _syntaxFactory.ArgumentList(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                                MakeSeparatedList(MakeArgument(context.Left.Get<ExpressionSyntax>()),
                                    MakeArgument(context.Right.Get<ExpressionSyntax>())),
                                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)))));
                    break;
                case XP.GT:
                    if (context.Gt == null)     // Normal Greater than
                        goto default;

                    SyntaxToken token = GetRShiftToken(context.Op, context.Gt);
                    context.Put(_syntaxFactory.BinaryExpression(
                        SyntaxKind.RightShiftExpression,
                        context.Left.Get<ExpressionSyntax>(),
                        token,
                        context.Right.Get<ExpressionSyntax>()));

                    break;
                default:
                    // Note
                    // in VO ~is XOR for binary expressions and bitwise negation for unary expressions
                    // in C# ^is XOR and ~is Bitwise negation
                    // SyntaxOp() takes care of the Binary Operators
                    context.Put(_syntaxFactory.BinaryExpression(
                        context.Op.ExpressionKindBinaryOp(),
                        context.Left.Get<ExpressionSyntax>(),
                        context.Op.SyntaxOp(),
                        context.Right.Get<ExpressionSyntax>()));
                    break;
            }
        }

        public override void ExitAssignmentExpression([NotNull] XP.AssignmentExpressionContext context)
        {
            context.Put(_syntaxFactory.AssignmentExpression(
                context.Op.ExpressionKindBinaryOp(),
                context.Left.Get<ExpressionSyntax>(),
                context.Op.SyntaxOp(),
                context.Right.Get<ExpressionSyntax>()));
        }

        public override void ExitPrimaryExpression([NotNull] XP.PrimaryExpressionContext context)
        {
            context.Put(context.Expr.Get<CSharpSyntaxNode>());
        }

        public override void ExitCheckedExpression([NotNull] XP.CheckedExpressionContext context)
        {
            var expr = MakeChecked(context.Expr.Get<ExpressionSyntax>(), context.ch.Type == XP.CHECKED);
            context.Put(expr);
        }

        private bool GenerateAltD(XP.MethodCallContext context)
        {
            // Pseudo function AltD()
            ArgumentListSyntax argList;
            if (context.ArgList != null)
            {
                argList = context.ArgList.Get<ArgumentListSyntax>();
            }
            else
            {
                argList = EmptyArgumentList();
            }
            var stmt = GenerateExpressionStatement(GenerateMethodCall(SystemQualifiedNames.DebuggerBreak, argList));
            var cond = MakeSimpleMemberAccess(
                        GenerateQualifiedName(SystemQualifiedNames.Debugger),
                        GenerateSimpleName("IsAttached"));
            context.Put(GenerateIfStatement(cond, stmt));
            return true;
        }

        private bool GenerateGetInst(XP.MethodCallContext context)
        {
            // Pseudo function _GetInst()
            ArgumentListSyntax argList;
            ExpressionSyntax expr;
            if (context.ArgList != null)
            {
                argList = context.ArgList.Get<ArgumentListSyntax>();
                if (argList.Arguments.Count != 0)
                {
                    context.Put(GenerateLiteral(0).WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_BadArgCount, "_getInst", argList.Arguments.Count)));
                    return true;
                }
            }

            TypeSyntax globaltype = GenerateQualifiedName(GlobalClassName);
            expr = MakeTypeOf(globaltype);

            expr = MakeSimpleMemberAccess(expr, GenerateSimpleName("Module"));
            argList = MakeArgumentList(MakeArgument(expr));
            expr = GenerateMethodCall(SystemQualifiedNames.GetHInstance, argList);
            context.Put(expr);
            return true;
        }

        private bool GenerateChr(XP.MethodCallContext context)
        {
            // Pseudo function _Chr and Chr()
            ArgumentListSyntax argList;
            ExpressionSyntax expr;
            int count = 0;
            if (context.ArgList != null)
            {
                argList = context.ArgList.Get<ArgumentListSyntax>();
                count = argList.Arguments.Count;
            }
            else
            {
                argList = null;
            }
            if (count != 1)
            {
                expr = context.Expr.Get<ExpressionSyntax>();
                string name = String.Empty;
                if (expr is IdentifierNameSyntax)
                {
                    name = ((IdentifierNameSyntax)expr).Identifier.Text;
                }
                else if (expr is GenericNameSyntax)
                {
                    name = ((GenericNameSyntax)expr).Identifier.Text;
                }

                context.Put(GenerateLiteral(0).WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_BadArgCount, name, count)));
                return true;
            }
            var arg = argList.Arguments[0];
            var exp = arg.Expression;
            var lit = exp.XNode.GetLiteralToken();
            if (lit != null && (lit.Type == XP.INT_CONST || lit.Type == XP.HEX_CONST || lit.Type == XP.BIN_CONST))
            {
                // get number and create a string literal value
                var value = lit.SyntaxLiteralValue(_options);
                Int64 number = Convert.ToInt64(value.Value);
                char ch = ' ';
                bool overflow = false; ;
                if (number < UInt16.MaxValue)
                    ch = (char)number;
                else
                    overflow = true;
                if (number > 0 && number <= 127)
                {
                    var literal = GenerateLiteral(ch.ToString());
                    if (overflow)
                    {
                        literal = literal.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_IntOverflow));
                    }
                    context.Put(literal);
                    return true;
                }
            }
            if (_options.HasRuntime)
            {
               context.Put(GenerateMethodCall(_options.XSharpRuntime ? XSharpQualifiedFunctionNames.Chr : VulcanQualifiedFunctionNames.Chr, argList));
            }
            else
            {
                context.Put(GenerateMethodCall(context.Expr.GetText(), argList));
            }
            return true;
        }

        public override void ExitMethodCall([NotNull] XP.MethodCallContext context)
        {
            var expr = context.Expr.Get<ExpressionSyntax>();
            string name = String.Empty;
            if (expr is IdentifierNameSyntax)
            {
                name = ((IdentifierNameSyntax)expr).Identifier.Text.ToUpper();
            }
            else if (expr is GenericNameSyntax)
            {
                name = ((GenericNameSyntax)expr).Identifier.Text.ToUpper();
            }
            switch (name)
            {
                case "ALTD":
                    if (GenerateAltD(context))
                        return;
                    break;
                case "_GETINST":
                    if (GenerateGetInst(context))
                        return;
                    break;
                case "_CHR":
                case "CHR":
                    if (GenerateChr(context))
                        return;
                    break;
                    //case "PCALL":
                    //case "CCALL":
                    //case "PCALLNATIVE":
                    //case "CCALLNATIVE":
                    //    expr = (ExpressionSyntax)NotInDialect(expr, name + " pseudo function");
                    //    break;
            }
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

        }

        public override void ExitCtorCall([NotNull] XP.CtorCallContext context)
        {
            if (!(context.Type is XP.ArrayDatatypeContext))
            {
                var type = context.Type.Get<TypeSyntax>();
                ArgumentListSyntax argList;
                InitializerExpressionSyntax init = null;
                if (context.ArgList != null)
                {
                    argList = context.ArgList.Get<ArgumentListSyntax>();
                }
                else
                {
                    argList = EmptyArgumentList();
                }
                if (context.Init != null)
                {
                    init = context.Init.Get<InitializerExpressionSyntax>();
                }
                context.Put(CreateObject(type, argList, init));
            }
            else
            {
                var type = (context.Type as XP.ArrayDatatypeContext).TypeName.Get<TypeSyntax>();
                var arrayType = context.Type.Get<ArrayTypeSyntax>();
                var rankSpecifiers = new ArrayRankSpecifierSyntax[arrayType.RankSpecifiers.Count];
                for (int i = 0; i < rankSpecifiers.Length; i++)
                {
                    rankSpecifiers[i] = arrayType.RankSpecifiers[i];
                }
                int ranks = rankSpecifiers[0].Sizes.Count;
                if (ranks != context.ArgList?._Args?.Count)
                    context.AddError(new ParseErrorData(ErrorCode.ERR_BadCtorArgCount, context.Type.GetText(), context.ArgList?._Args?.Count ?? 0));
                var sizes = _pool.AllocateSeparated<ExpressionSyntax>();
                if (context.ArgList?._Args != null)
                {
                    foreach (var size in context.ArgList?._Args)
                    {
                        if (size.Name != null)
                            context.AddError(new ParseErrorData(size, ErrorCode.ERR_BadNamedArgument, size));
                        if (size.RefOut != null)
                            context.AddError(new ParseErrorData(size, ErrorCode.ERR_BadTypeArgument, size));
                        if (sizes.Count > 0)
                            sizes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                        sizes.Add(size.Expr.Get<ExpressionSyntax>());
                    }
                }
                rankSpecifiers[0] = _syntaxFactory.ArrayRankSpecifier(
                            SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                            sizes,
                            SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken));
                _pool.Free(sizes);
                context.Put(_syntaxFactory.ArrayCreationExpression(SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                    _syntaxFactory.ArrayType(type, MakeList(rankSpecifiers)),
                    null));
            }
        }

        public override void ExitDelegateCtorCall([NotNull] XP.DelegateCtorCallContext context)
        {
            if (((context.Obj as XP.PrimaryExpressionContext)?.Expr as XP.LiteralExpressionContext)?.Literal.Token.Type == XP.NULL)
            {
                context.Put(MakeCastTo(context.Type.Get<TypeSyntax>(), context.Func.Get<NameSyntax>()));
            }
            else
            {
                var fobj = context.Obj.Get<ExpressionSyntax>();
                SimpleNameSyntax fname;
                if (context.Func.CsNode is SimpleNameSyntax)
                {
                    fname = context.Func.Get<SimpleNameSyntax>();
                }
                else
                {
                    var fCtx = context.Func as XP.QualifiedNameContext;
                    if (fCtx != null)
                    {
                        fobj = MakeCastTo(fCtx.Left.Get<NameSyntax>(), fobj);
                        fname = fCtx.Right.Get<SimpleNameSyntax>();
                    }
                    else
                    {
                        fname = _syntaxFactory.IdentifierName(SyntaxFactory.MakeIdentifier("<missing>"));
                        context.AddError(new ParseErrorData(fCtx, ErrorCode.ERR_IdentifierExpected));
                    }
                }
                context.Put(MakeCastTo(
                    context.Type.Get<TypeSyntax>(),
                    MakeSimpleMemberAccess(fobj, fname)
                    ));
            }
        }

        public override void ExitArrayAccess([NotNull] XP.ArrayAccessContext context)
        {
            var expr = context.Expr.Get<ExpressionSyntax>();
            BracketedArgumentListSyntax argList;
            if (context.ArgList != null)
            {
                argList = context.ArgList.Get<BracketedArgumentListSyntax>();
            }
            else
            {
                var openBracket = SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken);
                var closeBracket = SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken);
                var args = default(SeparatedSyntaxList<ArgumentSyntax>);
                argList = _syntaxFactory.BracketedArgumentList(openBracket, args, closeBracket);
            }
            context.Put(_syntaxFactory.ElementAccessExpression(expr, argList));
        }

        public override void ExitNameExpression([NotNull] XP.NameExpressionContext context)
        {
            ExpressionSyntax expr = context.Name.Get<NameSyntax>();
            context.Put(expr);
        }


        public override void ExitIifExpression([NotNull] XP.IifExpressionContext context)
        {
            context.Put(context.Expr.Get<ExpressionSyntax>());
        }

        public override void ExitParenExpression([NotNull] XP.ParenExpressionContext context)
        {
            context.Put(_syntaxFactory.ParenthesizedExpression(
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)));
        }

        public override void ExitIntrinsicExpression([NotNull] XP.IntrinsicExpressionContext context)
        {
            var kind = context.Op.ExpressionKindBinaryOp();
            var syntax = context.Op.SyntaxOp();
            if (kind == SyntaxKind.BitwiseNotExpression)
            {
                if (context._Exprs.Count > 1)
                {
                    context.AddError(new ParseErrorData(context.COMMA()[0], ErrorCode.ERR_CloseParenExpected));
                }
                context.Put(_syntaxFactory.PrefixUnaryExpression(
                    kind,
                    syntax,
                    context._Exprs[0].Get<ExpressionSyntax>()));
            }
            else
            {
                var e = context._Exprs[0].Get<ExpressionSyntax>();
                if (context._Exprs.Count > 1)
                {
                    for (int i = 1; i < context._Exprs.Count; i++)
                    {
                        e = _syntaxFactory.BinaryExpression(kind,
                                                            e,
                                                            syntax,
                                                            context._Exprs[i].Get<ExpressionSyntax>());
                    }
                    context.Put(e);
                }
                else
                {
                    context.Put(e);
                    context.AddError(new ParseErrorData(context.Op, ErrorCode.ERR_MissingArgument));
                }
            }
        }

        public override void ExitTypeCheckExpression([NotNull] XP.TypeCheckExpressionContext context)
        {
            if (context.Op.Type == XP.IS)
            {
                if (context.Id != null)
                {
                    var id = SyntaxFactory.Identifier(context.Id.GetText());
                    var designation = _syntaxFactory.SingleVariableDesignation(id);
                    var pattern =  _syntaxFactory.DeclarationPattern(context.Type.Get<TypeSyntax>(), designation);
                    context.Put(_syntaxFactory.IsPatternExpression(
                        context.Expr.Get<ExpressionSyntax>(),
                        SyntaxFactory.MakeToken(SyntaxKind.IsKeyword),
                        (PatternSyntax)pattern));
                }
                else
                { 
                    context.Put(_syntaxFactory.BinaryExpression(
                        SyntaxKind.IsExpression,
                        context.Expr.Get<ExpressionSyntax>(),
                        SyntaxFactory.MakeToken(SyntaxKind.IsKeyword),
                        context.Type.Get<ExpressionSyntax>()));
                }
            }
            else if (context.Op.Type == XP.ASTYPE)
            {
                context.Put(_syntaxFactory.BinaryExpression(
                    SyntaxKind.AsExpression,
                    context.Expr.Get<ExpressionSyntax>(),
                    SyntaxFactory.MakeToken(SyntaxKind.AsKeyword),
                    context.Type.Get<ExpressionSyntax>()));
            }

        }

        public override void ExitTypeCast([NotNull] XP.TypeCastContext context)
        {
            var expr = context.Expr.Get<ExpressionSyntax>();
            expr = MakeCastTo(context.Type.Get<TypeSyntax>(), expr);
            // If the expression is part of a CHECKED or UNCHECKED
            // Syntax then there is no need to explicitely add the Checked
            // for example C578: 
            // DEFINE d2 := unchecked ((WORD) -1)
            if (_options.HasRuntime  && _options.TargetDLL  == XSharpTargetDLL.Other && !(context.Parent is XP.CheckedExpressionContext))
            {
                expr = MakeChecked(expr, _options.Overflow);
            }
            context.Put(expr);
        }

        public override void ExitVoConversionExpression([NotNull] XP.VoConversionExpressionContext context)
        {
            var expr = MakeChecked(context.Expr.Get<ExpressionSyntax>(), false);
            if (context.Type != null)
            {
                expr = MakeChecked(MakeCastTo(context.Type.Get<TypeSyntax>(), expr), false);
            }
            else if (context.XType != null)
            {
                expr = MakeChecked(MakeCastTo(context.XType.Get<TypeSyntax>(), expr),false);
            }
            context.Put(expr);
        }

        public override void ExitVoCastExpression([NotNull] XP.VoCastExpressionContext context)
        {
            TypeSyntax type;
            int mask = 0;
            if (context.Type != null)
            {
                type = context.Type.Get<TypeSyntax>();
                switch (context.Type.Token.Type)
                {
                    case XP.BYTE:
                        mask = 0xff;
                        break;
                    case XP.CHAR:
                    case XP.WORD:
                    case XP.SHORTINT:
                        mask = 0xffff;
                        break;
                }
            }
            else
            {
                type = context.XType.Get<TypeSyntax>();
            }
            var expr = context.Expr.Get<ExpressionSyntax>();
            // check for cast from a logical literal to a numeric
            // in that case replace FALSE with 0 and TRUE with 1
            if (expr.Kind == SyntaxKind.TrueLiteralExpression || expr.Kind == SyntaxKind.FalseLiteralExpression)
            {
                bool numeric = false;
                if (type is PredefinedTypeSyntax)
                {
                    var pdt = type as PredefinedTypeSyntax;
                    switch (pdt.Keyword.Kind)
                    {
                        // 4 x unsigned
                        case SyntaxKind.ByteKeyword:
                        case SyntaxKind.CharKeyword:
                        case SyntaxKind.UShortKeyword:
                        case SyntaxKind.UIntKeyword:
                        case SyntaxKind.ULongKeyword:
                        // 4 x signed
                        case SyntaxKind.SByteKeyword:
                        case SyntaxKind.ShortKeyword:
                        case SyntaxKind.IntKeyword:
                        case SyntaxKind.LongKeyword:
                        // floating point
                        case SyntaxKind.FloatKeyword:
                        case SyntaxKind.DoubleKeyword:
                        case SyntaxKind.DecimalKeyword:
                            numeric = true;
                            break;
                    }
                }
                if (numeric)
                {
                    LiteralExpressionSyntax lit = expr as LiteralExpressionSyntax;
                    if (lit.Kind == SyntaxKind.TrueLiteralExpression)
                    {
                        expr = GenerateLiteral(1);
                    }
                    else
                    {
                        expr = GenerateLiteral(0);
                    }
                }
            }
            else
            {
                expr = MakeChecked(expr, false);
            }

            if (mask != 0)
            {
                expr = MakeChecked(_syntaxFactory.BinaryExpression(
                        SyntaxKind.BitwiseAndExpression,
                        expr,
                        SyntaxFactory.MakeToken(SyntaxKind.AmpersandToken),
                        GenerateLiteral(mask)), false);
            }
            context.Put(MakeChecked(MakeCastTo(type, expr), false));
            return;
        }

        public override void ExitVoCastPtrExpression([NotNull] XP.VoCastPtrExpressionContext context)
        {
            if (context.Expr is XP.MethodCallContext)
            {
                var expr = GenerateLiteral(0).WithAdditionalDiagnostics(
                    new SyntaxDiagnosticInfo(ErrorCode.ERR_PtrCastNotAllowed));
                context.Put(expr);
            }
            else
            {
                context.Put(MakeCastTo(
                    _syntaxFactory.PointerType(context.Type.Get<TypeSyntax>(), SyntaxFactory.MakeToken(SyntaxKind.AsteriskToken)),
                    _syntaxFactory.PrefixUnaryExpression(SyntaxKind.AddressOfExpression,
                        SyntaxFactory.MakeToken(SyntaxKind.AmpersandToken),
                        context.Expr.Get<ExpressionSyntax>())));
            }
        }

        public override void ExitSizeOfExpression([NotNull] XP.SizeOfExpressionContext context)
        {
            context.Put(
                _syntaxFactory.SizeOfExpression(
                SyntaxFactory.MakeToken(SyntaxKind.SizeOfKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                context.Type.Get<TypeSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)));
        }

        public override void ExitTypeOfExpression([NotNull] XP.TypeOfExpressionContext context)
        {
            context.Put(MakeTypeOf(context.Type.Get<TypeSyntax>()));
        }

        public override void ExitDefaultExpression([NotNull] XP.DefaultExpressionContext context)
        {
            context.Put(MakeDefault(context.Type.Get<TypeSyntax>()));
        }

        public override void ExitAwaitExpression([NotNull] XP.AwaitExpressionContext context)
        {
            context.Put(_syntaxFactory.AwaitExpression(SyntaxFactory.MakeToken(SyntaxKind.AwaitKeyword), context.Expr.Get<ExpressionSyntax>()));
        }

        public override void ExitSelfExpression([NotNull] XP.SelfExpressionContext context)
        {
            context.Put(GenerateSelf());
        }

        public override void ExitSuperExpression([NotNull] XP.SuperExpressionContext context)
        {
            context.Put(GenerateSuper());
        }

        public override void ExitArgListExpression([NotNull] XP.ArgListExpressionContext context)
        {
            context.Put(_syntaxFactory.LiteralExpression(SyntaxKind.ArgListExpression, SyntaxFactory.MakeToken(SyntaxKind.ArgListKeyword)));
        }


        #endregion

        #region Arguments 
        static private ArgumentListSyntax _emptyArgs = null;
        protected ArgumentListSyntax EmptyArgumentList()
        {
            if (_emptyArgs == null)
            {
                lock (gate)
                {
                    if (_emptyArgs == null)
                    {
                        _emptyArgs = _syntaxFactory.ArgumentList(
                                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                                default(SeparatedSyntaxList<ArgumentSyntax>),
                                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
                    }
                }
            }
            return _emptyArgs;
        }

        static private BracketedArgumentListSyntax _emptyBracketedArgs = null;
        protected BracketedArgumentListSyntax EmptyBracketedArgumentList()
        {
            if (_emptyBracketedArgs == null)
            {
                lock (gate)
                {

                    if (_emptyBracketedArgs == null)
                    {
                        _emptyBracketedArgs = _syntaxFactory.BracketedArgumentList(
                        SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                        default(SeparatedSyntaxList<ArgumentSyntax>),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken));
                    }
                }
            }
            return _emptyBracketedArgs;
        }

        protected ArgumentListSyntax MakeArgumentList(params ArgumentSyntax[] items)
        {
            return _syntaxFactory.ArgumentList(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    MakeSeparatedList<ArgumentSyntax>(items),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
        }

        protected BracketedArgumentListSyntax MakeBracketedArgumentList(params ArgumentSyntax[] items)
        {
            return _syntaxFactory.BracketedArgumentList(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                    MakeSeparatedList<ArgumentSyntax>(items),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken));
        }


        public override void ExitBracketedArgumentList([NotNull] XP.BracketedArgumentListContext context)
        {
            var args = _pool.AllocateSeparated<ArgumentSyntax>();
            foreach (var argCtx in context._Args)
            {
                if (args.Count != 0)
                    args.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                args.Add(argCtx.Get<ArgumentSyntax>());
            }
            context.Put(_syntaxFactory.BracketedArgumentList(
                SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                args,
                SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)));
            _pool.Free(args);
        }

        public override void ExitUnnamedArgument([NotNull] XP.UnnamedArgumentContext context)
        {
            if (context.Expr == null)
            {
                context.Put(MakeArgument(GenerateMissingExpression(_options.Dialect == XSharpDialect.Core)));
                return;
            }
            context.Put(MakeArgument(context.Expr.Get<ExpressionSyntax>()));
        }

        public override void ExitArgumentList([NotNull] XP.ArgumentListContext context)
        {
            var args = _pool.AllocateSeparated<ArgumentSyntax>();
            var openParen = SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken);
            var closeParen = SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken);
            foreach (var argCtx in context._Args)
            {
                if (args.Count != 0)
                    args.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                args.Add(argCtx.Get<ArgumentSyntax>());
            }
            context.Put(_syntaxFactory.ArgumentList(openParen, args, closeParen));
            _pool.Free(args);
        }

        public override void ExitNamedArgument([NotNull] XP.NamedArgumentContext context)
        {
            if (context.Expr == null)
            {
                context.Put(MakeArgument(GenerateMissingExpression(_options.Dialect == XSharpDialect.Core)));
                return;
            }
            context.Put(_syntaxFactory.Argument(
                context.Name == null ? null : _syntaxFactory.NameColon(context.Name.Get<IdentifierNameSyntax>(), SyntaxFactory.MakeToken(SyntaxKind.ColonToken)),
                context.RefOut?.SyntaxKeyword(), context.Expr.Get<ExpressionSyntax>()));
        }

        #endregion

        #region Names and Identifiers
        public override void ExitQualifiedNameDot([NotNull] XP.QualifiedNameDotContext context)
        {
            context.Put(_syntaxFactory.QualifiedName(context.Left.Get<NameSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                context.Right.Get<SimpleNameSyntax>()));
        }

        public override void ExitSimpleOrAliasedNameDot([NotNull] XP.SimpleOrAliasedNameDotContext context)
        {
            context.Put(context.Name.Get<NameSyntax>());
        }

        public override void ExitQualifiedName([NotNull] XP.QualifiedNameContext context)
        {
            context.Put(_syntaxFactory.QualifiedName(context.Left.Get<NameSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                context.Right.Get<SimpleNameSyntax>()));
        }

        public override void ExitAliasQualifiedName([NotNull] XP.AliasQualifiedNameContext context)
        {
            context.Put(_syntaxFactory.AliasQualifiedName(context.Alias.Get<IdentifierNameSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.ColonColonToken),
                context.Right.Get<SimpleNameSyntax>()));
        }

        public override void ExitSimpleOrAliasedName([NotNull] XP.SimpleOrAliasedNameContext context)
        {
            context.Put(context.Name.Get<NameSyntax>());
        }

        public override void ExitGlobalQualifiedName([NotNull] XP.GlobalQualifiedNameContext context)
        {
            context.Put(_syntaxFactory.AliasQualifiedName(_syntaxFactory.IdentifierName(context.Global.SyntaxKeyword()),
                SyntaxFactory.MakeToken(SyntaxKind.ColonColonToken),
                context.Right.Get<SimpleNameSyntax>()));
        }

        public override void ExitIdentifierOrGenericName([NotNull] XP.IdentifierOrGenericNameContext context)
        {
            context.Put(context.Name.Get<SimpleNameSyntax>());
        }
        public override void ExitIdentifierName([NotNull] XP.IdentifierNameContext context)
        {
            context.Put(_syntaxFactory.IdentifierName(context.Id.Get<SyntaxToken>()));
        }


        public override void ExitIdentifierString([NotNull] XP.IdentifierStringContext context)
        {
            if (context.Token != null)
                context.Put(GenerateLiteral(context.Token.Text ));
            else if (context.XsToken != null)
                context.Put(GenerateLiteral(context.XsToken.Token.Text));
            else if (context.VnToken != null)
                context.Put(GenerateLiteral(context.VnToken.Token.Text));
            else if (context.XppToken != null)
                context.Put(GenerateLiteral(context.XppToken.Token.Text));
        }

        public override void ExitIdentifier([NotNull] XP.IdentifierContext context)
        {
            context.Put(context.Token?.SyntaxIdentifier()
                ?? context.XsToken?.Token.SyntaxIdentifier()
                ?? context.VnToken?.Token.SyntaxIdentifier()
                ?? context.XppToken?.Token.SyntaxIdentifier()); 
        }

        public override void ExitKeyword([NotNull] XP.KeywordContext context)
        {
            context.Put(context.KwXs?.Token.SyntaxKeywordIdentifier()
                ?? context.KwVn?.Token.SyntaxKeywordIdentifier()
                ?? context.KwVo?.Token.SyntaxKeywordIdentifier());
        }

        public override void ExitKeywordxs([NotNull] XP.KeywordxsContext context)
        {
            // caught by the keyword/identifier rule
        }

        public override void ExitKeywordvn([NotNull] XP.KeywordvnContext context)
        {
            // caught by the keyword/identifier rule
        }

        public override void ExitKeywordvo([NotNull] XP.KeywordvoContext context)
        {
            // caught by the keyword/identifier rule
        }

        public override void ExitSimpleName([NotNull] XP.SimpleNameContext context)
        {
            if (context.GenericArgList == null)
                context.Put(_syntaxFactory.IdentifierName(context.Id.Get<SyntaxToken>()));
            else
                context.Put(_syntaxFactory.GenericName(context.Id.Get<SyntaxToken>(), context.GenericArgList.Get<TypeArgumentListSyntax>()));
        }


        #endregion

        #region Data Types

        public override void ExitArrayOfType([NotNull] XP.ArrayOfTypeContext context)
        {
            context.Put(NotInDialect(_objectType, "ARRAY OF <type>"));
        }

        public override void ExitPtrDatatype([NotNull] XP.PtrDatatypeContext context)
        {
            context.Put(
                _syntaxFactory.PointerType(context.TypeName.Get<TypeSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.AsteriskToken)));
        }

        public override void ExitArrayDatatype([NotNull] XP.ArrayDatatypeContext context)
        {
            var ranks = _pool.Allocate<ArrayRankSpecifierSyntax>();
            foreach (var rankCtx in context._Ranks)
            {
                ranks.Add(rankCtx.Get<ArrayRankSpecifierSyntax>());
            }
            context.Put(_syntaxFactory.ArrayType(context.TypeName.Get<TypeSyntax>(), ranks));
            _pool.Free(ranks);
        }

        public override void ExitArrayRank([NotNull] XP.ArrayRankContext context)
        {
            var sizes = _pool.AllocateSeparated<ExpressionSyntax>();
            var omittedArraySizeExpressionInstance = _syntaxFactory.OmittedArraySizeExpression(SyntaxFactory.MakeToken(SyntaxKind.OmittedArraySizeExpressionToken));
            foreach (var comma in context._Commas)
            {
                sizes.Add(omittedArraySizeExpressionInstance);
                sizes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
            }
            sizes.Add(omittedArraySizeExpressionInstance);
            context.Put(_syntaxFactory.ArrayRankSpecifier(
                SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                sizes,
                SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)));
            _pool.Free(sizes);
        }

        public override void ExitSimpleDatatype([NotNull] XP.SimpleDatatypeContext context)
        {
            context.Put(context.TypeName.Get<TypeSyntax>());
        }

        public override void ExitNullableDatatype([NotNull] XP.NullableDatatypeContext context)
        {
            context.Put(_syntaxFactory.NullableType(context.TypeName.Get<TypeSyntax>(), SyntaxFactory.MakeToken(SyntaxKind.QuestionToken)));
        }

        public override void ExitTypeName([NotNull] XP.TypeNameContext context)
        {
            if (context.NativeType != null)
                context.Put(context.NativeType.Get<TypeSyntax>());
            else if (context.XType != null)
                context.Put(context.XType.Get<TypeSyntax>());
            else if (context.Name != null)
                context.Put(context.Name.Get<NameSyntax>());
        }

        #endregion

        #region Literal Expressions
        public override void ExitLiteralExpression([NotNull] XP.LiteralExpressionContext context)
        {
            context.Put(context.Literal.Get<ExpressionSyntax>());
        }

        public override void ExitLiteralArrayExpression([NotNull] XP.LiteralArrayExpressionContext context)
        {
            context.Put(context.LiteralArray.Get<ExpressionSyntax>());
        }

        private ExpressionSyntax CreateInterPolatedStringExpression([NotNull] XP.LiteralValueContext context)
        {
            // Use the C# parser to do the parsing of the Interpolated strings.
            // SELF: Syntax will be translated to this.
            // All other property syntax needs use the '.' as separator
            IToken t = context.Token;
            var xsharpText = t.Text;
            int nIndex = xsharpText.IndexOf("{self:", StringComparison.OrdinalIgnoreCase);
            while (nIndex >= 0)
            {
                xsharpText = xsharpText.Substring(0, nIndex) + "{this." + xsharpText.Substring(nIndex + 6);
                nIndex = xsharpText.IndexOf("{self:", StringComparison.OrdinalIgnoreCase);
            }
            string originalText;
            if (xsharpText.IndexOf('"') == 1)           // i"..."
            {
                //C# verbatim, X# normal
                originalText = "$@" + xsharpText.Substring(1);
            }
            else                                      // ei"..." or ie"..."
            {
                // C# normal, X# extended strings
                originalText = "$" + xsharpText.Substring(2);
            }
            ExpressionSyntax result;
            using (var lexer = new Lexer(Text.SourceText.From(originalText), this._options, allowPreprocessorDirectives: false))
            {
                using (var parser = new LanguageParser(lexer, oldTree: null, changes: null, cancellationToken: default(CancellationToken)))
                {
                    result = parser.ParseInterpolatedStringToken();
                }
            }
            return result;
        }

        public override void ExitLiteralValue([NotNull] XP.LiteralValueContext context)
        {
            string replacement = null;
            if (context.Token.Type == XP.INTERPOLATED_STRING_CONST)
            {
                context.Put(CreateInterPolatedStringExpression(context));
            }
            else
            {
                if (context.Token.Type == XP.STRING_CONST && context.Token.Text.StartsWith("\"__"))
                {
                    switch (context.Token.Text.ToLowerInvariant())
                    {
                        case "\"__entity__\"":
                            replacement = GetEntityName(false);
                            break;
                        case "\"__function__\"":
                            replacement = GetEntityName(false,true);
                            break;
                        case "\"__sig__\"":
                            replacement = GetEntityName(true);
                            break;
                        case "\"__functions__\"":
                            replacement = GlobalClassName;
                            break;
                        default:
                            break;
                    }
                }

                if (!String.IsNullOrEmpty(replacement))
                {
                    context.Put(_syntaxFactory.LiteralExpression(context.Token.ExpressionKindLiteral(),
                        SyntaxToken.WithValue(SyntaxKind.StringLiteralToken, replacement, replacement)));
                }
                else
                {
                    context.Put(GenerateLiteral(context.Token));
                    if (context.Token.Type == XP.INCOMPLETE_STRING_CONST)
                    {
                        var litExpr = context.Get<LiteralExpressionSyntax>();
                        var diag = new SyntaxDiagnosticInfo(ErrorCode.ERR_UnterminatedStringLit);
                        context.Put(litExpr.WithAdditionalDiagnostics(diag));
                    }
                }
            }
        }
        public override void ExitLiteralArray([NotNull] XP.LiteralArrayContext context)
        {
            TypeSyntax type = null;
            ExpressionSyntax expr = null;
            // detect typed arrays.
            // <LONG> {...} indicates an array of type LONG
            // when no type is specified and the dialect VO or Vulcan the type is USUAL
            if (context.Type != null)
            {
                type = context.Type.Get<TypeSyntax>();
            }
            SeparatedSyntaxList<ExpressionSyntax> exprs;
            if ((context._Elements?.Count ?? 0) > 0)
            {
                //
                var l = _pool.AllocateSeparated<ExpressionSyntax>();
                foreach (var item in context._Elements)
                {
                    if (l.Count > 0)
                        l.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                    if (item.Expr != null)
                        l.Add(item.Expr.Get<ExpressionSyntax>());
                    else
                        l.Add((ExpressionSyntax)NotInDialect(GenerateLiteral(false), "omitting (typed) array elements"));
                }
                exprs = l.ToList();
                _pool.Free(l);
            }
            else
            {
                exprs = default(SeparatedSyntaxList<ExpressionSyntax>);
            }

            var initializer = _syntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression,
                SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                exprs,
                SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));
            if (type != null)
            {
                expr = _syntaxFactory.ArrayCreationExpression(SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                    _syntaxFactory.ArrayType(type,
                    MakeList(_syntaxFactory.ArrayRankSpecifier(
                        SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                        MakeSeparatedList<ExpressionSyntax>(
                            _syntaxFactory.OmittedArraySizeExpression(SyntaxFactory.MakeToken(SyntaxKind.OmittedArraySizeExpressionToken))),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)))),
                    initializer);
            }
            else
            {
                expr = _syntaxFactory.ImplicitArrayCreationExpression(SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                    SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                    EmptyList(),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken),
                    initializer);
                if (!isNestedArray(context))
                {
                    expr = expr.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_UntypedArrayNotAvailableInDialect, _options.Dialect.ToString()));
                }
            }
            context.Put<ExpressionSyntax>(expr);
        }

        private bool isNestedArray(XP.LiteralArrayContext context)
        {
            var parent = context.Parent as ParserRuleContext;
            while (parent != null)
            {
                if (parent is XP.LiteralArrayContext)
                    return true;
                if (parent is XP.StatementContext)
                    return false;
                parent = parent.Parent as ParserRuleContext;
            }
            return false;
        }
        public override void ExitArrayElement([NotNull] XP.ArrayElementContext context)
        {
            if (context.Expr != null)
                context.Put(context.Expr.Get<ExpressionSyntax>());
            return;
        }



        #endregion

          
        #region Anonymous Types
        public override void ExitAnonTypeExpression([NotNull] XP.AnonTypeExpressionContext context)
        {
            context.Put(context.AnonType.Get<AnonymousObjectCreationExpressionSyntax>());
        }

 
        public override void ExitAnonType([NotNull] XP.AnonTypeContext context)
        {
            context.Put(_syntaxFactory.AnonymousObjectCreationExpression(
                SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                MakeSeparatedList<AnonymousObjectMemberDeclaratorSyntax>(context._Members),
                SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken)));
        }

        private bool isAnonymousTypeExpression(ExpressionSyntax expr)
        {
            // COpyied from the Roslyn Language Parser
            while (true)
            {
                switch (expr.Kind)
                {
                    case SyntaxKind.QualifiedName:
                        expr = ((QualifiedNameSyntax)expr).Right;
                        continue;
                    case SyntaxKind.ConditionalAccessExpression:
                        expr = ((ConditionalAccessExpressionSyntax)expr).WhenNotNull;
                        if (expr.Kind == SyntaxKind.MemberBindingExpression)
                        {
                            return true;
                        }

                        continue;
                    case SyntaxKind.IdentifierName:
                    case SyntaxKind.SimpleMemberAccessExpression:
                        return true;
                    default:
                        return false;
                }
            }
        }
        public override void ExitAnonMember([NotNull] XP.AnonMemberContext context)
        {
            NameEqualsSyntax nameEquals = null;
            var expr = context.Expr.Get<ExpressionSyntax>();
            if (context.Name != null)
            {
                nameEquals = _syntaxFactory.NameEquals(context.Name.Get<IdentifierNameSyntax>(), SyntaxFactory.MakeToken(SyntaxKind.EqualsToken));
            }
            var amd = _syntaxFactory.AnonymousObjectMemberDeclarator(nameEquals, expr);
            {
                if (nameEquals == null && !isAnonymousTypeExpression(expr))
                {
                    amd = amd.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_InvalidAnonymousTypeMemberDeclarator));
                }
            }
            context.Put(amd);
        }
        #endregion

        #region Codeblocks
        public override void ExitCodeblockExpression([NotNull] XP.CodeblockExpressionContext context)
        {
            if (context.CbExpr != null)
                context.Put(context.CbExpr.Get<ExpressionSyntax>());
            else if (context.AnoExpr != null)
                context.Put(context.AnoExpr.Get<ExpressionSyntax>());
        }

        public override void ExitLambdaParameterList([NotNull] XP.LambdaParameterListContext context)
        {
            ParameterListSyntax paramList;
            if (context.ImplicitParams != null)
                paramList = context.ImplicitParams.Get<ParameterListSyntax>();
            else
                paramList = context.ExplicitParams.Get<ParameterListSyntax>();
            context.Put(paramList);
        }

        public override void ExitCodeblockCode([NotNull] XP.CodeblockCodeContext context)
        {
            CSharpSyntaxNode  block;
            // Convert everything to a stmt like block
            // so it is easier to fix Void expressions as last expression in the list
            if (context.Expr != null)
            {
                block = context.Expr?.Get<ExpressionSyntax>();
            }
            else
            {
               block = context.StmtBlk?.Get<BlockSyntax>()
                        ?? context.ExprList?.Get<BlockSyntax>()
                        ?? MakeBlock(_syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            }
            // set debugger sequence point to first statement or first expression
            if (context.StmtBlk != null && context.StmtBlk._Stmts.Count > 0)
            {
                context.SetSequencePoint(context.StmtBlk._Stmts[0].Stop);
            }
            else if (context.ExprList != null && context.ExprList._Exprs.Count > 0)
            {
                context.SetSequencePoint(context.ExprList._Exprs[0].Stop);
            }
            context.Put(block);
        }

        public override void ExitCodeblock([NotNull] XP.CodeblockContext context)
        {
            ParameterListSyntax paramList = context.LambdaParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList();
            if (context.lambda == null &&
                context.LambdaParamList?.ExplicitParams != null)
            {
                paramList = paramList.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_CodeBlockWithTypeParameters));
                
            }
            if (context.lambda != null )
            {
                bool bWarn;
                if (context.Or == null && context.P1 == null && context.P2 == null)
                {
                    // correct lambda
                    bWarn = false;
                }
                else if (context.Or != null)        // no parameters with 2 pipes  || =>
                {
                    bWarn = true;
                }
                else if (context.P1 != null && context.P2 != null) // params, or pipes separated with space | a | => or | | =>
                {
                    bWarn = true;
                }
                else // one of the pipes is missing
                {
                    bWarn = false;
                    if (context.P1 == null)
                    {
                        paramList = paramList.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_SyntaxError,"Opening Pipe ('|') character"));
                    }
                    if (context.P2 == null)
                    {
                        paramList = paramList.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_SyntaxError, "Closing Pipe ('|') character"));
                    }
                }
                if (bWarn)
                {
                    paramList = paramList.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.WRN_LamdaExpressionWithPipes));
                }
            }

            var node = _syntaxFactory.ParenthesizedLambdaExpression(
                asyncKeyword: null,
                parameterList: paramList,
                arrowToken: SyntaxFactory.MakeToken(SyntaxKind.EqualsGreaterThanToken),
                body: context.Code.Get<CSharpSyntaxNode>()
                );
            context.Put(node);

        }

        public override void ExitCodeblockParamList([NotNull] XP.CodeblockParamListContext context)
        {
            var @params = _pool.AllocateSeparated<ParameterSyntax>();
            foreach (var idCtx in context._Ids)
            {
                if (@params.Count > 0)
                    @params.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                SyntaxListBuilder<AttributeListSyntax> attributeLists = _pool.Allocate<AttributeListSyntax>();
                SyntaxListBuilder modifiers = _pool.Allocate();
                @params.Add(_syntaxFactory.Parameter(
                    attributeLists: attributeLists,
                    modifiers: modifiers.ToList<SyntaxToken>(),
                    type: null, // TODO: (grammar) codeblock param type
                    identifier: idCtx.Get<SyntaxToken>(),
                    @default: null));
                _pool.Free(attributeLists);
                _pool.Free(modifiers);
            }
            context.Put(_syntaxFactory.ParameterList(
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                @params,
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)));
            _pool.Free(@params);
        }

        public override void ExitCodeblockExprList([NotNull] XP.CodeblockExprListContext context)
        {
            context.Put(MakeBlock(MakeList<StatementSyntax>(
                from ctx in context._Exprs select _syntaxFactory.ExpressionStatement(ctx.Get<ExpressionSyntax>(), SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)),
                GenerateReturn(context.ReturnExpr.Get<ExpressionSyntax>())
                )));
        }
        public override void ExitAnonymousfunctionParameterModifier([NotNull] XP.AnonymousfunctionParameterModifierContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                if (m.Type != XP.AS)
                    modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            context.PutList(modifiers.ToList<SyntaxToken>());
            _pool.Free(modifiers);
        }

        public override void ExitExplicitAnonymousFunctionParameter([NotNull] XP.ExplicitAnonymousFunctionParameterContext context)
        {
            var attributeList = EmptyList<AttributeListSyntax>();
            var type = context.Type.Get<TypeSyntax>();
            var par = _syntaxFactory.Parameter(
                attributeLists: attributeList,
                modifiers: context.Mod.GetList<SyntaxToken>() ,
                type: type,
                identifier: context.Id.Get<SyntaxToken>(),
                @default: null 
                );
            context.Put(par);
        }
    
        public override void ExitExplicitAnonymousFunctionParamList([NotNull] XP.ExplicitAnonymousFunctionParamListContext context)
        {
            var @params = _pool.AllocateSeparated<ParameterSyntax>();
            foreach (XP.ExplicitAnonymousFunctionParameterContext param in context._Params)
            {
                if (@params.Count > 0)
                    @params.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));

                @params.Add(param.Get<ParameterSyntax>());
            }
            context.Put(_syntaxFactory.ParameterList(
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                @params,
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)));
            _pool.Free(@params);
        }


        public override void ExitAnonymousMethodExpression([NotNull] XP.AnonymousMethodExpressionContext context)
        {
            var block = context.Code.CsNode as BlockSyntax;
            if (block == null)
            {
                block = MakeBlock(GenerateExpressionStatement(context.Code.Get<ExpressionSyntax>()));
            }
            var ame = _syntaxFactory.AnonymousMethodExpression(
                context.Async?.SyntaxKeyword(),
                context.Delegate.SyntaxKeyword(),
                context.ParamList.Get<ParameterListSyntax>(),
                block);
            context.Put(ame);


        }

        #endregion

        #region LINQ
        public override void ExitQueryExpression([NotNull] XP.QueryExpressionContext context)
        {
            context.Put(context.Query.Get<QueryExpressionSyntax>());
        }

        public override void ExitLinqQuery([NotNull] XP.LinqQueryContext context)
        {
            context.Put(_syntaxFactory.QueryExpression(
                context.From.Get<FromClauseSyntax>(),
                context.Body.Get<QueryBodySyntax>()
                ));
        }

        public override void ExitFromClause([NotNull] XP.FromClauseContext context)
        {
            context.Put(_syntaxFactory.FromClause(SyntaxFactory.MakeToken(SyntaxKind.FromKeyword),
                context.Type?.Get<TypeSyntax>(),
                context.Id.Get<SyntaxToken>(),
                SyntaxFactory.MakeToken(SyntaxKind.InKeyword),
                context.Expr.Get<ExpressionSyntax>()
                ));
        }

        public override void ExitQueryBody([NotNull] XP.QueryBodyContext context)
        {
            context.Put(_syntaxFactory.QueryBody(
                MakeList<QueryClauseSyntax>(context._Bodyclauses),
                context.SorG.Get<SelectOrGroupClauseSyntax>(),
                context.Continuation?.Get<QueryContinuationSyntax>()
                ));
        }

        public override void ExitFromBodyClause([NotNull] XP.FromBodyClauseContext context)
        {
            context.Put(context.From.Get<FromClauseSyntax>());
        }

        public override void ExitLetClause([NotNull] XP.LetClauseContext context)
        {
            context.Put(_syntaxFactory.LetClause(
                SyntaxFactory.MakeToken(SyntaxKind.LetKeyword),
                context.Id.Get<SyntaxToken>(),
                SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                context.Expr.Get<ExpressionSyntax>()
                ));
        }

        public override void ExitWhereClause([NotNull] XP.WhereClauseContext context)
        {
            context.Put(_syntaxFactory.WhereClause(
                SyntaxFactory.MakeToken(SyntaxKind.WhereKeyword),
                context.Expr.Get<ExpressionSyntax>()
                ));
        }

        public override void ExitJoinClause([NotNull] XP.JoinClauseContext context)
        {
            context.Put(_syntaxFactory.JoinClause(
                SyntaxFactory.MakeToken(SyntaxKind.JoinKeyword),
                context.Type?.Get<TypeSyntax>(),
                context.Id.Get<SyntaxToken>(),
                SyntaxFactory.MakeToken(SyntaxKind.InKeyword),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.OnKeyword),
                context.OnExpr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.EqualsKeyword),
                context.EqExpr.Get<ExpressionSyntax>(),
                context.Into?.Get<JoinIntoClauseSyntax>()
                ));
        }

        public override void ExitJoinIntoClause([NotNull] XP.JoinIntoClauseContext context)
        {
            context.Put(_syntaxFactory.JoinIntoClause(
                SyntaxFactory.MakeToken(SyntaxKind.IntoKeyword),
                context.Id.Get<SyntaxToken>()));
        }

        public override void ExitOrderbyClause([NotNull] XP.OrderbyClauseContext context)
        {
            context.Put(_syntaxFactory.OrderByClause(
                SyntaxFactory.MakeToken(SyntaxKind.OrderByKeyword),
                MakeSeparatedList<OrderingSyntax>(context._Orders)
                ));
        }

        public override void ExitOrdering([NotNull] XP.OrderingContext context)
        {
            SyntaxToken direction;
            SyntaxKind kind;
            if (context.Direction != null && context.Direction.Type == XP.DESCENDING)
            {
                direction = SyntaxFactory.MakeToken(SyntaxKind.DescendingKeyword);
                kind = SyntaxKind.DescendingOrdering;
            }
            else
            {
                direction = SyntaxFactory.MakeToken(SyntaxKind.AscendingKeyword);
                kind = SyntaxKind.AscendingOrdering;
            }
            context.Put(_syntaxFactory.Ordering(kind, context.Expr.Get<ExpressionSyntax>(), direction));

        }

        public override void ExitSelectClause([NotNull] XP.SelectClauseContext context)
        {
            context.Put(_syntaxFactory.SelectClause(
                SyntaxFactory.MakeToken(SyntaxKind.SelectKeyword),
                context.Expr.Get<ExpressionSyntax>()
                ));
        }

        public override void ExitGroupClause([NotNull] XP.GroupClauseContext context)
        {
            context.Put(_syntaxFactory.GroupClause(
                SyntaxFactory.MakeToken(SyntaxKind.GroupKeyword),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.ByKeyword),
                context.ByExpr.Get<ExpressionSyntax>()
                ));
        }

        public override void ExitQueryContinuation([NotNull] XP.QueryContinuationContext context)
        {
            context.Put(_syntaxFactory.QueryContinuation(
                SyntaxFactory.MakeToken(SyntaxKind.IntoKeyword),
                context.Id.Get<SyntaxToken>(),
                context.Body.Get<QueryBodySyntax>()
                ));
        }

        #endregion
        #region Type Names
        public override void ExitNativeType([NotNull] XP.NativeTypeContext context)
        {
            switch (context.Token.Type)
            {
                case XP.PTR:
                    context.Put(_syntaxFactory.PointerType(VoidType(), SyntaxFactory.MakeToken(SyntaxKind.AsteriskToken)));
                    break;
                case XP.DYNAMIC:
                    context.Put(_syntaxFactory.IdentifierName(context.Token.SyntaxIdentifier()));
                    break;
                case XP.DATETIME:
                    context.Put(_syntaxFactory.IdentifierName(context.Token.SyntaxIdentifier()));
                    break;
                default:
                    context.Put(_syntaxFactory.PredefinedType(context.Token.SyntaxNativeType()));
                    break;
            }
        }
        public override void ExitXbaseType([NotNull] XP.XbaseTypeContext context)
        {
            var type = (TypeSyntax)NotInDialect(_objectType, context.Token.Text);
            context.Put(type);
        }

        public override void ExitUsualTypeNameExpression([NotNull] XP.UsualTypeNameExpressionContext context)
        {
            context.Put(context.Name.Get<LiteralExpressionSyntax>());
        }

        public override void ExitTypeExpression([NotNull] XP.TypeExpressionContext context)
        {
            context.Put(context.Type.Get<TypeSyntax>());
        }

        public override void ExitUsualTypeName([NotNull] XP.UsualTypeNameContext context)
        {
            /*
             * Please note that these numbers must match the BT_  types in basetype.h from VO:
             * Vulcan has added 2 numbers for 64 bit types INT64 (22) and UINT64 (23)
             * We have added 4 numbers for CHAR (24), DYNAMIC (25), DATETIME(26) and DECIMAL (27)
             * The Vulcan Runtime has an internal UsualType Enum that has matching values.
             *   #define BT_VOID         0       // == NIL
             *   #define BT_LONG         1       // signed long
             *   #define BT_DATE         2       //
             *   #define BT_FLOAT        3       // internal real format
             *   #define BT_FIXED        4       // internal fixed format
             *   #define BT_ARRAY        5       // array (ptr)
             *   #define BT_OP           6       // object pointer
             *   #define BT_STR          7       // string
             *   #define BT_LOGIC        8
             *   #define BT_CODE         9       // code block
             *   #define BT_SYMBOL       10      // atom nr in symbol atom table
             *
             *   #define BT_BYTE         11      // byte
             *   #define BT_INT          12      // signed int
             *   #define BT_WORD         13      // word
             *   #define BT_DWORD        14      // dword
             *   #define BT_REAL4        15      // C float
             *   #define BT_REAL8        16      // C double
             *   #define BT_PSZ          17      // PSZ
             *   #define BT_PTR          18      // raw ptr
             *   #define BT_POLY         19      // polymorphic
             * */
            int type;
            if (context.XType != null)
                type = context.XType.Token.Type;
            else
                type = context.NativeType.Token.Type;
            switch (type)
            {
                case XSharpLexer.ARRAY:
                    context.Put(GenerateLiteral(5));
                    break;
                case XSharpLexer.BYTE:
                    context.Put(GenerateLiteral(11));
                    break;
                case XSharpLexer.CHAR:
                    context.Put(GenerateLiteral(24));         // New in XSharp
                    break;
                case XSharpLexer.CODEBLOCK:
                    context.Put(GenerateLiteral(9));
                    break;
                case XSharpLexer.DATE:
                    context.Put(GenerateLiteral(2));
                    break;
                case XSharpLexer.DATETIME:
                    context.Put(GenerateLiteral(26));
                    break;
                case XSharpLexer.DECIMAL:
                    context.Put(GenerateLiteral(27));
                    break;
                case XSharpLexer.DYNAMIC:
                    context.Put(GenerateLiteral(25));
                    break;
                case XSharpLexer.DWORD:
                    context.Put(GenerateLiteral(14));
                    break;
                case XSharpLexer.FLOAT:
                    context.Put(GenerateLiteral(3));
                    break;
                case XSharpLexer.INT:
                case XSharpLexer.LONGINT:
                    context.Put(GenerateLiteral(1));
                    break;
                case XSharpLexer.INT64:
                    context.Put(GenerateLiteral(22));     // New in Vulcan
                    break;
                case XSharpLexer.LOGIC:
                    context.Put(GenerateLiteral(8));
                    break;
                case XSharpLexer.OBJECT:
                    context.Put(GenerateLiteral(6));
                    break;
                case XSharpLexer.PSZ:
                    context.Put(GenerateLiteral(17));
                    break;
                case XSharpLexer.PTR:
                    context.Put(GenerateLiteral(18));
                    break;
                case XSharpLexer.REAL4:
                    context.Put(GenerateLiteral(15));
                    break;
                case XSharpLexer.REAL8:
                    context.Put(GenerateLiteral(16));
                    break;
                case XSharpLexer.SHORTINT:
                    context.Put(GenerateLiteral(12));
                    break;
                case XSharpLexer.STRING:
                    context.Put(GenerateLiteral(7));
                    break;
                case XSharpLexer.SYMBOL:
                    context.Put(GenerateLiteral(10));
                    break;
                case XSharpLexer.UINT64:
                    context.Put(GenerateLiteral(23));         // New in Vulcan
                    break;
                case XSharpLexer.USUAL:
                    context.Put(GenerateLiteral(19));
                    break;
                case XSharpLexer.VOID:
                    context.Put(GenerateLiteral(0));
                    break;
                case XSharpLexer.WORD:
                    context.Put(GenerateLiteral(13));
                    break;
                default:
                    context.Put(GenerateLiteral(0).WithAdditionalDiagnostics(
                        new SyntaxDiagnosticInfo(ErrorCode.ERR_UnknownLiteralTypeName, context.GetText())));
                    break;
            }
        }
        #endregion  

        #region Workareas
        public override void ExitFielddecl([NotNull] XP.FielddeclContext context)
        {
            context.Put(NotInDialect("FIELD statement"));
            return;
        }

        private ExpressionSyntax NoAlias()
        {
            return (ExpressionSyntax)NotInDialect(GenerateLiteral("alias"), "ALIAS(->) operator");
        }
        public override void ExitAliasedExpr([NotNull] XP.AliasedExprContext context)
        {
            context.Put(NoAlias());
            return;
        }
        public override void ExitAliasedMemvar([NotNull] XP.AliasedMemvarContext context)
        {
            context.Put(NoAlias());
            return;
        }
        public override void ExitAliasedField([NotNull] XP.AliasedFieldContext context)
        {
            context.Put(NoAlias());
            return;
        }

        #endregion
        public override void ExitMacro([NotNull] XP.MacroContext context)
        {
            context.Put((ExpressionSyntax)NotInDialect(GenerateLiteral("macro"), "MACRO compiler"));
            return;
        }
        public override void ExitMacroName([NotNull] XP.MacroNameContext context)
        {
            context.Put((ExpressionSyntax)NotInDialect(GenerateLiteral("macro"), "MACRO compiler"));
            return;
        }
        public override void ExitAccessMemberLate([NotNull] XP.AccessMemberLateContext context)
        {
            context.Put((ExpressionSyntax)NotInDialect(GenerateLiteral("value"), "Late bound member access"));
            return;
        }
        public override void ExitAccessMemberLateName([NotNull] XP.AccessMemberLateNameContext context)
        {
            context.Put((ExpressionSyntax)NotInDialect(GenerateLiteral("value"), "Late bound member access"));
            return;
        }

        #region  object and collection initializers
        public override void ExitObjectinitializer([NotNull] XP.ObjectinitializerContext context)
        {
            var objinit = _syntaxFactory.InitializerExpression(
                SyntaxKind.ObjectInitializerExpression,
                SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                MakeSeparatedList<ExpressionSyntax>(context._Members),
                SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));
            context.Put<InitializerExpressionSyntax>(objinit);
        }
        public override void ExitCollectioninitializer([NotNull] XP.CollectioninitializerContext context)
        {
            var collinit = _syntaxFactory.InitializerExpression(
                SyntaxKind.CollectionInitializerExpression,
                SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                MakeSeparatedList<ExpressionSyntax>(context._Members),
                SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));
            context.Put<InitializerExpressionSyntax>(collinit);
        }
        public override void ExitMemberinitializer([NotNull] XP.MemberinitializerContext context)
        {
            var memberinit = _syntaxFactory.AssignmentExpression(
                SyntaxKind.SimpleAssignmentExpression,
                context.Name.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                context.Expr.Get<ExpressionSyntax>());
            context.Put<ExpressionSyntax>(memberinit);
        }
        public override void ExitObjectOrCollectioninitializer([NotNull] XP.ObjectOrCollectioninitializerContext context)
        {
            if (context.ObjInit != null)
                context.Put<ExpressionSyntax>(context.ObjInit.Get<ExpressionSyntax>());
            else
                context.Put<ExpressionSyntax>(context.CollInit.Get<ExpressionSyntax>());

        }
        public override void ExitInitializervalue([NotNull] XP.InitializervalueContext context)
        {
            if (context.Expr != null)
                context.Put<ExpressionSyntax>(context.Expr.Get<ExpressionSyntax>());
            else
                context.Put<ExpressionSyntax>(context.Init.Get<ExpressionSyntax>());
        }
        #endregion
        #endregion

    }
}

