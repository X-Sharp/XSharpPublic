//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

#nullable disable

using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    using System.Diagnostics;
    using System.Xml.Linq;
    using Microsoft.CodeAnalysis.Syntax.InternalSyntax;
    using Roslyn.Utilities;

    [DebuggerDisplay("{Name}")]
    public class XppClassInfo
    {
        internal IList<XppDeclaredMethodInfo> Methods { get; set; }
        private IList<string> Properties { get; set; }
        internal IList<XP.XppmethodContext> ExternalMethods { get; set; }
        internal string Name { get; set; }
        internal int CurrentVisibility { get; set; }
        internal XP.XppclassContext Entity { get; set; }
        internal XppClassInfo()
        {
            Methods = new List<XppDeclaredMethodInfo>();
            ExternalMethods = new List<XP.XppmethodContext>();
            Properties = new List<string>();
            CurrentVisibility = XP.HIDDEN;
        }
        internal XppDeclaredMethodInfo FindMethod(string name)
        {
            foreach (var decl in Methods)
            {
                if (XSharpString.Equals(name, decl.Name))
                    return decl;
                if (decl.IsProperty)
                {
                    if (XSharpString.Equals(name, decl.AccessMethod))
                        return decl;
                    if (XSharpString.Equals(name, decl.AssignMethod))
                        return decl;
                }
            }
            return null;
        }
        internal void AddProperty(string name)
        {
            name = name.ToLower();
            if (!Properties.Contains(name))
            {
                Properties.Add(name);
            }
        }
        internal bool HasProperty(string name)
        {
            name = name.ToLower();
            return Properties.Contains(name);
        }
    }
    [DebuggerDisplay("{Name}")]
    public class XppDeclaredMethodInfo
    {
        internal string Name { get; set; } = null;
        internal int Visibility { get; set; } = XP.HIDDEN;
        internal bool IsImplemented => Entity != null;
        internal XP.IEntityContext Entity { get; set; } = null;
        internal XP.IEntityContext SetEntity { get; set; } = null;
        internal XP.DatatypeContext DeclaredType { get; set; } = null;
        internal XppClassInfo Parent { get; set; } = null;
        internal bool Inline { get; set; } = false;
        internal bool IsProperty { get; set; } = false;
        internal bool HasVarName { get; set; } = false;
        internal string AccessMethod { get; set; } = null;
        internal string AssignMethod { get; set; } = null;
        internal XSharpParserRuleContext Declaration { get; set; } = null;
        internal IList<IToken> ModifierTokens { get; set; } = null;
        internal XP.AttributesContext Attributes { get; set; } = null;

        internal XppDeclaredMethodInfo()
        {
        }
        internal bool IsSync
        {
            get
            {
                if (Declaration is XP.XppdeclareMethodContext declmeth)
                {
                    var tokens = declmeth.Modifiers?._Tokens;
                    return tokens != null && tokens.Any(x => x.Type == XP.SYNC);
                }

                return false;
            }
        }
    }

    internal class XSharpTreeTransformationXPP : XSharpTreeTransformationRT
    {
        internal IList<XppClassInfo> _classes;
        internal Stack<IList<XppClassInfo>> _classstack;
        internal XppClassInfo _currentClass = null;

        protected override XSharpTreeTransformationCore CreateWalker(XSharpParser parser)
        {
            return new XSharpTreeTransformationXPP(parser, _options, _pool, _syntaxFactory, _fileName);
        }
        public XSharpTreeTransformationXPP(XSharpParser parser, CSharpParseOptions options, SyntaxListPool pool,
                    ContextAwareSyntax syntaxFactory, string fileName) :
                    base(parser, options, pool, syntaxFactory, fileName)
        {
            _classes = new List<XppClassInfo>();
            _classstack = new Stack<IList<XppClassInfo>>();
            _currentClass = null;
            _entryPoint = "Main";
        }
        private static SyntaxToken DecodeVisibility(int vis)
        {
            return vis switch
            {
                XP.EXPORTED or XP.PUBLIC => SyntaxFactory.MakeToken(SyntaxKind.PublicKeyword, "EXPORTED"),
                XP.PROTECTED => SyntaxFactory.MakeToken(SyntaxKind.ProtectedKeyword, "PROTECTED"),
                XP.INTERNAL => SyntaxFactory.MakeToken(SyntaxKind.InternalKeyword, "INTERNAL"),
                _ => SyntaxFactory.MakeToken(SyntaxKind.PrivateKeyword, "HIDDEN"),
            };
        }

        private XppClassInfo FindClassInfo(string name)
        {
            foreach (var info in _classes)
            {
                if (XSharpString.Compare(info.Name, name) == 0)
                {
                    return info;
                }
            }
            return null;
        }
        private XppClassInfo AddClassInfo(string name)
        {
            var classinfo = new XppClassInfo() { Name = name };
            _classes.Add(classinfo);
            return classinfo;
        }
        #region Class and ClassMembers
        public override void EnterXppclass([NotNull] XP.XppclassContext context)
        {
            ClassEntities.Push(CreateClassEntities());
            var info = AddClassInfo(context.Id.GetText());
            info.Entity = context;
            _currentClass = info;
        }
        public override void ExitXppclass([NotNull] XP.XppclassContext context) 
        {
            context.SetSequencePoint(context.C, context.e.Stop);
            var members = _pool.Allocate<MemberDeclarationSyntax>();
            var generated = ClassEntities.Pop();
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility();
            XP.DatatypeContext basetype = null;
            if (generated.Members.Count > 0)                // inline methods, properties and fields
            {
                members.AddRange(generated.Members);
            }
            if (context.Modifiers != null && context.Modifiers._Tokens.Any(t => t.Type == XP.STATIC))
            {
                context.TypeData.HasStatic = true;
            }
            // check if all declared methods have been implemented
            // and if so, then add the methods to the members list
            string className = context.Id.GetText();
            if (context._BaseTypes.Count == 1)
            {
                basetype = context._BaseTypes[0];
            }
            XppClassInfo thisClass = FindClassInfo(className);
            if (thisClass == null || thisClass.Entity != context)
            {
                ParseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_ParserError, "Could not locate ClassInfo for class " + className));
                return;
            }

            generated.Free();
            var baseTypes = _pool.AllocateSeparated<BaseTypeSyntax>();
            var baseType = basetype.Get<TypeSyntax>();
            if (baseType != null)
            {
                baseTypes.Add(_syntaxFactory.SimpleBaseType(baseType));
            }
            else if (_options.HasOption(CompilerOption.Xpp1, context, PragmaOptions))
            {
                baseTypes.Add(_syntaxFactory.SimpleBaseType(GenerateQualifiedName(XSharpQualifiedTypeNames.XppAbstract)));
            }
            foreach (var iCtx in context._Implements)
            {
                if (baseTypes.Count > 0)
                    baseTypes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                baseTypes.Add(_syntaxFactory.SimpleBaseType(iCtx.Get<TypeSyntax>()));
            }

            MemberDeclarationSyntax c = _syntaxFactory.ClassDeclaration(
                attributeLists: getAttributes(context.Attributes),
                modifiers: mods,
                keyword: SyntaxFactory.MakeToken(SyntaxKind.ClassKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: null,
                baseList: _syntaxFactory.BaseList(SyntaxFactory.MakeToken(SyntaxKind.ColonToken), baseTypes),
                constraintClauses: null,
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: members,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null);
            _pool.Free(members);
            _pool.Free(baseTypes);
            context.Put(c);

            _currentClass = null;

            // Generate XBase++ Class Function
            var expr = _syntaxFactory.TypeOfExpression(SyntaxFactory.MakeToken(SyntaxKind.TypeOfKeyword),
                                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                                    GenerateSimpleName(context.Id.GetText()),
                                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
            var args = MakeArgumentList(MakeArgument(expr));
            var result = GenerateMethodCall(ReservedNames.GetXppClassObject, args, true);
            var stmt = GenerateReturn(result, true);
            var body = MakeBlock(stmt);
            body.XGenerated = true;

            var func = _syntaxFactory.MethodDeclaration(
                attributeLists: MakeCompilerGeneratedAttribute(true),
                modifiers: TokenListWithDefaultVisibility(false, SyntaxKind.PublicKeyword, SyntaxKind.StaticKeyword),
                returnType: _usualType,
                explicitInterfaceSpecifier: null,
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: null,
                parameterList: EmptyParameterList(),
                constraintClauses: null,
                body: body,
                expressionBody: null,
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            func.XGenerated = true;
            if (context.TypeData.HasStatic)
            {
                GlobalEntities.StaticGlobalClassMembers.Add(func);
            }
            else
            {
                GlobalEntities.GlobalClassMembers.Add(func);
            }
        }
        public override void EnterXppdeclareMethod([NotNull] XP.XppdeclareMethodContext context)
        {
            // add method to list of declared methods in the class

            // use the current visibility saved with declMethodVis
            // and include the IsIn property for rerouting methods (should we support that ?)
            if (_currentClass == null)
            {
                // Generate an error
                return;
            }
            foreach (var name in context._Methods)
            {
                var declInfo = new XppDeclaredMethodInfo()
                {
                    Name = name.GetText(),
                    Parent = _currentClass,
                    Declaration = context,
                    DeclaredType = null,
                    Attributes = context.Attributes,
                    ModifierTokens = context.Modifiers?._Tokens
                };
                declInfo.Visibility = _currentClass.CurrentVisibility;
                _currentClass.Methods.Add(declInfo);
            }
        }

        public override void EnterXppclassvars([NotNull] XP.XppclassvarsContext context)
        {
            context.Visibility = _currentClass.CurrentVisibility;
        }

        public override void EnterXppdeclareproperty([NotNull] XP.XppdeclarepropertyContext context)
        {
            // When [VAR <VarName>] is missing then this is a declaration of an ACCESS or ASSIGN method.
            // In that case treat implementation like VO Access/assign
            // When VAR is available then the property name = the VAR name and the method names should be called
            // in the getter and setter
            // XPP allows to declare an ACCESS METHOD and then declare the method body without ACCESS prefix !
            // The method is called with an optional parameter (the value from the property)
            // In the method body the user needs to check for the type to see if the getter or setter is called.
            // 
            // 
            /*
             // example from XbZLog.prg
             Class XbZ_LogWriter
                Var cLogPath
                Var lLogActive
                Sync Method Open
                Sync Method Stop
                Access Assign Method Path
             EndClass
             Method XbZ_LogWriter:Path(cPath)
                if PCount() == 1 .and. ValType(cPath) == 'C'
                    ::cLogPath := alltrim(iif(empty(cPath), left(AppName(.t.), RAt('\', AppName(.t.))), cPath))
                    ::cLogPath += iif(right(::cLogPath, 1) == '\', '', '\')
                    if ::lLogActive .and. ::Stop(.t.)
                        ::Open()
                    endif
                endif
            return (::cLogPath)
             */
            string name = context.Id.GetText();
            bool hasVarName = context.VarName != null;
            if (hasVarName)
            {
                name = context.VarName.GetText();
            }
            _currentClass.AddProperty(name);
            // it is allowed to have a separate line for ACCESS and ASSIGN and map them to different methods
            var declInfo = _currentClass.Methods.Where(x => x.Name == name && x.IsProperty).FirstOrDefault();
            if (declInfo == null)
            {
                declInfo = new XppDeclaredMethodInfo()
                {
                    Name = name,
                    Declaration = context,
                    IsProperty = true,
                    Visibility = _currentClass.CurrentVisibility,
                    DeclaredType = context.Type,
                    Attributes = context.Attributes,
                    ModifierTokens = context.Modifiers?._Tokens
                };
                _currentClass.Methods.Add(declInfo);
            }
            declInfo.HasVarName = hasVarName;
            CheckAccessors(context.Accessors, declInfo, context.Id.GetText());
        }
        public override void ExitXppdeclareproperty([NotNull] XP.XppdeclarepropertyContext context)
        {
            context.SetSequencePoint(context.M, context.end.Stop);

        }
        public override void ExitXppclassvars([NotNull] XP.XppclassvarsContext context)
        {
            // add class vars to current class
            // use the current visibility saved with declMethodVis
            // IS and IN are not supported, so produce warning
            // include [SHARED], [READONLY] [ASSIGNMENT HIDDEN | PROTECTED | EXPORTED]  and [NOSAVE] modifiers
            // on top of the visibility modifier
            // The [Readonly] clause and [Assignment] clause are not supported yet
            // In as future build we will create a private backing Ivar and 
            // Create a property with Getter and Setter with property visibility
            // to emulate the possible combinations of [ReadOnly] and Assignment and Visibility
            // The following table lists what should be done
            // ========================================================================.
            //               | Visibility                                              |
            // Assignment    | Hidden           |  Protected        | Exported         |
            // ========================================================================.
            // Hidden        | pri Get/pri Set  |  pro Get pri Set  | pub Get pri Set  |
            // Protected     |                  |  pro Get pro Set  | pub Get pro Set  |
            // Exported      |    -             |                   | pub Get pub Set  |
            // ========================================================================.
            // // The Readonly clause does something similar as the ASSIGNMENT.
            // For the following visibilities this results in:
            // Private         Readonly is useless
            // Protected       Readonly creates a Protected Getter and a hidden/Private Setter
            // Exported/Public ReadOnly creates a Public Getter and a Protected Setter
            //
            var varList = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            // Check to see if the variables have not been also declared as property
            // when they are we do not generate variables
            var varType = getDataType(context.DataType);
            varType.XCanBeVoStruct = true;
            var fieldList = new List<FieldDeclarationSyntax>();
            var attributeLists = _pool.Allocate<AttributeListSyntax>();
            if (context.Nosave != null)
            {
                GenerateAttributeList(attributeLists, SystemQualifiedNames.NonSerialized);
            }
            foreach (var id in context._Vars)
            {
                varList.Clear();
                var variable = GenerateVariable(id.Get<SyntaxToken>());
                varList.Add(variable);
                // calculate modifiers
                // each field is added separately so we can later decide which field to keep and which one to delete when they are duplicated by a 
                var modifiers = decodeXppMemberModifiers(context, context.Visibility, false, context.Modifiers?._Tokens, false);
                if (varList.Count > 0)
                {
                    var decl = _syntaxFactory.VariableDeclaration(
                            type: varType,
                            variables: varList);

                    var fdecl = _syntaxFactory.FieldDeclaration(
                        attributeLists: attributeLists,
                        modifiers: modifiers,
                        declaration: decl,
                        semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                    fdecl.XNode = id;
                    ClassEntities.Peek().Members.Add(fdecl);
                    fieldList.Add(fdecl);
                }
            }
            context.PutList(MakeList(fieldList));
            _pool.Free(varList);
            _pool.Free(attributeLists);
        }

        public override void EnterXppmethodvis([NotNull] XP.XppmethodvisContext context)
        {
            if (_currentClass == null)
            {
                // Generate an error
                return;
            }
            _currentClass.CurrentVisibility = context.Vis.Token.Type;
        }

        #endregion
        #region Methods with Bodies

        private static void CheckInitMethods(XP.IMemberContext context)
        {
            context.Data.MustBeVoid = false;
            var idName = context.ShortName;
            if (XSharpString.Equals(idName, XSharpIntrinsicNames.InitMethod))
            {
                context.Data.MustBeVoid = true;
                context.Data.IsInitAxit = true;     // normal constructor
            }
            else if (XSharpString.Equals(idName, XSharpIntrinsicNames.InitClassMethod))
            {
                context.Data.MustBeVoid = true;
                context.Data.IsInitAxit = true;     // class constructor
                context.Data.HasClipperCallingConvention = false;
            }
        } 

        private static void CheckAccessors(XP.XppaccessorsContext context, XppDeclaredMethodInfo decl, string Id)
        {
            if (context != null)
            {
                if (context._Tokens.Any((t) => t.Type == XSharpLexer.ACCESS))
                {
                    decl.AccessMethod = Id;
                    decl.IsProperty = true;
                    
                }
                if (context._Tokens.Any((t) => t.Type == XSharpLexer.ASSIGN))
                {
                    decl.AssignMethod = Id;
                    decl.IsProperty = true;
                }
            }
        }

        public override void EnterXppinlineMethod([NotNull] XP.XppinlineMethodContext context)
        {
            // should do the same as the 'normal' methods in VO Class
            // method init becomes constructor
            // method initClass becomes Class constructor
            Check4ClipperCC(context, context.ParamList?._Params, null, context.Type);
            CheckInitMethods(context);
            var decl = new XppDeclaredMethodInfo()
            {
                Name = context.ShortName,
                Parent = _currentClass,
                Visibility = _currentClass.CurrentVisibility,
                Declaration = context,
                Entity = context,
                Inline = true,
                DeclaredType = context.Type,
                Attributes = context.Attributes,
                ModifierTokens = context.Modifiers?._Tokens
            };
            var name = context.Id.GetText();
            CheckAccessors(context.Accessors, decl, name);
            if (decl.IsProperty)
            {
                _currentClass.AddProperty(name);
            }
            context.Info = decl;
            _currentClass.Methods.Add(decl);
        }
        public override void ExitXppinlineMethod([NotNull] XP.XppinlineMethodContext context)
        {
            // should do the same as the 'normal' methods in VO Class
            // method init becomes constructor
            // method initClass becomes Class constructor
            context.SetSequencePoint(context.I, context.end.Stop);
            if (context.Info == null)   // This should not happen
            {
                ParseErrors.Add(new ParseErrorData(context, ErrorCode.WRN_XPPMethodNotDeclared, context.ShortName));
                // setup dummy declaration
                context.Info = new XppDeclaredMethodInfo()
                {
                    Name = context.ShortName,
                    Parent = _currentClass,
                    Declaration = context,
                    Entity = context,
                    Visibility = XP.HIDDEN,
                    Inline = true,
                    DeclaredType = context.Type,
                    Attributes = context.Attributes,
                    ModifierTokens = context.Modifiers?._Tokens
                };
            }

            if (context.Data.IsInitAxit)
            {
                implementConstructor(context);
                return;
            }
            var attributes = getAttributes(context.Attributes);
            var modifiers = decodeXppMemberModifiers(context, context.Info.Visibility, false, context.Modifiers?._Tokens, true);
            if (context.Info.IsProperty)
            {
                // the backing method becomes private
                modifiers = decodeXppMemberModifiers(context, XP.PRIVATE, false, context.Modifiers?._Tokens, true);
            }
            TypeSyntax returnType = getReturnType(context);
            var parameters = getParameters(context.ParamList);
            var name = context.Id.Get<SyntaxToken>();
            if (context.Info.IsProperty && !context.Info.HasVarName)
            {
                // rename method because the property has the same name as the method
                name = SyntaxFactory.MakeIdentifier(context.Id.GetText() + XSharpSpecialNames.PropertySuffix);
            }
            var method = XppCreateMethod(context, name, attributes, modifiers, parameters, returnType);
            context.Put(method);
        }
        public override void EnterXppmethod([NotNull] XP.XppmethodContext context)
        {
            CheckVirtualOverride(context, context.Modifiers?._Tokens);
            Check4ClipperCC(context, context.ParamList?._Params, null, context.Type);
            CheckInitMethods(context);
            string name;
            XppClassInfo current;
            if (context.ClassId == null)
            {
                current = _classes.LastOrDefault();
            }
            else
            {
                // when context contains a classname, find the right class in the list of classes
                name = context.ClassId.GetText();
                current = FindClassInfo(name);
                if (current == null)
                {
                    ParseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_XPPClassNotFound, name));
                }
                current = _classes.LastOrDefault();
            }
            current.ExternalMethods.Add(context);
            if (current != null)
            {
                // link to method
                name = context.Id.GetText();
                var decl = current.FindMethod(name);
                if (decl is null)
                {
                    ParseErrors.Add(new ParseErrorData(context, ErrorCode.WRN_XPPMethodNotDeclared, name));
                    decl = new XppDeclaredMethodInfo()
                    {
                        Name = name,
                        Declaration = context,
                        Parent = current,
                        Entity = context,
                        Visibility = XP.PUBLIC,
                        Inline = false,
                        DeclaredType = context.Type,
                        Attributes = context.Attributes,
                        ModifierTokens = context.Modifiers?._Tokens
                    };
                    current.Methods.Add(decl);
                }
                // Xbase++ ignores the modifiers for the external method
                CheckAccessors(context.Accessors, decl, name);
                if (decl.IsProperty)
                {
                    if (XSharpString.Equals(decl.AccessMethod, name))
                    {
                        decl.Entity = context;
                    }
                    if (XSharpString.Equals(decl.AssignMethod, name))
                    {
                        decl.SetEntity = context;
                    }
                }
                else
                {
                    decl.Entity = context;
                }
                context.Info = decl;
            }
        }
        private void implementConstructor([NotNull] XP.IXPPMemberContext context)
        {
            var idName = context.ShortName;
            var classCtor = XSharpString.Compare(idName, XSharpIntrinsicNames.InitClassMethod) == 0;
            // find method in the declarations and find the visibility
            var mods = decodeXppMemberModifiers((XSharpParserRuleContext)context, context.Info.Visibility, classCtor, classCtor ? null : context.Mods?._Tokens, false);
            if (mods.Any((int)SyntaxKind.StaticKeyword))
            {
                context.Data.HasClipperCallingConvention = false;
            }
            var ctor = createConstructor(context, mods, context.Atts, context.Parameters, context.Statements, (XSharpParserRuleContext)context);
            if (ctor != null)
            {
                context.Put(ctor);
            }
            else
            {
                context.SetStatements(null);
            }
        }

        public override void ExitXppmethod([NotNull] XP.XppmethodContext context)
        {
            // should do the same as the 'normal' methods in VO Class
            // Retrieve visibility from the XppClassInfo, from the list of declared methods
            // when not declared produce warning (error ?)
            // When classname clause is missing then assume the last class in the file before this method
            context.SetSequencePoint(context.M, context.end.Stop);
            if (context.Info == null)
            {
                ParseErrors.Add(new ParseErrorData(context, ErrorCode.WRN_XPPMethodNotDeclared, context.ShortName));
                // setup dummy declaration
                context.Info = new XppDeclaredMethodInfo()
                {
                    Name = context.ShortName,
                    Declaration = context,
                    Entity = context,
                    Visibility = XP.HIDDEN,
                    DeclaredType = context.Type,
                    Attributes = context.Attributes,
                    ModifierTokens = context.Modifiers?._Tokens
                };
            }
            if (context.Data.IsInitAxit)
            {
                // method init becomes constructor, method initClass becomes Class constructor
                implementConstructor(context);
                return;
            }
            List<IToken> tokens = new List<IToken>();
            if (context.Info.ModifierTokens != null)
            {
                tokens.AddRange(context.Info.ModifierTokens);
            }
            if (context.Modifiers != null)
            {
                foreach (var token in context.Modifiers._Tokens)
                {
                    if (!tokens.Any(t => t.Type == token.Type))
                        tokens.Add(token);
                }
            }
            var modifiers = decodeXppMemberModifiers(context, context.Info.Visibility, false, tokens, true);
            if (context.Info.IsProperty)
            {
                // the backing method becomes private
                modifiers = decodeXppMemberModifiers(context, XP.PRIVATE, false, tokens, true);
            }
            TypeSyntax returnType = getDataType(context.Type);
            var attributes = getAttributes(context.Attributes);
            var parameters = getParameters(context.ParamList);
            var name = context.Id.Get<SyntaxToken>();
            if (context.Info.IsProperty && !context.Info.HasVarName)
            {
                // rename method because the property has the same name as the method
                name = SyntaxFactory.MakeIdentifier(context.Id.GetText() + XSharpSpecialNames.PropertySuffix);
            }
            var method = XppCreateMethod(context, name, attributes, modifiers, parameters, returnType);
            context.Put(method);
        }

        MemberDeclarationSyntax XppCreateMethod(XP.IXPPMemberContext context, SyntaxToken idName, SyntaxList<AttributeListSyntax> attributes,
            SyntaxList<SyntaxToken> modifiers, ParameterListSyntax parameters, TypeSyntax returnType)
        {
            var nobody = context.ExprBody != null;
            var body = nobody ? null : processEntityBody(context);
            var expressionBody = GetExpressionBody(context.ExprBody);
            var oldbody = body;
            ImplementClipperAndPSZ(context, ref attributes, ref parameters, ref body, ref returnType);
            if (body != oldbody)
            {
                context.Statements.Put(body);
            }
            if (context.Info.IsSync)
            {
                body = MakeBlock(MakeLock(GenerateSelf(), body));
            }
            MemberDeclarationSyntax m = _syntaxFactory.MethodDeclaration(
                  attributeLists: attributes,
                  modifiers: modifiers,
                  returnType: returnType,
                  explicitInterfaceSpecifier: null,
                  identifier: idName,
                  typeParameterList: null,
                  parameterList: parameters,
                  constraintClauses: null,
                  body: body,
                  expressionBody: expressionBody,
                  semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            return m;
        }

        #endregion
        #region Source, Namespace and Entity
        // these methods call the worker methods in TreeTransform. They are only
        // here because their names and parameters are generated differently
        public override void ExitEntity([NotNull] XP.EntityContext context)
        {
            if (context.GetChild(0) is XP.XppclassContext)
            {
                // classes are handled separately because we need to add the external methods to their members
                // so suppress call to base implementation. We will bind them later from ExitSource by calling bindClasses
                return;
            }
            base.ExitEntity(context);
        }

        private PropertyDeclarationSyntax XppCreateProperty(XppDeclaredMethodInfo propDecl)
        {
            string propName = propDecl.Name;
            string accName = propDecl.AccessMethod;
            string assName = propDecl.AssignMethod;
            if (accName != null && !propDecl.HasVarName)
                accName += XSharpSpecialNames.PropertySuffix;
            if (assName != null && !propDecl.HasVarName)
                assName += XSharpSpecialNames.PropertySuffix;
            var method = propDecl.Entity as XP.IXPPMemberContext;
            var propType = getDataType(propDecl.DeclaredType);
            var methType = getDataType(method.ReturnType);
            if (propDecl.DeclaredType == null)
            {
                propType = methType;
            }
            var accessors = _pool.Allocate<AccessorDeclarationSyntax>();
            var modifiers = decodeXppMemberModifiers((XSharpParserRuleContext)propDecl.Entity, propDecl.Visibility, false, propDecl.ModifierTokens, true);
            bool isStatic = false;
            if (method.Mods != null)
            {
                isStatic = method.Mods._Tokens.Any(t => t.Type == XSharpLexer.CLASS);
            }
            SyntaxList<AttributeListSyntax> attributes = default;
            if (method.Atts != null && propDecl.Attributes == null)
            {
                attributes = getAttributes(method.Atts);
            }
            else if (method.Atts == null && propDecl.Attributes != null)
            {
                attributes = getAttributes(propDecl.Attributes);
            }

            #region Accessor
            if (!string.IsNullOrEmpty(accName))
            {
                ExpressionSyntax methodCall;
                if (isStatic)
                {
                    methodCall = GenerateMethodCall(accName, EmptyArgumentList(), true);
                }
                else
                {
                    methodCall = GenerateThisMethodCall(accName, EmptyArgumentList(), true);
                }
                var block = MakeBlock(GenerateReturn(methodCall));
                block.XGenerated = true;
                var accessor = _syntaxFactory.AccessorDeclaration(
                        SyntaxKind.GetAccessorDeclaration,
                        default,
                        default,
                        SyntaxFactory.MakeToken(SyntaxKind.GetKeyword),
                        block, null, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                accessor.XNode = method;
                accessors.Add(accessor);
            }
            #endregion
            #region Assign
            if (!string.IsNullOrEmpty(assName))
            {
                method = propDecl.SetEntity as XP.XppmethodContext;
                var args = MakeArgumentList(MakeArgument(GenerateSimpleName("value")));
                ExpressionSyntax methodCall;
                if (isStatic)
                {
                    methodCall = GenerateMethodCall(accName, args, true);
                }
                else
                {
                    methodCall = GenerateThisMethodCall(accName, args, true);
                }
                var stmt = GenerateExpressionStatement(methodCall, (XSharpParserRuleContext)method);
                var block = MakeBlock(stmt);
                block.XGenerated = true;
                var accessor = _syntaxFactory.AccessorDeclaration(
                        SyntaxKind.SetAccessorDeclaration,
                        default, default,
                        SyntaxFactory.MakeToken(SyntaxKind.SetKeyword),
                        block, null, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                accessor.XNode = method;
                accessors.Add(accessor);
            }
            #endregion
            var accessorList = _syntaxFactory.AccessorList(SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                          accessors, SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));
            var prop = _syntaxFactory.PropertyDeclaration(
                    attributes,
                    modifiers: modifiers,
                    type: propType,
                    explicitInterfaceSpecifier: null,
                    identifier: SyntaxFactory.MakeIdentifier(propName),
                    accessorList: accessorList,
                    expressionBody: null,
                    initializer: null,
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            prop.XNode = (IXParseTree)propDecl.Declaration;
            _pool.Free(accessors);
            if (methType.ToFullString() != propType.ToFullString())
            {
                prop = prop.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_XPPPropertyDeclarationTypeMismatch, propName));
            }

            return prop;
        }
        private void bindXPPClasses()
        {
            // note that this runs AFTER the entities have been walked and is called from ExitSource
            var classes = new List<XSharpParserRuleContext>();
            foreach (var current in _classes)
            {
                var members = _pool.Allocate<MemberDeclarationSyntax>();
                var classdecl = current.Entity.Get<ClassDeclarationSyntax>();
                foreach (var member in classdecl.Members)
                {
                    bool addMember = true;
                    // Add a check to see if we do not add a field with the same name as a property
                    if (member is FieldDeclarationSyntax fd)
                    {
                        var xnode = member.XNode as XSharpParserRuleContext;
                        var name = fd.Declaration.Variables[0].Identifier.Text;
                        if (current.HasProperty(name))
                        {
                            addMember = false;
                            var atts = fd.AttributeLists;
                            atts = MakeIsInstanceAttribute(atts);

                            var modifiers = fd.Modifiers;
                            if (modifiers.Any((int)SyntaxKind.PublicKeyword))
                            {
                                var list = new List<SyntaxToken>();
                                list.Add(SyntaxFactory.MakeGeneratedToken(SyntaxKind.PrivateKeyword));
                                foreach (var mod in modifiers)
                                {
                                    if (!SyntaxFacts.IsAccessibilityModifier(mod.Kind))
                                        list.Add(mod);
                                }
                                modifiers = MakeList(list);
                            }
                            fd = fd.Update(atts, modifiers, fd.Declaration, fd.SemicolonToken);
                            xnode.Put(fd);
                            members.Add(fd);
                        }
                    }
                    if (addMember)
                    {
                        members.Add(member);
                    }
                }
                foreach (var method in current.Methods.Where(x => !x.IsProperty))
                {
                    var entity = method.Entity;
                    if (entity == null)
                    {
                        ParseErrors.Add(new ParseErrorData(method.Declaration.Start, ErrorCode.WRN_XPPMethodNotImplemented, method.Name));
                    }
                    else
                    {
                        members.Add(entity.Get<MemberDeclarationSyntax>());
                    }
                }
                foreach (var mem in members.ToList())
                {
                    // when an instant constructors then remember this
                    if (mem is ConstructorDeclarationSyntax cds && !cds.IsStatic())
                    {
                        current.Entity.TypeData.HasInstanceCtor = true;
                        break;
                    }
                }
                // process properties
                foreach (var method in current.Methods.Where(x => x.IsProperty))
                {
                    var entity = method.Entity;
                    if (entity == null)
                    {
                        ParseErrors.Add(new ParseErrorData(method.Declaration.Start, ErrorCode.WRN_XPPMethodNotImplemented, method.Name));
                    }
                    else
                    {
                        members.Add(entity.Get<MemberDeclarationSyntax>());
                        var prop = XppCreateProperty(method);
                        members.Add(prop);
                    }
                }

                // update class declaration, add external methods
                classdecl = classdecl.Update(classdecl.AttributeLists, classdecl.Modifiers,
                    classdecl.Keyword, classdecl.Identifier, classdecl.TypeParameterList, classdecl.BaseList,
                    classdecl.ConstraintClauses, classdecl.OpenBraceToken, members, classdecl.CloseBraceToken, classdecl.SemicolonToken);
                _pool.Free(members);

                classdecl = GenerateDefaultClipperCtor(classdecl, current.Entity);
                current.Entity.Put(classdecl);
                // by binding the classdecl to the EntityContext it will be generated later
                var ent = current.Entity.Parent as XP.EntityContext;
                ent.Put(classdecl);

                // check to see if this is a static class. In that case we must add it to the static global members
                if (current.Entity.TypeData.HasStatic)
                {
                    var nsName = XSharpSpecialNames.XppStaticClassPrefix + GetStaticGlobalClassname();
                    AddUsingWhenMissing(nsName, false, null);
                    var ns = GenerateNamespace(nsName, MakeList<MemberDeclarationSyntax>(classdecl));
                    ent.Put(ns);
                }
            }
            return;
        }
        public override void ExitSource([NotNull] XP.SourceContext context)
        {
            // we do NOT call base.ExitSource because we want to skip methods that are bound to classes
            // Bind our XPP Classes first and then call _exitSource with the entities that are not a XppMethodContext
            bindXPPClasses();
            var entities = new List<XP.EntityContext>();
            // do not add the methods. These should be linked to a class
            entities.AddRange(context._Entities.Where(e => !(e.GetChild(0) is XP.XppmethodContext)));
            _exitSource(context, entities);
        }

        public override void EnterNamespace_([NotNull] XP.Namespace_Context context)
        {
            base.EnterNamespace_(context);
            _classstack.Push(_classes);
            _classes = new List<XppClassInfo>();
        }
        public override void ExitNamespace_([NotNull] XP.Namespace_Context context)
        {
            // we do not call base.ExitNamespace
            bindXPPClasses();
            _classes = _classstack.Pop();
            var entities = new List<XSharpParserRuleContext>();
            // do not add the methods. These should be linked to a class
            entities.AddRange(context._Entities.Where(e => !(e.GetChild(0) is XP.XppmethodContext)));
            _exitNamespace(context, context.Name.GetText(), entities);
        }
        #endregion 
        #region Modifiers 
        public override void ExitXppclassModifiers([NotNull] XP.XppclassModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                SyntaxToken kw = null;
                switch (m.Type)
                {
                    case XP.STATIC:
                        kw = SyntaxFactory.MakeToken(SyntaxKind.InternalKeyword, m.Text);
                        // todo place in unique namespace
                        break;
                    case XP.FINAL:
                    case XP.SEALED:
                        kw = SyntaxFactory.MakeToken(SyntaxKind.SealedKeyword, m.Text);
                        break;
                    case XP.ABSTRACT:
                        kw = SyntaxFactory.MakeToken(SyntaxKind.AbstractKeyword, m.Text);
                        break;
                    default:
                        break;
                }
                if (kw != null)
                {
                    modifiers.AddCheckUnique(kw);
                }
            }
            context.PutList(modifiers.ToList<SyntaxToken>());
            _pool.Free(modifiers);
        }

        #endregion

        private SyntaxList<SyntaxToken> decodeXppMemberModifiers(XSharpParserRuleContext context,
            int visibility, bool isStatic, IList<IToken> tokens, bool isMethod)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            if (isStatic)
            {
                modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword, "CLASS"));
            }
            else
            {
                modifiers.Add(DecodeVisibility(visibility));
            }
            bool hasFinal = false;
            bool noOverride = false;
            if (tokens != null)
            {
                foreach (var m in tokens)
                {
                    SyntaxToken kw = null;
                    switch (m.Type)
                    {
                        case XP.DEFERRED: // DEFERRED METHOD becomes ABSTRACT METHOD
                        case XP.ABSTRACT: // 
                            kw = SyntaxFactory.MakeToken(SyntaxKind.AbstractKeyword, m.Text);
                            break;
                        case XP.FINAL: // FINAL METHOD will generate non virtual method, even when the Default Virtual is on
                        case XP.SEALED:
                            hasFinal = true;
                            kw = SyntaxFactory.MakeToken(SyntaxKind.SealedKeyword, m.Text);
                            break;
                        case XP.INTRODUCE: //  INTRODUCE METHOD will generate NEW METHOD
                        case XP.NEW:
                            kw = SyntaxFactory.MakeToken(SyntaxKind.NewKeyword, m.Text);
                            noOverride = true;
                            break;
                        case XP.OVERRIDE: // OVERRIDE METHOD is obvious
                            kw = SyntaxFactory.MakeToken(SyntaxKind.OverrideKeyword, m.Text);
                            break;
                        case XP.ASYNC: //
                            kw = SyntaxFactory.MakeToken(SyntaxKind.AsyncKeyword, m.Text);
                            break;
                        case XP.EXTERN: //
                            kw = SyntaxFactory.MakeToken(SyntaxKind.ExternKeyword, m.Text);
                            break;
                        case XP.UNSAFE: //
                            kw = SyntaxFactory.MakeToken(SyntaxKind.UnsafeKeyword, m.Text);
                            break;
                        case XP.CLASS:
                            kw = SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword, m.Text);
                            break;
                        case XP.VIRTUAL:
                            kw = SyntaxFactory.MakeToken(SyntaxKind.VirtualKeyword, m.Text);
                            break;
                        case XP.SYNC:   // Handled later
                            break;
                        case XP.STATIC:
                            // remove visibility modifiers
                            // STATIC member (Visibility only in the scope of the prg).
                            // member becomes Internal 
                            kw = SyntaxFactory.MakeToken(SyntaxKind.InternalKeyword, m.Text);
                            var tmp = modifiers.ToList();
                            modifiers.Clear();

                            foreach (SyntaxToken mod in tmp)
                            {
                                // remove all existing visibility keywords
                                switch (mod.Kind)
                                {
                                    case SyntaxKind.ProtectedKeyword:
                                    case SyntaxKind.PublicKeyword:
                                    case SyntaxKind.PrivateKeyword:
                                    case SyntaxKind.InternalKeyword:
                                        break;
                                    default:
                                        modifiers.AddCheckUnique(mod);
                                        break;
                                }
                            }
                            break;
                        default:
                            break;
                    }
                    if (kw != null)
                    {
                        modifiers.AddCheckUnique(kw);
                    }
                }
            }
            bool enforceOverride = _options.HasOption(CompilerOption.EnforceOverride, context, PragmaOptions);
            if (isMethod && !hasFinal && !noOverride)
            {
                if (_options.HasOption(CompilerOption.VirtualInstanceMethods, context, PragmaOptions))
                {
                    modifiers.FixVirtual(enforceOverride);
                }
                else
                {
                    modifiers.FixOverride(enforceOverride);
                }
            }
            var result = modifiers.ToList<SyntaxToken>();
            _pool.Free(modifiers);
            return result;
        }
        public override void ExitMethodCall([NotNull] XP.MethodCallContext context)
        {
            // convert ClassName():New(...) to ClassName{...}
            if (context.Expr is XP.AccessMemberContext lhs)
            {
                if (lhs.Op.Type == XP.COLON && lhs.Name.GetText().ToLower() == "new")
                {
                    if (lhs.Expr is XP.MethodCallContext operand && operand.ArgList == null)
                    {
                        if (operand.Expr is XP.PrimaryExpressionContext primary)
                        {
                            var className = primary.GetText();
                            var seperators = new char[] { '.', ':' };
                            if (className.IndexOfAny(seperators) == -1)
                            {
                                var args = context.ArgList.Get<ArgumentListSyntax>();
                                var type = GenerateQualifiedName(className);
                                var expr = CreateObject(type, args);
                                context.Put(expr);
                                return;
                            }
                        }
                    }
                }
            }
            base.ExitMethodCall(context);
        }
        public override void ExitAccessMember([NotNull] XP.AccessMemberContext context)
        {
            if (context.Op.Type == XP.COLONCOLON)
            {
                if (CurrentEntity is XP.IXPPMemberContext mem)
                {
                    // for static members we translate :: to ParentName.
                    if (mem.IsStatic)
                    {
                        var info = mem.Info;
                        var parent = info.Parent;
                        var parentName = GenerateSimpleName(parent.Name);
                        context.Put(MakeSimpleMemberAccess(
                            parentName,
                            context.Name.Get<SimpleNameSyntax>()));
                        return;
                    }
                }
            }
            base.ExitAccessMember(context);
        }
    }
}
