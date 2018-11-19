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
    public class XppClassInfo
    {
        internal IList<XppDeclaredMethodInfo> Methods { get; set; }
        internal string Name { get; set; }
        internal int CurrentVisibility { get; set; }
        internal XP.XppclassContext Entity { get; set; }
        internal XppClassInfo()
        {
            Methods = new List<XppDeclaredMethodInfo>();
            CurrentVisibility = XP.HIDDEN;
        }
        internal XppDeclaredMethodInfo FindMethod( string name)
        {
            foreach (var decl in Methods)
            {
                if (string.Equals(name, decl.Name, StringComparison.OrdinalIgnoreCase))
                {
                    return decl;
                }
            }
            return null;
        }
    }

    public class XppDeclaredMethodInfo
    {
        internal string Name { get; set; }
        internal int Visibility { get; set; }
        internal bool IsImplemented => Entity != null;
        internal XP.IEntityContext Entity { get; set; }
        internal XppClassInfo Parent = null;
        internal bool Inline { get; set; }
        internal XppDeclaredMethodInfo()
        {
            Name = null;
            Visibility = XP.HIDDEN;
            Entity = null;
            Parent = null;
            Inline = false;
        }
    }

    internal class XSharpXPPTreeTransformation : XSharpVOTreeTransformation
    {
        internal IList<XppClassInfo> _classes;
        internal XppClassInfo _currentClass = null;
        public XSharpXPPTreeTransformation(XSharpParser parser, CSharpParseOptions options, SyntaxListPool pool,
                    ContextAwareSyntax syntaxFactory, string fileName) :
                    base(parser, options, pool, syntaxFactory, fileName)
        {
            _classes = new List<XppClassInfo>();
            _currentClass = null;
        }
        private SyntaxToken DecodeVisibility(int vis)
        { 
            switch (vis)
            {
                case XP.EXPORTED:
                    return SyntaxFactory.MakeToken(SyntaxKind.PublicKeyword, "EXPORTED");
                case XP.PROTECTED:
                    return SyntaxFactory.MakeToken(SyntaxKind.ProtectedKeyword, "PROTECTED");
                case XP.HIDDEN:
                default:
                    return SyntaxFactory.MakeToken(SyntaxKind.PrivateKeyword, "HIDDEN");
            }
        }

        private XppClassInfo FindClassInfo(string name)
        {
            foreach (var info in _classes)
            {
                if (String.Compare(info.Name, name, true) == 0)
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
            var members = _pool.Allocate<MemberDeclarationSyntax>();
            var generated = ClassEntities.Pop();
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility();
            XP.DatatypeContext basetype = null;
            context.Data.Partial = mods.Any((int)SyntaxKind.PartialKeyword);
            if (generated.Members.Count > 0)                // inline methods, properties and fields
            {
                members.AddRange(generated.Members);
            }
            // check if all declared methods have been implemented
            // and if so, then add the methods to the members list
            string className = context.Id.GetText();
            if (context._BaseTypes.Count > 1)
            {
                context.AddError(new ParseErrorData(context, ErrorCode.ERR_XPPMultipleInheritance));
            }
            else if (context._BaseTypes.Count == 1)
            {
                basetype = context._BaseTypes[0];
                if (context.From.Type != XP.SHARING)
                {
                    context.AddError(new ParseErrorData(context, ErrorCode.WRN_XPPSuperIVarsAlwaysShared));
                }
            }
            XppClassInfo thisClass = FindClassInfo(className);
            if (thisClass == null || thisClass.Entity != context)
            {
                context.AddError(new ParseErrorData(context, ErrorCode.ERR_ParserError, "Could not locate ClassInfo for class " + className));
                return;
            }

            foreach (var method in thisClass.Methods)
            {
                var entity = method.Entity;
                if (entity == null)
                {
                    context.AddError(new ParseErrorData(context, ErrorCode.WRN_XPPMethodNotImplemented, method.Name));
                }
                members.Add(entity.Get<MemberDeclarationSyntax>());
            }
            foreach (var mem in members.ToList())
            {
                // when an instant constructors then remember this
                if (mem is ConstructorDeclarationSyntax && !((ConstructorDeclarationSyntax)mem).IsStatic())
                {
                    context.Data.HasInstanceCtor = true;
                    break;
                }
            }

            generated.Free();
            var baseTypes = _pool.AllocateSeparated<BaseTypeSyntax>();
            var baseType = basetype.Get<TypeSyntax>();
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
                typeParameterList: null,
                baseList: _syntaxFactory.BaseList(SyntaxFactory.MakeToken(SyntaxKind.ColonToken), baseTypes),
                constraintClauses: null,
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: members,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null);
            _pool.Free(members);
            _pool.Free(baseTypes);
            m = CheckForGarbage(m, context.Ignored, "Name after END CLASS");
            context.Put(m);
            if (context.Data.Partial)
            {
                GlobalEntities.NeedsProcessing = true;
            }
            _currentClass = null;
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
                var declInfo = new XppDeclaredMethodInfo() { Name = name.GetText(), Parent = _currentClass };
                declInfo.Visibility = _currentClass.CurrentVisibility;
                _currentClass.Methods.Add(declInfo);
             }
        }

        public override void EnterXppclassvars([NotNull] XP.XppclassvarsContext context)
        {
            context.Visibility = _currentClass.CurrentVisibility;
        }

        public override void ExitXppproperty([NotNull] XP.XpppropertyContext context)
        {
            // When [VAR <VarName>] is missing then this is a declaration of an ACCESS or ASSIGN method.
            // In that case treat implementation like VO Access/assign
            // When VAR is available then treat like  Property declaration and implement the property
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
                Sync Access Assign Method Path
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
            var varType = context.DataType?.Get<TypeSyntax>() ?? _getMissingType();
            varType.XVoDecl = true;
            #region Unsupported options (for now)
            if (context.Is != null)
            {
                context.AddError(new ParseErrorData(context, ErrorCode.WRN_XPPVarIsInNotSupported));
            }
            if (context.Shared != null)
            {
                context.AddError(new ParseErrorData(context, ErrorCode.WRN_XPPSharedIsDefault));
            }
            if (context.ReadOnly != null)
            {
                context.AddError(new ParseErrorData(context, ErrorCode.WRN_XPPReadonlyClause));
            }
            if (context.Nosave != null)
            {
                context.AddError(new ParseErrorData(context, ErrorCode.WRN_XPPNoSaveNotSupported));
            }
            #endregion
            foreach (var id in context._Vars)
            {
                if (varList.Count > 0)
                    varList.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                var variable = GenerateVariable(id.Get<SyntaxToken>());
                varList.Add(variable);
            }
            // calculate modifiers
            var modifiers = decodeXppMemberModifiers(context.Visibility, false, context.Modifiers?._Tokens);
            if (varList.Count > 0)
            {
                var decl = _syntaxFactory.VariableDeclaration(
                        type: varType,
                        variables: varList);

                var fdecl = _syntaxFactory.FieldDeclaration(
                    attributeLists: EmptyList<AttributeListSyntax>(),
                    modifiers: modifiers,
                    declaration: decl,
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                context.Put(fdecl);
                ClassEntities.Peek().Members.Add(fdecl);
            }
            _pool.Free(varList);
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

        private void CheckInitMethods(XP.IEntityContext context)
        {
            context.Data.MustBeVoid = false;
            var idName = context.ShortName;
            if (String.Equals(idName, "init", StringComparison.OrdinalIgnoreCase))
            {
                context.Data.MustBeVoid = true;
                context.Data.IsInitAxit = true;     // normal constructor
            }
            else if (String.Equals(idName, "initclass", StringComparison.OrdinalIgnoreCase))
            {
                context.Data.MustBeVoid = true;
                context.Data.IsInitAxit = true;     // class constructor
            }
        }
        public override void EnterXppinlineMethod([NotNull] XP.XppinlineMethodContext context)
        {
            // should do the same as the 'normal' methods in VO Class
            // method init becomes constructor
            // method initClass becomes Class constructor
            Check4ClipperCC(context, context.ParamList?._Params, null, context.Type);
            CheckInitMethods(context);
            var decl = new XppDeclaredMethodInfo() { Name = context.ShortName, Parent = _currentClass, Visibility = _currentClass.CurrentVisibility, Entity = context , Inline = true};
            context.Info = decl;
            _currentClass.Methods.Add(decl);
        }
        public override void ExitXppinlineMethod([NotNull] XP.XppinlineMethodContext context)
        {
            // should do the same as the 'normal' methods in VO Class
            // method init becomes constructor
            // method initClass becomes Class constructor
            if (context.Info == null)   // This should not happen
            {
                context.AddError(new ParseErrorData(ErrorCode.WRN_XPPMethodNotDeclared, context.ShortName));
                // setup dummy declaration
                context.Info = new XppDeclaredMethodInfo() { Name = context.ShortName, Parent = _currentClass, Entity = context, Visibility = XP.HIDDEN, Inline = true};
            }

            if (context.Data.IsInitAxit)
            {
                implementConstructor(context);
                return;
            }
            var attributes = context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>();
            var modifiers = decodeXppMemberModifiers(context.Info.Visibility, false, context.Modifiers?._Tokens);
            TypeSyntax returnType = context.Type?.Get<TypeSyntax>();
            if (returnType == null)
            {
                returnType = _getMissingType();
            }
            var parameters = context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList();
            var method = XppCreateMethod(context, context.Id.Get<SyntaxToken>(), attributes, modifiers, parameters, returnType );
            context.Put(method);
        }
        public override void EnterXppmethod([NotNull] XP.XppmethodContext context)
        {
            Check4ClipperCC(context, context.ParamList?._Params, null, context.Type);
            CheckInitMethods(context);
            string name ;
            XppClassInfo current = null;
            if (context.ClassId == null)
            {
                current = _currentClass;
            }
            else
            {
                // when context contains a classname, find the right class in the list of classes
                name = context.ClassId.GetText();
                current = FindClassInfo(name);
                if (current == null)
                {
                    context.AddError(new ParseErrorData(ErrorCode.ERR_XPPClassNotFound, name));
                }
                current = _currentClass;    // set a meaningful value
            }
            
            if ( current != null)
            {
                // link to method
                name = context.Id.GetText();
                var decl = current.FindMethod(name);
                if (decl != null)
                {
                    decl.Entity = context;
                    context.Info = decl;
                }
            }
        }
        private void implementConstructor([NotNull] XP.IXPPEntityContext context)
        {
            var idName = context.ShortName;
            var classCtor = String.Compare(idName, "initclass", true) == 0;
            // find method in the declarations and find the visibility
            var mods = decodeXppMemberModifiers(context.Info.Visibility, classCtor, classCtor ? null : context.Mods?._Tokens);
            var ctor = createConstructor(context, mods, context.Atts, context.Parameters, context.Statements, (XSharpParserRuleContext) context);
            if (ctor != null)
            {
                context.Put(ctor);
            }
            else
            {
                context.Statements = null;
            }

        }

        public override void ExitXppmethod([NotNull] XP.XppmethodContext context)
        {
            // should do the same as the 'normal' methods in VO Class
            // Retrieve visibility from the XppClassInfo, from the list of declared methods
            // when not declared produce warning (error ?)
            // When classname clause is missing then assume the last class in the file before this method
            if (context.Info == null)
            {
                context.AddError(new ParseErrorData(ErrorCode.WRN_XPPMethodNotDeclared, context.ShortName));
                // setup dummy declaration
                context.Info = new XppDeclaredMethodInfo() { Name = context.ShortName, Entity = context, Visibility = XP.HIDDEN };
            }
            if (context.Data.IsInitAxit)
            {
                // method init becomes constructor, method initClass becomes Class constructor
                implementConstructor(context);
                return;
            }
            var modifiers = decodeXppMemberModifiers(context.Info.Visibility, false, context.Modifiers?._Tokens);
            TypeSyntax returnType = context.Type?.Get<TypeSyntax>();
            if (returnType == null)
            {
                returnType = _getMissingType();
            }
            var attributes = context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>();
            var parameters = context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList();
 
            var method = XppCreateMethod(context, context.Id.Get<SyntaxToken>(), attributes, modifiers, parameters, returnType);
            context.Put(method);
        }
        MemberDeclarationSyntax XppCreateMethod(XP.IXPPEntityContext context, SyntaxToken idName, SyntaxList<AttributeListSyntax> attributes,
            SyntaxList<SyntaxToken> modifiers, ParameterListSyntax parameters, TypeSyntax returnType)
        {
            var body = context.Statements.Get<BlockSyntax>();
            var oldbody = body;
            ImplementClipperAndPSZ(context, ref attributes, ref parameters, ref body, ref returnType);
            if (body != oldbody)
            {
                context.Statements.Put(body);
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
                  expressionBody: null, // TODO: (grammar) expressionBody methods
                  semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            return m;
        }

        #endregion
        #region Source, Namespace and Entity
        // these methods call the worker methods in TreeTransform. They are only
        // here because their names and parameters are generated differently
        public override void ExitXppentity([NotNull] XP.XppentityContext context)
        {
            _exitEntity(context);
        }
        public override void EnterXppsource([NotNull] XP.XppsourceContext context)
        {
            _enterSource();
        }
        public override void ExitXppsource([NotNull] XP.XppsourceContext context)
        {
            var entities = new List<XSharpParserRuleContext>();
            entities.AddRange(context._Entities);
            _exitSource(context, entities);
        }

        public override void ExitXppnamespace([NotNull] XP.XppnamespaceContext context)
        {
            var entities = new List<XSharpParserRuleContext>();
            entities.AddRange(context._Entities);
            _exitNamespace(context, context.Name.GetText(), context.Ignored, entities);
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
                        kw = SyntaxFactory.MakeToken(SyntaxKind.SealedKeyword, m.Text);
                        break;
                    case XP.FREEZE:
                        context.AddError(new ParseErrorData(ErrorCode.WRN_XPPFrozedNotSupported));
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
        public override void ExitXppdeclareModifiers([NotNull] XP.XppdeclareModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            bool isVirtual = true;
            foreach (var m in context._Tokens)
            {
                SyntaxToken kw = null;
                switch (m.Type)
                {
                    case XP.DEFERRED:
                        kw = SyntaxFactory.MakeToken(SyntaxKind.AbstractKeyword, m.Text);
                        break;
                    case XP.FINAL:
                        //kw = SyntaxFactory.MakeToken(SyntaxKind.SealedKeyword, m.Text);
                        isVirtual = false;
                        break;
                    case XP.INTRODUCE:
                        kw = SyntaxFactory.MakeToken(SyntaxKind.NewKeyword, m.Text);
                        break;
                    case XP.OVERRIDE:
                        kw = SyntaxFactory.MakeToken(SyntaxKind.OverrideKeyword, m.Text);
                        break;
                    case XP.CLASS:
                        kw = SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword, m.Text);
                        break;
                    case XP.SYNC:
                        context.AddError(new ParseErrorData(ErrorCode.WRN_XPPSyncNotSupported));
                        break;
                    default:
                        break;
                }
                if (kw != null)
                {
                    modifiers.AddCheckUnique(kw);
                }
            }
            if (_options.VirtualInstanceMethods && isVirtual)
                modifiers.FixDefaultVirtual();
            else
                modifiers.FixDefaultMethod();
            context.PutList(modifiers.ToList<SyntaxToken>());
            _pool.Free(modifiers);

        }
         #endregion

        private SyntaxList<SyntaxToken> decodeXppMemberModifiers(int visibility, bool isStatic, IList<IToken> tokens = null)
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
            if (tokens != null)
            {
                foreach (var token in tokens)
                {
                    SyntaxToken kw = null;
                    switch (token.Type)
                    {
                        // Member Modifiers from XppMemberModifiers rule
                        case XP.CLASS:
                            kw = SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword, token.Text);
                            break;
                        case XP.STATIC:
                            // remove visibility modifiers
                            kw = SyntaxFactory.MakeToken(SyntaxKind.InternalKeyword, token.Text);
                            var tmp = modifiers.ToList();
                            modifiers.Clear();
                            foreach (SyntaxToken mod in tmp)
                            {
                                switch (mod.Kind)
                                {
                                    case SyntaxKind.ProtectedKeyword:
                                    case SyntaxKind.PublicKeyword:
                                    case SyntaxKind.PrivateKeyword:
                                        break;
                                    default:
                                        modifiers.Add(mod);
                                        break;
                                }
                            }
                            break;
                    }
                    if (kw != null)
                    {
                        modifiers.AddCheckUnique(kw);
                    }

                }
            }
            var result = modifiers.ToList<SyntaxToken>();  
            _pool.Free(modifiers);
            return result;
        }
    }
    
}
