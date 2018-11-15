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
        internal XppClassInfo()
        {
            Methods = new List<XppDeclaredMethodInfo>();
            CurrentVisibility = XP.HIDDEN;
        }
    }

    public class XppDeclaredMethodInfo
    {
        internal string Name { get; set; }
        internal IList<IToken> Modifiers { get; set; }
        internal int Visibility { get; set; }
        internal bool IsImplemented => Entity != null;
        internal XP.IEntityContext Entity { get; set; }
        internal XppClassInfo Parent = null;
        internal XppDeclaredMethodInfo()
        {
            Modifiers = null;
            Name = null;
            Visibility = XP.HIDDEN;
            Entity = null;
            Parent = null;
        }

    }

    internal class XSharpXPPTreeTransformation : XSharpVOTreeTransformation
    {
        internal IList<XppClassInfo> _classes;
        internal XppClassInfo _currentClass = null;
        /// <summary>
        /// Keep track of properties of current declared class
        /// </summary>
         /// <summary>
        /// Keep track of properties of declared class
        /// </summary>
         public XSharpXPPTreeTransformation(XSharpParser parser, CSharpParseOptions options, SyntaxListPool pool,
                    ContextAwareSyntax syntaxFactory, string fileName) :
                    base(parser, options, pool, syntaxFactory, fileName)
        {
            _classes = new List<XppClassInfo>();
            _currentClass = null;
        }

        private void AddClass(string name)
        {
            var classinfo = new XppClassInfo() { Name = name };
            _classes.Add(classinfo);
            _currentClass = classinfo;
        }
        #region Class and ClassMembers
        public override void EnterXppclass([NotNull] XP.XppclassContext context)
        {
            // assign object with current class info, such as name, visibility, list of declared methods etc
            // 
            ;
        }
        public override void ExitXppclass([NotNull] XP.XppclassContext context)
        {
            // Check to see if all declared methods have been implemented.
            // And should this generate an error or a warning ?
            ;
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
                if (context.Modifiers != null)
                {
                    declInfo.Modifiers = context.Modifiers._Tokens;
                }
                _currentClass.Methods.Add(declInfo);
             }
        }

        public override void EnterXppclassvars([NotNull] XP.XppclassvarsContext context)
        {
            // add class vars to current class
            // use the current visibility saved with declMethodVis
            // IS and IN are not supported, so produce warning
            // include [SHARED], [READONLY] [ASSIGNMENT HIDDEN | PROTECTED | EXPORTED]  and [NOSAVE] modifiers
            // on top of the visibility modifier
            ;

        }
        public override void ExitXppclassMember([NotNull] XP.XppclassMemberContext context)
        {
            ; 
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
            // declare class vars like in VO/Core
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
            ;
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

        public override void EnterXppvisibility([NotNull] XP.XppvisibilityContext context)
        {
            ; // nothing to do Handled on the places where this is used
        }
        public override void ExitXppisin([NotNull] XP.XppisinContext context)
        {
            ; // nothing to do Handled on the places where this is used
        }

        #endregion
        #region Methods with Bodies

        private void CheckInitMethods(XP.IEntityContext context)
        {
            context.Data.MustBeVoid = false;
            Check4ClipperCC(context, context.Params, null, context.ReturnType);
            var idName = context.Name;
            if (String.Equals(idName, "init", StringComparison.OrdinalIgnoreCase))
            {
                context.Data.MustBeVoid = true;
                context.Data.IsInitAxit = true;     // normal constructor
            }
            if (String.Equals(idName, "initclass", StringComparison.OrdinalIgnoreCase))
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
            CheckInitMethods(context);
            var decl = new XppDeclaredMethodInfo() { Name = context.ShortName, Parent = _currentClass, Visibility = _currentClass.CurrentVisibility, Entity = context };
            context.Info = decl;
        }
        public override void ExitXppinlineMethod([NotNull] XP.XppinlineMethodContext context)
        {
            // should do the same as the 'normal' methods in VO Class
            // method init becomes constructor
            // method initClass becomes Class constructor
            if (context.Info == null)   // This should not 
            {
                context.AddError(new ParseErrorData(ErrorCode.WRN_XPPMethodNotDeclared, context.ShortName));
                // setup dummy declaration
                context.Info = new XppDeclaredMethodInfo() { Name = context.ShortName, Entity = context, Visibility = XP.HIDDEN, Parent = _currentClass };
            }

            if (context.Data.IsInitAxit)
            {
                implementConstructor(context);
                return;
            }
            
        }
        public override void EnterXppmethod([NotNull] XP.XppmethodContext context)
        {
            CheckInitMethods(context);
            string name = context.Id.GetText();
            XppClassInfo current = null;
            if (context.ClassId == null)
            {
                current = _currentClass;
            }
            else
            {
                // when context contains a classname, find the right class in the list of classes
                name = context.ClassId.GetText();
                foreach (var cls in _classes)
                {
                    if (string.Equals(name, cls.Name, StringComparison.OrdinalIgnoreCase) )
                    {
                        current = cls;
                        break;
                    }
                }
                // no error yet when class not found
                context.AddError(new ParseErrorData(ErrorCode.ERR_XPPClassNotFound, name));
                current = _currentClass;    // set a meaningful value
            }
            name = context.Id.GetText();
            if ( current != null)
            {
                // link to method
                foreach (var decl in _currentClass.Methods)
                {
                    if (string.Equals(name, decl.Name, StringComparison.OrdinalIgnoreCase))
                    {
                        decl.Entity = context;
                        context.Info = decl;
                    }
                }
            }
        }
        private void implementConstructor([NotNull] XP.IXPPEntityContext context)
        {
            var idName = context.ShortName;
            var pool = _pool.Allocate<SyntaxToken>();
            // find method in the declarations and find the visibility
            if (string.Equals(idName, "initClass", StringComparison.OrdinalIgnoreCase))
            {
                pool.Add(SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword, "CLASS"));
            }
            switch (context.Info.Visibility)
            {
                case XP.EXPORTED:
                    pool.Add(SyntaxFactory.MakeToken(SyntaxKind.PublicKeyword, "EXPORTED"));
                    break;
                case XP.PROTECTED:
                    pool.Add(SyntaxFactory.MakeToken(SyntaxKind.ProtectedKeyword, "PROTECTED"));
                    break;
                case XP.HIDDEN:
                default:
                    pool.Add(SyntaxFactory.MakeToken(SyntaxKind.PrivateKeyword, "HIDDEN"));
                    break;
            }
            if (context.Mods != null)
            {
                pool.AddRange(context.Mods.GetList<SyntaxToken>());
            }
            var mods = pool.ToList();
            _pool.Free(pool);
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
                // method init becomes constructor
                // method initClass becomes Class constructor
                implementConstructor(context);
                return;
            }
        }
        #endregion
        #region Source, Namespace and Entity
        public override void ExitXppentity([NotNull] XP.XppentityContext context)
        {
            // copied from normal entity rule in TreeTransform
            var entity = context.children[0] as IXParseTree;
            if (_isScript)
            {
                context.Put(entity.Get<CSharpSyntaxNode>());
            }
            else if (entity is XP.IGlobalEntityContext)
            {
                ProcessGlobalEntityContext(entity as XP.IGlobalEntityContext);
            }
            else
            {
                // When last entity has to go to the functions class 
                ProcessLastGlobalEntity(context, entity);
            }
        }
        public override void ExitXppsource([NotNull] XP.XppsourceContext context)
        {
            var globalTypes = _pool.Allocate<MemberDeclarationSyntax>();
            foreach (var entityCtx in context._Entities)
            {
                ProcessEntity(globalTypes, entityCtx);
            }
            finishCompilationUnit(globalTypes);
            _pool.Free(globalTypes);
            //System.Diagnostics.Debug.WriteLine("Exit XPPSource " + _fileName);
        }

        public override void ExitXppnamespace([NotNull] XP.XppnamespaceContext context)
        {
            // copied from normal namespace rule in TreeTransform
            // we may want to de-duplicate some of this code later
            var externs = _pool.Allocate<ExternAliasDirectiveSyntax>();
            var usings = _pool.Allocate<UsingDirectiveSyntax>();
            var members = _pool.Allocate<MemberDeclarationSyntax>();
            foreach (var entityCtx in context._Entities)
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

            string name = context.Name.GetText();
            MemberDeclarationSyntax ns = _syntaxFactory.NamespaceDeclaration(SyntaxFactory.MakeToken(SyntaxKind.NamespaceKeyword),
                name: GenerateQualifiedName(context.Name.GetText()),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                externs: externs,
                usings: usings,
                members: members,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));

            _pool.Free(externs);
            _pool.Free(usings);
            _pool.Free(members);
            ns = CheckForGarbage(ns, context.Ignored, "Name after END NAMESPACE");
            context.Put(ns);
            // Now add our namespace to the usings list so functions etc can find members 
            string ourname = context.Name.GetText();
            var parent = context.Parent;
            while (parent is XP.XppentityContext)
            {
                var parentns = parent.Parent as XP.XppnamespaceContext;
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
                        // todo warn that we do not support Frozen classes. This is default in .Net
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
                        // todo warn that we do not support Sync 
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
        public override void ExitXppmemberModifiers([NotNull] XP.XppmemberModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            var m = context.Token;
            if (m != null)
            {
                var kw = SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword, m.Text);
                modifiers.AddCheckUnique(kw);
            }
            context.PutList(modifiers.ToList<SyntaxToken>());
            _pool.Free(modifiers);
        }
        #endregion

    }
}
