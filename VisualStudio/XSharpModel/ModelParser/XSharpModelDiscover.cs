//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.CodeAnalysis;
using LanguageService.SyntaxTree;
using LanguageService.SyntaxTree.Misc;
using LanguageService.CodeAnalysis.XSharp;
using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.Text.Classification;
using System.Collections.Immutable;

namespace XSharpModel
{

    /// <summary>
    /// This class finds the types, namespaces and regions <br/>
    /// Internally it is NOT thread safe.
    /// Its properties return Immutable Lists or Arrays.
    /// </summary>
    partial class XSharpModelDiscover : XSharpBaseListener
    {
        //public List<XSharpParser.IEntityContext> entities = new List<XSharpParser.IEntityContext>();
        protected readonly XFile _file;
        private readonly Stack<XType> _currentTypes;
        private readonly Stack<XType> _currentNSpaces;
        private readonly XSharpParseOptions _options;
        protected readonly Dictionary<string, XType> _types;
        protected readonly List<string> _usings;
        protected readonly List<string> _staticusings;
        private readonly XType _globalType;
        private readonly List<XTypeMember> _classVars;
        protected XTypeMember _currentMethod;
        private Modifiers _currentVarVisibility;
        private bool _currentVarStatic;
        private string _defaultNS;
        private XSharpParser.SourceContext _xSource;
        IEnumerable<Diagnostic> _errors;
        public XSharpModelDiscover(XFile file, XSharpParser.SourceContext ctx, IEnumerable<Diagnostic> errors)
        {
            // To store intermediate declarations
            this._file = file;
            this._xSource = ctx;
            this._errors = errors;
            //
            this._currentTypes = new Stack<XType>();
            this._currentNSpaces = new Stack<XType>();
            //
            this._options = file.Project.ProjectNode.ParseOptions;
            this._types = new Dictionary<string, XType>();
            this._usings = new List<string>();
            this._staticusings = new List<String>();
            this._globalType = XType.CreateGlobalType(_file);
            this._classVars = new List<XTypeMember>();
            _types.Add(_globalType.Name, _globalType);
            if (this._file != null && this._file.Project != null)
            {
                if (this._file.Project.Loaded)       // this will fail if the project file is already unloaded
                {
                    if (this._file.Project.ProjectNode.PrefixClassesWithDefaultNamespace)
                    {
                        this._defaultNS = this._file.Project.ProjectNode.RootNameSpace;
                    }
                }
            }
        }

        private String currentNamespace
        {
            get
            {
                String ret = "";
                if (this._currentNSpaces.Count > 0)
                {
                    XType ns = this._currentNSpaces.Peek();
                    ret = ns.Name;
                }
                else if (!String.IsNullOrEmpty(this._defaultNS))
                {
                    ret = this._defaultNS;
                }
                return ret;
            }
        }

        public XFile File
        {
            get
            {
                return _file;
            }
        }

        public override void EnterSource([NotNull] XSharpParser.SourceContext context)
        {
            // Default namespaces
            addUsing("System", false);
            if (_file?.Project?.ProjectNode?.ParseOptions != null)
            {
                if (_file.Project.ProjectNode.ParseOptions.IsDialectVO)
                    addUsing("Vulcan", false);
            }
        }
        public override void ExitSource([NotNull] XSharpParser.SourceContext context)
        {
            // Reset TypeList for this file
            this.File.SetTypes(_types, _usings, _staticusings, false);
        }


        #region Types

        private XType currentType
        {
            get
            {
                if (_currentTypes.Count > 0)
                    return this._currentTypes.Peek();
                return null;
            }
        }

        private void popType()
        {
            if (_currentTypes.Count > 0)
            {

                this._currentTypes.Pop();
            }
        }

        private void pushType(XType type)
        {
            type.File = this._file;
            this._currentTypes.Push(type);
        }

        public override void EnterClass_([NotNull] XSharpParser.Class_Context context)
        {
            try
            {
                var tokens = context.Modifiers?._Tokens;
                XType newClass = new XType(context.Id?.GetText(),
                   Kind.Class,
                   decodeModifiers(tokens), decodeVisibility(tokens),
                   new TextRange(context), new TextInterval(context));
                //
                newClass.NameSpace = this.currentNamespace;
                // and push into the current Namespace
                //CurrentNamespace.Types.Add(newClass);
                // Static Class ?
                newClass.IsStatic = this.isStatic(tokens);
                // Partial Class ?
                newClass.IsPartial = this.isPartial(tokens);
                // INHERIT from ?
                if (context.BaseType != null)
                {
                    //newClass.BaseTypes.Add(new CodeTypeReference(context.BaseType.GetText()));
                    newClass.ParentName = context.BaseType.GetText();
                }
                // IMPLEMENTS ?
                if ((context._Implements != null) && (context._Implements.Count > 0))
                {
                    foreach (var interfaces in context._Implements)
                    {
                        //newClass.BaseTypes.Add(new CodeTypeReference(interfaces.GetText()));
                    }
                }
                if (newClass != null && newClass.FullName != null)
                {
                    newClass = addType(newClass);
                    // Set as Current working Class
                    pushType(newClass);
                }
            }
            catch { }
        }
        public override void ExitClass_([NotNull] XSharpParser.Class_Context context)
        {
            popType();
        }

        public override void ExitClassvars([NotNull] XSharpParser.ClassvarsContext context)
        {

            var type = this.currentType;
            if (type != null)
            {
                foreach (var v in _classVars)
                {
                    v.Parent = type;
                    type.AddMember(v);
                }
            }
            _classVars.Clear();

        }

        public override void EnterStructure_([NotNull] XSharpParser.Structure_Context context)
        {
            var tokens = context.Modifiers?._Tokens;
            XType newStruct = new XType(context.Id.GetText(),
                   Kind.Structure,
                   decodeModifiers(tokens),
                   decodeVisibility(tokens),
                   new TextRange(context), new TextInterval(context));
            //
            newStruct.NameSpace = this.currentNamespace;
            // and push into the current Namespace
            //CurrentNamespace.Types.Add(newClass);
            // Static Class ?
            newStruct.IsStatic = this.isStatic(tokens);
            // Partial Class ?
            newStruct.IsPartial = this.isPartial(tokens);
            // IMPLEMENTS ?
            if ((context._Implements != null) && (context._Implements.Count > 0))
            {
                foreach (var interfaces in context._Implements)
                {
                    //newClass.BaseTypes.Add(new CodeTypeReference(interfaces.GetText()));
                }
            }

            //
            newStruct = addType(newStruct);
            // Set as Current working Class
            pushType(newStruct);
        }
        public override void ExitStructure_([NotNull] XSharpParser.Structure_Context context)
        {
            popType();
        }

        public override void EnterInterface_([NotNull] XSharpParser.Interface_Context context)
        {
            var tokens = context.Modifiers?._Tokens;
            XType newIf = new XType(context.Id.GetText(),
                Kind.Interface,
                decodeModifiers(tokens),
                decodeVisibility(tokens),
                new TextRange(context), new TextInterval(context));
            //
            newIf.NameSpace = this.currentNamespace;
            // and push into the current Namespace
            //CurrentNamespace.Types.Add(newClass);
            // Static Class ?
            newIf.IsStatic = this.isStatic(tokens);
            // Partial Class ?
            newIf.IsPartial = this.isPartial(tokens);
            //
            newIf = addType(newIf);
            // Set as Current working Interface
            pushType(newIf);
        }
        public override void ExitInterface_([NotNull] XSharpParser.Interface_Context context)
        {
            popType();
        }

        public override void EnterEnum_([NotNull] XSharpParser.Enum_Context context)
        {
            var tokens = context.Modifiers?._Tokens;
            XType newEnum = new XType(context.Id.GetText(),
                Kind.Enum,
                decodeModifiers(tokens),
                decodeVisibility(tokens),
                new TextRange(context), new TextInterval(context));
            //
            newEnum.NameSpace = this.currentNamespace;
            //
            newEnum = addType(newEnum);
            // Set as Current working Interface
            pushType(newEnum);
        }
        public override void ExitEnum_([NotNull] XSharpParser.Enum_Context context)
        {
            popType();
        }

        public override void EnterVostruct([NotNull] XSharpParser.VostructContext context)
        {
            var tokens = context.Modifiers?._Tokens;
            XType newStruct = new XType(context.Id.GetText(),
                    Kind.VOStruct,
                    decodeModifiers(tokens),
                    decodeVisibility(tokens),
                    new TextRange(context), new TextInterval(context));
            //
            // Todo additional properties ?
            newStruct.IsStatic = isStatic(tokens);
            newStruct = addType(newStruct);
            pushType(newStruct);
        }
        public override void ExitVostruct([NotNull] XSharpParser.VostructContext context)
        {
            popType();
        }

        public override void EnterVounion([NotNull] XSharpParser.VounionContext context)
        {
            var tokens = context.Modifiers?._Tokens;
            XType newStruct = new XType(context.Id.GetText(),
                    Kind.Union,
                    decodeModifiers(tokens),
                    decodeVisibility(tokens),
                    new TextRange(context), new TextInterval(context));
            //
            // Todo additional properties ?
            newStruct = addType(newStruct);
            pushType(newStruct);
        }
        public override void ExitVounion([NotNull] XSharpParser.VounionContext context)
        {
            popType();
        }

        // Delegate is strictly a type but handled as a GLobal method because it has parameters
        public override void EnterDelegate_([NotNull] XSharpParser.Delegate_Context context)
        {
            var tokens = context.Modifiers?._Tokens;
            XTypeMember newMethod = new XTypeMember(context.Id.GetText(),
                    Kind.Delegate,
                    decodeModifiers(tokens),
                    decodeVisibility(tokens),
                    new TextRange(context), new TextInterval(context),
                    isStatic(tokens));
            //
            // Todo additional properties ?
            addParameters(context.Params, newMethod);
            addGlobalMember(newMethod);
        }
        public override void ExitDelegate_([NotNull] XSharpParser.Delegate_Context context)
        {
            endMember(context);
        }

        #endregion


        #region Global Members
        private void addGlobalMember(XTypeMember member)
        {
            member.File = this._file;
            member.Parent = _globalType;
            _globalType.AddMember(member);
            this._currentMethod = member;

        }
        public override void EnterFunction([NotNull] XSharpParser.FunctionContext context)
        {
            var tokens = context.Modifiers?._Tokens;
            XTypeMember newMethod = new XTypeMember(context.Id.GetText(),
                Kind.Function,
                decodeModifiers(tokens),
                decodeVisibility(tokens),
                new TextRange(context), new TextInterval(context),
                (context.Type == null) ? "Void" : context.Type.GetText(),
                    isStatic(tokens));
            //
            addParameters(context.Params, newMethod);
            addGlobalMember(newMethod);
        }

        public override void ExitFunction([NotNull] XSharpParser.FunctionContext context)
        {
            endMember(context);
        }

        public override void EnterProcedure([NotNull] XSharpParser.ProcedureContext context)
        {
            var tokens = context.Modifiers?._Tokens;
            XTypeMember newMethod = new XTypeMember(context.Id.GetText(),
                Kind.Procedure,
                decodeModifiers(tokens),
                decodeVisibility(tokens),
                new TextRange(context), new TextInterval(context), false);
            //
            addParameters(context.Params, newMethod);
            addGlobalMember(newMethod);
        }
        public override void ExitProcedure([NotNull] XSharpParser.ProcedureContext context)
        {
            endMember(context);
        }

        public override void EnterVodll([NotNull] XSharpParser.VodllContext context)
        {
            var tokens = context.Modifiers?._Tokens;
            XTypeMember newMethod = new XTypeMember(context.ShortName,
                Kind.VODLL,
                decodeModifiers(tokens),
                decodeVisibility(tokens),
                new TextRange(context), new TextInterval(context),
                context.ReturnType == null ? "Void" : context.ReturnType.GetText(),
                    isStatic(tokens));

            //
            addParameters(context.Params, newMethod);
            addGlobalMember(newMethod);
        }
        public override void ExitVodll([NotNull] XSharpParser.VodllContext context)
        {
            endMember(context);
        }
        public override void EnterVoglobal([NotNull] XSharpParser.VoglobalContext context)
        {
            var tokens = context.Modifiers?._Tokens;
            this._currentVarVisibility = decodeVisibility(tokens);
            this._currentVarStatic = isStatic(tokens);
        }
        public override void ExitVoglobal([NotNull] XSharpParser.VoglobalContext context)
        {

            foreach (var member in _classVars)
            {
                // convert from classvars to voglobal

                var newMember = new XTypeMember(member.Name, Kind.VOGlobal, member.Modifiers,
                    member.Visibility, member.Range, member.Interval, member.TypeName, _currentVarStatic);
                addGlobalMember(newMember);
            }
            _classVars.Clear();
            endMember(context);
        }

        public override void EnterVodefine([NotNull] XSharpParser.VodefineContext context)
        {
            var tokens = context.Modifiers?._Tokens;
            XTypeMember newMethod = new XTypeMember(context.ShortName,
                Kind.VODefine,
                decodeModifiers(tokens),
                decodeVisibility(tokens),
                new TextRange(context), new TextInterval(context),
                context.ReturnType == null ? "Void" : context.ReturnType.GetText(),
                    isStatic(tokens));
            //
            if (context.Expr != null)
            {
                newMethod.Suffix = " := " + context.Expr.GetText();
            }
            addGlobalMember(newMethod);

        }

        public override void ExitVodefine([NotNull] XSharpParser.VodefineContext context)
        {
            endMember(context);
        }
        #endregion

        #region Members

        private XType addType(XType newType)
        {
            if (_types.ContainsKey(newType.FullName))
                return newType;
            _types.Add(newType.FullName, newType);
            return newType;
        }

        private void addUsing(string name, bool staticUsing)
        {
            List<string> list;
            if (staticUsing)
                list = _staticusings;
            else
                list = _usings;

            // check for duplicate usings
            foreach (var u in list)
            {
                if (string.Compare(u, name, true) == 0)
                    return;
            }
            list.Add(name);
        }

        private void addMember(XTypeMember member)
        {
            var type = this.currentType;
            member.File = this._file;

            if (type != null)
            {
                member.Parent = type;
                type.AddMember(member);
                this._currentMethod = member;
            }
            if (ModelWalker.IsSuspended && System.Threading.Thread.CurrentThread.IsBackground)
            {
                System.Threading.Thread.Sleep(100);
            }
        }
        protected virtual  void endMember(LanguageService.SyntaxTree.ParserRuleContext context)
        {
            _currentMethod = null;
        }

        public override void EnterVostructmember([NotNull] XSharpParser.VostructmemberContext context)
        {
            XTypeMember newMember = new XTypeMember(context.Id.GetText(),
                    Kind.ClassVar,
                    Modifiers.Public,
                    Modifiers.Public,
                    new TextRange(context), new TextInterval(context), false);
            //
            // Todo additional properties ?
            newMember.IsArray = context.Dim != null;
            addMember(newMember);
        }
        public override void ExitVostructmember([NotNull] XSharpParser.VostructmemberContext context)
        {
            endMember(context);
        }

        public override void EnterEnummember([NotNull] XSharpParser.EnummemberContext context)
        {
            XTypeMember newMember = new XTypeMember(context.Id.GetText(),
                Kind.EnumMember,
                Modifiers.None,
                Modifiers.None,
                new TextRange(context), new TextInterval(context),
                "", false);
            //
            addMember(newMember);
        }
        public override void ExitEnummember([NotNull] XSharpParser.EnummemberContext context)
        {
            endMember(context);
        }



        public override void EnterConstructor([NotNull] XSharpParser.ConstructorContext context)
        {
            var tokens = context.Modifiers?._Tokens;
            XTypeMember newMethod = new XTypeMember("Constructor",
                Kind.Constructor,
                decodeModifiers(tokens),
                decodeVisibility(tokens),
                new TextRange(context), new TextInterval(context),
                    isStatic(tokens));
            //
            addParameters(context.Params, newMethod);
            addMember(newMethod);

        }
        public override void ExitConstructor([NotNull] XSharpParser.ConstructorContext context)
        {
            endMember(context);
        }


        public override void EnterMethod([NotNull] XSharpParser.MethodContext context)
        {
            Kind kind = Kind.Method;
            if (context.Name.Contains(":Access"))
            {
                kind = Kind.Access;
            }
            else if (context.Name.Contains(":Assign"))
            {
                kind = Kind.Assign;
            }
            //
            var tokens = context.Modifiers?._Tokens;
            XTypeMember newMethod = new XTypeMember(context.Id.GetText(),
                kind,
                decodeModifiers(tokens),
                decodeVisibility(tokens),
                new TextRange(context), new TextInterval(context),
                (context.Type == null) ? "Void" : context.Type.GetText(),
                    isStatic(tokens));
            //
            addParameters(context.Params, newMethod);
            addMember(newMethod);
        }

        public override void ExitMethod([NotNull] XSharpParser.MethodContext context)
        {
            endMember(context);
        }


        public override void EnterPropertyAccessor([NotNull] XSharpParser.PropertyAccessorContext context)
        {

        }
        public override void ExitPropertyAccessor([NotNull] XSharpParser.PropertyAccessorContext context)
        {
        }
        public override void EnterProperty([NotNull] XSharpParser.PropertyContext context)
        {
            String name = "";
            //
            if (context.Id != null)
                name = context.Id.GetText();
            if (context.SELF() != null)
                name = context.SELF()?.GetText();
            var tokens = context.Modifiers?._Tokens;
            XTypeMember newMethod = new XTypeMember(name,
                Kind.Property,
                decodeModifiers(tokens),
                decodeVisibility(tokens),
                new TextRange(context), new TextInterval(context),
                (context.Type == null) ? "Void" : context.Type.GetText(),
                    isStatic(tokens));
            //
            addParameters(context.Params, newMethod);
            //
            addMember(newMethod);
        }

        public override void ExitProperty([NotNull] XSharpParser.PropertyContext context)
        {
            endMember(context);
        }


        public override void EnterDestructor([NotNull] XSharpParser.DestructorContext context)
        {
            var tokens = context.Modifiers?._Tokens;
            XTypeMember newMethod = new XTypeMember("Destructor",
                Kind.Destructor,
                decodeModifiers(tokens),
                decodeVisibility(tokens),
                new TextRange(context), new TextInterval(context), "Void",
                    false);
            //
            addParameters(context.Params, newMethod);
            addMember(newMethod);
        }
        public override void ExitDestructor([NotNull] XSharpParser.DestructorContext context)
        {
            endMember(context);
        }

        public override void EnterEvent_([NotNull] XSharpParser.Event_Context context)
        {
            var tokens = context.Modifiers?._Tokens;
            XTypeMember newMethod = new XTypeMember(context.ShortName,
                Kind.Event,
                decodeModifiers(tokens),
                decodeVisibility(tokens),
                new TextRange(context), new TextInterval(context),
                context.ReturnType == null ? "Void" : context.ReturnType.GetText(), isStatic(tokens));
            //
            addParameters(context.Params, newMethod);
            addMember(newMethod);
        }
        public override void ExitEvent_([NotNull] XSharpParser.Event_Context context)
        {
            endMember(context);
        }
        public override void EnterEventAccessor([NotNull] XSharpParser.EventAccessorContext context)
        {
            // todo
        }
        public override void ExitEventAccessor([NotNull] XSharpParser.EventAccessorContext context)
        {
            // todo
        }
        public override void EnterOperator_([NotNull] XSharpParser.Operator_Context context)
        {
            var tokens = context.Modifiers?._Tokens;
            XTypeMember newMethod = new XTypeMember(context.ShortName,
                Kind.Operator,
                decodeModifiers(tokens),
                decodeVisibility(tokens),
                new TextRange(context), new TextInterval(context),
                context.ReturnType == null ? "Void" : context.ReturnType.GetText(),
                    isStatic(tokens));
            //
            addParameters(context.Params, newMethod);
            addMember(newMethod);
        }
        public override void ExitOperator_([NotNull] XSharpParser.Operator_Context context)
        {
            endMember(context);
        }

        public override void EnterClassvarModifiers([NotNull] XSharpParser.ClassvarModifiersContext context)
        {
            var tokens = context._Tokens;
            this._currentVarVisibility = decodeVisibility(tokens);
            this._currentVarStatic = isStatic(tokens);
        }
        public override void EnterClassVarList([NotNull] XSharpParser.ClassVarListContext context)
        {
            XSharpParser.ClassVarListContext current = (XSharpParser.ClassVarListContext)context;
            //
            // structure is:
            /*
            classvars			: (Attributes=attributes)? (Modifiers=classvarModifiers)? Vars=classVarList eos
                                ;

            classVarList		: Var+=classvar (COMMA Var+=classvar)* (As=(AS | IS) DataType=datatype)?
                                ;

            classvar			: (Dim=DIM)? Id=identifier (LBRKT ArraySub=arraysub RBRKT)? (ASSIGN_OP Initializer=expression)?
                                ;
            */
            // we want to include the stop position of the datatype in the classvar
            // so we set the range to the whole classvarlist 
            var parent = context.Parent as XSharpParserRuleContext;
            int count, currentVar;
            count = current._Var.Count;
            currentVar = 0;
            foreach (var varContext in current._Var)
            {
                //
                var mods = _currentVarStatic ? Modifiers.Static : Modifiers.None;
                // when many variables then the first one is from the start of the line until the 
                // end of its name, and the others start with the start of their name
                // the last one finishes at the end of the line
                currentVar += 1;
                var start = parent.Start;       // LOCAL keyword or GLOBAL keyword
                var stop = current.Stop;        // end of this line (datatype clause)
                if (currentVar > 1)
                {
                    start = varContext.Start;
                }
                if (currentVar < count) // end at this variable name
                {
                    stop = varContext.Stop;
                }
                var interval = new TextInterval(start.StartIndex, stop.StopIndex);
                string typeName = current.DataType != null ? current.DataType.GetText() : "USUAL";
                XTypeMember newClassVar = new XTypeMember(varContext.Id.GetText(),
                    Kind.ClassVar, mods, this._currentVarVisibility,
                    new TextRange(start.Line, start.Column, stop.Line, stop.Column + stop.Text.Length),
                    interval, typeName, _currentVarStatic);
                newClassVar.File = this._file;
                newClassVar.IsArray = varContext.Dim != null;
                //
                bool bAdd = true; 
                // Suppress classvars generated by errors in the source
                if (_errors != null)
                {
                    foreach (var err in _errors)
                    {
                        if (err.Location.IsInSource)
                        {
                            var ls = err.Location.GetLineSpan();
                            // Roslyns line numbers are 0 based
                            // we have 1 based line numbers
                            if (ls.StartLinePosition.Line == newClassVar.Range.StartLine-1)
                            {
                                bAdd = false;
                            }
                        }
                    }
                }
                if (bAdd)
                {
                    this._classVars.Add(newClassVar);
                }
            }
        }

        #endregion

        #region Namespaces
        public override void EnterUsing_([NotNull] XSharpParser.Using_Context context)
        {
            XSharpParser.Using_Context use = (XSharpParser.Using_Context)context;
            addUsing(use.Name?.GetText(), use.Static != null);
        }

        public override void ExitUsing_([NotNull] XSharpParser.Using_Context context)
        {
        }

        public override void EnterNamespace_([NotNull] XSharpParser.Namespace_Context context)
        {
            XSharpParser.Namespace_Context current = (XSharpParser.Namespace_Context)context;
            //
            XType nSpace = new XType(current.Name.GetText(), Kind.Namespace, Modifiers.None,
                Modifiers.Public, new TextRange(context), new TextInterval(context));
            this._currentNSpaces.Push(nSpace);
        }

        public override void ExitNamespace_([NotNull] XSharpParser.Namespace_Context context)
        {
            if (this._currentNSpaces.Count > 0)
            {
                XType cNS = this._currentNSpaces.Peek();
                // Is it necessary to Add the NameSpace in the TypeList ??
                this.addType(cNS);
                // Pop to support nested NameSpaces
                this._currentNSpaces.Pop();
            }
        }

        #endregion

        #region Method Body


        private void addParameters(IList<XSharpParser.ParameterContext> ctxtParams, XTypeMember newMethod)
        {
            if (ctxtParams != null)
            {
                foreach (XSharpParser.ParameterContext param in ctxtParams)
                {
                    XVariable var = new XVariable(newMethod, param.Id?.GetText(), Kind.Parameter, Modifiers.Public,
                        new TextRange(param), new TextInterval(param),
                        param.Type?.GetText(), true);
                    var.File = this._file;

                    //
                    newMethod.Parameters.Add(var);
                }
            }
        }

        public override void ExitForeachStmt([NotNull] XSharpParser.ForeachStmtContext context)
        {
            // LOCAL, IMPLIED or VAR ??
            base.ExitForeachStmt(context);
        }


        #endregion


        #region Helpers

        private Modifiers decodeModifiers(IList<IToken> tokens)
        {
            Modifiers retValue = Modifiers.None;
            if (tokens != null)
            {
                foreach (var token in tokens)
                {
                    switch (token.Type)
                    {
                        case XSharpParser.ABSTRACT:
                            retValue |= Modifiers.Abstract;
                            break;
                        case XSharpParser.NEW:
                            retValue |= Modifiers.New;
                            break;
                        case XSharpParser.SEALED:
                            retValue |= Modifiers.Sealed;
                            break;
                        case XSharpParser.UNSAFE:
                            retValue |= Modifiers.Unsafe;
                            break;
                    }
                }
            }
            return retValue;
        }

        private Modifiers decodeVisibility(IList<IToken> tokens)
        {
            Modifiers retValue = Modifiers.Public;
            if (tokens != null)
            {
                bool bHasProtect = false;
                bool bHasInternal = false;
                foreach (var token in tokens)
                {
                    switch (token.Type)
                    {
                        case XSharpParser.INTERNAL:
                            bHasInternal = true;
                            if (bHasProtect)
                                retValue = Modifiers.ProtectedInternal;
                            else
                                retValue = Modifiers.Internal;
                            break;
                        case XSharpParser.PROTECTED:
                            bHasProtect = true;
                            if (bHasInternal)
                                retValue = Modifiers.ProtectedInternal;
                            else
                                retValue = Modifiers.Protected;
                            break;
                        case XSharpParser.PRIVATE:
                        case XSharpParser.HIDDEN:
                            retValue = Modifiers.Private;
                            break;
                        case XSharpParser.EXPORT:
                        case XSharpParser.PUBLIC:
                            //
                            retValue = Modifiers.Public;
                            break;
                    }
                }
            }
            return retValue;
        }

        private bool isStatic(IList<IToken> modifiers)
        {
            bool retValue = false;
            if (modifiers != null)
            {
                retValue = modifiers.Where(t => t.Type == XSharpParser.STATIC).Count() > 0;
            }
            //
            return retValue;
        }

        private bool isPartial(IList<IToken> modifiers)
        {
            bool retValue = false;
            if (modifiers != null)
            {
                retValue = modifiers.Where(t => t.Type == XSharpParser.PARTIAL).Count() > 0;
            }
            //
            return retValue;
        }

        protected String buildLiteralValue(XSharpParser.LiteralValueContext context)
        {
            string value = "";
            //
            switch (context.Token.Type)
            {
                case XSharpParser.BIN_CONST:
                case XSharpParser.HEX_CONST:
                case XSharpParser.INT_CONST:
                    value = "Int";
                    break;
                case XSharpParser.REAL_CONST:
                    value = "Double"; // or Real8 ??
                    break;
                case XSharpParser.TRUE_CONST:
                case XSharpParser.FALSE_CONST:
                    value = "Logic";
                    break;
                case XSharpParser.STRING_CONST:
                case XSharpParser.ESCAPED_STRING_CONST:
                    value = "String";
                    break;
                case XSharpParser.CHAR_CONST:
                    break;
                case XSharpParser.NULL:
                    break;
                case XSharpParser.NIL:
                case XSharpParser.NULL_ARRAY:
                case XSharpParser.NULL_CODEBLOCK:
                case XSharpParser.NULL_DATE:
                case XSharpParser.NULL_OBJECT:
                case XSharpParser.NULL_PSZ:
                case XSharpParser.NULL_PTR:
                case XSharpParser.NULL_STRING:
                case XSharpParser.NULL_SYMBOL:
                case XSharpParser.SYMBOL_CONST:
                case XSharpParser.DATE_CONST:
                default:
                    break;
            }
            return value;
        }

        protected XCodeTypeReference buildDataType(XSharpParser.DatatypeContext context)
        {
            //
            // Datatype is ptrDatatype
            // or arrayDatatype
            // or simpleDatatype
            // or nullableDatatype 
            // they all have a TypeName
            XSharpParser.TypeNameContext tn = null;
            if (context is XSharpParser.PtrDatatypeContext)
            {
                XSharpParser.PtrDatatypeContext ptrData = (XSharpParser.PtrDatatypeContext)context;
                tn = ptrData.TypeName;
            }
            else if (context is XSharpParser.ArrayDatatypeContext)
            {
                XSharpParser.ArrayDatatypeContext aData = (XSharpParser.ArrayDatatypeContext)context;
                tn = aData.TypeName;
            }
            else if (context is XSharpParser.SimpleDatatypeContext)
            {
                XSharpParser.SimpleDatatypeContext sdt = (XSharpParser.SimpleDatatypeContext)context;
                tn = sdt.TypeName;
            }
            else if (context is XSharpParser.NullableDatatypeContext)
            {
                XSharpParser.NullableDatatypeContext ndc = context as XSharpParser.NullableDatatypeContext;
                tn = ndc.TypeName;
            }
            //
            XCodeTypeReference expr = null;
            if (tn.NativeType != null)
            {
                expr = buildNativeType(tn.NativeType);
            }
            else if (tn.XType != null)
            {
                expr = buildXBaseType(tn.XType);
            }
            else if (tn.Name != null)
            {
                expr = buildName(tn.Name);
            }
            //
            return expr;
        }
        protected virtual void addVariables([NotNull] ParserRuleContext context)
        {

        }
        protected XCodeTypeReference buildName(XSharpParser.NameContext context)
        {
            XCodeTypeReference expr = null;
            //
            var sName = context.GetText();
            if (context is XSharpParser.QualifiedNameContext)
            {
                XSharpParser.QualifiedNameContext qual = (XSharpParser.QualifiedNameContext)context;
                expr = buildName(qual.Left);
                expr = buildTypeReference(expr.TypeName + "." + buildSimpleName(qual.Right).TypeName);
            }
            else if (context is XSharpParser.SimpleOrAliasedNameContext)
            {
                var alias = context as XSharpParser.SimpleOrAliasedNameContext;
                var name = alias.Name as XSharpParser.AliasedNameContext;

                //
                if (name is XSharpParser.AliasQualifiedNameContext)
                {
                    XSharpParser.AliasQualifiedNameContext al = (XSharpParser.AliasQualifiedNameContext)name;
                    expr = buildSimpleName(al.Right);
                    expr = buildTypeReference(al.Alias.GetText() + "::" + expr.TypeName);
                }
                else if (name is XSharpParser.GlobalQualifiedNameContext)
                {
                    var gqn = name as XSharpParser.GlobalQualifiedNameContext;
                    expr = buildSimpleName(gqn.Right);
                    expr = buildTypeReference("global::" + expr.TypeName);
                }
                else if (name is XSharpParser.IdentifierOrGenericNameContext)
                {
                    var id = name as XSharpParser.IdentifierOrGenericNameContext;
                    expr = buildSimpleName(id.Name);
                }
            }
            //
            return expr;
        }

        protected XCodeTypeReference buildTypeReference(string name)
        {
            return new XCodeTypeReference(name);
        }

        protected XCodeTypeReference buildNativeType(XSharpParser.NativeTypeContext nativeType)
        {
            //
            Type type;
            switch (nativeType.Token.Type)
            {
                case XSharpParser.BYTE:
                    type = typeof(System.Byte);
                    break;
                case XSharpParser.DWORD:
                    type = typeof(System.UInt32);
                    break;
                case XSharpParser.SHORTINT:
                    type = typeof(System.Int16);
                    break;
                case XSharpParser.INT:
                case XSharpParser.LONGINT:
                    type = typeof(System.Int32);
                    break;
                case XSharpParser.INT64:
                    type = typeof(System.Int64);
                    break;
                case XSharpParser.UINT64:
                    type = typeof(System.UInt64);
                    break;
                case XSharpParser.LOGIC:
                    type = typeof(System.Boolean);
                    break;
                case XSharpParser.OBJECT:
                    type = typeof(System.Object);
                    break;
                case XSharpParser.REAL4:
                    type = typeof(System.Single);
                    break;
                case XSharpParser.REAL8:
                    type = typeof(System.Double);
                    break;
                case XSharpParser.STRING:
                    type = typeof(System.String);
                    break;
                case XSharpParser.WORD:
                    type = typeof(System.UInt16);
                    break;
                case XSharpParser.VOID:
                    type = typeof(void);
                    break;
                case XSharpParser.DYNAMIC:
                    type = typeof(System.Object);
                    break;
                default:
                    var strType = nativeType.Token.Text;
                    return buildTypeReference(strType);
            }
            return new XCodeTypeReference(type);
        }

        protected XCodeTypeReference buildXBaseType(XSharpParser.XbaseTypeContext xbaseType)
        {
            return new XCodeTypeReference(xbaseType.Token.Text);
        }

        protected XCodeTypeReference buildSimpleName(XSharpParser.SimpleNameContext simpleName)
        {
            XCodeTypeReference expr = null;
            //
            string name = simpleName.Id.GetText();
            string gen = "";
            if (simpleName.GenericArgList != null)
            {
                string argList = "";
                int i = 0;
                foreach (var generic in simpleName.GenericArgList._GenericArgs)
                {
                    if (i > 0)
                        argList += ",";
                    var tmp = buildDataType(generic);
                    argList += tmp.TypeName;
                    i++;
                }
                //
                //gen = "`" + i.ToString() + "[" + argList + "]";
                gen = "<" + argList + ">";
            }
            expr = buildTypeReference(name + gen);
            //
            return expr;
        }

        internal class XCodeTypeReference
        {

            internal String TypeName { get; set; }
            internal XCodeTypeReference(string typeName)
            {
                TypeName = typeName;
            }

            internal XCodeTypeReference(System.Type type)
            {
                TypeName = type.Name;
            }

        }


        #endregion
    }
}
