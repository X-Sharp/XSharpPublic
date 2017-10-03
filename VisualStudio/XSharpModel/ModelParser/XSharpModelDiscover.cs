//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
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
        private readonly XFile _file;
        private readonly Stack<XType> _currentTypes;
        private readonly Stack<XType> _currentNSpaces;
        private readonly Stack<XSharpParser.LocalvarContext> _localDecls;
        private readonly XSharpParseOptions _options;
        private readonly Dictionary<string, XType> _types;
        private readonly List<string> _usings;
        private readonly List<string> _staticusings;
        private readonly XType _globalType;
        private readonly List<XTypeMember> _classVars;
        private XTypeMember _currentMethod;
        private Modifiers _currentVarVisibility;
        private bool _currentVarStatic;
        private string _defaultNS;
        private readonly bool _buildLocals;
        public XSharpModelDiscover(XFile file, bool buildLocals)
        {
            // To store intermediate declarations
            this._file = file;
            this._buildLocals = buildLocals;
            this._localDecls = new Stack<XSharpParser.LocalvarContext>();
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
                    this._defaultNS = this._file.Project.ProjectNode.RootNameSpace;
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
            this.File.SetTypes(_types, _usings, _staticusings, _buildLocals);
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
                XType newClass = new XType(context.Id?.GetText(),
                   Kind.Class,
                   decodeModifiers(context.Modifiers?._Tokens),
                   decodeVisibility(context.Modifiers?._Tokens),
                   new TextRange(context), new TextInterval(context));
                //
                newClass.NameSpace = this.currentNamespace;
                // and push into the current Namespace
                //CurrentNamespace.Types.Add(newClass);
                // Static Class ?
                newClass.IsStatic = this.isStatic(context.Modifiers?._Tokens);
                // Partial Class ?
                newClass.IsPartial = this.isPartial(context.Modifiers?._Tokens);
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
                    type.Members.Add(v);
                }
            }
            _classVars.Clear();

        }

        public override void EnterStructure_([NotNull] XSharpParser.Structure_Context context)
        {
            XType newStruct = new XType(context.Id.GetText(),
                   Kind.Structure,
                   decodeModifiers(context.Modifiers?._Tokens),
                   decodeVisibility(context.Modifiers?._Tokens),
                   new TextRange(context), new TextInterval(context));
            //
            newStruct.NameSpace = this.currentNamespace;
            // and push into the current Namespace
            //CurrentNamespace.Types.Add(newClass);
            // Static Class ?
            newStruct.IsStatic = this.isStatic(context.Modifiers?._Tokens);
            // Partial Class ?
            newStruct.IsPartial = this.isPartial(context.Modifiers?._Tokens);
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
            XType newIf = new XType(context.Id.GetText(),
                Kind.Interface,
                decodeModifiers(context.Modifiers?._Tokens),
                decodeVisibility(context.Modifiers?._Tokens),
                new TextRange(context), new TextInterval(context));
            //
            newIf.NameSpace = this.currentNamespace;
            // and push into the current Namespace
            //CurrentNamespace.Types.Add(newClass);
            // Static Class ?
            newIf.IsStatic = this.isStatic(context.Modifiers?._Tokens);
            // Partial Class ?
            newIf.IsPartial = this.isPartial(context.Modifiers?._Tokens);
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
            XType newEnum = new XType(context.Id.GetText(),
                Kind.Enum,
                decodeModifiers(context.Modifiers?._Tokens),
                decodeVisibility(context.Modifiers?._Tokens),
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
            XType newStruct = new XType(context.Id.GetText(),
                    Kind.VOStruct,
                    decodeModifiers(context.Modifiers?._Tokens),
                    decodeVisibility(context.Modifiers?._Tokens),
                    new TextRange(context), new TextInterval(context));
            //
            // Todo additional properties ?

            newStruct = addType(newStruct);
            pushType(newStruct);
        }
        public override void ExitVostruct([NotNull] XSharpParser.VostructContext context)
        {
            popType();
        }

        public override void EnterVounion([NotNull] XSharpParser.VounionContext context)
        {
            XType newStruct = new XType(context.Id.GetText(),
                    Kind.Union,
                    decodeModifiers(context.Modifiers?._Tokens),
                    decodeVisibility(context.Modifiers?._Tokens),
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
            XTypeMember newMethod = new XTypeMember(context.Id.GetText(),
                    Kind.Delegate,
                    decodeModifiers(context.Modifiers?._Tokens),
                    decodeVisibility(context.Modifiers?._Tokens),
                    new TextRange(context), new TextInterval(context));
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
            _globalType.Members.Add(member);
            this._currentMethod = member;

        }
        public override void EnterFunction([NotNull] XSharpParser.FunctionContext context)
        {
            XTypeMember newMethod = new XTypeMember(context.Id.GetText(),
                Kind.Function,
                Modifiers.None,
                Modifiers.Public,
                new TextRange(context), new TextInterval(context),
                (context.Type == null) ? "Void" : context.Type.GetText());
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
            XTypeMember newMethod = new XTypeMember(context.Id.GetText(),
                Kind.Procedure,
                Modifiers.None,
                Modifiers.Public,
                new TextRange(context), new TextInterval(context));
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
            XTypeMember newMethod = new XTypeMember(context.ShortName,
                Kind.VODLL,
                decodeModifiers(context.Modifiers?._Tokens),
                decodeVisibility(context.Modifiers?._Tokens),
                new TextRange(context), new TextInterval(context),
                context.ReturnType == null ? "Void" : context.ReturnType.GetText());

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
            this._currentVarVisibility = decodeVisibility(context.Modifiers?._Tokens);
            this._currentVarStatic = isStatic(context.Modifiers?._Tokens);
        }
        public override void ExitVoglobal([NotNull] XSharpParser.VoglobalContext context)
        {

            foreach (var member in _classVars)
            {
                // convert from classvars to voglobal

                var newMember = new XTypeMember(member.Name, Kind.VOGlobal, member.Modifiers,
                    member.Visibility, member.Range, member.Interval, member.TypeName);
                addGlobalMember(newMember);
            }
            _classVars.Clear();
            endMember(context);
        }

        public override void EnterVodefine([NotNull] XSharpParser.VodefineContext context)
        {
            XTypeMember newMethod = new XTypeMember(context.ShortName,
                Kind.VODefine,
                decodeModifiers(context.Modifiers?._Tokens),
                decodeVisibility(context.Modifiers?._Tokens),
                new TextRange(context), new TextInterval(context),
                context.ReturnType == null ? "Void" : context.ReturnType.GetText());
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
                type.Members.Add(member);
                this._currentMethod = member;
            }
        }
        private void endMember(LanguageService.SyntaxTree.ParserRuleContext context)
        {
            addVariables(context);
            _currentMethod = null;
        }

        public override void EnterVostructmember([NotNull] XSharpParser.VostructmemberContext context)
        {
            XTypeMember newMember = new XTypeMember(context.Id.GetText(),
                    Kind.ClassVar,
                    Modifiers.Public,
                    Modifiers.Public,
                    new TextRange(context), new TextInterval(context));
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
                "");
            //
            addMember(newMember);
        }
        public override void ExitEnummember([NotNull] XSharpParser.EnummemberContext context)
        {
            endMember(context);
        }



        public override void EnterConstructor([NotNull] XSharpParser.ConstructorContext context)
        {
            XTypeMember newMethod = new XTypeMember("Constructor",
                Kind.Constructor,
                decodeModifiers(context.Modifiers?._Tokens),
                decodeVisibility(context.Modifiers?._Tokens),
                new TextRange(context), new TextInterval(context));
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
            XTypeMember newMethod = new XTypeMember(context.Id.GetText(),
                kind,
                decodeModifiers(context.Modifiers?._Tokens),
                decodeVisibility(context.Modifiers?._Tokens),
                new TextRange(context), new TextInterval(context),
                (context.Type == null) ? "Void" : context.Type.GetText());
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

            XTypeMember newMethod = new XTypeMember(name,
                Kind.Property,
                decodeModifiers(context.Modifiers?._Tokens),
                decodeVisibility(context.Modifiers?._Tokens),
                new TextRange(context), new TextInterval(context),
                (context.Type == null) ? "Void" : context.Type.GetText());
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
            XTypeMember newMethod = new XTypeMember("Destructor",
                Kind.Destructor,
                decodeModifiers(context.Modifiers?._Tokens),
                decodeVisibility(context.Modifiers?._Tokens),
                new TextRange(context), new TextInterval(context), "Void");
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
            XTypeMember newMethod = new XTypeMember(context.ShortName,
                Kind.Event,
                decodeModifiers(context.Modifiers?._Tokens),
                decodeVisibility(context.Modifiers?._Tokens),
                new TextRange(context), new TextInterval(context),
                context.ReturnType == null ? "Void" : context.ReturnType.GetText());
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
            XTypeMember newMethod = new XTypeMember(context.ShortName,
                Kind.Operator,
                decodeModifiers(context.Modifiers?._Tokens),
                decodeVisibility(context.Modifiers?._Tokens),
                new TextRange(context), new TextInterval(context),
                context.ReturnType == null ? "Void" : context.ReturnType.GetText());
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
            XSharpParser.ClassvarModifiersContext current = (XSharpParser.ClassvarModifiersContext)context;
            this._currentVarVisibility = decodeVisibility(current?._Tokens);
            this._currentVarStatic = isStatic(current?._Tokens);
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
                    interval, typeName);
                newClassVar.File = this._file;
                newClassVar.IsArray = varContext.Dim != null;
                //
                this._classVars.Add(newClassVar);
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
        public override void EnterLocalvar([NotNull] XSharpParser.LocalvarContext context)
        {
            if (!this._buildLocals)
                return;
            try
            {
                if (context.DataType != null)
                {
                    XVariable local;
                    String localType = context.DataType.GetText();
                    String localName;
                    // Push to stack so we can manage all contexts in one loop
                    _localDecls.Push(context);

                    while (_localDecls.Count > 0)
                    {
                        XSharpParser.LocalvarContext tmpContext = _localDecls.Pop();
                        localName = tmpContext.Id.GetText();
                        //
                        local = new XVariable(this._currentMethod, localName, Kind.Local, Modifiers.Public,
                            new TextRange(tmpContext), new TextInterval(tmpContext),
                            localType);
                        local.File = this._file;
                        local.IsArray = tmpContext.Dim != null;
                        //
                        if (this._currentMethod != null)
                        {
                            this._currentMethod.Locals.Add(local);
                        }
                    }
                }
                else
                {
                    // We may have something like
                    // LOCAL x,y as STRING
                    // for x, we don't have a DataType, so save it
                    _localDecls.Push(context);
                }
            }
            catch (Exception ex)
            {
                Support.Debug("EnterLocalvar : Error Walking {0}, at {1}/{2} : " + ex.Message, this.File.Name, context.Start.Line, context.Start.Column);
            }
        }

        private void addVariables([NotNull] ParserRuleContext context)
        {
            if (!this._buildLocals || this._currentMethod == null)
                return;
            // Don't forget to add Self and Super as Local vars
            if ((context is XSharpParser.ConstructorContext) ||
                (context is XSharpParser.DestructorContext) ||
                (context is XSharpParser.MethodContext) ||
                (context is XSharpParser.PropertyContext))
            {
                XVariable local;
                //
                local = new XVariable(this._currentMethod, "Self", Kind.Local, Modifiers.Public,
                    new TextRange(context), new TextInterval(context),
                    this._currentMethod.ParentName);
                //
                local.File = this._file;
                this._currentMethod.Locals.Add(local);
                //
                if (!String.IsNullOrEmpty(_currentMethod.Parent.ParentName))
                {
                    local = new XVariable(this._currentMethod, "Super", Kind.Local, Modifiers.Public,
                    new TextRange(context), new TextInterval(context),
                    this._currentMethod.Parent.ParentName);
                    local.File = this._file;
                    this._currentMethod.Locals.Add(local);
                }
                //

            }
        }

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

        public override void EnterImpliedvar([NotNull] XSharpParser.ImpliedvarContext context)
        {
            if (!this._buildLocals)
                return;
            try
            {
                if (context.Expression is XSharpParser.PrimaryExpressionContext)
                {
                    XSharpParser.PrimaryExpressionContext primaryEx = (XSharpParser.PrimaryExpressionContext)context.Expression;
                    XSharpParser.PrimaryContext primary = primaryEx.Expr;

                    if (primary is XSharpParser.LiteralExpressionContext)
                    {
                        XSharpParser.LiteralExpressionContext lit = (XSharpParser.LiteralExpressionContext)primary;
                        XVariable local;
                        String localType = buildLiteralValue(lit.Literal);
                        String localName;

                        localName = context.Id.GetText();
                        //
                        local = new XVariable(this._currentMethod, localName, Kind.Local, Modifiers.Public,
                            new TextRange(context), new TextInterval(context),
                            localType);
                        local.File = this._file;
                        local.IsArray = false;
                        //
                        if (this._currentMethod != null)
                        {
                            this._currentMethod.Locals.Add(local);
                        }
                    }
                    else if (primary is XSharpParser.NameExpressionContext )
                    {

                    }

                }
            }
            catch (Exception ex)
            {
                Support.Debug("EnterImpliedvar : Error Walking {0}, at {1}/{2} : " + ex.Message, this.File.Name, context.Start.Line, context.Start.Column);
            }
        }

        public override void EnterVarLocalDecl([NotNull] XSharpParser.VarLocalDeclContext context)
        {
            base.EnterVarLocalDecl(context);
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

        private String buildLiteralValue(XSharpParser.LiteralValueContext context)
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

        #endregion
    }
}
