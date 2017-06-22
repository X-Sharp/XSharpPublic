//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using LanguageService.SyntaxTree.Misc;
using LanguageService.SyntaxTree.Tree;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Text.Classification;


namespace XSharpModel
{
    partial class XSharpModelRegionDiscover : XSharpBaseListener
    {
        //public List<XSharpParser.IEntityContext> entities = new List<XSharpParser.IEntityContext>();
        private XFile _file;
        private Stack<XType> _currentTypes;
        private Stack<XType> _currentNSpaces;

        private XTypeMember _currentMethod;
        private Stack<XSharpParser.LocalvarContext> _localDecls;
        private Modifiers _currentVarVisibility;
        private string _defaultNS;
        private LanguageService.CodeAnalysis.XSharp.XSharpParseOptions _options;

        public XSharpModelRegionDiscover(XFile file)
        {
            // To store intermediate declarations
            this._file = file;
            this._localDecls = new Stack<XSharpParser.LocalvarContext>();
            //
            this._currentTypes = new Stack<XType>();
            this._currentNSpaces = new Stack<XType>();
            //
            this.tags = new List<ClassificationSpan>();
            this._options = file.Project.ProjectNode.ParseOptions;
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

            set
            {
                // Project may be null when this is a stand alone file
                this._file = value;
                if (this._file != null && this._file.Project != null)
                {
                    if (this._file.Project.Loaded)       // this will fail if the project file is already unloaded
                    {
                        this._defaultNS = this._file.Project.ProjectNode.RootNameSpace;
                    }
                }
            }
        }

        public override void EnterSource([NotNull] XSharpParser.SourceContext context)
        {
            if (this.BuildModel)
            {
                if (_reInitModel)
                {
                    // Reset TypeList for this file
                    this.File.InitTypeList();
                    // Default namespaces
                    _file.Usings.AddUnique("System");
                    if (_options != null && _options.IsDialectVO)
                        _file.Usings.AddUnique("Vulcan");

                    _reInitModel = false;
                }
            }
        }
        public override void ExitEveryRule([NotNull] ParserRuleContext context)
        {
            if (this.BuildRegionTags)
            {
                this.RegionExitEveryRule(context);
            }
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
            if (this.BuildModel && _currentTypes.Count > 0)
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
            if (this.BuildModel)
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
                        newClass = this._file.TypeList.AddUnique(newClass.FullName, newClass);
                        // Set as Current working Class
                        pushType(newClass);
                    }
                }
                catch { }
            }
        }
        public override void ExitClass_([NotNull] XSharpParser.Class_Context context)
        {
            popType();
        }
  
        public override void EnterStructure_([NotNull] XSharpParser.Structure_Context context)
        {
            if (this.BuildModel)
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
                newStruct = this._file.TypeList.AddUnique(newStruct.FullName, newStruct);
                // Set as Current working Class
                pushType(newStruct);
            }
        }
        public override void ExitStructure_([NotNull] XSharpParser.Structure_Context context)
        {
            popType();
        }

        public override void EnterInterface_([NotNull] XSharpParser.Interface_Context context)
        {
            if (this.BuildModel)
            {
                XType newIf = new XType(context.Id.GetText(),
                    Kind.Structure,
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
                newIf = this._file.TypeList.AddUnique(newIf.FullName, newIf);
                // Set as Current working Interface
                pushType(newIf);
            }
        }
        public override void ExitInterface_([NotNull] XSharpParser.Interface_Context context)
        {
            popType();
        }
  
        public override void EnterEnum_([NotNull] XSharpParser.Enum_Context context)
        {
            if (this.BuildModel)
            {
                XType newEnum = new XType(context.Id.GetText(),
                    Kind.Enum,
                    decodeModifiers(context.Modifiers?._Tokens),
                    decodeVisibility(context.Modifiers?._Tokens),
                    new TextRange(context), new TextInterval(context));
                //
                newEnum.NameSpace = this.currentNamespace;
                //
                newEnum = this._file.TypeList.AddUnique(newEnum.FullName, newEnum);
                // Set as Current working Interface
                pushType(newEnum);
            }
        }
        public override void ExitEnum_([NotNull] XSharpParser.Enum_Context context)
        {
            popType();
        }

        public override void EnterVostruct([NotNull] XSharpParser.VostructContext context)
        {
            if (this.BuildModel)
            {
                XType newStruct = new XType(context.Id.GetText(),
                        Kind.VOStruct,
                        decodeModifiers(context.Modifiers?._Tokens),
                        decodeVisibility(context.Modifiers?._Tokens),
                        new TextRange(context), new TextInterval(context));
                //
                // Todo additional properties ?

                newStruct = this._file.TypeList.AddUnique(newStruct.FullName, newStruct);
                pushType(newStruct);
            }
        }
        public override void ExitVostruct([NotNull] XSharpParser.VostructContext context)
        {
            popType();
        }

        public override void EnterVounion([NotNull] XSharpParser.VounionContext context)
        {
            if (this.BuildModel)
            {
                XType newStruct = new XType(context.Id.GetText(),
                        Kind.Union,
                        decodeModifiers(context.Modifiers?._Tokens),
                        decodeVisibility(context.Modifiers?._Tokens),
                        new TextRange(context), new TextInterval(context));
                //
                // Todo additional properties ?
                newStruct = this._file.TypeList.AddUnique(newStruct.FullName, newStruct);
                pushType(newStruct);
            }
        }
        public override void ExitVounion([NotNull] XSharpParser.VounionContext context)
        {
            popType();
        }

        // Delegate is strictly a type but handled as a GLobal method because it has parameters
        public override void EnterDelegate_([NotNull] XSharpParser.Delegate_Context context)
        {
            
            if (this.BuildModel)
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
            member.Parent = _file.GlobalType;
            this._file.GlobalType.Members.Add(member);
            this._currentMethod = member;

        }
        public override void EnterFunction([NotNull] XSharpParser.FunctionContext context)
        {
            if (this.BuildModel)
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

        }

        public override void ExitFunction([NotNull] XSharpParser.FunctionContext context)
        {
            endMember(context);
        }

        public override void EnterProcedure([NotNull] XSharpParser.ProcedureContext context)
        {
            if (this.BuildModel)
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
            XTypeMember newMethod = new XTypeMember(context.ShortName,
                Kind.VOGlobal,
                decodeModifiers(context.Modifiers?._Tokens),
                decodeVisibility(context.Modifiers?._Tokens),
                new TextRange(context), new TextInterval(context),
                context.ReturnType == null ? "Void" : context.ReturnType.GetText());
            //
            addGlobalMember(newMethod);
        }
        public override void ExitVoglobal([NotNull] XSharpParser.VoglobalContext context)
        {
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
            addGlobalMember(newMethod);

        }

        public override void ExitVodefine([NotNull] XSharpParser.VodefineContext context)
        {
            endMember(context);
        }
        #endregion

        #region Members

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
            //else
            //{
            //    if (System.Diagnostics.Debugger.IsAttached)
            //     System.Diagnostics.Debugger.Break();
            //}
        }
        private void endMember(LanguageService.SyntaxTree.ParserRuleContext context)
        {
            addVariables(context);
            _currentMethod = null;
        }

        public override void EnterVostructmember([NotNull] XSharpParser.VostructmemberContext context)
        {
            if (this.BuildModel)
            {
                XTypeMember newMember = new XTypeMember(context.Id.GetText(),
                        Kind.ClassVar,
                        Modifiers.Public,
                        Modifiers.Public,
                        new TextRange(context), new TextInterval(context));
                //
                // Todo additional properties ?
                addMember(newMember);
            }

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
            if (this.BuildModel)
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

        }
        public override void ExitConstructor([NotNull] XSharpParser.ConstructorContext context)
        {
            endMember(context);
        }


        public override void EnterMethod([NotNull] XSharpParser.MethodContext context)
        {
            if (this.BuildModel)
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
            if (this.BuildModel)
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
                new TextRange(context), new TextInterval(context),"Void");
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
        }
        public override void EnterClassVarList([NotNull] XSharpParser.ClassVarListContext context)
        {
            XSharpParser.ClassVarListContext current = (XSharpParser.ClassVarListContext)context;
            if (current.DataType != null)
            {
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
                foreach (var varContext in current._Var)
                {
                    //
                    XTypeMember newClassVar = new XTypeMember(varContext.Id.GetText(),
                        Kind.ClassVar, Modifiers.None, this._currentVarVisibility,
                        new TextRange(varContext.Start.Line, varContext.Start.Column, current.Stop.Line, current.Stop.Column),
                        new TextInterval(current), current.DataType.GetText());
                    newClassVar.File = this._file;
                    //
                    var type = this.currentType;
                    if (type != null)
                    {
                        newClassVar.Parent = type;
                        type.Members.Add(newClassVar);
                    }
                }
            }

        }

        #endregion

        #region Namespaces
        public override void EnterUsing_([NotNull] XSharpParser.Using_Context context)
        {
            if (this.BuildModel)
            {
                XSharpParser.Using_Context use = (XSharpParser.Using_Context)context;
                if ( use.Static == null)
                    this._file.Usings.AddUnique(use.Name?.GetText());
                else
                {
                    string typeName = use.Name?.GetText();
                    if (! string.IsNullOrEmpty(typeName))
                        this._file.UsingStatics.AddUnique(typeName);
                }
            }
        }

        public override void ExitUsing_([NotNull] XSharpParser.Using_Context context)
        {
        }

        public override void EnterNamespace_([NotNull] XSharpParser.Namespace_Context context)
        {
            if (this.BuildModel)
            {
                XSharpParser.Namespace_Context current = (XSharpParser.Namespace_Context)context;
                //
                XType nSpace = new XType(current.Name.GetText(), Kind.Namespace, Modifiers.None,
                    Modifiers.Public, new TextRange(context), new TextInterval(context));
                this._currentNSpaces.Push(nSpace);
            }
        }

        public override void ExitNamespace_([NotNull] XSharpParser.Namespace_Context context)
        {
            if (this.BuildModel && this._currentNSpaces.Count > 0)
            {
                XType cNS = this._currentNSpaces.Peek();
                // Is it necessary to Add the NameSpace in the TypeList ??
                this._file.TypeList.AddUnique(cNS.Name, cNS);
                // Pop to support nested NameSpaces
                this._currentNSpaces.Pop();
            }
        }

        #endregion

        #region Method Body
        public override void EnterLocalvar([NotNull] XSharpParser.LocalvarContext context)
        {
            if (!this._buildLocals  || ! this._buildModel)
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
                System.Diagnostics.Debug.WriteLine("EnterLocalvar : Error Walking {0}, at {1}/{2} : " + ex.Message, this.File.Name, context.Start.Line, context.Start.Column);
            }
        }

        private void addVariables([NotNull] ParserRuleContext context)
        {
            if (!this._buildLocals || ! this._buildModel || this._currentMethod == null)
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
                        param.Type?.GetText());
                    var.File = this._file;
                    //
                    newMethod.Parameters.Add(var);
                }
            }
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

        #endregion
    }
}
