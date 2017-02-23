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

        //private XType _currentType; //TODO: Should be a Stack to support NestedType
        private XTypeMember _currentMethod;
        private Stack<XSharpParser.LocalvarContext> _localDecls;
        private Modifiers _currentVarVisibility;
        private string _defaultNS;


        //private XType _currentNSpace;

        public XSharpModelRegionDiscover()
        {
            // To store intermediate declarations
            this._localDecls = new Stack<XSharpParser.LocalvarContext>();
            //
            this._currentTypes = new Stack<XType>();
            this._currentNSpaces = new Stack<XType>();
            //
            this.tags = new List<ClassificationSpan>();
        }

        private String CurrentNamespace
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
                this._file = value;
                if (this._file.Project.Loaded)       // this will fail if the project file is already unloaded
                {
                    this._defaultNS = this._file.Project.ProjectNode.RootNameSpace;
                }
            }
        }

        public override void EnterEveryRule([NotNull] ParserRuleContext context)
        {
            try
            {
                if (this.BuildModel)
                {
                    if (_reInitModel)
                    {
                        // Reset TypeList for this file
                        this.File.InitTypeList();
                        _reInitModel = false;
                    }
                    /*
                      // The following types implement IEntityContext
                      // in short: every element in the language that can have a "method body", calling convention etc.
                      // Please note that Namespace is not in here and ClassVars are also not in here
                      // Also note that Method includes Access & Assign
                      // The only  property inside IEntityContext is the Data property (type EntityData), a flags enum.
                      // that stores info such as ClipperCallingConvention, UsesPSZ etc.
                      // This is used for VO/Vulcan compatible code generation
                      // For names, type etc you need to cast to the proper type
                      //
                      public partial class ProcedureContext : ParserRuleContext, IEntityContext {
                      public partial class FunctionContext : ParserRuleContext, IEntityContext {
                      public partial class MethodContext : ParserRuleContext, IEntityContext {
                      public partial class PropertyContext : ParserRuleContext, IEntityContext {
                      public partial class PropertyAccessorContext : ParserRuleContext, IEntityContext
                      public partial class ClsctorContext : ClassmemberContext, IEntityContext {
                      public partial class ClsdtorContext : ClassmemberContext, IEntityContext {
                      public partial class Event_Context : ParserRuleContext, IEntityContext {
                      public partial class EventAccessorContext : ParserRuleContext, IEntityContext
                      public partial class Operator_Context : ParserRuleContext, IEntityContext {
                      public partial class Delegate_Context : ParserRuleContext, IEntityContext {
                      public partial class Class_Context : ParserRuleContext, IEntityContext {
                      public partial class Structure_Context : ParserRuleContext, IEntityContext {
                      public partial class VodllContext : ParserRuleContext, IEntityContext {
                      public partial class VoglobalContext : ParserRuleContext, IEntityContext
                     */
                    if (context is XSharpParser.IEntityContext)
                    {
                        if (context is XSharpParser.Class_Context)
                        {
                            XType newClass = this.FromClass((XSharpParser.Class_Context)context);
                            newClass.File = this._file;
                            //
                            newClass = this._file.TypeList.AddUnique(newClass.FullName, newClass);
                            // Set as Current working Class
                            this._currentTypes.Push(newClass);
                        }
                        else if (context is XSharpParser.Structure_Context)
                        {
                            XType newClass = this.FromStructure((XSharpParser.Structure_Context)context);
                            newClass.File = this._file;
                            //
                            newClass = this._file.TypeList.AddUnique(newClass.FullName, newClass);
                            // Set as Current working Class
                            this._currentTypes.Push(newClass);
                        }
                        else if (context is XSharpParser.Interface_Context)
                        {
                            XType newIf = this.FromInterface((XSharpParser.Interface_Context)context);
                            newIf.File = this._file;
                            //
                            newIf = this._file.TypeList.AddUnique(newIf.FullName, newIf);
                            // Set as Current working Interface
                            this._currentTypes.Push(newIf);
                        }
                        else if (context is XSharpParser.ClsctorContext)
                        {
                            XSharpParser.ClsctorContext current = (XSharpParser.ClsctorContext)context;
                            XTypeMember newMethod = this.FromCtor((XSharpParser.ClsctorContext)context);
                            newMethod.File = this._file;
                            if (this._currentTypes.Count > 0)
                            {
                                newMethod.Parent = this._currentTypes.Peek();
                                this._currentTypes.Peek().Members.Add(newMethod);
                            }
                            else
                            {
                                if (System.Diagnostics.Debugger.IsAttached)
                                    System.Diagnostics.Debugger.Break();
                            }
                            this._currentMethod = newMethod;
                        }
                        // clsdtor
                        else if (context is XSharpParser.MethodContext)
                        {
                            XSharpParser.MethodContext current = (XSharpParser.MethodContext)context;
                            XTypeMember newMethod = this.FromMethod((XSharpParser.MethodContext)context);
                            newMethod.File = this._file;
                            if (this._currentTypes.Count > 0)
                            {
                                newMethod.Parent = this._currentTypes.Peek();
                                this._currentTypes.Peek().Members.Add(newMethod);
                            }
                            //else
                            //{
                            //    if (System.Diagnostics.Debugger.IsAttached)
                            //        System.Diagnostics.Debugger.Break();
                            //}
                            this._currentMethod = newMethod;
                        }
                        else if (context is XSharpParser.PropertyContext)
                        {
                            XSharpParser.PropertyContext current = (XSharpParser.PropertyContext)context;
                            XTypeMember newMethod = this.FromProperty((XSharpParser.PropertyContext)context);
                            newMethod.File = this._file;
                            if (this._currentTypes.Count > 0)
                            {
                                newMethod.Parent = this._currentTypes.Peek();
                                this._currentTypes.Peek().Members.Add(newMethod);
                            }
                            //else
                            //{
                            //    if (System.Diagnostics.Debugger.IsAttached)
                            //        System.Diagnostics.Debugger.Break();
                            //}
                            this._currentMethod = newMethod;
                        }
                        // propertyaccessor
                        else if (context is XSharpParser.FunctionContext)
                        {
                            XSharpParser.FunctionContext current = (XSharpParser.FunctionContext)context;
                            XTypeMember newMethod = this.FromFunction((XSharpParser.FunctionContext)context);
                            newMethod.File = this._file;
                            newMethod.Parent = _file.GlobalType;
                            this._file.GlobalType.Members.Add(newMethod);
                            this._currentMethod = newMethod;
                        }
                        else if (context is XSharpParser.ProcedureContext)
                        {
                            XSharpParser.ProcedureContext current = (XSharpParser.ProcedureContext)context;
                            XTypeMember newMethod = this.FromProcedure((XSharpParser.ProcedureContext)context);
                            newMethod.File = this._file;
                            newMethod.Parent = _file.GlobalType;
                            this._file.GlobalType.Members.Add(newMethod);
                            this._currentMethod = newMethod;
                        }
                        // event
                        // eventaccessor
                        // vodll
                        // voglobal
                        // delegate
                        // operator
                        //entities.Add((XSharpParser.IEntityContext)context);
                    }
                    else if (context is XSharpParser.Using_Context)
                    {
                        XSharpParser.Using_Context use = (XSharpParser.Using_Context)context;
                        //if (use.Name != null)
                            this._file.Usings.AddUnique(use.Name.GetText());
                    }
                    else if (context is XSharpParser.Namespace_Context)
                    {
                        XSharpParser.Namespace_Context current = (XSharpParser.Namespace_Context)context;
                        //
                        XType nSpace = new XType(current.Name.GetText(), Kind.Namespace, Modifiers.None,
                            Modifiers.Public, new TextRange(context), new TextInterval(context));
                        this._currentNSpaces.Push(nSpace);
                    }
                    else if (context is XSharpParser.ClassvarModifiersContext)
                    {
                        XSharpParser.ClassvarModifiersContext current = (XSharpParser.ClassvarModifiersContext)context;
                        this._currentVarVisibility = decodeVisibility(current?._Tokens);
                    }
                    else if (context is XSharpParser.ClassVarListContext)
                    {
                        XSharpParser.ClassVarListContext current = (XSharpParser.ClassVarListContext)context;
                        if (current.DataType != null)
                        {
                            //
                            foreach (var varContext in current._Var)
                            {
                                //
                                Kind kind = Kind.ClassVar;
                                //
                                XTypeMember newClassVar = new XTypeMember(varContext.Id.GetText(),
                                    kind, Modifiers.None, this._currentVarVisibility,
                                    new TextRange(varContext), new TextInterval(varContext), current.DataType.GetText());
                                //
                                if (this._currentTypes != null && _currentTypes.Count > 0)
                                {
                                    newClassVar.Parent = this._currentTypes.Peek();
                                    this._currentTypes.Peek().Members.Add(newClassVar);
                                }
                            }
                        }
                    }
                }
            }
            catch ( Exception ex )
            {
                System.Diagnostics.Debug.WriteLine( "Error Walking {0}, at {1}/{2} : " + ex.Message, this.File.Name, context.Start.Line, context.Start.Column);
            }
        }

        public override void EnterLocalvar([NotNull] XSharpParser.LocalvarContext context)
        {
            if (this.BuildModel)
            {
                if (this._currentMethod != null)
                {
                    if (context.DataType != null)
                    {
                        XVariable local;
                        String localType = context.DataType.GetText();
                        String localName;
                        // Any previous Local ?
                        while (_localDecls.Count > 0)
                        {
                            XSharpParser.LocalvarContext tmpContext = _localDecls.Pop();
                            localName = context.Id.GetText();
                            //
                            local = new XVariable(this._currentMethod, localName, Kind.Local, Modifiers.Public,
                                new TextRange(tmpContext), new TextInterval(tmpContext),
                                localType);
                            local.File = this._file;
                            //
                            this._currentMethod.Locals.Add(local);
                        }
                        // Now, manage the current one
                        localName = context.Id.GetText();
                        //
                        local = new XVariable(this._currentMethod, localName, Kind.Local, Modifiers.Public,
                                new TextRange(context), new TextInterval(context),
                                localType);
                        local.File = this._file;
                        //
                        this._currentMethod.Locals.Add(local);
                    }
                    else
                    {
                        // We may have something like
                        // LOCAL x,y as STRING
                        // for x, we don't have a DataType, so save it
                        _localDecls.Push(context);
                    }
                }
            }
        }

        public override void ExitEveryRule([NotNull] ParserRuleContext context)
        {
            if ( this.BuildRegionTags )
            {
                this.RegionExitEveryRule(context);
            }
            if (this.BuildModel)
            {
                if (context is XSharpParser.IEntityContext)
                {
                    if (context is XSharpParser.Class_Context)
                    {
                        //XSharpParser.Class_Context current = (XSharpParser.Class_Context)context;
                        if (_currentTypes.Count > 0)
                        {
                            this._currentTypes.Pop();
                        }
                        //else
                        //    if (System.Diagnostics.Debugger.IsAttached)
                        //        System.Diagnostics.Debugger.Break();
                    }
                    else if ((context is XSharpParser.ClsctorContext) ||
                                (context is XSharpParser.MethodContext) ||
                                (context is XSharpParser.PropertyContext) ||
                                (context is XSharpParser.FunctionContext) ||
                                (context is XSharpParser.ProcedureContext))
                    {
                        // Don't forget to add Self and Super as Local vars
                        if ((context is XSharpParser.ClsctorContext) ||
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
                        // Reset the current Method/Function/Procedure element
                        this._currentMethod = null;
                    }
                    //entities.Add((XSharpParser.IEntityContext)context);
                }
                else if (context is XSharpParser.Namespace_Context)
                {
                    if (this._currentNSpaces.Count > 0)
                    {
                        XType cNS = this._currentNSpaces.Peek();
                        // Is it necessary to Add the NameSpace in the TypeList ??
                        this._file.TypeList.AddUnique(cNS.Name, cNS);
                        // Pop to support nested NameSpaces
                        this._currentNSpaces.Pop();
                    }
                }
            }
        }

        private XTypeMember FromMethod(XSharpParser.MethodContext context)
        {
            //
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
            AddParameters(context.Params, newMethod);
            //
            return newMethod;
        }

        private XTypeMember FromProperty(XSharpParser.PropertyContext context)
        {
            //
            Kind kind = Kind.Property;
            //
            XTypeMember newMethod = new XTypeMember(context.Id.GetText(),
                kind,
                decodeModifiers(context.Modifiers?._Tokens),
                decodeVisibility(context.Modifiers?._Tokens),
                new TextRange(context), new TextInterval(context),
                (context.Type == null) ? "Void" : context.Type.GetText());
            //
            AddParameters(context.Params, newMethod);
            //
            return newMethod;
        }

        private void AddParameters(IList<XSharpParser.ParameterContext> ctxtParams, XTypeMember newMethod)
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

        private XTypeMember FromCtor(XSharpParser.ClsctorContext context)
        {
            //
            XTypeMember newMethod = new XTypeMember("Constructor",
                Kind.Constructor,
                decodeModifiers(context.Modifiers?._Tokens),
                decodeVisibility(context.Modifiers?._Tokens),
                new TextRange(context), new TextInterval(context));
            //
            AddParameters(context.Params, newMethod);
            //
            return newMethod;
        }

        private XType FromClass(XSharpParser.Class_Context context)
        {
            //
            XType newClass = new XType(context.Id?.GetText(),
                Kind.Class,
                decodeModifiers(context.Modifiers?._Tokens),
                decodeVisibility(context.Modifiers?._Tokens),
                new TextRange(context), new TextInterval(context));
            //
            newClass.NameSpace = this.CurrentNamespace;
            // and push into the current Namespace
            //CurrentNamespace.Types.Add(newClass);
            // Static Class ?
            newClass.IsStatic = this.IsStatic(context.Modifiers?._Tokens);
            // Partial Class ?
            newClass.IsPartial = this.IsPartial(context.Modifiers?._Tokens);
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
            //
            return newClass;
        }

        private XType FromStructure(XSharpParser.Structure_Context context)
        {
            //
            XType newStruct = new XType(context.Id.GetText(),
                Kind.Structure,
                decodeModifiers(context.Modifiers?._Tokens),
                decodeVisibility(context.Modifiers?._Tokens),
                new TextRange(context), new TextInterval(context));
            //
            newStruct.NameSpace = this.CurrentNamespace;
            // and push into the current Namespace
            //CurrentNamespace.Types.Add(newClass);
            // Static Class ?
            newStruct.IsStatic = this.IsStatic(context.Modifiers?._Tokens);
            // Partial Class ?
            newStruct.IsPartial = this.IsPartial(context.Modifiers?._Tokens);
            // IMPLEMENTS ?
            if ((context._Implements != null) && (context._Implements.Count > 0))
            {
                foreach (var interfaces in context._Implements)
                {
                    //newClass.BaseTypes.Add(new CodeTypeReference(interfaces.GetText()));
                }
            }
            //
            return newStruct;
        }
        private XType FromInterface(XSharpParser.Interface_Context context)
        {
            //
            XType newIf = new XType(context.Id.GetText(),
                Kind.Structure,
                decodeModifiers(context.Modifiers?._Tokens),
                decodeVisibility(context.Modifiers?._Tokens),
                new TextRange(context), new TextInterval(context));
            //
            newIf.NameSpace = this.CurrentNamespace;
            // and push into the current Namespace
            //CurrentNamespace.Types.Add(newClass);
            // Static Class ?
            newIf.IsStatic = this.IsStatic(context.Modifiers?._Tokens);
            // Partial Class ?
            newIf.IsPartial = this.IsPartial(context.Modifiers?._Tokens);
            // IMPLEMENTS ?
            //
            return newIf;
        }

        private XTypeMember FromFunction(XSharpParser.FunctionContext context)
        {
            //
            XTypeMember newMethod = new XTypeMember(context.Id.GetText(),
                Kind.Function,
                Modifiers.None,
                Modifiers.Public,
                new TextRange(context), new TextInterval(context),
                (context.Type == null) ? "Void" : context.Type.GetText());
            //
            AddParameters(context.Params, newMethod);
            //
            return newMethod;
        }

        private XTypeMember FromProcedure(XSharpParser.ProcedureContext context)
        {
            //
            XTypeMember newMethod = new XTypeMember(context.Id.GetText(),
                Kind.Procedure,
                Modifiers.None,
                Modifiers.Public,
                new TextRange(context), new TextInterval(context));
            //
            AddParameters(context.Params, newMethod);
            //
            return newMethod;
        }

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

        private bool IsStatic(IList<IToken> modifiers)
        {
            bool retValue = false;
            if (modifiers != null)
            {
                retValue = modifiers.Where(t => t.Type == XSharpParser.STATIC).Count() > 0;
            }
            //
            return retValue;
        }

        private bool IsPartial(IList<IToken> modifiers)
        {
            bool retValue = false;
            if (modifiers != null)
            {
                retValue = modifiers.Where(t => t.Type == XSharpParser.PARTIAL).Count() > 0;
            }
            //
            return retValue;
        }

    }
}
