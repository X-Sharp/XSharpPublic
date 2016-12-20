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

namespace XSharpModel
{
    class EntityParser : XSharpBaseListener
    {
        //public List<XSharpParser.IEntityContext> entities = new List<XSharpParser.IEntityContext>();
        private XFile file;
        private Stack<XType> _currentTypes;
        private Stack<XType> _currentNSpaces;

        //private XType _currentType; //TODO: Should be a Stack to support NestedType
        private XTypeMember _currentMethod;
        private Stack<XSharpParser.LocalvarContext> _localDecls;
        private Modifiers _currentVarVisibility;
        private string _defaultNS;

        //private XType _currentNSpace;

        public EntityParser(XFile file)
        {
            this.file = file;
            this._defaultNS = this.file.Project.ProjectNode.RootNameSpace;
            // To store intermediate declarations
            this._localDecls = new Stack<XSharpParser.LocalvarContext>();
            //
            this._currentTypes = new Stack<XType>();
            this._currentNSpaces = new Stack<XType>();
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
                else if ( !String.IsNullOrEmpty( this._defaultNS ))
                {
                    ret = this._defaultNS;
                }
                return ret;
            }
        }

        public override void EnterEveryRule([NotNull] ParserRuleContext context)
        {
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
                    newClass.File = this.file;
                    //
                    this.file.TypeList.Add(newClass);
                    // Set as Current working Class
                    this._currentTypes.Push(newClass);
                }
                else if (context is XSharpParser.ClsctorContext)
                {
                    XSharpParser.ClsctorContext current = (XSharpParser.ClsctorContext)context;
                    XTypeMember newMethod = this.FromCtor((XSharpParser.ClsctorContext)context);
                    newMethod.File = this.file;
                    newMethod.Parent = this._currentTypes.Peek();
                    this._currentTypes.Peek().Members.Add(newMethod);
                    this._currentMethod = newMethod;
                }
                else if (context is XSharpParser.MethodContext)
                {
                    XSharpParser.MethodContext current = (XSharpParser.MethodContext)context;
                    XTypeMember newMethod = this.FromMethod((XSharpParser.MethodContext)context);
                    newMethod.File = this.file;
                    newMethod.Parent = this._currentTypes.Peek();
                    this._currentTypes.Peek().Members.Add(newMethod);
                    this._currentMethod = newMethod;
                }
                else if (context is XSharpParser.PropertyContext)
                {
                    XSharpParser.PropertyContext current = (XSharpParser.PropertyContext)context;
                    XTypeMember newMethod = this.FromProperty((XSharpParser.PropertyContext)context);
                    newMethod.File = this.file;
                    newMethod.Parent = this._currentTypes.Peek();
                    this._currentTypes.Peek().Members.Add(newMethod);
                    this._currentMethod = newMethod;
                }
                else if (context is XSharpParser.FunctionContext)
                {
                    XSharpParser.FunctionContext current = (XSharpParser.FunctionContext)context;
                    XTypeMember newMethod = this.FromFunction((XSharpParser.FunctionContext)context);
                    newMethod.File = this.file;
                    newMethod.Parent = file.Project.GlobalType;
                    file.Project.GlobalType.Members.Add(newMethod);
                    this._currentMethod = newMethod;
                }
                else if (context is XSharpParser.ProcedureContext)
                {
                    XSharpParser.ProcedureContext current = (XSharpParser.ProcedureContext)context;
                    XTypeMember newMethod = this.FromProcedure((XSharpParser.ProcedureContext)context);
                    newMethod.File = this.file;
                    newMethod.Parent = file.Project.GlobalType;
                    file.Project.GlobalType.Members.Add(newMethod);
                    this._currentMethod = newMethod;
                }
                //entities.Add((XSharpParser.IEntityContext)context);
            }
            else if (context is XSharpParser.Using_Context)
            {
                XSharpParser.Using_Context use = (XSharpParser.Using_Context)context;
                this.file.Usings.Add(use.Name.GetText());
            }
            else if (context is XSharpParser.Namespace_Context)
            {
                XSharpParser.Namespace_Context current = (XSharpParser.Namespace_Context)context;
                //
                TextRange range = new TextRange(context.Start.Line, context.Start.Column,
                                            context.Stop.Line, context.Stop.Column);
                TextInterval inter = new TextInterval(context.Start.StartIndex, context.Stop.StopIndex);
                //
                XType nSpace = new XType(current.Name.GetText(), Kind.Namespace, Modifiers.None, Modifiers.Public, range, inter);
                this._currentNSpaces.Push(nSpace);
            }
            else if (context is XSharpParser.ClassvarModifiersContext)
            {
                XSharpParser.ClassvarModifiersContext current = (XSharpParser.ClassvarModifiersContext)context;
                this._currentVarVisibility = decodeVisibility(current._Tokens);
            }
            else if (context is XSharpParser.ClassVarListContext)
            {
                XSharpParser.ClassVarListContext current = (XSharpParser.ClassVarListContext)context;
                if (current.DataType != null)
                {
                    //
                    foreach (var varContext in current._Var)
                    {
                        TextRange range = new TextRange(varContext.Start.Line, varContext.Start.Column,
                                varContext.Stop.Line, varContext.Stop.Column);
                        TextInterval inter = new TextInterval(varContext.Start.StartIndex, varContext.Stop.StopIndex);
                        //
                        Kind kind = Kind.ClassVar;
                        //
                        XTypeMember newClassVar = new XTypeMember(varContext.Id.GetText(),
                            kind, Modifiers.None, this._currentVarVisibility,
                            range, inter, current.DataType.GetText());
                        //
                        newClassVar.Parent = this._currentTypes.Peek();
                        this._currentTypes.Peek().Members.Add(newClassVar);
                    }
                }
            }
        }

        public override void EnterLocalvar([NotNull] XSharpParser.LocalvarContext context)
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
                            new TextRange(tmpContext.Start.Line, tmpContext.Start.Column, tmpContext.Stop.Line, tmpContext.Stop.Column),
                            new TextInterval(tmpContext.Start.StartIndex, tmpContext.Stop.StopIndex),
                            localType);
                        local.File = this.file;
                        //
                        this._currentMethod.Locals.Add(local);
                    }
                    // Now, manage the current one
                    localName = context.Id.GetText();
                    //
                    local = new XVariable(this._currentMethod, localName, Kind.Local, Modifiers.Public,
                            new TextRange(context.Start.Line, context.Start.Column, context.Stop.Line, context.Stop.Column),
                            new TextInterval(context.Start.StartIndex, context.Stop.StopIndex),
                            localType);
                    local.File = this.file;
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

        public override void ExitEveryRule([NotNull] ParserRuleContext context)
        {
            if (context is XSharpParser.IEntityContext)
            {
                if (context is XSharpParser.Class_Context)
                {
                    //XSharpParser.Class_Context current = (XSharpParser.Class_Context)context;
                    this._currentTypes.Pop();
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
                            new TextRange(context.Start.Line, context.Start.Column, context.Stop.Line, context.Stop.Column),
                            new TextInterval(context.Start.StartIndex, context.Stop.StopIndex),
                            this._currentMethod.ParentName);
                        //
                        local.File = this.file;
                        this._currentMethod.Locals.Add(local);
                        //
                        if ( ! String.IsNullOrEmpty(_currentMethod.Parent.ParentName ))
                        {
                            local = new XVariable(this._currentMethod, "Super", Kind.Local, Modifiers.Public,
                                new TextRange(context.Start.Line, context.Start.Column, context.Stop.Line, context.Stop.Column),
                                new TextInterval(context.Start.StartIndex, context.Stop.StopIndex),
                                this._currentMethod.Parent.ParentName);
                            local.File = this.file;
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
                    // Is it necessary to Add the NameSpace in the TypeList ??
                    this.file.TypeList.Add(this._currentNSpaces.Peek());
                    // Pop to support nested NameSpaces
                    this._currentNSpaces.Pop();
                }
            }
        }

        private XTypeMember FromMethod(XSharpParser.MethodContext context)
        {
            TextRange range = new TextRange(context.Start.Line, context.Start.Column,
                                            context.Stop.Line, context.Stop.Column);
            TextInterval inter = new TextInterval(context.Start.StartIndex, context.Stop.StopIndex);
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
                (context.Modifiers == null) ? Modifiers.None : decodeModifiers(context.Modifiers._Tokens),
                (context.Modifiers == null) ? Modifiers.Public : decodeVisibility(context.Modifiers._Tokens),
                range, inter,
                (context.Type == null) ? "Void" : context.Type.GetText());
            //
            AddParameters(context.Params, newMethod);
            //
            return newMethod;
        }

        private XTypeMember FromProperty(XSharpParser.PropertyContext context)
        {
            TextRange range = new TextRange(context.Start.Line, context.Start.Column,
                                            context.Stop.Line, context.Stop.Column);
            TextInterval inter = new TextInterval(context.Start.StartIndex, context.Stop.StopIndex);
            //
            Kind kind = Kind.Property;
            //
            XTypeMember newMethod = new XTypeMember(context.Id.GetText(),
                kind,
                (context.Modifiers == null) ? Modifiers.None : decodeModifiers(context.Modifiers._Tokens),
                (context.Modifiers == null) ? Modifiers.Public : decodeVisibility(context.Modifiers._Tokens),
                range, inter,
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
                    XVariable var = new XVariable(newMethod, param.Id.GetText(), Kind.Parameter, Modifiers.Public,
                        new TextRange(param.Start.Line, param.Start.Column, param.Stop.Line, param.Stop.Column),
                        new TextInterval(param.Start.StartIndex, param.Stop.StopIndex),
                        param.Type.GetText());
                    var.File = this.file;
                    //
                    newMethod.Parameters.Add(var);
                }
            }
        }

        private XTypeMember FromCtor(XSharpParser.ClsctorContext context)
        {
            TextRange range = new TextRange(context.Start.Line, context.Start.Column,
                                            context.Stop.Line, context.Stop.Column);
            TextInterval inter = new TextInterval(context.Start.StartIndex, context.Stop.StopIndex);
            //
            XTypeMember newMethod = new XTypeMember("Constructor",
                Kind.Constructor,
                (context.Modifiers == null) ? Modifiers.None : decodeModifiers(context.Modifiers._Tokens),
                (context.Modifiers == null) ? Modifiers.Public : decodeVisibility(context.Modifiers._Tokens),
                range, inter);
            //
            AddParameters(context.Params, newMethod);
            //
            return newMethod;
        }

        private XType FromClass(XSharpParser.Class_Context context)
        {
            TextRange range = new TextRange(context.Start.Line, context.Start.Column,
                                            context.Stop.Line, context.Stop.Column);
            TextInterval inter = new TextInterval(context.Start.StartIndex, context.Stop.StopIndex);
            //
            XType newClass = new XType(context.Id.GetText(),
                Kind.Class,
                (context.Modifiers == null) ? Modifiers.None : decodeModifiers(context.Modifiers._Tokens),
                (context.Modifiers == null) ? Modifiers.Public : decodeVisibility(context.Modifiers._Tokens),
                range, inter);
            //
            newClass.NameSpace = this.CurrentNamespace;
            // and push into the current Namespace
            //CurrentNamespace.Types.Add(newClass);
            // Static Class ?
            newClass.IsStatic = this.IsStatic(context.Modifiers);
            // Partial Class ?
            newClass.IsPartial = this.IsPartial(context.Modifiers);
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

        private XTypeMember FromFunction(XSharpParser.FunctionContext context)
        {
            TextRange range = new TextRange(context.Start.Line, context.Start.Column,
                                            context.Stop.Line, context.Stop.Column);
            TextInterval inter = new TextInterval(context.Start.StartIndex, context.Stop.StopIndex);
            //
            XTypeMember newMethod = new XTypeMember(context.Id.GetText(),
                Kind.Function,
                Modifiers.None,
                Modifiers.Public,
                range, inter,
                (context.Type == null) ? "Void" : context.Type.GetText());
            //
            AddParameters(context.Params, newMethod);
            //
            return newMethod;
        }

        private XTypeMember FromProcedure(XSharpParser.ProcedureContext context)
        {
            TextRange range = new TextRange(context.Start.Line, context.Start.Column,
                                            context.Stop.Line, context.Stop.Column);
            TextInterval inter = new TextInterval(context.Start.StartIndex, context.Stop.StopIndex);
            //
            XTypeMember newMethod = new XTypeMember(context.Id.GetText(),
                Kind.Procedure,
                Modifiers.None,
                Modifiers.Public,
                range, inter);
            //
            AddParameters(context.Params, newMethod);
            //
            return newMethod;
        }

        private Modifiers decodeModifiers(IList<IToken> tokens)
        {
            Modifiers retValue = Modifiers.None;
            foreach (var token in tokens)
            {
                switch (token.Type)
                {
                    case XSharpParser.ABSTRACT:
                        retValue = Modifiers.Abstract;
                        break;
                    case XSharpParser.NEW:
                        retValue = Modifiers.New;
                        break;
                    case XSharpParser.SEALED:
                        retValue = Modifiers.Sealed;
                        break;
                    case XSharpParser.UNSAFE:
                        retValue = Modifiers.Unsafe;
                        break;
                }
            }
            return retValue;
        }

        private Modifiers decodeVisibility(IList<IToken> tokens)
        {
            Modifiers retValue = Modifiers.Public;
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
            return retValue;
        }

        private bool IsStatic(XSharpParser.ClassModifiersContext modifiers)
        {
            bool retValue = false;
            if (modifiers != null)
            {
                ITerminalNode[] visibility;
                //
                visibility = modifiers.STATIC();
                if (visibility.Length > 0)
                    retValue = true;
            }
            //
            return retValue;
        }

        private bool IsPartial(XSharpParser.ClassModifiersContext modifiers)
        {
            bool retValue = false;
            if (modifiers != null)
            {
                ITerminalNode[] visibility;
                //
                visibility = modifiers.PARTIAL();
                if (visibility.Length > 0)
                    retValue = true;
            }
            //
            return retValue;
        }

    }
}
