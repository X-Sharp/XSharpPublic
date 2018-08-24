//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using LanguageService.SyntaxTree.Misc;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LanguageService.SyntaxTree.Tree;
using System.CodeDom;
using System.Reflection;
using Microsoft.VisualStudio.Shell.Design.Serialization.CodeDom;
using System.Diagnostics;
using System.Collections.Immutable;
using XSharpModel;
using System.Collections;

namespace XSharp.CodeDom
{

    [DebuggerDisplay("{ToString(),nq}")]
    internal class XMemberType
    {

        internal XMemberType(string name, MemberTypes memberType, bool inherited, System.Type type, string typeName)
        {
            Name = name;
            MemberType = memberType;
            Inherited = inherited;
            Type = type;
            TypeName = typeName;
        }

        internal XMemberType(string name, MemberTypes memberType, bool inherited) :
            this(name, memberType, inherited, typeof(void), "System.Void")
        {
        }

        internal string Name { get; private set; }
        internal MemberTypes MemberType { get; private set; }
        internal System.Type Type { get; private set; }
        internal String TypeName { get; private set; }
        internal bool Inherited { get; private set; }

        public override string ToString()
        {
            if (Name == null || TypeName == null)
                return "";
            return Name + "," + TypeName;
        }
    }

    internal class TypeXType
    {
        internal System.Type Type { get; private set; }
        internal XType xType { get; private set; }

        internal TypeXType(System.Type type) { Type = type; xType = null; }
        internal TypeXType(XType xtype) { xType = xtype; Type = null; }

    }

    class XSharpClassDiscover : XSharpBaseDiscover
    {

        private XCodeMemberMethod initComponent;
        private IDictionary<string, System.Type> _locals;          // used to keep track of local vars
        private int _startToken = 0;

        public XSharpClassDiscover(IProjectTypeHelper projectNode) : base(projectNode)
        {
            // The default (empty) CodeCompileUnit, so we can work if none is provided
            this.CodeCompileUnit = new CodeCompileUnit();
            // The default Namespace, so we can work if none is provided... :)
            this.CurrentNamespace = new XCodeNamespace("");
            this.CodeCompileUnit.Namespaces.Add(this.CurrentNamespace);

            // If we have some Nested Namespaces, we will need to keep track
            this.NamespaceStack = new Stack<XCodeNamespace>();
            // To store intermediate declarations
            this.LocalDecls = new Stack<XSharpParser.LocalvarContext>();
            this._locals = new Dictionary<string, System.Type>(StringComparer.OrdinalIgnoreCase);
            this._members = new Dictionary<string, XMemberType>(StringComparer.OrdinalIgnoreCase);
            //
            this.CurrentFile = "";
            _startToken = 0;
            _tokens = null;
        }

        public CodeCompileUnit CodeCompileUnit { get; internal set; }

        public XCodeTypeDeclaration CurrentClass { get; private set; }
        public Stack<XCodeNamespace> NamespaceStack { get; private set; }
        public Stack<XSharpParser.LocalvarContext> LocalDecls { get; private set; }

        private void AddCodeBefore(IDictionary userData, int startIndex)
        {
            if (startIndex > _startToken)
            {
                int length = startIndex - _startToken;
                string sourceCode = this.SourceCode.Substring(_startToken, length);
                userData[XSharpCodeConstants.USERDATA_CODEBEFORE] = sourceCode.TrimStart();
            }
        }

        #region Members Cache
        private Dictionary<string, XMemberType> _members;  // member cache for our members and parent class members
        private void addClassMember(XMemberType mtype)
        {
            if (!_members.ContainsKey(mtype.Name)) // overloads ?
            {
                this._members.Add(mtype.Name, mtype);
            }
        }
        private bool hasClassMember(TypeXType baseType, string name, MemberTypes mtype)
        {
            if (baseType != null)
            {
                if (baseType.Type != null)
                    return hasClassMember(baseType.Type, name, mtype);
                else if (baseType.xType != null)
                    return hasClassMember(baseType.xType, name, mtype);
            }
            return false;
        }
        private bool hasClassMember(System.Type baseType, string name, MemberTypes mtype)
        {
            if (_members.ContainsKey(name))
            {
                return (_members[name].MemberType | mtype) != 0;
            }
            var mi = baseType.GetMember(name, BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.IgnoreCase | BindingFlags.Instance);
            if (mi != null)
            {
                foreach (var m in mi)
                {
                    System.Type t;
                    switch (m.MemberType)
                    {
                        case MemberTypes.Field:
                            t = ((FieldInfo)m).FieldType;
                            addClassMember(new XMemberType(name, m.MemberType, true, t, t?.FullName));
                            break;
                        case MemberTypes.Property:
                            t = ((PropertyInfo)m).PropertyType;
                            addClassMember(new XMemberType(name, m.MemberType, true, t, t?.FullName));
                            break;
                        case MemberTypes.Method:
                            t = ((MethodInfo)m).ReturnType;
                            addClassMember(new XMemberType(name, m.MemberType, true, t, t?.FullName));
                            break;
                        case MemberTypes.Event:
                            addClassMember(new XMemberType(name, m.MemberType, true, typeof(void), "Void"));
                            break;
                        case MemberTypes.Constructor:
                            t = ((ConstructorInfo)m).DeclaringType;
                            addClassMember(new XMemberType(name, m.MemberType, true, t, t?.FullName));
                            break;
                    }
                    if ((m.MemberType | mtype) != 0)
                        return true;
                }
            }
            return false;
        }

        private bool hasClassMember(XType baseType, string name, MemberTypes mtype)
        {
            if (_members.ContainsKey(name))
            {
                // no need to include the type itself in the cache. This cache only
                // has fields and properties for the current window/control in the designer
                return (_members[name].MemberType | mtype) != 0;
            }
            bool result = false;
            XElement element = baseType.Members.Where(x => String.Equals(x.Name, name, StringComparison.OrdinalIgnoreCase)).FirstOrDefault();
            if (element != null)
            {
                System.Type t = typeof(void);
                var tm = element as XTypeMember;
                t = findType(tm?.TypeName);
                switch (element.Kind)
                {
                    case Kind.Field:
                        result = (mtype | MemberTypes.Field) != 0;
                        addClassMember(new XMemberType(name, MemberTypes.Field, true, t, t?.FullName));
                        break;
                    case Kind.Property:
                    case Kind.Access:
                    case Kind.Assign:
                        result = (mtype | MemberTypes.Property) != 0;
                        addClassMember(new XMemberType(name, MemberTypes.Property, true, t, t?.FullName));
                        break;
                    case Kind.Method:
                        result = (mtype | MemberTypes.Method) != 0;
                        addClassMember(new XMemberType(name, MemberTypes.Method, true, t, t?.FullName));
                        break;
                    case Kind.Event:
                        result = (mtype | MemberTypes.Event) != 0;
                        addClassMember(new XMemberType(name, MemberTypes.Event, true, t, t?.FullName));
                        break;
                    case Kind.Constructor:
                        result = (mtype | MemberTypes.Constructor) != 0;
                        addClassMember(new XMemberType(name, MemberTypes.Constructor, true, t, t?.FullName));
                        break;
                }
            }
            else
            {
                // not in our class, maybe in a parent class
                var parentType = findTypeXType(baseType.ParentName);
                return hasClassMember(parentType, name, mtype);
            }
            return result;
        }

        //private bool hasClassMember(EnvDTE.CodeElement type, string name, MemberTypes mtype)
        //{
        //    if (_members.ContainsKey(name))
        //    {
        //        return _members[name].MemberType == mtype;
        //    }
        //    return searchExternalClassMember(type, name, mtype);
        //}

        //private bool searchExternalClassMember(EnvDTE.CodeElement type, string name, MemberTypes mtype)
        //{
        //    bool result = false;
        //    //
        //    if ((type.Kind == EnvDTE.vsCMElement.vsCMElementClass) ||
        //        (type.Kind == EnvDTE.vsCMElement.vsCMElementEnum) ||
        //        (type.Kind == EnvDTE.vsCMElement.vsCMElementStruct))
        //    {

        //        //
        //        EnvDTE.CodeElements members = null;
        //        EnvDTE.CodeElements bases = null; ;
        //        if (type.Kind == EnvDTE.vsCMElement.vsCMElementClass)
        //        {
        //            EnvDTE.CodeClass envClass = (EnvDTE.CodeClass)type;
        //            members = envClass.Members;
        //            bases = envClass.Bases;
        //        }
        //        else if (type.Kind == EnvDTE.vsCMElement.vsCMElementEnum)
        //        {
        //            EnvDTE.CodeEnum envEnum = (EnvDTE.CodeEnum)type;
        //            members = envEnum.Members;
        //            bases = envEnum.Bases;
        //        }
        //        else if (type.Kind == EnvDTE.vsCMElement.vsCMElementStruct)
        //        {
        //            EnvDTE.CodeStruct envStruct = (EnvDTE.CodeStruct)type;
        //            members = envStruct.Members;
        //            bases = envStruct.Bases;
        //        }
        //        //
        //        System.Type t = typeof(void);
        //        foreach (EnvDTE.CodeElement member in members)
        //        {
        //            if (String.Equals(member.Name, name, StringComparison.OrdinalIgnoreCase))
        //            {
        //                switch (member.Kind)
        //                {
        //                    case EnvDTE.vsCMElement.vsCMElementEvent:
        //                        result = true;
        //                        addClassMember(new XMemberType(name, MemberTypes.Event, true, t, t?.FullName));
        //                        break;
        //                    case EnvDTE.vsCMElement.vsCMElementVariable:
        //                        result = true;
        //                        addClassMember(new XMemberType(name, MemberTypes.Field, true, t, t?.FullName));
        //                        break;
        //                    case EnvDTE.vsCMElement.vsCMElementFunction:
        //                        result = true;
        //                        addClassMember(new XMemberType(name, MemberTypes.Method, true, t, t?.FullName));
        //                        break;
        //                    case EnvDTE.vsCMElement.vsCMElementProperty:
        //                        result = true;
        //                        addClassMember(new XMemberType(name, MemberTypes.Property, true, t, t?.FullName));
        //                        break;
        //                }
        //                if (result)
        //                    break;
        //            }
        //        }
        //        if (!result)
        //        {
        //            if (bases != null)
        //            {
        //                foreach (EnvDTE.CodeElement parent in bases)
        //                {
        //                    if (parent.Kind == EnvDTE.vsCMElement.vsCMElementClass)
        //                    {
        //                        //
        //                        result = searchExternalClassMember(parent, name, mtype);
        //                        if (result)
        //                            break;
        //                    }
        //                }
        //            }
        //        }
        //    }
        //    return result;
        //}

        private bool findMemberInBaseTypes(string name, MemberTypes mtype)
        {
            foreach (XCodeTypeReference basetype in CurrentClass.BaseTypes)
            {
                string typeName = basetype.BaseType;
                var baseType = findTypeXType(typeName);
                if (baseType != null)
                {
                    return hasClassMember(baseType, name, mtype);
                }
                // External C#/VB/... Project
                // EnvDTE.CodeElement baseSType;
                //    baseSType = findStrangerType(typeName);
                //    if (baseSType != null)
                //    {
                //        return hasClassMember(baseSType, name, mtype);
                //    }
            }
            return false;
        }


        private System.Type getClassMemberType(string name, MemberTypes memberType)
        {
            if (_members.ContainsKey(name))
            {
                var mem = _members[name];
                if (mem.MemberType == memberType)
                    return mem.Type;
            }
            return null;
        }
        private bool isField(string name)
        {
            if (_members.ContainsKey(name) && _members[name].MemberType == MemberTypes.Field)
            {
                return true;
            }

            return findMemberInBaseTypes(name, MemberTypes.Field);
        }
        private bool isProperty(string name)
        {
            if (_members.ContainsKey(name) && _members[name].MemberType == MemberTypes.Property)
            {
                return true;
            }

            return findMemberInBaseTypes(name, MemberTypes.Property);
        }
        private bool isMethod(string name)
        {
            if (_members.ContainsKey(name) && _members[name].MemberType == MemberTypes.Method)
            {
                return true;
            }
            return findMemberInBaseTypes(name, MemberTypes.Method);
        }
        private bool isEvent(string name)
        {
            if (_members.ContainsKey(name) && _members[name].MemberType == MemberTypes.Event)
            {
                return true;
            }
            return findMemberInBaseTypes(name, MemberTypes.Event);
        }

        private bool isLocal(string name)
        {
            return _locals.ContainsKey(name);
        }

        #endregion
        public override void EnterNamespace_(XSharpParser.Namespace_Context context)
        {
            string newNamespaceName = context.Name.GetText();
            // We already have something in Stack
            // so we are nesting Namespaces, get the previous name prefix
            if (this.NamespaceStack.Count > 0 && !string.IsNullOrEmpty(CurrentNamespace.Name))
            {
                newNamespaceName = this.CurrentNamespace.Name + "." + newNamespaceName;
            }
            XCodeNamespace newNamespace = new XCodeNamespace(newNamespaceName);
            if (NamespaceStack.Count == 0)
            {
                AddCodeBefore(newNamespace.UserData, context.Start.StartIndex);
                _startToken = context._Entities[0].Start.StartIndex;
            }
            //
            newNamespace.Comments.AddRange(context.GetLeadingComments(_tokens));
            this.NamespaceStack.Push(this.CurrentNamespace);
            //
            if (string.IsNullOrEmpty(this.CurrentNamespace.Name))
            {
                // We could just have the empty fake Namespace here, but
                // if we have some Usings inside we must copy them
                if ((this.CurrentNamespace.Types.Count == 0) && (this.CurrentNamespace.Imports.Count > 0))
                {
                    // No Types means no Classes
                    // Ok, copy
                    foreach (CodeNamespaceImport import in this.CurrentNamespace.Imports)
                        newNamespace.Imports.Add(import);
                }
            }
            //
            this.CurrentNamespace = newNamespace;
        }

        public override void ExitNamespace_([NotNull] XSharpParser.Namespace_Context context)
        {
            // So, we end a Namespace declaration, was it empty ?
            if (!IsEmpty(this.CurrentNamespace))
            {
                this.CodeCompileUnit.Namespaces.Add(this.CurrentNamespace);
            }
            // Ok, get the previous one
            this.CurrentNamespace = this.NamespaceStack.Pop();
        }

        public override void EnterClass_(XSharpParser.Class_Context context)
        {
            XCodeTypeDeclaration newClass = new XCodeTypeDeclaration(context.Id.GetText());
            // Set as Current working Class
            AddCodeBefore(newClass.UserData, context.Start.StartIndex);
            CurrentClass = newClass;
            // and push into the Namespace
            CurrentNamespace.Types.Add(newClass);
            // That's a Class
            newClass.IsClass = true;
            newClass.Comments.AddRange(context.GetLeadingComments(_tokens));
            //
            if (context.Modifiers == null)
            {
                newClass.TypeAttributes = System.Reflection.TypeAttributes.Public;
            }
            else
            {
                // Modifiers
                foreach (var t in context.Modifiers._Tokens)
                {
                    switch (t.Type)
                    {
                        case XSharpParser.PARTIAL:
                            newClass.IsPartial = true;
                            break;
                        case XSharpParser.SEALED:
                            newClass.Attributes |= MemberAttributes.Final;
                            break;
                        case XSharpParser.ABSTRACT:
                            newClass.Attributes |= MemberAttributes.Abstract;
                            break;
                        case XSharpParser.INTERNAL:
                            newClass.Attributes |= MemberAttributes.Assembly;
                            break;
                        case XSharpParser.PUBLIC:
                            newClass.Attributes |= MemberAttributes.Public;
                            break;
                    }
                }
                // What Visibility ?
                newClass.TypeAttributes = ContextToClassModifiers(context.Modifiers);
            }
            // INHERIT from ?
            if (context.BaseType != null)
            {
                string baseName = context.BaseType.GetText();
                newClass.BaseTypes.Add(BuildTypeReference(baseName));
            }
            // IMPLEMENTS ?
            if ((context._Implements != null) && (context._Implements.Count > 0))
            {
                foreach (var interfaces in context._Implements)
                {
                    var ifName = interfaces.GetText();
                    newClass.BaseTypes.Add(BuildTypeReference(ifName));
                }
            }
            //
            // Add the variables from this class to the Members collection and lookup table
            _members.Clear();
            if (FieldList.ContainsKey(context))
            {
                var fields = FieldList[context];
                foreach (var f in fields)
                {
                    newClass.Members.Add(f);
                    addClassMember(new XMemberType(f.Name, MemberTypes.Field, false, findType(f.Type.BaseType), f.Type.BaseType));
                }
            }
            var token = context.Stop as XSharpToken;
            var tokenIndex = token.OriginalTokenIndex;
            var line = token.Line;
            for (;;)
            {
                if (tokenIndex >= _tokens.Count - 1)
                    break;
                tokenIndex++;
                if (_tokens[tokenIndex].Line > line)
                    break;
            }
            _startToken = _tokens[tokenIndex].StartIndex;
        }

        public override void ExitClass_([NotNull] XSharpParser.Class_Context context)
        {
            _members.Clear();
        }
        public override void EnterMethod([NotNull] XSharpParser.MethodContext context)
        {
            _locals.Clear();
            var newMethod = new XCodeMemberMethod();
            newMethod.Comments.AddRange(context.GetLeadingComments(_tokens));
            newMethod.Name = context.Id.GetText();
            newMethod.Attributes = MemberAttributes.Public;
            newMethod.Parameters.AddRange(GetParametersList(context.ParamList));
            var returnType = BuildDataType(context.Type);
            newMethod.ReturnType = returnType;
            //
            if (context.Modifiers != null)
            {
                // Get standard Visibilities
                newMethod.Attributes = ContextToMethodModifiers(context.Modifiers);
                // Is it a NEW method ?
                if (context.Modifiers.NEW().Length > 0)
                    newMethod.Attributes |= MemberAttributes.New;
                if (context.Modifiers.STATIC().Length > 0)
                    newMethod.Attributes |= MemberAttributes.Static;
                if (context.Modifiers.VIRTUAL().Length > 0)
                {
                    // According to MSDN, The absence of the Final flag makes a member virtual in C#, same for us
                    newMethod.Attributes &= ~MemberAttributes.Final;
                }
                else
                {
                    // Other cases = FINAL
                    newMethod.Attributes |= MemberAttributes.Final;
                }
            }
            // !!! WARNING !!!
            // If the method is InitializeComponent, we will have to find all CodeObjects, as the designer is using them
            // Else, we can just copy the whole code in USERDATA, that will be fine
            if (newMethod.Name == "InitializeComponent")
            {
                initComponent = newMethod;
            }
            else
            {

                if (context.StmtBlk != null)
                {
                    // Copy all source code to User_Data
                    // --> See XSharpCodeGenerator.GenerateMethod for writing
                    FillCodeSource(newMethod, context, _tokens);

                    // The designer will need to locate the code in the file, so we must add the location
                    if (context.StmtBlk.ChildCount > 0)
                        FillCodeDomDesignerData(newMethod, context.StmtBlk.Start.Line, context.StmtBlk.Start.Column);
                    else
                        FillCodeDomDesignerData(newMethod, context.Start.Line + 1, context.Start.Column);
                }
            }
            //
            this.CurrentClass.Members.Add(newMethod);
            this.addClassMember(new XMemberType(newMethod.Name, MemberTypes.Method, false, returnType.Type, returnType.Type?.FullName));

        }

        public override void ExitMethod([NotNull] XSharpParser.MethodContext context)
        {
            // Reset
            initComponent = null;
            _locals.Clear();
        }



        public override void EnterEvent_([NotNull] XSharpParser.Event_Context context)
        {
            var evt = new XCodeMemberEvent();
            evt.Comments.AddRange(context.GetLeadingComments(_tokens));
            evt.Name = context.Id.GetText();
            evt.Attributes = MemberAttributes.Public;
            var typeName = context.Type.GetText();
            evt.Type = BuildTypeReference(typeName);
            //
            if (context.Modifiers != null)
            {
                // Get standard Visibilities
                evt.Attributes = ContextToEventModifiers(context.Modifiers);
                if (context.Modifiers.NEW().Length > 0)
                    evt.Attributes |= MemberAttributes.New;
                if (context.Modifiers.STATIC().Length > 0)
                    evt.Attributes |= MemberAttributes.Static;
                if (context.Modifiers.VIRTUAL().Length > 0)
                {
                    // According to MSDN, The absence of the Final flag makes a member virtual in C#, same for us
                    evt.Attributes &= ~MemberAttributes.Final;
                }
                else
                {
                    // Other cases = FINAL
                    evt.Attributes |= MemberAttributes.Final;
                }
            }
            //
            this.CurrentClass.Members.Add(evt);
            this.addClassMember(new XMemberType(evt.Name, MemberTypes.Event, false, typeof(void), "Void"));
        }

        public override void EnterConstructor([NotNull] XSharpParser.ConstructorContext context)
        {
            var ctor = new XCodeConstructor();
            ctor.Comments.AddRange(context.GetLeadingComments(_tokens));
            ctor.Attributes = MemberAttributes.Public;
            ctor.Parameters.AddRange(GetParametersList(context.ParamList));
            //
            if (context.Modifiers != null)
            {
                // Get standard Visibilities
                ctor.Attributes = ContextToConstructorModifiers(context.Modifiers);
                if (context.Modifiers.STATIC().Length > 0)
                    ctor.Attributes |= MemberAttributes.Static;
            }
            FillCodeSource(ctor, context, _tokens);
            this.CurrentClass.Members.Add(ctor);
        }

        public override void EnterDestructor([NotNull] XSharpParser.DestructorContext context)
        {
            // Ok, let's "cheat" : We will not analyze the element
            // we will just copy the whole source code in a Snippet Member
            CodeSnippetTypeMember snippet = CreateSnippetMember(context);
            snippet.Comments.AddRange(context.GetLeadingComments(_tokens));
            this.CurrentClass.Members.Add(snippet);
        }

        public override void EnterProperty([NotNull] XSharpParser.PropertyContext context)
        {
            // Ok, let's "cheat" : We will not analyze the element
            // we will just copy the whole source code in a Snippet Member
            CodeSnippetTypeMember snippet = CreateSnippetMember(context);
            snippet.Comments.AddRange(context.GetLeadingComments(_tokens));
            this.CurrentClass.Members.Add(snippet);
        }

        public override void EnterOperator_([NotNull] XSharpParser.Operator_Context context)
        {
            // Ok, let's "cheat" : We will not analyze the element
            // we will just copy the whole source code in a Snippet Member
            CodeSnippetTypeMember snippet = CreateSnippetMember(context);
            snippet.Comments.AddRange(context.GetLeadingComments(_tokens));
            this.CurrentClass.Members.Add(snippet);
        }

        public override void EnterLocalvar([NotNull] XSharpParser.LocalvarContext context)
        {
            if (initComponent != null)
            {
                if (context.DataType != null)
                {
                    CodeStatementCollection locals = new CodeStatementCollection();
                    XCodeTypeReference localType = BuildDataType(context.DataType);
                    CodeVariableDeclarationStatement local;
                    // Any previous Local ?
                    while (LocalDecls.Count > 0)
                    {
                        XSharpParser.LocalvarContext tmpContext = LocalDecls.Pop();
                        local = BuildLocalVar(tmpContext, localType);
                        locals.Add(local);
                    }
                    // Now, manage the current one
                    local = BuildLocalVar(context, localType);
                    locals.Add(local);
                    //
                    initComponent.Statements.AddRange(locals);
                }
                else
                {
                    // We may have something like
                    // LOCAL x,y as string
                    // for x, we don't have a DataType, so save it
                    LocalDecls.Push(context);
                }
            }
        }
        public override void EnterReturnStmt([NotNull] XSharpParser.ReturnStmtContext context)
        {
            if (initComponent != null)
            {
                CodeMethodReturnStatement ret = new CodeMethodReturnStatement();
                if (context.Expr != null)
                {
                    ret.Expression = BuildSnippetExpression(context.Expr.GetText());
                }
                initComponent.Statements.Add(ret);
            }
        }

        public CodeStatement CreateAssignStatement(XSharpParser.AssignmentExpressionContext exp)
        {
            // Can be normal assign but also event handler assign
            CodeStatement stmt = null;
            //
            //what is the left hand side ?
            //    Self  -> check if Right is in the member of CurrentClass --> FieldReference
            // else --> always Property
            //
            CodeExpression left = BuildExpression(exp.Left, false);
            CodeExpression right = BuildExpression(exp.Right, true);
            CodeEventReferenceExpression cere = null;
            switch (exp.Op.Type)
            {
                case XSharpLexer.ASSIGN_OP:
                    stmt = new CodeAssignStatement(left, right);
                    break;
                case XSharpLexer.ASSIGN_ADD:
                    // += Event Handler
                    // We will decode Left as CodeFieldReferenceExpression or CodePropertyReferenceExpression, but we need a CodeEventReferenceExpression
                    if (left is CodeFieldReferenceExpression)
                        cere = new XCodeEventReferenceExpression(((CodeFieldReferenceExpression)left).TargetObject, ((CodeFieldReferenceExpression)left).FieldName);
                    else if (left is CodePropertyReferenceExpression)
                        cere = new XCodeEventReferenceExpression(((CodePropertyReferenceExpression)left).TargetObject, ((CodePropertyReferenceExpression)left).PropertyName);
                    else if (left is CodeEventReferenceExpression)
                        cere = (CodeEventReferenceExpression)left;
                    if (cere != null)
                        stmt = new CodeAttachEventStatement(cere, right);
                    break;
                case XSharpLexer.ASSIGN_SUB:
                    // -= Event Handler
                    if (left is CodeFieldReferenceExpression)
                        cere = new XCodeEventReferenceExpression(((CodeFieldReferenceExpression)left).TargetObject, ((CodeFieldReferenceExpression)left).FieldName);
                    else if (left is CodePropertyReferenceExpression)
                        cere = new XCodeEventReferenceExpression(((CodePropertyReferenceExpression)left).TargetObject, ((CodePropertyReferenceExpression)left).PropertyName);
                    else if (left is CodeEventReferenceExpression)
                        cere = (CodeEventReferenceExpression)left;
                    if (cere != null)
                        stmt = new CodeRemoveEventStatement(cere, right);
                    break;
                default:
                    var snip = BuildSnippetExpression(exp.GetText());
                    stmt = new CodeExpressionStatement(snip);
                    break;
            }
            return stmt;
        }

        public override void EnterExpressionStmt([NotNull] XSharpParser.ExpressionStmtContext context)
        {
            if (initComponent != null)
            {
                CodeStatement stmt = new CodeStatement();
                //
                if (context._expression is XSharpParser.AssignmentExpressionContext)
                {
                    stmt = CreateAssignStatement(context._expression as XSharpParser.AssignmentExpressionContext);
                }
                else
                {
                    var expr = BuildExpression(context._expression, true);
                    stmt = new CodeExpressionStatement(expr);
                }
                //
                initComponent.Statements.Add(stmt);
            }
        }

        private CodeVariableDeclarationStatement BuildLocalVar(XSharpParser.LocalvarContext context, XCodeTypeReference localType)
        {
            CodeVariableDeclarationStatement local = new CodeVariableDeclarationStatement();
            local.Name = context.Id.GetText();
            local.Type = localType;
            if (context.Expression != null)
            {
                local.InitExpression = BuildExpression(context.Expression, false);
            }
            var name = local.Name.ToLower();
            if (!_locals.ContainsKey(name))
            {
                _locals.Add(name, findType(localType.BaseType));
            }
            return local;
        }

        /// <summary>
        /// Resolve SELF:Name or SUPER:Name
        /// </summary>
        /// <param name="lhs"></param>
        /// <param name="name"></param>
        /// <returns></returns>
        private CodeExpression ResolveSelfExpression(CodeExpression lhs, string name)
        {
            CodeExpression expr = null;
            if (this.isField(name))
            {
                expr = new XCodeFieldReferenceExpression(lhs, name);
            }
            else if (this.isProperty(name))
            {
                expr = new XCodePropertyReferenceExpression(lhs, name);
            }
            else if (this.isMethod(name))
            {
                expr = new XCodeMethodReferenceExpression(lhs, name);
            }
            if (expr != null && _members.ContainsKey(name))
            {
                var inherited = _members[name].Inherited;
                if (inherited)  // always valid for both SELF and SUPER
                {
                    return expr;
                }
                else if (lhs is CodeThisReferenceExpression)
                {
                    return expr;
                }
                else
                {
                    // LHS is Super and not an inherited member
                    expr = null;
                }
            }
            return expr;
        }
        private System.Type findType(CodeExpression expr)
        {
            System.Type type;
            if (expr is CodeFieldReferenceExpression)
            {
                var cfr = expr as CodeFieldReferenceExpression;
                if (isField(cfr.FieldName) && (cfr.TargetObject is CodeThisReferenceExpression || cfr.TargetObject is CodeBaseReferenceExpression))
                {
                    return getClassMemberType(cfr.FieldName, MemberTypes.Field);
                }
                type = findType(cfr.TargetObject);
                if (type != null)
                {
                    var f = type.GetField(cfr.FieldName);
                    if (f != null)
                        return f.FieldType;
                }
            }
            if (expr is CodePropertyReferenceExpression)
            {
                var cpr = expr as CodePropertyReferenceExpression;
                if (isProperty(cpr.PropertyName) && (cpr.TargetObject is CodeThisReferenceExpression || cpr.TargetObject is CodeBaseReferenceExpression))
                {
                    return getClassMemberType(cpr.PropertyName, MemberTypes.Property);
                }
                type = findType(cpr.TargetObject);
                if (type != null)
                {
                    var p = type.GetProperty(cpr.PropertyName);
                    if (p != null)
                        return p.PropertyType;
                }
            }
            if (expr is CodeMethodInvokeExpression)
            {
                var cmi = expr as CodeMethodInvokeExpression;
                if (isMethod(cmi.Method.MethodName) && (cmi.Method.TargetObject is CodeThisReferenceExpression || cmi.Method.TargetObject is CodeBaseReferenceExpression))
                {
                    return getClassMemberType(cmi.Method.MethodName, MemberTypes.Method);

                }
                type = findType(cmi.Method.TargetObject);
                if (type != null)
                {
                    var m = type.GetMethod(cmi.Method.MethodName);
                    if (m != null)
                        return m.ReturnType;
                }
            }
            if (expr is CodeVariableReferenceExpression)
            {
                var cvr = expr as CodeVariableReferenceExpression;
                if (_locals.ContainsKey(cvr.VariableName))
                    return _locals[cvr.VariableName];
            }
            return null;
        }

        private CodeExpression buildTypeMemberExpression(System.Type type, string name)
        {
            var l = new XCodeTypeReferenceExpression(type);
            if (name.StartsWith("@@"))
            {
                name = name.Substring(2);
            }

            if (type.GetFields().Where(f => String.Compare(f.Name, name, true) == 0).Count() > 0)
            {
                return new XCodeFieldReferenceExpression(l, name);
            }
            if (type.GetProperties().Where(p => String.Compare(p.Name, name, true) == 0).Count() > 0)
            {
                return new XCodePropertyReferenceExpression(l, name);
            }
            if (type.GetMethods().Where(m => String.Compare(m.Name, name, true) == 0).Count() > 0)
            {
                return new XCodeMethodReferenceExpression(l, name);
            }
            if (type.GetEvents().Where(e => String.Compare(e.Name, name, true) == 0).Count() > 0)
            {
                return new XCodeEventReferenceExpression(l, name);
            }
            return null;
        }

        private CodeExpression buildTypeMemberExpression(CodeExpression target, TypeXType txtype, string name, out TypeXType memberType)
        {
            // Special Name ? (Keyword)
            if (name.StartsWith("@@"))
            {
                name = name.Substring(2);
            }
            CodeExpression expr = null;
            memberType = null;
            while (txtype != null)
            {
                System.Type type = txtype.Type;
                XType xtype = txtype.xType;
                if (xtype != null)
                {
                    var mi = xtype.GetMember(name);
                    if (mi.Count == 0)
                    {
                        // Member not found !? MayBe in the parent, which can be a System.Type
                        txtype = findTypeXType(xtype.ParentName);
                        continue;
                    }
                    //
                    if (mi.Count > 0)
                    {
                        XTypeMember m = mi[0];
                        switch (m.Kind)
                        {
                            case Kind.Field:
                                expr = new XCodeFieldReferenceExpression(target, name);
                                memberType = findTypeXType(m.TypeName);
                                break;
                            case Kind.Access:
                            case Kind.Assign:
                            case Kind.Property:
                                expr = new XCodePropertyReferenceExpression(target, name);
                                memberType = findTypeXType(m.TypeName);
                                break;
                            case Kind.Method:
                                expr = new XCodeMethodReferenceExpression(target, name);
                                memberType = findTypeXType(m.TypeName);
                                break;
                            case Kind.Event:
                                expr = new XCodeEventReferenceExpression(target, name);
                                memberType = findTypeXType(m.TypeName);
                                break;
                            default:
                                break;
                        }
                    }
                }
                //
                if (type != null)
                {
                    var mi = type.GetMember(name);
                    if (mi.Length > 0)
                    {
                        MemberInfo m = mi[0];
                        switch (m.MemberType)
                        {
                            case MemberTypes.Field:
                                expr = new XCodeFieldReferenceExpression(target, name);
                                memberType = new TypeXType(((FieldInfo)m).FieldType);
                                break;
                            case MemberTypes.Property:
                                expr = new XCodePropertyReferenceExpression(target, name);
                                memberType = new TypeXType(((PropertyInfo)m).PropertyType);
                                break;
                            case MemberTypes.Method:
                                expr = new XCodeMethodReferenceExpression(target, name);
                                memberType = new TypeXType(((MethodInfo)m).ReturnType);
                                break;
                            case MemberTypes.Event:
                                expr = new XCodeEventReferenceExpression(target, name);
                                memberType = new TypeXType(((EventInfo)m).EventHandlerType);
                                break;
                            default:
                                break;
                        }
                    }
                }
                //
                break;
            }
            return expr;
        }

        private CodeExpression buildSelfExpression(CodeExpression target, string name, out TypeXType memberType)
        {
            CodeExpression expr = null;
            memberType = null;
            if (!_members.ContainsKey(name))
            {
                findMemberInBaseTypes(name, MemberTypes.All);
            }
            if (_members.ContainsKey(name))
            {
                var mi = _members[name];
                switch (mi.MemberType)
                {
                    case MemberTypes.Field:
                        expr = new XCodeFieldReferenceExpression(target, name);
                        break;
                    case MemberTypes.Property:
                        expr = new XCodePropertyReferenceExpression(target, name);
                        break;
                    case MemberTypes.Method:
                        expr = new XCodeMethodReferenceExpression(target, name);
                        break;
                    case MemberTypes.Event:
                        expr = new XCodeEventReferenceExpression(target, name);
                        break;
                }
                //
                if (mi.Type != null)
                {
                    memberType = new TypeXType(mi.Type);
                }
                else
                {
                    memberType = findTypeXType(mi.TypeName);
                }
            }
            return expr;
        }

        private CodeExpression BuildAccessMember(XSharpParser.AccessMemberContext amc, bool right)
        {
            var elements = new List<XSharpParser.AccessMemberContext>();
            System.Type type = null;
            // if the top level element has a Dot it may be a type of a field of a type.
            if (amc.Op.Type == XSharpParser.DOT)
            {
                // aa.bb.cc.dd
                type = findType(amc.Expr.GetText());
                if (type != null)
                {

                    var result = buildTypeMemberExpression(type, amc.Name.GetText());
                    if (result != null)
                        return result;
                }
                type = findType(amc.GetText());
                if (type != null)
                {
                    return new XCodeTypeReferenceExpression(type);
                }
            }

            // Build a list of the elements from Left to Right so we can easily find without recursion
            // are AccessMember
            // LHS of first element is usually:
            // - self
            // - super
            // - cast with params
            // - local variable
            // - what else ?
            var element = amc;
            while (true)
            {
                elements.Insert(0, element);
                if (element.Expr is XSharpParser.AccessMemberContext)
                {
                    element = (XSharpParser.AccessMemberContext)element.Expr;
                }
                else
                {
                    break;
                }
            }
            // Determine the type of the first expression

            CodeExpression lhs = null;
            bool isSelf = false;
            amc = elements[0];    // Access Member
            type = null;
            switch (amc.Expr.Start.Type)
            {
                case XSharpParser.SELF:
                    lhs = new XCodeThisReferenceExpression();
                    isSelf = true;
                    break;
                case XSharpParser.SUPER:
                    lhs = new XCodeBaseReferenceExpression();
                    isSelf = true;
                    break;
                case XSharpParser.LPAREN:
                    var pe = amc.Expr as XSharpParser.PrimaryExpressionContext;
                    var pec = pe.Expr as XSharpParser.ParenExpressionContext;
                    var exp = pec.Expr as XSharpParser.TypeCastContext;
                    type = findType(exp.Type.GetText());
                    lhs = BuildExpression(pec.Expr, false);
                    lhs = new CodeCastExpression(type, lhs);
                    break;
                case XSharpParser.ID:
                    var lname = amc.Expr.GetText();
                    if (this.isLocal(lname))
                    {
                        type = _locals[lname];
                        lhs = new XCodeVariableReferenceExpression(lname);
                    }
                    break;
                default:
                    // what else ?
                    break;
            }
            // expr should have a value here
            if (lhs != null)
            {
                TypeXType txtype = new TypeXType(type);
                for (int i = 0; i < elements.Count; i++)
                {
                    amc = elements[i];
                    var rhs = amc.Name.GetText();
                    TypeXType memberType = null;
                    if (i == 0)
                    {
                        if (isSelf)
                        {
                            lhs = buildSelfExpression(lhs, rhs, out memberType);

                        }
                        else
                        {
                            lhs = buildTypeMemberExpression(lhs, txtype, rhs, out memberType);
                        }
                    }
                    else
                    {
                        lhs = buildTypeMemberExpression(lhs, txtype, rhs, out memberType);
                    }
                    txtype = memberType;
                    if (txtype == null)
                        break;
                }
            }
            else
            {
                lhs = BuildSnippetExpression(amc.GetText());
            }
            return lhs;
        }

        private CodeMethodInvokeExpression BuildMethodCall(XSharpParser.MethodCallContext meth)
        {
            CodeMethodInvokeExpression expr = null;

            // target: for example SELF:Controls:Add
            CodeExpression target = BuildExpression(meth.Expr, false);

            List<CodeExpression> exprlist = new List<CodeExpression>();
            if (meth.ArgList != null)
            {
                foreach (var arg in meth.ArgList._Args)
                {
                    exprlist.Add(BuildExpression(arg.Expr, true));
                }
            }
            // When we cannot find a field then we always map something like
            // SELF:Controls:Add to a propertyReferenceExpression
            // Change it to a method call now
            if (target is CodeMethodReferenceExpression)
            {
                var cmr = target as CodeMethodReferenceExpression;
                var lhs = cmr.TargetObject;
                var methodName = cmr.MethodName;
                expr = new CodeMethodInvokeExpression(lhs, methodName, exprlist.ToArray());
            }
            else if (target is CodePropertyReferenceExpression)
            {
                // method call on a property
                var cpr = target as CodePropertyReferenceExpression;
                var lhs = cpr.TargetObject;
                var methodName = cpr.PropertyName;
                expr = new CodeMethodInvokeExpression(lhs, methodName, exprlist.ToArray());
            }
            // can't be a variable ref: this will have only one element and not two
            // can't be a field ref: cannot have a field with the same name as a method
            else
            {
                // simple method call
                var methodName = meth.Expr.GetText();
                var pos = methodName.LastIndexOf(":");
                if (pos > 0)
                {
                    methodName = methodName.Substring(pos + 1);
                }
                expr = new CodeMethodInvokeExpression(null, methodName, exprlist.ToArray());
            }
            return expr;
        }

        private CodeExpression BuildExpression(XSharpParser.ExpressionContext expression, bool right)
        {
            CodeExpression expr = null;
            //
            if (expression is XSharpParser.PrimaryExpressionContext) // xyz.SimpleName
            {
                expr = BuildPrimaryExpression((XSharpParser.PrimaryExpressionContext)expression);
            }
            else if (expression is XSharpParser.AccessMemberContext) // xyz.SimpleName
            {
                expr = BuildAccessMember(expression as XSharpParser.AccessMemberContext, right);
            }
            else if (expression is XSharpParser.MethodCallContext)
            {
                expr = BuildMethodCall(expression as XSharpParser.MethodCallContext);
            }
            else if (expression is XSharpParser.TypeCastContext)
            {
                var tc = expression as XSharpParser.TypeCastContext;
                var name = tc.Type.GetText();
                var typeref = BuildTypeReference(name);
                expr = BuildExpression(tc.Expr, true);
                expr = new CodeCastExpression(typeref, expr);
            }
            else if (expression is XSharpParser.BinaryExpressionContext)
            {
                expr = BuildBinaryExpression(expression as XSharpParser.BinaryExpressionContext);
            }
            else if (expression is XSharpParser.PrefixExpressionContext)
            {
                var pref = expression as XSharpParser.PrefixExpressionContext;
                if (pref.PLUS() != null)
                {
                    expr = BuildExpression(pref.Expr, true);
                }
                else if (pref.MINUS() != null)
                {
                    expr = BuildExpression(pref.Expr, true);
                }
                else
                {
                    expr = BuildSnippetExpression(expression.GetText());
                }
            }
            // Unhandled expression types
            // ArrayAccessExpressionContext
            // CondAccessExpressionContext
            // PostFixExpressionContext
            // AwaitExpressionContext
            // PrefixExpressionContext
            // TypeCheckExpressionContext
            // AssignmentExpressionContext: Handled separately because CodeDom wants a Assign Statement
            else
            {
                expr = BuildSnippetExpression(expression.GetText());
            }
            //
            return expr;
        }

        private CodeExpression BuildBinaryExpression(XSharpParser.BinaryExpressionContext be)
        {
            var l = BuildExpression(be.Left, false);
            var r = BuildExpression(be.Right, true);
            var op = CodeBinaryOperatorType.Add;
            switch (be.Op.Type)
            {
                case XSharpParser.EXP:
                    // cannot be expressed in codedom
                    return BuildSnippetExpression(be.GetText());
                // MULT|DIV|MOD
                case XSharpParser.MULT:
                    op = CodeBinaryOperatorType.Multiply;
                    break;
                case XSharpParser.DIV:
                    op = CodeBinaryOperatorType.Divide;
                    break;
                case XSharpParser.MOD:
                    op = CodeBinaryOperatorType.Modulus;
                    break;
                // PLUS|MINUS
                case XSharpParser.PLUS:
                    op = CodeBinaryOperatorType.Add;
                    break;
                case XSharpParser.MINUS:
                    op = CodeBinaryOperatorType.Subtract;
                    break;
                // LSHIFT
                // RSHIFT
                case XSharpParser.LSHIFT:
                case XSharpParser.RSHIFT:
                    // cannot be expressed in codedom
                    return BuildSnippetExpression(be.GetText());
                // LT | LTE | GT | GTE | EQ | EEQ | SUBSTR | NEQ | NEQ2
                case XSharpParser.LT:
                    op = CodeBinaryOperatorType.LessThan;
                    break;
                case XSharpParser.LTE:
                    op = CodeBinaryOperatorType.LessThanOrEqual;
                    break;
                case XSharpParser.GT:
                    op = CodeBinaryOperatorType.GreaterThan;
                    break;
                case XSharpParser.GTE:
                    op = CodeBinaryOperatorType.GreaterThanOrEqual;
                    break;
                case XSharpParser.EQ:
                case XSharpParser.EEQ:
                    op = CodeBinaryOperatorType.IdentityEquality;
                    break;
                case XSharpParser.NEQ:
                case XSharpParser.NEQ2:
                    op = CodeBinaryOperatorType.IdentityInequality;
                    break;
                case XSharpParser.SUBSTR:
                    // cannot be expressed in codedom
                    return BuildSnippetExpression(be.GetText());
                // AMP
                case XSharpParser.AMP:
                    op = CodeBinaryOperatorType.BitwiseAnd;
                    break;
                // TILDE
                case XSharpParser.TILDE:
                    // cannot be expressed in codedom
                    return BuildSnippetExpression(be.GetText());
                // PIPE
                case XSharpParser.PIPE:
                    op = CodeBinaryOperatorType.BitwiseOr;
                    break;
                // LOGIC_AND | AND
                case XSharpParser.LOGIC_AND:
                case XSharpParser.AND:
                    op = CodeBinaryOperatorType.BooleanAnd;
                    break;
                // LOGIC_OR | OR
                case XSharpParser.LOGIC_OR:
                case XSharpParser.OR:
                    op = CodeBinaryOperatorType.BooleanOr;
                    break;
                // DEFAULT
                case XSharpParser.DEFAULT:
                    // cannot be expressed in codedom
                    return BuildSnippetExpression(be.GetText());
            }
            return new CodeBinaryOperatorExpression(l, op, r);
        }
        private CodeExpression BuildPrimaryExpression(XSharpParser.PrimaryExpressionContext expression)
        {
            CodeExpression expr = null;
            XSharpParser.PrimaryContext ctx = expression.Expr;
            //
            if (ctx is XSharpParser.SelfExpressionContext) // Self
            {
                expr = new XCodeThisReferenceExpression();
            }
            else if (ctx is XSharpParser.SuperExpressionContext) // Super
            {
                expr = new XCodeBaseReferenceExpression();
            }
            else if (ctx is XSharpParser.LiteralExpressionContext)
            {
                XSharpParser.LiteralExpressionContext lit = (XSharpParser.LiteralExpressionContext)ctx;
                expr = BuildLiteralValue(lit.Literal);
            }
            else if (ctx is XSharpParser.LiteralArrayExpressionContext) // { expr [, expr] }
            {
                XSharpParser.LiteralArrayContext arr = ((XSharpParser.LiteralArrayExpressionContext)ctx).LiteralArray;
                // Typed Array ?
                if (arr.Type != null)
                {
                    List<CodeExpression> exprlist = new List<CodeExpression>();
                    foreach (var Element in arr._Elements)
                    {
                        exprlist.Add(BuildExpression(Element.Expr, true));
                    }
                    expr = new CodeArrayCreateExpression(BuildDataType(arr.Type), exprlist.ToArray());
                }
                else
                {
                    expr = BuildSnippetExpression(arr.GetText());
                }
            }
            else if (ctx is XSharpParser.DelegateCtorCallContext)
            {
                XSharpParser.DelegateCtorCallContext delg = (XSharpParser.DelegateCtorCallContext)ctx;
                //
                XCodeTypeReference ctr = BuildDataType(delg.Type);
                CodeExpression ce = BuildExpression(delg.Obj, true);
                //
                expr = new CodeDelegateCreateExpression(BuildDataType(delg.Type), BuildExpression(delg.Obj, true), delg.Func.GetText());

            }
            else if (ctx is XSharpParser.CtorCallContext)
            {
                XSharpParser.CtorCallContext ctor = (XSharpParser.CtorCallContext)ctx;
                XCodeTypeReference ctr = BuildDataType(ctor.Type);
                List<CodeExpression> exprlist = new List<CodeExpression>();
                if (ctor.ArgList != null)
                {
                    foreach (var arg in ctor.ArgList._Args)
                    {
                        // We should handle arg.Name if arg.ASSIGN_OP is not null...
                        exprlist.Add(BuildExpression(arg.Expr, true));
                    }
                }
                expr = new CodeObjectCreateExpression(ctr.BaseType, exprlist.ToArray());
            }
            else if (ctx is XSharpParser.TypeOfExpressionContext)
            {
                XCodeTypeReference ctr = BuildDataType(((XSharpParser.TypeOfExpressionContext)ctx).Type);
                expr = new CodeTypeOfExpression(ctr);
            }
            else if (ctx is XSharpParser.NameExpressionContext)
            {
                string name = ((XSharpParser.NameExpressionContext)ctx).Name.Id.GetText();
                // Sometimes, we will need to do it that way....
                if (name.ToLower() == "self")
                {
                    expr = new XCodeThisReferenceExpression();
                }
                else if (name.ToLower() == "super")
                {
                    expr = new XCodeBaseReferenceExpression();
                }
                // Locals preferred over same named fields
                else if (isLocal(name))
                {
                    expr = new XCodeVariableReferenceExpression(name);
                }
                else if (isField(name))
                {
                    expr = new XCodeFieldReferenceExpression(new XCodeThisReferenceExpression(), name);
                }
                else if (isProperty(name))
                {
                    expr = new XCodePropertyReferenceExpression(new XCodeThisReferenceExpression(), name);
                }
                else
                {
                    var tr = BuildTypeReference(((XSharpParser.NameExpressionContext)ctx).Name.Id.GetText());
                    expr = new XCodeTypeReferenceExpression(tr);
                }
            }
            else if (ctx is XSharpParser.ParenExpressionContext)
            {
                var par = ctx as XSharpParser.ParenExpressionContext;
                expr = BuildExpression(par.Expr, true);
            }
            else if (ctx is XSharpParser.TypeExpressionContext)
            {
                var typ = ctx as XSharpParser.TypeExpressionContext;
                var name = typ.Type;
                // NativeType, xBaseType, Name
                if (name.Name is XSharpParser.QualifiedNameContext)
                {
                    var qnc = name.Name as XSharpParser.QualifiedNameContext;
                    var left = qnc.Left;
                    var right = qnc.Right;
                    var tr = new XCodeTypeReferenceExpression(left.GetText());
                    expr = new XCodePropertyReferenceExpression(tr, right.GetText());
                }
                else
                {
                    expr = BuildSnippetExpression(ctx.GetText());
                }
            }
            else
            // Unhandled expression types:
            // ---------------------------
            // AnonTypeExpressionContext
            // CodeBlockExpressionContext
            // QueryExpressionContext
            // CheckedExpressionContext
            // SizeOfExpressionContext
            // DefaultExpressionContext
            // VoConversionExpressionContext
            // VoCastExpressionContext
            // VoCastPtrExpressionContext
            // VoTypeNameExpressionContext
            // IifExpressionContext
            // IntrinsicExpressionContext  _OR(..)
            // AliasedFieldContect
            // AliasedExprContext
            // MacroContext
            // ArgListExpressionContext
            {
                expr = BuildSnippetExpression(ctx.GetText());
            }
            return expr;
        }






    }
    static class ParseHelpers
    {
        public static CodeCommentStatement[] GetLeadingComments(this ParserRuleContext context, IList<IToken> tokens)
        {
            // comment ends with token before the start of the method/class etc.
            int endPos = ((XSharpToken)context.Start).OriginalTokenIndex- 1;
            int startPos = endPos;
            var stmts = new List<CodeCommentStatement>();
            while (startPos >= 0 && (tokens[startPos].Channel != 0 || tokens[startPos].Type == XSharpLexer.EOS))
            {
                var token = tokens[startPos] ;
                if (XSharpLexer.IsComment(token.Type))
                {
                    string line = token.Text.TrimStart();
                    switch (token.Type)
                    {
                        case XSharpLexer.DOC_COMMENT:
                            stmts.Insert(0, new CodeCommentStatement(line.Substring(3).Trim(), true));
                            break;
                        case XSharpLexer.SL_COMMENT:
                            if (line.StartsWith("*"))
                            {
                                stmts.Insert(0, new CodeCommentStatement(line.Substring(1).Trim(), false));
                            }
                            else
                            {
                                stmts.Insert(0, new CodeCommentStatement(line.Substring(2).Trim(), false));
                            }
                            break;
                        case XSharpLexer.ML_COMMENT:
                            // remove terminators and convert to list of single line comments
                            var lines = line.Substring(2, line.Length - 4).Replace("\r", "").Split('\n');
                            CodeCommentStatement[] cmts = new CodeCommentStatement[lines.Length];
                            for (int i = 0; i < lines.Length; i++)
                            {
                                cmts[i] = new CodeCommentStatement(lines[i], false);
                            }
                            for (int i = lines.Length - 1; i >= 0; i--)
                            {
                                stmts.Insert(0, cmts[i]);
                            }
                            break;
                        default:
                            // what else
                            break;
                    }
                }
                startPos--;
            }
            // clear auto generated comments
            bool wasgenerated = false;
            foreach (var stmt in stmts)
            {
                if (stmt.Comment.Text.Contains("<auto-generated>"))
                {
                    wasgenerated = true;
                    break;
                }
            }
            if (wasgenerated)
            {
                stmts.Clear();
            }
            return stmts.ToArray();
        }

        public static System.Boolean IsNumeric(System.Object Expression)
        {
            if (Expression == null || Expression is DateTime)
                return false;

            if (Expression is Int16 || Expression is Int32 || Expression is Int64 || Expression is Decimal || Expression is Single || Expression is Double || Expression is Boolean)
                return true;

            return false;
        }
    }
}
