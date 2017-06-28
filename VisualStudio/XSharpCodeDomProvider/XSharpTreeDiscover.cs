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
namespace XSharp.CodeDom
{

    [DebuggerDisplay("{ToString()}")]
    internal class XMemberType
    {
        internal XMemberType(string name, MemberTypes memberType, bool inherited, System.Type type)
        {
            Name = name;
            MemberType = memberType;
            Inherited = inherited;
            Type = type;
        }
        internal XMemberType(string name, MemberTypes memberType, bool inherited) :
            this(name, memberType, inherited, typeof(void))
        {
        }

        internal string Name { get; private set; }
        internal MemberTypes MemberType { get; private set; }
        internal System.Type Type { get; private set; }
        internal bool Inherited { get; private set; }
        public override string ToString()
        {
            if (Name == null || Type == null)
                return "";
            return Name + "," + Type.Name;
        }
    }
    /// <summary>
    /// Enhanced Type reference with System.Type property, since CodeTypeReference does not hold on to the type
    /// </summary>
    internal class XCodeTypeReference : CodeTypeReference
    {

        internal System.Type Type { get; set; }
        internal XCodeTypeReference(string typeName) : base(typeName)
        { }

        internal XCodeTypeReference(System.Type type) : base(type)
        {
            Type = type;
        }

    }

    class XSharpClassDiscover : XSharpBaseDiscover
    {
  
        private CodeMemberMethod initComponent;
        private IList<string> _locals;          // used to keep track of local vars


        public XSharpClassDiscover(IProjectTypeHelper projectNode) : base(projectNode)
        {
            // The default (empty) CodeCompileUnit, so we can work if none is provided
            this.CodeCompileUnit = new CodeCompileUnit();
            // The default Namespace, so we can work if none is provided... :)
            this.CurrentNamespace = new CodeNamespace();
            this.CodeCompileUnit.Namespaces.Add(this.CurrentNamespace);

            // If we have some Nested Namespaces, we will need to keep track
            this.NamespaceStack = new Stack<CodeNamespace>();
            // To store intermediate declarations
            this.LocalDecls = new Stack<XSharpParser.LocalvarContext>();
            this._locals = new List<string>();
            this._members = new Dictionary<string, XMemberType>(StringComparer.OrdinalIgnoreCase);
            //
            this.CurrentFile = "";
            _tokens = null;
        }

        public CodeCompileUnit CodeCompileUnit { get; internal set; }

        private CodeNamespace _currentNamespace;
        public CodeNamespace CurrentNamespace
        {
            get
            {
                if (_currentNamespace == null)
                {
                    _currentNamespace = new CodeNamespace();
                }
                return _currentNamespace;
            }

            internal set
            {
                _currentNamespace = value;
            }
        }
        public CodeTypeDeclaration CurrentClass { get; private set; }
        public Stack<CodeNamespace> NamespaceStack { get; private set; }
        public Stack<XSharpParser.LocalvarContext> LocalDecls { get; private set; }

        #region Members Cache
        private Dictionary<string, XMemberType> _members;  // member cache for our members and parent class members
        private void addMember(XMemberType type)
        {
            if (!_members.ContainsKey(type.Name)) // overloads ?
            {
                this._members.Add(type.Name, type);
            }
        }
        private bool findMember(Type type, string name, MemberTypes mtype)
        {
            if (_members.ContainsKey(name))
            {
                return _members[name].MemberType == mtype;
            }
            var mi = type.GetMember(name, BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.IgnoreCase | BindingFlags.Instance);
            if (mi != null)
            {
                foreach (var m in mi)
                {
                    System.Type t;
                    switch (m.MemberType)
                    {
                        case MemberTypes.Field:
                            t = ((FieldInfo)m).FieldType;
                            addMember(new XMemberType(name, m.MemberType, true, t));
                            break;
                        case MemberTypes.Property:
                            t = ((PropertyInfo)m).PropertyType;
                            addMember(new XMemberType(name, m.MemberType, true, t));
                            break;
                        case MemberTypes.Method:
                            t = ((MethodInfo)m).ReturnType;
                            addMember(new XMemberType(name, m.MemberType, true, t));
                            break;
                        case MemberTypes.Event:
                            addMember(new XMemberType(name, m.MemberType, true, typeof(void)));
                            break;
                        case MemberTypes.Constructor:
                            t = ((ConstructorInfo)m).DeclaringType;
                            addMember(new XMemberType(name, m.MemberType, true, t));
                            break;
                    }
                    if (m.MemberType == mtype)
                        return true;
                }
            }
            return false;
        }
        private bool findMemberInBaseTypes(string name, MemberTypes mtype)
        {
            System.Type baseType;
            foreach (CodeTypeReference basetype in CurrentClass.BaseTypes)
            {
                string typeName = basetype.BaseType;
                baseType = findType(typeName);
                if (baseType != null)
                {
                    if (findMember(baseType, name, mtype))
                    {
                        return true;
                    }
                }
            }
            return false;
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

        private bool isLocal(string name)
        {
            return _locals.Contains(name.ToLower());
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
            CodeNamespace newNamespace = new CodeNamespace(newNamespaceName);
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
            CodeTypeDeclaration newClass = new CodeTypeDeclaration(context.Id.GetText());
            // Set as Current working Class
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
                // PARTIAL ?
                if (context.Modifiers.PARTIAL().Length > 0)
                    newClass.IsPartial = true;
                //
                if (context.Modifiers.SEALED().Length > 0)
                    newClass.Attributes |= MemberAttributes.Final;
                if (context.Modifiers.ABSTRACT().Length > 0)
                    newClass.Attributes |= MemberAttributes.Abstract;
                if (context.Modifiers.INTERNAL().Length > 0)
                    newClass.Attributes |= MemberAttributes.Private;

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
                    addMember(new XMemberType(f.Name, MemberTypes.Field, false));
                }
            }
        }

        public override void ExitClass_([NotNull] XSharpParser.Class_Context context)
        {
            _members.Clear();
        }
        public override void EnterMethod([NotNull] XSharpParser.MethodContext context)
        {
            _locals.Clear();
            CodeMemberMethod newMethod = new CodeMemberMethod();
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
                    FillCodeSource(newMethod, context.end.Stop, context);

                    // The designer will need to locate the code in the file, so we must add the location
                    if ( context.StmtBlk.ChildCount > 0 )
                        FillCodeDomDesignerData(newMethod, context.StmtBlk.Start.Line, context.StmtBlk.Start.Column);
                    else
                        FillCodeDomDesignerData(newMethod, context.Start.Line+1, context.Start.Column);
                }
            }
            //
            this.CurrentClass.Members.Add(newMethod);
            this.addMember(new XMemberType(newMethod.Name, MemberTypes.Method, false, returnType.Type));
           
        }

        public override void ExitMethod([NotNull] XSharpParser.MethodContext context)
        {
            // Reset 
            initComponent = null;
            _locals.Clear();
        }

        public override void EnterUsing_([NotNull] XSharpParser.Using_Context context)
        {
            var import = new CodeNamespaceImport(context.Name.GetText());
            CurrentNamespace.Imports.Add(import);
            _usings.Add(import.Namespace);
        }

        public override void EnterEvent_([NotNull] XSharpParser.Event_Context context)
        {
            CodeMemberEvent evt = new CodeMemberEvent();
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
            this.addMember(new XMemberType(evt.Name, MemberTypes.Event, false, typeof(void)));
        }

        public override void EnterConstructor([NotNull] XSharpParser.ConstructorContext context)
         {
            CodeConstructor ctor = new CodeConstructor();
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
            if (context.StmtBlk != null)
            {
                // Copy all source code to User_Data
                // --> See XSharpCodeGenerator.GenerateMethod for writing
                FillCodeSource(ctor, context.end.Stop, context);
            }
            //
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
                    CodeTypeReference localType = BuildDataType(context.DataType);
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
                    ret.Expression = new CodeSnippetExpression(context.Expr.GetText());
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
                        cere = new CodeEventReferenceExpression(((CodeFieldReferenceExpression)left).TargetObject, ((CodeFieldReferenceExpression)left).FieldName);
                    else if (left is CodePropertyReferenceExpression)
                        cere = new CodeEventReferenceExpression(((CodePropertyReferenceExpression)left).TargetObject, ((CodePropertyReferenceExpression)left).PropertyName);
                    else if (left is CodeEventReferenceExpression)
                        cere = (CodeEventReferenceExpression)left;
                    if (cere != null)
                        stmt = new CodeAttachEventStatement(cere, right);
                    break;
                case XSharpLexer.ASSIGN_SUB:
                    // -= Event Handler
                    if (left is CodeFieldReferenceExpression)
                        cere = new CodeEventReferenceExpression(((CodeFieldReferenceExpression)left).TargetObject, ((CodeFieldReferenceExpression)left).FieldName);
                    else if (left is CodePropertyReferenceExpression)
                        cere = new CodeEventReferenceExpression(((CodePropertyReferenceExpression)left).TargetObject, ((CodePropertyReferenceExpression)left).PropertyName);
                    else if (left is CodeEventReferenceExpression)
                        cere = (CodeEventReferenceExpression)left;
                    if (cere != null)
                        stmt = new CodeRemoveEventStatement(cere, right);
                    break;
                default:
                    var snip = new CodeSnippetExpression(exp.GetText());
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

        private CodeVariableDeclarationStatement BuildLocalVar(XSharpParser.LocalvarContext context, CodeTypeReference localType)
        {
            CodeVariableDeclarationStatement local = new CodeVariableDeclarationStatement();
            local.Name = context.Id.GetText();
            local.Type = localType;
            if (context.Expression != null)
            {
                local.InitExpression = BuildExpression(context.Expression, false);
            }
            var name = local.Name.ToLower();
            if (!_locals.Contains(name))
            {
                _locals.Add(name);
            }
            return local;
        }

        private CodeExpression _MemberExpression(System.Type type, string rhsName)
        {
            var members = type.GetMember(rhsName, MemberTypes.All, BindingFlags.FlattenHierarchy |
                BindingFlags.IgnoreCase | BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.Static);
            if (members.Length > 0)
            {
                bool isPrivate = false;
                CodeTypeReferenceExpression lhsExpr = new CodeTypeReferenceExpression(type);
                foreach (var m in members)
                {
                    switch (m.MemberType)
                    {
                        case MemberTypes.Field:
                            var fi = m as FieldInfo;
                            if (!fi.IsPrivate)
                                return new CodeFieldReferenceExpression(lhsExpr, fi.Name);
                            break;
                        case MemberTypes.Property:
                            var pi = m as PropertyInfo;
                            if (pi.CanRead)
                                isPrivate = pi.GetGetMethod().IsPrivate;
                            else
                                isPrivate = pi.GetSetMethod().IsPrivate;
                            if (!isPrivate)
                                return new CodePropertyReferenceExpression(lhsExpr, pi.Name);
                            break;
                        case MemberTypes.Method:
                        case MemberTypes.Constructor:
                            var mb = m as MethodBase;
                            isPrivate = mb.IsPrivate;
                            if (!isPrivate)
                                return new CodeMethodReferenceExpression(lhsExpr,mb.Name);
                            break;
                        case MemberTypes.Event:
                            var ei = m as EventInfo;
                            return new CodeEventReferenceExpression(lhsExpr, ei.Name);
                    }
                }
            }
            return null;
        }

        /// <summary>
        /// Resolve SELF:Name or SUPER:Name
        /// </summary>
        /// <param name="lhs"></param>
        /// <param name="name"></param>
        /// <returns></returns>
        private CodeExpression ResolveSelfExpression(CodeExpression lhs, string name  )
        {
            CodeExpression expr = null;
            if (this.isField(name))
            {
                expr = new CodeFieldReferenceExpression(lhs, name);
            }
            else if (this.isProperty(name))
            {
                expr = new CodePropertyReferenceExpression(lhs, name);
            }
            else if (this.isMethod(name))
            { 
                expr = new CodeMethodReferenceExpression(lhs, name);
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
        private CodeExpression BuildAccessMember(XSharpParser.AccessMemberContext member, bool right)
        {
            //what is the left hand side ?
            //    Self  -> check if Right is in the member of CurrentClass --> FieldReference
            // else --> query if member from parent type
            // else assume it is a property
            System.Type type = null;
            string name = member.GetText();
            CodeExpression expr = null;
            if (!name.Contains(":"))
            {

                type = findType(name);
                if (type != null)
                    return new CodeTypeReferenceExpression(type);
            }
            string lhsName = member.Expr.GetText();
            string rhsName = member.Name.GetText();
            // Expression could be something like  System.Drawing.Color.White
            // If that is the case then we can find the Color class and its White Property
            if (!lhsName.Contains(":") && lhsName.ToLower() != "self" && lhsName.ToLower() != "super")
            {
                type = findType(lhsName);
            }
            if (type != null)
            {
                expr = _MemberExpression(type, rhsName);
                if (expr != null)
                    return expr;
            }

            bool isLocal = false;
            CodeExpression left = BuildExpression(member.Expr, false);
            if (left is CodeThisReferenceExpression)
            {
                expr = ResolveSelfExpression(left, rhsName );
                if (expr != null)
                    return expr;
            }
            else if (left is CodeBaseReferenceExpression)
            {
                expr = ResolveSelfExpression(left, rhsName);
                if (expr != null)
                    return expr;
            }
            else if (left is CodeVariableReferenceExpression)
            {
                isLocal = this.isLocal(lhsName);
            }
            if (isLocal)
            {
                expr = new CodePropertyReferenceExpression(left, rhsName);
            }
            if (expr == null)
            {
                // Let's guess that on the Left member, we should have a Property if it is not a Field
                if (!right)
                {
                    expr = new CodePropertyReferenceExpression(left, rhsName);
                }
                else
                {
                    // We are processing the Right member of an Assignment...
                    // Most likely Enum Value, which is a typereference expression followed by a DOT and a field
                    if (member.Op.Type == XSharpLexer.DOT)
                    {
                        var typeexpr = new CodeTypeReferenceExpression(lhsName);
                        expr = new CodeFieldReferenceExpression(typeexpr, rhsName);
                    }
                    else
                    {
                        expr = new CodeSnippetExpression(member.GetText());
                    }
                }
            }

            return expr;
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

        private CodeExpression BuildExpression(XSharpParser.ExpressionContext expression, bool right )
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
                expr = BuildExpression(tc.Expr,true);
                expr = new CodeCastExpression(typeref, expr);
            }
            else
            // Unhandled expression types
            // ArrayAccessExpressionContext
            // CondAccessExpressionContext
            // PostFixExpressionContext
            // AwaitExpressionContext
            // PrefixExpressionContext
            // TypeCheckExpressionContext
            // BinaryExpressionContext
            // AssignmentExpressionContext: Handled separately because CodeDom wants a Assign Statement
            {
                expr = new CodeSnippetExpression(expression.GetText());
            }
            //
            return expr;
        }

        private CodeExpression BuildPrimaryExpression(XSharpParser.PrimaryExpressionContext expression)
        {
            CodeExpression expr = null;
            XSharpParser.PrimaryContext ctx = expression.Expr;
            //
            if (ctx is XSharpParser.SelfExpressionContext) // Self
            {
                expr = new CodeThisReferenceExpression();
            }
            else if (ctx is XSharpParser.SuperExpressionContext) // Super
            {
                expr = new CodeBaseReferenceExpression();
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
                        exprlist.Add(BuildExpression(Element.Expr,true));
                    }
                    expr = new CodeArrayCreateExpression(BuildDataType(arr.Type), exprlist.ToArray());
                }
                else
                {
                    expr = new CodeSnippetExpression(arr.GetText());
                }
            }
            else if (ctx is XSharpParser.DelegateCtorCallContext)
            {
                XSharpParser.DelegateCtorCallContext delg = (XSharpParser.DelegateCtorCallContext)ctx;
                //
                CodeTypeReference ctr = BuildDataType(delg.Type);
                CodeExpression ce = BuildExpression(delg.Obj,true);
                //
                expr = new CodeDelegateCreateExpression(BuildDataType(delg.Type), BuildExpression(delg.Obj,true), delg.Func.GetText());

            }
            else if (ctx is XSharpParser.CtorCallContext)
            {
                XSharpParser.CtorCallContext ctor = (XSharpParser.CtorCallContext)ctx;
                CodeTypeReference ctr = BuildDataType(ctor.Type);
                List<CodeExpression> exprlist = new List<CodeExpression>();
                if (ctor.ArgList != null)
                {
                    foreach (var arg in ctor.ArgList._Args)
                    {
                        // We should handle arg.Name if arg.ASSIGN_OP is not null...
                        exprlist.Add(BuildExpression(arg.Expr,true));
                    }
                }
                expr = new CodeObjectCreateExpression(ctr.BaseType, exprlist.ToArray());
            }
            else if (ctx is XSharpParser.TypeOfExpressionContext)
            {
                CodeTypeReference ctr = BuildDataType(((XSharpParser.TypeOfExpressionContext)ctx).Type);
                expr = new CodeTypeOfExpression(ctr);
            }
            else if (ctx is XSharpParser.NameExpressionContext)
            {
                string name = ((XSharpParser.NameExpressionContext)ctx).Name.Id.GetText();
                // Sometimes, we will need to do it that way....
                if (name.ToLower() == "self")
                {
                    expr = new CodeThisReferenceExpression();
                }
                else if (name.ToLower() == "super")
                {
                    expr = new CodeBaseReferenceExpression();
                }
                // Locals preferred over same named fields
                else if (isLocal(name))
                {
                    expr = new CodeVariableReferenceExpression(name);
                }
                else if (isField(name))
                {
                    expr = new CodeFieldReferenceExpression(new CodeThisReferenceExpression(), name);
                }
                else if (isProperty(name))
                {
                    expr = new CodePropertyReferenceExpression(new CodeThisReferenceExpression(), name);
                }
                else
                {
                    var tr = BuildTypeReference(((XSharpParser.NameExpressionContext)ctx).Name.Id.GetText());
                    expr = new CodeTypeReferenceExpression(tr);
                }
            }
            else if (ctx is XSharpParser.ParenExpressionContext)
            {
                var par = ctx as XSharpParser.ParenExpressionContext;
                expr = BuildExpression(par.Expr,true);
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
                    var tr = new CodeTypeReferenceExpression(left.GetText());
                    expr = new CodePropertyReferenceExpression(tr, right.GetText());
                }
                else 
                {
                    expr = new CodeSnippetExpression(ctx.GetText());
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
                expr = new CodeSnippetExpression(ctx.GetText());
            }
            return expr;
        }

  
 
      
    }

    internal class XSharpFieldsDiscover : XSharpBaseDiscover
    {

        internal Stack<ParserRuleContext> classes;
        internal ParserRuleContext currentClass;
        private MemberAttributes classVarModifiers;

        internal XSharpFieldsDiscover(IProjectTypeHelper projectNode) : base(projectNode)
        {
            classes = new Stack<ParserRuleContext>();
            currentClass =null;
        }

        public override void EnterClass_(XSharpParser.Class_Context context)
        {
            classes.Push(currentClass);
            currentClass = context;
            FieldList.Add(context, new List<CodeMemberField>());
        }
        public override void ExitClass_(XSharpParser.Class_Context context)
        {
            currentClass = classes.Pop();
        }
        public override void EnterClassvarModifiers([NotNull] XSharpParser.ClassvarModifiersContext context)
        {

            this.classVarModifiers = MemberAttributes.Public;
            //
            ITerminalNode[] visibility;
            //
            visibility = context.INTERNAL();
            if (visibility.Length > 0)
                this.classVarModifiers = MemberAttributes.Assembly;
            //
            visibility = context.HIDDEN();
            if (visibility.Length > 0)
                this.classVarModifiers = MemberAttributes.Private;
            //
            visibility = context.PRIVATE();
            if (visibility.Length > 0)
                this.classVarModifiers = MemberAttributes.Private;
            //
            visibility = context.PROTECTED();
            if (visibility.Length > 0)
            {
                visibility = context.INTERNAL();
                if (visibility.Length > 0)
                    this.classVarModifiers = MemberAttributes.FamilyOrAssembly;
                else
                    this.classVarModifiers = MemberAttributes.Family;
            }
            //
            visibility = context.EXPORT();
            if (visibility.Length > 0)
                this.classVarModifiers = MemberAttributes.Public;
            //
            if (context.CONST().Length > 0)
                this.classVarModifiers |= MemberAttributes.Const;
            if (context.STATIC().Length > 0)
                this.classVarModifiers |= MemberAttributes.Static;
        }
        public override void EnterClassVarList([NotNull] XSharpParser.ClassVarListContext context)
        {
            //
            if (context.DataType != null)
            {
                var fieldType = BuildDataType(context.DataType);
                //
                foreach (var varContext in context._Var)
                {
                    CodeMemberField field = new CodeMemberField();
                    field.Name = varContext.Id.GetText();
                    field.Type = fieldType;
                    field.Attributes = this.classVarModifiers;
                    if (varContext.Initializer != null)
                    {
                        if (varContext.Initializer is XSharpParser.PrimaryExpressionContext)
                        {
                            XSharpParser.PrimaryContext ctx = ((XSharpParser.PrimaryExpressionContext)varContext.Initializer).Expr;
                            if (ctx is XSharpParser.LiteralExpressionContext)
                            {
                                XSharpParser.LiteralExpressionContext lit = (XSharpParser.LiteralExpressionContext)ctx;
                                field.InitExpression = BuildLiteralValue(lit.Literal);
                            }
                        }
                        else
                        {
                            field.InitExpression = new CodeSnippetExpression(varContext.Initializer.GetText());
                        }
                    }
                    FillCodeDomDesignerData(field, varContext.Start.Line, varContext.Start.Column);
                    //
                    FieldList[currentClass].Add(field);
                }
                //
            }
        }


    }

    internal class XSharpBaseDiscover : XSharpBaseListener
    {

        protected IProjectTypeHelper _projectNode;
        protected Dictionary<string, Type> _types;    // type cache
        protected IList<string> _usings;          // uses for type lookup
        protected IList<IToken> _tokens;          // used to find comments 

        internal Dictionary<ParserRuleContext, List<CodeMemberField>> FieldList { get;  set; }
        internal string SourceCode { get; set; }
        internal string CurrentFile { get; set; }


        internal XSharpBaseDiscover(IProjectTypeHelper projectNode) : base()
        {
            FieldList = new Dictionary<ParserRuleContext, List<CodeMemberField>>();
            _projectNode = projectNode;
            this._types = new Dictionary<string, Type>(StringComparer.OrdinalIgnoreCase);
            this._usings = new List<string>();
        }

        public override void EnterSource([NotNull] XSharpParser.SourceContext context)
        {
            var source = (XSharpLexer)context.Start.TokenSource;
            source.Reset();
            _tokens = source.GetAllTokens();
        }

        public override void EnterUsing_([NotNull] XSharpParser.Using_Context context)
        {
            var import = new CodeNamespaceImport(context.Name.GetText());
        }

        #region Helpers

        protected XCodeTypeReference BuildDataType(XSharpParser.DatatypeContext context)
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
            XCodeTypeReference expr = null;

            if (tn.NativeType != null)
            {
                expr = BuildNativeType(tn.NativeType);
            }
            else if (tn.XType != null)
            {
                expr = BuildXBaseType(tn.XType);
            }
            else if (tn.Name != null)
            {
                expr = BuildName(tn.Name);
            }
            //
            return expr;
        }
        protected CodeExpression BuildLiteralValue(XSharpParser.LiteralValueContext context)
        {
            CodeExpression expr = null;
            string value;
            //
            switch (context.Token.Type)
            {
                case XSharpParser.BIN_CONST:
                case XSharpParser.INT_CONST:
                case XSharpParser.HEX_CONST:
                case XSharpParser.REAL_CONST:
                    expr = new CodePrimitiveExpression(GetNumericValue(context));
                    break;
                case XSharpParser.TRUE_CONST:
                    expr = new CodePrimitiveExpression(true);
                    break;
                case XSharpParser.FALSE_CONST:
                    expr = new CodePrimitiveExpression(true);
                    break;
                case XSharpParser.STRING_CONST:
                case XSharpParser.ESCAPED_STRING_CONST:
                    value = context.GetText();
                    value = value.Substring(1, value.Length - 2);
                    if (context.Token.Type == XSharpParser.ESCAPED_STRING_CONST)
                        value = BuildUnEscapedString(value);
                    expr = new CodePrimitiveExpression(value);
                    break;
                case XSharpParser.CHAR_CONST:
                    value = context.GetText();
                    value = value.Substring(1, value.Length - 2);
                    if (value.Length >= 1)
                        expr = new CodePrimitiveExpression(value[0]);
                    break;
                case XSharpParser.NULL:
                    expr = new CodePrimitiveExpression(null);
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
                    expr = new CodeSnippetExpression(context.Token.Text);
                    break;
            }
            return expr;
        }
        protected XCodeTypeReference BuildName(XSharpParser.NameContext context)
        {
            XCodeTypeReference expr = null;
            //
            var sName = context.GetText();
            System.Type type = findType(sName);
            if (type != null)
                return new XCodeTypeReference(type);
            if (context is XSharpParser.QualifiedNameContext)
            {
                XSharpParser.QualifiedNameContext qual = (XSharpParser.QualifiedNameContext)context;
                expr = BuildName(qual.Left);
                expr = BuildTypeReference(expr.BaseType + "." + BuildSimpleName(qual.Right).BaseType);
            }
            else if (context is XSharpParser.SimpleOrAliasedNameContext)
            {
                var alias = context as XSharpParser.SimpleOrAliasedNameContext;
                var name = alias.Name as XSharpParser.AliasedNameContext;

                //
                if (name is XSharpParser.AliasQualifiedNameContext)
                {
                    XSharpParser.AliasQualifiedNameContext al = (XSharpParser.AliasQualifiedNameContext)name;
                    expr = BuildSimpleName(al.Right);
                    expr = BuildTypeReference(al.Alias.GetText() + "::" + expr.BaseType);
                }
                else if (name is XSharpParser.GlobalQualifiedNameContext)
                {
                    var gqn = name as XSharpParser.GlobalQualifiedNameContext;
                    expr = BuildSimpleName(gqn.Right);
                    expr = BuildTypeReference("global::" + expr.BaseType);
                }
                else if (name is XSharpParser.IdentifierOrGenericNameContext)
                {
                    var id = name as XSharpParser.IdentifierOrGenericNameContext;
                    expr = BuildSimpleName(id.Name);
                }
            }
            //
            return expr;
        }
        protected TypeAttributes ContextToClassModifiers(XSharpParser.ClassModifiersContext modifiers)
        {
            TypeAttributes retValue = TypeAttributes.Public;
            ITerminalNode[] visibility;
            //
            visibility = modifiers.INTERNAL();
            if (visibility.Length > 0)
                retValue = TypeAttributes.NestedAssembly;
            //
            visibility = modifiers.HIDDEN();
            if (visibility.Length > 0)
                retValue = TypeAttributes.NestedPrivate;
            //
            visibility = modifiers.PRIVATE();
            if (visibility.Length > 0)
                retValue = TypeAttributes.NestedPrivate;
            //
            visibility = modifiers.PROTECTED();
            if (visibility.Length > 0)
            {
                visibility = modifiers.INTERNAL();
                if (visibility.Length > 0)
                    retValue = TypeAttributes.NestedFamORAssem;
                else
                    retValue = TypeAttributes.NestedFamily;
            }
            //
            visibility = modifiers.EXPORT();
            if (visibility.Length > 0)
                retValue = TypeAttributes.Public;
            //
            return retValue;
        }

        protected MemberAttributes decodeMemberAttributes(IList<IToken> tokens)
        {
            MemberAttributes retValue = MemberAttributes.Public;
            bool bHasProtect = false;
            bool bHasInternal = false;
            foreach (var token in tokens)
            {
                switch (token.Type)
                {
                    case XSharpParser.INTERNAL:
                        bHasInternal = true;
                        if (bHasProtect)
                            retValue = MemberAttributes.FamilyOrAssembly;
                        else
                            retValue = MemberAttributes.Assembly;
                        break;
                    case XSharpParser.PROTECTED:
                        bHasProtect = true;
                        if (bHasInternal)
                            retValue = MemberAttributes.FamilyOrAssembly;
                        else
                            retValue = MemberAttributes.Family;
                        break;
                    case XSharpParser.PRIVATE:
                    case XSharpParser.HIDDEN:
                        retValue = MemberAttributes.Private;
                        break;
                    case XSharpParser.EXPORT:
                    case XSharpParser.PUBLIC:
                        //
                        retValue = MemberAttributes.Public;
                        break;
                }
            }
            return retValue;
        }
        protected MemberAttributes ContextToMethodModifiers(XSharpParser.MemberModifiersContext modifiers)
        {
            return decodeMemberAttributes(modifiers._Tokens);
        }
        protected MemberAttributes ContextToConstructorModifiers(XSharpParser.ConstructorModifiersContext modifiers)
        {
            return decodeMemberAttributes(modifiers._Tokens);
        }

        protected MemberAttributes ContextToEventModifiers(XSharpParser.EventModifiersContext modifiers)
        {
            return decodeMemberAttributes(modifiers._Tokens);
        }

        protected bool IsEmpty(CodeNamespace nspace)
        {
            return (nspace.Types.Count == 0);
        }

        protected CodeParameterDeclarationExpressionCollection GetParametersList(XSharpParser.ParameterListContext paramList)
        {
            CodeParameterDeclarationExpressionCollection pList = new CodeParameterDeclarationExpressionCollection();
            //
            if (paramList != null)
            {
                foreach (var param in paramList._Params)
                {
                    CodeParameterDeclarationExpression pm = new CodeParameterDeclarationExpression();
                    pm.Name = param.Id.GetText();
                    pm.Type = BuildDataType(param.Type);
                    pm.Direction = FieldDirection.In;
                    if (param.Modifiers != null)
                    {

                        if (param.Modifiers.REF() != null)
                        {
                            pm.Direction = FieldDirection.Ref;
                        }
                        else if (param.Modifiers.OUT() != null)
                        {
                            pm.Direction = FieldDirection.Out;
                        }
                    }
                    //
                    pList.Add(pm);
                }
            }
            //
            return pList;
        }


        protected void FillCodeDomDesignerData(CodeObject newElement, int line, int col)
        {
            CodeDomDesignerData data = new CodeDomDesignerData();
            //
            data.CaretPosition = new System.Drawing.Point(col, line);
            data.FileName = this.CurrentFile;
            // point is where the designer will try to focus if the
            // user wants to add event handler stuff.  
            newElement.UserData[typeof(CodeDomDesignerData)] = data;
            newElement.UserData[typeof(System.Drawing.Point)] = data.CaretPosition;
        }

        protected void FillCodeSource(CodeObject element, IToken endOfFirstLine, ParserRuleContext context)
        {
            int length = context.Stop.StopIndex - endOfFirstLine.StopIndex - 2;
            string extract = "";
            if (length > 0)
                extract = this.SourceCode.Substring(endOfFirstLine.StopIndex + 1, length).TrimStart();
            element.UserData[XSharpCodeConstants.USERDATA_CODE] = extract;
        }

        protected CodeSnippetTypeMember CreateSnippetMember(ParserRuleContext context)
        {
            // The original source code
            string sourceCode = context.GetText();
            //
            CodeSnippetTypeMember snippet = new CodeSnippetTypeMember(sourceCode);
            FillCodeDomDesignerData(snippet, context.Start.Line, context.Start.Column);
            return snippet;
        }

        /// <summary>
        /// Get a LiteralValueContext containing a BIN_CONST, INT_CONST, HEX_CONST, or a REAL_CONST
        /// as a string, and convert it to the "real" value, with the corresponding Type.
        /// </summary>
        /// <param name="context"></param>
        /// <returns>An Object of the needed Type, with the value</returns>
        protected object GetNumericValue(XSharpParser.LiteralValueContext context)
        {
            Object ret = null;
            string value = context.Token.Text;
            var type = context.Token.Type;
            //
            if (type == XSharpParser.BIN_CONST || type == XSharpParser.INT_CONST || type == XSharpParser.HEX_CONST)
            {
                bool isUnsigned = value.EndsWith("u", StringComparison.OrdinalIgnoreCase);
                bool isSigned = value.EndsWith("l", StringComparison.OrdinalIgnoreCase);
                if (isSigned || isUnsigned)
                {
                    value = value.Substring(0, value.Length - 1);
                }
                // -1 for Unsigned; -2 for 0x or 0b
                int len = value.Length - (context.BIN_CONST() != null || context.HEX_CONST() != null ? 2 : 0);
                //
                if (context.BIN_CONST() != null)
                {
                    if (len > 64)
                    {
                        ret = Double.NaN;
                    }
                    else
                    {
                        // Don't forget to remove the prefix !!!
                        value = value.Substring(2);
                        // BIN are always unsigned (??)
                        UInt64 bin64;
                        try
                        {
                            bin64 = Convert.ToUInt64(value, 2);
                            // Maybe 32 bits is enough ?
                            if (bin64 <= UInt32.MaxValue)
                            {
                                UInt32 bin32 = Convert.ToUInt32(bin64);
                                ret = bin32;
                            }
                            else
                            {
                                ret = bin64;
                            }
                        }
                        catch
                        {
                            ret = Double.NaN;
                        }
                    }
                }
                else if (type == XSharpParser.HEX_CONST)
                {
                    if (len > 16)
                    {
                        ret = Double.NaN;
                    }
                    else
                    {
                        // Don't forget to remove the prefix !!!
                        value = value.Substring(2);
                        // HEX are always unsigned (??)
                        UInt64 hex64;
                        try
                        {
                            hex64 = Convert.ToUInt64(value, 16);
                            // Maybe 32 bits is enough ?
                            if (hex64 <= UInt32.MaxValue)
                            {
                                UInt32 hex32 = Convert.ToUInt32(hex64);
                                ret = hex32;
                            }
                            else
                            {
                                ret = hex64;
                            }
                        }
                        catch
                        {
                            ret = Double.NaN;
                        }
                    }
                }
                else
                {
                    // context.INT_CONST() != null
                    if (len > 64)
                    {
                        ret = Double.NaN;
                    }
                    else if (isUnsigned)
                    {
                        UInt64 myUInt64;
                        try
                        {
                            myUInt64 = Convert.ToUInt64(value, 10);
                            // Maybe 32 bits is enough ?
                            if (myUInt64 <= UInt32.MaxValue)
                            {
                                UInt32 myUInt32 = Convert.ToUInt32(myUInt64);
                                ret = myUInt32;
                            }
                            else
                            {
                                ret = myUInt64;
                            }
                        }
                        catch
                        {
                            ret = Double.NaN;
                        }
                    }
                    else
                    {
                        Int64 myInt64;
                        try
                        {
                            myInt64 = Convert.ToInt64(value, 10);
                            // Maybe 32 bits is enough ?
                            if ((myInt64 >= UInt32.MinValue) && (myInt64 <= UInt32.MaxValue))
                            {
                                Int32 myInt32 = Convert.ToInt32(myInt64);
                                ret = myInt32;
                            }
                            else
                            {
                                ret = myInt64;
                            }
                        }
                        catch
                        {
                            ret = Double.NaN;
                        }
                    }
                }
            }
            else
            {
                double d;
                if (value.EndsWith("m", StringComparison.OrdinalIgnoreCase) ||
                    value.EndsWith("r", StringComparison.OrdinalIgnoreCase) ||
                    value.EndsWith("d", StringComparison.OrdinalIgnoreCase))
                {
                    value = value.Substring(0, value.Length - 1);
                }
                try
                {
                    d = double.Parse(value, System.Globalization.CultureInfo.InvariantCulture);
                }
                catch (Exception)
                {
                    d = Double.NaN;
                }
                ret = d;
            }
            return ret;
        }

        /// <summary>
        /// UnEscape the string, converting the Escape sequence into it's "real" form
        /// https://en.wikipedia.org/wiki/Escape_sequences_in_C
        /// </summary>
        /// <param name="text"></param>
        /// <returns></returns>
        public string BuildUnEscapedString(string text)
        {
            if (string.IsNullOrEmpty(text))
            {
                return text;
            }
            StringBuilder retval = new StringBuilder(text.Length);
            for (int ix = 0; ix < text.Length;)
            {
                // Search the next escape char '\'
                int jx = text.IndexOf('\\', ix);
                // If none, or too far (at least one char is needed AFTER the escape)
                if (jx < 0 || jx == text.Length - 1)
                    jx = text.Length;
                // Copy all text, up to that escape char
                retval.Append(text, ix, jx - ix);
                // End ?
                if (jx >= text.Length)
                    break;
                // Get the next one
                switch (text[jx + 1])
                {
                    case 'a':
                        // Alarm
                        retval.Append('\a');
                        break;
                    case 'b':
                        // BackSpace
                        retval.Append('\b');
                        break;
                    case 'f':
                        // Form feed
                        retval.Append('\f');
                        break;
                    case 'n':
                        // Line feed
                        retval.Append('\n');
                        break;
                    case 'r':
                        // Carriage return
                        retval.Append('\r');
                        break;
                    case 't':
                        // Tab
                        retval.Append('\t');
                        break;
                    case 'v':
                        // Vertical Tab
                        retval.Append('\v');
                        break;
                    case '\\':
                        // Don't escape
                        retval.Append('\\');
                        break;
                    case '\'':
                        // Simple Quote
                        retval.Append('\'');
                        break;
                    case '\"':
                        // Double Quote
                        retval.Append('\"');
                        break;
                    case '?':
                        // Question Mark
                        retval.Append('?');
                        break;
                    case '0':
                        // EOS
                        retval.Append('\0');
                        break;
                    case 'x':
                        // Hexadecimal code is following
                        // We need at least ONE char after the "\x"
                        if (jx + 2 >= text.Length)
                        {
                            break;
                        }
                        // Move after the 'x'
                        jx = jx + 2;
                        int digits = 0;
                        char[] hex = new char[4];
                        while (jx < text.Length && digits < 4)
                        {
                            char hexChar = text[jx];
                            if ((hexChar >= '0' && hexChar <= '9') || (hexChar >= 'a' && hexChar <= 'f') || (hexChar >= 'A' && hexChar <= 'F'))
                            {
                                hex[digits] = hexChar;
                                digits++;
                                jx++;
                            }
                            else
                            {
                                break;
                            }
                        }
                        // Something wrong in the value
                        if (digits == 0)
                        {
                            break;
                        }
                        else
                        {
                            // Put the Digits in a string
                            string hexValue = new string(hex, 0, digits);
                            // and convert....
                            UInt32 intValue = Convert.ToUInt32(hexValue, 16);
                            Char asciiChar = (char)intValue;
                            retval.Append(asciiChar);
                        }
                        // Adjust jx, because we have jx+2 for the loop
                        jx = jx - 2;
                        break;
                    default:
                        // Unrecognized, copy as-is
                        retval.Append('\\').Append(text[jx + 1]);
                        break;
                }
                // Jump over the escape char and the next one
                ix = jx + 2;
            }
            return retval.ToString();
        }

        protected XCodeTypeReference BuildTypeReference(string name)
        {
            System.Type type;
            type = findType(name);
            if (type != null)
            {
                return new XCodeTypeReference(type);
            }
            else
                return new XCodeTypeReference(name);

        }
        protected System.Type findType(string typeName)
        {
            if (_types.ContainsKey(typeName))
            {
                return _types[typeName];
            }
            var type = _projectNode.ResolveType(typeName, _usings.ToImmutableArray());
            if (type != null)
                _types.Add(typeName, type);
            return type;

        }
        protected XCodeTypeReference BuildNativeType(XSharpParser.NativeTypeContext nativeType)
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
                    return BuildTypeReference(strType);
            }
            return new XCodeTypeReference(type);
        }

        protected XCodeTypeReference BuildXBaseType(XSharpParser.XbaseTypeContext xbaseType)
        {
            return new XCodeTypeReference(xbaseType.Token.Text);
        }

        protected XCodeTypeReference BuildSimpleName(XSharpParser.SimpleNameContext simpleName)
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
                    var tmp = BuildDataType(generic);
                    argList += tmp.BaseType;
                    i++;
                }
                //
                gen = "`" + i.ToString() + "[" + argList + "]";
            }
            expr = BuildTypeReference(name + gen);
            //
            return expr;
        }


        #endregion
    }
    static class ParseHelpers
    {
        public static CodeCommentStatement[] GetLeadingComments(this ParserRuleContext context, IList<IToken> tokens)
        {
            int endPos = context.Start.TokenIndex - 1;
            int startPos = endPos;
            var stmts = new List<CodeCommentStatement>();
            while (startPos >= 0 && (tokens[startPos].Channel != 0 || tokens[startPos].Type == XSharpLexer.EOS))
            {
                var token = tokens[startPos];
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
                            var lines = line.Substring(2, line.Length - 4).Replace("\r","").Split('\n');
                            CodeCommentStatement[]  cmts = new CodeCommentStatement[lines.Length];
                            for (int i = 0; i < lines.Length; i++)
                            {
                                cmts[i] = new CodeCommentStatement(lines[i], false);
                            }
                            for (int i = lines.Length-1; i >= 0; i--)
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
    }
}

