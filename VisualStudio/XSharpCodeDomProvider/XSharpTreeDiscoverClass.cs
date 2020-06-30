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
using System.CodeDom;
using System.Reflection;
using System.Diagnostics;

using XSharpModel;
namespace XSharp.CodeDom
{

    [DebuggerDisplay("{ToString(),nq}")]
    internal class XMemberType
    {

        internal XMemberType(string name, MemberTypes memberType, bool inherited, string typeName)
        {
            Name = name;
            MemberType = memberType;
            Inherited = inherited;
            Type = null;
            TypeName = typeName;
        }
        internal XMemberType(string name, MemberTypes memberType, bool inherited, IXType type, string typeName)
        {
            Name = name;
            MemberType = memberType;
            Inherited = inherited;
            Type = type;
            TypeName = typeName;
        }

        internal XMemberType(string name, MemberTypes memberType, bool inherited) :
            this(name, memberType, inherited, null, "System.Void")
        {
        }

        internal string Name { get; private set; }
        internal MemberTypes MemberType { get; private set; }
        internal IXType Type { get; private set; }
        internal string TypeName { get; private set; }
        internal bool Inherited { get; private set; }

        public override string ToString()
        {
            if (Name == null || TypeName == null)
                return "";
            return Name + "," + TypeName;
        }
    }

  
    class XSharpClassDiscover : XSharpBaseDiscover
    {
        private XCodeMemberMethod initComponent;

        public XSharpClassDiscover(IProjectTypeHelper projectNode) : base(projectNode)
        {
            // The default (empty) CodeCompileUnit, so we can work if none is provided
            this.CodeCompileUnit = new XCodeCompileUnit();
            // The default Namespace, so we can work if none is provided... :)
            this.CurrentNamespace = new XCodeNamespace("");
            this.CodeCompileUnit.Namespaces.Add(this.CurrentNamespace);

            // If we have some Nested Namespaces, we will need to keep track
            this.NamespaceStack = new Stack<XCodeNamespace>();
            // To store intermediate declarations
            this.LocalDecls = new Stack<XSharpParser.LocalvarContext>();
            //
            this.CurrentFile = "";
            _tokens = null;
        }

        public XCodeCompileUnit CodeCompileUnit { get; internal set; }

        public Stack<XCodeNamespace> NamespaceStack { get; private set; }
        public Stack<XSharpParser.LocalvarContext> LocalDecls { get; private set; }

        public override void EnterNamespace_(XSharpParser.Namespace_Context context)
        {
            string newNamespaceName = context.Name.GetCleanText();
            // We already have something in Stack
            // so we are nesting Namespaces, get the previous name prefix
            if (this.NamespaceStack.Count > 0 && !string.IsNullOrEmpty(CurrentNamespace.Name))
            {
                newNamespaceName = this.CurrentNamespace.Name + "." + newNamespaceName;
            }
            XCodeNamespace newNamespace = new XCodeNamespace(newNamespaceName);
            writeTrivia(newNamespace, context);
            this.NamespaceStack.Push(this.CurrentNamespace);
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

        public override void ExitSource([NotNull] XSharpParser.SourceContext context)
        {
            var lastEnt = context._Entities.LastOrDefault();
            if (lastEnt != null)
            {

                writeTrivia(CodeCompileUnit, lastEnt, true);

            }
        }

        public CodeAttributeDeclarationCollection GenerateAttributes(XSharpParser.AttributesContext context)
        {
            /*
                Parser definition for attributes
                attributes          : ( AttrBlk+=attributeBlock )+
                                    ;
                attributeBlock      : LBRKT Target=attributeTarget? Attributes+=attribute (COMMA Attributes+=attribute)* RBRKT
                                    ;
                attributeTarget     : Id=identifier COLON
                                    | Kw=keyword COLON
                                    ;
                attribute           : Name=name (LPAREN (Params+=attributeParam (COMMA Params+=attributeParam)* )? RPAREN )?
                                    ;
                attributeParam      : Name=identifierName Op=assignoperator Expr=expression
                                    | Expr=expression
               attributes      = CodeAttributeDeclarationCollection
               attributeBlock  = list of attributes: CodeAttributeDeclaration
               attributeTarget = not used at this level.
               attribute       = CodeAttributeDeclaration
               arg for attrib = CodeAttributeArgument
               list of args   = CodeAttributeArgumentCollection
            */
            var result = new CodeAttributeDeclarationCollection();
            foreach (var blk in context._AttrBlk)
            {
                foreach (var attr in blk._Attributes)
                {
                    var name = attr.Name.GetText();
                    var codeattr = new CodeAttributeDeclaration(name);
                    foreach (XSharpParser.AttributeParamContext par in attr._Params)
                    {
                        var arg = new CodeAttributeArgument();
                        if (par is XSharpParser.PropertyAttributeParamContext )
                        {
                            var papc = par as XSharpParser.PropertyAttributeParamContext;
                            if (papc.Name != null)
                            {
                                arg.Name = papc.Name.GetText();
                            }
                            arg.Value = BuildExpression(papc.Expr, false);
                        }
                        else if (par is XSharpParser.ExprAttributeParamContext)
                        {
                            var eapc = par as XSharpParser.ExprAttributeParamContext;
                            arg.Value = BuildExpression(eapc.Expr, false);
                        }
                        codeattr.Arguments.Add(arg);
                    }
                    result.Add(codeattr);
                }
            }
            return result;
        }

        public override void EnterClass_(XSharpParser.Class_Context context)
        {
            XCodeTypeDeclaration newClass = new XCodeTypeDeclaration(context.Id.GetCleanText());
            if (context.Attributes != null)
            {
                newClass.CustomAttributes = GenerateAttributes(context.Attributes);
            }
            CurrentClass = newClass;
            // and push into the Namespace
            CurrentNamespace.Types.Add(newClass);
            // That's a Class
            newClass.IsClass = true;
            writeTrivia(newClass, context);
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
                string baseName = context.BaseType.GetCleanText();
                newClass.BaseTypes.Add(BuildTypeReference(baseName));
            }
            // IMPLEMENTS ?
            if ((context._Implements != null) && (context._Implements.Count > 0))
            {
                foreach (var interfaces in context._Implements)
                {
                    var ifName = interfaces.GetCleanText();
                    newClass.BaseTypes.Add(BuildTypeReference(ifName));
                }
            }
            //
            // Add the variables from this class to the Members collection and lookup table
            ClearMembers();
            if (FieldList.ContainsKey(context))
            {
                var fields = FieldList[context];
                foreach (var f in fields)
                {
                    newClass.Members.Add(f);
                    var xtype = findType(f.Type.BaseType);
                    if (xtype != null)
                    {
                        addClassMember(new XMemberType(f.Name, MemberTypes.Field, false, xtype.FullName));
                    }
                    else
                    {
                        addClassMember(new XMemberType(f.Name, MemberTypes.Field, false, f.Type.BaseType));
                    }
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
        }

        public override void EnterStructure_(XSharpParser.Structure_Context context)
        {
            XCodeTypeDeclaration newClass = new XCodeTypeDeclaration(context.Id.GetCleanText());
            // Set as Current working Class
            CurrentClass = newClass;
            if (context.Attributes != null)
            {
                newClass.CustomAttributes = GenerateAttributes(context.Attributes);
            }
            // and push into the Namespace
            CurrentNamespace.Types.Add(newClass);
            // That's a Class
            newClass.IsStruct = true;
            writeTrivia(newClass, context);
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
                newClass.TypeAttributes = ContextToStructureModifiers(context.Modifiers);
            }

            // IMPLEMENTS ?
            if ((context._Implements != null) && (context._Implements.Count > 0))
            {
                foreach (var interfaces in context._Implements)
                {
                    var ifName = interfaces.GetCleanText();
                    newClass.BaseTypes.Add(BuildTypeReference(ifName));
                }
            }
            //
            // Add the variables from this class to the Members collection and lookup table
            ClearMembers();
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
        }

        public override void ExitStructure_([NotNull] XSharpParser.Structure_Context context)
        {
            var lastmember = context._Members.LastOrDefault();
            if (lastmember != null)
            {
                // collect trivia after last member
                writeTrivia(CurrentClass, lastmember, true);
            }
            ClearMembers();
        }

        public override void ExitClass_([NotNull] XSharpParser.Class_Context context)
        {
            var lastmember = context._Members.LastOrDefault();
            if (lastmember != null)
            {
                // collect trivia after last member
                writeTrivia(CurrentClass, lastmember, true);
            }
            ClearMembers();
        }
        public override void EnterMethod([NotNull] XSharpParser.MethodContext context)
        {
            _locals.Clear();
            var newMethod = new XCodeMemberMethod();
            writeTrivia(newMethod, context);
            newMethod.Name = context.Sig.Id.GetCleanText();
            newMethod.Attributes = MemberAttributes.Public;
            newMethod.Parameters.AddRange(GetParametersList(context.Sig.ParamList));
            FillCodeDomDesignerData(newMethod, context.Start.Line, context.Start.Column);
            var returnType = BuildDataType(context.Sig.Type);
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
                if (context.Attributes != null)
                {
                    newMethod.CustomAttributes = GenerateAttributes(context.Attributes);
                }
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
            this.addClassMember(new XMemberType(newMethod.Name, MemberTypes.Method, false, returnType.BaseType));

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
            writeTrivia(evt, context);
            FillCodeDomDesignerData(evt, context.Start.Line, context.Start.Column);
            evt.Name = context.Id.GetCleanText();
            evt.Attributes = MemberAttributes.Public;
            var typeName = context.Type.GetCleanText();
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
            this.addClassMember(new XMemberType(evt.Name, MemberTypes.Event, false, null, "Void"));
        }

        public override void EnterConstructor([NotNull] XSharpParser.ConstructorContext context)
        {
            var ctor = new XCodeConstructor();
            writeTrivia(ctor, context);
            FillCodeDomDesignerData(ctor, context.Start.Line, context.Start.Column);
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
            writeTrivia(snippet, context);
            this.CurrentClass.Members.Add(snippet);
        }

        public override void EnterProperty([NotNull] XSharpParser.PropertyContext context)
        {
            // Ok, let's "cheat" : We will not analyze the element
            // we will just copy the whole source code in a Snippet Member
            CodeSnippetTypeMember snippet = CreateSnippetMember(context);
            writeTrivia(snippet, context);
            this.CurrentClass.Members.Add(snippet);
        }

        public override void EnterOperator_([NotNull] XSharpParser.Operator_Context context)
        {
            // Ok, let's "cheat" : We will not analyze the element
            // we will just copy the whole source code in a Snippet Member
            CodeSnippetTypeMember snippet = CreateSnippetMember(context);
            writeTrivia(snippet, context);
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
            local.Name = context.Id.GetCleanText();
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
        private IXType findType(CodeExpression expr)
        {
            IXType type;
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
                    if (f.Length > 0)
                        return findType(f[0].OriginalTypeName);
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
                    if (p.Length > 0)
                        return findType(p[0].OriginalTypeName);
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
                    if (m.Length > 0)
                        return findType(m[0].OriginalTypeName);
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

    


 


    }
    static class ParseHelpers
    {
        public static string GetLeadingTrivia(this ParserRuleContext context, IList<IToken> tokens)
        {
            // comment ends with token before the start of the method/class etc.
            int pos = ((XSharpToken)context.Start).OriginalTokenIndex ;
            pos--;
            var sb = new StringBuilder();
            while (pos >= 0 )
            {
                var token = tokens[pos];
                if (token.Channel != 0 )
                {
                    sb.Insert(0, token.Text);
                    pos--;
                }
                else if (token.Type == XSharpParser.EOS)
                {
                    // when the EOS is the end of a normal line of code
                    // then it is already included with that line
                    if (pos > 0 && tokens[pos - 1].Channel != 0)
                    {
                        sb.Insert(0, token.Text);
                        pos--;
                    }
                    else
                    {
                        break;
                    }
                }
                else
                {
                    break;
                }
            }
            var result = sb.ToString();
            if (! string.IsNullOrEmpty(result))
            {
                result = removeGeneratedComment(result);
            }
            return result;
        }

        private static string removeGeneratedComment(string source)
        {
            if (source.IndexOf("<auto-generated>") == -1)
                return source;
            var lines = source.Split(new string[] { "\r\n" },StringSplitOptions.None);
            var sb = new StringBuilder();
            for (int i = 0; i < lines.Length; i++)
            {
                var line = lines[i];
                if (!line.StartsWith("//"))
                {
                    sb.AppendLine(line);
                }
            }
            return sb.ToString();
        }

        public static string GetEndingTrivia(this ParserRuleContext context, IList<IToken> tokens)
        {
            // comment ends with token before the start of the method/class etc.
            int pos = ((XSharpToken)context.Stop).OriginalTokenIndex +1;
            var sb = new StringBuilder();
            var lastIsSpecial = false;
            while (pos  < tokens.Count )
            {
                var token = tokens[pos];
                if (token.Channel != 0)
                {
                    sb.Append(token.Text);
                    pos++;
                    lastIsSpecial = true;
                }
                else if (token.Type == XSharpParser.EOS && lastIsSpecial)
                {
                    sb.Append(token.Text);
                    pos++;
                    lastIsSpecial = false;
                }
                else
                {
                    break;
                }
            }
            return sb.ToString();
        }


        public static System.Boolean IsNumeric(System.Object Expression)
        {
            if (Expression == null || Expression is DateTime)
                return false;

            if (Expression is Int16 || Expression is Int32 || Expression is Int64 || Expression is Decimal || Expression is Single || Expression is Double || Expression is Boolean)
                return true;

            return false;
        }

        public static string GetCleanText(this XSharpParserRuleContext context)
        {
            string name = context.GetText();
            if (name.IndexOf("@@") >= 0)
                name = name.Replace("@@", "");
            return name;
        }
    }
}

