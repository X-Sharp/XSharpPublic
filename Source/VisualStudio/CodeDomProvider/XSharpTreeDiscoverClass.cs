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
        internal XMemberType(string name, MemberTypes memberType, bool inherited, IXTypeSymbol type, string typeName)
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
        internal IXTypeSymbol Type { get; private set; }
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
        public Stack<CodeTypeDeclaration> CurrentTypeStack { get; protected set; }
        public List<CodeObject> globalmembers;
        public bool IsNested => CurrentTypeStack.Count > 0;

        public XSharpClassDiscover(IProjectTypeHelper projectNode, CodeTypeDeclaration typeInOtherFile) : base(projectNode, typeInOtherFile)
        {
            // The default (empty) CodeCompileUnit, so we can work if none is provided
            this.CodeCompileUnit = new XCodeCompileUnit();
            // The default Namespace, so we can work if none is provided... :)
            this.CurrentNamespace = new XCodeNamespace("");
            this.CodeCompileUnit.Namespaces.Add(this.CurrentNamespace);
            CurrentTypeStack = new Stack<CodeTypeDeclaration>();

            // If we have some Nested Namespaces, we will need to keep track
            this.NamespaceStack = new Stack<XCodeNamespace>();
            // To store intermediate declarations
            this.LocalDecls = new Stack<XSharpParser.LocalvarContext>();
            //
            this.CurrentFile = "";
            _tokens = null;
            globalmembers = new List<CodeObject>();
        }

        #region GLobalMembers

        public void ProcessGlobalMember(XSharpParserRuleContext context)
        {
            CodeSnippetTypeMember snippet = CreateSnippetMember(context);
            this.globalmembers.Add(snippet);
        }
        public override void EnterFuncproc([NotNull] XSharpParser.FuncprocContext context)
        {
            ProcessGlobalMember(context);
        }
        public override void EnterVoglobal([NotNull] XSharpParser.VoglobalContext context)
        {
            ProcessGlobalMember(context);
        }

        public override void EnterVodefine([NotNull] XSharpParser.VodefineContext context)
        {
            ProcessGlobalMember(context);
        }
        public override void EnterVounion([NotNull] XSharpParser.VounionContext context)
        {
            ProcessGlobalMember(context);
        }
        public override void EnterVostruct([NotNull] XSharpParser.VostructContext context)
        {
            ProcessGlobalMember(context);
        }
        public override void EnterVodll([NotNull] XSharpParser.VodllContext context)
        {
            ProcessGlobalMember(context);
        }
        #endregion


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

        private void _ExitSource(IList<XSharpParser.EntityContext> entities)
        {
            var lastEnt = entities.LastOrDefault();
            if (lastEnt != null)
            {
                writeTrivia(CodeCompileUnit, lastEnt, true);
            }
            if (globalmembers.Count > 0)
            {
                CodeCompileUnit.SetGlobals(globalmembers);
            }

        }
        public override void ExitSource([NotNull] XSharpParser.SourceContext context)
        {
            _ExitSource(context._Entities);
        }
        public override void ExitFoxsource([NotNull] XSharpParser.FoxsourceContext context)
        {
            _ExitSource(context._Entities);
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
                        if (par is XSharpParser.PropertyAttributeParamContext papc)
                        {
                            if (papc.Name != null)
                            {
                                arg.Name = papc.Name.GetText();
                            }
                            arg.Value = BuildExpression(papc.Expr, false);
                        }
                        else if (par is XSharpParser.ExprAttributeParamContext eapc)
                        {
                            arg.Value = BuildExpression(eapc.Expr, false);
                        }
                        codeattr.Arguments.Add(arg);
                    }
                    result.Add(codeattr);
                }
            }
            return result;
        }

        private void addFields(XCodeTypeDeclaration newClass, XSharpParserRuleContext context)
        {
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
        }

        public void addInterfaces( XCodeTypeDeclaration newClass, IList<XSharpParser.DatatypeContext> parents)
        {
            if ((parents != null) && (parents.Count > 0))
            {
                foreach (var typecontext in parents)
                {
                    var ifName = typecontext.GetCleanText();
                    var baseType = BuildTypeReference(ifName);
                    SaveSourceCode(baseType, typecontext);
                    newClass.BaseTypes.Add(baseType);
                }
            }
        }

        public void addAttributes(CodeTypeDeclaration newClass, XSharpParser.AttributesContext attributes)
        {
            if (attributes != null)
            {
                newClass.CustomAttributes = GenerateAttributes(attributes);
            }
        }

        private void pushCurrentType(CodeTypeDeclaration newClass)
        {
            if (CurrentType != null)
            {
                CurrentTypeStack.Push(CurrentType);
                CurrentType.Members.Add(newClass);
            }
            else
            {
                CurrentNamespace.Types.Add(newClass);
            }
            CurrentType = newClass;
        }
       

        public override void EnterClass_(XSharpParser.Class_Context context)
        {
            base.EnterClass_(context);
            XCodeTypeDeclaration newClass = new XCodeTypeDeclaration(context.Id.GetCleanText())
            {
                IsClass = true
            };
            bool nested = CurrentType != null;
            pushCurrentType(newClass);
            addAttributes(newClass, context.Attributes);
            writeTrivia(newClass, context);

            ContextToTypeAttributes(newClass, context.Modifiers);
            ContextToTypeModifiers(newClass, context.Modifiers);

            // INHERIT from ?
            if (context.BaseType != null)
            {
                string baseName = context.BaseType.GetCleanText();
                var baseType = BuildTypeReference(baseName);
                SaveSourceCode(baseType, context.BaseType);
                newClass.BaseTypes.Add(baseType);
            }
            // IMPLEMENTS ?
            addInterfaces(newClass, context._Implements);
            //
            // Add the variables from this class to the Members collection and lookup table
            ClearMembers();
            addFields(newClass, context);
            if (nested)
            {
                SaveSourceCode(newClass, context);
            }
        }

        public override void EnterEnum_([NotNull] XSharpParser.Enum_Context context)
        {
            base.EnterEnum_(context);
            XCodeTypeDeclaration newClass = new XCodeTypeDeclaration(context.Id.GetCleanText())
            {
                IsEnum = true
            };

            pushCurrentType(newClass);
            addAttributes(newClass, context.Attributes);
            ContextToTypeModifiers(newClass, context.Modifiers);
            ContextToTypeAttributes(newClass, context.Modifiers);
            ClearMembers();
            addFields(newClass, context);
            SaveSourceCode(newClass, context);
        }

        public override void ExitEnum_([NotNull] XSharpParser.Enum_Context context)
        {
            base.ExitEnum_(context);
            closeType(null);
        }

        public override void EnterDelegate_([NotNull] XSharpParser.Delegate_Context context)
        {
            base.EnterDelegate_(context);
            var newClass = new XCodeTypeDelegate(context.Id.GetCleanText())
            {
                ReturnType = BuildDataType(context.Type)
            };
            newClass.Parameters.AddRange(GetParametersList(context.ParamList));
            pushCurrentType(newClass);
            addAttributes(newClass, context.Attributes);
            writeTrivia(newClass, context);
            SaveSourceCode(newClass, context);

        }
        public override void ExitDelegate_([NotNull] XSharpParser.Delegate_Context context)
        {
            base.ExitDelegate_(context);
            closeType(null);
        }

        public override void EnterInterface_([NotNull] XSharpParser.Interface_Context context)
        {
            base.EnterInterface_(context);
            XCodeTypeDeclaration newClass = new XCodeTypeDeclaration(context.Id.GetCleanText())
            {
                IsInterface = true
            };

            pushCurrentType(newClass);
            addAttributes(newClass, context.Attributes);

            ContextToTypeModifiers(newClass, context.Modifiers);
            ContextToTypeAttributes(newClass, context.Modifiers);

            // Interfaces
            addInterfaces(newClass, context._Parents);
            //
            // Add the variables from this class to the Members collection and lookup table
            ClearMembers();
            SaveSourceCode(newClass, context);

        }
        public override void EnterStructure_(XSharpParser.Structure_Context context)
        {
            base.EnterStructure_(context);
            XCodeTypeDeclaration newClass = new XCodeTypeDeclaration(context.Id.GetCleanText())
            {
                IsStruct = true
            };

            pushCurrentType(newClass);
            addAttributes(newClass, context.Attributes);
            writeTrivia(newClass, context);
            //
            ContextToTypeAttributes(newClass, context.Modifiers);
            ContextToTypeModifiers(newClass, context.Modifiers);

            // Interfaces
            addInterfaces(newClass, context._Implements);
            //
            // Add the variables from this class to the Members collection and lookup table
            ClearMembers();
            addFields(newClass, context);
            SaveSourceCode(newClass, context);


        }

        private void closeType(IList<XSharpParser.ClassmemberContext> members)
        {
            if (members != null)
            {
                var lastmember = members.LastOrDefault();
                if (lastmember != null)
                {
                    // collect trivia after last member
                    writeTrivia(CurrentType, lastmember, true);
                }
            }
            ClearMembers();
            if (CurrentTypeStack.Count > 0)
            {
                CurrentType = CurrentTypeStack.Pop();
            }
            else
            {
                CurrentType = null;
            }
        }
        public override void ExitStructure_([NotNull] XSharpParser.Structure_Context context)
        {
            closeType(context._Members);
            base.ExitStructure_(context);
        }

        public override void ExitClass_([NotNull] XSharpParser.Class_Context context)
        {
            closeType(context._Members);
            base.ExitClass_(context);
        }

        public override void ExitInterface_([NotNull] XSharpParser.Interface_Context context)
        {
            closeType(context._Members);
            base.ExitInterface_(context);
        }


        public override void EnterMethod([NotNull] XSharpParser.MethodContext context)
        {
            // Nested types are stored in Source code completely, so no need to analyze
            if (IsNested)
                return;

            _locals.Clear();
            var newMethod = new XCodeMemberMethod
            {
                Name = context.Sig.Id.GetCleanText(),
                Attributes = MemberAttributes.Public
            };
            newMethod.Parameters.AddRange(GetParametersList(context.Sig.ParamList));
            var returnType = BuildDataType(context.Sig.Type);
            newMethod.ReturnType = returnType;
            //
            if (context.Modifiers != null)
            {
                // Get standard Visibilities
                newMethod.Attributes = decodeMemberAttributes(context.Modifiers._Tokens,context.Modifiers.VIRTUAL());
            }
            // !!! WARNING !!!
            // If the method is InitializeComponent, we will have to find all CodeObjects, as the designer is using them
            // Else, we can just copy the whole code in USERDATA, that will be fine
            if (newMethod.Name == "InitializeComponent")
            {

                writeTrivia(newMethod, context);
                initComponent = newMethod;
                if (context.Attributes != null)
                {
                    newMethod.CustomAttributes = GenerateAttributes(context.Attributes);
                }
                FillCodeDomDesignerData(newMethod, context.Start.Line, context.Start.Column);
            }
            else
            {
                // no trivia needed. We write the complete source code including trivia
                if (context.StmtBlk != null)
                {
                    // The designer will need to locate the code in the file, so we must add the location
                    // When there are no statements then we position on the line after the member declaration
                    int line = context.Start.Line + 1;
                    int column = context.Start.Column;
                    if (context.StmtBlk.ChildCount > 0)
                    {
                        line = context.StmtBlk.Start.Line;
                        column = context.StmtBlk.Start.Column;
                    }
                    // --> See XSharpCodeGenerator.GenerateMethod for writing
                    // Copy all source code to User_Data, this includes the leading comments !
                    SaveSourceCode(newMethod, context);
                    FillCodeDomDesignerData(newMethod, line, column);
                }
            }
            //
            this.CurrentType.Members.Add(newMethod);
            // write original source for the attributes
            AddMemberAttributes(newMethod, newMethod.Attributes, context.Modifiers);
            this.addClassMember(new XMemberType(newMethod.Name, MemberTypes.Method, false, returnType.BaseType));

        }

        public override void ExitMethod([NotNull] XSharpParser.MethodContext context)
        {
            // Nested types are stored in Source code completely, so no need to analyze
            if (IsNested)
                return;
            // Reset
            initComponent = null;
            _locals.Clear();
        }



        public override void EnterEvent_([NotNull] XSharpParser.Event_Context context)
        {
            // Nested types are stored in Source code completely, so no need to analyze
            if (IsNested)
                return;
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
                evt.Attributes = decodeMemberAttributes(context.Modifiers._Tokens, context.Modifiers.VIRTUAL());
               
            }
            //
            this.CurrentType.Members.Add(evt);
            // write original source for the attributes
            AddMemberAttributes(evt, evt.Attributes, context.Modifiers);
            this.addClassMember(new XMemberType(evt.Name, MemberTypes.Event, false, null, "Void"));
            SaveSourceCode(evt, context);
        }

        public override void EnterConstructor([NotNull] XSharpParser.ConstructorContext context)
        {
            // Nested types are stored in Source code completely, so no need to analyze
            if (IsNested)
                return;
            CodeTypeMember member;
            MemberAttributes atts = MemberAttributes.Public;
            if (context.Modifiers != null)
            {
                atts = decodeMemberAttributes(context.Modifiers._Tokens);
            }
            if (atts.HasFlag(MemberAttributes.Static))
            {
                member = new XCodeTypeConstructor();
            }
            else
            {
                var ctor = new XCodeConstructor();
                ctor.Parameters.AddRange(GetParametersList(context.ParamList));
                member = ctor;
            }
            member.Attributes = atts;
            // Copy the whole source code in a Snippet Member including the leading trivia and codedom designer data
            SaveSourceCode(member, context);
            // Copy the original Modifiers, so we will not add "Public" afterwards when it did not exist
            AddMemberAttributes(member, member.Attributes, context.Modifiers);
            this.CurrentType.Members.Add(member);
        }

        public override void EnterDestructor([NotNull] XSharpParser.DestructorContext context)
        {
            // Nested types are stored in Source code completely, so no need to analyze
            if (IsNested)
                return;
            // Copy the whole source code in a Snippet Member including the leading trivia and codedom designer data
            CodeSnippetTypeMember snippet = CreateSnippetMember(context);
            this.CurrentType.Members.Add(snippet);
        }

        public override void EnterProperty([NotNull] XSharpParser.PropertyContext context)
        {
            // Nested types are stored in Source code completely, so no need to analyze
            if (IsNested)
                return;
            // Copy the whole source code in a Snippet Member including the leading trivia and codedom designer data
            CodeSnippetTypeMember snippet = CreateSnippetMember(context);
            this.CurrentType.Members.Add(snippet);
        }

        public override void EnterOperator_([NotNull] XSharpParser.Operator_Context context)
        {
            // Nested types are stored in Source code completely, so no need to analyze
            if (IsNested)
                return;
            // Copy the whole source code in a Snippet Member including the leading trivia and codedom designer data
            CodeSnippetTypeMember snippet = CreateSnippetMember(context);
            this.CurrentType.Members.Add(snippet);
        }

        public override void EnterLocalvar([NotNull] XSharpParser.LocalvarContext context)
        {
            // Nested types are stored in Source code completely, so no need to analyze
            if (IsNested)
                return;

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
                    ret.Expression = BuildSnippetExpression(context.Expr);
                }
                initComponent.Statements.Add(ret);
            }
        }

        public override void EnterExpressionStmt([NotNull] XSharpParser.ExpressionStmtContext context)
        {
            // Nested types are stored in Source code completely, so no need to analyze
            if (IsNested)
                return;
            if (initComponent != null)
            {
                CodeStatement stmt ;
                //
                if (context._expression is XSharpParser.AssignmentExpressionContext aec)
                {
                    stmt = CreateAssignStatement(aec);
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
            CodeVariableDeclarationStatement local = new CodeVariableDeclarationStatement
            {
                Name = context.Id.GetCleanText(),
                Type = localType
            };
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

        
        private IXTypeSymbol findType(CodeExpression expr)
        {
            IXTypeSymbol type;
            if (expr is CodeFieldReferenceExpression cfr)
            {
                if (isField(cfr.FieldName) && (cfr.TargetObject is CodeThisReferenceExpression || cfr.TargetObject is CodeBaseReferenceExpression))
                {
                    return getClassMemberType(cfr.FieldName, MemberTypes.Field);
                }
                type = findType(cfr.TargetObject);
                if (type != null)
                {
                    var f = type.GetFields(cfr.FieldName);
                    if (f.Length > 0)
                        return findType(f[0].OriginalTypeName);
                }
            }
            if (expr is CodePropertyReferenceExpression cpr)
            {
                if (isProperty(cpr.PropertyName) && (cpr.TargetObject is CodeThisReferenceExpression || cpr.TargetObject is CodeBaseReferenceExpression))
                {
                    return getClassMemberType(cpr.PropertyName, MemberTypes.Property);
                }
                type = findType(cpr.TargetObject);
                if (type != null)
                {
                    var p = type.GetProperties(cpr.PropertyName);
                    if (p.Length > 0)
                        return findType(p[0].OriginalTypeName);
                }
            }
            if (expr is CodeMethodInvokeExpression cmi)
            {
                if (isMethod(cmi.Method.MethodName) && (cmi.Method.TargetObject is CodeThisReferenceExpression || cmi.Method.TargetObject is CodeBaseReferenceExpression))
                {
                    return getClassMemberType(cmi.Method.MethodName, MemberTypes.Method);

                }
                type = findType(cmi.Method.TargetObject);
                if (type != null)
                {
                    var m = type.GetMethods(cmi.Method.MethodName);
                    if (m.Length > 0)
                        return findType(m[0].OriginalTypeName);
                }
            }
            if (expr is CodeVariableReferenceExpression cvr)
            {
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
                if (!line.StartsWith("//") && i != lines.Length-1)
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
            //if (name.IndexOf("@@") >= 0)
            //    name = name.Replace("@@", "");
            return name;
        }
    }
}

