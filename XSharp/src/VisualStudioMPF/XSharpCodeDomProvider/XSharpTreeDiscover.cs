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

namespace XSharp.CodeDom
{

    class XSharpTreeDiscover : XSharpBaseListener
    {
        private MemberAttributes classVarModifiers;
        private CodeMemberMethod initComponent;

        public XSharpTreeDiscover()
        {
            // The default (empty) CodeCompileUnit, so we can work if none is provided
            this.CodeCompileUnit = new CodeCompileUnit();
            // The default Namespace, so we can work if none is provided... :)
            //this.CurrentNamespace = new CodeNamespace();
            //this.CodeCompileUnit.Namespaces.Add(this.CurrentNamespace);

            // If we have some Nested Namespaces, we will need to keep track
            this.NamespaceStack = new Stack<CodeNamespace>();
            // To store intermediate declarations
            this.LocalDecls = new Stack<XSharpParser.LocalvarContext>();
            //
            this.CurrentFile = "";
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
        public string CurrentFile { get; set; }
        public Stack<XSharpParser.LocalvarContext> LocalDecls { get; private set; }
        public string SourceCode { get; internal set; }

        public override void EnterNamespace_(XSharpParser.Namespace_Context context)
        {
            String newNamespaceName = context.Name.GetText();
            // We already have something in Stack
            // so we are nesting Namespaces, get the previous name prefix
            if (this.NamespaceStack.Count > 0)
            {
                newNamespaceName = this.CurrentNamespace.Name + "." + newNamespaceName;
            }
            CodeNamespace newNamespace = new CodeNamespace(newNamespaceName);
            //
            this.NamespaceStack.Push(this.CurrentNamespace);
            //
            if (String.IsNullOrEmpty(this.CurrentNamespace.Name))
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
                newClass.BaseTypes.Add(new CodeTypeReference(context.BaseType.GetText()));
            }
            // IMPLEMENTS ?
            if ((context._Implements != null) && (context._Implements.Count > 0))
            {
                foreach (var interfaces in context._Implements)
                {
                    newClass.BaseTypes.Add(new CodeTypeReference(interfaces.GetText()));
                }
            }
            //
        }

        public override void EnterMethod([NotNull] XSharpParser.MethodContext context)
        {
            CodeMemberMethod newMethod = new CodeMemberMethod();
            newMethod.Name = context.Id.GetText();
            newMethod.Attributes = MemberAttributes.Public;
            newMethod.Parameters.AddRange(GetParametersList(context.ParamList));
            newMethod.ReturnType = BuildDataType(context.Type); // new CodeTypeReference(context.Type.GetText());
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
                    FillCodeSource(newMethod, context.StmtBlk);

                    // The designer will need to locate the code in the file, so we must add the location
                    FillCodeDomDesignerData(newMethod, context.StmtBlk.Start.Line, context.StmtBlk.Start.Column);
                }
            }
            //
            this.CurrentClass.Members.Add(newMethod);
        }

        public override void ExitMethod([NotNull] XSharpParser.MethodContext context)
        {
            // Reset 
            initComponent = null;
        }

        public override void EnterUsing_([NotNull] XSharpParser.Using_Context context)
        {
            CurrentNamespace.Imports.Add(new CodeNamespaceImport(context.Name.GetText()));
        }

        public override void EnterEvent_([NotNull] XSharpParser.Event_Context context)
        {
            CodeMemberEvent evt = new CodeMemberEvent();
            evt.Name = context.Id.GetText();
            evt.Attributes = MemberAttributes.Public;
            evt.Type = new CodeTypeReference(context.Type.GetText());
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
        }

        public override void EnterClsctor([NotNull] XSharpParser.ClsctorContext context)
        {
            CodeConstructor ctor = new CodeConstructor();
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
                FillCodeSource(ctor, context.StmtBlk);
            }
            //
            this.CurrentClass.Members.Add(ctor);
        }

        public override void EnterClsdtor([NotNull] XSharpParser.ClsdtorContext context)
        {
            // Ok, let's "cheat" : We will not analyze the element
            // we will just copy the whole source code in a Snippet Member
            CodeSnippetTypeMember snippet = CreateSnippetMember(context);
            this.CurrentClass.Members.Add(snippet);
        }

        public override void EnterProperty([NotNull] XSharpParser.PropertyContext context)
        {
            // Ok, let's "cheat" : We will not analyze the element
            // we will just copy the whole source code in a Snippet Member
            CodeSnippetTypeMember snippet = CreateSnippetMember(context);
            this.CurrentClass.Members.Add(snippet);
        }

        public override void EnterOperator_([NotNull] XSharpParser.Operator_Context context)
        {
            // Ok, let's "cheat" : We will not analyze the element
            // we will just copy the whole source code in a Snippet Member
            CodeSnippetTypeMember snippet = CreateSnippetMember(context);
            this.CurrentClass.Members.Add(snippet);
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
                CodeTypeReference fieldType = new CodeTypeReference(context.DataType.GetText());
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
                    this.CurrentClass.Members.Add(field);
                }
                //
            }
        }

        public override void EnterLocalvar([NotNull] XSharpParser.LocalvarContext context)
        {
            if (initComponent != null)
            {
                if (context.DataType != null)
                {
                    CodeStatementCollection locals = new CodeStatementCollection();
                    CodeTypeReference localType = new CodeTypeReference(context.DataType.GetText());
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
                    // LOCAL x,y as STRING
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

        public override void EnterExpressionStmt([NotNull] XSharpParser.ExpressionStmtContext context)
        {
            if (initComponent != null)
            {
                CodeExpression expr = new CodeExpression();
                CodeStatement stmt = new CodeStatement();
                //
                if (context._expression is XSharpParser.AssignmentExpressionContext)
                {
                    XSharpParser.AssignmentExpressionContext exp = (XSharpParser.AssignmentExpressionContext)context._expression;
                    //
                    //what is the left hand side ?
                    //    Self  -> check if Right is in the member of CurrentClass --> FieldReference
                    // else --> always Property
                    //
                    CodeExpression left = BuildExpression(exp.Left, false);
                    CodeExpression right = BuildExpression(exp.Right, true);
                    if (exp.ASSIGN_OP() != null)
                    {
                        //expr = new CodeBinaryOperatorExpression(left, CodeBinaryOperatorType.Assign, right);
                        stmt = new CodeAssignStatement(left, right);
                    }
                    else if (exp.ASSIGN_ADD() != null)
                    {
                        // += Event Handler
                        // We will decode Left as CodeFieldReferenceExpression or CodePropertyReferenceExpression, but we need a CodeEventReferenceExpression
                        CodeEventReferenceExpression cere;
                        if (left is CodeFieldReferenceExpression)
                            cere = new CodeEventReferenceExpression(((CodeFieldReferenceExpression)left).TargetObject, ((CodeFieldReferenceExpression)left).FieldName);
                        else
                            cere = new CodeEventReferenceExpression(((CodePropertyReferenceExpression)left).TargetObject, ((CodePropertyReferenceExpression)left).PropertyName);
                        stmt = new CodeAttachEventStatement(cere, right);
                        //
                    }
                    else if (exp.ASSIGN_SUB() != null)
                    {
                        // -= Event Handler
                        CodeEventReferenceExpression cere;
                        if (left is CodeFieldReferenceExpression)
                            cere = new CodeEventReferenceExpression(((CodeFieldReferenceExpression)left).TargetObject, ((CodeFieldReferenceExpression)left).FieldName);
                        else
                            cere = new CodeEventReferenceExpression(((CodePropertyReferenceExpression)left).TargetObject, ((CodePropertyReferenceExpression)left).PropertyName);
                        stmt = new CodeRemoveEventStatement(cere, right);
                    }
                }
                else if (context._expression is XSharpParser.MethodCallContext)
                {
                    XSharpParser.MethodCallContext exp = (XSharpParser.MethodCallContext)context._expression;
                    expr = BuildExpression(exp,false);
                    stmt = new CodeExpressionStatement(expr);
                }
                else
                {
                    expr = new CodeSnippetExpression(context.GetText());
                    stmt = new CodeExpressionStatement(expr);
                }
                //
                initComponent.Statements.Add(stmt);
            }
        }

        #region Helpers
        private TypeAttributes ContextToClassModifiers(XSharpParser.ClassModifiersContext modifiers)
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

        private MemberAttributes decodeMemberAttributes(IList<IToken> tokens)
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
        private MemberAttributes ContextToMethodModifiers(XSharpParser.MemberModifiersContext modifiers)
        {
            return decodeMemberAttributes(modifiers._Tokens);
        }
        private MemberAttributes ContextToConstructorModifiers(XSharpParser.ConstructorModifiersContext modifiers)
        {
            return decodeMemberAttributes(modifiers._Tokens);
        }

        private MemberAttributes ContextToEventModifiers(XSharpParser.EventModifiersContext modifiers)
        {
            return decodeMemberAttributes(modifiers._Tokens);
        }

        private bool IsEmpty(CodeNamespace nspace)
        {
            return (nspace.Types.Count == 0);
        }

        private CodeParameterDeclarationExpressionCollection GetParametersList(XSharpParser.ParameterListContext paramList)
        {
            CodeParameterDeclarationExpressionCollection pList = new CodeParameterDeclarationExpressionCollection();
            //
            foreach (var param in paramList._Params)
            {
                CodeParameterDeclarationExpression pm = new CodeParameterDeclarationExpression();
                pm.Name = param.Id.GetText();
                pm.Type = BuildDataType(param.Type); // new CodeTypeReference(param.Type.GetText());
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
            //
            return pList;
        }


        private void FillCodeDomDesignerData(CodeObject newElement, int line, int col)
        {
            CodeDomDesignerData data = new CodeDomDesignerData();
            // point is where the designer will try to focus if the
            // user wants to add event handler stuff.  
            data.CaretPosition = new System.Drawing.Point(col, line);
            data.FileName = this.CurrentFile;
            newElement.UserData[typeof(CodeDomDesignerData)] = data;
            newElement.UserData[typeof(System.Drawing.Point)] = data.CaretPosition;
        }

        private void FillCodeSource(CodeObject element, XSharpParser.StatementBlockContext statement)
        {
            int length = statement.Stop.StopIndex - statement.Start.StartIndex;
            string extract = "";
            if (length > 0)
                extract = this.SourceCode.Substring(statement.Start.StartIndex, length);
            element.UserData[XSharpCodeConstants.USERDATA_CODE] = extract;
        }

        private CodeSnippetTypeMember CreateSnippetMember(ParserRuleContext context)
        {
            // The original source code
            String sourceCode = context.GetText();
            //
            CodeSnippetTypeMember snippet = new CodeSnippetTypeMember(sourceCode);
            FillCodeDomDesignerData(snippet, context.Start.Line, context.Start.Column);
            return snippet;
        }



        /// <summary>
        /// Get a LiteralValueContext containing a BIN_CONST, INT_CONST, HEX_CONST, or a REAL_CONST
        /// as a String, and convert it to the "real" value, with the corresponding Type.
        /// </summary>
        /// <param name="context"></param>
        /// <returns>An Object of the needed Type, with the value</returns>
        private object GetNumericValue(XSharpParser.LiteralValueContext context)
        {
            Object ret = null;
            String value = context.GetText();
            //
            if (context.BIN_CONST() != null || context.INT_CONST() != null || context.HEX_CONST() != null)
            {
                bool isUnsigned = (value.Substring(0, 1).ToLower().CompareTo("u") == 0);
                // -1 for Unsigned; -2 for 0x or 0b
                int len = value.Length - (isUnsigned ? 1 : 0) - (context.BIN_CONST() != null || context.HEX_CONST() != null ? 2 : 0);
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
                else if (context.HEX_CONST() != null)
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
                // Should be REAL_CONST
                if (!double.TryParse(value, out d))
                {
                    d = double.NaN;
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
                            // Put the Digits in a String
                            String hexValue = new String(hex, 0, digits);
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

        private CodeVariableDeclarationStatement BuildLocalVar(XSharpParser.LocalvarContext context, CodeTypeReference localType)
        {
            CodeVariableDeclarationStatement local = new CodeVariableDeclarationStatement();
            local.Name = context.Id.GetText();
            local.Type = localType;
            if (context.Expression != null)
            {
                if (context.Expression is XSharpParser.PrimaryExpressionContext)
                {
                    XSharpParser.PrimaryContext ctx = ((XSharpParser.PrimaryExpressionContext)context.Expression).Expr;
                    if (ctx is XSharpParser.LiteralExpressionContext)
                    {
                        XSharpParser.LiteralExpressionContext lit = (XSharpParser.LiteralExpressionContext)ctx;
                        local.InitExpression = BuildLiteralValue(lit.Literal);
                    }
                }
                else
                {
                    local.InitExpression = new CodeSnippetExpression(context.Expression.GetText());
                }
            }

            return local;
        }


        private CodeExpression BuildExpression(XSharpParser.ExpressionContext expression, bool right )
        {
            CodeExpression expr = null;
            //
            if (expression is XSharpParser.PrimaryExpressionContext) // xyz.SimpleName
            {
                expr = BuildExpression((XSharpParser.PrimaryExpressionContext)expression);
            }
            else if (expression is XSharpParser.AccessMemberContext) // xyz.SimpleName
            {
                XSharpParser.AccessMemberContext member = (XSharpParser.AccessMemberContext)expression;
                //what is the left hand side ?
                //    Self  -> check if Right is in the member of CurrentClass --> FieldReference
                // else --> always Property
                bool isMember = false;
                CodeExpression left = BuildExpression(member.Expr,false);
                if (left is CodeThisReferenceExpression)
                {
                    string fieldCandidate = member.Name.GetText();
                    foreach (CodeTypeMember cm in this.CurrentClass.Members)
                    {
                        if (cm is CodeMemberField)
                        {
                            if (String.Compare(fieldCandidate, cm.Name, true) == 0)
                            {
                                isMember = true;
                                break;
                            }
                        }
                    }
                }
                // It seems to be a member...
                if (isMember)
                {
                    expr = new CodeFieldReferenceExpression(BuildExpression(member.Expr,false), member.Name.GetText());
                }
                else
                {
                    // Let's guess that on the Left member, we should have a Property if it is not a Field
                    if (!right)
                    {
                        expr = new CodePropertyReferenceExpression(BuildExpression(member.Expr, false), member.Name.GetText());
                    }
                    else
                    {
                        // We are processing the Right member of an Assignment...
                        // Most likely Enum Value, which is a typereference expression followed by a DOT and a field
                        if (member.DOT() != null) {
                            var typeexpr = new CodeTypeReferenceExpression(member.Expr.GetText());
                            expr = new CodeFieldReferenceExpression(typeexpr, member.Name.GetText());
                        }
                        else {
                            expr = new CodeSnippetExpression(member.GetText());
                        }
                    }
                }
            }
            else if (expression is XSharpParser.MethodCallContext)
            {
                XSharpParser.MethodCallContext meth = (XSharpParser.MethodCallContext)expression;
                CodeExpression target = BuildExpression(meth.Expr,false);
                List<CodeExpression> exprlist = new List<CodeExpression>();
                if (meth.ArgList != null)
                {
                    foreach (var arg in meth.ArgList._Args)
                    {
                        exprlist.Add(BuildExpression(arg.Expr,false));
                    }
                }
                if (target is CodeFieldReferenceExpression)
                {
                    //
                    expr = new CodeMethodInvokeExpression(((CodeFieldReferenceExpression)target).TargetObject, ((CodeFieldReferenceExpression)target).FieldName, exprlist.ToArray());
                }
                else if (target is CodePropertyReferenceExpression)
                {
                    //
                    expr = new CodeMethodInvokeExpression(((CodePropertyReferenceExpression)target).TargetObject, ((CodePropertyReferenceExpression)target).PropertyName, exprlist.ToArray());
                }
                else
                    expr = new CodeMethodInvokeExpression(null, meth.Expr.GetText(), exprlist.ToArray());
            }
            else
            {
                expr = new CodeSnippetExpression(expression.GetText());
            }
            //
            return expr;
        }

        private CodeExpression BuildExpression(XSharpParser.PrimaryExpressionContext expression)
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
                    foreach (var Expression in arr._Exprs)
                    {
                        exprlist.Add(BuildExpression(Expression,true));
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
                CodeExpression ce = BuildExpression(delg.Obj,false);
                //
                expr = new CodeDelegateCreateExpression(BuildDataType(delg.Type), BuildExpression(delg.Obj,false), delg.Func.GetText());

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
                        exprlist.Add(BuildExpression(arg.Expr,false));
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
                String name = ((XSharpParser.NameExpressionContext)ctx).Name.Id.GetText();
                // Sometimes, we will need to do it that way....
                if (name.ToLower() == "self")
                {
                    expr = new CodeThisReferenceExpression();
                }
                else if (name.ToLower() == "super")
                {
                    expr = new CodeBaseReferenceExpression();
                }
                else
                {
                    CodeTypeReference ctr = BuildSimpleName(((XSharpParser.NameExpressionContext)ctx).Name);
                    expr = new CodeVariableReferenceExpression(name);
                }
            }
            else
            {
                expr = new CodeSnippetExpression(ctx.GetText());
            }
            return expr;
        }

        private CodeTypeReference BuildNativeType(XSharpParser.NativeTypeContext nativeType)
        {
            CodeTypeReference expr = null;
            //
            if ((nativeType.BYTE() != null) ||
                (nativeType.CHAR() != null) ||
                (nativeType.DWORD() != null) ||
                (nativeType.DYNAMIC() != null) ||
                (nativeType.INT() != null) ||
                (nativeType.INT64() != null) ||
                (nativeType.LOGIC() != null) ||
                (nativeType.LONGINT() != null) ||
                (nativeType.OBJECT() != null) ||
                (nativeType.PTR() != null) ||
                (nativeType.REAL4() != null) ||
                (nativeType.REAL8() != null) ||
                (nativeType.SHORTINT() != null) ||
                (nativeType.STRING() != null) ||
                (nativeType.UINT64() != null) ||
                (nativeType.VOID() != null) ||
                (nativeType.WORD() != null))
            {
                expr = BuildNativeType(nativeType.GetText());
            }
            //
            return expr;
        }

        private CodeTypeReference BuildNativeType(String nativeType)
        {
            CodeTypeReference expr = null;
            //
            switch (nativeType.ToLower())
            {
                case "byte":
                    nativeType = "System.Byte";
                    break;
                case "dword":
                    nativeType = "System.UInt32";
                    break;
                case "shortint":
                    nativeType = "System.Int16";
                    break;
                case "int":
                    nativeType = "System.Int32";
                    break;
                case "int64":
                    nativeType = "System.Int64";
                    break;
                case "uint64":
                    nativeType = "System.UInt64";
                    break;
                case "logic":
                    nativeType = "System.Boolean";
                    break;
                case "object":
                    nativeType = "System.Object";
                    break;
                case "real4":
                    nativeType = "System.Single";
                    break;
                case "real8":
                    nativeType = "System.Double";
                    break;
                case "string":
                    nativeType = "System.String";
                    break;
                case "word":
                    nativeType = "System.UInt16";
                    break;
                case "void":
                    nativeType = "System.Void";
                    break;
            }
            expr = new CodeTypeReference(nativeType);
            return expr;
        }

        private CodeTypeReference BuildXBaseType(XSharpParser.XbaseTypeContext xbaseType)
        {
            CodeTypeReference expr = null;
            //
            if ((xbaseType.ARRAY() != null) ||
                (xbaseType.CODEBLOCK() != null) ||
                (xbaseType.DATE() != null) ||
                (xbaseType.FLOAT() != null) ||
                (xbaseType.PSZ() != null) ||
                (xbaseType.SYMBOL() != null) ||
                (xbaseType.USUAL() != null))
            {
                expr = new CodeTypeReference(xbaseType.GetText());
            }
            //
            return expr;
        }

        private CodeTypeReference BuildSimpleName(XSharpParser.SimpleNameContext simpleName)
        {
            CodeTypeReference expr = null;
            //
            String name = simpleName.Id.GetText();
            String gen = "";
            if (simpleName.GenericArgList != null)
            {
                string argList = "";
                int i = 0;
                foreach (var generic in simpleName.GenericArgList._GenericArgs)
                {
                    if (i > 0)
                        argList += ",";
                    CodeTypeReference tmp = BuildDataType(generic);
                    argList += tmp.BaseType;
                    i++;
                }
                //
                gen = "`" + i.ToString() + "[" + argList + "]";
            }
            expr = new CodeTypeReference(name + gen);
            //
            return expr;
        }

        private CodeExpression BuildLiteralValue(XSharpParser.LiteralValueContext context)
        {
            CodeExpression expr = null;
            ITerminalNode node;
            //
            node = context.BIN_CONST();
            if (node != null)
            {
                expr = new CodePrimitiveExpression(GetNumericValue(context));
            }
            node = context.INT_CONST();
            if (node != null)
            {
                expr = new CodePrimitiveExpression(GetNumericValue(context));
            }
            //
            node = context.HEX_CONST();
            if (node != null)
            {
                expr = new CodePrimitiveExpression(GetNumericValue(context));
            }
            //
            node = context.REAL_CONST();
            if (node != null)
            {
                expr = new CodePrimitiveExpression(GetNumericValue(context));
            }
            //
            node = context.TRUE_CONST();
            if (node != null)
            {
                expr = new CodePrimitiveExpression(true);
            }
            //
            node = context.FALSE_CONST();
            if (node != null)
            {
                expr = new CodePrimitiveExpression(false);
            }
            //
            node = context.STRING_CONST();
            if (node != null)
            {
                // Remove the quotes
                String value = context.GetText();
                value = value.Substring(1, value.Length - 2);
                expr = new CodePrimitiveExpression(value);
            }
            //
            node = context.ESCAPED_STRING_CONST();
            if (node != null)
            {
                // Remove the e in front of quotes, AND the Quotes
                String value = context.GetText();
                value = value.Substring(1);
                value = value.Substring(1, value.Length - 2);
                expr = new CodePrimitiveExpression(BuildUnEscapedString(value));
            }
            //
            node = context.CHAR_CONST();
            if (node != null)
            {
                // Remove the quotes
                String value = context.GetText();
                value = value.Substring(1, value.Length - 2);
                if (value.Length >= 1)
                    expr = new CodePrimitiveExpression(value[0]);
            }
            //
            node = context.NIL();
            if (node != null)
            {
                expr = new CodeSnippetExpression("NIL");
            }
            //
            node = context.NULL();
            if (node != null)
            {
                expr = new CodePrimitiveExpression(null);
            }
            //
            node = context.NULL_ARRAY();
            if (node != null)
            {
                expr = new CodeSnippetExpression("NULL_ARRAY");
            }
            //
            node = context.NULL_CODEBLOCK();
            if (node != null)
            {
                expr = new CodeSnippetExpression("NULL_CODEBLOCK");
            }
            //
            node = context.NULL_DATE();
            if (node != null)
            {
                expr = new CodeSnippetExpression("NULL_DATE");
            }
            //
            node = context.NULL_OBJECT();
            if (node != null)
            {
                expr = new CodeSnippetExpression("NULL_OBJECT");
            }
            //            
            node = context.NULL_PSZ();
            if (node != null)
            {
                expr = new CodeSnippetExpression("NULL_PSZ");
            }
            //            
            node = context.NULL_PTR();
            if (node != null)
            {
                expr = new CodeSnippetExpression("NULL_PTR");
            }
            //            
            node = context.NULL_STRING();
            if (node != null)
            {
                expr = new CodeSnippetExpression("NULL_STRING");
            }
            //            
            node = context.NULL_SYMBOL();
            if (node != null)
            {
                expr = new CodeSnippetExpression("NULL_SYMBOL");
            }
            //
            node = context.SYMBOL_CONST();
            if (node != null)
            {
                expr = new CodeSnippetExpression(context.GetText().ToUpper());
            }
            //
            node = context.DATE_CONST();
            if (node != null)
            {
                expr = new CodeSnippetExpression(context.GetText());
            }
            //
            if ( expr == null )
            {
                expr = new CodeSnippetExpression(context.GetText());
            }
            return expr;
        }

        private CodeTypeReference BuildDataType(XSharpParser.DatatypeContext context)
        {
            CodeTypeReference expr = null;
            //
            if (context is XSharpParser.PtrDatatypeContext)
            {
                XSharpParser.PtrDatatypeContext ptrData = (XSharpParser.PtrDatatypeContext)context;
                if (ptrData.TypeName.NativeType != null)
                    expr = BuildNativeType(ptrData.TypeName.NativeType);
                else if (ptrData.TypeName.XType != null)
                    expr = BuildXBaseType(ptrData.TypeName.XType);
                else if (ptrData.TypeName.Name != null)
                    expr = BuildName(ptrData.TypeName.Name);
            }
            else if (context is XSharpParser.ArrayDatatypeContext)
            {

            }
            else if (context is XSharpParser.SimpleDatatypeContext)
            {
                XSharpParser.SimpleDatatypeContext sdt = (XSharpParser.SimpleDatatypeContext)context;
                if (sdt.TypeName.Name != null)
                    expr = BuildName(sdt.TypeName.Name);
                else
                    expr = BuildNativeType(sdt.TypeName.GetText());
            }
            else if (context is XSharpParser.NullableDatatypeContext)
            {

            }
            //
            return expr;
        }

        private CodeTypeReference BuildName(XSharpParser.NameContext context)
        {
            CodeTypeReference expr = null;
            //
            if (context is XSharpParser.QualifiedNameContext)
            {
                XSharpParser.QualifiedNameContext qual = (XSharpParser.QualifiedNameContext)context;
                expr = BuildName(qual.Left);
                expr = new CodeTypeReference(expr.BaseType + "." + BuildSimpleName(qual.Right).BaseType);
            }
            else if (context is XSharpParser.SimpleOrAliasedNameContext)
            {
                XSharpParser.SimpleOrAliasedNameContext alias = (XSharpParser.SimpleOrAliasedNameContext)context;
                //
                if (alias.Name is XSharpParser.AliasQualifiedNameContext)
                {
                    XSharpParser.AliasQualifiedNameContext al = (XSharpParser.AliasQualifiedNameContext)alias.Name;
                    expr = BuildSimpleName(al.Right);
                    expr = new CodeTypeReference(al.Alias.GetText() + "::" + expr.BaseType);
                }
                else if (alias.Name is XSharpParser.GlobalQualifiedNameContext)
                {
                    XSharpParser.GlobalQualifiedNameContext gbl = (XSharpParser.GlobalQualifiedNameContext)alias.Name;
                    expr = BuildSimpleName(gbl.Right);
                    expr = new CodeTypeReference("GLOBAL::" + expr.BaseType);
                }
                else if (alias.Name is XSharpParser.IdentifierOrGenericNameContext)
                {
                    XSharpParser.IdentifierOrGenericNameContext id = (XSharpParser.IdentifierOrGenericNameContext)alias.Name;
                    expr = BuildSimpleName(id.Name);
                }
            }
            //
            return expr;
        }

        #endregion
    }
}
