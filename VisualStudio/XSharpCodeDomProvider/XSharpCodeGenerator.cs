//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using System.IO;

// XSharpCodeGenerator
// Used by Designers (WPF and WinForms at least)
// !!!WARNING !!! XSharp does support "." or ":" as selector...

// XSHARP   : Warning, when a Parser read some code, it will store/save it in the UserData Property of the element
//          : When passed back currently that code is not used/ReInjected

namespace XSharp.CodeDom
{
    public partial class XSharpCodeGenerator : CodeCompiler
    {
        private bool generatingForLoop;
        private string selector;
        private string staticSelector;
        private string entrypointCode = null;
        private List<String> _using;
        public XSharpCodeGenerator() : base()
        {
            this.selector = ":";
            this.staticSelector = ".";
            _using = new List<String>();
        }

        protected override string NullToken
        {
            get
            {
                return "NULL";
            }
        }

        protected override string CreateEscapedIdentifier(string value)
        {
            // Is is a reserved Keyword ?
            if (XSharpKeywords.Contains(value))
            {
                value = "@@" + value;
            }
            return value;
        }

        protected override string CreateValidIdentifier(string value)
        {
            // Is is a reserved Keyword ?
            if (XSharpKeywords.Contains(value))
            {
                value = "_" + value;
            }
            return value;
        }
        protected override void GenerateArgumentReferenceExpression(CodeArgumentReferenceExpression e)
        {
            // Be sure to write a correct string
            this.OutputIdentifier(e.ParameterName);
        }

        protected override void GenerateArrayCreateExpression(CodeArrayCreateExpression e)
        {
            CodeExpressionCollection initializers = e.Initializers;
            // Literal array
            if (initializers.Count > 0)
            {
                // the syntax is something like <int>{ 10,12,14 }
                this.Output.Write("<");
                // Is a specific type indicated ?
                if (e.CreateType.ArrayElementType != null)
                {
                    this.OutputType(e.CreateType.ArrayElementType);
                }
                else
                {
                    this.OutputType(e.CreateType);
                }
                //
                this.Output.Write(">{ ");
                this.OutputExpressionList(initializers, false);
                this.Output.Write(" }");
            }
            else
            {
                // Standard Array declaration
                base.Output.Write(this.GetBaseTypeOutput(e.CreateType));
                base.Output.Write("[");
                if (e.SizeExpression != null)
                {
                    base.GenerateExpression(e.SizeExpression);
                }
                else
                {
                    base.Output.Write(e.Size);
                }
                base.Output.Write("]");
            }
        }

        protected override void GenerateArrayIndexerExpression(CodeArrayIndexerExpression e)
        {
            this.GenerateExpression(e.TargetObject);
            this.Output.Write("[");
            bool flag = true;
            foreach (CodeExpression expression in e.Indices)
            {
                if (flag)
                {
                    flag = false;
                }
                else
                {
                    this.Output.Write(", ");
                }
                this.GenerateExpression(expression);
            }
            this.Output.Write("]");
        }

        protected override void GenerateAssignStatement(CodeAssignStatement e)
        {
            this.GenerateExpression(e.Left);
            this.Output.Write(" := ");
            this.GenerateExpression(e.Right);
            // This one is set in GenerateIterationStatement
            if (!this.generatingForLoop)
            {
                this.Output.WriteLine();
            }
        }

        protected override void GenerateAttachEventStatement(CodeAttachEventStatement e)
        {
            this.GenerateEventReferenceExpression(e.Event);
            this.Output.Write(" += ");
            this.GenerateExpression(e.Listener);
            this.Output.WriteLine();
        }

        protected override void GenerateAttributeDeclarationsEnd(CodeAttributeDeclarationCollection attributes)
        {
            this.Output.Write("] ;");
        }

        protected override void GenerateAttributeDeclarationsStart(CodeAttributeDeclarationCollection attributes)
        {
            this.Output.Write("[");
        }

        protected override void GenerateBaseReferenceExpression(CodeBaseReferenceExpression e)
        {
            base.Output.Write("SUPER");
        }

        protected override void GenerateCastExpression(CodeCastExpression e)
        {
            this.Output.Write("((");
            this.OutputType(e.TargetType);
            this.Output.Write(")(");
            this.GenerateExpression(e.Expression);
            this.Output.Write("))");
        }

        protected override void GenerateComment(CodeComment e)
        {
            string startLine = e.DocComment ? "///" : "//";
            this.Output.Write(startLine);
            this.Output.Write(" ");
            string chars = e.Text;
            for (int i = 0; i < chars.Length; i++)
            {
                if (chars[i] != '\0')
                {
                    this.Output.Write(chars[i].ToString());
                    if (chars[i] == '\r')
                    {
                        if ((i < (chars.Length - 1)) && (chars[i + 1] == '\n'))
                        {
                            this.Output.WriteLine("");
                            i++;
                        }
                        this.Output.Write(startLine);
                    }
                    else if (chars[i] == '\n')
                    {
                        this.Output.Write(startLine);
                    }
                    // Unicode version ?
                    else if (((chars[i] == '\u2028') || (chars[i] == '\u2029')) || (chars[i] == '\x0085'))
                    {
                        this.Output.Write(startLine);
                    }
                }
            }
            this.Output.WriteLine();
        }

        protected override void GenerateConditionStatement(CodeConditionStatement e)
        {
            base.Output.Write("if (");
            base.GenerateExpression(e.Condition);
            base.Output.Write(")");
            base.Output.WriteLine();
            this.Indent++;
            base.GenerateStatements(e.TrueStatements);
            this.Indent--;
            if (e.FalseStatements.Count > 0)
            {
                base.Output.WriteLine();
                base.Output.Write("else");
                base.Output.WriteLine();
                this.Indent++;
                base.GenerateStatements(e.FalseStatements);
                this.Indent--;
            }
            base.Output.WriteLine("endif");
        }

        protected override void GenerateConstructor(CodeConstructor e, CodeTypeDeclaration c)
        {
            if (base.IsCurrentClass || base.IsCurrentStruct)
            {
                // Do we have some Source Code pushed here by our Parser ??
                // when so then that source code also includes the constructor line
                if (e.UserData.Contains(XSharpCodeConstants.USERDATA_CODE))
                {
                    String sourceCode = e.UserData[XSharpCodeConstants.USERDATA_CODE] as string;
                    this.Output.Write(sourceCode);
                }
                else
                {

                    if (e.CustomAttributes.Count > 0)
                    {
                        this.GenerateAttributes(e.CustomAttributes);
                    }
                    this.OutputMemberAccessModifier(e.Attributes);
                    base.Output.Write("CONSTRUCTOR(");
                    this.OutputParameters(e.Parameters);
                    base.Output.WriteLine(")  STRICT");
                    CodeExpressionCollection baseConstructorArgs = e.BaseConstructorArgs;
                    CodeExpressionCollection chainedConstructorArgs = e.ChainedConstructorArgs;
                    if (baseConstructorArgs.Count > 0)
                    {
                        this.Indent++;
                        base.Output.Write("SUPER(");
                        this.OutputExpressionList(baseConstructorArgs);
                        base.Output.WriteLine(");");
                        this.Indent--;
                    }
                    else if (chainedConstructorArgs.Count > 0)
                    {
                        this.Indent++;
                        base.Output.Write("SELF(");
                        this.OutputExpressionList(chainedConstructorArgs);
                        base.Output.WriteLine(");");
                        this.Indent--;
                    }
                    this.Indent++;
                    this.GenerateStatements(e.Statements);
                }
                this.Indent--;
            }
        }

        protected override void GenerateDelegateCreateExpression(CodeDelegateCreateExpression e)
        {
            this.OutputType(e.DelegateType);
            base.Output.Write("{ ");
            this.GenerateExpression(e.TargetObject);
            base.Output.Write(", @");
            this.OutputIdentifier(e.MethodName);
            base.Output.Write("() }");
        }

        protected override void GenerateDelegateInvokeExpression(CodeDelegateInvokeExpression e)
        {
            if (e.TargetObject != null)
            {
                this.GenerateExpression(e.TargetObject);
            }
            base.Output.Write("(");
            this.OutputExpressionList(e.Parameters);
            base.Output.Write(")");
        }

        protected override void GenerateEntryPointMethod(CodeEntryPointMethod e, CodeTypeDeclaration c)
        {
            // we must collect this and insert it at the end of the unit
            // so replace the output field in the parent class and restore it later
            var writer = new StringWriter();
            FieldInfo field = typeof(CodeGenerator).GetField("output", BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.FlattenHierarchy);
            FieldInfo field2 = typeof(IndentedTextWriter).GetField("tabString", BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.FlattenHierarchy);
            IndentedTextWriter oldWriter = (IndentedTextWriter)field.GetValue(this);
            String tabString = (String)field2.GetValue(oldWriter);
            IndentedTextWriter newWriter = new IndentedTextWriter(writer, tabString); ;
            try
            {
                field.SetValue(this, newWriter);
                this.GenerateCommentStatements(e.Comments);

                if (e.CustomAttributes.Count > 0)
                {
                    this.GenerateAttributes(e.CustomAttributes);
                }
                base.Output.Write("FUNCTION Start() AS ");
                this.OutputType(e.ReturnType);
                base.Output.WriteLine();
                this.Indent++;
                this.GenerateStatements(e.Statements);
                this.Indent--;
                base.Output.WriteLine();
            }
            finally
            {
                entrypointCode = writer.GetStringBuilder().ToString();
                field.SetValue(this, oldWriter);
            }
        }

        protected override void GenerateEvent(CodeMemberEvent e, CodeTypeDeclaration c)
        {
            if (!this.IsCurrentDelegate && !this.IsCurrentEnum)
            {
                if (e.CustomAttributes.Count > 0)
                {
                    this.GenerateAttributes(e.CustomAttributes);
                }
                if (e.PrivateImplementationType == null)
                {
                    this.OutputMemberAccessModifier(e.Attributes);
                }
                else
                {
                    base.Output.Write("VIRTUAL ");
                }
                base.Output.Write("EVENT ");
                string fqdn = e.Name;

                if (e.PrivateImplementationType != null)
                {
                    fqdn = e.PrivateImplementationType.BaseType + this.selector + fqdn;
                }
                this.OutputIdentifier(fqdn);
                base.Output.Write(" AS ");
                this.OutputType(e.Type);
                base.Output.WriteLine();
            }
        }

        protected override void GenerateEventReferenceExpression(CodeEventReferenceExpression e)
        {
            if (e.TargetObject != null)
            {
                base.GenerateExpression(e.TargetObject);
                base.Output.Write(this.selector);
            }
            this.OutputIdentifier(e.EventName);
        }

        protected override void GenerateExpressionStatement(CodeExpressionStatement e)
        {
            base.GenerateExpression(e.Expression);
            if (!this.generatingForLoop)
            {
                base.Output.WriteLine();
            }
        }

        protected override void GenerateField(CodeMemberField e)
        {
            if (!this.IsCurrentDelegate && !this.IsCurrentInterface)
            {
                if (this.IsCurrentEnum)
                {
                    if (e.CustomAttributes.Count > 0)
                    {
                        this.GenerateAttributes(e.CustomAttributes);
                    }
                    this.OutputIdentifier(e.Name);
                    if (e.InitExpression != null)
                    {
                        base.Output.Write(" := ");
                        this.GenerateExpression(e.InitExpression);
                    }
                    base.Output.WriteLine();
                }
                else
                {
                    if (e.CustomAttributes.Count > 0)
                    {
                        this.GenerateAttributes(e.CustomAttributes);
                    }

                    this.OutputMemberAccessModifier(e.Attributes);
                    this.OutputFieldScopeModifier(e.Attributes);

                    this.OutputIdentifier(e.Name);

                    if (e.InitExpression != null)
                    {
                        base.Output.Write(" := ");
                        this.GenerateExpression(e.InitExpression);
                    }
                    base.Output.Write(" AS ");
                    this.OutputType(e.Type);
                    //base.Output.WriteLine();
                }
            }
        }

        protected override void GenerateFieldReferenceExpression(CodeFieldReferenceExpression e)
        {
            if (e.TargetObject != null)
            {
                this.GenerateExpression(e.TargetObject);
                // If we have a Type, we must use a dot as a selector
                if (e.TargetObject.GetType() == typeof(CodeTypeReferenceExpression))
                {
                    base.Output.Write(this.staticSelector);
                }
                else
                {
                    base.Output.Write(this.selector);
                }
            }

            this.OutputIdentifier(e.FieldName);
        }

        protected override void GenerateGotoStatement(CodeGotoStatement e)
        {
            // Nothing here currently ;)
        }

        protected override void GenerateIndexerExpression(CodeIndexerExpression e)
        {
            this.GenerateExpression(e.TargetObject);
            base.Output.Write("[");
            bool flag = true;
            foreach (CodeExpression expression1 in e.Indices)
            {
                if (flag)
                {
                    flag = false;
                }
                else
                {
                    base.Output.Write(", ");
                }
                this.GenerateExpression(expression1);
            }
            base.Output.Write("]");
        }

        protected override void GenerateIterationStatement(CodeIterationStatement e)
        {
            this.generatingForLoop = true;
            this.GenerateStatement(e.InitStatement);
            base.Output.WriteLine();
            base.Output.Write("DO WHILE ");
            this.GenerateExpression(e.TestExpression);
            base.Output.WriteLine();
            this.generatingForLoop = false;
            this.Indent++;
            this.GenerateStatements(e.Statements);
            this.GenerateStatement(e.IncrementStatement);
            this.Indent--;
            base.Output.WriteLine("ENDDO");
        }

        protected override void GenerateLabeledStatement(CodeLabeledStatement e)
        {
            // Currently, nothing here
        }

        protected override void GenerateLinePragmaEnd(CodeLinePragma e)
        {
            // Currently, nothing here
        }

        protected override void GenerateLinePragmaStart(CodeLinePragma e)
        {
            // Currently, nothing here
        }

        protected override void GenerateMethod(CodeMemberMethod e, CodeTypeDeclaration c)
        {

            //
            if ((this.IsCurrentClass || this.IsCurrentStruct) || this.IsCurrentInterface)
            {
                // Do we have some Source Code pushed here by our Parser ??
                // this code contains the method declaration line as well
                if (e.UserData.Contains(XSharpCodeConstants.USERDATA_CODE))
                {
                    String sourceCode = e.UserData[XSharpCodeConstants.USERDATA_CODE] as string;
                    this.Output.Write(sourceCode);
                }
                else
                {
                    if (e.CustomAttributes.Count > 0)
                    {
                        this.GenerateAttributes(e.CustomAttributes);
                    }

                    if (e.ReturnTypeCustomAttributes.Count > 0)
                    {
                        this.GenerateAttributes(e.ReturnTypeCustomAttributes, "return: ");
                    }

                    if (!base.IsCurrentInterface)
                    {
                        if (e.PrivateImplementationType == null)
                        {
                            this.OutputMemberAccessModifier(e.Attributes);
                            this.OutputMemberScopeModifier(e.Attributes);
                        }
                        else
                        {
                            // Per default, all Methods are VIRTUALs
                            base.Output.Write("VIRTUAL ");
                        }
                    }

                    base.Output.Write("METHOD ");

                    if (e.PrivateImplementationType != null)
                    {
                        base.Output.Write(e.PrivateImplementationType.BaseType);
                        base.Output.Write(this.staticSelector);
                    }

                    this.OutputIdentifier(e.Name);
                    this.OutputGenericParameters(e.TypeParameters);
                    base.Output.Write("(");
                    this.OutputParameters(e.Parameters);
                    base.Output.Write(") AS ");
                    this.OutputType(e.ReturnType);
                    base.Output.Write(" STRICT");
                    base.Output.WriteLine();
                    this.Indent++;
                    if (!this.IsCurrentInterface && ((e.Attributes & MemberAttributes.ScopeMask) != MemberAttributes.Abstract))
                    {
                        this.GenerateStatements(e.Statements);
                    }
                    this.Indent--;
                }
            }

        }

        protected override void GenerateMethodInvokeExpression(CodeMethodInvokeExpression e)
        {
            if (e.Method.TargetObject is CodeTypeReferenceExpression)
            {
                GenerateExpression(e.Method.TargetObject);
                base.Output.Write(staticSelector);
            }
            else if (e.Method.TargetObject is CodeBinaryOperatorExpression)
            {
                base.Output.Write("(");
                this.GenerateExpression(e.Method.TargetObject);
                base.Output.Write(")");
                base.Output.Write(selector);
            }
            else
            {
                this.GenerateExpression(e.Method.TargetObject);
                base.Output.Write(selector);
            }
            if (e.Method.MethodName != null)
            {

                base.Output.Write(e.Method.MethodName);
            }
            EmitGenericTypeArgs(e.Method.TypeArguments);
            base.Output.Write("(");
            OutputExpressionList(e.Parameters);
            base.Output.Write(")");
        }

        private void EmitGenericTypeArgs(CodeTypeReferenceCollection typeArgs)
        {
            if (typeArgs != null && typeArgs.Count > 0)
            {
                base.Output.Write("[");
                for (int i = 0; i < typeArgs.Count; i++)
                {
                    if (i != 0)
                        base.Output.Write(", ");
                    base.Output.Write(typeArgs[i].BaseType);
                }
                base.Output.Write("]");
            }
        }


        protected override void GenerateCompileUnitStart(CodeCompileUnit e)
        {
            bool generateComment = true;
            _using.Clear();
            base.GenerateCompileUnitStart(e);
            if (e.UserData.Contains(XSharpCodeConstants.USERDATA_NOHEADER))
            {
                generateComment = false;
            }
            if (generateComment)
            {
                this.Output.WriteLine("//------------------------------------------------------------------------------");
                this.Output.WriteLine("//  <auto-generated>");
                this.Output.WriteLine("//     This code was generated by a tool.");
                this.Output.WriteLine("//     Runtime version: " + Environment.Version.ToString());
                this.Output.WriteLine("//     Generator      : XSharp.CodeDomProvider " + typeof(XSharpCodeGenerator).Assembly.GetName().Version.ToString());
                this.Output.WriteLine("//     Timestamp      : " + System.DateTime.Now.ToString());
                this.Output.WriteLine("//     ");
                this.Output.WriteLine("//     Changes to this file may cause incorrect behavior and may be lost if");
                this.Output.WriteLine("//     the code is regenerated.");
                this.Output.WriteLine("//  </auto-generated>");
                this.Output.WriteLine("//------------------------------------------------------------------------------");
            }
        }
        protected override void GenerateCompileUnitEnd(CodeCompileUnit e)
        {
            if (!String.IsNullOrEmpty(entrypointCode))
            {
                this.Output.Write(entrypointCode);
                entrypointCode = null;
            }
            base.GenerateCompileUnitEnd(e);
        }

        protected override void GenerateMethodReferenceExpression(CodeMethodReferenceExpression e)
        {
            if (e.TargetObject != null)
            {
                if (e.TargetObject is CodeBinaryOperatorExpression)
                {
                    base.Output.Write("(");
                    base.GenerateExpression(e.TargetObject);
                    base.Output.Write(")");
                }
                else
                {
                    base.GenerateExpression(e.TargetObject);
                }
                base.Output.Write(this.staticSelector);
            }
            this.OutputIdentifier(e.MethodName);
            if (e.TypeArguments.Count > 0)
            {
                this.OutputGenericArguments(e.TypeArguments);
            }
        }

        protected override void GenerateMethodReturnStatement(CodeMethodReturnStatement e)
        {
            base.Output.Write("RETURN");
            if (e.Expression != null)
            {
                base.Output.Write(" ");
                base.GenerateExpression(e.Expression);
            }
            base.Output.WriteLine();
        }

        private void writeCodeBefore(IDictionary userData)
        {
            if (userData.Contains(XSharpCodeConstants.USERDATA_CODEBEFORE))
            {
                string before = (string)userData[XSharpCodeConstants.USERDATA_CODEBEFORE];
                if (! before.Contains("<auto-generated>"))
                {
                    this.Output.Write(before);
                }
            }
        }

        protected override void GenerateNamespace(CodeNamespace e)
        {
            this.Options.BlankLinesBetweenMembers = false;
                this.GenerateCommentStatements(e.Comments);

            // Generate Imports BEFORE the NameSpace
            writeCodeBefore(e.UserData);
            //
            this.GenerateNamespaceStart(e);
            //this.Output.WriteLine("");
            this.GenerateTypes(e);
            this.GenerateNamespaceEnd(e);
        }

        protected override void GenerateNamespaceEnd(CodeNamespace e)
        {
            if (!String.IsNullOrEmpty(e.Name))
            {
                if (this.Indent >= 0)
                    this.Indent--;
                base.Output.WriteLine("END NAMESPACE");
            }
        }

        protected override void GenerateNamespaceImport(CodeNamespaceImport e)
        {
            if (!_using.Contains(e.Namespace.ToLowerInvariant()))
            {
                base.Output.Write("USING ");
                this.OutputIdentifier(e.Namespace);
                base.Output.WriteLine();
                _using.Add(e.Namespace.ToLowerInvariant());
            }
        }

        protected override void GenerateNamespaceStart(CodeNamespace e)
        {
            if (!String.IsNullOrEmpty(e.Name))
            {
                string name = e.Name;
                if (name.StartsWith("global::",StringComparison.OrdinalIgnoreCase))
                {
                    name = name.Substring(8);
                }
                base.Output.WriteLine("BEGIN NAMESPACE " + name);
                this.Indent++;
            }
        }

        protected override void GenerateObjectCreateExpression(CodeObjectCreateExpression e)
        {
            this.OutputType(e.CreateType);
            base.Output.Write("{");
            this.OutputExpressionList(e.Parameters);
            base.Output.Write("}");
        }

        protected override void GenerateProperty(CodeMemberProperty e, CodeTypeDeclaration c)
        {
            if ((this.IsCurrentClass || this.IsCurrentStruct) || this.IsCurrentInterface)
            {
                if (e.CustomAttributes.Count > 0)
                {
                    this.GenerateAttributes(e.CustomAttributes);
                }

                if (!this.IsCurrentInterface)
                {
                    if (e.PrivateImplementationType == null)
                    {
                        this.OutputMemberAccessModifier(e.Attributes);
                        this.OutputMemberScopeModifier(e.Attributes);
                    }
                    else
                    {
                        base.Output.Write("VIRTUAL ");
                    }
                }

                base.Output.Write("PROPERTY ");
                if ((e.PrivateImplementationType != null) && !this.IsCurrentInterface)
                {
                    base.Output.Write(e.PrivateImplementationType.BaseType);
                    base.Output.Write(this.staticSelector);
                }
                this.OutputIdentifier(e.Name);

                if (e.Parameters.Count > 0)
                {
                    base.Output.Write("[");
                    this.OutputParameters(e.Parameters);
                    base.Output.Write("]");
                }
                base.Output.Write(" AS ");
                this.OutputType(e.Type);
                base.Output.WriteLine();
                //
                if (e.HasGet)
                {
                    base.Output.WriteLine();
                    this.Indent++;
                    base.Output.Write("GET");
                    base.Output.WriteLine();
                    this.Indent++;
                    this.GenerateStatements(e.GetStatements);
                    this.Indent--;
                    base.Output.Write("END GET");
                    base.Output.WriteLine();
                    this.Indent--;
                }

                if (e.HasSet)
                {
                    base.Output.WriteLine();
                    this.Indent++;
                    base.Output.Write("SET");
                    base.Output.WriteLine();
                    this.Indent++;
                    this.GenerateStatements(e.SetStatements);
                    this.Indent--;
                    base.Output.Write("END SET");
                    base.Output.WriteLine();
                    this.Indent--;
                }
                //
                base.Output.Write("END PROPERTY");
                base.Output.WriteLine();
            }
        }

        protected override void GeneratePropertyReferenceExpression(CodePropertyReferenceExpression e)
        {
            bool isIdentifier = true;

            if (e.TargetObject != null)
            {
                this.GenerateExpression(e.TargetObject);
                if (e.TargetObject is CodeTypeReferenceExpression)
                {
                    isIdentifier = false;
                    this.Output.Write(this.staticSelector);
                }
                else
                {
                    this.Output.Write(this.selector);
                }
            }
            if (isIdentifier)
                this.OutputIdentifier(e.PropertyName);
            else
                this.Output.Write(e.PropertyName);

        }

        protected override void GeneratePropertySetValueReferenceExpression(CodePropertySetValueReferenceExpression e)
        {
            base.Output.Write("value");
        }

        protected override void GenerateRemoveEventStatement(CodeRemoveEventStatement e)
        {
            this.GenerateEventReferenceExpression(e.Event);
            base.Output.Write(" -= ");
            this.GenerateExpression(e.Listener);
            base.Output.WriteLine();
        }

        protected override void GenerateSnippetExpression(CodeSnippetExpression e)
        {
            base.Output.Write(e.Value);
        }

        protected override void GenerateSnippetMember(CodeSnippetTypeMember e)
        {
            base.Output.Write(e.Text);
        }

        protected override void GenerateThisReferenceExpression(CodeThisReferenceExpression e)
        {
            base.Output.Write("SELF");
        }

        protected override void GenerateThrowExceptionStatement(CodeThrowExceptionStatement e)
        {
            base.Output.Write("THROW");
            if (e.ToThrow != null)
            {
                base.Output.Write(" ");
                this.GenerateExpression(e.ToThrow);
            }
            base.Output.WriteLine();
        }

        protected override void GenerateTryCatchFinallyStatement(CodeTryCatchFinallyStatement e)
        {
            base.Output.WriteLine("TRY");
            this.Indent++;
            this.GenerateStatements(e.TryStatements);
            this.Indent--;
            CodeCatchClauseCollection catches = e.CatchClauses;
            if (catches.Count > 0)
            {
                IEnumerator enumerator1 = catches.GetEnumerator();
                while (enumerator1.MoveNext())
                {
                    CodeCatchClause catchSt = (CodeCatchClause)enumerator1.Current;
                    base.Output.Write("CATCH ");
                    this.OutputIdentifier(catchSt.LocalName);
                    base.Output.Write(" AS ");
                    this.OutputType(catchSt.CatchExceptionType);
                    base.Output.WriteLine();

                    this.Indent++;
                    this.GenerateStatements(catchSt.Statements);
                    this.Indent--;
                }
            }
            CodeStatementCollection finallySt = e.FinallyStatements;
            if (finallySt.Count > 0)
            {
                base.Output.WriteLine("FINALLY");
                this.Indent++;
                this.GenerateStatements(finallySt);
                this.Indent--;
            }
            base.Output.WriteLine("END TRY");
            base.Output.WriteLine();
        }

        protected override void GenerateTypeConstructor(CodeTypeConstructor e)
        {
            if (this.IsCurrentClass || this.IsCurrentStruct)
            {
                if (e.CustomAttributes.Count > 0)
                {
                    this.GenerateAttributes(e.CustomAttributes);
                }
                base.Output.WriteLine("STATIC CONSTRUCTOR()");
                this.Indent++;
                this.GenerateStatements(e.Statements);
                this.Indent--;
                base.Output.WriteLine();
            }
        }

        protected override void GenerateTypeEnd(CodeTypeDeclaration e)
        {
            if (!this.IsCurrentDelegate)
            {
                this.Indent--;
                base.Output.WriteLine();
                base.Output.Write("END ");
                if (e.IsClass)
                {
                    base.Output.Write("CLASS");
                }
                else if (e.IsStruct)
                {
                    base.Output.Write("STRUCTURE");
                }
                else if (e.IsInterface)
                {
                    base.Output.Write("INTERFACE");
                }
                else if (e.IsEnum)
                {
                    base.Output.Write("ENUM");
                }
                base.Output.WriteLine();
                this.Options.BlankLinesBetweenMembers = false;
            }
        }

        protected override void GenerateTypeStart(CodeTypeDeclaration e)
        {
            this.Options.BlankLinesBetweenMembers = true;
            writeCodeBefore(e.UserData);
            if (e.CustomAttributes.Count > 0)
            {
                this.GenerateAttributes(e.CustomAttributes);
            }
            if (!this.IsCurrentDelegate)
            {
                this.OutputTypeAttributes(e);
                if (e.IsStruct)
                {
                    if (e.IsPartial)
                    {
                        base.Output.Write("PARTIAL ");
                    }
                    base.Output.Write("STRUCTURE ");
                }
                else if (e.IsEnum)
                {
                    base.Output.Write("ENUM ");
                }
                else if (e.IsClass)
                {
                    if ((e.TypeAttributes & TypeAttributes.Sealed) == TypeAttributes.Sealed)
                    {
                        base.Output.Write("SEALED ");
                    }
                    if ((e.TypeAttributes & TypeAttributes.Abstract) == TypeAttributes.Abstract)
                    {
                        base.Output.Write("ABSTRACT ");
                    }
                    if (e.IsPartial)
                    {
                        base.Output.Write("PARTIAL ");
                    }
                    base.Output.Write("CLASS ");
                }
                else
                {
                    base.Output.Write("INTERFACE ");
                }
                this.OutputIdentifier(e.Name);
                this.OutputGenericParameters(e.TypeParameters);
                if (e.IsEnum)
                {
                    if (e.BaseTypes.Count > 0)
                    {
                        base.Output.Write(" AS ");
                        this.OutputType(e.BaseTypes[0]);
                    }
                }
                else
                {
                    int count = 0;
                    //
                    foreach (CodeTypeReference reference in e.BaseTypes)
                    {
                        if (e.IsStruct)
                        {
                            if (count == 0)
                            {
                                base.Output.Write(" IMPLEMENTS ");
                            }
                            else
                            {
                                base.Output.Write(", ");
                            }
                        }
                        else if (e.IsInterface)
                        {
                            if (count == 0)
                            {
                                base.Output.WriteLine(" ;");
                                this.Indent++;
                                base.Output.Write("INHERIT ");
                                this.Indent--;
                            }
                            else
                            {
                                base.Output.Write(", ");
                            }
                        }
                        else
                        {
                            // Currently we suppose that the First type will always be a class to INHERIT
                            // but sometimes we may only have interfaces to IMPLEMENTs
                            if (count == 0)
                            {
                                base.Output.WriteLine(" ;");
                                this.Indent++;
                                base.Output.Write("INHERIT ");
                                this.Indent--;
                            }
                            else if (count == 1)
                            {
                                base.Output.WriteLine(" ;");
                                this.Indent++;
                                base.Output.Write("IMPLEMENTS ");
                                this.Indent--;
                            }
                            else
                            {
                                base.Output.Write(", ");
                            }
                        }
                        this.OutputType(reference);
                        count++;
                    }
                }
                base.Output.WriteLine();
                this.Indent++;
            }
            else
            {
                CodeTypeDelegate delegate1 = (CodeTypeDelegate)e;
                base.Output.Write("DELEGATE ");
                this.OutputIdentifier(e.Name);
                base.Output.Write("(");
                this.OutputParameters(delegate1.Parameters);
                base.Output.Write(") AS ");
                this.OutputType(delegate1.ReturnType);
                base.Output.WriteLine();
            }
        }

        protected override void GenerateVariableDeclarationStatement(CodeVariableDeclarationStatement e)
        {
            base.Output.Write("LOCAL ");
            this.OutputIdentifier(e.Name);
            if (e.InitExpression != null)
            {
                base.Output.Write(" := ");
                this.GenerateExpression(e.InitExpression);
            }
            base.Output.Write(" AS ");
            this.OutputType(e.Type);
            if (!this.generatingForLoop)
            {
                base.Output.WriteLine();
            }
        }

        protected override void GenerateVariableReferenceExpression(CodeVariableReferenceExpression e)
        {
            this.OutputIdentifier(e.VariableName);
        }

        protected override void GenerateParameterDeclarationExpression(CodeParameterDeclarationExpression e)
        {
            if (e.CustomAttributes.Count > 0)
            {
                this.GenerateAttributes(e.CustomAttributes, null, true);
                base.Output.Write(" ");
            }
            //
            this.OutputIdentifier(e.Name);
            switch (e.Direction)
            {
                case FieldDirection.In:
                    base.Output.Write(" AS ");
                    break;
                case FieldDirection.Out:
                    base.Output.Write(" OUT ");
                    break;
                case FieldDirection.Ref:
                    base.Output.Write(" REF ");
                    break;
            }
            this.OutputType(e.Type);
        }

        protected override string GetTypeOutput(CodeTypeReference typeRef)
        {
            string str = string.Empty;
            CodeTypeReference arrayElementType = typeRef;
            if (typeRef.UserData.Contains(XSharpCodeConstants.USERDATA_CODE))
            {
                // some types with parsing problems have their definition saved as userdata
                return typeRef.UserData[XSharpCodeConstants.USERDATA_CODE] as string;
            }

            while (arrayElementType.ArrayElementType != null)
            {
                arrayElementType = arrayElementType.ArrayElementType;
            }
            str = str + this.GetBaseTypeOutput(arrayElementType);
            while ((typeRef != null) && (typeRef.ArrayRank > 0))
            {
                char[] chArray = new char[typeRef.ArrayRank + 1];
                chArray[0] = '[';
                chArray[typeRef.ArrayRank] = ']';
                for (int i = 1; i < typeRef.ArrayRank; i++)
                {
                    chArray[i] = ',';
                }
                str = str + new string(chArray);
                typeRef = typeRef.ArrayElementType;
            }
            return str;
        }

        private string GetBaseTypeOutput(CodeTypeReference typeRef)
        {
            string baseType = typeRef.BaseType;

            if (baseType.Length == 0)
            {
                return "VOID";
            }
            switch (baseType.ToLower(CultureInfo.InvariantCulture))
            {
                case "system.int16":
                    return "SHORT";

                case "system.uint16":
                    return "WORD";

                case "system.int32":
                    return "INT";

                case "system.uint32":
                    return "DWORD";

                case "system.int64":
                    return "INT64";

                case "system.uint64":
                    return "UINT64";

                case "system.string":
                    return "STRING";

                case "system.char":
                    return "CHAR";

                case "system.object":
                    return "OBJECT";

                case "system.boolean":
                    return "LOGIC";

                case "system.void":
                    return "VOID";

                case "system.byte":
                    return "BYTE";

                case "system.sbyte":
                    return "System.SByte";

                case "system.single":
                    return "REAL4";

                case "system.double":
                    return "REAL8";

                case "system.decimal":
                    return "System.Decimal";
            }
            //
            StringBuilder sb = new StringBuilder(baseType.Length + 10);
            string str3 = typeRef.BaseType;
            int startIndex = 0;
            int start = 0;

            for (int i = 0; i < str3.Length; i++)
            {
                switch (str3[i])
                {
                    case '+':
                    case '.':
                        sb.Append(str3.Substring(startIndex, i - startIndex));
                        sb.Append('.');
                        i++;
                        startIndex = i;
                        break;

                    case '`':
                        {
                            sb.Append(str3.Substring(startIndex, i - startIndex));
                            i++;
                            int length = 0;
                            while (((i < str3.Length) && (str3[i] >= '0')) && (str3[i] <= '9'))
                            {
                                length = (length * 10) + (str3[i] - '0');
                                i++;
                            }
                            // Generic Type like System.Int`ValueType : so, ignore  ValueType
                            if (length > 0)
                            {
                                sb.Append(this.BuildGenericArguments(typeRef.TypeArguments, start, length));
                                start += length;
                                if ((i < str3.Length) && ((str3[i] == '+') || (str3[i] == '.')))
                                {
                                    sb.Append('.');
                                    i++;
                                }
                                startIndex = i;
                            }
                            else
                                startIndex = str3.Length;
                            break;
                        }
                }
            }
            if (startIndex < str3.Length)
            {
                sb.Append(str3.Substring(startIndex));
            }
            return sb.ToString();
        }

        protected override bool IsValidIdentifier(string value)
        {
            if ((value == null) || (value.Length == 0))
            {
                return false;
            }
            if (value[0] != '@')
            {
                if (XSharpKeywords.Contains(value))
                {
                    return false;
                }
            }
            else
            {
                value = value.Substring(1);
                if (!XSharpKeywords.Contains(value))
                {
                    return false;
                }
            }
            return CodeGenerator.IsValidLanguageIndependentIdentifier(value);
        }

        protected override void OutputType(CodeTypeReference typeRef)
        {
            this.Output.Write(this.GetTypeOutput(typeRef));
        }

        protected override void OutputOperator(CodeBinaryOperatorType op)
        {
            switch (op)
            {
                case CodeBinaryOperatorType.Add:
                    this.Output.Write("+");
                    return;

                case CodeBinaryOperatorType.Subtract:
                    this.Output.Write("-");
                    return;

                case CodeBinaryOperatorType.Multiply:
                    this.Output.Write("*");
                    return;

                case CodeBinaryOperatorType.Divide:
                    this.Output.Write("/");
                    return;

                case CodeBinaryOperatorType.Modulus:
                    this.Output.Write("%");
                    return;

                case CodeBinaryOperatorType.Assign:
                    this.Output.Write(":=");
                    return;

                case CodeBinaryOperatorType.IdentityInequality:
                    this.Output.Write("!=");
                    return;

                case CodeBinaryOperatorType.IdentityEquality:
                    this.Output.Write("==");
                    return;

                case CodeBinaryOperatorType.ValueEquality:
                    this.Output.Write("==");
                    return;

                case CodeBinaryOperatorType.BitwiseAnd:
                    this.Output.Write("&");
                    return;

                case CodeBinaryOperatorType.BitwiseOr:
                    this.Output.Write("|");
                    return;

                case CodeBinaryOperatorType.BooleanAnd:
                    this.Output.Write("&&");
                    return;

                case CodeBinaryOperatorType.BooleanOr:
                    this.Output.Write("||");
                    return;

                case CodeBinaryOperatorType.LessThan:
                    this.Output.Write("<");
                    return;

                case CodeBinaryOperatorType.LessThanOrEqual:
                    this.Output.Write("<=");
                    return;

                case CodeBinaryOperatorType.GreaterThan:
                    this.Output.Write(">");
                    return;

                case CodeBinaryOperatorType.GreaterThanOrEqual:
                    this.Output.Write(">=");
                    return;
            }
        }


        protected override string QuoteSnippetString(string value)
        {
            if (value.Length == 0)
            {
                return "\"\"";
            }
            StringBuilder sb = new StringBuilder(value.Length + 10);
            bool extended = false;
            sb.Append("\"");
            foreach (Char ch in value)
            {
                switch (ch)
                {
                    case (Char)0:
                        sb.Append(@"\0");
                        extended = true;
                        break;

                    case '\a':
                        sb.Append(@"\a");
                        extended = true;
                        break;

                    case '\b':
                        sb.Append(@"\b");
                        extended = true;
                        break;

                    case '\f':
                        sb.Append(@"\f");
                        extended = true;
                        break;

                    case '\n':
                        sb.Append(@"\n");
                        extended = true;
                        break;

                    case '\r':
                        sb.Append(@"\r");
                        extended = true;
                        break;

                    case '\t':
                        sb.Append(@"\t");
                        extended = true;
                        break;

                    case '\v':
                        sb.Append(@"\v");
                        extended = true;
                        break;

                    case '\\':
                        sb.Append(@"\\");
                        extended = true;
                        break;

                    case '\"':
                        sb.Append("\\\"");
                        extended = true;
                        break;

                    default:
                        if (ch < 32)
                        {
                            // Hexa code
                            sb.Append(String.Format("\\x{0:x4}", (int)ch));
                            extended = true;
                        }
                        else
                        {
                            sb.Append(ch);
                        }

                        break;
                }
            }

            sb.Append('"');
            string result;
            result = sb.ToString();
            if (extended)
            {
                result = "e" + result;
            }
            return result;
        }

        protected override bool Supports(GeneratorSupport support)
        {
            switch (support)
            {
                case GeneratorSupport.ArraysOfArrays:
                case GeneratorSupport.AssemblyAttributes:
                case GeneratorSupport.ChainedConstructorArguments:
                case GeneratorSupport.ComplexExpressions:
                case GeneratorSupport.DeclareDelegates:
                case GeneratorSupport.DeclareEnums:
                case GeneratorSupport.DeclareEvents:
                case GeneratorSupport.DeclareIndexerProperties:
                case GeneratorSupport.DeclareInterfaces:
                case GeneratorSupport.DeclareValueTypes:
                case GeneratorSupport.EntryPointMethod:
                case GeneratorSupport.GenericTypeDeclaration:
                case GeneratorSupport.GenericTypeReference:
                //case GeneratorSupport.GotoStatements;
                case GeneratorSupport.MultidimensionalArrays:
                case GeneratorSupport.MultipleInterfaceMembers:
                case GeneratorSupport.NestedTypes:
                case GeneratorSupport.ParameterAttributes:
                case GeneratorSupport.PartialTypes:
                case GeneratorSupport.PublicStaticMembers:
                case GeneratorSupport.ReferenceParameters:
                case GeneratorSupport.Resources:
                case GeneratorSupport.ReturnTypeAttributes:
                case GeneratorSupport.StaticConstructors:
                case GeneratorSupport.TryCatchStatements:
                case GeneratorSupport.Win32Resources:
                    return true;
                default:
                    break;
            }
            return false;
        }


        // in XSharp, continuing on the next line is done using a semi-colon !
        protected override void ContinueOnNewLine(string st)
        {
            this.Output.Write(st);
            this.Output.WriteLine( " ;" );
        }

        private void GenerateAttributes(CodeAttributeDeclarationCollection attributes)
        {
            this.GenerateAttributes(attributes, null, false);
        }

        private void GenerateAttributes(CodeAttributeDeclarationCollection attributes, string prefix)
        {
            this.GenerateAttributes(attributes, prefix, false);
        }

        private void GenerateAttributes(CodeAttributeDeclarationCollection attributes, string prefix, bool inLine)
        {
            if (attributes.Count != 0)
            {
                IEnumerator enumerator = attributes.GetEnumerator();
                while (enumerator.MoveNext())
                {
                    CodeAttributeDeclaration current = (CodeAttributeDeclaration)enumerator.Current;
                    this.GenerateAttributeDeclarationsStart(attributes);
                    if (prefix != null)
                    {

                        base.Output.Write(prefix + ": ");
                    }
                    if (current.AttributeType != null)
                    {
                        base.Output.Write(this.GetTypeOutput(current.AttributeType));
                    }
                    base.Output.Write("(");
                    bool flag = true;
                    foreach (CodeAttributeArgument argument in current.Arguments)
                    {
                        if (flag)
                        {
                            flag = false;
                        }
                        else
                        {
                            base.Output.Write(", ");
                        }
                        this.OutputAttributeArgument(argument);
                    }
                    base.Output.Write(")");
                    this.GenerateAttributeDeclarationsEnd(attributes);
                    if (inLine)
                    {
                        base.Output.Write(" ");
                    }
                    else
                    {
                        base.Output.WriteLine();
                    }
                }
            }
        }
        protected override void OutputAttributeArgument(CodeAttributeArgument arg)
        {
            // Base class outputs "=" in stead of ":=" for named arguments
            string name = null;
            if ((arg.Name != null) && (arg.Name.Length > 0))
            {
                name = arg.Name;
                this.OutputIdentifier(arg.Name);
                this.Output.Write(":=");
                arg.Name = null;
            }
            base.OutputAttributeArgument(arg);
            //restore name
            arg.Name = name;
        }


        private void OutputGenericParameters(CodeTypeParameterCollection typeParameters)
        {
            if (typeParameters.Count != 0)
            {
                base.Output.Write("<");
                bool flag = true;
                for (int i = 0; i < typeParameters.Count; i++)
                {
                    if (flag)
                    {
                        flag = false;
                    }
                    else
                    {
                        base.Output.Write(", ");
                    }
                    if (typeParameters[i].CustomAttributes.Count > 0)
                    {
                        this.GenerateAttributes(typeParameters[i].CustomAttributes, null, true);
                        base.Output.Write(" ");
                    }
                    base.Output.Write(typeParameters[i].Name);
                }
                base.Output.Write(">");
            }
        }

        private void OutputGenericArguments(CodeTypeReferenceCollection typeArguments)
        {
            if (typeArguments.Count != 0)
            {
                String gen = this.BuildGenericArguments(typeArguments, 0, typeArguments.Count);
                base.Output.Write(gen);
            }
        }

        private String BuildGenericArguments(CodeTypeReferenceCollection typeArguments, int start, int len)
        {
            String ret = "";
            StringBuilder sb = new StringBuilder(0x80);
            //
            if (typeArguments.Count != 0)
            {
                sb.Append("<");
                bool flag = true;
                for (int i = start; i < start + len; i++)
                {
                    if (flag)
                    {
                        flag = false;
                    }
                    else
                    {
                        sb.Append(", ");
                    }
                    if (i < typeArguments.Count)
                    {
                        sb.Append(this.GetTypeOutput(typeArguments[i]));
                    }
                }
                sb.Append(">");
                ret = sb.ToString();
            }
            return ret;
        }

        private void OutputTypeAttributes(CodeTypeDeclaration e)
        {
            if ((e.Attributes & MemberAttributes.New) != ((MemberAttributes)0))
            {
                //this.Output.Write("NEW ");
            }
            TypeAttributes typeAttributes = e.TypeAttributes;
            switch ((typeAttributes & TypeAttributes.VisibilityMask))
            {
                case TypeAttributes.AutoLayout:
                case TypeAttributes.NestedAssembly:
                case TypeAttributes.NestedFamANDAssem:
                    base.Output.Write("INTERNAL ");
                    break;

                case TypeAttributes.Public:
                case TypeAttributes.NestedPublic:
                    base.Output.Write("PUBLIC ");
                    break;

                case TypeAttributes.NestedPrivate:
                    base.Output.Write("PRIVATE ");
                    break;

                case TypeAttributes.NestedFamily:
                    base.Output.Write("PROTECTED ");
                    break;

                case TypeAttributes.NestedFamORAssem:
                    base.Output.Write("PROTECTED INTERNAL ");
                    break;
            }
        }

        protected override void OutputIdentifier(string ident)
        {
            base.OutputIdentifier(CreateEscapedIdentifier(ident));
        }
        protected override void OutputMemberAccessModifier(MemberAttributes attributes)
        {
            switch ((attributes & MemberAttributes.AccessMask))
            {
                case MemberAttributes.Public:
                    this.Output.Write("PUBLIC ");
                    break;
                case MemberAttributes.Private:
                    this.Output.Write("PRIVATE ");
                    break;
                case MemberAttributes.Family:
                    this.Output.Write("PROTECTED ");
                    break;
                case MemberAttributes.FamilyOrAssembly:
                    this.Output.Write("PROTECTED INTERNAL ");
                    break;
                case MemberAttributes.Assembly:
                    this.Output.Write("INTERNAL ");
                    break;
            }
        }
        protected override void OutputMemberScopeModifier(MemberAttributes attributes)
        {
            MemberAttributes attributes2 = attributes & MemberAttributes.VTableMask;
            if (attributes2 == MemberAttributes.New)
            {
                this.Output.Write("NEW ");
            }
            switch ((attributes & MemberAttributes.ScopeMask))
            {
                case MemberAttributes.Abstract:
                    this.Output.Write("ABSTRACT ");
                    return;

                case MemberAttributes.Final:
                    this.Output.Write("");
                    return;

                case MemberAttributes.Static:
                    this.Output.Write("STATIC ");
                    return;

                case MemberAttributes.Override:
                    this.Output.Write("VIRTUAL ");
                    return;
            }
            switch ((attributes & MemberAttributes.AccessMask))
            {
                case MemberAttributes.Family:
                case MemberAttributes.Public:
                    this.Output.Write("VIRTUAL ");
                    break;
            }
        }


    }
}
