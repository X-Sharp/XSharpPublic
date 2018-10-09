//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
//#define WRITE2LOGFILE
using Microsoft.VisualStudio.Shell.Design.Serialization;
using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.Collections;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Security.Permissions;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.CodeDom
{
    [ComVisibleAttribute(true)]
    [PermissionSetAttribute(SecurityAction.LinkDemand, Name = "FullTrust")]
    [PermissionSetAttribute(SecurityAction.InheritanceDemand, Name = "FullTrust")]
    public partial class XSharpCodeDomProvider : CodeDomProvider
    {
        protected XSharpCodeGenerator xsGenerator;

        // The Tab setting is shared by all instance of our CodeDomProvider
        protected IProjectTypeHelper _projectNode;
        public static int TabSize { get; set; }
        public string FileName { get; set; }

        public XSharpCodeDomProvider()
        {
            this.xsGenerator = new XSharpCodeGenerator();
            XSharpCodeDomProvider.TabSize = 1;
        }

        public override string FileExtension
        {
            get
            {
                return ".prg";
            }
        }

        [Obsolete]
        public override ICodeCompiler CreateCompiler()
        {
            return this.xsGenerator;
        }

        [Obsolete]
        public override ICodeGenerator CreateGenerator()
        {
            return this.xsGenerator;
        }

        [Obsolete]
        public override ICodeParser CreateParser()
        {
            var parser = new XSharpCodeParser(_projectNode);
            parser.TabSize = XSharpCodeDomProvider.TabSize;
            parser.FileName = this.FileName;
            return parser;
        }


        public override void GenerateCodeFromCompileUnit(CodeCompileUnit compileUnit, TextWriter writer, CodeGeneratorOptions options)
        {

            {
                //
                base.GenerateCodeFromCompileUnit(compileUnit, writer, options);
                //
#if WRITE2LOGFILE
                string path = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments);
                path = Path.Combine(path, "XSharpDumpCodeCompileUnit_generate.log");
                XSharpCodeDomHelper1.DumpCodeCompileUnit(compileUnit, path, true);
#endif
            }
        }


        // Called by the WinForm designer at load time
        public override CodeCompileUnit Parse(TextReader codeStream)
        {
            CodeCompileUnit compileUnit = null;
            compileUnit = base.Parse(codeStream);
            //
#if WRITE2LOGFILE
            string path = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments);
            path = Path.Combine(path, "XSharpDumpCodeCompileUnit_parse.log");
            XSharpCodeDomHelper1.DumpCodeCompileUnit(compileUnit, path, true);
#endif
            return compileUnit;
        }

    }
    public class XSharpCodeDomHelper1
    {
        static String Delimiter = new String('-', 5);
        static void dumpUserData(IDictionary userData)
        {
            if (userData.Count > 0)
            {
                writeLine(Delimiter);
                writeLine("UserData");
                foreach (DictionaryEntry value in userData)
                {
                    writeLine(value.Key.ToString() + "  = " + value.Value.ToString());
                }
                writeLine(Delimiter);
            }

        }
        static public void DumpCodeCompileUnit(CodeCompileUnit ccu, string FileName, bool append)
        {
            //
            writer = new StreamWriter(FileName, append, System.Text.Encoding.UTF8);
            try
            {

                String Line = new String('=', 25);
                _indent = 0;
                //
                writeLine(Line);
                writeLine(DateTime.Now.ToString());
                writeLine(Delimiter);

                writeLine("CodeCompileUnit:");
                //
                dumpUserData(ccu.UserData);
                CodeTypeDeclaration ctd = findFirstClass(ccu);
                if (ctd == null)
                    return;
                writeLine(Line);
                writeLine("CodeTypeDeclaration : " +ctd.Name);
                //
                dumpUserData(ctd.UserData);
                //
                foreach (CodeTypeMember member in ctd.Members)
                {
                    writeLine(Delimiter);
                    writeLine("CodeTypeMember : " + member.Name);
                    writeLine(Delimiter);
                    writeLine("CodeTypeMember UserData :");
                    //
                    dumpUserData(member.UserData);
                    if (member is CodeMemberField)
                    {
                        CodeMemberField cmf = (CodeMemberField)member;
                        writeLine(" -=> CodeMemberField : "+ cmf.Type.BaseType);
                        dumpUserData(cmf.UserData);
                    }
                    else if (member is CodeMemberMethod)
                    {
                        CodeMemberMethod cmm = (CodeMemberMethod)member;
                        writeLine(" -=> CodeMemberMethod : " +cmm.Name);
                        dumpUserData(cmm.UserData);
                        foreach (CodeStatement stmt in cmm.Statements)
                        {
                            writeLine(stmt.GetType().ToString());
                            DumpStatement(writer, stmt);
                        }
                        writeLine(Delimiter);
                    }
                }
            }
            finally
            {
                writer.Close();
            }
        }
        #region Dump Tools
        static int _indent = 0;
        static StreamWriter writer;

        static String indent
        {
            get
            {
                return new string(' ', _indent * 3);
            }
        }

        static void writeLineIndent(string str)
        {
            writer.WriteLine(indent + str);
        }

        static void writeIndent(string str)
        {
            writer.Write(indent + str);
        }

        static void writeLine(string str)
        {
            writer.WriteLine(str);
        }


        private static string getExpression(CodeExpression expr)
        {
            if (expr is CodeFieldReferenceExpression)
            {
                return ((CodeFieldReferenceExpression)expr).FieldName;
            }
            if (expr is CodePropertyReferenceExpression)
            {
                return ((CodePropertyReferenceExpression)expr).PropertyName;
            }
            if (expr is CodeMethodReferenceExpression)
            {
                return ((CodeMethodReferenceExpression)expr).MethodName;
            }
            if (expr is CodePrimitiveExpression)
            {
                return ((CodePrimitiveExpression)expr).Value.ToString();
            }
            return expr.ToString();
        }

        private static void DumpStatement(StreamWriter writer, CodeStatement s)
        {
            if (s.UserData.Count > 0)
            {
                dumpUserData(s.UserData);
            }
            /*
                  System.CodeDom.CodeAssignStatement
                  System.CodeDom.CodeAttachEventStatement
                  System.CodeDom.CodeCommentStatement
                  System.CodeDom.CodeConditionStatement
                  System.CodeDom.CodeExpressionStatement
                  System.CodeDom.CodeGotoStatement
                  System.CodeDom.CodeIterationStatement
                  System.CodeDom.CodeLabeledStatement
                  System.CodeDom.CodeMethodReturnStatement
                  System.CodeDom.CodeRemoveEventStatement
                  System.CodeDom.CodeSnippetStatement
                  System.CodeDom.CodeThrowExceptionStatement
                  System.CodeDom.CodeTryCatchFinallyStatement
                  System.CodeDom.CodeVariableDeclarationStatement
            */
            _indent++;
            if (s is CodeAssignStatement)
            {
                var stmt = (CodeAssignStatement)s;
                if (stmt.Left != null)
                    dumpExpression(stmt.Left);
                writeLineIndent(getExpression(stmt.Right));
                if (stmt.Right != null)
                    dumpExpression(stmt.Right);
            }
            else if (s is CodeExpressionStatement)
            {
                var stmt = (CodeExpressionStatement)s;
                writeLineIndent(stmt.Expression.GetType().ToString());
                dumpExpression(stmt.Expression);
            }
            else if (s is CodeVariableDeclarationStatement)
            {
                var stmt = (CodeVariableDeclarationStatement)s;
                writeLineIndent(stmt.Name);
                var tr = stmt.Type;
                writeLineIndent(tr.BaseType);
            }
            else if (s is CodeAttachEventStatement)
            {
                var stmt = (CodeAttachEventStatement)s;
                dumpExpression(stmt.Event);
                dumpExpression(stmt.Listener);
            }
            else if (s is CodeMethodReturnStatement)
            {
                var stmt = (CodeMethodReturnStatement)s;
                dumpExpression(stmt.Expression);
            }
            else
            {
                writeLineIndent(s.GetType().ToString());
            }
            _indent--;
        }

        private static void dumpExpressionList(CodeExpressionCollection coll)
        {
            _indent++;
            writeLineIndent("Parameter count: " + coll.Count.ToString());
            foreach (CodeExpression exp in coll)
            {
                dumpExpression(exp);
            }
            _indent--;
        }

        private static void dumpExpression(CodeExpression e)
        {
            /*
              System.CodeDom.CodeArgumentReferenceExpression
                System.CodeDom.CodeArrayCreateExpression
                System.CodeDom.CodeArrayIndexerExpression
                System.CodeDom.CodeBaseReferenceExpression
                System.CodeDom.CodeBinaryOperatorExpression
                System.CodeDom.CodeCastExpression
                System.CodeDom.CodeDefaultValueExpression
                System.CodeDom.CodeDelegateCreateExpression
                System.CodeDom.CodeDelegateInvokeExpression
                System.CodeDom.CodeDirectionExpression
                System.CodeDom.CodeEventReferenceExpression
                System.CodeDom.CodeFieldReferenceExpression
                System.CodeDom.CodeIndexerExpression
                System.CodeDom.CodeMethodInvokeExpression
                System.CodeDom.CodeMethodReferenceExpression
                System.CodeDom.CodeObjectCreateExpression
                System.CodeDom.CodeParameterDeclarationExpression
                System.CodeDom.CodePrimitiveExpression
                System.CodeDom.CodePropertyReferenceExpression
                System.CodeDom.CodePropertySetValueReferenceExpression
                System.CodeDom.CodeSnippetExpression
                System.CodeDom.CodeThisReferenceExpression
                System.CodeDom.CodeTypeOfExpression
                System.CodeDom.CodeTypeReferenceExpression
                System.CodeDom.CodeVariableReferenceExpression
            */
            if (e == null)
                return;
            if (e is CodeFieldReferenceExpression)
            {
                CodeFieldReferenceExpression exp = (CodeFieldReferenceExpression)e;
                dumpExpression(exp.TargetObject);
                writeLineIndent("F: "+exp.FieldName);
            }
            else if (e is CodeObjectCreateExpression)
            {
                var exp = (CodeObjectCreateExpression)e;
                var tr = exp.CreateType as CodeTypeReference;
                writeLineIndent(tr.BaseType+"{}");
                dumpExpressionList(exp.Parameters);
            }
            else if (e is CodeMethodInvokeExpression)
            {
                var exp = (CodeMethodInvokeExpression)e;
                dumpExpression(exp.Method.TargetObject);
                writeLineIndent("M: " + exp.Method.MethodName);
                dumpExpressionList(exp.Parameters);
            }
            else if (e is CodePropertyReferenceExpression)
            {
                var exp = (CodePropertyReferenceExpression)e;
                dumpExpression(exp.TargetObject);
                writeLineIndent("P: " + exp.PropertyName);
            }
            else if (e is CodePrimitiveExpression)
            {
                var exp = (CodePrimitiveExpression)e;
                writeLineIndent(exp.ToString());
                writeLineIndent(exp.Value.ToString());
            }
            else if (e is CodeCastExpression)
            {
                var exp = (CodeCastExpression)e;
                writeLineIndent(exp.TargetType.ToString());
                dumpExpression(exp.Expression);
            }
            else if (e is CodeThisReferenceExpression)
            {
                writeLineIndent("SELF");
            }
            else if (e is CodeBinaryOperatorExpression)
            {
                var exp = (CodeBinaryOperatorExpression)e;
                dumpExpression(exp.Left);
                CodeBinaryOperatorType opType = exp.Operator;
                writeLineIndent(opType.ToString());
                dumpExpression(exp.Right);
            }
            else if (e is CodeTypeReferenceExpression)
            {
                var exp = (CodeTypeReferenceExpression)e;
                var tr = exp.Type;
                writeLineIndent(tr.BaseType);
            }
            else if (e is CodeBaseReferenceExpression)
            {
                writeLineIndent("SUPER");
            }
            else
            {
                writeLineIndent(e.GetType().FullName);
            }
       }

        /// <summary>
        /// Reading the CodeCompileUnit, enumerate all NameSpaces, enumerate All Types, searching for the first Class declaration
        /// </summary>
        /// <param name="ccu"></param>
        /// <returns></returns>
        private static XCodeTypeDeclaration findFirstClass(CodeCompileUnit ccu)
        {
            XCodeNamespace namespaceName;
            return findFirstClass(ccu, out namespaceName);
        }

        private static XCodeTypeDeclaration findFirstClass(CodeCompileUnit ccu, out XCodeNamespace namespaceName)
        {
            namespaceName = null;
            XCodeTypeDeclaration rstClass = null;
            if (ccu != null)
            {
                foreach (XCodeNamespace namespace2 in ccu.Namespaces)
                {
                    foreach (XCodeTypeDeclaration declaration in namespace2.Types)
                    {
                        //  The first Type == The first Class declaration
                        if (declaration.IsClass)
                        {
                            namespaceName = namespace2;
                            rstClass = declaration;
                            break;
                        }
                    }
                }
            }
            return rstClass;
        }

        #endregion
    }
}
