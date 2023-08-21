//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
//#define WRITE2LOGFILE
using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Collections;
using System.IO;
using System.Runtime.InteropServices;
using System.Security.Permissions;

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
        public string FileName { get; set; }

        public XSharpCodeDomProvider()
        {
            this.xsGenerator = new XSharpCodeGenerator();
        }

        public override string FileExtension => ".prg";

        [Obsolete]
        public override ICodeCompiler CreateCompiler() => this.xsGenerator;

        [Obsolete]
        public override ICodeGenerator CreateGenerator() => this.xsGenerator;

        [Obsolete]
        public override ICodeParser CreateParser()
        {
            var parser = new XSharpCodeParser(_projectNode)
            {
                FileName = this.FileName
            };
            return parser;
        }

        public override void GenerateCodeFromCompileUnit(CodeCompileUnit compileUnit, TextWriter writer, CodeGeneratorOptions options)
        {
            //
            // validate to see if something has changed in the compileUnit
            //
            if (options == null)
                options = new CodeGeneratorOptions();
            options.BlankLinesBetweenMembers = false;
            // VerbatimOrder writes the members in the order in which they appear in the collection
            options.VerbatimOrder = true;
            base.GenerateCodeFromCompileUnit(compileUnit, writer, options);
            //
#if WRITE2LOGFILE
                string path = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments);
                path = Path.Combine(path, "XSharpDumpCodeCompileUnit_generate.log");
                XSharpCodeDomHelper1.DumpCodeCompileUnit(compileUnit, path, true);
#endif
        }


		XCodeCompileUnit ToXCodeCompileUnit(CodeCompileUnit unit)
		{
		if (unit is XCodeCompileUnit xccu)
                return xccu;
            return new XCodeCompileUnit(unit);
		}

        // Called by the WinForm designer at load time
        public override CodeCompileUnit Parse(TextReader codeStream)
        {
            var compileUnit = ToXCodeCompileUnit(base.Parse(codeStream));
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
        #region Dump Tools
#if WRITE2LOGFILE
        static readonly String Delimiter = new String('-', 5);
        static void DumpUserData(IDictionary userData)
        {
            if (userData.Count > 0)
            {
                WriteLine(Delimiter);
                WriteLine("UserData");
                foreach (DictionaryEntry value in userData)
                {
                    WriteLine(value.Key.ToString() + "  = " + value.Value.ToString());
                }
                WriteLine(Delimiter);
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
                WriteLine(Line);
                WriteLine(DateTime.Now.ToString());
                WriteLine(Delimiter);

                WriteLine("CodeCompileUnit:");
                //
                DumpUserData(ccu.UserData);
                CodeTypeDeclaration ctd = FindFirstClass(ccu);
                if (ctd == null)
                    return;
                WriteLine(Line);
                WriteLine("CodeTypeDeclaration : " +ctd.Name);
                //
                DumpUserData(ctd.UserData);
                //
                foreach (CodeTypeMember member in ctd.Members)
                {
                    WriteLine(Delimiter);
                    WriteLine("CodeTypeMember : " + member.Name);
                    WriteLine(Delimiter);
                    WriteLine("CodeTypeMember UserData :");
                    //
                    if (member is CodeMemberField cmf)
                    {
                        WriteLine(" -=> CodeMemberField : " + cmf.Type.BaseType);
                        DumpUserData(cmf.UserData);
                    }
                    else if (member is CodeMemberMethod cmm)
                    {
                        WriteLine(" -=> CodeMemberMethod : " + cmm.Name);
                        DumpUserData(cmm.UserData);
                        foreach (CodeStatement stmt in cmm.Statements)
                        {
                            WriteLine(stmt.GetType().ToString());
                            DumpStatement(writer, stmt);
                        }
                        WriteLine(Delimiter);
                    }
                    else
                    {
                        DumpUserData(member.UserData);

                    }
                }
            }
            finally
            {
                writer.Close();
            }
        }
        static int _indent = 0;
        static StreamWriter writer;

        static String Indent
        {
            get
            {
                return new string(' ', _indent * 3);
            }
        }

        static void WriteLineIndent(string str)
        {
            writer.WriteLine(Indent + str);
        }


        static void WriteLine(string str)
        {
            writer.WriteLine(str);
        }


        private static string GetExpression(CodeExpression expr)
        {
            if (expr is CodeFieldReferenceExpression expression)
            {
                return expression.FieldName;
            }
            if (expr is CodePropertyReferenceExpression expression1)
            {
                return expression1.PropertyName;
            }
            if (expr is CodeMethodReferenceExpression cmre)
            {
                return cmre.MethodName;
            }
            if (expr is CodePrimitiveExpression cpe)
            {
                return cpe.Value.ToString();
            }
            return expr.ToString();
        }

        private static void DumpStatement(StreamWriter writer, CodeStatement s)
        {
            if (writer is null)
            {
                throw new ArgumentNullException(nameof(writer));
            }

            if (s.UserData.Count > 0)
            {
                DumpUserData(s.UserData);
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
            if (s is CodeAssignStatement cas)
            {
                if (cas.Left != null)
                    DumpExpression(cas.Left);
                WriteLineIndent(GetExpression(cas.Right));
                if (cas.Right != null)
                    DumpExpression(cas.Right);
            }
            else if (s is CodeExpressionStatement ces)
            {
                WriteLineIndent(ces.Expression.GetType().ToString());
                DumpExpression(ces.Expression);
            }
            else if (s is CodeVariableDeclarationStatement cvds)
            {
                WriteLineIndent(cvds.Name);
                var tr = cvds.Type;
                WriteLineIndent(tr.BaseType);
            }
            else if (s is CodeAttachEventStatement caes)
            {
                DumpExpression(caes.Event);
                DumpExpression(caes.Listener);
            }
            else if (s is CodeMethodReturnStatement cmrs)
            {
                DumpExpression(cmrs.Expression);
            }
            else
            {
                WriteLineIndent(s.GetType().ToString());
            }
            _indent--;
        }

        private static void DumpExpressionList(CodeExpressionCollection coll)
        {
            _indent++;
            WriteLineIndent("Parameter count: " + coll.Count.ToString());
            foreach (CodeExpression exp in coll)
            {
                DumpExpression(exp);
            }
            _indent--;
        }

        private static void DumpExpression(CodeExpression e)
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
            if (e is CodeFieldReferenceExpression cfre)
            {
                DumpExpression(cfre.TargetObject);
                WriteLineIndent("F: "+ cfre.FieldName);
            }
            else if (e is CodeObjectCreateExpression coce)
            {
                var tr = coce.CreateType as CodeTypeReference;
                WriteLineIndent(tr.BaseType+"{}");
                DumpExpressionList(coce.Parameters);
            }
            else if (e is CodeMethodInvokeExpression cmie)
            {
                DumpExpression(cmie.Method.TargetObject);
                WriteLineIndent("M: " + cmie.Method.MethodName);
                DumpExpressionList(cmie.Parameters);
            }
            else if (e is CodePropertyReferenceExpression cpre)
            {
                DumpExpression(cpre.TargetObject);
                WriteLineIndent("P: " + cpre.PropertyName);
            }
            else if (e is CodePrimitiveExpression cpe)
            {
                WriteLineIndent(cpe.ToString());
                WriteLineIndent(cpe.Value.ToString());
            }
            else if (e is CodeCastExpression cce)
            {
                WriteLineIndent(cce.TargetType.ToString());
                DumpExpression(cce.Expression);
            }
            else if (e is CodeThisReferenceExpression)
            {
                WriteLineIndent("SELF");
            }
            else if (e is CodeBinaryOperatorExpression cboe)
            {
                DumpExpression(cboe.Left);
                CodeBinaryOperatorType opType = cboe.Operator;
                WriteLineIndent(opType.ToString());
                DumpExpression(cboe.Right);
            }
            else if (e is CodeTypeReferenceExpression ctre)
            {
                var tr = ctre.Type;
                WriteLineIndent(tr.BaseType);
            }
            else if (e is CodeBaseReferenceExpression)
            {
                WriteLineIndent("SUPER");
            }
            else
            {
                WriteLineIndent(e.GetType().FullName);
            }
       }

        /// <summary>
        /// Reading the CodeCompileUnit, enumerate all NameSpaces, enumerate All Types, searching for the first Class declaration
        /// </summary>
        /// <param name="ccu"></param>
        /// <returns></returns>
        private static XCodeTypeDeclaration FindFirstClass(CodeCompileUnit ccu)
        {
            return FindFirstClass(ccu, out _);
        }

        private static XCodeTypeDeclaration FindFirstClass(CodeCompileUnit ccu, out XCodeNamespace namespaceName)
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
#endif
        #endregion
    }
}
