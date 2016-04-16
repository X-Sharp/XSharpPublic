using System;
using System.CodeDom;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.CodeDom
{
    public class XSharpCodeDomHelper
    {
        static int indent = 0;
        static StreamWriter writer;

        static String Indent
        {
            get
            {
                return new string(' ', indent * 3);
            }
        }

        static void WriteLineIndent(string str)
        {
            writer.WriteLine(Indent + str);
        }

        static void WriteIndent(string str)
        {
            writer.Write(Indent + str);
        }

        static void WriteLine(string str)
        {
            writer.WriteLine(str);
        }

        static void Write(string str)
        {
            writer.Write(str);
        }

        static public void DumpCodeCompileUnit(CodeCompileUnit ccu, string FileName, bool append)
        {
            //
            writer = new StreamWriter(FileName, append, System.Text.Encoding.Default);
            String Delimiter = new String('-', 5);
            String Line = new String('=', 25);
            indent = 0;
            //
            WriteLine(Line);
            WriteLine(DateTime.Now.ToString());
            WriteLine(Delimiter);

            WriteLine("CodeCompileUnit UserData :");
            //
            foreach (DictionaryEntry value in ccu.UserData)
            {
                WriteLine(value.Key.ToString() + "  = " + value.Value.ToString());
            }
            CodeTypeDeclaration ctd = FindFirstClass(ccu);
            WriteLine(Line);
            Write("CodeTypeDeclaration : ");
            WriteLine(ctd.Name);
            //
            WriteLine(Delimiter);
            WriteLine("UserData");
            foreach (DictionaryEntry value in ctd.UserData)
            {
                WriteLine(value.Key.ToString() + "  = " + value.Value.ToString());
            }
            //
            foreach (CodeTypeMember member in ctd.Members)
            {
                WriteLine(Delimiter);
                Write("CodeTypeMember : ");
                WriteLine(member.Name);
                WriteLine(Delimiter);
                WriteLine("CodeTypeMember UserData :");
                //
                foreach (DictionaryEntry value in member.UserData)
                {
                    WriteLine(value.Key.ToString() + "  = " + value.Value.ToString());
                }
                WriteLine(Delimiter);
                if (member is CodeMemberField)
                {
                    CodeMemberField cmf = (CodeMemberField)member;
                    Write(" -=> CodeMemberField : ");
                    WriteLine(cmf.Type.BaseType);
                    WriteLine(Delimiter);
                }
                else if (member is CodeMemberMethod)
                {
                    CodeMemberMethod cmm = (CodeMemberMethod)member;
                    Write(" -=> CodeMemberMethod : ");
                    WriteLine(cmm.Name);
                    foreach (CodeStatement stmt in cmm.Statements)
                    {
                        WriteLine(stmt.GetType().ToString());
                        DumpStatement(writer, stmt);
                    }
                    WriteLine(Delimiter);
                }
            }
            writer.Close();
        }

        private static void DumpStatement(StreamWriter writer, CodeStatement s)
        {
            indent++;
            if (s is CodeAssignStatement)
            {
                CodeAssignStatement stmt = (CodeAssignStatement)s;
                WriteLineIndent(stmt.Left.GetType().ToString());
                DumpExpression(stmt.Left);
                WriteLineIndent(stmt.Right.GetType().ToString());
                DumpExpression(stmt.Right);
            }
            else if (s is CodeExpressionStatement)
            {
                CodeExpressionStatement stmt = (CodeExpressionStatement)s;
                WriteLineIndent(stmt.Expression.GetType().ToString());
                DumpExpression(stmt.Expression);
            }
            indent--;
        }

        private static void DumpExpression(CodeExpression e)
        {
            indent++;
            if (e is CodeFieldReferenceExpression)
            {
                CodeFieldReferenceExpression exp = (CodeFieldReferenceExpression)e;
                WriteLineIndent(exp.TargetObject?.ToString());
            }
            else if (e is CodeObjectCreateExpression)
            {
                CodeObjectCreateExpression exp = (CodeObjectCreateExpression)e;
                WriteLineIndent(exp.CreateType.ToString());
            }
            else if (e is CodeMethodInvokeExpression)
            {
                CodeMethodInvokeExpression exp = (CodeMethodInvokeExpression)e;
                WriteLineIndent(exp.Method.TargetObject?.ToString());
                WriteLineIndent(exp.Method.MethodName);
            }
            else if (e is CodePropertyReferenceExpression)
            {
                CodePropertyReferenceExpression exp = (CodePropertyReferenceExpression)e;
                WriteLineIndent(exp.TargetObject?.ToString());
                WriteLineIndent(exp.PropertyName);
            }
            else if (e is CodePrimitiveExpression)
            {
                CodePrimitiveExpression exp = (CodePrimitiveExpression)e;
                WriteLineIndent(exp.ToString());
                WriteLineIndent(exp.Value.ToString());
            }
            indent--;
        }

        private static CodeTypeDeclaration FindFirstClass(CodeCompileUnit ccu)
        {
            CodeTypeDeclaration rstClass = null;
            if (ccu != null)
            {
                foreach (CodeNamespace namespace2 in ccu.Namespaces)
                {
                    foreach (CodeTypeDeclaration declaration in namespace2.Types)
                    {
                        //  The first Type == The first Class declaration
                        if (declaration.IsClass)
                        {
                            rstClass = declaration;
                            break;
                        }
                    }
                }
            }
            return rstClass;
        }
    }
}
