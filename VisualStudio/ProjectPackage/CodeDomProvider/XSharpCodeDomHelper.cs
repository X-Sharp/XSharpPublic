//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.CodeDom;
using System.Collections;
using System.IO;

namespace XSharp.CodeDom
{
    public class XSharpCodeDomHelper
    {
        /// <summary>
        /// Merge both CodeCompileUnit. The main type (class) will come from formCompileUnit
        /// </summary>
        /// <param name="compileUnit"></param>
        /// <param name="designerCompileUnit"></param>
        /// <returns></returns>
        internal static XMergedCodeCompileUnit MergeCodeCompileUnit( XCodeCompileUnit formCompileUnit, XCodeCompileUnit designerCompileUnit)
        {
            // Create the merged CodeCompileUnit
            var mergedCodeCompileUnit = new XMergedCodeCompileUnit(formCompileUnit);

            CodeTypeDeclaration designerClass = FindDesignerClass(designerCompileUnit, out var designerNamespace);
            if (designerClass != null)
            {
                // Do the same with the form
                HasPartialClass(formCompileUnit, out var formNameSpace, out var formClass);
                // and merge only if ...
                if (string.Compare(designerClass.Name, formClass.Name, true) == 0)
                {
                    // Ignore the namespace of the designer.prg. C# does that too

                    // So, the "main" class is...
                    XCodeTypeDeclaration mergedType = new XCodeTypeDeclaration(formClass.Name);
                    // And does inherit from
                    mergedType.BaseTypes.AddRange(formClass.BaseTypes);
                    mergedType.IsPartial = true;
                    mergedType.TypeAttributes = formClass.TypeAttributes;
                    // Now, read members from each side, and put a stamp on each
                    foreach (CodeTypeMember member in designerClass.Members)
                    {
                        member.SetFromDesigner(true);
                        mergedType.Members.Add(member);
                    }
                    foreach (CodeTypeMember member in formClass.Members)
                    {
                        member.SetFromDesigner(false);
                        mergedType.Members.Add(member);
                    }
                    // A class is always in a NameSpace
                    XCodeNamespace mergedNamespace = new XCodeNamespace(formNameSpace.Name);
                    mergedNamespace.Types.Add(mergedType);
                    // Now, add it to the CompileUnit
                    mergedCodeCompileUnit.Namespaces.Clear();
                    mergedCodeCompileUnit.Namespaces.Add(mergedNamespace);
                    mergedCodeCompileUnit.FormNamespace = formNameSpace;
                    mergedCodeCompileUnit.DesignerNamespace = designerNamespace;
                    //
                }
                else
                {
                    // Something went wrong, return the designer CodeCompileUnit
                    mergedCodeCompileUnit = new XMergedCodeCompileUnit(formCompileUnit);
                }
            }
            mergedCodeCompileUnit.FormUnit = formCompileUnit;
            mergedCodeCompileUnit.FileName = formCompileUnit.FileName;
            mergedCodeCompileUnit.DesignerUnit = designerCompileUnit;
            return mergedCodeCompileUnit;

        }

        internal static CodeTypeDeclaration FindDesignerClass(CodeCompileUnit ccu, CodeTypeDeclaration masterClass)
        {
            return FindDesignerClass(ccu, out _, masterClass.Name);
        }
        /// <summary>
        /// Reading the CodeCompileUnit, enumerate all NameSpaces, enumerate All Types, searching for the first Class that contains an InitializeComponent member
        /// </summary>
        /// <param name="ccu"></param>
        /// <param name="namespaceName"></param>
        /// <returns></returns>
        internal static CodeTypeDeclaration FindDesignerClass(CodeCompileUnit ccu)
        {
            return FindDesignerClass(ccu, out _);
        }

        internal static CodeTypeDeclaration FindDesignerClass(CodeCompileUnit ccu, out CodeNamespace namespaceName, string name = "")
        {
            namespaceName = null;
            // We search the first Class that has a Candidate for InitializeComponent
            foreach (CodeNamespace nameSpace in ccu.Namespaces)
            {
                foreach (CodeTypeDeclaration typeElement in nameSpace.Types)
                {
                    if (typeElement.IsClass)
                    {
                        // Find class by name
                        if (! string.IsNullOrEmpty(name) && string.Compare(typeElement.Name, name, true) == 0)
                        {
                            return typeElement;
                        }
                        // Find class by expected member InitializeComponent
                        // Looking for InitializeComponent, returning a void, and with no Parameters
                        foreach (CodeTypeMember member in typeElement.Members)
                        {
                            CodeMemberMethod method = member as CodeMemberMethod;
                            if ((method != null) &&
                                (method.Name == "InitializeComponent") &&
                                (method.ReturnType.BaseType == "System.Void") &&
                                (method.ReturnType.TypeArguments.Count == 0) &&
                                (method.Parameters.Count == 0))
                            {
                                // This one seems to be ok
                                // Return where it is
                                namespaceName = nameSpace;
                                // and what it is
                                return typeElement;
                            }
                        }
                    }
                }
            }
            // No way
            return null;
        }

        /// <summary>
        /// Reading the CodeCompileUnit, enumerate all NameSpaces, enumerate All Types, searching for the first Partial Class.
        /// </summary>
        /// <param name="ccu"></param>
        /// <param name="contextNameSpace">The NameSpace in wich the partial Class is defined</param>
        /// <param name="contextClass">The found partial Class</param>
        /// <returns>True if a partial Class has been found</returns>
        public static bool HasPartialClass(CodeCompileUnit ccu, out CodeNamespace contextNameSpace, out CodeTypeDeclaration contextClass)
        {
            bool retValue = false;
            contextNameSpace = null;
            contextClass = null;
            // in all NameSpace, search for Types
            foreach (CodeNamespace nameSpace in ccu.Namespaces)
            {
                // Check if the type is a class
                foreach (CodeTypeDeclaration typeElement in nameSpace.Types)
                {
                    // Ok, so could it be a partial class
                    if (typeElement.IsClass)
                    {
                        //
                        retValue = typeElement.IsPartial;
                        if (retValue)
                        {
                            contextNameSpace = nameSpace;
                            contextClass = typeElement;
                            break;
                        }
                    }
                }
            }
            return retValue;
        }
        /// <summary>
        /// Return the FileName with .Designer inserted
        /// </summary>
        /// <param name="prgFile"></param>
        /// <returns></returns>
        public static string BuildDesignerFileName(string prgFile)
        {
            // Retrieve path information from the FulPath
            String prgPath = Path.GetDirectoryName(prgFile);
            if (prgFile.EndsWith(".xaml.prg", StringComparison.OrdinalIgnoreCase))
                return "";
            string extension = Path.GetExtension(prgFile).ToLower();
            String prg = Path.GetFileNameWithoutExtension(prgFile);
            // Does the FileName ends with .Designer ?
            if (!prg.EndsWith(".Designer"))
                prg += ".Designer";
            // Add the original file extension
            String ext = Path.GetExtension(prgFile);
            //
            return Path.Combine(prgPath, prg) + ext;
        }



        #region Dump Tools
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
            writer = new StreamWriter(FileName, append, System.Text.Encoding.UTF8);
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
            CodeTypeDeclaration ctd = ccu.GetFirstClass();
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
            if (s.HasSourceCode())
            {
                WriteLineIndent("Original code: " + s.GetSourceCode());
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


        #endregion
    }
}
