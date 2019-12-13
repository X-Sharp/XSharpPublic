//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis.XSharp;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using System;
using System.CodeDom;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using XSharpModel;

namespace XSharp.CodeDom
{
    public class XSharpCodeDomHelper
    {

        /// <summary>
        /// Merge both CodeCompileUnit. The main type (class) will come from designerCompileUnit
        /// </summary>
        /// <param name="compileUnit"></param>
        /// <param name="designerCompileUnit"></param>
        /// <returns></returns>
        internal static CodeCompileUnit MergeCodeCompileUnit(CodeCompileUnit compileUnit, CodeCompileUnit designerCompileUnit)
        {
            return MergeCodeCompileUnit(null, compileUnit, designerCompileUnit);
        }

        internal static CodeCompileUnit MergeCodeCompileUnit(CodeCompileUnit mergedCodeCompileUnit, CodeCompileUnit compileUnit, CodeCompileUnit designerCompileUnit)
        {
            // Create the merged CodeCompileUnit
            if ( mergedCodeCompileUnit == null )
                mergedCodeCompileUnit = new CodeCompileUnit();
            //
            CodeNamespace designerNamespace;
            CodeTypeDeclaration designerClass = FindDesignerClass(designerCompileUnit, out designerNamespace);
            if (designerClass != null)
            {
                // Do the same with the form
                CodeNamespace nameSpace;
                CodeTypeDeclaration className;
                XSharpCodeDomHelper.HasPartialClass(compileUnit, out nameSpace, out className);
                // and merge only if ...
                if ((String.Compare(designerNamespace.Name, nameSpace.Name, true) == 0) &&
                    (String.Compare(designerClass.Name, className.Name, true) == 0))
                {
                    // Ok, same Namespace & same Class : Merge !

                    // So, the "main" class is...
                    CodeTypeDeclaration mergedType = new CodeTypeDeclaration(className.Name);
                    // And does inherit from
                    mergedType.BaseTypes.AddRange(className.BaseTypes);
                    mergedType.TypeAttributes = className.TypeAttributes;
                    // Now, read members from each side, and put a stamp on each
                    foreach (CodeTypeMember member in designerClass.Members)
                    {
                        member.UserData[XSharpCodeConstants.USERDATA_FROMDESIGNER] = true;
                        mergedType.Members.Add(member);
                    }
                    foreach (CodeTypeMember member in className.Members)
                    {
                        member.UserData[XSharpCodeConstants.USERDATA_FROMDESIGNER] = false;
                        mergedType.Members.Add(member);
                    }
                    // A class is always in a NameSpace
                    CodeNamespace mergedNamespace = new CodeNamespace(nameSpace.Name);
                    mergedNamespace.Types.Add(mergedType);
                    // Now, add it to the CompileUnit
                    mergedCodeCompileUnit.Namespaces.Clear();
                    mergedCodeCompileUnit.Namespaces.Add(mergedNamespace);
                    //
                }
                else
                {
                    // Something went wrong, return the designer CodeCompileUnit
                    mergedCodeCompileUnit = designerCompileUnit;
                }
            }
            else
            {
                // Sorry, no designer class
                mergedCodeCompileUnit = designerCompileUnit;
            }
            return mergedCodeCompileUnit;
        }

        /// <summary>
        /// Reading the CodeCompileUnit, enumerate all NameSpaces, enumerate All Types, searching for the first Class declaration
        /// </summary>
        /// <param name="ccu"></param>
        /// <returns></returns>
        public static CodeTypeDeclaration FindFirstClass(CodeCompileUnit ccu)
        {
            CodeNamespace namespaceName;
            return FindFirstClass(ccu, out namespaceName);
        }

        public static CodeTypeDeclaration FindFirstClass(CodeCompileUnit ccu, out CodeNamespace namespaceName)
        {
            namespaceName = null;
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
                            namespaceName = namespace2;
                            rstClass = declaration;
                            break;
                        }
                    }
                }
            }
            return rstClass;
        }

        /// <summary>
        /// Reading the CodeCompileUnit, enumerate all NameSpaces, enumerate All Types, searching for the first Class that contains an InitializeComponent member
        /// </summary>
        /// <param name="ccu"></param>
        /// <param name="namespaceName"></param>
        /// <returns></returns>
        internal static CodeTypeDeclaration FindDesignerClass(CodeCompileUnit ccu)
        {
            CodeNamespace namespaceName;
            return FindDesignerClass(ccu, out namespaceName);
        }

        internal static CodeTypeDeclaration FindDesignerClass(CodeCompileUnit ccu, out CodeNamespace namespaceName)
        {
            namespaceName = null;
            // We search the first Class that has a Candidate for InitializeComponent
            foreach (CodeNamespace nameSpace in ccu.Namespaces)
            {
                foreach (CodeTypeDeclaration typeElement in nameSpace.Types)
                {
                    if (typeElement.IsClass)
                    {
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
            if (s.UserData.Contains(XSharpCodeConstants.USERDATA_CODE))
            {
                WriteLineIndent("Original code: " + s.UserData[XSharpCodeConstants.USERDATA_CODE].ToString());
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
