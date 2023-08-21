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
using System.Reflection;
using System.Text;
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
        #region static members
        internal static IDictionary<string, string> SystemToXSharp;

        internal static IDictionary<string, string> XSharpToSystem;
        static XSharpCodeGenerator()
        {
            SystemToXSharp = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
            XSharpToSystem = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
            SystemToXSharp.Add("System.Boolean", "LOGIC");
            SystemToXSharp.Add("System.Byte", "BYTE");
            SystemToXSharp.Add("System.Char", "CHAR");
            SystemToXSharp.Add("System.Double", "REAL8");
            SystemToXSharp.Add("System.Int16", "SHORT");
            SystemToXSharp.Add("System.Int32", "INT");
            SystemToXSharp.Add("System.Int64", "INT64");
            SystemToXSharp.Add("System.Object", "OBJECT");
            SystemToXSharp.Add("System.Single", "REAL4");
            SystemToXSharp.Add("System.String", "STRING");
            SystemToXSharp.Add("System.UInt16", "WORD");
            SystemToXSharp.Add("System.UInt32", "DWORD");
            SystemToXSharp.Add("System.UInt64", "UINT64");
            SystemToXSharp.Add("System.Void", "VOID");
            SystemToXSharp.Add("XSharp._CodeBlock", "CODEBLOCK");
            SystemToXSharp.Add("XSharp.__Array", "ARRAY");
            SystemToXSharp.Add("XSharp.__Binary", "BINARY");
            SystemToXSharp.Add("XSharp.__Currency", "CURRENCY");
            SystemToXSharp.Add("XSharp.__Date", "DATE");
            SystemToXSharp.Add("XSharp.__Float", "FLOAT");
            SystemToXSharp.Add("XSharp.__Psz", "PSZ");
            SystemToXSharp.Add("XSharp.__Symbol", "SYMBOL");
            SystemToXSharp.Add("XSharp.__Usual", "USUAL");
            SystemToXSharp.Add("XSharp.__WinBool", "LOGIC");
            SystemToXSharp.Add("XSharp.__WinDate", "DATE");

            foreach (KeyValuePair<string, string> pair in SystemToXSharp)
            {
                if (!XSharpToSystem.ContainsKey(pair.Value))
                {
                    XSharpToSystem.Add(pair.Value, pair.Key);
                }
            }
            XSharpToSystem.Add("LONG", "System.Int32");
            XSharpToSystem.Add("LONGINT", "System.Int32");
            XSharpToSystem.Add("SHORTINT", "System.Int16");

            return;
        }
        #endregion
        private bool generatingForLoop;
        private readonly string _selector;
        private readonly string _staticSelector;
        private readonly List<string> _using;
        private int _indentSave = 0;
        private int _keywordCase;
        private int _privateKeyword;
        private int _publicKeyword;
        private bool _useTabs;
        private int _tabSize;
        private int _indentSize;
        private List<CodeObject> _globalmembers;
        #region Keywords
        private string keywordBEGIN;
        private string keywordEND;
        private string keywordDELEGATE;
        private string keywordCLASS;
        private string keywordINHERIT;
        private string keywordIMPLEMENTS;
        private string keywordSTRUCTURE;
        private string keywordINTERFACE;
        private string keywordENUM;
        private string keywordNAMESPACE;
        private string keywordPUBLIC;
        private string keywordEXPORT;
        private string keywordPRIVATE;
        private string keywordHIDDEN;
        private string keywordPROTECTED;
        private string keywordINTERNAL;
        private string keywordABSTRACT;
        private string keywordVIRTUAL;
        private string keywordSTATIC;
        private string keywordCONST;
        private string keywordAS;
        private string keywordOUT;
        private string keywordREF;
        private string keywordNULL;
        private string keywordSELF;
        private string keywordSUPER;
        private string keywordSTRICT;
        private string keywordMETHOD;
        private string keywordEVENT;
        private string keywordPROPERTY;
        private string keywordCONSTRUCTOR;
        private string keywordFUNCTION;
        private string keywordGET;
        private string keywordSET;
        private string keywordPARTIAL;
        private string keywordSEALED;
        private string keywordLOCAL;
        private string keywordTRY;
        private string keywordCATCH;
        private string keywordFINALLY;
        private string keywordDO;
        private string keywordWHILE;
        private string keywordENDDO;
        private string keywordRETURN;
        private string keywordUSING;
        private string keywordTHROW;
        private string keywordIF;
        private string keywordELSE;
        private string keywordENDIF;
        #endregion
        private bool _mustEscape = true;
        private string _ws = " ";
        private string _indentString = " ";
        private string _tabString = " ";
        private string _lastTrivia = null;
        private CodeEntryPointMethod _entryPoint = null;
        private CodeTypeDeclaration _entryPointType = null;
        private Stack<CodeTypeDeclaration> _types;
        private bool _hasSource;
        private bool Nested => _types.Count > 1;
        private bool SuppressCodeGen => Nested || _hasSource;
        private XSharpIndentedTextWriter _textWriter = null;
        public XSharpCodeGenerator() : base()
        {
            _selector = ":";
            _staticSelector = ".";
            _using = new List<string>();
            ReadSettings();
            _types = new Stack<CodeTypeDeclaration>();
        }

        private void OverrideTextWriter()
        {
            FieldInfo field = typeof(CodeGenerator).GetField("output", BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.FlattenHierarchy);
            var oldWriter = (IndentedTextWriter)field.GetValue(this);
            if (!(oldWriter is XSharpIndentedTextWriter))
            {
                var writer = oldWriter.InnerWriter;
                _textWriter = new XSharpIndentedTextWriter(writer, _tabString);
                try
                {
                    field.SetValue(this, _textWriter);
                }
                catch
                {

                }
            }
        }
        private void ReadSettings()
        {
            _keywordCase = (int)XConstants.GetSetting(XConstants.RegistryKeywordCase, 1);
            _privateKeyword = (int)XConstants.GetSetting(XConstants.RegistryPrivateKeyword, 0); // 0 = private, 1 = hidden
            _publicKeyword = (int)XConstants.GetSetting(XConstants.RegistryPublicKeyword, 0);   // 0 = public, 1 = export, 2 = none
            _useTabs = (int)XConstants.GetSetting(XConstants.RegistryUseTabs, 1) == 1;
            _tabSize = (int)XConstants.GetSetting(XConstants.RegistryTabSize, 3);
            _indentSize = (int)XConstants.GetSetting(XConstants.RegistryIndentSize, 3);
            _indentString = new string(' ', _indentSize);
            _tabString = new string(' ', _tabSize);
            _ws = _useTabs ? "\t" : " ";
            keywordBEGIN = FormatKeyword("BEGIN ");
            keywordEND = FormatKeyword("END ");
            keywordDELEGATE = FormatKeyword("DELEGATE ");
            keywordCLASS = FormatKeyword("CLASS ");
            keywordINHERIT = FormatKeyword("INHERIT ");
            keywordIMPLEMENTS = FormatKeyword("IMPLEMENTS ");
            keywordSTRUCTURE = FormatKeyword("STRUCTURE ");
            keywordINTERFACE = FormatKeyword("INTERFACE ");
            keywordENUM = FormatKeyword("ENUM ");
            keywordNAMESPACE = FormatKeyword("NAMESPACE ");
            keywordPUBLIC = FormatKeyword("PUBLIC ");
            keywordEXPORT = FormatKeyword("EXPORT ");
            keywordPRIVATE = FormatKeyword("PRIVATE ");
            keywordHIDDEN = FormatKeyword("HIDDEN ");
            keywordPROTECTED = FormatKeyword("PROTECTED ");
            keywordINTERNAL = FormatKeyword("INTERNAL ");
            keywordABSTRACT = FormatKeyword("ABSTRACT ");
            keywordVIRTUAL = FormatKeyword("VIRTUAL ");
            keywordSTATIC = FormatKeyword("STATIC ");
            keywordCONST = FormatKeyword("CONST ");
            keywordPARTIAL = FormatKeyword("PARTIAL ");
            keywordSEALED = FormatKeyword("SEALED ");
            keywordAS = FormatKeyword("AS ");
            keywordOUT = FormatKeyword("OUT ");
            keywordREF = FormatKeyword("REF ");
            keywordNULL = FormatKeyword("NULL");
            keywordSELF = FormatKeyword("SELF");
            keywordSUPER = FormatKeyword("SUPER");
            keywordSTRICT = " " + FormatKeyword("STRICT");
            keywordLOCAL = FormatKeyword("LOCAL ");
            keywordCONSTRUCTOR = FormatKeyword("CONSTRUCTOR");
            keywordMETHOD = FormatKeyword("METHOD ");
            keywordEVENT = FormatKeyword("EVENT ");
            keywordPROPERTY = FormatKeyword("PROPERTY ");
            keywordFUNCTION = FormatKeyword("FUNCTION ");
            keywordGET = FormatKeyword("GET");
            keywordSET = FormatKeyword("SET");
            keywordTRY = FormatKeyword("TRY");
            keywordCATCH = FormatKeyword("CATCH ");
            keywordFINALLY = FormatKeyword("FINALLY");
            keywordDO = FormatKeyword("DO ");
            keywordWHILE = FormatKeyword("WHILE ");
            keywordENDDO = FormatKeyword("ENDDO ");
            keywordRETURN = FormatKeyword("RETURN");
            keywordUSING = FormatKeyword("USING ");
            keywordTHROW = FormatKeyword("THROW ");
            keywordIF = FormatKeyword("IF ");
            keywordELSE = FormatKeyword("ELSE");
            keywordENDIF = FormatKeyword("ENDIF");
        }

        private string FormatKeyword(string keyword)
        {
            switch (_keywordCase)
            {
                case 0:
                    return keyword;
                case 1:
                    return keyword.ToUpper();
                case 2:
                    return keyword.ToLower();
            }
            if (keyword.Length > 1)
                return keyword.Substring(0, 1).ToUpper() + keyword.Substring(1).ToLower();
            return keyword.ToUpper();
        }

        protected override string NullToken
        {
            get
            {
                return keywordNULL;
            }
        }
        private void WriteAssignment()
        {
            this.Output.Write(_ws + ":=" + _ws);
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
                if (e.SizeExpression == null && e.Size == 0)
                {
                    // the syntax is something like <int>{}
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
                    this.Output.Write(">{");
                    this.Output.Write("}");

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
            this.WriteAssignment();
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
            this.Output.Write(_ws + "+=" + _ws);
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
            base.Output.Write(keywordSUPER);
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
            this.Output.Write(_ws);
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
            base.Output.Write(keywordIF);
            base.GenerateExpression(e.Condition);
            base.Output.WriteLine();
            this.Indent++;
            base.GenerateStatements(e.TrueStatements);
            this.Indent--;
            if (e.FalseStatements.Count > 0)
            {
                base.Output.WriteLine();
                base.Output.Write(keywordELSE);
                base.Output.WriteLine();
                this.Indent++;
                base.GenerateStatements(e.FalseStatements);
                this.Indent--;
            }
            base.Output.WriteLine(keywordENDIF);
        }

        private void writeMemberAccessModifier(CodeObject o, MemberAttributes newAttributes, bool scope, bool field = false)
        {
            if (o.HasModifiers())
            {
                var attrs = o.GetMemberAttributes();
                if (attrs == newAttributes)
                {
                    base.Output.Write(o.GetModifiers());
                    return;
                }
            }
            this.OutputMemberAccessModifier(newAttributes);
            if (scope)
            {
                this.OutputMemberScopeModifier(newAttributes);
            }
            if (field)
            {
                this.OutputFieldScopeModifier(newAttributes);
            }
        }

        protected override void GenerateConstructor(CodeConstructor e, CodeTypeDeclaration c)
        {
            if (SuppressCodeGen)
                return;
            if (e.HasSourceCode() && writeOriginalCode(e))
            {
                _textWriter.SuppressNewLine = true;
                return;
            }
            if (base.IsCurrentClass || base.IsCurrentStruct)
            {
                // Do we have some Source Code pushed here by our Parser ??
                // when so then that source code also includes the constructor line
                writeTrivia(e);
                if (!writeOriginalCode(e))
                {
                    if (e.CustomAttributes.Count > 0)
                    {
                        this.GenerateAttributes(e.CustomAttributes);
                    }
                    writeMemberAccessModifier(e, e.Attributes, false);
                    base.Output.Write(keywordCONSTRUCTOR + "(");
                    this.OutputParameters(e.Parameters);
                    base.Output.WriteLine(")" + keywordSTRICT);
                    CodeExpressionCollection baseConstructorArgs = e.BaseConstructorArgs;
                    CodeExpressionCollection chainedConstructorArgs = e.ChainedConstructorArgs;
                    if (baseConstructorArgs.Count > 0)
                    {
                        this.Indent++;
                        base.Output.Write(keywordSUPER + "(");
                        this.OutputExpressionList(baseConstructorArgs);
                        base.Output.WriteLine(")");
                        this.Indent--;
                    }
                    else if (chainedConstructorArgs.Count > 0)
                    {
                        this.Indent++;
                        base.Output.Write(keywordSELF + "(");
                        this.OutputExpressionList(chainedConstructorArgs);
                        base.Output.WriteLine(")");
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
            // save the parameters and delay the code generation until the end of the 
            // compile unit, where ImplementEntryPointMethod() is called
            _entryPoint = e;
            _entryPointType = c;
            return;
        }
        private void ImplementEntryPointMethod(CodeEntryPointMethod e, CodeTypeDeclaration c)
        {
            if (e == null || c == null)
                return;
            this.GenerateCommentStatements(e.Comments);

            if (e.CustomAttributes.Count > 0)
            {
                this.GenerateAttributes(e.CustomAttributes);
            }
            base.Output.Write(keywordFUNCTION + " Start() " + keywordAS);
            this.OutputType(e.ReturnType);
            base.Output.WriteLine();
            this.Indent++;
            this.GenerateStatements(e.Statements);
            this.Indent--;
            base.Output.WriteLine();
        }

        protected override void GenerateEvent(CodeMemberEvent e, CodeTypeDeclaration c)
        {
            if (SuppressCodeGen)
                return;
            if (e.HasSourceCode() && writeOriginalCode(e))
            {
                _textWriter.SuppressNewLine = true;
                return;
            }
            if (!this.IsCurrentDelegate && !this.IsCurrentEnum)
            {
                writeTrivia(e);
                if (e.CustomAttributes.Count > 0)
                {
                    this.GenerateAttributes(e.CustomAttributes);
                }
                if (e.PrivateImplementationType == null)
                {
                    writeMemberAccessModifier(e, e.Attributes, false);
                }
                else
                {
                    base.Output.Write(keywordVIRTUAL);
                }
                base.Output.Write("EVENT ");
                string fqdn = e.Name;

                if (e.PrivateImplementationType != null)
                {
                    fqdn = e.PrivateImplementationType.BaseType + this._selector + fqdn;
                }
                this.OutputIdentifier(fqdn);
                base.Output.Write(" " + keywordAS);
                this.OutputType(e.Type);
                base.Output.WriteLine();
            }
        }

        protected override void GenerateEventReferenceExpression(CodeEventReferenceExpression e)
        {
            var oldescape = _mustEscape;
            if (e.TargetObject != null)
            {
                base.GenerateExpression(e.TargetObject);
                base.Output.Write(this._selector);
                _mustEscape = false;
            }
            this.OutputIdentifier(e.EventName);
            _mustEscape = oldescape;
        }

        protected override void GenerateExpressionStatement(CodeExpressionStatement e)
        {
            base.GenerateExpression(e.Expression);
            if (!this.generatingForLoop)
            {
                base.Output.WriteLine();
            }
        }
        private void writeEnumMember(CodeMemberField e)
        {
            bool fromDesigner = true;
            if (e.HasFromDesigner())
            {
                fromDesigner = e.GetFromDesigner();
            }
            writeTrivia(e);
            if (e.CustomAttributes.Count > 0)
            {
                this.GenerateAttributes(e.CustomAttributes);
            }
            this.OutputIdentifier(e.Name);
            if (e.InitExpression != null)
            {

                bool hasCode = e.InitExpression.HasSourceCode();
                if (fromDesigner || !hasCode)
                {
                    this.WriteAssignment();
                    this.GenerateExpression(e.InitExpression);
                }
                else
                {
                    writeOriginalCode(e.InitExpression, true);
                }

            }
            base.Output.WriteLine();
        }
        protected void writeNormalField(CodeMemberField e)
        {
            bool fromDesigner = true;
            if (e.HasFromDesigner())
            {
                fromDesigner = e.GetFromDesigner();
            }
            writeTrivia(e);
            if (e.CustomAttributes.Count > 0)
            {
                this.GenerateAttributes(e.CustomAttributes);
            }

            writeMemberAccessModifier(e, e.Attributes, false, true);

            if (e.HasSourceCode() && !fromDesigner)
            {
                // we do not use writeOriginalCode because
                // the modifier is not part of the source code in the case
                // of a field and that would add another CRLF after the modifier
                var sourceCode = e.GetSourceCode();
                this.Output.Write(sourceCode);
            }
            else
            {
                this.OutputIdentifier(e.Name);

                if (e.InitExpression != null)
                {
                    bool hasCode = e.InitExpression.HasSourceCode();
                    this.WriteAssignment();
                    if (fromDesigner || !hasCode)
                    {
                        this.GenerateExpression(e.InitExpression);
                    }
                    else
                    {
                        writeOriginalCode(e.InitExpression, true);
                    }
                }
                base.Output.Write(" " + keywordAS);
                this.OutputType(e.Type);
            }
            base.Output.WriteLine();

        }

        protected override void GenerateField(CodeMemberField e)
        {
            if (SuppressCodeGen)
                return;

            if (!this.IsCurrentDelegate && !this.IsCurrentInterface)
            {
                if (this.IsCurrentEnum)
                {
                    writeEnumMember(e);
                }
                else
                {
                    writeNormalField(e);
                }
            }
        }
        protected override void OutputFieldScopeModifier(MemberAttributes attributes)
        {
            switch ((attributes & MemberAttributes.ScopeMask))
            {
                case MemberAttributes.Static:
                    this.Output.Write(keywordSTATIC);
                    return;
                case MemberAttributes.Const:
                    this.Output.Write(keywordCONST);
                    return;
            }
        }

        protected override void GenerateFieldReferenceExpression(CodeFieldReferenceExpression e)
        {
            var oldescape = _mustEscape;
            if (e.TargetObject != null)
            {
                this.GenerateExpression(e.TargetObject);
                // If we have a Type, we must use a dot as a selector
                if (e.TargetObject.GetType() == typeof(CodeTypeReferenceExpression))
                {
                    base.Output.Write(this._staticSelector);
                }
                else
                {
                    base.Output.Write(this._selector);
                }
                _mustEscape = false;
            }

            this.OutputIdentifier(e.FieldName);
            _mustEscape = oldescape;
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
            generatingForLoop = true;
            this.GenerateStatement(e.InitStatement);
            base.Output.WriteLine();
            base.Output.Write(keywordDO + keywordWHILE);
            this.GenerateExpression(e.TestExpression);
            base.Output.WriteLine();
            generatingForLoop = false;
            this.Indent++;
            this.GenerateStatements(e.Statements);
            this.GenerateStatement(e.IncrementStatement);
            this.Indent--;
            base.Output.WriteLine(keywordENDDO);
        }
        protected bool writeOriginalCode(CodeObject e, bool trim = false)
        {
            var saveindent = this.Indent;
            bool result = false;
            Indent = 0;
            if (e.HasSourceCode())
            {

                if (e.HasLeadingTrivia())
                    writeTrivia(e, false);
                string sourceCode = e.GetSourceCode();
                if (trim)
                    sourceCode = sourceCode.Trim();
                this.Output.Write(sourceCode);
                result = true;
            }
            else if (e is CodeSnippetTypeMember snippet)
            {
                this.Output.Write(snippet.Text);
                result = true;
            }
            Indent = saveindent;
            return result;
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
            if (SuppressCodeGen)
                return;
            if (e.HasSourceCode() && writeOriginalCode(e))
            {
                _textWriter.SuppressNewLine = true;
                return;
            }
            if ((this.IsCurrentClass || this.IsCurrentStruct) || this.IsCurrentInterface)
            {
                writeTrivia(e);
                _textWriter.SuppressNewLine = false;
                // Do we have some Source Code pushed here by our Parser ??
                // this code contains the method declaration line as well
                // and also the body of the method
                if (!writeOriginalCode(e))
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
                            writeMemberAccessModifier(e, e.Attributes, true);
                        }
                        else
                        {
                            // Per default, all Methods are VIRTUALs
                            base.Output.Write(keywordVIRTUAL);
                        }
                    }

                    base.Output.Write(keywordMETHOD);

                    if (e.PrivateImplementationType != null)
                    {
                        base.Output.Write(e.PrivateImplementationType.BaseType);
                        base.Output.Write(this._staticSelector);
                    }

                    this.OutputIdentifier(e.Name);
                    this.OutputGenericParameters(e.TypeParameters);
                    base.Output.Write("(");
                    this.OutputParameters(e.Parameters);
                    base.Output.Write(") " + keywordAS);
                    this.OutputType(e.ReturnType);
                    base.Output.Write(keywordSTRICT);
                    base.Output.WriteLine();
                    this.Indent++;
                    if (!this.IsCurrentInterface && ((e.Attributes & MemberAttributes.ScopeMask) != MemberAttributes.Abstract))
                    {
                        if (e.Statements.Count == 0)
                        {
                            this.GenerateMethodReturnStatement(new CodeMethodReturnStatement());
                        }
                        else
                        {
                            this.GenerateStatements(e.Statements);
                        }
                    }
                    this.Indent--;
                    // close the method.
                    this.Output.Write("END METHOD");
                    base.Output.WriteLine();
                }
            }

        }

        protected override void GenerateMethodInvokeExpression(CodeMethodInvokeExpression e)
        {
            if (e.Method.TargetObject is CodeTypeReferenceExpression)
            {
                GenerateExpression(e.Method.TargetObject);
                base.Output.Write(_staticSelector);
            }
            else if (e.Method.TargetObject is CodeBinaryOperatorExpression)
            {
                base.Output.Write("(");
                this.GenerateExpression(e.Method.TargetObject);
                base.Output.Write(")");
                base.Output.Write(_selector);
            }
            else
            {
                this.GenerateExpression(e.Method.TargetObject);
                base.Output.Write(_selector);
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
            _entryPoint = null;
            _lastTrivia = null;
            _entryPointType = null;
            _types = new Stack<CodeTypeDeclaration>();
            var globals = e.GetGlobals();
            if (globals != null)
            {
                _globalmembers = globals as List<CodeObject>;
            }
            ReadSettings();
            this.OverrideTextWriter();
            // VerbatimOrder writes the members in the order in which they appear in the collection
            this.Options.VerbatimOrder = true;
            this.Options.BlankLinesBetweenMembers = false;
            var tabStr = "";
            if (_useTabs)
            {
                this.Options.IndentString = "\t";
                tabStr = "\t";
            }
            else
            {
                this.Options.IndentString = _indentString;
                tabStr = _tabString;
            }
            // Hack to set the private field in the IndentedTextWriter
            if (this.Output is IndentedTextWriter itw)
            {
                var field = typeof(IndentedTextWriter).GetField("tabString", BindingFlags.Instance | BindingFlags.NonPublic);
                if (field != null)
                    field.SetValue(itw, tabStr);
            }

            _using.Clear();
            base.GenerateCompileUnitStart(e);
            if (e is XCodeCompileUnit xcu && !xcu.GenerateHeader)
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
            if (_entryPoint != null && _entryPointType != null)
            {
                ImplementEntryPointMethod(_entryPoint, _entryPointType);
            }
            _entryPoint = null;
            _entryPointType = null;
            base.GenerateCompileUnitEnd(e);
            if (_globalmembers != null)
            {
                foreach (var member in _globalmembers)
                {
                    writeOriginalCode(member);
                }
            }
            writeTrivia(e, true);

        }

        protected override void GenerateMethodReferenceExpression(CodeMethodReferenceExpression e)
        {
            var oldEscape = _mustEscape;

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
                base.Output.Write(this._staticSelector);
            }
            this.OutputIdentifier(e.MethodName);
            if (e.TypeArguments.Count > 0)
            {
                this.OutputGenericArguments(e.TypeArguments);
            }
        }

        protected override void GenerateMethodReturnStatement(CodeMethodReturnStatement e)
        {
            base.Output.Write(keywordRETURN);
            if (e.Expression != null)
            {
                base.Output.Write(_ws);
                base.GenerateExpression(e.Expression);
            }
            base.Output.WriteLine();
        }


        private bool _writeTrivia(string trivia)
        {
            var saveIndent = this.Indent;
            Indent = 0;
            this.Output.Write(trivia);
            Indent = saveIndent;
            return true;
        }


        private bool writeTrivia(CodeObject o, bool ending = false)
        {
            string key;
            key = ending ? XSharpCodeConstants.USERDATA_ENDINGTRIVIA : XSharpCodeConstants.USERDATA_LEADINGTRIVIA;
            if (o.UserData.Contains(key))
            {
                string trivia = o.UserData[key] as string;
                _writeTrivia(trivia);
                var l1 = trivia.Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                var l2 = new List<string>();
                foreach (var l in l1)
                {
                    var lTemp = l.Trim();
                    if (!lTemp.StartsWith("//") && !lTemp.StartsWith("#"))
                        l2.Add(l);
                }
                //lastTrivia = String.Join("\r\n", l2);
                return true;
            }
            else if (_lastTrivia != null)
            {
                //_writeTrivia(lastTrivia);
            }
            return false;
        }

        private void writeCodeBefore(CodeObject e)
        {
            if (e.HasCodeBefore())
            {
                string before = e.GetCodeBefore();
                if (!before.Contains("<auto-generated>"))
                {
                    this.Output.Write(before);
                }
            }
        }

        protected override void GenerateNamespace(CodeNamespace e)
        {
            writeTrivia(e);
            this.GenerateCommentStatements(e.Comments);

            // Generate Imports BEFORE the NameSpace
            writeCodeBefore(e);
            //
            this.GenerateNamespaceStart(e);
            //this.Output.WriteLine("");
            this.GenerateNamespaceImports(e);
            this.GenerateTypes(e);
            this.GenerateNamespaceEnd(e);
        }

        protected override void GenerateNamespaceEnd(CodeNamespace e)
        {
            if (!String.IsNullOrEmpty(e.Name))
            {
                if (this.Indent >= 0)
                    this.Indent--;
                _textWriter.SuppressNewLine = false;
                base.Output.WriteLine(keywordEND + keywordNAMESPACE.TrimEnd());
            }
        }

        protected override void GenerateNamespaceImport(CodeNamespaceImport e)
        {
            if (!_using.Contains(e.Namespace.ToLowerInvariant()))
            {
                writeCodeBefore(e);
                writeTrivia(e);
                base.Output.Write(keywordUSING);
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
                if (name.StartsWith("global::", StringComparison.OrdinalIgnoreCase))
                {
                    name = name.Substring(8);
                }
                base.Output.WriteLine(keywordBEGIN + keywordNAMESPACE + name);
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
            // Inside the form editor Properties are parsed into
            // Snippets, so this should normally not happen
            if (SuppressCodeGen)
                return;
            if (e.HasSourceCode() && writeOriginalCode(e))
            {
                _textWriter.SuppressNewLine = true;
                return;
            }
            if ((this.IsCurrentClass || this.IsCurrentStruct) || this.IsCurrentInterface)
            {
                writeTrivia(e);
                if (e.CustomAttributes.Count > 0)
                {
                    this.GenerateAttributes(e.CustomAttributes);
                }

                if (!this.IsCurrentInterface)
                {
                    if (e.PrivateImplementationType == null)
                    {
                        writeMemberAccessModifier(e, e.Attributes, true);
                    }
                    else
                    {
                        base.Output.Write(keywordVIRTUAL);
                    }
                }

                base.Output.Write(keywordPROPERTY);
                if ((e.PrivateImplementationType != null) && !this.IsCurrentInterface)
                {
                    base.Output.Write(e.PrivateImplementationType.BaseType);
                    base.Output.Write(this._staticSelector);
                }
                this.OutputIdentifier(e.Name);

                if (e.Parameters.Count > 0)
                {
                    base.Output.Write("[");
                    this.OutputParameters(e.Parameters);
                    base.Output.Write("]");
                }
                base.Output.Write(" " + keywordAS);
                this.OutputType(e.Type);
                base.Output.WriteLine();
                //
                if (e.HasGet)
                {
                    base.Output.WriteLine();
                    this.Indent++;
                    base.Output.Write(keywordGET);
                    base.Output.WriteLine();
                    this.Indent++;
                    this.GenerateStatements(e.GetStatements);
                    this.Indent--;
                    base.Output.Write(keywordEND + keywordGET);
                    base.Output.WriteLine();
                    this.Indent--;
                }

                if (e.HasSet)
                {
                    base.Output.WriteLine();
                    this.Indent++;
                    base.Output.Write(keywordSET);
                    base.Output.WriteLine();
                    this.Indent++;
                    this.GenerateStatements(e.SetStatements);
                    this.Indent--;
                    base.Output.Write(keywordEND + keywordSET);
                    base.Output.WriteLine();
                    this.Indent--;
                }
                //
                base.Output.Write(keywordEND + keywordPROPERTY);
                base.Output.WriteLine();
            }
        }

        protected override void GeneratePropertyReferenceExpression(CodePropertyReferenceExpression e)
        {
            bool isIdentifier = true;
            var oldescape = _mustEscape;

            if (e.TargetObject != null)
            {
                this.GenerateExpression(e.TargetObject);
                if (e.TargetObject is CodeTypeReferenceExpression)
                {
                    isIdentifier = false;
                    this.Output.Write(this._staticSelector);
                }
                else
                {
                    this.Output.Write(this._selector);
                }
                _mustEscape = false;
            }
            if (isIdentifier)
                this.OutputIdentifier(e.PropertyName);
            else
                this.Output.Write(e.PropertyName);
            _mustEscape = oldescape;

        }

        protected override void GeneratePropertySetValueReferenceExpression(CodePropertySetValueReferenceExpression e)
        {
            base.Output.Write("value");
        }

        protected override void GenerateRemoveEventStatement(CodeRemoveEventStatement e)
        {
            this.GenerateEventReferenceExpression(e.Event);
            base.Output.Write(_ws + "-=" + _ws);
            this.GenerateExpression(e.Listener);
            base.Output.WriteLine();
        }

        protected override void GenerateSnippetExpression(CodeSnippetExpression e)
        {
            base.Output.Write(e.Value);
        }

        protected override void GenerateSnippetMember(CodeSnippetTypeMember e)
        {
            // the base class resets indent for Snippet Members
            Indent = this._indentSave;
            writeTrivia(e);
            base.Output.Write(e.Text);
        }

        protected override void GenerateThisReferenceExpression(CodeThisReferenceExpression e)
        {
            base.Output.Write(keywordSELF);
        }

        protected override void GenerateThrowExceptionStatement(CodeThrowExceptionStatement e)
        {
            base.Output.Write(keywordTHROW);
            if (e.ToThrow != null)
            {
                base.Output.Write(_ws);
                this.GenerateExpression(e.ToThrow);
            }
            base.Output.WriteLine();
        }

        protected override void GenerateTryCatchFinallyStatement(CodeTryCatchFinallyStatement e)
        {
            base.Output.WriteLine(keywordTRY);
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
                    base.Output.Write(keywordCATCH);
                    this.OutputIdentifier(catchSt.LocalName);
                    base.Output.Write(" " + keywordAS);
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
                base.Output.WriteLine(keywordFINALLY);
                this.Indent++;
                this.GenerateStatements(finallySt);
                this.Indent--;
            }
            base.Output.WriteLine(keywordEND + keywordTRY);
            base.Output.WriteLine();
        }

        protected override void GenerateTypeConstructor(CodeTypeConstructor e)
        {
            if (SuppressCodeGen)
                return;

            if (this.IsCurrentClass || this.IsCurrentStruct)
            {
                writeTrivia(e);
                if (e.CustomAttributes.Count > 0)
                {
                    this.GenerateAttributes(e.CustomAttributes);
                }
                base.Output.WriteLine(keywordSTATIC + keywordCONSTRUCTOR + "()");
                this.Indent++;
                this.GenerateStatements(e.Statements);
                this.Indent--;
                base.Output.WriteLine();
            }
        }

        protected override void GenerateTypeEnd(CodeTypeDeclaration e)
        {
            if (!SuppressCodeGen)
            {
                _textWriter.SuppressNewLine = false;
                if (!this.IsCurrentDelegate)
                {
                    this.Indent--;

                    if (!writeTrivia(e, true))
                    {
                        base.Output.WriteLine();
                    }
                    base.Output.Write(keywordEND);
                    if (e.IsClass)
                    {
                        base.Output.Write(keywordCLASS);
                    }
                    else if (e.IsStruct)
                    {
                        base.Output.Write(keywordSTRUCTURE);
                    }
                    else if (e.IsInterface)
                    {
                        base.Output.Write(keywordINTERFACE);
                    }
                    else if (e.IsEnum)
                    {
                        base.Output.Write(keywordENUM);
                    }
                    base.Output.WriteLine();
                }
            }
            _types.Pop();
            _hasSource = false;
        }

        private void writeTypeModifiers(CodeTypeDeclaration e)
        {
            if (e.TypeAttributes.HasFlag(TypeAttributes.Sealed))
            {
                base.Output.Write(keywordSEALED);
            }
            if (e.TypeAttributes.HasFlag(TypeAttributes.Abstract))
            {
                base.Output.Write(keywordABSTRACT);
            }
            if (e.IsPartial)
            {
                base.Output.Write(keywordPARTIAL);
            }
        }

        protected override void GenerateTypeStart(CodeTypeDeclaration e)
        {

            _types.Push(e);
            // Sort the members on the line/column position
            // The form editor reorders them
            var sortedmembers = Helpers.SortMembers(e.Members);
            e.Members.Clear();

            _hasSource = e.HasSourceCode();
            if (e.HasSourceCode() && writeOriginalCode(e))
            {
                _textWriter.SuppressNewLine = true;
                return;
            }
            e.Members.AddRange(sortedmembers);
            writeTrivia(e);
            writeCodeBefore(e);
            if (e.CustomAttributes.Count > 0)
            {
                this.GenerateAttributes(e.CustomAttributes);
            }
            if (!this.IsCurrentDelegate)
            {
                this.OutputTypeAttributes(e);
                if (e.IsStruct)
                {
                    writeTypeModifiers(e);
                    base.Output.Write(keywordSTRUCTURE);
                }
                else if (e.IsEnum)
                {
                    base.Output.Write(keywordENUM);
                }
                else if (e.IsClass)
                {
                    writeTypeModifiers(e);
                    base.Output.Write(keywordCLASS);
                }
                else if (e.IsInterface)
                {
                    base.Output.Write(keywordINTERFACE);
                }
                this.OutputIdentifier(e.Name);
                this.OutputGenericParameters(e.TypeParameters);
                if (e.IsEnum)
                {
                    if (e.BaseTypes.Count > 0)
                    {
                        base.Output.Write(" " + keywordAS);
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
                                base.Output.Write(_ws + keywordIMPLEMENTS);
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
                                base.Output.WriteLine(_ws + ";");
                                this.Indent++;
                                base.Output.Write(keywordINHERIT);
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
                                base.Output.WriteLine(_ws + ";");
                                this.Indent++;
                                base.Output.Write(keywordINHERIT);
                                this.Indent--;
                            }
                            else if (count == 1)
                            {
                                base.Output.WriteLine(_ws + ";");
                                this.Indent++;
                                base.Output.Write(keywordIMPLEMENTS);
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
                _indentSave = this.Indent;
            }
            else
            {
                if (e.HasSourceCode())
                {
                    writeOriginalCode(e);
                }
                else
                {
                    CodeTypeDelegate delegate1 = (CodeTypeDelegate)e;
                    base.Output.Write(keywordDELEGATE);
                    this.OutputIdentifier(e.Name);
                    base.Output.Write("(");
                    this.OutputParameters(delegate1.Parameters);
                    base.Output.Write(") " + keywordAS);
                    this.OutputType(delegate1.ReturnType);
                    base.Output.WriteLine();
                }
            }
        }

        protected override void GenerateVariableDeclarationStatement(CodeVariableDeclarationStatement e)
        {
            base.Output.Write(keywordLOCAL);
            this.OutputIdentifier(e.Name);
            if (e.InitExpression != null)
            {
                this.WriteAssignment();
                this.GenerateExpression(e.InitExpression);
            }
            base.Output.Write(" " + keywordAS);
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

        private void WriteEscapedChar(char value)
        {
            base.Output.Write("\\u");
            int num = value;
            base.Output.Write(num.ToString("X4", CultureInfo.InvariantCulture));

        }


        private void GeneratePrimitiveChar(char c)
        {
            this.Output.Write("c\'");
            switch (c)
            {
                case '\r':
                    base.Output.Write("\\r");
                    break;
                case '\t':
                    base.Output.Write("\\t");
                    break;
                case '"':
                    base.Output.Write("\\\"");
                    break;
                case '\'':
                    base.Output.Write("\\'");
                    break;
                case '\\':
                    base.Output.Write("\\\\");
                    break;
                case '\0':
                    base.Output.Write("\\0");
                    break;
                case '\n':
                    base.Output.Write("\\n");
                    break;
                case '\u0084':
                case '\u0085':
                case '\u2028':
                case '\u2029':
                    this.WriteEscapedChar(c);
                    break;
                default:
                    if (char.IsSurrogate(c))
                    {
                        this.WriteEscapedChar(c);
                    }
                    else
                    {
                        base.Output.Write(c);
                    }
                    break;
            }
            this.Output.Write('\'');
        }
        protected override void GeneratePrimitiveExpression(CodePrimitiveExpression e)
        {
            if (e.Value is char c)
            {
                GeneratePrimitiveChar(c);

            }
            else
            {
                if (e.Value is uint || e.Value is ulong)
                {
                    var tmp = Convert.ToDouble(e.Value);
                    if (tmp < long.MaxValue)
                        e.Value = Convert.ToInt64(e.Value);
                    else
                        e.Value = tmp;
                }
                base.GeneratePrimitiveExpression(e);
            }
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
                    base.Output.Write(" " + keywordAS);
                    break;
                case FieldDirection.Out:
                    base.Output.Write(" " + keywordOUT);
                    break;
                case FieldDirection.Ref:
                    base.Output.Write(" " + keywordREF);
                    break;
            }
            this.OutputType(e.Type);
        }

        protected override string GetTypeOutput(CodeTypeReference typeRef)
        {
            string str = string.Empty;
            if (typeRef.Options.HasFlag(CodeTypeReferenceOptions.GlobalReference))
            {
                str = "global::";
            }
            CodeTypeReference arrayElementType = typeRef;
            if (typeRef.HasSourceCode())
            {
                // some types with parsing problems have their definition saved as userdata
                var res = typeRef.GetSourceCode();
                return res.TrimStart();
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
            if (baseType.ToLower() == "system.object")
            {
                // keep because it is used as in System.Object.ReferenceEquals
                return "System.Object";
            }

            if (SystemToXSharp.ContainsKey(baseType))
            {
                return FormatKeyword(SystemToXSharp[baseType]);
            }
            if (XSharpToSystem.ContainsKey(baseType))
            {
                return FormatKeyword(baseType);
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
            if (XSharpKeywords.Contains(value))
            {
                return false;
            }
            if (value.StartsWith("@@"))
                return true;
            return CodeGenerator.IsValidLanguageIndependentIdentifier(value);
        }

        protected override void ValidateIdentifier(string value)
        {
            base.ValidateIdentifier(value);
        }
        protected override void OutputType(CodeTypeReference typeRef)
        {
            // Fix problem where Windows Forms designer does not put the fully qualified path to the global
            // resources in the typereference
            string typeName = this.GetTypeOutput(typeRef);
            if (typeName.EndsWith(".Resources") && !typeName.EndsWith("Properties.Resources"))
            {
                if (typeName.ToLower().StartsWith("global::"))
                {
                    typeName = typeName.Replace(".Resources", ".Properties.Resources");
                }
            }
            this.Output.Write(typeName);
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
                default:
                    break;
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
                case GeneratorSupport.GotoStatements:
                    return false;
                default:
                    break;
            }
            return false;
        }


        // in XSharp, continuing on the next line is done using a semi-colon !
        protected override void ContinueOnNewLine(string st)
        {
            this.Output.Write(st);
            this.Output.WriteLine(_ws + ";");
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
                        base.Output.Write(_ws);
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
                this.WriteAssignment();
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
            if (e.Attributes.HasFlag(MemberAttributes.New))
            {
                //this.Output.Write("NEW ");
            }
            TypeAttributes typeAttributes = e.TypeAttributes;
            switch ((typeAttributes & TypeAttributes.VisibilityMask))
            {

                case TypeAttributes.NestedAssembly:
                case TypeAttributes.NestedFamANDAssem:
                    base.Output.Write(keywordINTERNAL);
                    break;


                case TypeAttributes.NestedPrivate:
                    if (_privateKeyword == 0)
                        base.Output.Write(keywordPRIVATE);
                    else
                        base.Output.Write(keywordHIDDEN);
                    break;

                case TypeAttributes.NestedFamily:
                    base.Output.Write(keywordPROTECTED);
                    break;

                case TypeAttributes.NestedFamORAssem:
                    base.Output.Write(keywordPROTECTED + keywordINTERNAL);
                    break;
                default:
                case TypeAttributes.AutoLayout:
                case TypeAttributes.Public:
                case TypeAttributes.NestedPublic:
                    if (_publicKeyword == 0)
                        base.Output.Write(keywordPUBLIC);
                    else if (_publicKeyword == 1)
                        base.Output.Write(keywordEXPORT);
                    else
                        base.Output.Write("");
                    break;
            }
        }

        protected override void OutputIdentifier(string ident)
        {
            if (_mustEscape)
                base.OutputIdentifier(CreateEscapedIdentifier(ident));
            else
                base.OutputIdentifier(ident);
        }
        protected override void OutputMemberAccessModifier(MemberAttributes attributes)
        {
            switch ((attributes & MemberAttributes.AccessMask))
            {
                case MemberAttributes.Public:

                    if (_publicKeyword == 0)
                        base.Output.Write(keywordPUBLIC);
                    else if (_publicKeyword == 1)
                        base.Output.Write(keywordEXPORT);
                    else
                        base.Output.Write("");
                    break;
                case MemberAttributes.Family: // A member that is accessible within the family of its class and derived classes.
                    this.Output.Write(keywordPROTECTED);
                    break;
                case MemberAttributes.FamilyOrAssembly: // A member that is accessible within its class, its derived classes in any assembly, and any class in the same assembly.
                    this.Output.Write(keywordPROTECTED + keywordINTERNAL);
                    break;
                case MemberAttributes.FamilyAndAssembly: // A member that is accessible within its class, and derived classes in the same assembly
                case MemberAttributes.Assembly:
                    // C# maps these two both to Internal
                    this.Output.Write(keywordINTERNAL);
                    break;
                case MemberAttributes.Private:
                default:
                    if (_privateKeyword == 0)
                        base.Output.Write(keywordPRIVATE);
                    else
                        base.Output.Write(keywordHIDDEN);
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
                    this.Output.Write(keywordABSTRACT);
                    return;

                case MemberAttributes.Final:
                    this.Output.Write("");
                    return;

                case MemberAttributes.Static:
                    this.Output.Write(keywordSTATIC);
                    return;

                case MemberAttributes.Override:
                    this.Output.Write(keywordVIRTUAL);
                    return;
            }
            switch ((attributes & MemberAttributes.AccessMask))
            {
                // Same rules as in C#
                case MemberAttributes.Assembly:
                case MemberAttributes.Family:
                case MemberAttributes.Public:
                    this.Output.Write(keywordVIRTUAL);
                    break;
            }
        }
        protected override string CompilerName => "XSharp";
        protected override string FileExtension => ".prg";
        protected override string CmdArgsFromParameters(CompilerParameters options)
        {
            throw new NotImplementedException();
        }
        protected override void ProcessCompilerOutputLine(CompilerResults results, string line)
        {
            throw new NotImplementedException();
        }

    }
}
