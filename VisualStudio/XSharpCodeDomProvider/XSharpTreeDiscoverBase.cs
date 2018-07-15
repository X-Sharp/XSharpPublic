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
using System.Diagnostics;
using System.Collections.Immutable;
using XSharpModel;
namespace XSharp.CodeDom
{
    internal class XSharpBaseDiscover : XSharpBaseListener
    {

        protected IProjectTypeHelper _projectNode;
        protected Dictionary<string, Type> _types;    // type cache
        protected Dictionary<string, XType> _xtypes;    // XSharp type cache
        protected Dictionary<string, EnvDTE.CodeElement> _stypes;    // ENVDTE.CodeElement kind = type cache
        protected IList<string> _usings;          // uses for type lookup
        protected IList<IToken> _tokens;          // used to find comments 

        internal Dictionary<ParserRuleContext, List<XCodeMemberField>> FieldList { get; set; }
        internal string SourceCode { get; set; }
        internal string CurrentFile { get; set; }
        const string SnippetsTxt = @"D:\Snippets.txt";
        protected CodeExpression BuildSnippetExpression(String txt)
        {
            if (!System.IO.File.Exists(SnippetsTxt))
                System.IO.File.WriteAllText(SnippetsTxt, "");
            string old = System.IO.File.ReadAllText(SnippetsTxt);
            old += "\n" + txt;
            System.IO.File.WriteAllText(SnippetsTxt, old);
            return new CodeSnippetExpression(txt);
        }

        internal XSharpBaseDiscover(IProjectTypeHelper projectNode) : base()
        {
            FieldList = new Dictionary<ParserRuleContext, List<XCodeMemberField>>();
            _projectNode = projectNode;
            this._types = new Dictionary<string, Type>(StringComparer.OrdinalIgnoreCase);
            this._usings = new List<string>();
            this._xtypes = new Dictionary<string, XType>(StringComparer.OrdinalIgnoreCase);
            this._stypes = new Dictionary<string, EnvDTE.CodeElement>(StringComparer.OrdinalIgnoreCase);
        }

        public override void EnterSource([NotNull] XSharpParser.SourceContext context)
        {
            var source = (XSharpLexer)context.Start.TokenSource;
            source.Reset();
            _tokens = source.GetAllTokens();
        }


        #region Helpers

        protected XCodeTypeReference BuildDataType(XSharpParser.DatatypeContext context)
        {
            //
            // Datatype is ptrDatatype
            // or arrayDatatype
            // or simpleDatatype
            // or nullableDatatype 
            // they all have a TypeName
            XSharpParser.TypeNameContext tn = null;
            if (context is XSharpParser.PtrDatatypeContext)
            {
                XSharpParser.PtrDatatypeContext ptrData = (XSharpParser.PtrDatatypeContext)context;
                tn = ptrData.TypeName;
            }
            else if (context is XSharpParser.ArrayDatatypeContext)
            {
                XSharpParser.ArrayDatatypeContext aData = (XSharpParser.ArrayDatatypeContext)context;
                tn = aData.TypeName;
            }
            else if (context is XSharpParser.SimpleDatatypeContext)
            {
                XSharpParser.SimpleDatatypeContext sdt = (XSharpParser.SimpleDatatypeContext)context;
                tn = sdt.TypeName;
            }
            else if (context is XSharpParser.NullableDatatypeContext)
            {
                XSharpParser.NullableDatatypeContext ndc = context as XSharpParser.NullableDatatypeContext;
                tn = ndc.TypeName;
            }
            XCodeTypeReference expr = null;
            if (tn != null)
            {
                if (tn.NativeType != null)
                {
                    expr = BuildNativeType(tn.NativeType);
                }
                else if (tn.XType != null)
                {
                    expr = BuildXBaseType(tn.XType);
                }
                else if (tn.Name != null)
                {
                    expr = BuildName(tn.Name);
                }
            }
            else
            {
                expr = new XCodeTypeReference(typeof(void));
            }
            //
            return expr;
        }
        protected CodeExpression BuildLiteralValue(XSharpParser.LiteralValueContext context)
        {
            CodeExpression expr = null;
            string value;
            //
            switch (context.Token.Type)
            {
                case XSharpParser.BIN_CONST:
                case XSharpParser.INT_CONST:
                case XSharpParser.HEX_CONST:
                case XSharpParser.REAL_CONST:
                    expr = new CodePrimitiveExpression(GetNumericValue(context));
                    break;
                case XSharpParser.TRUE_CONST:
                    expr = new CodePrimitiveExpression(true);
                    break;
                case XSharpParser.FALSE_CONST:
                    expr = new CodePrimitiveExpression(false);
                    break;
                case XSharpParser.STRING_CONST:
                case XSharpParser.ESCAPED_STRING_CONST:
                    value = context.GetText();
                    value = value.Substring(1, value.Length - 2);
                    if (context.Token.Type == XSharpParser.ESCAPED_STRING_CONST)
                        value = BuildUnEscapedString(value);
                    expr = new CodePrimitiveExpression(value);
                    break;
                case XSharpParser.CHAR_CONST:
                    value = context.GetText();
                    value = value.Substring(1, value.Length - 2);
                    if (value.Length >= 1)
                        expr = new CodePrimitiveExpression(value[0]);
                    break;
                case XSharpParser.NULL:
                    expr = new CodePrimitiveExpression(null);
                    break;
                case XSharpParser.NIL:
                case XSharpParser.NULL_ARRAY:
                case XSharpParser.NULL_CODEBLOCK:
                case XSharpParser.NULL_DATE:
                case XSharpParser.NULL_OBJECT:
                case XSharpParser.NULL_PSZ:
                case XSharpParser.NULL_PTR:
                case XSharpParser.NULL_STRING:
                case XSharpParser.NULL_SYMBOL:
                case XSharpParser.SYMBOL_CONST:
                case XSharpParser.DATE_CONST:
                default:
                    expr = BuildSnippetExpression(context.Token.Text);
                    break;
            }
            return expr;
        }
        protected XCodeTypeReference BuildName(XSharpParser.NameContext context)
        {
            XCodeTypeReference expr = null;
            //
            var sName = context.GetText();
            //if (!sName.Contains(".") && !sName.Contains(":") && !sName.Contains(">"))
            //{
            //    return BuildTypeReference(sName);
            //}
            if (!sName.EndsWith(">"))
            {
                System.Type type = findType(sName);
                if (type != null)
                {
                    return new XCodeTypeReference(type);
                }
            }
            if (context is XSharpParser.QualifiedNameContext)
            {
                XSharpParser.QualifiedNameContext qual = (XSharpParser.QualifiedNameContext)context;
                expr = BuildName(qual.Left);
                expr = BuildTypeReference(expr.BaseType + "." + BuildSimpleName(qual.Right).BaseType);
            }
            else if (context is XSharpParser.SimpleOrAliasedNameContext)
            {
                var alias = context as XSharpParser.SimpleOrAliasedNameContext;
                var name = alias.Name as XSharpParser.AliasedNameContext;

                //
                if (name is XSharpParser.AliasQualifiedNameContext)
                {
                    XSharpParser.AliasQualifiedNameContext al = (XSharpParser.AliasQualifiedNameContext)name;
                    expr = BuildSimpleName(al.Right);
                    expr = BuildTypeReference(al.Alias.GetText() + "::" + expr.BaseType);
                }
                else if (name is XSharpParser.GlobalQualifiedNameContext)
                {
                    var gqn = name as XSharpParser.GlobalQualifiedNameContext;
                    expr = BuildSimpleName(gqn.Right);
                    expr = BuildTypeReference("global::" + expr.BaseType);
                }
                else if (name is XSharpParser.IdentifierOrGenericNameContext)
                {
                    var id = name as XSharpParser.IdentifierOrGenericNameContext;
                    expr = BuildSimpleName(id.Name);
                }
            }
            if (sName.Contains(">"))
            {
                // work around to fix type problems with generics
                expr.UserData[XSharpCodeConstants.USERDATA_CODE] = sName;
            }
            //
            return expr;
        }
        protected TypeAttributes ContextToClassModifiers(XSharpParser.ClassModifiersContext modifiers)
        {
            TypeAttributes retValue = TypeAttributes.Public;
            //
            if (modifiers.INTERNAL().Length > 0)
                retValue = TypeAttributes.NestedAssembly;
            //
            if (modifiers.HIDDEN().Length > 0)
                retValue = TypeAttributes.NestedPrivate;
            //
            if (modifiers.PRIVATE().Length > 0)
                retValue = TypeAttributes.NestedPrivate;
            //
            if (modifiers.PROTECTED().Length > 0)
            {
                if (modifiers.INTERNAL().Length > 0)
                    retValue = TypeAttributes.NestedFamORAssem;
                else
                    retValue = TypeAttributes.NestedFamily;
            }
            //
            if (modifiers.EXPORT().Length > 0 || modifiers.PUBLIC().Length > 0)
                retValue = TypeAttributes.Public;
            //
            return retValue;
        }

        protected MemberAttributes decodeMemberAttributes(IList<IToken> tokens)
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
        protected MemberAttributes ContextToMethodModifiers(XSharpParser.MemberModifiersContext modifiers)
        {
            return decodeMemberAttributes(modifiers._Tokens);
        }
        protected MemberAttributes ContextToConstructorModifiers(XSharpParser.ConstructorModifiersContext modifiers)
        {
            return decodeMemberAttributes(modifiers._Tokens);
        }

        protected MemberAttributes ContextToEventModifiers(XSharpParser.EventModifiersContext modifiers)
        {
            return decodeMemberAttributes(modifiers._Tokens);
        }

        protected bool IsEmpty(CodeNamespace nspace)
        {
            return (nspace.Types.Count == 0);
        }

        protected CodeParameterDeclarationExpressionCollection GetParametersList(XSharpParser.ParameterListContext paramList)
        {
            CodeParameterDeclarationExpressionCollection pList = new CodeParameterDeclarationExpressionCollection();
            //
            if (paramList != null)
            {
                foreach (var param in paramList._Params)
                {
                    CodeParameterDeclarationExpression pm = new CodeParameterDeclarationExpression();
                    pm.Name = param.Id.GetText();
                    pm.Type = BuildDataType(param.Type);
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
            }
            //
            return pList;
        }


        protected void FillCodeDomDesignerData(CodeObject newElement, int line, int col)
        {
            CodeDomDesignerData data = new CodeDomDesignerData();
            //
            data.CaretPosition = new System.Drawing.Point(col, line);
            data.FileName = this.CurrentFile;
            // point is where the designer will try to focus if the
            // user wants to add event handler stuff.  
            newElement.UserData[typeof(CodeDomDesignerData)] = data;
            newElement.UserData[typeof(System.Drawing.Point)] = data.CaretPosition;
        }

        protected void FillCodeSource(CodeObject element, IToken endOfFirstLine, ParserRuleContext context)
        {
            int length = context.Stop.StopIndex - endOfFirstLine.StopIndex - 2;
            string extract = "";
            if (length > 0)
                extract = this.SourceCode.Substring(endOfFirstLine.StopIndex + 1, length).TrimStart();
            element.UserData[XSharpCodeConstants.USERDATA_CODE] = extract;
        }

        protected CodeSnippetTypeMember CreateSnippetMember(ParserRuleContext context)
        {
            // The original source code
            var xtoken = context.Start as XSharpToken;
            var start = xtoken.OriginalTokenIndex;
            while (start > 0)
            {
                if (_tokens[start-1].Channel == XSharpLexer.Hidden)
                {
                    start = start - 1;
                    continue;
                }
                else
                {
                    break;
                }
            }
            var startIndex = _tokens[start].StartIndex;
            int length = context.Stop.StopIndex - startIndex + 1;
            string sourceCode = this.SourceCode.Substring(startIndex, length);
            CodeSnippetTypeMember snippet = new CodeSnippetTypeMember(sourceCode);
            FillCodeDomDesignerData(snippet, context.Start.Line, context.Start.Column);
            return snippet;
        }

        /// <summary>
        /// Get a LiteralValueContext containing a BIN_CONST, INT_CONST, HEX_CONST, or a REAL_CONST
        /// as a string, and convert it to the "real" value, with the corresponding Type.
        /// </summary>
        /// <param name="context"></param>
        /// <returns>An Object of the needed Type, with the value</returns>
        protected object GetNumericValue(XSharpParser.LiteralValueContext context)
        {
            Object ret = null;
            string value = context.Token.Text;
            var type = context.Token.Type;
            // try to check if this literal has a Prefix
            try
            {
                if ( context.Parent.Parent is XSharpParser.PrimaryExpressionContext )
                {
                    var prim = context.Parent.Parent as XSharpParser.PrimaryExpressionContext;
                    if ( prim.Parent is XSharpParser.PrefixExpressionContext )
                    {
                        var pref = prim.Parent as XSharpParser.PrefixExpressionContext;
                        if( pref.MINUS() != null )
                        {
                            value = "-" + value;
                        }
                    }
                }
            }
            catch
            {
                // eat the troubles and ignore please...
            }
            //
            if (type == XSharpParser.BIN_CONST || type == XSharpParser.INT_CONST || type == XSharpParser.HEX_CONST)
            {
                bool isUnsigned = value.EndsWith("u", StringComparison.OrdinalIgnoreCase);
                bool isSigned = value.EndsWith("l", StringComparison.OrdinalIgnoreCase);
                if (isSigned || isUnsigned)
                {
                    value = value.Substring(0, value.Length - 1);
                }
                // -1 for Unsigned; -2 for 0x or 0b
                int len = value.Length - (context.BIN_CONST() != null || context.HEX_CONST() != null ? 2 : 0);
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
                else if (type == XSharpParser.HEX_CONST)
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
                if (value.EndsWith("m", StringComparison.OrdinalIgnoreCase) ||
                    value.EndsWith("r", StringComparison.OrdinalIgnoreCase) ||
                    value.EndsWith("d", StringComparison.OrdinalIgnoreCase))
                {
                    value = value.Substring(0, value.Length - 1);
                }
                try
                {
                    d = double.Parse(value, System.Globalization.CultureInfo.InvariantCulture);
                }
                catch (Exception)
                {
                    d = Double.NaN;
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
                            // Put the Digits in a string
                            string hexValue = new string(hex, 0, digits);
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

        protected XCodeTypeReference BuildTypeReference(string name)
        {
            System.Type type;
            type = findType(name);
            if (type != null)
            {
                return new XCodeTypeReference(type);
            }
            else
            {
                var xtype = _projectNode.ResolveXType(name, _usings.ToImmutableArray());
                if (xtype != null)
                    return new XCodeTypeReference(xtype.FullName);
                else
                    return new XCodeTypeReference(name);
            }
                

        }
        protected System.Type findType(string typeName)
        {
            if (_types.ContainsKey(typeName))
            {
                return _types[typeName];
            }
            var type = _projectNode.ResolveType(typeName, _usings.ToImmutableArray());
            if (type != null)
            {
                _types.Add(typeName, type);
            }
            return type;

        }

        protected XType findXType(string typeName)
        {
            if (_xtypes.ContainsKey(typeName))
                return _xtypes[typeName];
            var type = _projectNode.ResolveXType(typeName, _usings.ToImmutableArray());
            if (type != null)
                _xtypes.Add(typeName, type);
            return type;
        }

        protected XType findReferencedType(string typeName)
        {
            if (_xtypes.ContainsKey(typeName))
            {
                return _xtypes[typeName];
            }
            var type = _projectNode.ResolveReferencedType(typeName, _usings.ToImmutableArray());
            //
            if (type != null)
                _xtypes.Add(typeName, type);
            return type;
        }

        //protected EnvDTE.CodeElement findStrangerType(string typeName)
        //{
        //    if (_stypes.ContainsKey(typeName))
        //    {
        //        return _stypes[typeName];
        //    }
        //    var type = _projectNode.ResolveStrangerType(typeName, _usings.ToImmutableArray());
        //    //
        //    if (type != null)
        //        _stypes.Add(typeName, type);
        //    return type;
        //}

        protected TypeXType findTypeXType(string typeName)
        {
            TypeXType txtype = null;
            var type = findType(typeName);
            if (type != null)
            {
                txtype = new TypeXType(type);
            }
            else
            {
                var xtype = findXType(typeName);
                if (xtype == null)
                {
                    xtype = findReferencedType(typeName);
                }
                if (xtype != null)
                {
                    txtype = new TypeXType(xtype);
                }
            }
            return txtype;
        }

        protected XCodeTypeReference BuildNativeType(XSharpParser.NativeTypeContext nativeType)
        {
            //
            Type type;
            switch (nativeType.Token.Type)
            {
                case XSharpParser.BYTE:
                    type = typeof(System.Byte);
                    break;
                case XSharpParser.DWORD:
                    type = typeof(System.UInt32);
                    break;
                case XSharpParser.SHORTINT:
                    type = typeof(System.Int16);
                    break;
                case XSharpParser.INT:
                case XSharpParser.LONGINT:
                    type = typeof(System.Int32);
                    break;
                case XSharpParser.INT64:
                    type = typeof(System.Int64);
                    break;
                case XSharpParser.UINT64:
                    type = typeof(System.UInt64);
                    break;
                case XSharpParser.LOGIC:
                    type = typeof(System.Boolean);
                    break;
                case XSharpParser.OBJECT:
                    type = typeof(System.Object);
                    break;
                case XSharpParser.REAL4:
                    type = typeof(System.Single);
                    break;
                case XSharpParser.REAL8:
                    type = typeof(System.Double);
                    break;
                case XSharpParser.STRING:
                    type = typeof(System.String);
                    break;
                case XSharpParser.WORD:
                    type = typeof(System.UInt16);
                    break;
                case XSharpParser.VOID:
                    type = typeof(void);
                    break;
                case XSharpParser.DYNAMIC:
                    type = typeof(System.Object);
                    break;
                default:
                    var strType = nativeType.Token.Text;
                    return BuildTypeReference(strType);
            }
            return new XCodeTypeReference(type);
        }

        protected XCodeTypeReference BuildXBaseType(XSharpParser.XbaseTypeContext xbaseType)
        {
            return new XCodeTypeReference(xbaseType.Token.Text);
        }

        protected XCodeTypeReference BuildSimpleName(XSharpParser.SimpleNameContext simpleName)
        {
            XCodeTypeReference expr = null;
            //
            string name = simpleName.Id.GetText();
            string gen = "";
            if (simpleName.GenericArgList != null)
            {
                string argList = "";
                int i = 0;
                foreach (var generic in simpleName.GenericArgList._GenericArgs)
                {
                    if (i > 0)
                        argList += ",";
                    var tmp = BuildDataType(generic);
                    argList += tmp.BaseType;
                    i++;
                }
                //
                gen = "`" + i.ToString() + "[" + argList + "]";
            }
            expr = BuildTypeReference(name + gen);
            //
            return expr;
        }
        #endregion
        #region Common Methods
        private XCodeNamespace _currentNamespace;
        public XCodeNamespace CurrentNamespace  
        {
            get
            {
                if (_currentNamespace == null)
                {
                    _currentNamespace = new XCodeNamespace("");
                }
                return _currentNamespace;
            }

            internal set
            {
                _currentNamespace = value;
            }
        }
        public override void EnterUsing_([NotNull] XSharpParser.Using_Context context)
        {
            var import = new XCodeNamespaceImport(context.Name.GetText());
            CurrentNamespace.Imports.Add(import);
            _usings.Add(import.Namespace);
        }
        #endregion
    }
}