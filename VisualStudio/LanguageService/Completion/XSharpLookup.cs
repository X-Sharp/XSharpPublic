//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Text;
using XSharpModel;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.CodeAnalysis.XSharp;
using System.Collections.Immutable;

namespace XSharp.LanguageService
{
    internal static class XSharpLookup
    {

        internal static bool StringEquals(string lhs, string rhs)
        {
            if (string.Equals(lhs, rhs, StringComparison.OrdinalIgnoreCase))
                return true;
            return false;
        }
        static List<string> nestedSearches = new List<string>();
        static public CompletionElement FindIdentifier(XSourceMemberSymbol member, string name, ref CompletionType cType,
            Modifiers visibility, string currentNS, ITextSnapshot snapshot, int currentLine, XSharpDialect dialect)
        {
            IXSymbolBase element;
            CompletionElement foundElement = null;
            if (nestedSearches.Contains(name, StringComparer.OrdinalIgnoreCase))
            {
                return null;
            }
            int nestedLevel = nestedSearches.Count();
            try
            {
                nestedSearches.Add(name);
                if (cType.IsEmpty())
                {
                    cType = new CompletionType(member.ParentType, null);
                }
                WriteOutputMessage($"--> FindIdentifier in {cType.FullName}, {name} ");
                element = member.Parameters.Where(x => StringEquals(x.Name, name)).FirstOrDefault();
                if (element == null)
                {
                    // then Locals
                    // line numbers in the range are 1 based. currentLine = 0 based !

                    element = member.GetLocals(snapshot, currentLine, dialect).Where(x => StringEquals(x.Name, name) && x.Range.StartLine - 1 <= currentLine).LastOrDefault();
                    if (element == null)
                    {
                        // We can have a Property/Field of the current CompletionType
                        if (!cType.IsEmpty())
                        {
                            cType = SearchPropertyOrFieldIn(cType, name, visibility, out foundElement);
                            if (foundElement != null)
                            {
                                element = foundElement.Result;
                            }

                        }
                        // Find Defines and globals in this file
                        if (element == null && cType.IsEmpty() && member.File.GlobalType != null)
                        {
                            element = member.File.GlobalType.GetMembers(name, true).FirstOrDefault();
                        }
                        if (element == null)
                        {
                            var type = member.File.Project.Lookup(XSharpModel.XLiterals.GlobalName);
                            if (type != null)
                            {
                                element = type.GetMembers(name, true).FirstOrDefault();
                            }
                        }
                        if (element == null)
                        {
                            // Now, search for a Global in external Assemblies
                            //
                            cType = SearchGlobalFieldIn(member.File, name, out foundElement);
                        }
                    }
                }
                if (element != null)
                {
                    if (element is XSourceVariableSymbol xVar)
                    {
                        if (xVar.TypeName == XLiterals.VarType)
                        {
                            resolveVarType(xVar, member, ref cType, visibility, currentNS, snapshot, currentLine, dialect);
                        }
                        cType = new CompletionType(xVar, currentNS);
                        foundElement = new CompletionElement(xVar);
                    }
                    else if (element is IXMemberSymbol)
                    {
                        var xMember = (IXMemberSymbol)element;
                        cType = new CompletionType(xMember);
                        foundElement = new CompletionElement(xMember);
                    }
                    else if (element is IXTypeSymbol xType)
                    {
                        cType = new CompletionType(xType);
                        foundElement = new CompletionElement(xType);
                    }
                }
            }
            catch (Exception ex)
            {
                XSettings.DisplayOutputMessage("FindIdentifier failed: ");
                XSettings.DisplayException(ex);
            }
            finally
            {
                while (nestedSearches.Count > nestedLevel)
                {
                    nestedSearches.Remove(nestedSearches.Last());
                }
            }
            return foundElement;
        }

        public static XSourceMemberSymbol FindMemberAtPosition(int nPosition, XFile file)
        {
            if (file == null || file.EntityList == null)
            {
                return null;
            }
            var member = file.FindMemberAtPosition(nPosition);
            if (member is XSourceMemberSymbol)
            {
                return member as XSourceMemberSymbol;
            }
            // if we can't find a member then look for the global type in the file
            // and return its last member
            var xType = file.TypeList.FirstOrDefault();
            if (xType.Value != null)
            {
                return xType.Value.XMembers.LastOrDefault();
            }
            WriteOutputMessage(string.Format("Cannot find member at 0 based position {0} in file {0} .", nPosition, file.FullPath));
            return null;

        }

        public static XSourceMemberSymbol FindMember(int nLine, XFile file)
        {
            if (file == null || file.EntityList == null)
            {
                return null;
            }
            var member = file.FindMemberAtRow(nLine);
            if (member is XSourceMemberSymbol)
            {
                return member as XSourceMemberSymbol;
            }
            if (member is XSourceTypeSymbol xtype)
            {
                if (xtype.Members.Count > 0)
                {
                    return xtype.Members.LastOrDefault() as XSourceMemberSymbol;
                }
            }
            // try a few rows before
            member = file.FindMemberAtRow(Math.Max(nLine - 10, 1));
            if (member is XSourceMemberSymbol)
            {
                return member as XSourceMemberSymbol;
            }
            if (member is XSourceTypeSymbol xtype1)
            {
                if (xtype1.XMembers.Count > 0)
                {
                    return xtype1.XMembers.LastOrDefault();
                }
            }

            // if we can't find a member then look for the global type in the file
            // and return its last member
            var ent = file.EntityList.LastOrDefault();
            if (ent is XSourceMemberSymbol)
                return ent as XSourceMemberSymbol;
            if (ent is XSourceTypeSymbol)
                return ((XSourceTypeSymbol)ent).XMembers.LastOrDefault();


#if DEBUG
            WriteOutputMessage(string.Format("Cannot find member at 0 based line {0} in file {0} .", nLine, file.FullPath));
#endif
            return null;
        }

        private static string GetTypeFromFoundElement(CompletionElement foundElement)
        {
            // Let's set the Std Type for this VAR
            if (foundElement.IsGeneric)
            {
                return foundElement.GenericTypeName;
            }
            else if (foundElement.IsArray)
            {
                return foundElement.ReturnType.FullName;
            }
            var type = foundElement.ReturnType;
            if (type.Type != null)
            {
                foreach (var prop in type.Type.GetProperties())
                {
                    var pars = prop.Parameters;
                    if (pars.Count == 1)
                    {
                        switch (pars[0].TypeName.ToLower())
                        {
                            case "long":
                            case "int":
                            case "system.int32":
                                return prop.TypeName;
                        }
                    }

                }
            }

            return "object";
        }


        private static void resolveVarType(XSourceVariableSymbol xVar, XSourceMemberSymbol member, ref CompletionType cType,
            Modifiers visibility, string currentNS, ITextSnapshot snapshot, int currentLine, XSharpDialect dialect)
        {
            var tokens = xVar.Expression;
            //List<XSharpToken> tokenList;
            //string expr;
            CompletionElement foundElement = null;
            bool resolved = false;
            if (tokens == null || tokens.Count == 0)
            {
                xVar.TypeName = "OBJECT";
                return;
            }
            if (tokens.Count == 1)
            {
                resolved = true;
                switch (tokens[0].Type)
                {
                    case XSharpLexer.STRING_CONST:
                    case XSharpLexer.NULL_STRING:
                        xVar.TypeName = "STRING";
                        break;
                    case XSharpLexer.INT_CONST:
                        xVar.TypeName = "INT";
                        break;
                    case XSharpLexer.CHAR_CONST:
                        xVar.TypeName = "CHAR";
                        break;
                    case XSharpLexer.REAL_CONST:
                        xVar.TypeName = "REAL8";
                        break;
                    case XSharpLexer.SYMBOL_CONST:
                        xVar.TypeName = "SYMBOL";
                        break;
                    case XSharpLexer.NULL_DATE:
                    case XSharpLexer.DATE_CONST:
                        xVar.TypeName = "DATE";
                        break;
                    case XSharpLexer.TRUE_CONST:
                    case XSharpLexer.FALSE_CONST:
                        xVar.TypeName = "LOGIC";
                        break;
                    case XSharpLexer.NIL:
                        xVar.TypeName = "USUAL";
                        break;
                    case XSharpLexer.NULL:
                        xVar.TypeName = "OBJECT";
                        break;
                    case XSharpLexer.NULL_PTR:
                        xVar.TypeName = "IntPtr";
                        break;
                    default:
                        resolved = false;
                        break;

                }
                if (resolved)
                    return;
            }
            if (tokens[0].Type == XSharpLexer.IN && tokens.Count > 1)
            {
                // foreach loop with IN <somevariable> or IN <list>
                if (tokens.Count == 2)
                {
                    string collection = tokens[1].Text;
                    foundElement = FindIdentifier(member, collection, ref cType,
                                visibility, currentNS, snapshot, currentLine, dialect);
                    if (foundElement != null)
                    {
                        xVar.TypeName = GetTypeFromFoundElement(foundElement);
                        return;
                    }
                }
                //Todo: Type Lookup Var
                //expr = BuildTokenString(tokens, 1);
                //if (!string.IsNullOrEmpty(expr))
                //{
                //    tokenList = new List<XSharpToken> { expr };
                //    cType = RetrieveType(xVar.File, tokenList, member, currentNS, null, out foundElement, snapshot, currentLine, dialect);
                //    if (foundElement != null)
                //    {
                //        xVar.TypeName = GetTypeFromFoundElement(foundElement);
                //    }
                //}
                return;
            }
            //Todo: Type Lookup Var
            //expr = BuildTokenString(tokens, 0);
            //tokenList = new List<string> { expr };
            //cType = RetrieveType(xVar.File, tokenList, member, currentNS, null, out foundElement, snapshot, currentLine, dialect);
            //if (cType != null)
            //{
            //    xVar.TypeName = cType.FullName;
            //}
            return;
        }

        private static string BuildTokenString(IList<IToken> tokens, int start = 0)
        {
            var sb = new StringBuilder();
            bool left = false, right = false;
            int nested = 0;
            bool done = false;
            int leftToken = 0;
            int rightToken = 0;
            string leftStr = "";
            string rightStr = "";
            for (int i = start; i < tokens.Count && !done; i++)
            {
                var t = tokens[i];
                switch (t.Type)
                {
                    case XSharpLexer.LPAREN:
                        leftToken = t.Type;
                        rightToken = XSharpLexer.RPAREN;
                        leftStr = "(";
                        rightStr = ")";
                        break;
                    case XSharpLexer.LCURLY:
                        leftToken = t.Type;
                        rightToken = XSharpLexer.RCURLY;
                        leftStr = "{";
                        rightStr = "}";
                        break;
                    case XSharpLexer.LBRKT:
                        leftToken = t.Type;
                        rightToken = XSharpLexer.RBRKT;
                        leftStr = "[";
                        rightStr = "]";
                        break;
                }
                if (leftToken != 0)
                    break;
            }
            if (leftToken == 0 || rightToken == 0)
                return "";
            for (int i = start; i < tokens.Count && !done; i++)
            {
                var t = tokens[i];
                if (t.Type == leftToken)
                {
                    left = true;
                    nested++;
                }
                else if (t.Type == rightToken)
                {
                    right = true;
                    nested--;
                    if (nested == 0)
                    {
                        done = true;
                    }
                }
                else
                {
                    if (!left)
                    {
                        sb.Append(t.Text);
                    }
                }
            }
            if (left && right)
            {
                return sb.ToString() + leftStr + rightStr;
            }
            return "";
        }
        /// <summary>
        /// Retrieve the CompletionType based on :
        ///  The Token list returned by GetTokenList()
        ///  The Token that stops the building of the Token List.
        /// </summary>
        /// <param name="file"></param>
        /// <param name="tokenList"></param>
        /// <param name="currentMember"></param>
        /// <param name="currentNS"></param>
        /// <param name="stopToken"></param>
        /// <param name="foundElement"></param>
        /// <returns></returns>
        public static CompletionType RetrieveType(XFile file, List<XSharpToken> tokenList, XSourceMemberSymbol currentMember, string currentNS,
            IToken stopToken, out CompletionElement foundElement, ITextSnapshot snapshot, int currentLine, XSharpDialect dialect, bool stopAtOpenToken = false)
        {
            //
#if TRACE
                Stopwatch stopWatch = new Stopwatch();
                stopWatch.Start();
#endif
            foundElement = null;
            CompletionType cType = null;

            int currentPos = 0;
            var startOfExpression = true;
            var findConstructor = true;
            XSharpToken currentToken = null;
            var currentScopes = new List<string>();
            IXSymbol scope;
            scope = currentMember;
            //Todo
            while (scope != null)
            {
                string ns = "";
                if (scope is XSourceTypeSymbol && !XSourceTypeSymbol.IsGlobalType(scope))
                {
                    ns = scope.FullName;
                }
                if (scope.Kind == Kind.Namespace)
                {
                    ns = scope.FullName;
                }
                if (ns?.Length > 0)
                {
                    var elements = ns.Split(".".ToCharArray());
                    ns = "";
                    for (int i = 0; i < elements.Length; i++)
                    {
                        if (i > 0)
                            ns += "." + elements[i];
                        else
                            ns = elements[0];
                        if (!currentScopes.Contains(ns))
                        {
                            currentScopes.Add(ns);
                        }
                    }
                }
                scope = scope.Parent;

            }

            //
            if (currentMember == null)
            {
                // try to find the first member in the file
                if (file != null)
                {
                    var elt = file.FindMemberAtRow(currentLine);
                    if (elt is XSourceMemberSymbol)
                    {
                        currentMember = (XSourceMemberSymbol)elt;
                    }
                    else if (elt is XSourceTypeSymbol && stopToken != null)
                    {
                        // We might be in the Class Declaration !?
                        switch (stopToken.Type)
                        {
                            case XSharpLexer.IMPLEMENTS:
                            case XSharpLexer.INHERIT:
                                if (tokenList.Count == 1)
                                {
                                    currentToken = tokenList[currentPos];
                                    cType = new CompletionType(currentToken.Text, file, ((XSourceTypeSymbol)(elt)).Namespace);
                                }
                                break;
                            default:
                                cType = new CompletionType(elt.Name, file, file.Usings);
                                break;
                        }
                        if (!cType.IsEmpty())
                        {
                            SearchConstructorIn(cType, Modifiers.Private, out foundElement);
                            if (foundElement?.Result == null && cType.XTypeDef != null)
                            {
                                foundElement = new CompletionElement(cType.XTypeDef);
                            }
                            return cType;
                        }
                    }
                }
                //
                if (currentMember == null)
                {
#if TRACE
                    stopWatch.Stop();
                    WriteOutputMessage(string.Format("Retrieve current Type : Member cannot be null."));
#endif
                    return null;
                }
            }
            else
            {
                file = currentMember.File;
            }

            //
            // we have to walk the tokenList, searching for the current Type
            // As we have separators every even token, we will walk by step 2
            CompletionType cTemp = null;
            if (tokenList.Count == 0)
                return null;
            // Context Type....
            cType = new CompletionType((currentMember.ParentType as XSourceTypeSymbol).Clone);
            Modifiers visibility = Modifiers.Private;
            int lastopentoken = tokenList.Count - 1;
            if (stopAtOpenToken)
            {
                for (int i = 0; i < tokenList.Count; i++)
                {
                    var token = tokenList[i];
                    switch (token.Type)
                    {
                        case XSharpLexer.LPAREN:
                        case XSharpLexer.LCURLY:
                        case XSharpLexer.LBRKT:
                            lastopentoken = i;
                            break;
                    }
                }
            }

            while (currentPos <= lastopentoken)
            {
                currentToken = tokenList[currentPos];
                var currentName = currentToken.Text;
                var lastToken = currentToken;
                switch (currentToken.Type)
                {
                    case XSharpLexer.LPAREN:
                    case XSharpLexer.LCURLY:
                    case XSharpLexer.LBRKT:
                        currentPos += 1;
                        continue;
                }

                var qualifiedName = false;
                var findType = false;
                var findMethod = false;
                var hasBracket = false;
                if (currentPos < lastopentoken)
                {
                    var nextType    = tokenList[currentPos + 1].Type;
                    qualifiedName   = nextType == XSharpLexer.DOT;
                    findMethod      = nextType == XSharpLexer.LPAREN;
                    findType        = nextType == XSharpLexer.LCURLY;
                    hasBracket      = nextType == XSharpLexer.LBRKT;

                }

                if (stopToken != null)
                {
                    switch (stopToken.Type)
                    {
                        case XSharpLexer.AS:
                        case XSharpLexer.IS:
                        case XSharpLexer.REF:
                        case XSharpLexer.OUT:
                        case XSharpLexer.ASTYPE:
                        case XSharpLexer.IMPLEMENTS:
                        case XSharpLexer.INHERIT:
                        case XSharpLexer.OF:
                        case XSharpLexer.SHARING:
                        case XSharpLexer.FROM:
                            if (tokenList.Count == 1)
                            {
                                findType = true;
                                findConstructor = false;
                            }
                            break;
                        default:
                            break;
                    }
                }
                //
                if (findType)
                {
                    // Look for a type

                    SearchType(currentMember.File, currentName, out foundElement, currentMember.Parent.Namespace);


                    cType = new CompletionType(currentName, currentMember.File, currentMember.Parent.Namespace);
                    if (!cType.IsEmpty())
                    {
                        foundElement = new CompletionElement(cType.Type);
                        if (findConstructor)
                        {
                            SearchConstructorIn(cType, visibility, out foundElement);
                        }
                        if (foundElement == null)
                        {
                            foundElement = new CompletionElement(cType.XTypeDef);
                        }
                        else if (foundElement.Result == null && cType.XTypeDef != null)
                        {
                            foundElement = new CompletionElement(cType.XTypeDef);
                        }

                        if ((foundElement != null) && (foundElement.IsGeneric))
                        {
                            if (string.IsNullOrEmpty(foundElement.GenericTypeName))
                            {
                                if (currentName.EndsWith(">"))
                                {
                                    string genName = currentName;
                                    int index = genName.IndexOf('<');
                                    if (index != -1)
                                    {
                                        // Extract the Generic params
                                        genName = genName.Substring(index + 1);
                                        genName = genName.Substring(0, genName.Length - 1);
                                        foundElement.GenericTypeName = genName;
                                    }
                                }
                            }
                        }
                    }
                }
                else if (findMethod)
                {
                    // this a Method call

                    // Do we already know in which Type we are ?
                    if (currentName.ToLower() == "self")
                    {
                        SearchConstructorIn(cType, visibility, out foundElement);
                    }
                    else if (currentName.ToLower() == "super")
                    {
                        SearchConstructorIn(cType.ParentType, visibility, out foundElement);
                    }
                    // The first token in the list can be a Function or a Procedure
                    // Except if we already have a Type
                    if (currentPos == 0 || startOfExpression)
                    {
                        var globType = SearchFunctionIn(currentMember.File, currentName, out foundElement);
                        if (currentPos == lastopentoken && globType != null)
                            return globType;
                    }
                    if (!cType.IsEmpty())
                    {
                        // Now, search for a Method
                        cTemp = SearchMethodTypeIn(cType, currentName, visibility, false, out foundElement, dialect);
                        if ((foundElement != null) && (foundElement.IsInitialized))
                        {
                            cType = cTemp;
                        }
                        else
                        {
                            cType = new CompletionType();
                        }
                    }
                    if (cType.IsEmpty())
                    {
                        // check to see if this is a method from the Object Type, such as ToString().
                        cTemp = SearchMethodTypeIn(new CompletionType("System.Object", file, file.Usings), currentName, visibility, false, out foundElement, dialect);
                        if ((foundElement != null) && (foundElement.IsInitialized))
                        {
                            cType = cTemp;
                        }
                    }
                    if (cType.IsEmpty())
                    {
                        // Could it be Static Method with "Using Static"
                        // Now, search for a Method
                        cType = SearchMethodStaticIn(currentMember.File, currentName, out foundElement, dialect);
                    }
                    if (cType.IsEmpty())
                    {
                        cType = null;
                    }
                }
                else
                {
                    var literal = XSharpLexer.IsConstant(currentToken.Type);
                    if (literal)
                    {
                        cType = getConstantType(currentToken.Type, file);
                    }
                    else if (startOfExpression)
                    {
                        // Search in Parameters, Locals, Field and Properties
                        foundElement = FindIdentifier(currentMember, currentName, ref cType, Modifiers.Private, currentNS, snapshot, currentLine, dialect);
                        if ((foundElement != null) && (foundElement.IsInitialized))
                        {
                            cType = foundElement.ReturnType;
                        }
                    }
                    else
                    {
                        // We can have a Property/Field of the current CompletionType
                        if (!cType.IsEmpty())
                        {
                            cType = SearchPropertyOrFieldIn(cType, currentName, visibility, out foundElement);
                        }
                    }
                    if (!literal)
                    {
                        if (foundElement == null)
                        {
                            cType = SearchType(file, currentName, out foundElement, currentNS);
                        }
                        // We have it
                        if ((foundElement != null) && (foundElement.IsInitialized))
                        {
                            // and we are in an Array, so we need the "other" type
                            if (hasBracket)
                            {
                                if (foundElement.IsGeneric)
                                {
                                    // Retrieve the inner Type
                                    if (foundElement.Result != null)
                                    {
                                        if (!string.IsNullOrEmpty(foundElement.GenericTypeName))
                                        {
                                            var usings = new List<string>(currentScopes);
                                            usings.AddRange(file.Usings);
                                            if (!string.IsNullOrEmpty(currentNS) && !usings.Contains(currentNS))
                                            {
                                                usings.Add(currentNS);
                                            }
                                            if (foundElement.GenericTypeName.Contains(','))
                                            {
                                                // Ok, this is might be wrong, but...
                                                string[] items = foundElement.GenericTypeName.Split(',');
                                                if (items.Length > 1)
                                                    cType = new CompletionType(items[1], file, usings);
                                            }
                                            else
                                                cType = new CompletionType(foundElement.GenericTypeName, file, usings);
                                        }
                                    }
                                }
                                else if (foundElement.IsArray)
                                {
                                    // Retrieve the inner Type
                                    if (foundElement.Result != null)
                                    {
                                        cType = foundElement.ReturnType;
                                    }
                                }
                            }
                            else if (foundElement.IsArray)
                            {
                                cType = new CompletionType("System.Array", file, "");
                            }
                        }
                    }
                }
                if (cType.IsEmpty())
                {

                    cType = null;
                }
                // Next Token
                currentPos += 1;
                if (currentPos >= tokenList.Count)
                {
                    break;
                }
                currentToken = tokenList[currentPos];
                switch (currentToken.Type)
                {
                    case XSharpLexer.DOT:
                    case XSharpLexer.COLON:
                    case XSharpLexer.COLONCOLON:
                        currentPos += 1;
                        break;
                }
                switch (lastToken.Type)
                {
                    case XSharpLexer.LPAREN:
                    case XSharpLexer.LCURLY:
                    case XSharpLexer.LBRKT:
                    case XSharpLexer.COMMA:
                    case XSharpLexer.PLUS:
                    case XSharpLexer.MINUS:
                    case XSharpLexer.MULT:
                    case XSharpLexer.DIV:
                    case XSharpLexer.EQ:
                    case XSharpLexer.LT:
                    case XSharpLexer.GT:
                        startOfExpression = true;
                        break;
                    case XSharpLexer.COLON:
                    case XSharpLexer.DOT:
                    default:
                        startOfExpression = false;
                        break;
                }
                //
                visibility = Modifiers.Public;
                if ((foundElement != null) && (foundElement.IsInitialized))
                {
                    if (string.Compare(foundElement.Name, "self", true) == 0)
                    {
                        visibility = Modifiers.Private;
                    }
                    else if (string.Compare(foundElement.Name, "super", true) == 0)
                    {
                        visibility = Modifiers.Protected;
                    }
                }
            }
            if (cType != null && foundElement == null && cType.XTypeDef != null)
            {
                foundElement = new CompletionElement(cType.XTypeDef);
            }
#if TRACE
                //
                stopWatch.Stop();
                // Get the elapsed time as a TimeSpan value.
                TimeSpan ts = stopWatch.Elapsed;
                // Format and display the TimeSpan value.
                string elapsedTime = string.Format("{0:00}h {1:00}m {2:00}.{3:00}s",
                    ts.Hours, ts.Minutes, ts.Seconds,
                    ts.Milliseconds / 10);
            //
            WriteOutputMessage("XSharpTokenTools::RetrieveType : Done in " + elapsedTime);
#endif

            return cType;
        }

        /// <summary>
        /// Search for the Constructor in the corresponding Type,
        /// no return value, the constructor is returned by foundElement
        /// </summary>
        /// <param name="cType"></param>
        /// <param name="minVisibility"></param>
        /// <param name="foundElement"></param>
        private static void SearchConstructorIn(CompletionType cType, Modifiers minVisibility, out CompletionElement foundElement)
        {
            WriteOutputMessage($"--> SearchConstructorIn {cType?.FullName}");
            foundElement = null;
            if (cType.Type != null)
            {
                //
                var xMethod = cType.Type.Members.Where(x => x.Kind == Kind.Constructor).FirstOrDefault();
                if ((xMethod != null) && (xMethod.Visibility < minVisibility))
                {
                    xMethod = null;
                }
                if (xMethod != null)
                {
                    foundElement = new CompletionElement(xMethod);
                    return;
                }
            }
            return;
        }

        internal static CompletionType getConstantType(int type, XFile file)
        {
            CompletionType cType = null;
            switch (type)
            {
                case XSharpLexer.FALSE_CONST:
                case XSharpLexer.TRUE_CONST:
                    cType = new CompletionType("System.Boolean", file, "");
                    break;
                case XSharpLexer.HEX_CONST:
                case XSharpLexer.BIN_CONST:
                case XSharpLexer.INT_CONST:
                    cType = new CompletionType("System.Int32", file, "");
                    break;
                case XSharpLexer.DATE_CONST:
                case XSharpLexer.NULL_DATE:
                    cType = new CompletionType("__Date", file, "");
                    break;
                case XSharpLexer.DATETIME_CONST:
                    cType = new CompletionType("System.DateTime", file, "");
                    break;
                case XSharpLexer.REAL_CONST:
                    cType = new CompletionType("System.Double", file, "");
                    break;
                case XSharpLexer.SYMBOL_CONST:
                case XSharpLexer.NULL_SYMBOL:
                    cType = new CompletionType("__Symbol", file, "");
                    break;
                case XSharpLexer.CHAR_CONST:
                    cType = new CompletionType("System.Char", file, "");
                    break;
                case XSharpLexer.STRING_CONST:
                case XSharpLexer.ESCAPED_STRING_CONST:
                case XSharpLexer.INTERPOLATED_STRING_CONST:
                case XSharpLexer.INCOMPLETE_STRING_CONST:
                case XSharpLexer.TEXT_STRING_CONST:
                case XSharpLexer.BRACKETED_STRING_CONST:
                case XSharpLexer.NULL_STRING:
                case XSharpLexer.MACRO:
                    cType = new CompletionType("System.String", file, "");
                    break;
                case XSharpLexer.BINARY_CONST:
                    cType = new CompletionType("__Binary", file, "");
                    break;

                case XSharpLexer.NULL_ARRAY:
                    cType = new CompletionType("__Array", file, "");
                    break;
                case XSharpLexer.NULL_CODEBLOCK:
                    cType = new CompletionType("CodeBlock", file, "");
                    break;
                case XSharpLexer.NULL_PSZ:
                    cType = new CompletionType("__Psz", file, "");
                    break;
                case XSharpLexer.NULL_PTR:
                    cType = new CompletionType("System.Intptr", file, "");
                    break;
                case XSharpLexer.NULL_OBJECT:
                default:
                    cType = new CompletionType("System.Object", file, "");
                    break;
            }
            return cType;
        }

        /// <summary>
        /// Search for a Property or a Field, in a CompletionType, based on the Visibility.
        /// A Completion can have a XSourceTypeSymbol (XSharp parsed type) or a SType (A System type or a Type found inside a library Reference)
        /// </summary>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="currentToken">The Property we are searching</param>
        /// <param name="minVisibility"></param>
        /// <returns>The CompletionType of the Property (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// </returns>
        internal static CompletionType SearchPropertyOrFieldIn(CompletionType cType, string currentToken, Modifiers minVisibility, out CompletionElement foundElement)
        {
            CompletionType result = SearchFieldTypeIn(cType, currentToken, minVisibility, out foundElement);
            if (result.IsEmpty())
            {
                result = SearchPropertyTypeIn(cType, currentToken, minVisibility, out foundElement);
            }
            if (result.IsEmpty())
            {
                result = SearchEventTypeIn(cType, currentToken, minVisibility, out foundElement);
            }

            return result;
        }


        static public CompletionType SearchType(XFile xFile, string currentToken, out CompletionElement foundElement, string currentNs)
        {
            foundElement = null;
            CompletionType cType = null;
            WriteOutputMessage($"SearchType in file {xFile.SourcePath} {currentToken}");
            // check for a system type
            var usings = xFile.Usings.ToList();
            if (!string.IsNullOrEmpty(currentNs))
                usings.Add(currentNs);
            // Try to check Check System Types
            cType = new CompletionType(currentToken, xFile, currentNs);
            if (cType.XTypeRef != null)
            {
                foundElement = new CompletionElement(cType.XTypeRef);
                if (foundElement.IsGeneric)
                {
                    // We may need to adapt...
                    string searchTypeName = currentToken;
                    int genMarker = searchTypeName.IndexOf("<");
                    if (genMarker > -1)
                    {
                        searchTypeName = searchTypeName.Substring(genMarker + 1);
                        searchTypeName = searchTypeName.Substring(0, searchTypeName.Length - 1);
                        foundElement.GenericTypeName = searchTypeName;
                    }
                }
            }
            else if (cType.XTypeDef != null)
            {
                foundElement = new CompletionElement(cType.XTypeDef);
            }
            if (cType.IsEmpty())
            {
                usings.Add(currentNs);
                var type = xFile.Project.Lookup(currentToken, usings);
                if (type != null)
                {
                    cType = new CompletionType(type);
                    foundElement = new CompletionElement(type);
                }
            }
            return cType;
        }

        /// <summary>
        /// Search for a Property, in a CompletionType, based on the Visibility.
        /// A Completion can have a XSourceTypeSymbol (XSharp parsed type) or a SType (A System type or a Type found inside a library Reference)
        /// </summary>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="currentToken">The Property we are searching</param>
        /// <param name="minVisibility"></param>
        /// <returns>The CompletionType of the Property (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// </returns>
        private static CompletionType SearchPropertyTypeIn(CompletionType cType, string currentToken, Modifiers minVisibility, out CompletionElement foundElement)
        {
            WriteOutputMessage($" SearchPropertyTypeIn {cType.FullName} , {currentToken}");
            foundElement = null;
            if (cType.Type != null)
            {

                IXMemberSymbol property = cType.Type.GetProperties(currentToken).FirstOrDefault();
                //
                if ((property != null) && (property.Visibility < minVisibility))
                {
                    property = null;
                }
                //
                if (property == null)
                {
                    if (!string.IsNullOrEmpty(cType.BaseType) && cType.File != null)
                    {
                        // Parent has just a Name, so one of the System Types
                        return SearchPropertyTypeIn(new CompletionType(cType.BaseType, cType.File, cType.File.Usings), currentToken, Modifiers.Public, out foundElement);
                    }
                }
                else
                {
                    foundElement = new CompletionElement(property);
                    return foundElement.ReturnType;
                }
            }
            // Sorry, not found 
            return new CompletionType();

        }


        private static CompletionType SearchEventTypeIn(CompletionType cType, string currentToken, Modifiers minVisibility, out CompletionElement foundElement)
        {
            WriteOutputMessage($" SearchEventTypeIn {cType.FullName} , {currentToken}");
            foundElement = null;
            if (cType.Type != null)
            {

                IXMemberSymbol evt = cType.Type.GetEvents().Where(e => String.Compare(e.Name, currentToken, true) == 0).FirstOrDefault();
                //
                if ((evt != null) && (evt.Visibility < minVisibility))
                {
                    evt = null;
                }
                //
                if (evt == null)
                {
                    if (!string.IsNullOrEmpty(cType.BaseType) && cType.File != null)
                    {
                        // Parent has just a Name, so one of the System Types
                        return SearchEventTypeIn(new CompletionType(cType.BaseType, cType.File, cType.File.Usings), currentToken, Modifiers.Public, out foundElement);
                    }
                }
                else
                {
                    foundElement = new CompletionElement(evt);
                    return foundElement.ReturnType;
                }
            }
            // Sorry, not found
            return new CompletionType();

        }
        /// <summary>
        /// Search for a Field, in a CompletionType, based on the Visibility.
        /// A Completion can have a XSourceTypeSymbol (XSharp parsed type) or a SType (A System type or a Type found inside a library Reference)
        /// </summary>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="currentToken">The Field we are searching</param>
        /// <param name="minVisibility"></param>
        /// <returns>The CompletionType of the Field (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// </returns>
        private static CompletionType SearchFieldTypeIn(CompletionType cType, string currentToken, Modifiers minVisibility, out CompletionElement foundElement)
        {
            WriteOutputMessage($" SearchFieldTypeIn {cType.FullName} , {currentToken}");
            foundElement = null;
            if (cType.Type != null)
            {
                IXMemberSymbol field = cType.Type.GetFields(currentToken).FirstOrDefault();
                if ((field != null) && (field.Visibility < minVisibility))
                {
                    field = null;
                }
                if (field == null)
                {
                    if (!string.IsNullOrEmpty(cType.BaseType) && cType.File != null)
                    {
                        // Search in base type
                        return SearchFieldTypeIn(new CompletionType(cType.BaseType, cType.File, cType.File.Usings), currentToken, Modifiers.Protected, out foundElement);
                    }
                }
                else
                {
                    foundElement = new CompletionElement(field);
                    return foundElement.ReturnType;
                }
            }

            // Sorry, not found
            return new CompletionType();
        }
        /// <summary>
        /// Search for a Method, in a CompletionType, based on the Visibility.
        /// </summary>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="currentToken">The Method we are searching</param>
        /// <param name="minVisibility"></param>
        /// <returns>The CompletionType that the Method will return (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// and FoundElement is null
        /// </returns>
        internal static CompletionType SearchMethodTypeIn(CompletionType cType, string currentToken, Modifiers minVisibility, bool staticOnly, out CompletionElement foundElement, XSharpDialect dialect)
        {
            WriteOutputMessage($" SearchMethodTypeIn {cType.FullName} , {currentToken}");
            foundElement = null;
            if (cType.Type != null)
            {
                IXMemberSymbol xMethod = cType.Type.GetMembers(currentToken, true).Where(x => x.Kind.IsClassMethod(dialect)).FirstOrDefault();
                if ((xMethod != null) && staticOnly && !xMethod.IsStatic)
                {
                    xMethod = null;
                }
                if ((xMethod != null) && (xMethod.Visibility < minVisibility))
                {
                    xMethod = null;
                }
                if (xMethod == null)
                {
                    if (!string.IsNullOrEmpty(cType.BaseType) && cType.File != null)
                    {
                        cType.Type.ForceComplete();
                        //
                        if (minVisibility == Modifiers.Private)
                            minVisibility = Modifiers.Protected;
                        return SearchMethodTypeIn(new CompletionType(cType.BaseType, cType.File, cType.File.Usings), currentToken, minVisibility, staticOnly, out foundElement, dialect);
                    }
                }
                else
                {
                    foundElement = new CompletionElement(xMethod);
                    return foundElement.ReturnType;
                }
            }

            // Sorry, not found
            return new CompletionType();
        }
        /// <summary>
        /// Search for a static Method in a File
        /// </summary>
        /// <param name="xFile">The File to search in</param>
        /// <param name="currentToken">The Toekn to look after</param>
        /// <param name="foundElement">The Found Element</param>
        /// <returns>The CompletionType that contains the Element</returns>
        private static CompletionType SearchMethodStaticIn(XFile xFile, string currentToken, out CompletionElement foundElement, XSharpDialect dialect)
        {
            WriteOutputMessage($" SearchMethodStaticIn {xFile.SourcePath}, {currentToken} ");
            foundElement = null;
            if (xFile == null)
            {
                return null;
            }
            //
            CompletionType cType = null;
            List<string> emptyUsings = new List<string>();
            foreach (string staticUsing in xFile.AllUsingStatics)
            {
                // Provide an Empty Using list, so we are looking for FullyQualified-name only
                CompletionType tempType = new CompletionType(staticUsing, xFile, emptyUsings);
                //
                cType = SearchMethodTypeIn(tempType, currentToken, Modifiers.Public, true, out foundElement, dialect);
                if (foundElement != null)
                {
                }
                if (!cType.IsEmpty())
                    break;
            }
            //
            return cType;
        }

        private static CompletionType SearchGlobalFieldIn(XFile xFile, string currentToken, out CompletionElement foundElement)
        {
            WriteOutputMessage($" SearchGlobalFieldIn {xFile.SourcePath}, {currentToken} ");
            foundElement = null;
            if (xFile == null)
            {
                return null;
            }
            if (xFile.Project == null)
            {
                return null;
            }
            if (xFile.Project.AssemblyReferences == null)
            {
                return null;
            }
            //
            var global = xFile.Project.FindGlobalOrDefine(currentToken);
            if (global != null)
            {
                foundElement = new CompletionElement(global);
                return new CompletionType(global.ParentType);
            }
            CompletionType cType = null;
            List<string> emptyUsings = new List<string>();
            var found = xFile.Project.FindGlobalMembersInAssemblyReferences(currentToken).Where(m => m.Kind.IsField()).ToArray();
            string declType = null;
            IXMemberSymbol field = null;
            foreach (var member in found)
            {
                if (StringEquals(member.Name, currentToken))
                {
                    field = member;
                    declType = field.TypeName;
                    break;
                }
            }
            if (declType != null)
            {
                foundElement = new CompletionElement(field);
                return new CompletionType(declType, xFile, xFile.Usings);
            }
            return cType;
        }

        private static CompletionType SearchFunctionIn(XFile xFile, string currentToken, out CompletionElement foundElement)
        {
            WriteOutputMessage($" SearchFunctionIn {xFile.SourcePath}, {currentToken} ");

            foundElement = null;
            if (xFile == null)
            {
                return null;
            }
            if (xFile.Project == null)
            {
                return null;
            }
            //
            CompletionType cType = null;
            //
            IXMemberSymbol xMethod = xFile.Project.FindFunction(currentToken);
            //
            if (xMethod == null)
            {
                var found = xFile.Project.FindGlobalMembersInAssemblyReferences(currentToken).Where(m => m.Kind.IsMethod()).ToArray();
                if (found.Length > 1)
                {
                    foreach (var m in found)
                    {
                        // Find an exact match first
                        if (string.Compare(m.Name, currentToken, true) == 0)
                        {
                            foundElement = new CompletionElement(m);
                            return new CompletionType(m.ParentType);

                        }
                    }
                }
                if (found.Length > 0)
                {
                    // no exact match, return the first one
                    foundElement = new CompletionElement(found[0]);
                    return new CompletionType(found[0].ParentType);
                }
            }

            foundElement = new CompletionElement(xMethod);
            if (xMethod?.Parent != null)
            {
                // Parent is a XElement, so one of our Types
                return new CompletionType((xMethod.ParentType as XSourceTypeSymbol).Clone);
            }
            //
            return cType;
        }

        private static CompletionType SearchGlobalOrDefineIn(XFile xFile, string currentToken, out CompletionElement foundElement)
        {
            WriteOutputMessage($" SearchGlobalOrDefineIn {xFile.SourcePath}, {currentToken} ");

            foundElement = null;
            if (xFile == null)
            {
                return null;
            }
            if (xFile.Project == null)
            {
                return null;
            }
            //
            CompletionType cType = null;
            //
            IXMemberSymbol xMethod = xFile.Project.FindGlobalOrDefine(currentToken);
            //
            if (xMethod == null)
            {
                if (xMethod == null)
                {
                    var found = xFile.Project.FindGlobalMembersInAssemblyReferences(currentToken).Where(m => m.Kind.IsField()).ToArray();
                    if (found.Length > 1)
                    {
                        foreach (var m in found)
                        {
                            // Find an exact match first
                            if (string.Compare(m.Name, currentToken, true) == 0)
                            {
                                foundElement = new CompletionElement(m);
                                return new CompletionType(m.ParentType);

                            }
                        }
                    }
                    if (found.Length > 0)
                    {
                        // no exact match, return the first one
                        foundElement = new CompletionElement(found[0]);
                        return new CompletionType(found[0].ParentType);
                    }
                }
            }
            foundElement = new CompletionElement(xMethod);
            if (xMethod?.Parent != null)
            {
                // Parent is a XElement, so one of our Types
                return new CompletionType((xMethod.Parent as XSourceTypeSymbol).Clone);
            }
            //
            return cType;
        }
        static void WriteOutputMessage(string message)
        {
            if (XSettings.EnableCodeCompletionLog)
            {
                XSettings.DisplayOutputMessage("XSharp.Codecompletion :" + message);
            }
        }
    }


}
