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
using System.Windows.Navigation;
using System.Diagnostics;

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
        static public IEnumerable<IXSymbol> FindIdentifier(XSharpSearchLocation location, string name, IXTypeSymbol currentType, Modifiers visibility)
        {
            var result = new List<IXSymbol>();
            if (nestedSearches.Contains(name, StringComparer.OrdinalIgnoreCase))
            {
                return null;
            }
            int nestedLevel = nestedSearches.Count();
            try
            {
                nestedSearches.Add(name);
                if (currentType == null)
                {
                    currentType = location.Member.ParentType;
                }
                WriteOutputMessage($"--> FindIdentifier in {currentType.FullName}, {name} ");
                result.AddRange(location.Member.Parameters.Where(x => StringEquals(x.Name, name)));
                if (result.Count == 0)
                {
                    // then Locals
                    // line numbers in the range are 1 based. currentLine = 0 based !
                    var local = location.Member.GetLocals(location).Where(x => StringEquals(x.Name, name) && x.Range.StartLine - 1 <= location.LineNumber).LastOrDefault();
                    if (local != null)
                    {
                        result.Add(local);
                    }
                    else
                    {
                        // We can have a Property/Field of the current CompletionType
                        if (currentType != null)
                        {
                            result.AddRange(SearchPropertyOrField(location, currentType, name, visibility));
                        }
                        // Find Defines and globals in this file
                        if (result.Count == 0 && location.File.GlobalType != null)
                        {
                            result.AddRange(location.File.GlobalType.GetMembers(name, true));
                        }
                        if (result.Count == 0)
                        {
                            var type = location.File.Project.Lookup(XSharpModel.XLiterals.GlobalName);
                            if (type != null)
                            {
                                result.AddRange(type.GetMembers(name, true));
                            }
                        }
                        if (result.Count == 0)
                        {
                            // Now, search for a Global in external Assemblies
                            //
                            result.AddRange(SearchGlobalField(location, name));
                        }
                    }
                }
              if (result.Count > 0)
                {
                    var element = result[0];
                    if (element is XSourceImpliedVariableSymbol xVar)
                    {
                        var type = resolveImpliedType(location, xVar, currentType, visibility);
                        if (type != null)
                        {
                            xVar.TypeName = type.FullName;
                        }

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
            return result;
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
#if NOTUSED
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

#endif
        private static string getTypeNameFromSymbol( IXSymbol symbol)
        {
            if (symbol is IXTypeSymbol ts)
                return ts.FullName;
            if (symbol is IXMemberSymbol mem)
            {
                return mem.OriginalTypeName;
            }
            if (symbol is IXVariableSymbol)
            {
                return symbol.TypeName;
            }
            return null;
        }


        private static IXTypeSymbol getTypeFromSymbol(XSharpSearchLocation location, IXSymbol symbol)
        {
            if (symbol is IXTypeSymbol ts)
                return ts;
            var name = getTypeNameFromSymbol(symbol);
            return SearchType(location, name).FirstOrDefault();
        }


        private static IXTypeSymbol resolveImpliedLoop(XSharpSearchLocation location, XSourceImpliedVariableSymbol xVar, IXTypeSymbol currentType, Modifiers visibility)
        {
            Debug.Assert(xVar.ImpliedKind == ImpliedKind.LoopCounter);
            var start = new List<XSharpToken>();
            var end = new List<XSharpToken>();
            bool seento = false;
            foreach (var t in xVar.Expression)
            {
                if (t.Type == XSharpLexer.TO)
                    seento = true;
                else if (seento)
                    end.Add(t);
                else
                    start.Add(t);
            }
            if (start.Count > 0 && end.Count > 0)
            {
                if (start.Count == 1 && end.Count == 1)
                {
                    var startType = start[0].Type;
                    var endType = end[0].Type;
                    if (startType == XSharpLexer.ID)
                    {
                        var id = FindIdentifier(location, start[0].Text, currentType, visibility);
                        return getTypeFromSymbol(location, id.FirstOrDefault());
                    }
                    else if (endType == XSharpLexer.ID)
                    {
                        var id = FindIdentifier(location, end[0].Text, currentType, visibility);
                        return getTypeFromSymbol(location, id.FirstOrDefault());
                    }
                    else if (XSharpLexer.IsConstant(startType))
                    {
                        return getConstantType(start[0], location.File);
                    }
                    else if (XSharpLexer.IsConstant(endType))
                    {
                        return getConstantType(end[0], location.File);
                    }
                    //todo what else ?
                    return null;
                }
                if (end.Count > 1)  // prefer type of end over type of start
                {
                    var res = RetrieveElement(location, end, CompletionState.General);
                    return getTypeFromSymbol(location, res.FirstOrDefault());
                }
                // start must be > 1
                var result = RetrieveElement(location, start, CompletionState.General);
                return getTypeFromSymbol(location, result.FirstOrDefault());
            }
            return null;
        }
        private static IXTypeSymbol resolveImpliedCollection(XSharpSearchLocation location, XSourceImpliedVariableSymbol xVar, IXTypeSymbol currentType, Modifiers visibility)
        {
            var tokenList = xVar.Expression;
            var result = RetrieveElement(location, tokenList, CompletionState.General);
            var element = result.FirstOrDefault();
            var elementType = getTypeNameFromSymbol(element);
            var type = getTypeFromSymbol(location, element);
            if (type != null)
            {
                // detect if the type has an implementation of:
                // System.Collections.IEnumerator
                // Array
                //
                type.ForceComplete();
                var interfaces = type.Interfaces;
                bool hasEnumerator = false;
                foreach (var interf in interfaces)
                {
                    if (interf.EndsWith("IEnumerable", StringComparison.OrdinalIgnoreCase))
                    {
                        hasEnumerator = true;
                        break;
                    }
                    else if (interf.EndsWith("IEnumerable`1", StringComparison.OrdinalIgnoreCase))
                    {
                        hasEnumerator = true;
                        break;
                    }
                    else if (interf.EndsWith("IEnumerable<T>", StringComparison.OrdinalIgnoreCase))
                    {
                        hasEnumerator = true;
                        break;
                    }
                }
                if (hasEnumerator)
                {
                    if (type.IsGeneric)
                    {
                        if (elementType != null && elementType.IndexOf('<') > 0)
                        {
                            var elements = elementType.Split(new char[] { '<', '>' }, StringSplitOptions.RemoveEmptyEntries);
                            if (elements.Length == 2 ) //  // List<T>
                            {
                                type = SearchType(location, elements[1]).FirstOrDefault();
                            }
                            else // Dictionary <T,U> we need to determine the type of the enumerator and see which of the generic arguments is returned
                            {
                                type = null;
                            }
                        }
                        else
                        {
                            var member = type.GetMembers("GetEnumerator").FirstOrDefault();
                            var name = member.OriginalTypeName;
                            // translate name from definition to name used by variable
                            type = null;
                        }
                         
                    }
                    else
                    {
                        var member = type.GetMembers("GetEnumerator").FirstOrDefault();
                        type = SearchType(location, member.OriginalTypeName).FirstOrDefault();
                    }
                }
                else if (type.IsArray)
                {
                    type = null;
                }
                else
                {
                    type = null;
                }

            }
            return type;
        }
        private static IXTypeSymbol resolveImpliedOutParam(XSharpSearchLocation location, XSourceImpliedVariableSymbol xVar, IXTypeSymbol currentType, Modifiers visibility)
        {
            Debug.Assert(xVar.ImpliedKind == ImpliedKind.OutParam);
            return null;
        }
        private static IXTypeSymbol resolveImpliedAssign(XSharpSearchLocation location, XSourceImpliedVariableSymbol xVar, IXTypeSymbol currentType, Modifiers visibility)
        {
            Debug.Assert(xVar.ImpliedKind == ImpliedKind.Assignment);
            var tokenList = xVar.Expression;
            var result = RetrieveElement(location, tokenList, CompletionState.General);
            var element = result.FirstOrDefault();
            return getTypeFromSymbol(location, element);
        }
        private static IXTypeSymbol resolveImpliedType(XSharpSearchLocation location, XSourceImpliedVariableSymbol xVar, IXTypeSymbol currentType, Modifiers visibility)
        {
            // the following Implied Kinds are followed by a := and should return the type of the expression in the ExpressionList
            switch (xVar.ImpliedKind)
            {
                case ImpliedKind.Assignment:
                case ImpliedKind.Using:
                    return resolveImpliedAssign(location, xVar, currentType, visibility);
                case ImpliedKind.TypeCheck:
                    return SearchType(location, xVar.TypeName).FirstOrDefault();
                case ImpliedKind.InCollection:
                    return resolveImpliedCollection(location, xVar, currentType, visibility);
                case ImpliedKind.LoopCounter:
                    return resolveImpliedLoop(location, xVar, currentType, visibility);
                case ImpliedKind.OutParam:
                    return resolveImpliedOutParam(location, xVar, currentType, visibility);
                case ImpliedKind.None:
                    break;
            }
            return null;
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
        /// <param name="location"></param>
        /// <param name="tokenList"></param>
        /// <param name="state"></param>
        /// <param name="foundElement"></param>
        /// <param name="stopAtOpenToken"></param>
        /// <returns></returns>
        public static IEnumerable<IXSymbol> RetrieveElement(XSharpSearchLocation location, IList<XSharpToken> tokenList, CompletionState state, bool stopAtOpenToken = false)
        {
            //
#if TRACE
                Stopwatch stopWatch = new Stopwatch();
                stopWatch.Start();
#endif
            var result = new List<IXSymbol>();
            IXTypeSymbol currentType = null;
            int currentPos = 0;
            var startOfExpression = true;
            var findConstructor = true;
            XSharpToken currentToken = null;
            var currentScopes = new List<string>();
            IXSymbol scope;
            scope = location.Member;
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
            if (location.Member == null)
            {
                // try to find the first member in the file
                if (location.File != null)
                {
                    var elt = location.File.FindMemberAtRow(location.LineNumber);
                    if (elt is XSourceMemberSymbol xms)
                    {
                        location = location.With(xms);
                    }
                    else if (elt is XSourceTypeSymbol)
                    {
                        // We might be in the Class Declaration !?
                        if (state.HasFlag(CompletionState.Types))
                        {
                            if (tokenList.Count == 1)
                            {
                                currentToken = tokenList[currentPos];
                            }
                        }
                    }
                    //
                    if (location.Member == null)
                    {
#if TRACE
                    stopWatch.Stop();
                    WriteOutputMessage(string.Format("Retrieve current Type : Member cannot be null."));
#endif
                        return null;
                    }
                }
            }

            //
            if (tokenList.Count == 0)
                return result;
            // Context Type....
            if (location.Member.Kind.IsClassMember(location.Dialect))
            {
                currentType = location.Member.ParentType;
            }
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
            string preFix = "";
            while (currentPos <= lastopentoken)
            {
                if (result.Count > 0)
                {
                    var element = result[0];
                    if (element is IXMemberSymbol mem)
                    {
                        var typeName = mem.OriginalTypeName;
                        currentType = location.FindType(typeName);
                    }
                    else if (element is IXTypeSymbol type)
                    {
                        currentType = type;
                    }
                    else if (element is IXVariableSymbol var)
                    {
                        var typeName = var.TypeName;
                        currentType = location.FindType(typeName);
                    }
                }
                currentToken = tokenList[currentPos];
                var currentName = currentToken.Text;
                var lastToken = currentToken;
                switch (currentToken.Type)
                {
                    case XSharpLexer.LPAREN:
                    case XSharpLexer.LCURLY:
                    case XSharpLexer.LBRKT:
                    case XSharpLexer.RPAREN:
                    case XSharpLexer.RCURLY:
                    case XSharpLexer.RBRKT:
                        currentPos += 1;
                        continue;
                }
                result.Clear();
                if (currentToken.Type == XSharpLexer.ID &&
                    currentPos < lastopentoken &&
                    tokenList[currentPos + 1].Type == XSharpLexer.LT)
                {
                    currentPos += 1;
                    while (currentPos <= lastopentoken)
                    {
                        var nextToken = tokenList[currentPos];
                        currentName += nextToken.Text;
                        currentPos += 1;
                        if (nextToken.Type == XSharpLexer.GT)
                            break;
                    }
                }
                var qualifiedName = false;
                var findMethod = false;
                var hasBracket = false;
                var findType = false;
                if (currentPos < lastopentoken)
                {
                    var nextType = tokenList[currentPos + 1].Type;
                    qualifiedName = nextType == XSharpLexer.DOT;
                    findMethod = nextType == XSharpLexer.LPAREN;
                    findConstructor = nextType == XSharpLexer.LCURLY;
                    hasBracket = nextType == XSharpLexer.LBRKT;
                    findType = preFix.Length > 0;
                }

                if (state.HasFlag(CompletionState.Types))
                {
                    // Look for a type

                    result.AddRange(SearchType(location, currentName));
                    if (result.Count > 0 && result[0] is IXTypeSymbol type)
                    {
                        if (findConstructor)
                        {
                            result.Clear();
                            result.AddRange(SearchConstructor(type, visibility));
                        }
                    }
                }
                else if (findMethod)
                {
                    // this a Method call

                    // Do we already know in which Type we are ?
                    if (currentName.ToLower() == "self")
                    {
                        result.AddRange(SearchConstructor(currentType, visibility));
                    }
                    else if (currentName.ToLower() == "super")
                    {
                        if (currentType is XSourceTypeSymbol source)
                        {
                            var p = source.File.FindType(source.BaseType);
                            result.AddRange(SearchConstructor(p, visibility));
                        }
                        else
                        {
                            var p = location.FindType(currentType.BaseType);
                            result.AddRange(SearchConstructor(p, visibility));
                        }

                    }
                    // The first token in the list can be a Function or a Procedure
                    // Except if we already have a Type
                    if (currentPos == 0 || startOfExpression)
                    {
                        result.AddRange(SearchFunction(location, currentName));
                        result.AddRange(SearchMethodStatic(location, currentName));

                        if (result.Count == 0)
                        {
                            // Foo() could be a delegate call where Foo is a local or Field
                            result.AddRange(SearchDelegateCall(location, currentName, currentType, visibility));
                        }

                        if (currentPos == lastopentoken || currentPos == lastopentoken - 1)
                            return result;
                    }
                    if (currentType != null)
                    {
                        // Now, search for a Method. this will search the whole hierarchy
                        result.AddRange(SearchMethod(location, currentType, currentName, visibility, false));

                    }
                    if (result.Count == 0)
                    {
                        // Could it be Static Method with "Using Static"
                        result.AddRange(SearchMethodStatic(location, currentName));
                    }
                }
                else
                {
                    var literal = XSharpLexer.IsConstant(currentToken.Type);
                    if (literal)
                    {
                        currentType = getConstantType(currentToken, location.File);
                        if (currentType != null)
                        {
                            result.Add(currentType);
                        }
                    }
                    if (startOfExpression || findType)
                    {
                        result.AddRange(SearchType(location, preFix + currentName));
                    }
                    if (startOfExpression && result.Count == 0)
                    {
                        // Search in Parameters, Locals, Field and Properties
                        if (currentName == "::" || currentName.ToLower() == "this")
                            currentName = "SELF";


                        result.AddRange(FindIdentifier(location, currentName, currentType, Modifiers.Private));
                    }
                    else
                    {
                        result.AddRange(SearchPropertyOrField(location, currentType, currentName, visibility));
                    }
                    if (!literal)
                    {
                        // We have it
                        if (result.Count > 0)
                        {
                            // and we are in an Array, so we need the "other" type
                            /*
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
                                            usings.AddRange(location.File.Usings);
                                            if (!string.IsNullOrEmpty(location.CurrentNamespace) && !usings.Contains(location.CurrentNamespace))
                                            {
                                                usings.Add(location.CurrentNamespace);
                                            }
                                            if (foundElement.GenericTypeName.Contains(','))
                                            {
                                                // Ok, this is might be wrong, but...
                                                string[] items = foundElement.GenericTypeName.Split(',');
                                                if (items.Length > 1)
                                                    cType = new CompletionType(items[1], location.File, usings);
                                            }
                                            else
                                                cType = new CompletionType(foundElement.GenericTypeName, location.File, usings);
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
                                cType = new CompletionType("System.Array", location.File, "");
                            }
                            */
                        }
                    }
                }
                if (result.Count == 0 && (currentToken.Type == XSharpLexer.ID || XSharpLexer.IsKeyword(currentToken.Type)))
                {
                    preFix += currentToken.Text;
                }               // Next Token
                currentPos += 1;
                if (currentPos >= tokenList.Count)
                {
                    break;
                }
                currentToken = tokenList[currentPos];
                switch (currentToken.Type)
                {
                    case XSharpLexer.DOT:
                        preFix += currentToken.Text;
                        currentPos += 1;
                        break;

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
                //if ((foundElement != null) && (foundElement.IsInitialized))
                //{
                //    if (string.Compare(foundElement.Name, "self", true) == 0)
                //    {
                //        visibility = Modifiers.Private;
                //    }
                //    else if (string.Compare(foundElement.Name, "super", true) == 0)
                //    {
                //        visibility = Modifiers.Protected;
                //    }
                //}
            }
            if (result.Count == 0 && currentType != null)
                result.Add(currentType);
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
            return result;
        }

        /// <summary>
        /// Search for the Constructor in the corresponding Type,
        /// no return value, the constructor is returned by foundElement
        /// </summary>
        /// <param name="cType"></param>
        /// <param name="minVisibility"></param>
        /// <param name="foundElement"></param>
        private static IList<IXMemberSymbol> SearchConstructor(IXTypeSymbol type, Modifiers minVisibility)
        {
            WriteOutputMessage($"--> SearchConstructorIn {type?.FullName}");
            var result = new List<IXMemberSymbol>();
            if (type != null)
            {
                //
                var xMethod = type.Members.Where(x => x.Kind == Kind.Constructor).FirstOrDefault();
                if ((xMethod != null) && (xMethod.Visibility < minVisibility))
                {
                    xMethod = null;
                }
                if (xMethod != null)
                {
                    result.Add(xMethod);
                }
            }
            return result;
        }

        internal static IXTypeSymbol getConstantType(XSharpToken token, XFile file)
        {
            IXTypeSymbol result;
            var project = file.Project;
            var xusings = new string[] { "XSharp", "Vulcan" };
            var susings = new string[] { };
            switch (token.Type)
            {
                case XSharpLexer.FALSE_CONST:
                case XSharpLexer.TRUE_CONST:
                    result = project.FindSystemType("System.Boolean", susings);
                    break;
                case XSharpLexer.HEX_CONST:
                case XSharpLexer.BIN_CONST:
                case XSharpLexer.INT_CONST:
                    if (token.Text.ToUpper().EndsWith("L"))
                    {
                        result = project.FindSystemType("System.Int32", susings);
                    }
                    else if (token.Text.ToUpper().EndsWith("U"))
                    {
                        result = project.FindSystemType("System.UInt32", susings);
                    }
                    else
                    {
                        result = project.FindSystemType("System.Int32", susings);
                    }
                    break;
                case XSharpLexer.DATE_CONST:
                case XSharpLexer.NULL_DATE:
                    result = project.FindSystemType("__Date", xusings);
                    break;
                case XSharpLexer.DATETIME_CONST:
                    result = project.FindSystemType("System.DateTime", susings);
                    break;
                case XSharpLexer.REAL_CONST:
                    if (token.Text.ToUpper().EndsWith("M"))
                    {
                        result = project.FindSystemType("System.Decimal", susings);
                    }
                    else if (token.Text.ToUpper().EndsWith("S"))
                    {
                        result = project.FindSystemType("System.Single", susings);
                    }
                    else // if (token.Text.ToUpper().EndsWith("D"))
                    {
                        result = project.FindSystemType("System.Double", susings);
                    }
                    break;
                case XSharpLexer.SYMBOL_CONST:
                case XSharpLexer.NULL_SYMBOL:
                    result = project.FindSystemType("__Symbol", xusings);
                    break;
                case XSharpLexer.CHAR_CONST:
                    result = project.FindSystemType("System.Char", susings);
                    break;
                case XSharpLexer.STRING_CONST:
                case XSharpLexer.ESCAPED_STRING_CONST:
                case XSharpLexer.INTERPOLATED_STRING_CONST:
                case XSharpLexer.INCOMPLETE_STRING_CONST:
                case XSharpLexer.TEXT_STRING_CONST:
                case XSharpLexer.BRACKETED_STRING_CONST:
                case XSharpLexer.NULL_STRING:
                case XSharpLexer.MACRO:
                    result = project.FindSystemType("System.String", susings);
                    break;
                case XSharpLexer.BINARY_CONST:
                    result = project.FindSystemType("__Binary", xusings);
                    break;

                case XSharpLexer.NULL_ARRAY:
                    result = project.FindSystemType("__Array", xusings);
                    break;
                case XSharpLexer.NULL_CODEBLOCK:
                    result = project.FindSystemType("CodeBlock", xusings);
                    break;
                case XSharpLexer.NULL_PSZ:
                    result = project.FindSystemType("__Psz", xusings);
                    break;
                case XSharpLexer.NULL_PTR:
                    result = project.FindSystemType("System.IntPtr", susings);
                    break;
                case XSharpLexer.NULL_OBJECT:
                default:
                    result = project.FindSystemType("System.Object", susings);
                    break;
            }
            return result;
        }

        /// <summary>
        /// Search for a Property or a Field, in a CompletionType, based on the Visibility.
        /// A Completion can have a XSourceTypeSymbol (XSharp parsed type) or a SType (A System type or a Type found inside a library Reference)
        /// </summary>
        /// <param name="location"></param>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="name">The Property we are searching</param>
        /// <param name="minVisibility"></param>
        /// <param name="foundElement"></param>
        /// <returns>The CompletionType of the Property (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// </returns>
        internal static IEnumerable<IXMemberSymbol> SearchPropertyOrField(XSharpSearchLocation location, IXTypeSymbol type, string name, Modifiers minVisibility)
        {
            return SearchMembers(location, type, name, minVisibility).Where((m) => m.Kind.IsProperty() || m.Kind == Kind.Field || m.Kind == Kind.Event);
        }

        private static IEnumerable<IXSymbol> SearchDelegateCall(XSharpSearchLocation location, string currentName, IXTypeSymbol currentType, Modifiers visibility)
        {
            var result = new List<IXSymbol>();
            WriteOutputMessage($"SearchDelegateCall in file {location.File.SourcePath} {currentName}");

            result.AddRange(FindIdentifier(location, currentName, currentType, visibility));
            if (result.Count > 0)
            {
                var first = result[0];
                var name = first.TypeName;
                var type = location.FindType(name);
                result.Clear();
                if (type != null && type.Kind is Kind.Delegate)
                {
                    var temp = type.Members.Where((m) => m.Name == "Invoke");
                    var member = temp.FirstOrDefault();
                    if (member != null)
                    {
                        member = member.WithName(type.ShortName);
                        //if (first is XSourceEntity src && src.IsGeneric )
                        //{
                        //    member = member.WithGenericArgs(src.GenericArgs);
                        //}
                        result.Add(member);
                    }
                }
            }
            return result;
        }

        private static IEnumerable<IXMemberSymbol> SearchDelegate(XSharpSearchLocation location, string name)
        {
            WriteOutputMessage($"SearchDelegate in file {location.File.SourcePath} {name}");
            var result = new List<IXMemberSymbol>();
            var type = location.FindType(name);
            if (type != null && type.Kind == Kind.Delegate)
            {
                result.AddRange(type.Members);
            }
            return result;
        }

        private static IEnumerable<IXTypeSymbol> SearchType(XSharpSearchLocation location, string name)
        {
            WriteOutputMessage($"SearchType in file {location.File.SourcePath} {name}");
            var result = new List<IXTypeSymbol>();
            // translate out type names to system type names
            name = name.GetSystemTypeName(location.Project.ParseOptions.XSharpRuntime);

            var type = location.FindType(name);
            if (type != null)
                result.Add(type);
            return result;
        }

        private static IEnumerable<IXMemberSymbol> SearchMembers(XSharpSearchLocation location, IXTypeSymbol type, string name, Modifiers minVisibility)
        {
            var result = new List<IXMemberSymbol>();
            if (type != null)
            {
                WriteOutputMessage($" SearchMembers {type?.FullName} , {name}");
                result.AddRange(type.GetMembers(name, true).Where((m) => m.Visibility >= minVisibility));
                if (result.Count() == 0 && !string.IsNullOrEmpty(type.BaseType))
                {
                    if (minVisibility == Modifiers.Private)
                        minVisibility = Modifiers.Protected;
                    IXTypeSymbol baseType;
                    if (type is XSourceTypeSymbol sourceType)
                    {
                        baseType = sourceType.File.FindType(type.BaseType);
                    }
                    else
                    {
                        baseType = location.FindType(type.BaseType);
                    }
                    result.AddRange(SearchMembers(location, baseType, name, minVisibility));
                }
            }

            return result;
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
        internal static IEnumerable<IXMemberSymbol> SearchMethod(XSharpSearchLocation location, IXTypeSymbol type, string name, Modifiers minVisibility, bool staticOnly)
        {
            var result = new List<IXMemberSymbol>();
            if (type != null)
            {
                WriteOutputMessage($" SearchMethodTypeIn {type.FullName} , {name}");
                var tmp = type.GetMembers(name, true).Where(x => x.Kind.IsClassMethod(location.Dialect));
                foreach (var m in tmp)
                {
                    var add = true;
                    if (staticOnly && !m.IsStatic)
                        add = false;
                    if (add && m.Visibility < minVisibility)
                        add = false;
                    if (add)
                    {
                        result.Add(m);
                    }
                }

                if (result.Count == 0)
                {
                    if (!string.IsNullOrEmpty(type.BaseType))
                    {
                        if (minVisibility == Modifiers.Private)
                            minVisibility = Modifiers.Protected;
                        IXTypeSymbol baseType;
                        if (type is XSourceTypeSymbol sourceType)
                        {
                            baseType = sourceType.File.FindType(type.BaseType);
                        }
                        else
                        {
                            baseType = location.FindType(type.BaseType);
                        }
                        result.AddRange(SearchMethod(location, baseType, name, minVisibility, staticOnly));
                    }
                }
                if (result.Count == 0 && type.Interfaces.Count > 0)
                {
                    foreach (var ifname in type.Interfaces)
                    {
                        IXTypeSymbol baseType;
                        if (type is XSourceTypeSymbol sourceType)
                        {
                            baseType = sourceType.File.FindType(ifname);
                        }
                        else
                        {
                            baseType = location.FindType(ifname);
                        }
                        result.AddRange(SearchMethod(location, baseType, name, minVisibility, staticOnly));
                    }
                }
            }
            return result;

        }

        /// <summary>
        /// Search for a static Method in a File
        /// </summary>
        /// <param name="xFile">The File to search in</param>
        /// <param name="currentToken">The Toekn to look after</param>
        /// <param name="foundElement">The Found Element</param>
        /// <returns>The CompletionType that contains the Element</returns>
        private static IEnumerable<IXMemberSymbol> SearchMethodStatic(XSharpSearchLocation location, string name)
        {
            var result = new List<IXMemberSymbol>();
            if (location.File == null || location.Project == null)
            {
                return result;
            }
            WriteOutputMessage($" SearchMethodStaticIn {location.File.SourcePath}, {name} ");
            //
            var emptyusing = new string[] { };
            foreach (string staticUsing in location.File.AllUsingStatics)
            {
                // Provide an Empty Using list, so we are looking for FullyQualified-name only
                var temp = location.Project.FindType(staticUsing, emptyusing);
                //
                if (temp!= null)
                {
                    var found = SearchMethod(location, temp, name, Modifiers.Public, true);
                    result.AddRange(found);
                }
                if (result.Count > 0)
                {
                    break;
                }
            }
            return result;
        }

        private static IEnumerable<IXMemberSymbol> SearchGlobalField(XSharpSearchLocation location, string name)
        {
            var result = new List<IXMemberSymbol>();
            if (location.File == null || location.Project == null)
            {
                return result;
            }
            WriteOutputMessage($" SearchGlobalField {location.File.SourcePath}, {name} ");
            if (location.Project.AssemblyReferences == null)
            {
                return result;
            }
            //
            var global = location.Project.FindGlobalOrDefine(name);
            if (global != null)
            {
                result.Add(global);
            }
            else
            {
                List<string> emptyUsings = new List<string>();
                var found = location.Project.FindGlobalMembersInAssemblyReferences(name).Where(m => m.Kind.IsField()).ToArray();
                result.AddRange(found);
            }
            return result;
        }

        private static IEnumerable<IXMemberSymbol> SearchFunction(XSharpSearchLocation location, string name)
        {

            var result = new List<IXMemberSymbol>();
            if (location.File == null || location.Project == null)
            {
                return result;
            }
            WriteOutputMessage($" SearchFunction {location.File.SourcePath}, {name} ");
            IXMemberSymbol xMethod = location.File.Project.FindFunction(name);
            if (xMethod != null)
            {
                result.Add(xMethod);
            }
            else
            {
                var found = location.Project.FindGlobalMembersInAssemblyReferences(name).Where(m => m.Kind.IsMethod()).ToArray();
                result.AddRange(found);
            }
            return result;
        }

        private static IEnumerable<IXMemberSymbol> SearchGlobalOrDefineIn(XSharpSearchLocation location, string name)
        {

            var result = new List<IXMemberSymbol>();
            if (location.File == null || location.Project == null)
            {
                return result;
            }
            WriteOutputMessage($" SearchGlobalOrDefineIn {location.File.SourcePath}, {name} ");
            //
            //
            IXMemberSymbol xMethod = location.Project.FindGlobalOrDefine(name);
            //
            if (xMethod != null)
            {
                result.Add(xMethod);
            }
            else
            {
                var found = location.Project.FindGlobalMembersInAssemblyReferences(name).Where(m => m.Kind.IsField()).ToArray();
                result.AddRange(found);
            }
            //
            return result;
        }
        static void WriteOutputMessage(string message)
        {
            if (XSettings.EnableCodeCompletionLog)
            {
                XSettings.DisplayOutputMessage("XSharp.Lookup :" + message);
            }
        }
    }


}
