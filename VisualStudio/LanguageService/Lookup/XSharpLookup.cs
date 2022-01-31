﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using XSharpModel;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.VisualStudio.Shell.Interop;

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
        static readonly List<string> nestedSearches = new List<string>();
        static public IEnumerable<IXSymbol> FindIdentifier(XSharpSearchLocation location, string name, IXTypeSymbol currentType, Modifiers visibility)
        {
            if (name != null && name.EndsWith("."))
                name = name.Substring(0, name.Length - 1);

            var result = new List<IXSymbol>();
            if (nestedSearches.Contains(name, StringComparer.OrdinalIgnoreCase)
                || location?.Member == null)
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
                // we search in the order:
                // 1) Type parameters
                // 2) Parameters (for entities with parameters)
                // 3) Locals (for entities with locals)
                // 4) Properties or Fields
                // 5) Globals and Defines
                WriteOutputMessage($"--> FindIdentifier in {currentType?.FullName}, '{name}' ");
                var member = location.Member;
                if (currentType?.TypeParameters.Count > 0 && currentType is XSourceTypeSymbol source)
                {
                    foreach (var param in currentType.TypeParameters)
                    {
                        if (StringEquals(param, name) )
                        {
                            var sym = new XSourceTypeParameterSymbol(source, name, source.Range, source.Interval);
                            result.Add(sym);
                        }
                    }
                }
                if (result.Count == 0 && member.Kind.HasParameters())
                {
                    result.AddRange(member.Parameters.Where(x => StringEquals(x.Name, name)));
                }
                if (result.Count == 0)
                {
                    // then Locals
                    // line numbers in the range are 1 based. currentLine = 0 based !
                    if (member.Kind.HasBody())
                    {
                        var local = member.GetLocals(location).Where(x => StringEquals(x.Name, name) && x.Range.StartLine - 1 <= location.LineNumber).LastOrDefault();
                        if (local != null)
                        {
                            result.Add(local);
                        }
                    }
                }
                foreach (XSourceVariableSymbol variable in result)
                {
                    if (variable.File == null)
                        variable.File = member.File;
                }
                if (result.Count == 0)
                {
                    // We can have a Property/Field of the current CompletionType
                    if (currentType != null && !currentType.IsGlobalType())
                    {
                        result.AddRange(SearchPropertyOrField(location, currentType, name, visibility));
                    }
                    if (result.Count == 0)
                    {
                        var glob = location.File.Project.FindGlobalOrDefine(name);
                        if (glob != null)
                        {
                            result.Add(glob);
                        }
                    }
                    if (result.Count == 0)
                    {
                        // Now, search for a Global in external Assemblies
                        //
                        result.AddRange(SearchGlobalField(location, name));
                    }
                }
                if (result.Count > 0)
                {
                    var element = result[0];
                    if (element is XSourceImpliedVariableSymbol xVar)
                    {
                        var type = ResolveImpliedType(location, xVar, currentType, visibility);
                        if (type != null)
                        {
                            xVar.TypeName = type.FullName.GetXSharpTypeName();
                        }

                    }
                }
            }
            catch (Exception ex)
            {
                XSettings.LogException(ex, "FindIdentifier failed");
            }
            finally
            {
                while (nestedSearches.Count > nestedLevel)
                {
                    nestedSearches.Remove(nestedSearches.Last());
                }
            }
            DumpResults(result, $"--> FindIdentifier in {currentType?.FullName}, '{name}'");
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

        public static XSourceEntity FindEntity(int nLine, XFile file)
        {
            if (file == null || file.EntityList == null)
            {
                return null;
            }
            return file.FindMemberAtRow(nLine);

        }
        public static XSourceMemberSymbol FindMember(int nLine, XFile file)
        {
            var member = FindEntity(nLine, file);
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
            if (ent is XSourceTypeSymbol symbol)
            {
                return symbol.XMembers.LastOrDefault();
            }

            WriteOutputMessage(string.Format("Cannot find member at 0 based line {0} in file {0} .", nLine, file.FullPath));

            return null;
        }

        private static string GetTypeNameFromSymbol(XSharpSearchLocation location, IXSymbol symbol)
        {
            if (symbol is IXTypeSymbol ts)
                return ts.FullName;
            string name = null;
            if (symbol is IXMemberSymbol mem)
            {
                if (mem.Kind is Kind.Constructor)
                {
                    name = mem.ParentType.FullName;
                }
                else
                {
                    name = mem.OriginalTypeName;
                }
            }
            if (symbol is IXVariableSymbol)
            {
                name = symbol.TypeName;
            }
            return name;
        }


        private static IXTypeSymbol GetTypeFromSymbol(XSharpSearchLocation location, IXSymbol symbol)
        {
            if (symbol is IXTypeSymbol ts)
                return ts;
            var name = GetTypeNameFromSymbol(location, symbol);
            if (name == null)
                return null;
            if (name.EndsWith("[]"))
            {
                name = "System.Array";
            }
            return SearchType(location, name).FirstOrDefault();
        }

        private static IXTypeSymbol ResolveImpliedLoop(XSharpSearchLocation location, XSourceImpliedVariableSymbol xVar, IXTypeSymbol currentType, Modifiers visibility)
        {
            //  Resolve the type of a loop variable. To do so we split the line in the part before and after the TO/UPTO/DOWNTO
            Debug.Assert(xVar.ImpliedKind == ImpliedKind.LoopCounter);
            var start = new List<XSharpToken>();
            var end = new List<XSharpToken>();
            bool seento = false;
            foreach (var t in xVar.Expression)
            {
                switch (t.Type)
                {
                    case XSharpLexer.TO:
                    case XSharpLexer.UPTO:
                    case XSharpLexer.DOWNTO:
                        seento = true;
                        break;
                    default:
                        if (seento)
                            end.Add(t);
                        else
                            start.Add(t);
                        break;
                }
            }
            if (start.Count > 0 && end.Count > 0)
            {
                if (start.Count == 1 && end.Count == 1)
                {
                    var startType = start[0].Type;
                    var endType = end[0].Type;
                    // when one of the 2 is an ID then we resolve to the type of the Id. First is leading
                    if (startType == XSharpLexer.ID)
                    {
                        var id = FindIdentifier(location, start[0].Text, currentType, visibility);
                        return GetTypeFromSymbol(location, id.FirstOrDefault());
                    }
                    else if (endType == XSharpLexer.ID)
                    {
                        var id = FindIdentifier(location, end[0].Text, currentType, visibility);
                        return GetTypeFromSymbol(location, id.FirstOrDefault());
                    }
                    // if the one of the two is constant we take the type of the constant.
                    else if (XSharpLexer.IsConstant(startType))
                    {
                        return GetConstantType(start[0], location.File);
                    }

                    else if (XSharpLexer.IsConstant(endType))
                    {
                        return GetConstantType(end[0], location.File);
                    }
                    //todo what else ?
                    return null;
                }
                string notProcessed;
                if (end.Count > 1)  // prefer type of end over type of start
                {
                    var res = RetrieveElement(location, end, CompletionState.General, out notProcessed);
                    return GetTypeFromSymbol(location, res.FirstOrDefault());
                }
                // start must be > 1
                var result = RetrieveElement(location, start, CompletionState.General, out notProcessed);
                return GetTypeFromSymbol(location, result.FirstOrDefault());
            }
            return null;
        }
        private static IXTypeSymbol ResolveImpliedCollection(XSharpSearchLocation location, XSourceImpliedVariableSymbol xVar, IXTypeSymbol currentType, Modifiers visibility)
        {
            // Resolve the VAR type of an element in a collection
            var tokenList = xVar.Expression;
            var result = RetrieveElement(location, tokenList, CompletionState.General, out var notProcessed);
            var element = result.FirstOrDefault();
            if (element == null)
                return null;
            xVar.Collection = element;
            var elementType = GetTypeNameFromSymbol(location, element);
            if (elementType.EndsWith("[]"))
            {
                return SearchType(location, elementType.Substring(0, elementType.Length - 2)).FirstOrDefault();
            }
            var type = GetTypeFromSymbol(location, element);
            var etype = getElementType(type, location, element);

            return etype;
        }

        private static IXTypeSymbol getElementType(IXTypeSymbol type, XSharpSearchLocation location, IXSymbol element)
        {
            string elementType;
            type.ForceComplete();
            if (type.IsArray)
            {
                elementType = type.ElementType;
                var p = location.FindType(elementType);
                if (p != null)
                    return p;
            }
            var prop = type.GetProperties().Where((p) => p.Parameters.Count > 0).FirstOrDefault();
            if (prop != null)
            {
                var typeName = prop.TypeName;
                if (type.IsGeneric)
                {
                    var typeparams = type.TypeParameters;
                    if (typeparams.Count > 0 && element is XSourceEntity xse)
                    {
                        var genargs = xse.GenericArgs;
                        var index = typeparams.IndexOf(typeName);
                        if (index > -1 && index < genargs.Length)
                        {
                            typeName = genargs[index];
                        }
                    }
                }
                var p = location.FindType(typeName);
                if (p != null)
                    return p;

            }
            if (type.HasEnumerator())
            {
                var member = type.GetMembers("GetEnumerator").FirstOrDefault();
                var enumtype = SearchType(location, member.OriginalTypeName).FirstOrDefault();
                var current = enumtype.GetProperties("Current").FirstOrDefault();
                elementType = current?.OriginalTypeName;
                if (type.IsGeneric)
                {
                    var p = location.FindType(elementType);
                    if (p != null)
                        return p;
                }
            }
            return null;
        }

        private static IXTypeSymbol ResolveImpliedOutParam(XSharpSearchLocation location, XSourceImpliedVariableSymbol xVar, IXTypeSymbol currentType, Modifiers visibility)
        {
            // TODO: Resolve type lookup Out Parameters
            Debug.Assert(xVar.ImpliedKind == ImpliedKind.OutParam);
            return null;
        }
        private static IList<XSharpToken> DeleteNestedTokens(IList<XSharpToken> tokens)
        {
            IList<XSharpToken> result = new List<XSharpToken>();
            if (tokens == null)
                return result;
            int level = 0;
            bool hasId = false;
            foreach (var token in tokens)
            {
                switch (token.Type)
                {
                    case XSharpLexer.LPAREN:
                    case XSharpLexer.LCURLY:
                    case XSharpLexer.LBRKT:
                        if (hasId)
                        {
                            if (level == 0)
                                result.Add(token);
                            level += 1;
                        }
                        break;
                    case XSharpLexer.RPAREN:
                    case XSharpLexer.RCURLY:
                    case XSharpLexer.RBRKT:
                        level -= 1;
                        if (level == 0)
                            result.Add(token);
                        break;
                    case XSharpLexer.ID:
                        hasId = true;
                        if (level == 0)
                            result.Add(token);
                        break;
                    default:
                        if (level == 0)
                            result.Add(token);
                        break;
                }
            }
            return result;
        }
        private static IXTypeSymbol ResolveImpliedAssign(XSharpSearchLocation location, XSourceImpliedVariableSymbol xVar, IXTypeSymbol currentType, Modifiers visibility)
        {
            Debug.Assert(xVar.ImpliedKind == ImpliedKind.Assignment || xVar.ImpliedKind == ImpliedKind.Using);
            var tokenList = xVar.Expression;
            // delete tokens between {} and other operators so we get the return type of the outer construct
            tokenList = DeleteNestedTokens(tokenList);
            var result = RetrieveElement(location, tokenList, CompletionState.General, out var notProcessed);
            var element = result.FirstOrDefault();
            return GetTypeFromSymbol(location, element);
        }
        private static IXTypeSymbol ResolveImpliedType(XSharpSearchLocation location, XSourceImpliedVariableSymbol xVar, IXTypeSymbol currentType, Modifiers visibility)
        {
            // the following Implied Kinds are followed by a := and should return the type of the expression in the ExpressionList
            switch (xVar.ImpliedKind)
            {
                case ImpliedKind.Assignment:
                case ImpliedKind.Using:
                    return ResolveImpliedAssign(location, xVar, currentType, visibility);
                case ImpliedKind.TypeCheck:
                    return SearchType(location, xVar.TypeName).FirstOrDefault();
                case ImpliedKind.InCollection:
                    return ResolveImpliedCollection(location, xVar, currentType, visibility);
                case ImpliedKind.LoopCounter:
                    return ResolveImpliedLoop(location, xVar, currentType, visibility);
                case ImpliedKind.OutParam:
                    return ResolveImpliedOutParam(location, xVar, currentType, visibility);
                case ImpliedKind.None:
                    break;
            }
            return null;
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
        /// <param name="forQuickinfo"></param>
        /// <returns></returns>
        public static IList<IXSymbol> RetrieveElement(XSharpSearchLocation location, IList<XSharpToken> xtokenList,
            CompletionState state, out string notProcessed, bool forQuickinfo = false )
        {
            //
            notProcessed = "";
            var result = new List<IXSymbol>();
            if (xtokenList == null || xtokenList.Count == 0)
                return result;
            var list = new XSharpTokenList(xtokenList);
            var symbols = new Stack<IXSymbol>();
            IXTypeSymbol currentType = null;
            var startOfExpression = true;
            var findConstructor = false;
            XSharpToken currentToken = null;
            IXTypeSymbol startType = null;
            state = CompletionState.General;
            if (location.Member == null)
            {
                // This is a lookup outside code.
                // Could be USING or USING STATIC statement.
                // We allow the lookup of Namespaces or Types
                // 
                if (!state.HasFlag(CompletionState.Namespaces) && !state.HasFlag(CompletionState.Types))
                    return result;
                StringBuilder sb = new StringBuilder();
                foreach (var token in xtokenList)
                {
                    sb.Append(token.Text);
                }
                if (state.HasFlag(CompletionState.Namespaces))
                    result.AddRange(SearchNamespaces(location, sb.ToString()));
                if (state.HasFlag(CompletionState.Types))
                    result.AddRange(SearchType(location, sb.ToString()));

                return result;
            }
            // Context Type....
            if (location.Member.Kind.IsClassMember(location.Dialect))
            {
                currentType = location.Member.ParentType;
                if (currentType != null)
                {
                    symbols.Push(currentType);
                }
            }
            else if (location.Member.Kind == Kind.EnumMember)
            {
                currentType = location.Member.ParentType;
                if (currentType != null)
                {
                    symbols.Push(currentType);
                }
            }
            Modifiers visibility = Modifiers.Private;
            string namespacePrefix = "";
            var hasBracket = false;
            int count = -1;
            startType = currentType;
            bool resetState = false;
           while (! list.Eoi())
            {
                // after LPAREN, LCURLY and LBRKT we skip until we see the closing token
                currentToken = list.ConsumeAndGet();
                if (currentToken.Type == XSharpLexer.DOT ||
                    currentToken.Type == XSharpLexer.COLON)
                    startOfExpression = false;
                result.Clear();
                notProcessed = "";
                if (symbols.Count > 0 && symbols.Count != count)
                {
                    var top = symbols.Peek();
                    currentType = GetTypeFromSymbol(location, top);
                    if (top != null)
                        top.ResolvedType = currentType;
                    count = symbols.Count;
                    if (top.Kind == Kind.Namespace)
                    {
                        namespacePrefix = top.Name+".";
                    }
                }
                if (resetState)
                {
                    state = CompletionState.General;
                }
                if (startOfExpression)
                {
                    currentType = startType;
                    namespacePrefix = "";
                }


                var currentName = currentToken.CleanText();
                var lastToken = currentToken;
                switch (currentToken.Type)
                {
                    case XSharpLexer.LPAREN:
                        startOfExpression = true;
                        continue;
                    case XSharpLexer.LCURLY:
                        startOfExpression = true;
                        continue;
                    case XSharpLexer.LBRKT:
                        hasBracket = true;
                        startOfExpression = true;
                        continue;
                    case XSharpLexer.RPAREN:
                    case XSharpLexer.RCURLY:
                    case XSharpLexer.RBRKT:
                        hasBracket = (currentToken.Type == XSharpLexer.RBRKT);
                        if (symbols.Count > 0 && hasBracket )
                        {
                            var nextType = list.La1;

                            if (nextType == XSharpLexer.LCURLY)
                            {
                                // [] is followed by a ctor as in 
                                // var aValue := string[]{
                                var top = symbols.Peek();
                                var typeName = top.FullName + "[]";
                                var type = SearchType(location, typeName).FirstOrDefault();
                                if (type != null)
                                    symbols.Push(type);
                                state = CompletionState.Constructors;
                                resetState = false;
                            }
                            else
                            {
                                var top = symbols.Peek();
                                var type = FindElementType(top, location);
                                if (type != null)
                                    symbols.Push(type);
                            }
                        }
                        else if (symbols.Count > 0)
                        {
                            if (symbols.Peek() is IXTypeSymbol type)
                                currentType = type;
                            else if (symbols.Peek() is IXMemberSymbol member)
                            {
                                currentType = member.ResolvedType;
                            }
                        }
                        continue;
                    case XSharpLexer.DOT:
                        state = CompletionState.StaticMembers | CompletionState.Namespaces | CompletionState.Types;
                        if (location.Project.ParseOptions.AllowDotForInstanceMembers ||
                            (currentType != null && currentType.IsVoStruct() ))
                            state |= CompletionState.InstanceMembers;
                        resetState = false;
                        startOfExpression = false;
                        continue;

                    case XSharpLexer.COLON:
                        state = CompletionState.InstanceMembers;
                        resetState = false;
                        startOfExpression = false;
                        continue;

                    case XSharpLexer.COLONCOLON:
                        startOfExpression = false;
                        state = CompletionState.Namespaces;
                        continue;
                    case XSharpLexer.COMMA:
                        startOfExpression = true;
                        state = CompletionState.General;
                        break;
                    default:
                        hasBracket = false;
                        if (XSharpLexer.IsOperator(currentToken.Type))
                        {
                            state = CompletionState.General;
                            startOfExpression = true;
                        }
                        break;
                }
                bool isType = XSharpLexer.IsType(currentToken.Type);
                var isId = currentToken.Type == XSharpLexer.ID ||
                                  currentToken.Type == XSharpLexer.KWID ||
                                  currentToken.Type == XSharpLexer.SELF ||
                                  currentToken.Type == XSharpLexer.SUPER ||
                                  currentToken.Type == XSharpLexer.TYPEOF ||
                                  currentToken.Type == XSharpLexer.NAMEOF ||
                                  currentToken.Type == XSharpLexer.SIZEOF ||
                                  currentToken.Type == XSharpLexer.COLONCOLON ||
                                  isType;
                if (isId && !list.Eoi() && list.La1 == XSharpLexer.LT)
                {
                    currentName += list.ConsumeAndGetText();

                    while (! list.Eoi())
                    {
                        var tokenNext = list.ConsumeAndGet();
                        currentName += tokenNext.Text;
                        if (tokenNext.Type == XSharpLexer.GT)
                            break;
                    }
                }
                var qualifiedName = false;
                var findMethod = false;
                var findType = state.HasFlag(CompletionState.Types) || state.HasFlag(CompletionState.General);
                var literal = XSharpLexer.IsConstant(currentToken.Type);
                if (isId)
                {
                    qualifiedName = list.La1 == XSharpLexer.DOT;
                    findMethod = list.La1 == XSharpLexer.LPAREN;
                    findConstructor = list.La1 == XSharpLexer.LCURLY;
                    if (state.HasFlag(CompletionState.StaticMembers) && !findMethod && result.Count == 0)
                    {
                        var props = SearchPropertyOrField(location, currentType, namespacePrefix + currentName, visibility).Where(m => m.IsStatic);
                        if (props != null)
                            result.AddRange(props);
                    }
                    if (state.HasFlag(CompletionState.InstanceMembers) && !findMethod && result.Count == 0)
                    {
                        var props = SearchPropertyOrField(location, currentType, namespacePrefix + currentName, visibility).Where(m => !m.IsStatic);
                        if (props != null)
                            result.AddRange(props);
                    }
                }
                if (literal)
                {
                    currentType = GetConstantType(currentToken, location.File);
                    symbols.Push(currentType);
                }
                else if (findMethod)
                {
                    // Do we already know in which Type we are ?
                    if (currentToken.Type == XSharpLexer.SELF)  // SELF(..)
                    {
                        var ctors = SearchConstructors(currentType, Modifiers.Private);
                        if (ctors != null)
                            result.AddRange(ctors);
                    }
                    else if (currentToken.Type == XSharpLexer.SUPER) // SUPER(..)
                    {
                        if (currentType is XSourceTypeSymbol source)
                        {
                            var p = source.File.FindType(source.BaseTypeName, source.Namespace);
                            var ctors = SearchConstructors(p, Modifiers.Protected);
                            if (ctors != null)
                                result.AddRange(ctors);
                        }
                        else
                        {
                            var p = location.FindType(currentType.BaseTypeName);
                            var ctors = SearchConstructors(p, Modifiers.Protected);
                            if (ctors != null)
                                result.AddRange(ctors);
                        }
                    }
                    else if (startOfExpression)
                    {
                        
                        // The first token in the list can be a Function or a Procedure
                        // Except if we already have a Type
                        result.AddRange(SearchFunction(location, currentName));

                        if (result.Count == 0 && currentType != null )
                        {
                            // no method lookup when enforceself is enabled
                            if (! location.Project.ProjectNode.EnforceSelf)
                                result.AddRange(SearchMethod(location, currentType, currentName, visibility, false));
                        }
                        if (result.Count == 0 )
                        {
                            result.AddRange(SearchMethodStatic(location, currentName));
                        }
                        if (result.Count == 0)
                        {
                            // Foo() could be a delegate call where Foo is a local or Field
                            var delgs = SearchDelegateCall(location, currentName, currentType, visibility);
                            if (delgs != null)
                                result.AddRange(delgs);
                        }
                        if (result.Count == 0)
                        {
                            // Foo() can be a method call inside the current class
                            var type = location?.Member?.ParentType ?? currentType;
                            var meths = SearchMethod(location, type, currentName, visibility, false);
                            if (meths != null)
                                result.AddRange(meths);

                        }

                        if (list.Eoi())
                        {
                            return result;
                        }
                    }
                    else if (currentType != null)
                    {
                        // Now, search for a Method. this will search the whole hierarchy
                        // Respect the StaticMembers and InstanceMembers states
                        if (state.HasFlag(CompletionState.StaticMembers))
                        {
                            var meths = SearchMethod(location, currentType, currentName, visibility, true);
                            if (meths != null)
                                result.AddRange(meths);
                        }
                        if (state.HasFlag(CompletionState.InstanceMembers))
                        {
                            var meths = SearchMethod(location, currentType, currentName, visibility, false);
                            if (meths != null)
                                result.AddRange(meths);
                        }
                    }
                    
                    if (result.Count == 0)
                    {
                        // Could it be Static Method with "Using Static"
                        var mstatic = SearchMethodStatic(location, currentName);
                        if (mstatic != null)
                            result.AddRange(mstatic);
                    }
                    // method found ?
                    if (result.Count > 0)
                    {
                        symbols.Push(result[0]);
                    }
                    else
                    {
                        symbols.Clear();
                        break;
                    }
                }
                else if (isId)
                {
                    // Order:
                    // 1) Locals and Parameters (only at start)
                    // 2) Properties and Fields (only at start)
                    // 3) Types
                    // 4) Namespaces
                    // Types first because when we have a nested type then the parent is also registered as a namespace
                    // but we want to find the type of course
                    if (startOfExpression)
                    {
                        // Search in Parameters, Locals, Field and Properties
                        if (currentName.EndsWith("."))
                        {
                            currentName = currentName.Substring(0, currentName.Length - 1);
                        }
                        else if (currentName == "::" || currentName.ToLower() == "this")
                        {
                            currentName = "SELF";
                        }
                        // 1) Locals and Parameters
                        var locals = FindIdentifier(location, currentName, currentType, Modifiers.Private);
                        if (locals != null)
                            result.AddRange(locals);
                        if (result.Count == 0)
                        {
                            // 2) Properties and Fields
                            var fldsprops = SearchPropertyOrField(location, currentType, currentName, visibility);
                            if (fldsprops != null)
                                result.AddRange(fldsprops);
                        }
                    }
                    if (result.Count == 0 && (startOfExpression || findType || findConstructor || qualifiedName))
                    {
                        // 3) Types
                        var types = SearchType(location, namespacePrefix + currentName);
                        if (types != null)
                        {
                            result.AddRange(types);
                        }
                        if (result.Count == 0)
                        {
                            // 4) Namespaces
                            var namespaces = SearchNamespaces(location, namespacePrefix + currentName);
                            if (namespaces != null)
                                result.AddRange(namespaces);
                        }
                        if (result.Count > 0)
                            namespacePrefix = "";
                    }
                    if (result.Count == 0 && currentToken.Type == XSharpLexer.ID)
                    {
                        notProcessed = namespacePrefix + currentName;
                        var sym = new XSourceUndeclaredVariableSymbol(location.Member, notProcessed, location.Member.Range, location.Member.Interval);
                        result.Add(sym);
                        break;
                    }
                    // id found ?
                    if (result.Count > 0)
                    {
                        symbols.Push(result[0]);
                    }
                    else
                    {
                        symbols.Clear();
                        break;
                    }
                }
                else if (XSharpLexer.IsOperator(currentToken.Type))
                {
                    state = CompletionState.General;
                    startOfExpression = true;
                    
                }
                else if (currentToken.Type == XSharpLexer.CONSTRUCTOR)
                {
                    if (currentType != null)
                    {
                        result.AddRange(currentType.GetConstructors());
                        if (result.Count > 0)
                        {
                            IXSymbol selected;
                            selected = result[0];
                            if ( selected is XSourceEntity)
                            {
                                foreach (var item in result)
                                {
                                    if (item is XSourceEntity xs && xs.File.FullPath == location.File.FullPath)
                                    {
                                        if (xs.Range.StartLine <= location.LineNumber && xs.Range.EndLine >= location.LineNumber)
                                        {
                                            selected = xs;
                                            break;
                                        }
                                    }
                                }
                            }
                            symbols.Push(selected);
                        }
                    }
                }
                else
                {
                    //Do nothing
                }
                // We have it
                if (hasBracket && symbols.Count > 0)
                {
                    var type = currentType;
                    var symbol = symbols.Peek();
                }
            }
            result.Clear();
            if (symbols.Count > 0)
            {
                result.Add(symbols.Pop());
                if (result[0] is IXMemberSymbol xmember && xmember.ParentType != null && xmember.ParentType.IsGeneric && symbols.Count > 0)
                {
                    result.Clear();
                    result.Add(AdjustGenericMember(xmember, symbols.Peek()));
                }
            }
            if (result.Count > 0 && result[0] is IXTypeSymbol xtype && state == CompletionState.Constructors)
            {
                result.Clear();
                var ctors = SearchConstructors(xtype, Modifiers.Public);
                if (ctors.Count == 0 && xtype is XSourceTypeSymbol)
                {
                    var ctor = new XSourceMemberSymbol(XLiterals.ConstructorName, Kind.Constructor, Modifiers.Public,
                        location.Member.Range, location.Member.Interval, "", false);
                    ctor.Parent = xtype;
                    ctor.DeclaringType = xtype.FullName;

                    result.Add(ctor);
                }
                else
                {
                    result.AddRange(ctors);
                }
            }
            if (result.Count == 0)
            {
                var namespaces = location.Project.AllNamespaces.Where(n => n == namespacePrefix);
                if (namespaces.Count() > 0)
                {
                    var ns = namespaces.First();
                    result.Add(new XSymbol(ns, Kind.Namespace, Modifiers.Public));
                }
            }
            if (!string.IsNullOrEmpty(notProcessed))
            {
                result.Clear();
                //result.AddRange(SearchFunction(location, notProcessed));
                if (result.Count == 0)
                {
                    result.AddRange(SearchGlobalField(location, notProcessed));
                }
                if (result.Count == 0)
                {
                    if (currentToken.Type == XSharpLexer.ID)
                    {
                        var sym = new XSourceUndeclaredVariableSymbol(location.Member, notProcessed, location.Member.Range, location.Member.Interval);
                        result.Add(sym);
                    }
                }
            }
            else if (result.Count == 0 && XSharpLexer.IsKeyword(currentToken.Type))
            {
                currentToken.Text = XSettings.FormatKeyword(currentToken.Text);
                var sym = new XSymbol(currentToken.Text, Kind.Keyword, Modifiers.Public);
                result.Add(sym);
            }
            else if (result.Count > 0 && result[0] != null)
            {
                var res = result[0];
                if (res.Kind.IsClassMember(location.Project.Dialect))
                {
                    var member = res as IXMemberSymbol;
                    var type = res.Parent as IXTypeSymbol;
                    if (type != null && type.IsGeneric)
                    {
                        // find element on the stack that has this type
                        IXSymbol symbol = null;
                        foreach (var item in symbols)
                        {
                            if (item.ResolvedType == type)
                            {
                                symbol = item;
                                break;
                            }
                        }
                        string[] elements = null;
                        if (symbol is XSourceEntity xse && xse.GenericArgs != null)
                        {
                            elements = xse.GenericArgs.ToArray();
                        }
                        if (elements != null)
                        {
                            member = member.Clone();
                            var typeParams = type.TypeParameters;
                            // check to see if parameters or return value is one of the type parameters
                            // if so, then check the symbols to see where the type is used and with which
                            // concrete parameters
                            int pos1;
                            foreach (var param in member.Parameters)
                            {
                                pos1 = typeParams.IndexOf(param.TypeName);
                                if (pos1 >= 0)
                                {
                                    param.TypeName = elements[pos1];
                                }
                            }
                            pos1 = typeParams.IndexOf(member.TypeName);
                            if (pos1 >= 0)
                            {
                                member.TypeName = elements[pos1];
                            }
                            result[0] = member;
                        }
                    }
                }
            }
            DumpResults(result,"RetrieveElements");
            return result;
        }

        private static IXTypeSymbol FindElementType(IXSymbol symbol, XSharpSearchLocation location)
        {
            if (symbol.IsArray)
            {
                var elementType = symbol.ElementType;
                var p = location.FindType(elementType);
                if (p != null)
                    return p;
            }
            if (symbol.TypeName != null && symbol.TypeName.EndsWith("[]"))
            {
                return SearchType(location, symbol.TypeName.Substring(0, symbol.TypeName.Length - 2)).FirstOrDefault();
            }
            else
            {
                var type = symbol.ResolvedType;
                if (type != null)
                {
                    var prop = type.GetProperties().Where((p) => p.Parameters.Count > 0).FirstOrDefault();
                    if (prop != null)
                    {
                        var tn = prop.TypeName;
                        if (type.IsGeneric && symbol is XSourceEntity xse)
                        {
                            var realargs = xse.GenericArgs;
                            tn = ReplaceTypeParameters(tn, type.TypeParameters, realargs);

                        }
                        var p = location.FindType(tn);
                        if (p != null)
                            return p;
                    }
                }
            }
            return null;
        }

        private static IXMemberSymbol AdjustGenericMember(IXMemberSymbol xmember, IXSymbol memberdefinition)
        {
            /*
             * This code was added to solve the following
             * LOCAL coll as Dictionary<STRING, LONG>
             * FOREACH var item in coll
             *   ? Item:Key: // should return members of STRING
             *   ? Item:Value: // should return members of LONG
             * NEXT
             * Item:Key is defined as TKey
             * Item:Value is defined as TValue
             * We will probably have to get the next element on the stack, and extract its generic args
             * and then find the correct argument and resolve TKey to STRING and TValue to LONG
             * 
             * We should also fix the parameter types for
             * LOCAL coll as Dictionary<STRING, LONG>
             * coll:Add()           // types are TKey and TValue and should become STRING and INT
             * coll:ContainsKey()   // param type = TKey and should be changed to STRING
            */
            var type = xmember.ParentType;
            var typeParameters = type.TypeParameters;
            var resultType = xmember.TypeName;
            var pos = typeParameters.IndexOf(resultType);
            if (pos == -1)
            {
                foreach (var param in xmember.Parameters)
                {
                    pos = typeParameters.IndexOf(param.TypeName);
                    if (pos >= 0)
                    {
                        break;
                    }
                }
            }
            if (pos >= 0 )       // return type or parameter type of member is one of the generic arguments
            {
                xmember = xmember.Clone();
                if (memberdefinition is XSourceVariableSymbol xvar)
                {
                    var realargs = xvar.GenericArgs;
                    if (!xvar.IsGeneric && xvar is XSourceImpliedVariableSymbol impvar)
                    {
                        if (impvar.Collection != null && impvar.Collection is XSourceVariableSymbol xvar2)
                        {
                            xvar = xvar2;
                            realargs = xvar.GenericArgs;
                        }
                    }
                    if (xvar.IsGeneric  && realargs.Length == typeParameters.Count)
                    {
                        pos = typeParameters.IndexOf(xmember.TypeName);
                        if (pos >=0)
                            xmember.TypeName = realargs[pos];
                        foreach (var param in xmember.Parameters)
                        {
                            pos = typeParameters.IndexOf(param.TypeName);
                            if (pos >= 0)
                            {
                                param.TypeName = realargs[pos];
                            }
                        }
                    }
                }
            }
            return xmember;
        }

        //private static string[] GetRealTypeParameters(string typeName)
        //{
        //    var pos = typeName.IndexOf('<');
        //    if (pos > 0)
        //    {
        //        var args = typeName.Substring(pos);
        //        var elements = args.Split(new char[] { '<', '>', ',' }, StringSplitOptions.RemoveEmptyEntries);
        //        return elements;
        //    }
        //    return new string[] { };
        //}
        private static string ReplaceTypeParameters(string typeName, IList<String> genericParameters, IList<String> realParameters)
        {
            if (genericParameters.Count == realParameters.Count)
            {
                for (int i = 0; i < genericParameters.Count; i++)
                {
                    typeName = typeName.Replace(genericParameters[i], realParameters[i]);
                }
            }
            return typeName;
        }

        /// <summary>
        /// Search for the Constructor in the corresponding Type,
        /// no return value, the constructor is returned by foundElement
        /// </summary>
        /// <param name="cType"></param>
        /// <param name="minVisibility"></param>
        /// <param name="foundElement"></param>
        private static IList<IXMemberSymbol> SearchConstructors(IXTypeSymbol type, Modifiers minVisibility)
        {
            var result = new List<IXMemberSymbol>();
            if (type != null)
            {
                WriteOutputMessage($"--> SearchConstructorIn {type?.FullName}");
                //
                var ctors = type.Members.Where(x => x.Kind == Kind.Constructor && x.IsVisible(minVisibility));
                result.AddRange(ctors);
                DumpResults(result, $"--> SearchConstructorIn {type?.FullName}");
            }
            return result;
        }

        internal static IXTypeSymbol GetConstantType(XSharpToken token, XFile file)
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
            return SearchMembers(location, type, name, minVisibility).Where((m) => m.Kind.IsProperty() || m.Kind == Kind.Field || m.Kind == Kind.Event || m.Kind == Kind.EnumMember);
        }

        private static IEnumerable<IXSymbol> SearchDelegateCall(XSharpSearchLocation location, string currentName, IXTypeSymbol currentType, Modifiers visibility)
        {
            var result = new List<IXSymbol>();
            if (location == null || location.File == null)
            {
                return result;
            }
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
            DumpResults(result, $"--> SearchDelegateCall in file {location.File.SourcePath} {currentName} ");
            return result;
        }

        //private static IEnumerable<IXSymbol> SearchNestedTypes(XSharpSearchLocation location, string name)
        //{
        //    var result = new List<IXSymbol>();
        //    var parent = SearchType(location, name).FirstOrDefault();
        //    if (parent != null)
        //    {
        //        result.AddRange(parent.Children);
        //    }
        //    return result;
        //}


        private static IEnumerable<IXSymbol> SearchNamespaces(XSharpSearchLocation location, string name)
        {
            var result = new List<IXSymbol>();
            if (location == null || location.File == null)
            {
                return result;
            }
            WriteOutputMessage($"SearchNamespaces in file {location.File.SourcePath} '{name}'");
            var namespaces = location.Project.AllNamespaces;
            foreach (var ns in namespaces)
            {
                if (ns.StartsWith(name, StringComparison.OrdinalIgnoreCase))
                {
                    result.Add(new XSymbol(name, Kind.Namespace, Modifiers.Public));
                    break;
                }
            }
            DumpResults(result, $"SearchNamespaces in file {location.File.SourcePath} '{name}' ");
            return result;
        }

        private static IEnumerable<IXTypeSymbol> SearchType(XSharpSearchLocation location, string name)
        {
            var result = new List<IXTypeSymbol>();
            if (location == null || location.File == null)
            {
                return result;
            }
            WriteOutputMessage($"SearchType in file {location.File.SourcePath} '{name}'");
            // translate out type names to system type names
            name = name.GetSystemTypeName(location.Project.ParseOptions.XSharpRuntime);

            var type = location.FindType(name);
            if (type != null)
                result.Add(type);
            DumpResults(result, $"SearchType in file {location.File.SourcePath} '{name}'");
            return result;
        }

        private static IXTypeSymbol EnsureComplete(IXTypeSymbol type, XSharpSearchLocation location)
        {
            if (type is XSourceTypeSymbol srcType && srcType.IsPartial)
            {
                var newtype = location.FindType(type.Name);
                if (newtype != null)
                    return newtype;
            }
            return type;
        }

        private static IEnumerable<IXMemberSymbol> SearchMembers(XSharpSearchLocation location, IXTypeSymbol type, string name, Modifiers minVisibility)
        {
            var result = new List<IXMemberSymbol>();
            if (location == null || location.File == null || type == null)
            {
                return result;
            }
            if (type is XSourceTypeSymbol)
            {
                type = EnsureComplete(type, location);
            }
            WriteOutputMessage($" SearchMembers {type?.FullName} , '{name}'");
            result.AddRange(type.GetMembers(name, true).Where((m) => m.IsVisible(minVisibility)));
            if (result.Count() == 0 && !string.IsNullOrEmpty(type.BaseTypeName))
            {
                if (minVisibility == Modifiers.Private)
                    minVisibility = Modifiers.Protected;
                IXTypeSymbol baseType;
                if (type is XSourceTypeSymbol sourceType)
                {
                    baseType = sourceType.File.FindType(type.BaseTypeName, type.Namespace);
                }
                else
                {
                    baseType = location.FindType(type.BaseTypeName);
                }
                if (baseType?.FullName == type.FullName)
                {
                    WriteOutputMessage("*** Recursion detected *** " + type.FullName + " inherits from " + baseType.FullName);
                    DumpResults(result, $" SearchMembers {type?.FullName} , '{name}'");
                    return result;
                }
                result.AddRange(SearchMembers(location, baseType, name, minVisibility));
            }
            DumpResults(result, $" SearchMembers {type?.FullName} , '{name}'");
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
            if (location == null || location.File == null || type == null)
            {
                return result;
            }
            if (type is XSourceTypeSymbol)
            {
                type = EnsureComplete(type, location);
            }

            WriteOutputMessage($" SearchMethod {type.FullName} , '{name}'");
            IEnumerable<IXMemberSymbol> tmp;
            if (type.IsFunctionsClass)
            {
                tmp = type.GetMembers(name, true).Where(x => x.Kind == Kind.Function);
                staticOnly = false;
            }
            else
            {
                tmp = type.GetMembers(name, true).Where(x => x.Kind.IsClassMethod(location.Dialect));
            }
            foreach (var m in tmp)
            {
                var add = true;
                if (staticOnly && !m.IsStatic)
                    add = false;
                if (add && (m.Visibility == Modifiers.Internal || m.Visibility == Modifiers.ProtectedInternal))
                {
                    if (m is IXSourceSymbol source && source.File.Project == location.Project)
                        add = true;
                    else if (!m.IsVisible(minVisibility))
                        add = false;
                }
                if (add)
                {
                    result.Add(m);
                }
            }

            if (result.Count == 0 && type.FullName != "System.Object")
            {
                var baseTypeName = type.BaseTypeName ?? "System.Object";
                if (minVisibility == Modifiers.Private)
                    minVisibility = Modifiers.Protected;
                IXTypeSymbol baseType;
                if (type is XSourceTypeSymbol sourceType)
                {
                    baseType = sourceType.File.FindType(baseTypeName, sourceType.Namespace);
                }
                else
                {
                    baseType = location.FindType(baseTypeName);
                }
                result.AddRange(SearchMethod(location, baseType, name, minVisibility, staticOnly));
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
            DumpResults(result, $" SearchMethod {type.FullName} , '{name}'");
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
            WriteOutputMessage($" SearchMethodStaticIn {location.File.SourcePath}, '{name}' ");
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
            DumpResults(result, $" SearchMethodStaticIn {location.File.SourcePath}, '{name}'");
            return result;
        }

        private static IEnumerable<IXMemberSymbol> SearchGlobalField(XSharpSearchLocation location, string name)
        {
            var result = new List<IXMemberSymbol>();
            if (location.File == null || location.Project == null)
            {
                return result;
            }
            WriteOutputMessage($" SearchGlobalField {location.File.SourcePath},'{name}' ");
            if (location.Project.AssemblyReferences == null)
            {
                return result;
            }
            //
            var global = location.Project.FindGlobalsInAssemblyReferences(name);
            if (global != null)
            {
                result.AddRange(global);
            }
            DumpResults(result, $" SearchGlobalField {location.File.SourcePath},'{name}'");
            return result;
        }

        private static IEnumerable<IXMemberSymbol> SearchFunction(XSharpSearchLocation location, string name)
        {

            var result = new List<IXMemberSymbol>();
            if (location.File == null || location.Project == null)
            {
                return result;
            }
            WriteOutputMessage($" SearchFunction {location.File.SourcePath}, '{name}' ");
            IXMemberSymbol xMethod = location.File.Project.FindFunction(name);
            if (xMethod != null)
            {
                result.Add(xMethod);
            }
            else
            {
                var found = location.Project.FindFunctionsInAssemblyReferences(name);
                result.AddRange(found);
            }
            DumpResults(result, $" SearchFunction {location.File.SourcePath}, '{name}'");
            return result;
        }

        static void DumpResults(IEnumerable<IXSymbol> results, string heading)
        {
            if (XSettings.EnableTypelookupLog)
            {
                XSettings.LogMessage(heading + " returns " + results.Count().ToString()+" items") ;
                int i = 0;
                foreach (var result in results)
                {
                    ++i;
                    XSettings.LogMessage($"{i}: {result.Kind} {result.Prototype}");
                }
            }
        }

        static void WriteOutputMessage(string message)
        {
            if (XSettings.EnableTypelookupLog)
            {
                XSettings.LogMessage("XSharp.Lookup :" + message);
            }
        }
    }


}
