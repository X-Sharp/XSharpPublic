//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using InternalSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

namespace Antlr4.Runtime.Tree
{
    internal class ParseTreeTypedProperty
    {
        protected internal ConcurrentDictionary<IParseTree, object> annotations = new ConcurrentDictionary<IParseTree, object>(); 

        public virtual T Get<T>(IParseTree node)
        {
            object value;
            if (!annotations.TryGetValue(node, out value))
                return default(T);

            return (T)value;
        }

        public virtual void Put<T>(IParseTree node, T value)
        {
            annotations[node] = value;
        }

        public virtual T RemoveFrom<T>(IParseTree node)
        {
            object value;
            if (!annotations.TryRemove(node, out value))
                return default(T);

            return (T)value;
        }
    }
}
