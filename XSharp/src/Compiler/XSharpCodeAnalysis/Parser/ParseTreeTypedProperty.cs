/*
   Copyright 2016-2017 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and   
limitations under the License.
*/ 
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
