// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Immutable;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal abstract partial class BoundTreeWalker : BoundTreeVisitor
    {
        protected BoundTreeWalker()
        {
        }

        public void VisitList<T>(ImmutableArray<T> list) where T : BoundNode
        {
            if (!list.IsDefault)
            {
                for (int i = 0; i < list.Length; i++)
                {
                    this.Visit(list[i]);
                }
            }
        }

        protected void VisitUnoptimizedForm(BoundQueryClause queryClause)
        {
            BoundExpression unoptimizedForm = queryClause.UnoptimizedForm;

            // The unoptimized form of a query has an additional argument in the call,
            // which is typically the "trivial" expression x where x is the query
            // variable.  So that we can make sense of x in this
            // context, we store the unoptimized form and visit this extra argument.
            var qc = unoptimizedForm as BoundQueryClause;
            if (qc != null) unoptimizedForm = qc.Value;
            var call = unoptimizedForm as BoundCall;
            if (call != null && (object)call.Method != null)
            {
                var arguments = call.Arguments;
#if XSHARP
                if (CaseInsensitiveComparison.Equals(call.Method.Name, "Select"))
#else
                if (call.Method.Name == "Select")
#endif
                {
                    this.Visit(arguments[arguments.Length - 1]);
                }
#if XSHARP
                else if (CaseInsensitiveComparison.Equals(call.Method.Name, "GroupBy"))
#else
                else if (call.Method.Name == "GroupBy")
#endif
                {
                    this.Visit(arguments[arguments.Length - 2]);
                }
            }
        }
    }
}
