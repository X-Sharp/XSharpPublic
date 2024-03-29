﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

namespace Microsoft.CodeAnalysis.CSharp.Syntax
{
    public abstract partial class TypeSyntax
    {
#if XSHARP	
        public bool IsVar
        {
            get
            {

                var ts = this.Green as InternalSyntax.IdentifierNameSyntax;
                if (ts != null )
                { 
                    return ts.Identifier.ToString() == XSharpSpecialNames.ImpliedTypeName;
                }
                return ((InternalSyntax.TypeSyntax)this.Green).IsVar;
            }
        }
#else
        public bool IsVar => ((InternalSyntax.TypeSyntax)this.Green).IsVar;
#endif
        public bool IsUnmanaged => ((InternalSyntax.TypeSyntax)this.Green).IsUnmanaged;

        public bool IsNotNull => ((InternalSyntax.TypeSyntax)this.Green).IsNotNull;

        public bool IsNint => ((InternalSyntax.TypeSyntax)this.Green).IsNint;

        public bool IsNuint => ((InternalSyntax.TypeSyntax)this.Green).IsNuint;
    }
}
