// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

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
    }
}
