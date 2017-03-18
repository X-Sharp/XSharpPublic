// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.CodeAnalysis.CSharp.Syntax
{
    public abstract partial class TypeSyntax
    {
        public bool IsVar
        {
            get
            {
#if XSHARP
                var ts = this.Green as InternalSyntax.IdentifierNameSyntax;
                return ts != null && ts.Identifier.ToString() == XSharpSpecialNames.ImpliedTypeName;
#else
                return ((InternalSyntax.TypeSyntax)this.Green).IsVar;
#endif
            }
        }
    }
}
