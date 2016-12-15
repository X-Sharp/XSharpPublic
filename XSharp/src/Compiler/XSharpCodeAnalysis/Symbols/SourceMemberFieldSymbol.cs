// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using Microsoft.CodeAnalysis.CSharp.Emit;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslyn.Utilities;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Threading;
using Microsoft.CodeAnalysis.Text;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;

namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    internal partial class SourceMemberFieldSymbol : SourceFieldSymbolWithSyntaxReference
    {
        internal TypeSymbol GetVOGlobalType(CSharpCompilation compilation, Binder binder)
        {
            if (compilation.Options.VOResolveTypedFunctionPointersToPtr)
            {
                var xNode = this.SyntaxNode.XNode;
                if (xNode is XP.ClassvarContext &&
                    xNode.Parent is XP.ClassVarListContext)
                {
                    var cvl = xNode.Parent as XP.ClassVarListContext;
                    if (cvl.Parent is XP.VoglobalContext)
                    {
                        var dt = cvl.DataType;
                        if (dt is XP.PtrDatatypeContext)
                        {
                            // So we have a global as typed ptr
                            // change the type from typed ptr to just ptr
                            return compilation.GetSpecialType(SpecialType.System_IntPtr);
                        }
                    }
                }
            }
            return null;
        }
    }
}
