// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using System.Collections.Generic;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal sealed partial class OverloadResolution
    {
        private bool VOBetterFunctionMember<TMember>(
            MemberResolutionResult<TMember> m1,
            MemberResolutionResult<TMember> m2,
            out BetterResult result)
            where TMember : Symbol
        {
            result = BetterResult.Neither;
            // Prefer the member not declared in VulcanRT, if applicable
            if (Compilation.Options.IsDialectVO )
            {
                var asm1 = m1.Member.ContainingAssembly;
                var asm2 = m2.Member.ContainingAssembly;
                if (asm1 != asm2)
                {
                    if (asm1.IsVulcanRT())
                    {
                        result = BetterResult.Right;
                        return true;
                    }
                    else if (asm2.IsVulcanRT())
                    {
                        result = BetterResult.Left;
                        return true;
                    }
                    // prefer functions/method in the current assembly over external methods
                    if (asm1.IsFromCompilation(Compilation))
                    {
                        result = BetterResult.Left;
                        return true;
                    }
                    if (asm2.IsFromCompilation(Compilation))
                    {
                        result = BetterResult.Right;
                        return true;
                    }
                }
            }
            return false;
        }

        //private bool VOBetterConversionFromExpression(BoundExpression node, TypeSymbol t1, TypeSymbol t2, out BetterResult result )
        //{
        //    // Check for SignedNess
        //    result = BetterResult.Neither;
        //    if (Compilation.Options.IsDialectVO && node.Type != null)
        //    {
        //        if (node.Type.SpecialType.IsIntegralType())
        //        {
        //            if (node.Type.SpecialType.IsSignedIntegralType())
        //            {
        //                if (t1.SpecialType.IsSignedIntegralType())
        //                {
        //                    result = BetterResult.Left;
        //                    return true;
        //                }
        //                if (t2.SpecialType.IsSignedIntegralType())
        //                {
        //                    result = BetterResult.Right;
        //                    return true;
        //                }
        //            }
        //            else
        //            {
        //                if (!t1.SpecialType.IsSignedIntegralType())
        //                {
        //                     result = BetterResult.Left;
        //                    return true;
        //                }
        //                if (!t2.SpecialType.IsSignedIntegralType())
        //                {
        //                    result = BetterResult.Right;
        //                    return true;
        //                }
        //            }
        //        }
        //    }
        //    return false;
        //}

    }
}