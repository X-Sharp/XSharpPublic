//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;
using System.Threading;
using Microsoft.CodeAnalysis.Collections;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    /// <summary>
    /// Represents a named type symbol whose members are declared in source.
    /// </summary>
    internal abstract partial class SourceMemberContainerTypeSymbol : NamedTypeSymbol
    {
        ConstructorDeclarationSyntax AdjustGeneratedContructor(ConstructorDeclarationSyntax ctor, SourceMemberContainerTypeSymbol type)
        {
            // not generated or inherit from nothing (OBJECT)
            if (!ctor.XGenerated || ctor.ParameterList.ParameterCount == 0)
                return ctor;
            var parentType = type.BaseTypeNoUseSiteDiagnostics;
            while (parentType != null)
            {
                // we expect exactly 1 parent ctor
                var ctors = parentType.GetMembers(".ctor");
                if (ctors.Length != 1)
                    return ctor;
                var parentctor = ctors[0] as MethodSymbol;
                if (parentctor.MethodKind != MethodKind.Constructor)
                    return ctor;
                // now check for # of parameters and type of parameters
                //if (parentctor.Parameters.Length == 0)
                //{
                //    // remove parameters and return
                //    var pars = ctor.ParameterList;
                //    pars = pars.Update(pars.OpenParenToken, new SeparatedSyntaxList<ParameterSyntax>(), pars.CloseParenToken);
                //    var init = ctor.Initializer;
                //    var args = init.ArgumentList;
                //    args = args.Update(args.OpenParenToken, new SeparatedSyntaxList<ArgumentSyntax>(), args.CloseParenToken);
                //    init = init.Update(init.ColonToken, init.ThisOrBaseKeyword, args);
                //    ctor = ctor.Update(ctor.AttributeLists, ctor.Modifiers, ctor.Identifier, pars, init, ctor.Body, ctor.SemicolonToken);
                //    return ctor;
                //}

                parentType = parentType.BaseTypeNoUseSiteDiagnostics;
            }


            return ctor;
        }
    }
}
