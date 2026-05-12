//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable

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

        bool XsHasClipperProblems(BindingDiagnosticBag diagnostics)
        {
            Dictionary<ReadOnlyMemory<char>, ImmutableArray<Symbol>> membersByName = GetMembersByName();
            foreach (var members in membersByName.Values)
            {
                int clipper = 0;
                int paramless = 0;
                int withparams = 0;
                Symbol clipperSym = null;
                foreach (var member in members)
                {
                    if (member is MethodSymbol method)
                    {
                        if (method.ParameterCount == 0)
                            paramless++;
                        else if (method.HasClipperCallingConvention())
                        {
                            clipper++;
                            clipperSym = method;
                        }
                        else
                        {
                            withparams++;
                        }
                    }
                }
                if (clipper > 0)
                {
                    if (withparams > 0 || paramless > 1 || clipper > 1)
                    {
                        diagnostics.Add(ErrorCode.ERR_ClipperOverloaded, clipperSym.Locations[0], clipperSym.ContainingType, clipperSym.Name);
                        return true;
                    }
                }
            }
            return false;
        }
        void XsHandlePropertyErrors(Symbol symbol, Symbol lastSym, BindingDiagnosticBag diagnostics)
        {
            // Properties that are generated from an ACCESS and ASSIGN need special handling
            // for their location. We have added a ErrorLocation property for this.
            // We also suppress the error for protected fields when the conflicting member is a property
            bool suppressError = false;
            if (symbol.Kind == SymbolKind.Property)
            {
                suppressError = lastSym.Kind == SymbolKind.Field && lastSym.DeclaredAccessibility <= Accessibility.Protected;
            }
            else if (symbol.Kind == SymbolKind.Field)
            {
                suppressError = lastSym.Kind == SymbolKind.Property && symbol.DeclaredAccessibility <= Accessibility.Protected;
            }
            if (!suppressError)
            {
                if (symbol is SourcePropertySymbol ps)
                {
                    diagnostics.Add(ErrorCode.ERR_DuplicateNameInClass, ps.ErrorLocation, this, symbol.Name);
                }
                else
                {
                    diagnostics.Add(ErrorCode.ERR_DuplicateNameInClass, symbol.Locations[0], this, symbol.Name);
                }
            }
        }

        void XsCheckVoStructMembers(MembersAndInitializers membersAndInitializers, BindingDiagnosticBag diagnostics)
        {
            if ((this as SourceNamedTypeSymbol)?.IsSourceVoStructOrUnion == true && DeclaringCompilation.Options.HasRuntime)
            {
                foreach (var m in membersAndInitializers.NonTypeMembers)
                {
                    if (m is FieldSymbol f)
                    {
                        if (f.Type.IsPointerType())
                            continue;
                        if (f.Type.SpecialType == SpecialType.System_IntPtr)
                            continue;
                        if (f.Type.SpecialType == SpecialType.System_UIntPtr)
                            continue;
                        var fType = f.Type as NamedTypeSymbol;
                        if (fType.IsVoStructOrUnion())
                            continue;
                        if (fType.IsPszType())
                            continue;
                        if (fType.IsWinBoolType())
                            continue;
                        if (fType.IsWinDateType())
                            continue;
                        if (fType.IsSymbolType())
                            continue;
                        if (fType.IsDateType())
                            continue;
                        if (f.Type.FixedBufferElementSizeInBytes() != 0)
                            continue;
                        diagnostics.Add(ErrorCode.ERR_IllegalVoStructMemberType, f.Locations[0], f.Type);
                    }
                }
            }
        }

        //ConstructorDeclarationSyntax AdjustGeneratedContructor(ConstructorDeclarationSyntax ctor, SourceMemberContainerTypeSymbol type)
        //{
        //    // not generated or inherit from nothing (OBJECT)
        //    if (!ctor.XGenerated || ctor.ParameterList.ParameterCount == 0)
        //        return ctor;
        //    var parentType = type.BaseTypeNoUseSiteDiagnostics;
        //    while (parentType is { })
        //    {
        //        // we expect exactly 1 parent ctor
        //        var ctors = parentType.GetMembers(WellKnownMemberNames.InstanceConstructorName);
        //        if (ctors.Length != 1)
        //            return ctor;
        //        var parentctor = ctors[0] as MethodSymbol;
        //        if (parentctor.MethodKind != MethodKind.Constructor)
        //            return ctor;
        //        // now check for # of parameters and type of parameters
        //        //if (parentctor.Parameters.Length == 0)
        //        //{
        //        //    // remove parameters and return
        //        //    var pars = ctor.ParameterList;
        //        //    pars = pars.Update(pars.OpenParenToken, new SeparatedSyntaxList<ParameterSyntax>(), pars.CloseParenToken);
        //        //    var init = ctor.Initializer;
        //        //    var args = init.ArgumentList;
        //        //    args = args.Update(args.OpenParenToken, new SeparatedSyntaxList<ArgumentSyntax>(), args.CloseParenToken);
        //        //    init = init.Update(init.ColonToken, init.ThisOrBaseKeyword, args);
        //        //    ctor = ctor.Update(ctor.AttributeLists, ctor.Modifiers, ctor.Identifier, pars, init, ctor.Body, ctor.SemicolonToken);
        //        //    return ctor;
        //        //}
        //        parentType = parentType.BaseTypeNoUseSiteDiagnostics;
        //    }
        //    return ctor;
        //}
    }
}
