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
    internal  sealed partial class SourceOrdinaryMethodSymbol : SourceMemberMethodSymbol
    {

        private bool XsGenerateDebugInfo
        {
            get
            {
                if (IsAsync || IsIterator)
                {
                    return false;
                }
                var node = this.GetNonNullSyntaxNode();
                if (node.XGenerated)
                    return false;
                return true;
            }
        }

        private int _overrideState = 0; // 0 = unchecked, 1 = checked and false, 2 = checked and true
        public sealed override bool IsOverride
        {
            get
            {
                return (this.DeclarationModifiers & DeclarationModifiers.Override) != 0 && _overrideState != 1;
            }
        }

        internal MethodSymbol validateMethod(MethodSymbol overriddenMethod, DiagnosticBag diagnostics, Location location)
        {
            if ((object)overriddenMethod != null)
            {
                if (this.HasClipperCallingConvention() != overriddenMethod.HasClipperCallingConvention())
                {
                    if (this.HasClipperCallingConvention())
                    {
                        diagnostics.Add(ErrorCode.ERR_ClipperInSubClass, location, this.Name);
                    }
                    else
                    {
                        diagnostics.Add(ErrorCode.ERR_ClipperInParentClass, location, this.Name);
                    }
                    overriddenMethod = null;
                }
            }
            if ((object)overriddenMethod != null)
                _overrideState = 2;  // checked and has a (correct) override
            else
                _overrideState = 1; // checked and does not have an override

            if ((object)overriddenMethod != null)
            {
                CustomModifierUtils.CopyMethodCustomModifiers(overriddenMethod, this, out _lazyReturnType,
                                                              out _lazyCustomModifiers,
                                                              out _lazyParameters, alsoCopyParamsModifier: true);
            }
            var node = this.SyntaxNode.Green as Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax.MethodDeclarationSyntax;
            var mods = flags.DeclarationModifiers;
            if ((object)overriddenMethod != null)
            {
                if (this.Name != overriddenMethod.Name)
                {
                    this._name = overriddenMethod.Name;
                }
                // remove generated Virtual Modifiers
                foreach (var token in node.Modifiers)
                {
                    if (token.Kind == SyntaxKind.VirtualKeyword && token.XGenerated)
                    {
                        mods = mods & ~DeclarationModifiers.Virtual;
                    }
                }

                flags = new Flags(flags.MethodKind, mods, flags.ReturnsVoid, flags.IsExtensionMethod, flags.IsMetadataVirtual(true));
            }
            else
            {
                // remove generated Override Modifiers
                foreach (var token in node.Modifiers)
                {
                    if (token.Kind == SyntaxKind.OverrideKeyword && token.XGenerated)
                    {
                        mods = mods & ~DeclarationModifiers.Override;
                    }
                }
                flags = new Flags(flags.MethodKind, mods, flags.ReturnsVoid, flags.IsExtensionMethod, flags.IsMetadataVirtual(true));
            }

            return overriddenMethod;
        }
    }

}
