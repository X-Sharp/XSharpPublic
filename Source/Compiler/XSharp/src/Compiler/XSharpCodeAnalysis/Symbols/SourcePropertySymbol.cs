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
using Microsoft.CodeAnalysis.PooledObjects;
using Roslyn.Utilities;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;

namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    internal partial class SourcePropertyAccessorSymbol
    {
        private TypeWithAnnotations _changedReturnType = default;
        private ImmutableArray<ParameterSymbol> _changedParameters = default;
        private bool _signatureChanged = false;
        private MethodSymbol? _parentMethod;

        internal void ChangeSignature(TypeWithAnnotations returnType, ImmutableArray<ParameterSymbol> parameters)
        {
            _changedReturnType = returnType;

            var newparameters = ArrayBuilder<ParameterSymbol>.GetInstance(parameters.Length);
            foreach (var p in parameters)
            {
                var type = TypeWithAnnotations.Create(p.Type);
                newparameters.Add(new SynthesizedAccessorValueParameterSymbol(this, type, newparameters.Count));

            }
            _changedParameters = newparameters.ToImmutableAndFree();
            _signatureChanged = true;
            this.DeclarationModifiers |= DeclarationModifiers.Override;
            this.flags = new Flags(flags.MethodKind, this.DeclarationModifiers, this.ReturnsVoid, flags.IsExtensionMethod, flags.IsNullableAnalysisEnabled, flags.IsMetadataVirtual());

        }
        internal bool RemoveModifier(DeclarationModifiers mod)
        {
            var ent = this._property.GetEntity();
            bool Ok = true;
            if (ent != null)
            {
                if (mod == DeclarationModifiers.Override && ent.Data.HasExplicitOverride)
                    Ok = false;
                if (mod == DeclarationModifiers.Virtual && ent.Data.HasExplicitVirtual)
                    Ok = false;
            }
            if (Ok)
            {
                this.DeclarationModifiers &= ~mod;
            }
            return Ok;
        }
        internal void SetOverriddenMethod(MethodSymbol m)
        {
            _parentMethod = m;
        }

        public override MethodSymbol OverriddenMethod
        {
            get
            {
                if (_parentMethod != null)
                    return _parentMethod;
                return base.OverriddenMethod;
            }
        }
    }

    internal static class SourceExtensions
    {
        internal static XP.IMemberContext? GetEntity(this SourcePropertySymbolBase prop)
        {
            XP.IMemberContext? result = null;
            var node = prop.CSharpSyntaxNode.XNode;
            if (node is XP.IMemberContext ent)
                result = ent;
            else if (node.GetChild(0) is XP.IMemberContext entchild)
                result = entchild;
            return result;
        }
        internal static XP.IMemberContext? GetEntity(this SourceEventSymbol evt)
        {
            XP.IMemberContext? result = null;
            var node = evt.CSharpSyntaxNode.XNode;
            if (node is XP.IMemberContext ent)
                result = ent;
            else if (node.GetChild(0) is XP.IMemberContext entchild)
                result = entchild;
            return result;
        }
    }

    internal abstract partial class SourcePropertySymbolBase
    {
        internal bool RemoveModifier(DeclarationModifiers mod)
        {
            var ent = this.GetEntity();
            bool Ok = true;
            if (ent != null)
            {
                if (mod == DeclarationModifiers.Override && ent.Data.HasExplicitOverride)
                    Ok = false;
                if (mod == DeclarationModifiers.Virtual && ent.Data.HasExplicitVirtual)
                    Ok = false;
            }
            if (Ok)
            {
                this._modifiers &= ~mod;
                if (this.GetMethod is SourcePropertyAccessorSymbol gm)
                {
                    gm.RemoveModifier(mod);
                }
                if (this.SetMethod is SourcePropertyAccessorSymbol sm)
                {
                    sm.RemoveModifier(mod);
                }
            }
            return Ok;
        }
    }
    internal sealed partial class SourcePropertySymbol
    {
        private TypeWithAnnotations _newPropertyType = default;

        public override bool IsIndexedProperty
        {
            get { return _isIndexedProperty; }
        }
        // The error location for properties generated from an ACCESS and ASSIGN is read from the first GET or SET method.
        internal Location ErrorLocation
        {
            get
            {
                if (_IsGeneratedFromAccessAssign)
                {
                    if (GetMethod != null)
                    {
                        var xnode = GetMethod.GetNonNullSyntaxNode().XNode;
                        if (xnode != null)
                        {
                            var src = GetMethod as SourceMethodSymbol;
                            return xnode.GetLocation(src?.DeclaringSyntaxReferences.First().SyntaxTree);
                        }
                    }
                    if (SetMethod != null)
                    {
                        var xnode = SetMethod.GetNonNullSyntaxNode().XNode;
                        if (xnode != null)
                        {
                            var src = SetMethod as SourceMethodSymbol;
                            return xnode.GetLocation(src?.DeclaringSyntaxReferences.First().SyntaxTree);
                        }
                    }
                }
                return base.Location;
            }
        }

        public override bool IsIndexer
        {
            get { return base.IsIndexer && !_isIndexedProperty; }
        }

        internal override void validateProperty(PropertySymbol overriddenProperty, DiagnosticBag diagnostics)
        {
            if (overriddenProperty == null && XSharpString.CaseSensitive)
            {
                // check if we have a base type and if the base type has a method with the same name but different casing
                var baseType = this.ContainingType.BaseTypeNoUseSiteDiagnostics;
                var members = baseType.GetMembersUnordered().Where(
                        member => member.Kind == SymbolKind.Property && member.IsVirtual && string.Equals(member.Name, this.Name, StringComparison.OrdinalIgnoreCase));
                if (members.Count() > 0)
                {
                    foreach (var member in members)
                    {
                        if (member is PropertySymbol propSym)
                        {
                            bool equalSignature = propSym.ParameterCount == this.ParameterCount && Equals(this.Type, propSym.Type);
                            if (equalSignature)
                            {
                                var thisTypes = this.Parameters;
                                var theirTypes = propSym.Parameters;
                                for (int i = 0; i < thisTypes.Length; i++)
                                {
                                    if (!Equals(thisTypes[i].Type, theirTypes[i].Type))
                                    {
                                        equalSignature = false;
                                        break;
                                    }
                                }
                            }
                            if (equalSignature)
                            {
                                diagnostics.Add(ErrorCode.ERR_CaseDifference, this.Location, baseType.Name, "property", member.Name, this.Name);
                            }
                        }

                    }
                }

            }
            return;
        }
        internal void SetChangedParentType(TypeWithAnnotations type)
        {
            _newPropertyType = type;
            if (this.GetMethod != null && this.OverriddenProperty.GetMethod != null)
            {
                var m = (SourcePropertyAccessorSymbol)this.GetMethod;
                var m2 = this.OverriddenProperty.GetMethod;
                m.ChangeSignature(TypeWithAnnotations.Create(m2.ReturnType), m2.Parameters);
                m.SetOverriddenMethod(m2);
            }
            if (this.SetMethod != null && this.OverriddenProperty.SetMethod != null)
            {
                var m = (SourcePropertyAccessorSymbol)this.SetMethod;
                var m2 = this.OverriddenProperty.SetMethod;
                m.ChangeSignature(TypeWithAnnotations.Create(m2.ReturnType), m2.Parameters);
                m.SetOverriddenMethod(m2);
            }
        }
    }

}
