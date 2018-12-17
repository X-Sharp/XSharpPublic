// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    internal sealed partial class AnonymousTypeManager
    {
        /// <summary>
        /// Represents an anonymous type 'public' symbol which is used in binding and lowering.
        /// In emit phase it is being substituted with implementation symbol.
        /// </summary>
#if XSHARP
        private class AnonymousTypePublicSymbol : NamedTypeSymbol
#else
        private sealed class AnonymousTypePublicSymbol : NamedTypeSymbol
#endif
        {
#if XSHARP
            internal readonly ImmutableArray<Symbol> _members;
#else
            private readonly ImmutableArray<Symbol> _members;
#endif

            /// <summary> Properties defined in the type </summary>
            internal readonly ImmutableArray<AnonymousTypePropertySymbol> Properties;

            /// <summary> Maps member names to symbol(s) </summary>
#if XSHARP
            internal readonly MultiDictionary<string, Symbol> _nameToSymbols = new MultiDictionary<string, Symbol>(CaseInsensitiveComparison.Comparer);
#else
            private readonly MultiDictionary<string, Symbol> _nameToSymbols = new MultiDictionary<string, Symbol>();
#endif

            /// <summary> Anonymous type manager owning this template </summary>
            internal readonly AnonymousTypeManager Manager;

            /// <summary> Anonymous type descriptor </summary>
            internal readonly AnonymousTypeDescriptor TypeDescriptor;

            internal AnonymousTypePublicSymbol(AnonymousTypeManager manager, AnonymousTypeDescriptor typeDescr)
            {
                typeDescr.AssertIsGood();

                this.Manager = manager;
                this.TypeDescriptor = typeDescr;

                int fieldsCount = typeDescr.Fields.Length;

                //  members
                Symbol[] members = new Symbol[fieldsCount * 2 + 1];
                int memberIndex = 0;

                // The array storing property symbols to be used in 
                // generation of constructor and other methods
                if (fieldsCount > 0)
                {
                    AnonymousTypePropertySymbol[] propertiesArray = new AnonymousTypePropertySymbol[fieldsCount];

                    // Process fields
                    for (int fieldIndex = 0; fieldIndex < fieldsCount; fieldIndex++)
                    {
                        // Add a property
                        AnonymousTypePropertySymbol property = new AnonymousTypePropertySymbol(this, typeDescr.Fields[fieldIndex]);
                        propertiesArray[fieldIndex] = property;

                        // Property related symbols
                        members[memberIndex++] = property;
                        members[memberIndex++] = property.GetMethod;
                    }

                    this.Properties = propertiesArray.AsImmutableOrNull();
                }
                else
                {
                    this.Properties = ImmutableArray<AnonymousTypePropertySymbol>.Empty;
                }

                // Add a constructor
                members[memberIndex++] = new AnonymousTypeConstructorSymbol(this, this.Properties);
                _members = members.AsImmutableOrNull();
                Debug.Assert(memberIndex == _members.Length);

                //  fill nameToSymbols map
                foreach (var symbol in _members)
                {
                    _nameToSymbols.Add(symbol.Name, symbol);
                }
            }
#if XSHARP
            internal bool IsCodeblock { get; } = false;

            protected AnonymousTypePublicSymbol(AnonymousTypeManager manager, TypeSymbol[] codeblockParams, Location location)
            {
                Debug.Assert(codeblockParams.Length > 0);

                this.Manager = manager;
                this.TypeDescriptor = new AnonymousTypeDescriptor(codeblockParams.Select((t, i) => new AnonymousTypeField("Cb$Param$" + i, location, t))
                    .ToImmutableArray(), location);
                var codeblockDelegate = manager.SynthesizeDelegate(codeblockParams.Length-1, default(BitVector), false, 0).Construct(codeblockParams);
                this.Properties = new[] { new AnonymousTypePropertySymbol(this, new AnonymousTypeField("Cb$Eval$", location, codeblockDelegate)) }.AsImmutableOrNull();

                Symbol[] members = new Symbol[3];
                int memberIndex = 0;
                members[memberIndex++] = Properties[0];
                members[memberIndex++] = new AnonymousTypeConstructorSymbol(this, this.Properties);
                members[memberIndex++] = new CodeblockEvalMethod(this);

                _members = members.AsImmutableOrNull();
                Debug.Assert(memberIndex == _members.Length);

                //  fill nameToSymbols map
                foreach (var symbol in _members)
                {
                    _nameToSymbols.Add(symbol.Name, symbol);
                }

                IsCodeblock = true;
            }
#endif

            public override ImmutableArray<Symbol> GetMembers()
            {
                return _members;
            }

            internal override IEnumerable<FieldSymbol> GetFieldsToEmit()
            {
                throw ExceptionUtilities.Unreachable;
            }

            internal override ImmutableArray<TypeSymbol> TypeArgumentsNoUseSiteDiagnostics
            {
                get { return ImmutableArray<TypeSymbol>.Empty; }
            }

            internal override bool HasTypeArgumentsCustomModifiers
            {
                get
                {
                    return false;
                }
            }

            internal override bool HasCodeAnalysisEmbeddedAttribute => false;

            public override ImmutableArray<CustomModifier> GetTypeArgumentCustomModifiers(int ordinal)
            {
                return GetEmptyTypeArgumentCustomModifiers(ordinal);
            }

            public override ImmutableArray<Symbol> GetMembers(string name)
            {
                var symbols = _nameToSymbols[name];
                var builder = ArrayBuilder<Symbol>.GetInstance(symbols.Count);
                foreach (var symbol in symbols)
                {
                    builder.Add(symbol);
                }

                return builder.ToImmutableAndFree();
            }

            internal override ImmutableArray<Symbol> GetEarlyAttributeDecodingMembers()
            {
                return this.GetMembersUnordered();
            }

            internal override ImmutableArray<Symbol> GetEarlyAttributeDecodingMembers(string name)
            {
                return this.GetMembers(name);
            }

            public override IEnumerable<string> MemberNames
            {
                get { return _nameToSymbols.Keys; }
            }

            public override Symbol ContainingSymbol
            {
                get { return this.Manager.Compilation.SourceModule.GlobalNamespace; }
            }

            public override string Name
            {
                get { return string.Empty; }
            }

            public override string MetadataName
            {
                get { return string.Empty; }
            }

            internal override bool MangleName
            {
                get { return false; }
            }

            public override int Arity
            {
                get { return 0; }
            }

            public override bool IsImplicitlyDeclared
            {
                get { return false; }
            }

            public override ImmutableArray<TypeParameterSymbol> TypeParameters
            {
                get { return ImmutableArray<TypeParameterSymbol>.Empty; }
            }

            public override bool IsAbstract
            {
                get { return false; }
            }

            internal sealed override bool IsByRefLikeType
            {
                get { return false;  }
            }

            internal sealed override bool IsReadOnly
            {
                get { return false; }
            }

            public override bool IsSealed
            {
                get { return true; }
            }

            public override bool MightContainExtensionMethods
            {
                get { return false; }
            }

            internal override bool HasSpecialName
            {
                get { return false; }
            }

            public override ImmutableArray<NamedTypeSymbol> GetTypeMembers()
            {
                return ImmutableArray<NamedTypeSymbol>.Empty;
            }

            public override ImmutableArray<NamedTypeSymbol> GetTypeMembers(string name)
            {
                return ImmutableArray<NamedTypeSymbol>.Empty;
            }

            public override ImmutableArray<NamedTypeSymbol> GetTypeMembers(string name, int arity)
            {
                return ImmutableArray<NamedTypeSymbol>.Empty;
            }

            public override Accessibility DeclaredAccessibility
            {
                get { return Accessibility.Internal; }
            }

            internal override ImmutableArray<NamedTypeSymbol> InterfacesNoUseSiteDiagnostics(ConsList<Symbol> basesBeingResolved)
            {
                return ImmutableArray<NamedTypeSymbol>.Empty;
            }

            internal override ImmutableArray<NamedTypeSymbol> GetInterfacesToEmit()
            {
                throw ExceptionUtilities.Unreachable;
            }

            internal override NamedTypeSymbol BaseTypeNoUseSiteDiagnostics
            {
                get { return this.Manager.System_Object; }
            }

            public override TypeKind TypeKind
            {
                get { return TypeKind.Class; }
            }

            internal override bool IsInterface
            {
                get { return false; }
            }

            public override ImmutableArray<Location> Locations
            {
                get { return ImmutableArray.Create<Location>(this.TypeDescriptor.Location); }
            }

            public override ImmutableArray<SyntaxReference> DeclaringSyntaxReferences
            {
                get
                {
                    return GetDeclaringSyntaxReferenceHelper<AnonymousObjectCreationExpressionSyntax>(this.Locations);
                }
            }

            public override bool IsStatic
            {
                get { return false; }
            }

            public override bool IsAnonymousType
            {
                get { return true; }
            }

            public override NamedTypeSymbol ConstructedFrom
            {
                get { return this; }
            }

            internal override bool ShouldAddWinRTMembers
            {
                get { return false; }
            }

            internal override bool IsWindowsRuntimeImport
            {
                get { return false; }
            }

            internal override bool IsComImport
            {
                get { return false; }
            }

            internal sealed override ObsoleteAttributeData ObsoleteAttributeData
            {
                get { return null; }
            }

            internal override TypeLayout Layout
            {
                get { return default(TypeLayout); }
            }

            internal override CharSet MarshallingCharSet
            {
                get { return DefaultMarshallingCharSet; }
            }

            public override bool IsSerializable
            {
                get { return false; }
            }

            internal override bool HasDeclarativeSecurity
            {
                get { return false; }
            }

            internal override IEnumerable<Microsoft.Cci.SecurityAttribute> GetSecurityInformation()
            {
                throw ExceptionUtilities.Unreachable;
            }

            internal override ImmutableArray<string> GetAppliedConditionalSymbols()
            {
                return ImmutableArray<string>.Empty;
            }

            internal override AttributeUsageInfo GetAttributeUsageInfo()
            {
                return AttributeUsageInfo.Null;
            }

            internal override NamedTypeSymbol GetDeclaredBaseType(ConsList<Symbol> basesBeingResolved)
            {
                return this.Manager.System_Object;
            }

            internal override ImmutableArray<NamedTypeSymbol> GetDeclaredInterfaces(ConsList<Symbol> basesBeingResolved)
            {
                return ImmutableArray<NamedTypeSymbol>.Empty;
            }

            internal override bool Equals(TypeSymbol t2, TypeCompareKind comparison)
            {
                if (ReferenceEquals(this, t2))
                {
                    return true;
                }

                var other = t2 as AnonymousTypePublicSymbol;
                return (object)other != null && this.TypeDescriptor.Equals(other.TypeDescriptor, comparison);
            }

            public override int GetHashCode()
            {
                return this.TypeDescriptor.GetHashCode();
            }
        }
    }
}
