using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;
using Microsoft.Cci;
using Microsoft.CodeAnalysis.CSharp.Emit;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.PooledObjects;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    [DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
    internal sealed class XsVariableSymbol : PropertySymbol
    {
        private readonly string _name;
        private readonly string _alias;
        private readonly MethodSymbol _getMethod;
        private readonly MethodSymbol _setMethod;
        private TypeSymbol _type;

        internal XsVariableSymbol(string name, MethodSymbol getMethod, MethodSymbol setMethod, TypeSymbol type)
        {
            _name = name;
            _getMethod = getMethod;
            _setMethod = setMethod;
            _type = type;
        }
        internal XsVariableSymbol(string alias, string name, MethodSymbol getMethod, MethodSymbol setMethod, TypeSymbol type)
        {
            _alias = alias;
            _name = name;
            _getMethod = getMethod;
            _setMethod = setMethod;
            _type = type;
        }

        public override RefKind RefKind => RefKind.None;

        public override TypeSymbol Type => _type;
        public override string Name => _name;
        public string Alias => _alias;
        public bool HasAlias => !string.IsNullOrEmpty(_alias);

        public override ImmutableArray<CustomModifier> TypeCustomModifiers => ImmutableArray<CustomModifier>.Empty;

        public override ImmutableArray<CustomModifier> RefCustomModifiers => ImmutableArray<CustomModifier>.Empty;

        public override ImmutableArray<ParameterSymbol> Parameters => ImmutableArray<ParameterSymbol>.Empty;

        public override bool IsIndexer => false;

        public override MethodSymbol GetMethod => _getMethod;

        public override MethodSymbol SetMethod => _setMethod;

        public override ImmutableArray<PropertySymbol> ExplicitInterfaceImplementations => ImmutableArray<PropertySymbol>.Empty;

        public override Symbol ContainingSymbol => throw new System.NotImplementedException();

        public override ImmutableArray<Location> Locations => throw new System.NotImplementedException();

        public override ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => throw new System.NotImplementedException();

        public override Accessibility DeclaredAccessibility => Accessibility.Public;

        public override bool IsStatic => true;

        public override bool IsVirtual => false;

        public override bool IsOverride => false;

        public override bool IsAbstract => false;

        public override bool IsSealed => true;

        public override bool IsExtern => false;

        internal override bool HasSpecialName => true;

        internal override CallingConvention CallingConvention => CallingConvention.Default;

        internal override bool MustCallMethodsDirectly => true;

        internal override ObsoleteAttributeData ObsoleteAttributeData => throw new System.NotImplementedException();
        internal new string GetDebuggerDisplay()
        {
            if (string.IsNullOrEmpty(Alias))
            {
                return Name;
            }
            return Alias + "->" + Name;
        }
    }
}
