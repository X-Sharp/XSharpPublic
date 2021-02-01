//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable

using System.Collections.Immutable;
using System.Linq;
using System.Reflection.Metadata;

namespace Microsoft.CodeAnalysis.CSharp.Symbols.Metadata.PE
{
    internal abstract partial class PENamedTypeSymbol : NamedTypeSymbol
    {
        private PropertySymbol _arrayIndexerOne = null;
        internal PropertySymbol XSharpArrayIndexerOne
        {
            // this constructs an indexer that has the __GetElement and __SetElement as getter and setter with a single dimensional (inter) parameter
            get
            {
                if (_arrayIndexerOne == null)
                {
                    EnsureAllMembersAreLoaded();

                    var moduleSymbol = this.ContainingPEModule;
                    var module = moduleSymbol.Module;

                    var getMethods = GetSimpleNonTypeMembers(ReservedNames.GetElement);
                    var setMethods = GetSimpleNonTypeMembers(ReservedNames.SetElement);

                    if (getMethods != ImmutableArray<Symbol>.Empty && setMethods != ImmutableArray<Symbol>.Empty)
                    {
                        var getOne = (from PEMethodSymbol m in getMethods where !m.Parameters[0].Type.IsArray() select m).FirstOrDefault();
                        var setOne = (from PEMethodSymbol m in setMethods where !m.Parameters[1].Type.IsArray() select m).FirstOrDefault();

                        if (getOne is { } || setOne is { })
                        {
                            PropertyDefinitionHandle h = new PropertyDefinitionHandle();
                            _arrayIndexerOne = PEPropertySymbol.Create(moduleSymbol, this, h, getOne, setOne);
                        }
                    }
                }
                return _arrayIndexerOne;
            }
        }

        private PropertySymbol _arrayIndexerMany = null;
        internal PropertySymbol XSharpArrayIndexerMany
        {
            // this constructs an indexer that has the __GetElement and __SetElement as getter and setter with a multi dimensional parameter array
            get
            {
                if (_arrayIndexerMany == null)
                {
                    EnsureAllMembersAreLoaded();

                    var moduleSymbol = this.ContainingPEModule;
                    var module = moduleSymbol.Module;

                    var getMethods = GetSimpleNonTypeMembers(ReservedNames.GetElement);
                    var setMethods = GetSimpleNonTypeMembers(ReservedNames.SetElement);

                    if (getMethods != ImmutableArray<Symbol>.Empty && setMethods != ImmutableArray<Symbol>.Empty)
                    {
                        var getMany = (from PEMethodSymbol m in getMethods where m.Parameters[0].Type.IsArray() select m).FirstOrDefault();
                        var setMany = (from PEMethodSymbol m in setMethods where m.Parameters[1].Type.IsArray() select m).FirstOrDefault();
                        if (getMany is { } || setMany is { })
                        {
                            PropertyDefinitionHandle h = new PropertyDefinitionHandle();
                            _arrayIndexerMany = PEPropertySymbol.Create(moduleSymbol, this, h, getMany, setMany);
                        }
                    }
                }
                return _arrayIndexerMany;
            }
        }

    }
}
