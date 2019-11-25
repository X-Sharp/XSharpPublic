//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections.Immutable;
using System.Linq;
using System.Reflection.Metadata;

namespace Microsoft.CodeAnalysis.CSharp.Symbols.Metadata.PE
{
    internal abstract partial class PENamedTypeSymbol : NamedTypeSymbol
    {
        private PropertySymbol _vulcanArrayIndexerOne = null;
        internal PropertySymbol VulcanArrayIndexerOne
        {
            // this constructs an indexer that has the __GetElement and __SetElement as getter and setter with a single dimensional (inter) parameter
            get
            {
                if (_vulcanArrayIndexerOne == null)
                {
                    EnsureAllMembersAreLoaded();

                    var moduleSymbol = this.ContainingPEModule;
                    var module = moduleSymbol.Module;

                    var getMethods = GetSimpleNonTypeMembers(ReservedNames.GetElement);
                    var setMethods = GetSimpleNonTypeMembers(ReservedNames.SetElement);

                    if (getMethods != ImmutableArray<Symbol>.Empty && setMethods != ImmutableArray<Symbol>.Empty)
                    {
                        var getOne = (from PEMethodSymbol m in getMethods where !m.ParameterTypes[0].IsArray() select m).FirstOrDefault();
                        var setOne = (from PEMethodSymbol m in setMethods where !m.ParameterTypes[1].IsArray() select m).FirstOrDefault();

                        if (((object)getOne != null) || ((object)setOne != null))
                        {
                            PropertyDefinitionHandle h = new PropertyDefinitionHandle();
                            _vulcanArrayIndexerOne = PEPropertySymbol.Create(moduleSymbol, this, h, getOne, setOne);
                        }
                    }
                }
                return _vulcanArrayIndexerOne;
            }
        }

        private PropertySymbol _vulcanArrayIndexerMany = null;
        internal PropertySymbol VulcanArrayIndexerMany
        {
            // this constructs an indexer that has the __GetElement and __SetElement as getter and setter with a multi dimensional parameter array
            get
            {
                if (_vulcanArrayIndexerMany == null)
                {
                    EnsureAllMembersAreLoaded();

                    var moduleSymbol = this.ContainingPEModule;
                    var module = moduleSymbol.Module;

                    var getMethods = GetSimpleNonTypeMembers(ReservedNames.GetElement);
                    var setMethods = GetSimpleNonTypeMembers(ReservedNames.SetElement);

                    if (getMethods != ImmutableArray<Symbol>.Empty && setMethods != ImmutableArray<Symbol>.Empty)
                    {
                        var getMany = (from PEMethodSymbol m in getMethods where m.ParameterTypes[0].IsArray() select m).FirstOrDefault();
                        var setMany = (from PEMethodSymbol m in setMethods where m.ParameterTypes[1].IsArray() select m).FirstOrDefault();
                        if (((object)getMany != null) || ((object)setMany != null))
                        {
                            PropertyDefinitionHandle h = new PropertyDefinitionHandle();
                            _vulcanArrayIndexerMany = PEPropertySymbol.Create(moduleSymbol, this, h, getMany, setMany);
                        }
                    }
                }
                return _vulcanArrayIndexerMany;
            }
        }

    }
}
