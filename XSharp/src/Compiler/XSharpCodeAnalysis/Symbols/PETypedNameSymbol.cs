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

        internal override bool HasCompilerGeneratedAttribute
        {
            get
            {
                var uncommon = GetUncommonProperties();
                if (uncommon == s_noUncommonProperties)
                {
                    return false;
                }

                if (!uncommon.lazyHasCompilerGeneratedAttribute.HasValue())
                {
                    uncommon.lazyHasCompilerGeneratedAttribute = ContainingPEModule.Module.HasCompilerGeneratedAttribute(_handle).ToThreeState();
                }

                return uncommon.lazyHasCompilerGeneratedAttribute.Value();
            }
        }
        internal bool IsVoStruct
        {
            get
            {
                var uncommon = GetUncommonProperties();
                if (uncommon == s_noUncommonProperties)
                {
                    return false;
                }

                if (!uncommon.lazyVoStruct.HasValue())
                {
                    uncommon.lazyVoStruct = ThreeState.False;
                    var attrs = this.GetAttributes();
                    foreach (var attr in attrs)
                    {
                        var atype = attr.AttributeClass;
                        if (atype.IsOurAttribute(OurTypeNames.VOStructAttribute))
                        {
                            uncommon.lazyVoStruct = ThreeState.True;
                            break;
                        }
                    }
                }

                return uncommon.lazyVoStruct.Value();
            }
        }

        internal int VoStructSizeInBytes
        {
            get
            {
                if (!IsVoStruct)
                    return 0;
                var uncommon = GetUncommonProperties();
                if (uncommon == s_noUncommonProperties)
                {
                    return 0;
                }
                if (uncommon.lazyVostructSize.HasValue)
                    return uncommon.lazyVostructSize.Value;

                uncommon.lazyVostructSize = 0;
                var attrs = GetAttributes();
                foreach (var attr in attrs)
                {
                    var atype = attr.AttributeClass;
                    if (atype.IsOurAttribute(OurTypeNames.VOStructAttribute))
                    {
                        if (attr.ConstructorArguments != null)
                        {
                            uncommon.lazyVostructSize = attr.ConstructorArguments.First().DecodeValue<int>(SpecialType.System_Int32);
                            break;
                        }
                    }
                }
                return uncommon.lazyVostructSize.Value;
            }
        }
        internal int VoStructLargestElementSize
        {
            get
            {
                if (!IsVoStruct)
                    return 0;
                var uncommon = GetUncommonProperties();
                if (uncommon == s_noUncommonProperties)
                {
                    return 0;
                }
                if (uncommon.lazyVostructLargestElementSize.HasValue)
                    return uncommon.lazyVostructLargestElementSize.Value;
                uncommon.lazyVostructLargestElementSize = 0;
                var attrs = GetAttributes();
                foreach (var attr in attrs)
                {
                    var atype = attr.AttributeClass;
                    if (atype.IsOurAttribute(OurTypeNames.VOStructAttribute))
                    {
                        if (attr.ConstructorArguments != null)
                        {
                            uncommon.lazyVostructLargestElementSize = attr.ConstructorArguments.Last().DecodeValue<int>(SpecialType.System_Int32);
                            break;
                        }
                    }
                }
                return uncommon.lazyVostructLargestElementSize.Value;
            }
        }
    }

}
