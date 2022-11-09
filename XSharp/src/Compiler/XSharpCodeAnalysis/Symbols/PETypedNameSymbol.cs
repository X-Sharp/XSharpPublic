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
        internal bool HasCompilerGlobalScopeAttribute
        {
            get
            {
                var uncommon = GetUncommonProperties();
                if (uncommon == s_noUncommonProperties)
                {
                    return false;
                }

                if (!uncommon.lazyHasCompilerGlobalScopeAttribute.HasValue())
                {
                    uncommon.lazyHasCompilerGlobalScopeAttribute = ContainingPEModule.Module.HasCompilerGlobalScopeAttribute(_handle).ToThreeState();
                }

                return uncommon.lazyHasCompilerGlobalScopeAttribute.Value();
            }
        }
        bool _hasVoStructValues = false;
        bool _isVoStruct = false;
        int _voStructSize = 0;
        int _voStructLargestElement = 0;

        private void ReadVoStructValues()
        {
            lock (this)
            {
                if (_hasVoStructValues)
                    return;
                var attrs = this.GetAttributes();
                foreach (var attr in attrs)
                {
                    var atype = attr.AttributeClass;
                    if (atype.IsOurAttribute(OurTypeNames.VOStructAttribute))
                    {
                        _isVoStruct = true;
                        if (attr.ConstructorArguments != null)
                        {
                            _voStructSize = attr.ConstructorArguments.First().DecodeValue<int>(SpecialType.System_Int32);
                            _voStructLargestElement = attr.ConstructorArguments.Last().DecodeValue<int>(SpecialType.System_Int32);
                        }
                        break;
                    }
                }
                _hasVoStructValues = true;
            }
        }


        internal bool IsVoStruct
        {
            get
            {
                ReadVoStructValues();
                return _isVoStruct;
            }
        }

        internal int VoStructSizeInBytes
        {
            get
            {
                ReadVoStructValues();
                return _voStructSize;
            }
        }
        internal int VoStructLargestElementSize
        {
            get
            {
                ReadVoStructValues();
                return _voStructLargestElement;
            }
        }
    }

}
