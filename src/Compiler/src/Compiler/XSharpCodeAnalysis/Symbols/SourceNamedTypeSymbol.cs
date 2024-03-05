//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Runtime.InteropServices;
using System.Threading;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslyn.Utilities;
using Microsoft.CodeAnalysis.PooledObjects;
namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    // This is a type symbol associated with a type definition in source code.
    // That is, for a generic type C<T> this is the instance type C<T>.  
    internal sealed partial class SourceNamedTypeSymbol : SourceMemberContainerTypeSymbol, IAttributeTargetSymbol
    {

        private readonly bool _isVoStructOrUnion = false;

        internal bool IsSourceVoStructOrUnion => _isVoStructOrUnion;
        private int _voStructSize = -1;
        private int _voStructElementSize = -1;

        internal int VoStructSize
        {
            get
            {
                if (_voStructSize == -1)
                    EvalVoStructMemberSizes();
                return _voStructSize;
            }
        }
        internal int VoStructElementSize
        {
            get
            {
                if (_voStructElementSize == -1)
                    EvalVoStructMemberSizes();
                return _voStructElementSize;
            }
        }

        private void EvalVoStructMemberSizes()
        {
            /*
             * When no alignment is specified then each element must be aligned on
             * a memory boundary that is a multiple of its own size
             * and the total size of the structure is a multiple of the size of 
             * the largest element. If an element in a structure is a structure
             * itself then the "Largest" element is determined by the size of the
             * largest element of the sub structure
             * When an alignment is specified then each element must be aligned on 
             * a memory boundary that is a multiple of the specified boundary
             * and the total size of the structure is a multiple of the size specified.
             */
            if (_isVoStructOrUnion && DeclaringCompilation.Options.HasRuntime)
            {
                int voStructSize = 0;
                int largestElementSize = 0;
                int align = this.Layout.Alignment;
                foreach (var m in GetMembers())
                {
                    if (m.Kind == SymbolKind.Field)
                    {
                        var f = (FieldSymbol)m;
                        int fieldSize, elementSize;
                        if (f.IsFixedSizeBuffer == true)
                        {
                            elementSize = (f.Type as PointerTypeSymbol).PointedAtType.VoFixedBufferElementSizeInBytes(DeclaringCompilation);
                            fieldSize = f.FixedSize * elementSize;
                        }
                        else
                        {
                            fieldSize = f.Type.VoFixedBufferElementSizeInBytes(DeclaringCompilation);
                            if ((f.Type as SourceNamedTypeSymbol)?.IsSourceVoStructOrUnion == true)
                            {
                                // get the size of the largest element in the structure in source
                                elementSize = (f.Type as SourceNamedTypeSymbol).VoStructElementSize;
                            }
                            else if (f.Type.IsVoStructOrUnion())
                            {
                                // get the size of the largest element in the structure in a PE
                                elementSize = f.Type.VoStructOrUnionLargestElementSizeInBytes();
                            }
                            else if (f.Type.IsWinBoolType() || f.Type.IsSymbolType() || f.Type.IsDateType() || f.Type.IsWinDateType())
                            {
                                // these are all structures but their size is set to fixed 4 bytes.
                                elementSize = fieldSize = 4;
                            }
                            else if (f.Type.IsPszType())
                            {
                                if (DeclaringCompilation?.Options.Platform == Platform.X86)
                                {
                                    elementSize = fieldSize = 4;
                                }
                                else
                                {
                                    elementSize = fieldSize = 8;
                                }
                            }
                            else
                            {
                                elementSize = fieldSize;
                            }
                        }
                        if (fieldSize != 0)
                        {
                            var elementAlignment = align;
                            if (elementAlignment == 0)
                                elementAlignment = elementSize;
                            if (voStructSize % elementAlignment != 0)
                            {
                                voStructSize += elementAlignment - (voStructSize % elementAlignment);
                            }

                            if (!f.TypeLayoutOffset.HasValue)
                            {
                                // no explicit layout
                                voStructSize += fieldSize;
                            }
                            else
                            {
                                // field offset is set: this is a union
                                int fieldLen = fieldSize + f.TypeLayoutOffset.Value;
                                if (fieldLen > voStructSize)
                                {
                                    voStructSize = fieldLen;
                                }
                            }

                            if (largestElementSize < elementSize)
                                largestElementSize = elementSize;

                        }
                    }
                }
                if (align == 0)
                {
                    if (voStructSize % largestElementSize != 0)
                    {
                        voStructSize += largestElementSize - (voStructSize % largestElementSize);
                    }
                }
                else if (align != 1)
                {
                    if (voStructSize % align != 0)
                    {
                        voStructSize += align - (voStructSize % align);
                    }

                }
                _voStructSize = voStructSize;
                _voStructElementSize = largestElementSize;

            }
        }

        internal SynthesizedAttributeData GetVoStructAttributeData()
        {
            var attributeType = DeclaringCompilation.VOStructAttributeType();
            var int32type = DeclaringCompilation.GetSpecialType(SpecialType.System_Int32);
            var attributeConstructor = attributeType.GetMembers(WellKnownMemberNames.InstanceConstructorName).FirstOrDefault() as MethodSymbol;
            var constructorArguments = ArrayBuilder<TypedConstant>.GetInstance();
            constructorArguments.Add(new TypedConstant(int32type, TypedConstantKind.Primitive, VoStructSize));
            constructorArguments.Add(new TypedConstant(int32type, TypedConstantKind.Primitive, VoStructElementSize));
            return new SynthesizedAttributeData(attributeConstructor, constructorArguments.ToImmutableAndFree(), ImmutableArray<KeyValuePair<string, TypedConstant>>.Empty);
        }

        private CSharpAttributeData XsDecodeWellKnownAttribute(ref EarlyDecodeWellKnownAttributeArguments<EarlyWellKnownAttributeBinder, NamedTypeSymbol, AttributeSyntax, AttributeLocation> arguments)
        {
            bool hasAnyDiagnostics;
            CSharpAttributeData boundAttribute;
            if (CSharpAttributeData.IsTargetEarlyAttribute(arguments.AttributeType, arguments.AttributeSyntax, AttributeDescription.CompilerGeneratedAttribute))
            {
                boundAttribute = arguments.Binder.GetAttribute(arguments.AttributeSyntax, arguments.AttributeType, out hasAnyDiagnostics);
                if (!boundAttribute.HasErrors)
                {
                    arguments.GetOrCreateData<CommonTypeEarlyWellKnownAttributeData>().HasCompilerGeneratedAttribute = true;
                    if (!hasAnyDiagnostics)
                    {
                        return boundAttribute;
                    }
                }
                return null;
            }
            if (CSharpAttributeData.IsTargetEarlyAttribute(arguments.AttributeType, arguments.AttributeSyntax, AttributeDescription.CompilerGlobalScopeAttribute))
            {
                boundAttribute = arguments.Binder.GetAttribute(arguments.AttributeSyntax, arguments.AttributeType, out hasAnyDiagnostics);
                if (!boundAttribute.HasErrors)
                {
                    arguments.GetOrCreateData<CommonTypeEarlyWellKnownAttributeData>().HasCompilerGlobalScopeAttribute = true;
                    if (!hasAnyDiagnostics)
                    {
                        return boundAttribute;
                    }
                }
                return null;
            }
            return null;
        }

    }
}
