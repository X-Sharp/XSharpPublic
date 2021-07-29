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

        internal bool IsSourceVoStructOrUnion { get { return _isVoStructOrUnion; } }
        private int _voStructSize = -1;
        private int _voStructElementSize = -1;

        internal int VoStructSize { get { if (_voStructSize == -1) EvalVoStructMemberSizes(); return _voStructSize; } }
        internal int VoStructElementSize { get { if (_voStructElementSize == -1) EvalVoStructMemberSizes(); return _voStructElementSize; } }

        private void EvalVoStructMemberSizes()
        {
            if (_isVoStructOrUnion && DeclaringCompilation.Options.HasRuntime)
            {
                int voStructSize = 0;
                int voStructElementSize = 0;
                int align = this.Layout.Alignment;
                if (align == 0)
                    align = 4;
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
                                elementSize = (f.Type as SourceNamedTypeSymbol).VoStructElementSize;
                            }
                            else if (f.Type.IsVoStructOrUnion())
                            {
                                elementSize = f.Type.VoStructOrUnionLargestElementSizeInBytes();
                            }
                            else if (f.Type.IsWinBoolType()
                                || f.Type.IsSymbolType()
                                || f.Type.IsDateType())
                            {
                                elementSize = fieldSize = 4;
                            }
                            if ( f.Type.IsPszType())
                            {
                                if (DeclaringCompilation?.Options.Platform == Platform.X86)
                                    elementSize = fieldSize = 4;
                                else
                                    elementSize = fieldSize = 8;
                            }
                            else
                            {
                                elementSize = fieldSize;
                            }
                        }
                        if (fieldSize != 0)
                        {
                            if (fieldSize < align)
                                fieldSize = align;
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

                            if (voStructElementSize < elementSize)
                                voStructElementSize = elementSize;
                            if (voStructSize % align != 0)
                            {
                                voStructSize += align - (voStructSize % align);
                            }
                        }
                    }
                }
                _voStructSize = voStructSize;
                _voStructElementSize = voStructElementSize;
            }
        }
        internal SynthesizedAttributeData GetVoStructAttributeData()
        {
            var syntax = ((CSharpSyntaxNode)declaration.SyntaxReferences.FirstOrDefault()?.GetSyntax());
            var attributeType = DeclaringCompilation.VOStructAttributeType();
            var int32type = DeclaringCompilation.GetSpecialType(SpecialType.System_Int32);
            var attributeConstructor = attributeType.GetMembers(".ctor").FirstOrDefault() as MethodSymbol;
            var constructorArguments = ArrayBuilder<TypedConstant>.GetInstance();
            constructorArguments.Add(new TypedConstant(int32type, TypedConstantKind.Primitive, VoStructSize));
            constructorArguments.Add(new TypedConstant(int32type, TypedConstantKind.Primitive, VoStructElementSize));
            return new SynthesizedAttributeData(attributeConstructor, constructorArguments.ToImmutableAndFree(), ImmutableArray<KeyValuePair<string, TypedConstant>>.Empty);
        }

    }
}
