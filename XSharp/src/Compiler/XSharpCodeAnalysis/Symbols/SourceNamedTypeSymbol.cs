/*
   Copyright 2016-2017 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
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

namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    // This is a type symbol associated with a type definition in source code.
    // That is, for a generic type C<T> this is the instance type C<T>.  
    internal sealed partial class SourceNamedTypeSymbol : SourceMemberContainerTypeSymbol, IAttributeTargetSymbol
    {

        private bool _isVoStructOrUnion = false;

        internal bool IsSourceVoStructOrUnion { get { return _isVoStructOrUnion; } }
        private int _voStructSize = -1;
        private int _voStructElementSize = -1;

        internal int VoStructSize { get { if (_voStructSize == -1) EvalVoStructMemberSizes(); return _voStructSize; } }
        internal int VoStructElementSize { get { if (_voStructElementSize == -1) EvalVoStructMemberSizes(); return _voStructElementSize; } }

        private void EvalVoStructMemberSizes()
        {
            if (_isVoStructOrUnion && DeclaringCompilation.Options.IsDialectVO)
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
                        int sz, elsz;
                        if (f.IsFixed == true)
                        {
                            elsz = (f.Type as PointerTypeSymbol).PointedAtType.VoFixedBufferElementSizeInBytes();
                            sz = f.FixedSize * elsz;
                        }
                        else
                        {
                            sz = f.Type.VoFixedBufferElementSizeInBytes();
                            if ((f.Type as SourceNamedTypeSymbol)?.IsSourceVoStructOrUnion == true)
                            {
                                elsz = (f.Type as SourceNamedTypeSymbol).VoStructElementSize;
                            }
                            else if (f.Type.IsVoStructOrUnion())
                            {
                                elsz = f.Type.VoStructOrUnionLargestElementSizeInBytes();
                            }
                            else if (f.Type == DeclaringCompilation.WinBoolType())
                            {
                                elsz = sz = 4;
                            }
                            else
                            {
                                elsz = sz;
                            }
                        }
                        if (sz != 0)
                        {
                            int al = align;
                            if (elsz < al)
                                al = elsz;
                            if (voStructSize % al != 0)
                            {
                                voStructSize += al - (voStructSize % al);
                            }
                            if (!f.TypeLayoutOffset.HasValue)
                            {
                                // no explicit layout
                                voStructSize += sz;
                            }
                            else
                            {
                                // field offset is set: this is a union
                                int fieldLen = sz + f.TypeLayoutOffset.Value;
                                if (fieldLen > voStructSize)
                                {
                                    voStructSize = fieldLen;
                                }
                            }

                            if (voStructElementSize < elsz)
                                voStructElementSize = elsz;
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