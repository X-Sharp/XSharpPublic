// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

using System.Collections.Immutable;
using System.Reflection.Metadata;

namespace XSharpDebugger
{
    internal class IrisTypeProvider : ISignatureTypeProvider<IrisType, object>
    {
        private MetadataReader _reader;

        public IrisTypeProvider(MetadataReader reader)
        {
            _reader = reader;
        }

        #region ISimpleTypeProvider<IrisType> implementation

        IrisType ISimpleTypeProvider<IrisType>.GetTypeFromDefinition(MetadataReader reader, TypeDefinitionHandle handle, byte rawTypeKind)
        {
            // Iris doesn't define any types that can be referenced.
            return IrisType.Invalid;
        }

        IrisType ISimpleTypeProvider<IrisType>.GetTypeFromReference(MetadataReader reader, TypeReferenceHandle handle, byte rawTypeKind)
        {
            // We shouldn't be referencing any non-primitive types.
            return IrisType.Invalid;
        }

        IrisType ISimpleTypeProvider<IrisType>.GetPrimitiveType(PrimitiveTypeCode typeCode)
        {
            switch (typeCode)
            {
                case PrimitiveTypeCode.Boolean:
                    return IrisType.Boolean;
                case PrimitiveTypeCode.Int32:
                    return IrisType.Integer;
                case PrimitiveTypeCode.String:
                    return IrisType.String;
                case PrimitiveTypeCode.Void:
                    return IrisType.Void;
                default:
                    return IrisType.Invalid;
            }
        }

        #endregion

        #region ISignatureTypeProvider<IrisType, object> implementation

        IrisType ISignatureTypeProvider<IrisType, object>.GetTypeFromSpecification(
            MetadataReader reader,
            object genericContext,
            TypeSpecificationHandle handle,
            byte rawTypeKind)
        {
            TypeSpecification typeSpec = _reader.GetTypeSpecification(handle);
            return typeSpec.DecodeSignature(this, genericContext);
        }

        IrisType ISignatureTypeProvider<IrisType, object>.GetFunctionPointerType(MethodSignature<IrisType> signature)
        {
            return IrisType.Invalid;
        }

        IrisType ISignatureTypeProvider<IrisType, object>.GetGenericMethodParameter(object genericContext, int index)
        {
            return IrisType.Invalid;
        }

        IrisType ISignatureTypeProvider<IrisType, object>.GetGenericTypeParameter(object genericContext, int index)
        {
            return IrisType.Invalid;
        }

        IrisType ISignatureTypeProvider<IrisType, object>.GetModifiedType(IrisType modifier, IrisType unmodifiedType, bool isRequired)
        {
            return unmodifiedType;
        }

        IrisType ISignatureTypeProvider<IrisType, object>.GetPinnedType(IrisType elementType)
        {
            return IrisType.Invalid;
        }

        #endregion

        #region IConstructedTypeProvider<IrisType> implementation

        IrisType IConstructedTypeProvider<IrisType>.GetPointerType(IrisType elementType)
        {
            return IrisType.Invalid;
        }

        IrisType IConstructedTypeProvider<IrisType>.GetGenericInstantiation(IrisType genericType, ImmutableArray<IrisType> typeArguments)
        {
            return IrisType.Invalid;
        }

        IrisType IConstructedTypeProvider<IrisType>.GetArrayType(IrisType elementType, ArrayShape shape)
        {
            // In the world of .NET metadata, we only support SZArray.
            return IrisType.Invalid;
        }

        IrisType IConstructedTypeProvider<IrisType>.GetByReferenceType(IrisType elementType)
        {
            return elementType.MakeByRefType();
        }

        #endregion

        #region ISZArrayTypeProvider<IrisType> implementation

        IrisType ISZArrayTypeProvider<IrisType>.GetSZArrayType(IrisType elementType)
        {
            return elementType.MakeArrayType();
        }

        #endregion
    }
}
