//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Collections.Immutable;
using System.Reflection.Metadata;

namespace XSharpDebugger
{
    internal class XSharpTypeProvider : ISignatureTypeProvider<XSharpType, object>
    {
        private MetadataReader _reader;

        
        public XSharpTypeProvider(MetadataReader reader)
        {
            _reader = reader;
        }

        #region ISimpleTypeProvider<XSharpType> implementation

        XSharpType ISimpleTypeProvider<XSharpType>.GetTypeFromDefinition(MetadataReader reader, TypeDefinitionHandle handle, byte rawTypeKind)
        {
            TypeDefinition typeDef = _reader.GetTypeDefinition(handle);
            return XSharpType.Create(_reader.GetString(typeDef.Namespace)+"."+_reader.GetString(typeDef.Name));
        }

        XSharpType ISimpleTypeProvider<XSharpType>.GetTypeFromReference(MetadataReader reader, TypeReferenceHandle handle, byte rawTypeKind)
        {
            TypeReference typeRef = _reader.GetTypeReference(handle);
            return XSharpType.Create(_reader.GetString(typeRef.Namespace)+"."+_reader.GetString(typeRef.Name));
        }

        XSharpType ISimpleTypeProvider<XSharpType>.GetPrimitiveType(PrimitiveTypeCode typeCode)
        {
            switch (typeCode)
            {
                case PrimitiveTypeCode.Boolean:
                    return XSharpType.Logic;
                case PrimitiveTypeCode.SByte:
                    return XSharpType.SByte;
                case PrimitiveTypeCode.Int16:
                    return XSharpType.Short;
                case PrimitiveTypeCode.Int32:
                    return XSharpType.Integer;
                case PrimitiveTypeCode.Int64:
                    return XSharpType.Int64;
                case PrimitiveTypeCode.Byte:
                    return XSharpType.Byte;
                case PrimitiveTypeCode.UInt16:
                    return XSharpType.Word;
                case PrimitiveTypeCode.UInt32:
                    return XSharpType.DWord;
                case PrimitiveTypeCode.UInt64:
                    return XSharpType.UInt64;
                case PrimitiveTypeCode.String:
                    return XSharpType.String;
                case PrimitiveTypeCode.Void:
                    return XSharpType.Void;
                case PrimitiveTypeCode.Char:
                    return XSharpType.Char;
                case PrimitiveTypeCode.Single:
                    return XSharpType.Real4;
                case PrimitiveTypeCode.Double:
                    return XSharpType.Real8;
                case PrimitiveTypeCode.IntPtr:
                    return XSharpType.Ptr;
                case PrimitiveTypeCode.UIntPtr:
                    return XSharpType.Create("UINTPTR");
                case PrimitiveTypeCode.Object:
                    return XSharpType.Object;
                case PrimitiveTypeCode.TypedReference:
                default:
                    break;
            }
            return XSharpType.Create("UNKNOWN");
        }

        #endregion

        #region ISignatureTypeProvider<XSharpType, object> implementation

        XSharpType ISignatureTypeProvider<XSharpType, object>.GetTypeFromSpecification(
            MetadataReader reader,
            object genericContext,
            TypeSpecificationHandle handle,
            byte rawTypeKind)
        {
            TypeSpecification typeSpec = _reader.GetTypeSpecification(handle);
            return typeSpec.DecodeSignature(this, genericContext);
        }
        XSharpType ISignatureTypeProvider<XSharpType, object>.GetFunctionPointerType(MethodSignature<XSharpType> signature)
        {
            return signature.ReturnType;
        }

        XSharpType ISignatureTypeProvider<XSharpType, object>.GetGenericMethodParameter(object genericContext, int index)
        {
            return XSharpType.Invalid;
        }

        XSharpType ISignatureTypeProvider<XSharpType, object>.GetGenericTypeParameter(object genericContext, int index)
        {
            return XSharpType.Invalid;
        }

        XSharpType ISignatureTypeProvider<XSharpType, object>.GetModifiedType(XSharpType modifier, XSharpType unmodifiedType, bool isRequired)
        {
            return unmodifiedType;
        }

        XSharpType ISignatureTypeProvider<XSharpType, object>.GetPinnedType(XSharpType elementType)
        {
            return elementType;
        }

        #endregion

        #region IConstructedTypeProvider<XSharpType> implementation

        XSharpType IConstructedTypeProvider<XSharpType>.GetPointerType(XSharpType elementType)
        {
            return XSharpType.Create(elementType.Name+" PTR");
        }

        XSharpType IConstructedTypeProvider<XSharpType>.GetGenericInstantiation(XSharpType genericType, ImmutableArray<XSharpType> typeArguments)
        {
            return XSharpType.Invalid;
        }

        XSharpType IConstructedTypeProvider<XSharpType>.GetArrayType(XSharpType elementType, ArrayShape shape)
        {
            // In the world of .NET metadata, we only support SZArray.
            return XSharpType.Create(elementType.Name + "[]");
        }

        XSharpType IConstructedTypeProvider<XSharpType>.GetByReferenceType(XSharpType elementType)
        {
            return elementType.MakeByRefType();
        }

        #endregion

        #region ISZArrayTypeProvider<XSharpType> implementation

        XSharpType ISZArrayTypeProvider<XSharpType>.GetSZArrayType(XSharpType elementType)
        {
            return elementType.MakeArrayType();
        }

        #endregion
    }
}
