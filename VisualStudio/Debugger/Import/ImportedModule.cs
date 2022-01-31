//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;
using System.Text;

namespace XSharpDebugger
{
    /// <summary>
    /// Represents a .NET module that has been imported into the compiler.
    /// </summary>
    public class ImportedModule
    {
        private Dictionary<TypeDefinitionHandle, ImportedType> _resolvedTypes = new Dictionary<TypeDefinitionHandle, ImportedType>();
        private Dictionary<MethodDefinitionHandle, ImportedMethod> _resolvedMethods = new Dictionary<MethodDefinitionHandle, ImportedMethod>();
        private Dictionary<string, TypeDefinitionHandle> _typeNameMap;
        private MetadataReader _reader;

        private string _cachedAssemblyName;

        internal unsafe ImportedModule(PEMemoryBlock metadataBlock)
        {
            _reader = new MetadataReader(metadataBlock.Pointer, metadataBlock.Length);
        }

        internal unsafe ImportedModule(IntPtr metadataPtr, uint blockSize)
        {
            _reader = new MetadataReader((byte*)metadataPtr, (int)blockSize);
        }

        public MetadataReader Reader
        {
            get
            {
                return _reader;
            }
        }

        internal XSharpTypeProvider XSharpTypeProvider
        {
            get
            {
                return new XSharpTypeProvider(_reader);
            }
        }

        public string AssemblyName
        {
            get
            {
                if (_cachedAssemblyName == null)
                {
                    AssemblyDefinition assemblyDef = _reader.GetAssemblyDefinition();
                    _cachedAssemblyName = _reader.GetString(assemblyDef.Name);
                }

                return _cachedAssemblyName;
            }
        }

        public ImportedType TryGetTypeByName(string name)
        {
            if (_typeNameMap == null)
            {
                // First we need to scan the TypeDef table and get the names of all types in this
                // module.
                _typeNameMap = new Dictionary<string, TypeDefinitionHandle>();

                StringBuilder nameBuilder = new StringBuilder();
                foreach (TypeDefinitionHandle handle in _reader.TypeDefinitions)
                {
                    TypeDefinition typeDef = _reader.GetTypeDefinition(handle);
                    if (!typeDef.Attributes.HasFlag(TypeAttributes.Public))
                    {
                        // Skip non-public types
                        continue;
                    }

                    if (!typeDef.GetDeclaringType().IsNil)
                    {
                        // Skip nested types
                        continue;
                    }

                    nameBuilder.Length = 0;
                    StringHandle nameHandle = typeDef.Name;
                    StringHandle namespaceHandle = typeDef.Namespace;
                    if (!namespaceHandle.IsNil)
                    {
                        nameBuilder.Append(_reader.GetString(namespaceHandle));
                        nameBuilder.Append(".");
                    }

                    nameBuilder.Append(_reader.GetString(nameHandle));
                    _typeNameMap.Add(nameBuilder.ToString(), handle);
                }
            }

            // Look up the name in the map
            TypeDefinitionHandle typeDefHandle;
            if (_typeNameMap.TryGetValue(name, out typeDefHandle))
                return ResolveType(typeDefHandle);

            return null;
        }

        /// <summary>
        /// Find a method given its metadata token.  This method is used for debugging support.
        /// </summary>
        /// <param name="mdMethodToken">Method metadata token.</param>
        /// <returns></returns>
        public ImportedMethod GetMethod(int mdToken)
        {
            MethodDefinitionHandle methodHandle = (MethodDefinitionHandle)MetadataTokens.EntityHandle(mdToken);
            return ResolveMethod(methodHandle);
        }

        /// <summary>
        /// Get the types of the local variables given the metadata token for the local variable
        /// signature.  This method is used for debugging support.
        /// </summary>
        /// <param name="mdToken">Local variable signature metadata token</param>
        /// <returns>Immutable array of local variable types.  The types are ordered by the local slot number</returns>
        public ImmutableArray<XSharpType> DecodeLocalVariableTypes(int mdToken)
        {
            StandaloneSignatureHandle localVarSigHandle = (StandaloneSignatureHandle)MetadataTokens.EntityHandle(mdToken);
            StandaloneSignature sig = _reader.GetStandaloneSignature(localVarSigHandle);
            return sig.DecodeLocalSignature(XSharpTypeProvider, genericContext: null);
        }

        internal ImportedType ResolveType(TypeDefinitionHandle handle)
        {
            ImportedType type;
            if (!_resolvedTypes.TryGetValue(handle, out type))
            {
                TypeDefinition typeDef = _reader.GetTypeDefinition(handle);
                type = new ImportedType(this, typeDef);
                _resolvedTypes.Add(handle, type);
            }

            return type;
        }

        internal ImportedMethod ResolveMethod(MethodDefinitionHandle handle, ImportedType declaringType = null)
        {
            ImportedMethod method;
            if (!_resolvedMethods.TryGetValue(handle, out method))
            {
                MethodDefinition methodDef = _reader.GetMethodDefinition(handle);
                if (declaringType == null)
                    declaringType = ResolveType(methodDef.GetDeclaringType());
                var name = methodDef.Name;
                var strName = this.Reader.GetString(name);
                method = new ImportedMethod(this, methodDef, declaringType );
                _resolvedMethods.Add(handle, method);
            }

            return method;
        }
    }
}
