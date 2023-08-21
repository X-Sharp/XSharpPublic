//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using System.Reflection.Metadata;
using System.Text;
using XSharp.Settings;
namespace XSharpDebugger
{
    /// <summary>
    /// Represents a type that can potentially be imported into the X# compiler.
    /// ImportedType is an intermediary between .NET's complex type system and the X# type system
    /// </summary>
    [DebuggerDisplay("ImportedType: {DebugDisplay}")]
    public class ImportedType : ImportedMember
    {
        private readonly TypeDefinition _typeDef;

        private List<ImportedField> _fields;
        private List<ImportedMethod> _methods;
        private string _cachedNamespace;
        private string _cachedFullName;

        internal ImportedType(ImportedModule module, TypeDefinition typeDef)
            : base(module, typeDef.Name, null)
        {
            _typeDef = typeDef;
        }

        public override bool IsPublic
        {
            get
            {
                return _typeDef.Attributes.HasFlag(TypeAttributes.Public);
            }
        }

        public override bool IsStatic
        {
            get
            {
                return _typeDef.Attributes.HasFlag(TypeAttributes.Sealed) && _typeDef.Attributes.HasFlag(TypeAttributes.Abstract);
            }
        }

        public string Namespace
        {
            get
            {
                if (_cachedNamespace == null)
                {
                    StringHandle nsHandle = _typeDef.Namespace;
                    if (nsHandle.IsNil)
                        _cachedNamespace = string.Empty;
                    else
                        _cachedNamespace = Module.Reader.GetString(nsHandle);
                }

                return _cachedNamespace;
            }
        }

        public string FullName
        {
            get
            {
                if (_cachedFullName == null)
                {
                    StringBuilder nameBuilder = new StringBuilder();
                    AppendFullName(nameBuilder);
                    _cachedFullName = nameBuilder.ToString();
                }

                return _cachedFullName;
            }
        }

        public string DebugDisplay
        {
            get
            {
                return _cachedFullName ?? "<Unresolved name.  Expand to see full name>";
            }
        }

        public ImportedField[] GetFields()
        {
            EnsureFields();
            return _fields.ToArray();
        }

        public ImportedMethod[] GetMethods()
        {
            EnsureMethods();
            return _methods.ToArray();
        }

        public ImportedMethod TryFindMethod(string name, bool instance, XSharpType returnType, XSharpType[] paramTypes)
        {
            EnsureMethods();

            foreach (ImportedMethod method in _methods)
            {
                if (method.IsStatic == instance)
                    continue;

                if (method.ReturnType != returnType)
                    continue;

                if (!string.Equals(method.Name, name))
                    continue;

                Variable[] methodParams = method.GetParameters();
                if (methodParams.Length != paramTypes.Length)
                    continue;

                bool match = true;
                for (int i = 0; i < paramTypes.Length; i++)
                {
                    if (paramTypes[i] != methodParams[i].Type)
                    {
                        match = false;
                        break;
                    }
                }

                if (match)
                    return method; // Found
            }

            return null; // Not found
        }

        public XSharpType ConvertToXSharpType()
        {
            switch (FullName)
            {
                case KnownTypes.SystemBoolean:
                    return XSharpType.Logic;
                case KnownTypes.SystemInt32:
                    return XSharpType.Integer;
                case KnownTypes.SystemString:
                    return XSharpType.String;
                case KnownTypes.SystemVoid:
                    return XSharpType.Void;
                default:
                    return XSharpType.Create(FullName);
            }
        }

        public ImportedField TryGetPublicStaticField(string name)
        {
            ImportedField result = null;

            EnsureFields();
            foreach (ImportedField field in _fields)
            {
                if (!field.IsPublic || !field.IsStatic)
                    continue; // Is not public static

                if (!string.Equals(name, field.Name))
                    continue; // Name doesn't match

                if (result != null)
                    return null; // Ambiguous match

                result = field;
            }

            return result;
        }

        private void AppendFullName(StringBuilder nameBuilder)
        {
            if (DeclaringType != null)
            {
                DeclaringType.AppendFullName(nameBuilder);
                nameBuilder.Append("/");
            }
            else if (!string.IsNullOrEmpty(Namespace))
            {
                nameBuilder.Append(Namespace);
                nameBuilder.Append(".");
            }

            nameBuilder.Append(Name);
        }

        private void EnsureFields()
        {
            if (_fields == null)
            {
                _fields = new List<ImportedField>();
                foreach (FieldDefinitionHandle fieldHandle in _typeDef.GetFields())
                {
                    FieldDefinition fieldDef = Module.Reader.GetFieldDefinition(fieldHandle);
                    ImportedField field = new ImportedField(Module, fieldDef, this);
                    _fields.Add(field);
                }
            }
        }

        private void EnsureMethods()
        {
            if (_methods == null)
            {
                _methods = new List<ImportedMethod>();
                foreach (MethodDefinitionHandle methodHandle in _typeDef.GetMethods())
                {
                    ImportedMethod method = Module.ResolveMethod(methodHandle, this);
                    _methods.Add(method);
                }
            }
        }
    }
}
