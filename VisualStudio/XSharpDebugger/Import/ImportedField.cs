//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Reflection;
using System.Reflection.Metadata;

namespace XSharpDebugger
{
    /// <summary>
    /// Represents the field of a type that has been imported into the compiler.
    /// </summary>
    public class ImportedField : ImportedMember
    {
        private FieldDefinition _fieldDef;
        private XSharpType _cachedType;

        internal ImportedField(ImportedModule module, FieldDefinition fieldDef, ImportedType declaringType)
            : base(module, fieldDef.Name, declaringType)
        {
            _fieldDef = fieldDef;
        }

        public override bool IsPublic
        {
            get
            {
                return _fieldDef.Attributes.HasFlag(FieldAttributes.Public);
            }
        }

        public override bool IsStatic
        {
            get
            {
                return _fieldDef.Attributes.HasFlag(FieldAttributes.Static);
            }
        }

        public XSharpType FieldType
        {
            get
            {
                if (_cachedType == null)
                    _cachedType = _fieldDef.DecodeSignature(Module.XSharpTypeProvider, genericContext: null);

                return _cachedType;
            }
        }
    }
}
