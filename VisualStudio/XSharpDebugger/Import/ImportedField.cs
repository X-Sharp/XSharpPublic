// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

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
        private IrisType _cachedType;

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

        public IrisType FieldType
        {
            get
            {
                if (_cachedType == null)
                    _cachedType = _fieldDef.DecodeSignature(Module.IrisTypeProvider, genericContext: null);

                return _cachedType;
            }
        }
    }
}
