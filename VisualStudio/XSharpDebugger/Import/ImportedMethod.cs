// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Reflection;
using System.Reflection.Metadata;

namespace XSharpDebugger
{
    /// <summary>
    /// Represents a method on a type that has been imported into the compiler
    /// </summary>
    public class ImportedMethod : ImportedMember
    {
        private MethodDefinition _methodDef;
        private MethodSignature<IrisType> _signature;

        private Variable[] _cachedParameters;

        internal ImportedMethod(ImportedModule module, MethodDefinition methodDef, ImportedType declaringType)
            : base(module, methodDef.Name, declaringType)
        {
            _methodDef = methodDef;
            _signature = methodDef.DecodeSignature(Module.IrisTypeProvider, genericContext: null);
        }

        public override bool IsPublic
        {
            get
            {
                return _methodDef.Attributes.HasFlag(MethodAttributes.Public);
            }
        }

        public override bool IsStatic
        {
            get
            {
                return _methodDef.Attributes.HasFlag(MethodAttributes.Static);
            }
        }

        public IrisType ReturnType
        {
            get
            {
                return _signature.ReturnType;
            }
        }


        public Variable[] GetParameters()
        {
            if (_cachedParameters != null)
                return _cachedParameters;

            List<Variable> variables = new List<Variable>();
            ImmutableArray<IrisType> paramTypes = _signature.ParameterTypes;
            MetadataReader mdReader = Module.Reader;
            foreach (ParameterHandle handle in _methodDef.GetParameters())
            {
                Parameter param = mdReader.GetParameter(handle);
                string name = mdReader.GetString(param.Name);
                variables.Add(new Variable(paramTypes[param.SequenceNumber - 1], name));
            }

            _cachedParameters = variables.ToArray();
            return _cachedParameters;
        }

    }
}
