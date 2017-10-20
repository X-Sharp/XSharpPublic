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

        public Method ConvertToIrisMethod()
        {
            IrisType returnType = _signature.ReturnType;
            Variable[] parameters = GetParameters();

            if (IsStatic)
            {
                return CreateMethodHelper(returnType, parameters);
            }
            else
            {
                // Iris can call instance methods by passing "this" as parameter 0
                Variable[] staticParams = new Variable[parameters.Length + 1];
                IrisType instanceType = DeclaringType.ConvertToIrisType();

                if (instanceType == IrisType.Integer || instanceType == IrisType.Boolean)
                    instanceType = instanceType.MakeByRefType();

                staticParams[0] = new Variable(instanceType, "this");
                Array.Copy(parameters, 0, staticParams, 1, parameters.Length);
                return CreateMethodHelper(returnType, staticParams);
            }
        }

        private Method CreateMethodHelper(IrisType returnType, Variable[] parameters)
        {
            if (returnType == IrisType.Void)
                return Procedure.Create(parameters);
            else
                return Function.Create(returnType, parameters);
        }
    }
}
