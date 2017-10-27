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

namespace XSharpDebugger
{
    /// <summary>
    /// Represents a method on a type that has been imported into the compiler
    /// </summary>
    public class ImportedMethod : ImportedMember
    {
        private MethodDefinition _methodDef;
        private MethodSignature<XSharpType> _signature;

        private Variable[] _cachedParameters;

        internal ImportedMethod(ImportedModule module, MethodDefinition methodDef, ImportedType declaringType)
            : base(module, methodDef.Name, declaringType)
        {
            _methodDef = methodDef;
            _signature = methodDef.DecodeSignature(Module.XSharpTypeProvider, genericContext: null);
        }

        internal MethodSignature<XSharpType> Signature => _signature;

        //string _clipperArgs = null;
        //public string ClipperArgs
        //{
        //    get
        //    {
        //        if (_clipperArgs == null)
        //        {
        //            _clipperArgs = "";
        //            var custatts = _methodDef.GetCustomAttributes();
        //            foreach (var handle in custatts)
        //            { 
        //                EntityHandle ctorType = default(EntityHandle);
        //                string TypeName = null;
        //                var custatt = Module.Reader.GetCustomAttribute(handle);
        //                BlobHandle val = custatt.Value;
        //                if (custatt.Constructor.Kind == HandleKind.MemberReference)
        //                {
        //                    var memberref = Module.Reader.GetMemberReference((MemberReferenceHandle)custatt.Constructor);
        //                    ctorType = memberref.Parent;
                            

        //                }
        //                else if (custatt.Constructor.Kind == HandleKind.MethodDefinition)
        //                {
        //                    var methoddef = Module.Reader.GetMethodDefinition((MethodDefinitionHandle)custatt.Constructor);
        //                    ctorType = methoddef.GetDeclaringType();
        //                }
        //                if (!val.IsNil)
        //                {
        //                    BlobReader sig = Module.Reader.GetBlobReader(val);
        //                    byte[] data = sig.ReadBytes(sig.Length);

        //                }

        //                if (! ctorType.IsNil)
        //                {
        //                    if (ctorType.Kind == HandleKind.TypeDefinition)
        //                    {
        //                        var td = Module.Reader.GetTypeDefinition((TypeDefinitionHandle)ctorType);
        //                        TypeName = Module.Reader.GetString(td.Name);
        //                        _clipperArgs += TypeName;
        //                    }
        //                    else if (ctorType.Kind == HandleKind.TypeReference)
        //                    {
        //                        var tr = Module.Reader.GetTypeReference((TypeReferenceHandle)ctorType);
        //                        TypeName = Module.Reader.GetString(tr.Name);
        //                        _clipperArgs += TypeName;
        //                    }
        //                }
        //                var blobValue = Module.Reader.GetBlobContent(custatt.Value);


        //            }
        //        }
        //        return _clipperArgs;
        //    }
        //}
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

        public XSharpType ReturnType
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
            ImmutableArray<XSharpType> paramTypes = _signature.ParameterTypes;
            MetadataReader mdReader = Module.Reader;
            foreach (ParameterHandle handle in _methodDef.GetParameters())
            {
                Parameter param = mdReader.GetParameter(handle);
                string name = mdReader.GetString(param.Name);
                var variable = new Variable(paramTypes[param.SequenceNumber - 1], name);
                variable.In = param.Attributes.HasFlag(ParameterAttributes.In);
                variable.Out = param.Attributes.HasFlag(ParameterAttributes.Out);
                variables.Add(variable);
            }

            _cachedParameters = variables.ToArray();
            return _cachedParameters;
        }

    }
}
