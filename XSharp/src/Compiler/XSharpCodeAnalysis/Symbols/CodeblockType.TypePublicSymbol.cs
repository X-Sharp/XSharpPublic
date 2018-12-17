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
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using Microsoft.CodeAnalysis.PooledObjects;
namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    internal sealed partial class AnonymousTypeManager
    {
        /// <summary>
        /// Represents a codeblock type 'public' symbol which is used in binding and lowering.
        /// In emit phase it is being substituted with implementation symbol.
        /// </summary>
        private sealed class CodeblockTypePublicSymbol : AnonymousTypePublicSymbol
        {
            internal CodeblockTypePublicSymbol(AnonymousTypeManager manager, TypeSymbol[] codeblockParams, Location location)
                : base(manager, codeblockParams, location)
            {
            }

            public override Symbol ContainingSymbol
            {
                get { return this.Manager.Compilation.SourceModule.GlobalNamespace; }
            }

            internal override NamedTypeSymbol BaseTypeNoUseSiteDiagnostics
            {
                get { return this.DeclaringCompilation.CodeBlockType(); }
            }

            internal override NamedTypeSymbol GetDeclaredBaseType(ConsList<Symbol> basesBeingResolved)
            {
                return this.DeclaringCompilation.CodeBlockType();
            }

            internal override bool Equals(TypeSymbol t2, TypeCompareKind comparison)
            {
                if (ReferenceEquals(this, t2))
                {
                    return true;
                }

                var other = t2 as CodeblockTypePublicSymbol;
                return (object)other != null && this.TypeDescriptor.Equals(other.TypeDescriptor, comparison);
            }
        }

        private sealed partial class CodeblockEvalMethod : SynthesizedMethodBase
        {
            private readonly ImmutableArray<ParameterSymbol> _parameters;

            internal CodeblockEvalMethod(NamedTypeSymbol container)
                : base(container, "Eval")
            {
                if (this.Manager.Compilation.Options.XSharpRuntime)
                {
                    _parameters = ImmutableArray.Create<ParameterSymbol>(
                            new SynthesizedParamsParameterSymbol(this, this.Manager.Compilation.CreateArrayTypeSymbol(this.Manager.UsualType), 0, RefKind.None, "args")
                    );

                }
                else
                {
                    _parameters = ImmutableArray.Create<ParameterSymbol>(
                            new SynthesizedParamsParameterSymbol(this, this.Manager.Compilation.CreateArrayTypeSymbol(this.Manager.UsualType), 0, RefKind.None, "args",
                                customModifiers: new[] { CSharpCustomModifier.CreateOptional(this.Manager.Compilation.GetWellKnownType(WellKnownType.System_Runtime_CompilerServices_IsConst)) }.ToImmutableArray())
                        );
                }
            }

            public override MethodKind MethodKind
            {
                get { return MethodKind.Ordinary; }
            }

            public override bool ReturnsVoid
            {
                get { return false; }
            }

            public  override RefKind RefKind
            {
                get { return RefKind.None; }
            }

            public override TypeSymbol ReturnType
            {
                get { return this.Manager.UsualType; }
            }

            public override ImmutableArray<ParameterSymbol> Parameters
            {
                get { return _parameters; }
            }

            public override bool IsOverride
            {
                get { return true; }
            }

            internal sealed override bool IsMetadataVirtual(bool ignoreInterfaceImplementationChanges = false)
            {
                return true;
            }

            internal override bool IsMetadataFinal
            {
                get
                {
                    return false;
                }
            }

            public override ImmutableArray<Location> Locations
            {
                get
                {
                    // The accessor for an anonymous type constructor has the same location as the type.
                    return this.ContainingSymbol.Locations;
                }
            }

            internal override void GenerateMethodBody(TypeCompilationState compilationState, DiagnosticBag diagnostics)
            {
                AnonymousTypeManager manager = ((AnonymousTypeTemplateSymbol)this.ContainingType).Manager;
                SyntheticBoundNodeFactory F = this.CreateBoundNodeFactory(compilationState, diagnostics);

                //  Method body:
                //
                //  {
                //      return Cb$Eval$(
                //             _BlockArg(args, 0),
                //             ...
                //             _BlockArg(args, N),
                //          );
                //  }

                AnonymousTypeTemplateSymbol anonymousType = (AnonymousTypeTemplateSymbol)this.ContainingType;
                var cbDel = (anonymousType.GetMembers()[0] as AnonymousTypePropertySymbol).BackingField;
                MethodSymbol invokeMethod = cbDel.Type.GetDelegateType().DelegateInvokeMethod();
                MethodSymbol argMethod = DeclaringCompilation.CodeBlockType().GetMembers("_BlockArg").First() as MethodSymbol;

                BoundExpression paramArr = F.Parameter(_parameters[0]);

                var args = new BoundExpression[invokeMethod.ParameterCount];

                for (int index = 0; index < invokeMethod.ParameterCount; index++)
                {
                    args[index] = F.StaticCall(manager.CodeblockType, argMethod, paramArr, F.Literal(index));
                }

                BoundExpression retExpression = F.Call(F.Field(F.This(), cbDel), invokeMethod, args);

                // Final return statement
                BoundStatement retStatement = F.Return(retExpression);

                // Create a bound block
                F.CloseMethod(F.Block(retStatement));
            }

            internal override bool HasSpecialName
            {
                get { return false; }
            }
        }

        internal sealed class SynthesizedParamsParameterSymbol: SynthesizedParameterSymbolBase
        {
            ImmutableArray<CustomModifier> _customModifiers;

            public SynthesizedParamsParameterSymbol(
                MethodSymbol container,
                TypeSymbol type,
                int ordinal,
                RefKind refKind,
                string name = "",
                ImmutableArray<CustomModifier> customModifiers = default(ImmutableArray<CustomModifier>))
                    : base (container, type, ordinal, refKind, name)
            {
                _customModifiers = customModifiers;
            }

            public override bool IsParams
            {
                get { return true; }
            }

            public override ImmutableArray<CustomModifier> CustomModifiers
            {
                get { return _customModifiers.NullToEmpty(); }
            }

            public override ImmutableArray<CustomModifier> RefCustomModifiers
            {
                get { return ImmutableArray<CustomModifier>.Empty; }
            }

            internal sealed override void AddSynthesizedAttributes(Emit.PEModuleBuilder modulebuilder, ref ArrayBuilder<SynthesizedAttributeData> attributes)
            {
                base.AddSynthesizedAttributes(modulebuilder, ref attributes);

                var compilation = this.DeclaringCompilation;

                AddSynthesizedAttribute(ref attributes, compilation.TrySynthesizeAttribute(WellKnownMember.System_ParamArrayAttribute__ctor));
            }
        }
    }
}
