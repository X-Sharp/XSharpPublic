//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
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

            internal override NamedTypeSymbol GetDeclaredBaseType(ConsList<TypeSymbol> basesBeingResolved)
            {
                return this.DeclaringCompilation.CodeBlockType();
            }
        }

        private sealed partial class CodeblockEvalMethod : SynthesizedMethodBase
        {
            private readonly ImmutableArray<ParameterSymbol> _parameters;

            internal CodeblockEvalMethod(NamedTypeSymbol container)
                : base(container, "Eval")
            {
                ImmutableArray<CustomModifier> customModifiers;
                if (this.Manager.Compilation.Options.XSharpRuntime)
                {
                    customModifiers = default;
                }
                else
                {
                    customModifiers = new[] { CSharpCustomModifier.CreateOptional(this.Manager.Compilation.GetWellKnownType(WellKnownType.System_Runtime_CompilerServices_IsConst)) }.ToImmutableArray();
                }
                _parameters = ImmutableArray.Create<ParameterSymbol>(
                        new CodeBlockEvalMethodParameterSymbol(this, TypeWithAnnotations.Create(this.Manager.Compilation.CreateArrayTypeSymbol(this.Manager.UsualType)), 0, RefKind.None, XSharpSpecialNames.ClipperArgs,
                            customModifiers: customModifiers)
                    );

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

            public override TypeWithAnnotations ReturnTypeWithAnnotations
            {
                get { return TypeWithAnnotations.Create(this.Manager.UsualType); }
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
                // The lambda expression has a fixed number of arguments.
                // The parameters are received an an array of usuals. This array may be to long or to short
                // The runtime function _BlockArg() extract the right element from the array or NIL when the
                // array is not long enough.
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
                MethodSymbol argMethod = DeclaringCompilation.CodeBlockType().GetMembers(ReservedNames.BlockArg).First() as MethodSymbol;

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

        internal sealed class CodeBlockEvalMethodParameterSymbol: SynthesizedParameterSymbolBase
        {
            ImmutableArray<CustomModifier> _customModifiers;

            public CodeBlockEvalMethodParameterSymbol(
                MethodSymbol container,
                TypeWithAnnotations type,
                int ordinal,
                RefKind refKind,
                string name = "",
                ImmutableArray<CustomModifier> customModifiers = default)
                    : base(container, type, ordinal, refKind, name)
            {
                _customModifiers = customModifiers;

            }

            public override bool IsParams
            {
                get { return true; }
            }


            public override ImmutableArray<CustomModifier> RefCustomModifiers
            {
                get { return _customModifiers.NullToEmpty(); }
            }
            internal override MarshalPseudoCustomAttributeData MarshallingInformation
            {
                get
                {
                    return null;
                }
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
