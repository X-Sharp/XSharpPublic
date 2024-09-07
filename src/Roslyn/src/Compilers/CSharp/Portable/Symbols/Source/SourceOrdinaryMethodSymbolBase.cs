// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#nullable disable

using System.Collections.Immutable;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis.CSharp.Emit;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.PooledObjects;
using Roslyn.Utilities;
#if XSHARP
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;
#endif

namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    /// <summary>
    /// Unlike <see cref="SourceOrdinaryMethodSymbol"/>, this type doesn't depend
    /// on any specific kind of syntax node associated with it. Any syntax node is good enough
    /// for it.
    /// </summary>
#if XSHARP
    internal abstract partial class SourceOrdinaryMethodSymbolBase : SourceOrdinaryMethodOrUserDefinedOperatorSymbol
#else
    internal abstract class SourceOrdinaryMethodSymbolBase : SourceOrdinaryMethodOrUserDefinedOperatorSymbol
#endif
    {
#if XSHARP
        private string _name;
#else
        private readonly string _name;
#endif

        protected SourceOrdinaryMethodSymbolBase(
            NamedTypeSymbol containingType,
            string name,
            Location location,
            CSharpSyntaxNode syntax,
            bool isIterator,
            (DeclarationModifiers declarationModifiers, Flags flags) modifiersAndFlags) :
            base(containingType,
                 syntax.GetReference(),
                 location,
                 isIterator: isIterator,
                 modifiersAndFlags)
        {
            _name = name;
#if XSHARP
                    // additional checks to see if we are overriding clipper with non clipper etc.
                    overriddenOrExplicitlyImplementedMethod = validateMethod(overriddenOrExplicitlyImplementedMethod, diagnostics, location);

#else
#endif
        }
#if XSHARP
                if (this.HasClipperCallingConvention() != overriddenOrExplicitlyImplementedMethod.HasClipperCallingConvention())
                {
                    diagnostics.Add(ErrorCode.ERR_InterfaceImplementationDifferentCallingConvention, Locations[0], this,overriddenOrExplicitlyImplementedMethod);
                    overriddenOrExplicitlyImplementedMethod = null;
                }
#endif

        protected sealed override void LazyAsyncMethodChecks(CancellationToken cancellationToken)
        {
            if (!this.IsAsync)
            {
                CompleteAsyncMethodChecks(diagnosticsOpt: null, cancellationToken: cancellationToken);
                return;
            }

            var diagnostics = BindingDiagnosticBag.GetInstance();
            AsyncMethodChecks(diagnostics);

            CompleteAsyncMethodChecks(diagnostics, cancellationToken);
            diagnostics.Free();
        }

        private void CompleteAsyncMethodChecks(BindingDiagnosticBag diagnosticsOpt, CancellationToken cancellationToken)
        {
            if (state.NotePartComplete(CompletionPart.StartAsyncMethodChecks))
            {
                if (diagnosticsOpt != null)
                {
                    AddDeclarationDiagnostics(diagnosticsOpt);
                }

                CompleteAsyncMethodChecksBetweenStartAndFinish();
                state.NotePartComplete(CompletionPart.FinishAsyncMethodChecks);
            }
            else
            {
                state.SpinWaitComplete(CompletionPart.FinishAsyncMethodChecks, cancellationToken);
            }
        }

        protected abstract void CompleteAsyncMethodChecksBetweenStartAndFinish();

        public abstract override ImmutableArray<TypeParameterSymbol> TypeParameters { get; }

        public abstract override string GetDocumentationCommentXml(CultureInfo preferredCulture = null, bool expandIncludes = false, CancellationToken cancellationToken = default(CancellationToken));

        public override string Name
        {
            get
            {
                return _name;
            }
        }

        protected abstract override SourceMemberMethodSymbol BoundAttributesSource { get; }

        internal abstract override OneOrMany<SyntaxList<AttributeListSyntax>> GetAttributeDeclarations();

#if XSHARP
            else if (IsOverride && IsNew)
#else
#endif
#if !XSHARP // TODO nvk: Possibly add this check after the other errors have been added, only if /vo3 is not used (it is a warning in X#)
#endif
#if XSHARP
                var syntax = this.DeclaringSyntaxReferences[0].GetSyntax();
                bool wasExplicitVirtual = !this.DeclaringCompilation.Options.VirtualInstanceMethods;
                var node = syntax.XNode;
                if (node is not XP.IMemberContext ent)
                {
                    ent = node.GetChild(0) as XP.IMemberContext;
                }
                if (ent != null)
                {
                    wasExplicitVirtual = ent.Data.HasExplicitVirtual;
                }
                // Disable warning when compiling with /vo3
                if (wasExplicitVirtual)
                {
                   // '{0}' is a new virtual member in sealed type '{1}'
                   diagnostics.Add(ErrorCode.ERR_NewVirtualInSealed, location, this, ContainingType);
                }
#else
#endif
        internal override void AddSynthesizedAttributes(PEModuleBuilder moduleBuilder, ref ArrayBuilder<SynthesizedAttributeData> attributes)
        {
            base.AddSynthesizedAttributes(moduleBuilder, ref attributes);

            if (this.IsExtensionMethod)
            {
                // No need to check if [Extension] attribute was explicitly set since
                // we'll issue CS1112 error in those cases and won't generate IL.
                var compilation = this.DeclaringCompilation;

                AddSynthesizedAttribute(ref attributes, compilation.TrySynthesizeAttribute(
                    WellKnownMember.System_Runtime_CompilerServices_ExtensionAttribute__ctor));
            }
        }
    }
}
