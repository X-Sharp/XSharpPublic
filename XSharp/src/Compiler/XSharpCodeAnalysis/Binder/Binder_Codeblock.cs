using System.Collections.Generic;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class Binder
    {
        private BoundExpression BindCodeblock(CSharpSyntaxNode syntax, UnboundLambda unboundLambda, Conversion conversion, bool isCast, TypeSymbol destination, DiagnosticBag diagnostics)
        {
            Conversion conv = Conversion.ImplicitReference;

            if (!destination.IsCodeblock() && !destination.IsObjectType())
            {
                HashSet<DiagnosticInfo> useSiteDiagnostics = null;
                conv = Conversions.ClassifyConversion(Compilation.GetWellKnownType(WellKnownType.Vulcan_Codeblock), destination, ref useSiteDiagnostics);
                diagnostics.Add(syntax, useSiteDiagnostics);
            }

            Debug.Assert(destination.IsCodeblock() || conv.Exists);

            AnonymousTypeManager manager = this.Compilation.AnonymousTypeManager;
            var delegateSignature = new TypeSymbol[unboundLambda.ParameterCount + 1];
            var usualType = this.Compilation.GetWellKnownType(WellKnownType.Vulcan___Usual);
            for (int i = 0; i < delegateSignature.Length; i++)
            {
                delegateSignature[i] = usualType;
            }
            NamedTypeSymbol cbType = manager.ConstructCodeblockTypeSymbol(delegateSignature, syntax.Location);
            var delType = manager.GetCodeblockDelegateType(cbType);
            var _boundLambda = unboundLambda.Bind(delType);
            diagnostics.AddRange(_boundLambda.Diagnostics);
            var cbDel = new BoundConversion(
                syntax,
                _boundLambda,
                conversion,
                @checked: false,
                explicitCastInCode: false,
                constantValueOpt: ConstantValue.NotAvailable,
                type: delType)
            { WasCompilerGenerated = unboundLambda.WasCompilerGenerated };
            BoundExpression cbInst = new BoundAnonymousObjectCreationExpression(syntax,
                cbType.InstanceConstructors[0],
                new BoundExpression[] { cbDel }.ToImmutableArrayOrEmpty(),
                System.Collections.Immutable.ImmutableArray<BoundAnonymousPropertyDeclaration>.Empty, cbType)
            { WasCompilerGenerated = unboundLambda.WasCompilerGenerated }; ;
            if (conv != Conversion.ImplicitReference)
            {
                cbInst = new BoundConversion(syntax, cbInst, Conversion.ImplicitReference, false, false, ConstantValue.NotAvailable, Compilation.GetWellKnownType(WellKnownType.Vulcan_Codeblock))
                { WasCompilerGenerated = unboundLambda.WasCompilerGenerated }; ;
            }
            if (!conv.IsValid || (!isCast && conv.IsExplicit))
            {
                GenerateImplicitConversionError(diagnostics, syntax, conv, cbInst, destination);

                return new BoundConversion(
                    syntax,
                    cbInst,
                    conv,
                    false,
                    explicitCastInCode: isCast,
                    constantValueOpt: ConstantValue.NotAvailable,
                    type: destination,
                    hasErrors: true)
                { WasCompilerGenerated = unboundLambda.WasCompilerGenerated };
            }
            return new BoundConversion(
                syntax,
                cbInst,
                conv,
                false,
                explicitCastInCode: isCast,
                constantValueOpt: ConstantValue.NotAvailable,
                type: destination)
            { WasCompilerGenerated = unboundLambda.WasCompilerGenerated };
        }
    }
}