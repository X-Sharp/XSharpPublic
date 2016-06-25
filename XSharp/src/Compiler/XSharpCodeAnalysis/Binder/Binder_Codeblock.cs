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
            Debug.Assert(destination.IsCodeblock());

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
            var cbInst = new BoundAnonymousObjectCreationExpression(syntax,
                cbType.InstanceConstructors[0],
                new BoundExpression[] { cbDel }.ToImmutableArrayOrEmpty(),
                System.Collections.Immutable.ImmutableArray<BoundAnonymousPropertyDeclaration>.Empty, cbType);
            return new BoundConversion(syntax, cbInst, Conversion.ImplicitReference, false, isCast, ConstantValue.NotAvailable, destination);
        }
    }
}