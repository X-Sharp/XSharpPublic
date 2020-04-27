//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Microsoft.CodeAnalysis.CSharp
{
    // Extension methods responsible for looking up 'our' types
    static internal class TypeExtensions
    {
        static internal NamedTypeSymbol PszType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp___Psz);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan___Psz);
            }
        }
        static internal NamedTypeSymbol UsualType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp___Usual);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan___Usual);
            }
        }
        static internal NamedTypeSymbol SymbolType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp___Symbol);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan___Symbol);
            }
        }

        static internal NamedTypeSymbol WinBoolType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp___WinBool);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan___WinBool);
            }
        }

        static internal NamedTypeSymbol DateType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp___Date);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan___VODate);
            }
        }

        static internal NamedTypeSymbol FloatType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp___Float);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan___VOFloat);
            }
        }
        static internal NamedTypeSymbol ArrayType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp___Array);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan___Array);
            }
        }
        static internal NamedTypeSymbol ArrayBaseType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp___ArrayBase_T1);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan___Array);
            }
        }

        static internal NamedTypeSymbol IndexerType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
                return compilation.GetWellKnownType(WellKnownType.XSharp_IIndexer);
            return null;
        }
        static internal NamedTypeSymbol NamedIndexerType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
                return compilation.GetWellKnownType(WellKnownType.XSharp_INamedIndexer);
            return null;
        }
        static internal NamedTypeSymbol IndexedPropertiesType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
                return compilation.GetWellKnownType(WellKnownType.XSharp_IIndexedProperties);
            return null;
        }


        static internal NamedTypeSymbol CodeBlockType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp_Codeblock);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan_Codeblock);
            }
        }
        static internal NamedTypeSymbol ClassLibraryType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp_Internal_ClassLibraryAttribute);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan_Internal_VulcanClassLibraryAttribute);
            }
        }
        static internal NamedTypeSymbol ImplicitNamespaceType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp_ImplicitNamespaceAttribute);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan_VulcanImplicitNamespaceAttribute);
            }
        }

        static internal NamedTypeSymbol RuntimeFunctionsType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp_RT_Functions);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.VulcanRTFuncs_Functions);
            }
        }
        static internal NamedTypeSymbol CompilerServicesType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp_Internal_CompilerServices);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan_Internal_CompilerServices);
            }
        }
        static internal NamedTypeSymbol VOStructAttributeType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp_Internal_VoStructAttribute);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan_Internal_VOStructAttribute);
            }
        }

        static internal bool IsXsCompilerGenerated(this Symbol symbol) 
        {
            if (symbol.Kind == SymbolKind.Local || symbol.Kind == SymbolKind.Parameter)
            {
                if (symbol.Name != null && symbol.Name.StartsWith("Xs$") )
                {
                    return true;
                }
            }
            if (symbol is SourceLocalSymbol local)
            {
                var syntax = local.GetDeclaratorSyntax();
                var vardecl = syntax.Parent as VariableDeclarationSyntax;
                if (vardecl != null && vardecl.XGenerated)
                    return true;
            }
            return false;
        }

        static internal bool IsValidVOUsualType(this TypeSymbol type, CSharpCompilation compilation)
        {
            switch (type.SpecialType)
            {
                case SpecialType.System_Int32:
                case SpecialType.System_Int64:
                case SpecialType.System_Boolean:
                case SpecialType.System_String:
                case SpecialType.System_IntPtr:
                case SpecialType.System_Decimal:
                case SpecialType.System_DateTime:
                case SpecialType.System_Object:
                    return true;
                
            }
            if (type == compilation.ArrayType())
                return true;
            if (type == compilation.CodeBlockType())
                return true;
            if (type == compilation.DateType())
                return true;
            if (type == compilation.FloatType())
                return true;
            if (type == compilation.SymbolType())
                return true;
            if (type == compilation.PszType())
                return true;
            return false;
        }
        static internal TypeSymbol LargestOperand(this BoundBinaryOperator binop, Compilation compilation, bool checkConversions = true)
        {
            if (binop.OperatorKind.IsComparison() || !binop.Type.IsIntegralType())
                return binop.Type;

            var left = binop.Left;
            var right = binop.Right;
            if (checkConversions)
            {
                if (left is BoundConversion lconv)
                {
                    left = lconv.Operand;
                }
                if (right is BoundConversion rconv)
                {
                    right = rconv.Operand;
                }
            }
            var leftType = left.Type;
            var rightType = right.Type;
            if (left is BoundBinaryOperator binopl)
                leftType = binopl.LargestOperand(compilation) ;
            if (right is BoundBinaryOperator binopr)
                rightType = binopr.LargestOperand(compilation);
            if (left is BoundLiteral)
                leftType = left.ConstantType(compilation);
            if (right is BoundLiteral)
                rightType = right.ConstantType(compilation);

            var leftSize = leftType.SpecialType.SizeInBytes();
            var rightSize = rightType.SpecialType.SizeInBytes();
            if (leftSize >= rightSize)
                return leftType;
            return rightType;
        }

        static internal TypeSymbol ConstantType(this BoundExpression expression, Compilation compilation)
        {
            var type = expression.Type;
            if (expression.ConstantValue == null)
                return type;
            if (!expression.ConstantValue.IsIntegral)
                return type;
            var stype = type.SpecialType;
            if (type.SpecialType.IsSignedIntegralType())
            {
                var value = expression.ConstantValue.Int64Value;
                if (value == 0)
                {
                    stype = SpecialType.System_Byte;
                }
                else if (value < 0)
                {
                    if (value >= sbyte.MinValue)
                        stype = SpecialType.System_SByte;
                    else if (value >= short.MinValue)
                        stype = SpecialType.System_Int16;
                    else if (value >= int.MinValue)
                        stype = SpecialType.System_Int32;
                    else
                        stype = SpecialType.System_Int64;
                }
                // > 0
                else
                {
                    // prefer unsigned types when < 32 bits
                    if (value <= byte.MaxValue)
                        stype = SpecialType.System_Byte;
                    else if (value <= ushort.MaxValue)
                        stype = SpecialType.System_UInt16;
                    else if (value <= int.MaxValue)
                        stype = SpecialType.System_Int32;
                    else if (value <= uint.MaxValue)
                        stype = SpecialType.System_UInt32;
                    else
                        stype = SpecialType.System_Int64;
                }
            }
            else
            {
                // UnSigned
                var uvalue = expression.ConstantValue.UInt64Value;
                if (uvalue <= (ulong)sbyte.MaxValue)
                    stype = SpecialType.System_SByte;
                else if (uvalue <= (ulong)byte.MaxValue)
                    stype = SpecialType.System_Byte;
                else if (uvalue <= (ulong)short.MaxValue)
                    stype = SpecialType.System_Int16;
                else if (uvalue <= (ulong)ushort.MaxValue)
                    stype = SpecialType.System_UInt16;
                else if (uvalue <= (ulong)int.MaxValue)
                    stype = SpecialType.System_Int32;
                else if (uvalue <= (ulong)uint.MaxValue)
                    stype = SpecialType.System_UInt32;
                else if (uvalue <= (ulong)long.MaxValue)
                    stype = SpecialType.System_Int64;
                else
                    stype = SpecialType.System_UInt64;
            }
            return (TypeSymbol) compilation.GetSpecialType(stype);
        }
    }
}
