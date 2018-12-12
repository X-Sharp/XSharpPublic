using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;

namespace XSharp.MacroCompiler
{
    using Syntax;

    internal partial class Binder
    {
        internal static CompilationError MethodCallBindError(Expr expr, Symbol symbol, ArgList args, OverloadResult ovRes)
        {
            if (ovRes?.Valid != true)
            {
                if (symbol is SymbolList)
                    return expr.Error(ErrorCode.NoSuitableOverload, symbol.MemberName());
                else if (symbol is MethodSymbol)
                    return expr.Error(ErrorCode.ArgumentsNotMatch, symbol.MemberName());
                else if (symbol is MemberSymbol)
                    return expr.Error(ErrorCode.MemberNotMethod, symbol.MemberName());
                else
                    return expr.Error(ErrorCode.MemberNotFound);
            }

            if (ovRes.Unique == false)
                return expr.Error(ErrorCode.AmbiguousCall);

            return expr.Error(ErrorCode.MemberNotFound);
        }

        internal static CompilationError CtorCallBindError(Expr expr, Symbol symbol, ArgList args, OverloadResult ovRes)
        {
            if (ovRes?.Valid != true)
            {
                if (symbol is SymbolList)
                    return expr.Error(ErrorCode.NoSuitableCtor);
                else if (symbol is ConstructorSymbol)
                    return expr.Error(ErrorCode.ArgumentsNotMatchCtor);
                else
                    return expr.Error(ErrorCode.CtorNotFound);
            }

            if (ovRes.Unique == false)
                return expr.Error(ErrorCode.AmbiguousCall);

            return expr.Error(ErrorCode.CtorNotFound);
        }

        internal static CompilationError ArrayAccessBindError(Expr self, Symbol symbol, ArgList args, OverloadResult ovRes)
        {
            return self.Error(ErrorCode.IndexerNotFound, self);
        }

        internal static CompilationError ConversionError(Expr expr, TypeSymbol type)
        {
            return expr.Error(ErrorCode.NoConversion, expr.Datatype, type);
        }

        internal static CompilationError ImplicitConversionError(Expr expr, TypeSymbol type)
        {
            return expr.Error(ErrorCode.NoImplicitConversion, expr.Datatype, type);
        }

        internal static CompilationError LookupError(Expr expr, NameExpr name)
        {
            if (expr.Symbol is NamespaceSymbol)
                return name.Error(ErrorCode.TypeNotFoundInNamespace, name.Name, expr);
            if (expr.Symbol is TypeSymbol)
                return name.Error(ErrorCode.MemberNotFoundInType, name.Name, expr);
            return name.Error(ErrorCode.NotTypeOrNamespace, expr);
        }

        internal static CompilationError BinaryOperationError(BinaryExpr expr, BinaryOperatorKind kind, bool isLogic = false)
        {
            return expr.Error(ErrorCode.BinaryOperationNotFound, BinaryOperatorSymbol.OperatorSymbol(kind), expr.Left.Datatype, expr.Right.Datatype);
        }

        internal static CompilationError UnaryOperationError(UnaryExpr expr, UnaryOperatorKind kind, bool isLogic = false)
        {
            return expr.Error(ErrorCode.UnaryOperationNotFound, UnaryOperatorSymbol.OperatorSymbol(kind), expr.Expr.Datatype);
        }
    }
}
