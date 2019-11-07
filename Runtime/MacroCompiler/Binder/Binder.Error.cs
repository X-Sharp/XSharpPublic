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
                var self = (expr as MemberAccessExpr)?.Expr;
                bool isStatic = self == null;
                if (symbol is SymbolList)
                {
                    bool hasMethod = (symbol as SymbolList).SymbolTypes.HasFlag(MemberTypes.Method) || (symbol as SymbolList).SymbolTypes.HasFlag(MemberTypes.Constructor);
                    bool validStatic = false;
                    foreach (var s in (symbol as SymbolList).Symbols)
                    {
                        if ((s as MethodSymbol)?.Method.IsStatic == isStatic || symbol is ConstructorSymbol)
                            validStatic = true;
                    }
                    if (validStatic)
                        return expr.Error(ErrorCode.NoSuitableOverload, symbol.MemberName());
                    else if (isStatic)
                        return expr.Error(ErrorCode.NoStaticOverload, symbol.MemberName());
                    else
                        return expr.Error(ErrorCode.NoInstanceOverload, symbol.MemberName());
                }
                else if (symbol is MethodSymbol)
                {
                    if ((symbol as MethodSymbol)?.Method.IsStatic == isStatic || symbol is ConstructorSymbol)
                        return expr.Error(ErrorCode.ArgumentsNotMatch, symbol.MemberName());
                    else if (isStatic)
                        return expr.Error(ErrorCode.NoStaticMethod, symbol.MemberName());
                    else
                        return expr.Error(ErrorCode.NoInstanceMethod, symbol.MemberName());
                }
                else if (symbol is MemberSymbol)
                    return expr.Error(ErrorCode.MemberNotMethod, symbol.MemberName());
                else if (expr.Symbol != null)
                    return expr.Error(ErrorCode.NotAMethod, symbol);
                else if (expr is QualifiedNameExpr)
                    return expr.Error(ErrorCode.MemberNotFound, (expr as QualifiedNameExpr).Name);
                else if (expr is MemberAccessExpr exprAccess)
                {
                    if (exprAccess.Member is NameExpr memberName)
                        return expr.Error(ErrorCode.MemberNotFound, memberName.Name);
                    else
                        return expr.Error(ErrorCode.NameExpected);
                }
            }

            if (ovRes?.Unique == false)
            {
                string sMessage1 = "";
                string sMessage2 = "";
                if (ovRes?.Symbol is MethodSymbol msym)
                {
                    sMessage1 = msym.Signature;
                }
                if (ovRes?.Equivalent?.Symbol is MethodSymbol msym2)
                {
                    sMessage2 = msym2.Signature;
                }
                return expr.Error(ErrorCode.AmbiguousCall, sMessage1, sMessage2);
            }
            return expr.Error(ErrorCode.NotFound, "Expression", "");
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
            {
                string sMessage1 = "";
                string sMessage2 = "";
                if (ovRes?.Symbol is ConstructorSymbol msym)
                {
                    sMessage1 = msym.Signature;
                }
                if (ovRes?.Equivalent?.Symbol is ConstructorSymbol msym2)
                {
                    sMessage2 = msym2.Signature;
                }
                return expr.Error(ErrorCode.AmbiguousCall, sMessage1,sMessage2);
            }

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

        internal static CompilationError BinaryOperationError(BinaryExpr expr, BinaryOperatorKind kind, BindOptions options)
        {
            return expr.Error(ErrorCode.BinaryOperationNotFound, BinaryOperatorSymbol.OperatorSymbol(kind), expr.Left.Datatype, expr.Right.Datatype);
        }

        internal static CompilationError UnaryOperationError(UnaryExpr expr, UnaryOperatorKind kind, BindOptions options)
        {
            return expr.Error(ErrorCode.UnaryOperationNotFound, UnaryOperatorSymbol.OperatorSymbol(kind), expr.Expr.Datatype);
        }

        internal static CompilationError AccessModeError(Expr expr, Symbol s, Symbol.AccessMode access)
        {
            return expr.Error(ErrorCode.NoAccessMode, s, access);
        }
    }
}
