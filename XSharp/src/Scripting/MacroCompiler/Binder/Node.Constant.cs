using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;
using static System.Diagnostics.Debug;

namespace XSharp.MacroCompiler.Syntax
{
    using static TokenAttr;

    abstract internal partial class Node
    {
        internal virtual bool IsConstant { get => false; }
    }
    abstract internal partial class Expr : Node
    {
        internal override bool IsConstant { get => Symbol.IsConstant(); }
    }
    abstract internal partial class TypeExpr : Expr
    {
    }
    abstract internal partial class NameExpr : TypeExpr
    {
    }
    internal partial class CachedExpr : Expr
    {
    }
    internal partial class NativeTypeExpr : TypeExpr
    {
    }
    internal partial class IdExpr : NameExpr
    {
    }
    internal partial class MemberAccessExpr : Expr
    {
    }
    internal partial class QualifiedNameExpr : NameExpr
    {
    }
    internal partial class AssignExpr : Expr
    {
    }
    internal partial class AssignOpExpr : AssignExpr
    {
    }
    internal partial class BinaryExpr : Expr
    {
        internal override bool IsConstant { get => Left.IsConstant && Right.IsConstant; }
    }
    internal partial class BinaryLogicExpr : BinaryExpr
    {
        internal override bool IsConstant { get => Left.IsConstant && Right.IsConstant; }
    }
    internal partial class UnaryExpr : Expr
    {
        internal override bool IsConstant { get => Expr.IsConstant; }
    }
    internal partial class PrefixExpr : UnaryExpr
    {
        internal override bool IsConstant { get => Expr.IsConstant; }
    }
    internal partial class PostfixExpr : UnaryExpr
    {
        internal override bool IsConstant { get => Expr.IsConstant; }
    }
    internal partial class LiteralExpr : Expr
    {
    }
    internal partial class SelfExpr : Expr
    {
    }
    internal partial class SuperExpr : Expr
    {
    }
    internal partial class CheckedExpr : Expr
    {
        internal override bool IsConstant { get => Expr.IsConstant; }
    }
    internal partial class UncheckedExpr : Expr
    {
        internal override bool IsConstant { get => Expr.IsConstant; }
    }
    internal partial class TypeOfExpr : Expr
    {
    }
    internal partial class SizeOfExpr : Expr
    {
    }
    internal partial class DefaultExpr : Expr
    {
    }
    internal partial class TypeCast : Expr
    {
        internal override bool IsConstant { get => Expr.IsConstant; }
    }
    internal partial class TypeConversion : TypeCast
    {
    }
    internal partial class IsExpr : Expr
    {
    }
    internal partial class AsTypeExpr : Expr
    {
        internal override bool IsConstant { get => Expr.IsConstant; }
    }
    internal partial class MethodCallExpr : Expr
    {
    }
    internal partial class CtorCallExpr : MethodCallExpr
    {
    }
    internal partial class IntrinsicCallExpr : MethodCallExpr
    {
    }
    internal partial class ArrayAccessExpr : MethodCallExpr
    {
    }
    internal partial class EmptyExpr : Expr
    {
    }
    internal partial class ExprList : Expr
    {
    }
    internal partial class LiteralArray : Expr
    {
    }
    internal partial class IifExpr : Expr
    {
    }
    internal partial class AliasExpr : Expr
    {
    }
    internal partial class SubstrExpr : BinaryExpr
    {
    }
    internal partial class AutoVarExpr : Expr
    {
    }
    internal partial class Arg : Node
    {
    }
    internal partial class ArgList : Node
    {
    }
    internal partial class Codeblock : Node
    {
    }
}
