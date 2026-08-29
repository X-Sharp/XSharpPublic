using System;
using System.CodeDom;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace XSharp.Project.ShadowDesigner
{
    /// <summary>
    /// Translates the statement shapes the out-of-process WinForms Designer emits into the
    /// companion project's InitializeComponent into equivalent System.CodeDom objects, so
    /// the existing, production XSharpCodeGenerator can turn them into X# text -- reusing
    /// tested codegen instead of a second hand-rolled string templater (see
    /// EventHandlerSync's narrower, template-based approach for boilerplate handler stubs).
    ///
    /// Key wrinkle: the Designer's regenerated code uses IMPLICIT `this` everywhere
    /// (`oButton1.Location = ...`, not `this.oButton1.Location = ...`) and introduces local
    /// temp variables for nested-object properties on some 3rd-party controls. Both handled
    /// below.
    ///
    /// Deliberately syntax-only (no semantic model): distinguishing "this.field" chains from
    /// "Namespace.Type.StaticMember" chains is done by checking whether the chain's root
    /// identifier is a known field/local/`this`, not by resolving symbols -- good enough for
    /// InitializeComponent's constrained shape, not a general C#->X# transpiler.
    /// </summary>
    internal static class RoslynToCodeDom
    {
        public sealed class TranslationContext
        {
            public HashSet<string> KnownFieldNames { get; } = new HashSet<string>(StringComparer.Ordinal);
            public HashSet<string> LocalVarNames { get; } = new HashSet<string>(StringComparer.Ordinal);
            public List<string> SkippedStatements { get; } = new List<string>();
        }

        public static CodeStatement TranslateStatement(StatementSyntax stmt, TranslationContext ctx)
        {
            switch (stmt)
            {
                case LocalDeclarationStatementSyntax localDecl:
                    {
                        var decl = localDecl.Declaration;
                        string typeName = decl.Type.ToString();
                        // Only single-variable declarations are expected/handled (that's all the
                        // Designer emits for its CustomizableEdges-style temp-variable pattern).
                        var variable = decl.Variables.FirstOrDefault();
                        if (variable == null) return null;
                        ctx.LocalVarNames.Add(variable.Identifier.Text);
                        CodeExpression init = variable.Initializer != null
                            ? TranslateExpression(variable.Initializer.Value, ctx)
                            : null;
                        return new CodeVariableDeclarationStatement(typeName, variable.Identifier.Text, init);
                    }

                case ExpressionStatementSyntax exprStmt:
                    return TranslateExpressionStatement(exprStmt.Expression, ctx);

                case ReturnStatementSyntax _:
                    // InitializeComponent never wants a trailing RETURN in the X# CodeDom
                    // model -- the Designer's own parser rejects it. Skip silently rather than
                    // flagging as unsupported.
                    return null;

                default:
                    ctx.SkippedStatements.Add(stmt.ToString().Trim());
                    return null;
            }
        }

        private static CodeStatement TranslateExpressionStatement(ExpressionSyntax expr, TranslationContext ctx)
        {
            if (expr is AssignmentExpressionSyntax assignment)
            {
                if (assignment.OperatorToken.IsKind(SyntaxKind.PlusEqualsToken))
                {
                    return TranslateAttachEvent(assignment, ctx);
                }
                if (assignment.OperatorToken.IsKind(SyntaxKind.EqualsToken))
                {
                    var left = TranslateExpression(assignment.Left, ctx);
                    var right = TranslateExpression(assignment.Right, ctx);
                    if (left == null || right == null)
                    {
                        ctx.SkippedStatements.Add(assignment.ToString().Trim());
                        return null;
                    }
                    return new CodeAssignStatement(left, right);
                }
                ctx.SkippedStatements.Add(assignment.ToString().Trim());
                return null;
            }

            if (expr is InvocationExpressionSyntax invocation)
            {
                var invokeExpr = TranslateInvocation(invocation, ctx);
                if (invokeExpr == null)
                {
                    ctx.SkippedStatements.Add(invocation.ToString().Trim());
                    return null;
                }
                return new CodeExpressionStatement(invokeExpr);
            }

            ctx.SkippedStatements.Add(expr.ToString().Trim());
            return null;
        }

        private static CodeStatement TranslateAttachEvent(AssignmentExpressionSyntax assignment, TranslationContext ctx)
        {
            // Left side is always "target.EventName" (or implicit-this "EventName" -- not
            // observed in practice since events always belong to a field, e.g. oButton1.Click,
            // but handled for robustness).
            if (!(assignment.Left is MemberAccessExpressionSyntax eventAccess))
            {
                ctx.SkippedStatements.Add(assignment.ToString().Trim());
                return null;
            }
            var eventTarget = TranslateExpression(eventAccess.Expression, ctx);
            if (eventTarget == null)
            {
                ctx.SkippedStatements.Add(assignment.ToString().Trim());
                return null;
            }
            var eventRef = new CodeEventReferenceExpression(eventTarget, eventAccess.Name.Identifier.Text);

            // Right side: bare method-group ("oButton1_Click") or "new EventHandler(oButton1_Click)".
            ExpressionSyntax handlerExpr = assignment.Right;
            if (handlerExpr is ObjectCreationExpressionSyntax creation && creation.ArgumentList?.Arguments.Count == 1)
            {
                handlerExpr = creation.ArgumentList.Arguments[0].Expression;
            }

            CodeExpression handler;
            if (handlerExpr is IdentifierNameSyntax handlerId)
            {
                // A bare identifier here is always a method-group reference to a sibling
                // method on the same class (that's the only shape WinForms codegen ever
                // emits for event wiring) -- NOT a type name, so it must bypass
                // TranslateExpression's generic "unknown identifier -> type reference"
                // fallback (methods are never in KnownFieldNames) and always resolve as
                // SELF:name, matching the convention used everywhere else in this codebase
                // (e.g. EventHandlerSync's generated wiring).
                handler = new CodeFieldReferenceExpression(new CodeThisReferenceExpression(), handlerId.Identifier.Text);
            }
            else
            {
                handler = TranslateExpression(handlerExpr, ctx);
            }
            if (handler == null)
            {
                ctx.SkippedStatements.Add(assignment.ToString().Trim());
                return null;
            }
            return new CodeAttachEventStatement(eventRef, handler);
        }

        private static CodeExpression TranslateInvocation(InvocationExpressionSyntax invocation, TranslationContext ctx)
        {
            var args = invocation.ArgumentList.Arguments
                .Select(a => TranslateExpression(a.Expression, ctx))
                .ToArray();
            if (args.Any(a => a == null)) return null;

            if (invocation.Expression is IdentifierNameSyntax bareCall)
            {
                // Implicit-this call, e.g. "SuspendLayout()" -> SELF:SuspendLayout().
                return new CodeMethodInvokeExpression(new CodeThisReferenceExpression(), bareCall.Identifier.Text, args);
            }

            if (invocation.Expression is MemberAccessExpressionSyntax memberAccess)
            {
                // Distinguish "oButton1.Focus()" (instance call on a known field/local/this)
                // from "System.Drawing.Color.FromArgb(...)" (static call on a type) using the
                // same root-identifier heuristic as TranslateExpression.
                if (TryGetStaticTypeChain(memberAccess.Expression, ctx, out string typeName))
                {
                    var typeTarget = new CodeTypeReferenceExpression(typeName);
                    return new CodeMethodInvokeExpression(typeTarget, memberAccess.Name.Identifier.Text, args);
                }

                var instanceTarget = TranslateExpression(memberAccess.Expression, ctx);
                if (instanceTarget == null) return null;
                return new CodeMethodInvokeExpression(instanceTarget, memberAccess.Name.Identifier.Text, args);
            }

            return null;
        }

        public static CodeExpression TranslateExpression(ExpressionSyntax expr, TranslationContext ctx)
        {
            switch (expr)
            {
                case ParenthesizedExpressionSyntax paren:
                    return TranslateExpression(paren.Expression, ctx);

                case ThisExpressionSyntax _:
                    return new CodeThisReferenceExpression();

                case IdentifierNameSyntax id:
                    {
                        string name = id.Identifier.Text;
                        if (ctx.LocalVarNames.Contains(name)) return new CodeVariableReferenceExpression(name);
                        if (ctx.KnownFieldNames.Contains(name)) return new CodeFieldReferenceExpression(new CodeThisReferenceExpression(), name);
                        // Not a known local/field. A caller walking a multi-segment static
                        // chain (TryGetStaticTypeChain) never reaches this case for the
                        // chain's root -- it builds the dotted string itself without calling
                        // TranslateExpression. So a bare identifier reaching HERE is always a
                        // standalone, single-segment reference (e.g. "AutoScaleMode = ...",
                        // "Controls.Add(...)", "Name = ..." -- implicit-this Form properties/
                        // methods the Designer emits unqualified), never a real type on its
                        // own -- default to implicit SELF, matching every other convention in
                        // this codebase (e.g. EventHandlerSync's wiring).
                        return new CodeFieldReferenceExpression(new CodeThisReferenceExpression(), name);
                    }

                case MemberAccessExpressionSyntax memberAccess:
                    {
                        if (TryGetStaticTypeChain(memberAccess, ctx, out string wholeChainAsType))
                        {
                            // The whole expression (e.g. "System.Windows.Forms.AutoScaleMode.Font")
                            // is a pure static dotted chain -- split off the last segment as the
                            // static field/enum-member name, everything before it is the type.
                            int lastDot = wholeChainAsType.LastIndexOf('.');
                            if (lastDot > 0)
                            {
                                string typePart = wholeChainAsType.Substring(0, lastDot);
                                string memberPart = wholeChainAsType.Substring(lastDot + 1);
                                return new CodeFieldReferenceExpression(new CodeTypeReferenceExpression(typePart), memberPart);
                            }
                            return new CodeTypeReferenceExpression(wholeChainAsType);
                        }

                        var target = TranslateExpression(memberAccess.Expression, ctx);
                        if (target == null) return null;
                        // X# uses ':' for both field and property access -- CodeFieldReferenceExpression
                        // and CodePropertyReferenceExpression render identically in
                        // XSharpCodeGenerator, so there's no need for a semantic model to
                        // disambiguate the two.
                        return new CodeFieldReferenceExpression(target, memberAccess.Name.Identifier.Text);
                    }

                case ObjectCreationExpressionSyntax creation:
                    {
                        var args = creation.ArgumentList?.Arguments
                            .Select(a => TranslateExpression(a.Expression, ctx))
                            .ToArray() ?? Array.Empty<CodeExpression>();
                        if (args.Any(a => a == null)) return null;
                        return new CodeObjectCreateExpression(creation.Type.ToString(), args);
                    }

                case InvocationExpressionSyntax invocation:
                    return TranslateInvocation(invocation, ctx);

                case LiteralExpressionSyntax literal:
                    return TranslateLiteral(literal);

                case PrefixUnaryExpressionSyntax prefixUnary when prefixUnary.OperatorToken.IsKind(SyntaxKind.MinusToken)
                                                                    && prefixUnary.Operand is LiteralExpressionSyntax:
                    {
                        var inner = TranslateLiteral((LiteralExpressionSyntax)prefixUnary.Operand);
                        if (inner is CodePrimitiveExpression prim && prim.Value is IConvertible convertible)
                        {
                            // Negate common numeric primitive kinds; anything else falls through
                            // to "unsupported" below rather than risk a silently wrong value.
                            switch (convertible)
                            {
                                case int i: return new CodePrimitiveExpression(-i);
                                case double d: return new CodePrimitiveExpression(-d);
                                case float f: return new CodePrimitiveExpression(-f);
                                case long l: return new CodePrimitiveExpression(-l);
                            }
                        }
                        return null;
                    }

                default:
                    return null;
            }
        }

        private static CodeExpression TranslateLiteral(LiteralExpressionSyntax literal)
        {
            switch (literal.Kind())
            {
                case SyntaxKind.StringLiteralExpression:
                    return new CodePrimitiveExpression(literal.Token.ValueText);
                case SyntaxKind.NumericLiteralExpression:
                    return new CodePrimitiveExpression(literal.Token.Value);
                case SyntaxKind.TrueLiteralExpression:
                    return new CodePrimitiveExpression(true);
                case SyntaxKind.FalseLiteralExpression:
                    return new CodePrimitiveExpression(false);
                case SyntaxKind.NullLiteralExpression:
                    return new CodePrimitiveExpression(null);
                default:
                    return null;
            }
        }

        /// <summary>
        /// True if <paramref name="expr"/> is a pure dotted-identifier chain (no invocations,
        /// no indexing) whose ROOT identifier is NOT `this`, a known local, or a known field --
        /// i.e. it reads as a namespace/type path like "System.Windows.Forms.AutoScaleMode"
        /// rather than an instance member-access chain. Walks down to the root without
        /// consuming/mutating anything; safe to call speculatively.
        /// </summary>
        private static bool TryGetStaticTypeChain(ExpressionSyntax expr, TranslationContext ctx, out string dottedName)
        {
            var segments = new List<string>();
            ExpressionSyntax current = expr;
            while (true)
            {
                if (current is MemberAccessExpressionSyntax ma)
                {
                    segments.Insert(0, ma.Name.Identifier.Text);
                    current = ma.Expression;
                    continue;
                }
                if (current is IdentifierNameSyntax id)
                {
                    segments.Insert(0, id.Identifier.Text);
                    string rootName = id.Identifier.Text;
                    bool rootIsInstanceRef = ctx.LocalVarNames.Contains(rootName) || ctx.KnownFieldNames.Contains(rootName);
                    if (rootIsInstanceRef)
                    {
                        dottedName = null;
                        return false;
                    }
                    if (segments.Count < 2)
                    {
                        // A single bare segment (e.g. "Controls" as the target of
                        // "Controls.Add(...)") is never a standalone valid static reference in
                        // this codebase's generated code -- real static/type paths are always
                        // fully qualified (e.g. "System.Drawing.Color"). Treat it as an
                        // implicit-this member instead (handled by TranslateExpression's
                        // IdentifierNameSyntax case).
                        dottedName = null;
                        return false;
                    }
                    dottedName = string.Join(".", segments);
                    return true;
                }
                // Anything else (this-expression, invocation, cast, ...) breaks the pure-chain
                // assumption -- not a static type path.
                dottedName = null;
                return false;
            }
        }
    }
}
