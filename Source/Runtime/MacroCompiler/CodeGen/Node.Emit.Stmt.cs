using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection.Emit;
using System.Runtime.CompilerServices;

namespace XSharp.MacroCompiler.Syntax
{
    using static CodeGen;

    abstract internal partial class Stmt : Node
    {
        internal List<Action> FinallyClauses;
        internal virtual void EmitStmt(ILGenerator ilg) { throw new InternalError(); }
        internal sealed override void Emit(ILGenerator ilg)
        {
            if (RequireExceptionHandling)
            {
                FinallyClauses = new List<Action>();
                ilg.BeginExceptionBlock();
            }
            EmitStmt(ilg);
            if (RequireExceptionHandling)
            {
                ilg.BeginFinallyBlock();
                if (FinallyClauses.Count > 0)
                {
                    foreach (var a in FinallyClauses) a();
                }
                ilg.EndExceptionBlock();
            }
        }
    }
    internal partial interface ILoopableStmt
    {
        void EmitLoop(ILGenerator ilg);
    }
    internal partial interface IExitableStmt
    {
        void EmitExit(ILGenerator ilg);
    }
    internal partial class StmtBlock : Stmt
    {
        internal override void EmitStmt(ILGenerator ilg)
        {
            foreach(var s in StmtList)
                s.Emit(ilg);
        }
    }
    internal partial class ExprStmt : Stmt
    {
        internal override void EmitStmt(ILGenerator ilg)
        {
            Expr.Emit(ilg, false);
        }
    }
    internal partial class ExprResultStmt : ExprStmt
    {
        internal override void EmitStmt(ILGenerator ilg)
        {
            bool isVoid = true;
            if (Expr != null)
            {
                isVoid &= Expr.Datatype.IsVoid;
                Expr.Emit(ilg, Symbol != null);
            }
            if (Symbol != null)
            {
                if (isVoid)
                {
                    EmitDefault(ilg, (TypeSymbol)Symbol);
                }
                if (TargetEntity != null)
                {
                    if (TargetEntity.Symbol == null)
                    {
                        var temp = ilg.DeclareLocal(((TypeSymbol)Symbol).Type);
                        TargetEntity.Symbol = new LocalSymbol((TypeSymbol)Symbol, temp.LocalIndex);
                    }
                    TargetEntity.Symbol.EmitSet(ilg);
                }
            }
        }
    }
    internal partial class ReturnStmt : ExprResultStmt
    {
        internal override void EmitStmt(ILGenerator ilg)
        {
            base.EmitStmt(ilg);
            if (TargetEntity is Script s)
            {
                if (s.End == null)
                    s.End = ilg.DefineLabel();
                ilg.Emit(OpCodes.Br, s.End.Value);
            }
        }
    }
    internal partial class DeclStmt : Stmt
    {
        internal override void EmitStmt(ILGenerator ilg)
        {
            foreach (var decl in VarDecls)
                decl.Emit(ilg);
        }
    }
    internal partial class VarDecl : Node
    {
        internal override void Emit(ILGenerator ilg)
        {
            if (Symbol is LocalSymbol loc)
            {
                loc.Declare(ilg);
                if (Initializer != null)
                    Initializer.Emit(ilg, false);
            }
        }
    }
    internal partial class MemVarDecl : VarDecl
    {
        // Emit handled by parent
    }
    internal partial class ImpliedVarDecl : VarDecl
    {
        // Emit handled by parent
    }
    internal partial class FieldDeclStmt : Stmt
    {
        internal override void EmitStmt(ILGenerator ilg)
        {
            // Nothing to emit
        }
    }
    internal partial class EmptyStmt : Stmt
    {
        internal override void EmitStmt(ILGenerator ilg)
        {
            // Nothing to emit
        }
    }
    internal partial class WhileStmt : Stmt, ILoopableStmt, IExitableStmt
    {
        internal Label Begin;
        internal Label End;
        internal override void EmitStmt(ILGenerator ilg)
        {
            Begin = ilg.DefineLabel();
            End = ilg.DefineLabel();
            ilg.MarkLabel(Begin);
            Cond.Emit(ilg, true);
            ilg.Emit(OpCodes.Brfalse, End);
            Stmt.Emit(ilg);
            ilg.Emit(OpCodes.Br, Begin);
            ilg.MarkLabel(End);
        }
        public void EmitLoop(ILGenerator ilg)
        {
            ilg.Emit(OpCodes.Br, Begin);
        }
        public void EmitExit(ILGenerator ilg)
        {
            ilg.Emit(OpCodes.Br, End);
        }
    }
    internal partial class RepeatStmt : WhileStmt, ILoopableStmt, IExitableStmt
    {
        Label? Cont = null;
        internal override void EmitStmt(ILGenerator ilg)
        {
            Begin = ilg.DefineLabel();
            End = ilg.DefineLabel();
            ilg.MarkLabel(Begin);
            Stmt.Emit(ilg);
            if (Cont != null)
                ilg.MarkLabel(Cont.Value);
            Cond.Emit(ilg, true);
            ilg.Emit(OpCodes.Brfalse, Begin);
            ilg.MarkLabel(End);
        }
        public new void EmitLoop(ILGenerator ilg)
        {
            if (Cont == null)
                Cont = ilg.DefineLabel();
            ilg.Emit(OpCodes.Br, Cont.Value);
        }
    }
    internal partial class ForBaseStmt : Stmt, ILoopableStmt, IExitableStmt
    {
        Label Begin;
        Label? Cont = null;
        Label End;
        internal override void EmitStmt(ILGenerator ilg)
        {
            if (IterDecl != null)
            {
                IterDecl.Emit(ilg);
            }
            else
            {
                IterInit.Emit(ilg, false);
            }
            Begin = ilg.DefineLabel();
            End = ilg.DefineLabel();
            ilg.MarkLabel(Begin);
            WhileExpr.Emit(ilg, true);
            ilg.Emit(OpCodes.Brfalse, End);
            InnerDecl?.Emit(ilg);
            Stmt.Emit(ilg);
            if (Cont != null)
                ilg.MarkLabel(Cont.Value);
            IncrExpr?.Emit(ilg, false);
            ilg.Emit(OpCodes.Br, Begin);
            ilg.MarkLabel(End);
            if (Dispose)
            {
                FinallyClauses.Add(() => {
                    var loc = IterDecl.Symbol as LocalSymbol;
                    ilg.Emit(OpCodes.Ldloc, loc.Index);
                    ilg.Emit(OpCodes.Isinst, Compilation.Get(WellKnownTypes.System_IDisposable).Type);
                    ilg.Emit(OpCodes.Callvirt, (Compilation.Get(WellKnownMembers.System_IDisposable_Dispose) as MethodSymbol).Method);
                });
            }
        }
        public void EmitLoop(ILGenerator ilg)
        {
            if (Cont == null)
                Cont = ilg.DefineLabel();
            ilg.Emit(OpCodes.Br, Cont.Value);
        }
        public void EmitExit(ILGenerator ilg)
        {
            ilg.Emit(OpCodes.Br, End);
        }
    }
    internal partial class ForStmt : ForBaseStmt, ILoopableStmt, IExitableStmt
    {
        // Emit handled by parent
    }
    internal partial class ForeachStmt : ForBaseStmt, ILoopableStmt, IExitableStmt
    {
        // Emit handled by parent
    }
    internal partial class IfStmt : Stmt
    {
        internal override void EmitStmt(ILGenerator ilg)
        {
            Cond.Emit(ilg, true);
            var lb = ilg.DefineLabel();
            var le = ilg.DefineLabel();
            ilg.Emit(OpCodes.Brtrue, lb);
            StmtElse?.Emit(ilg);
            ilg.Emit(OpCodes.Br, le);
            ilg.MarkLabel(lb);
            StmtIf?.Emit(ilg);
            ilg.MarkLabel(le);
        }
    }
    internal partial class DoCaseStmt : Stmt
    {
        internal override void EmitStmt(ILGenerator ilg)
        {
            var le = ilg.DefineLabel();
            foreach (var c in Cases)
            {
                c.Emit(ilg, le);
            }
            Otherwise.Emit(ilg);
            ilg.MarkLabel(le);
        }
    }
    internal partial class CaseBlock : Node
    {
        internal void Emit(ILGenerator ilg, Label end)
        {
            Cond.Emit(ilg, true);
            var lf = ilg.DefineLabel();
            ilg.Emit(OpCodes.Brfalse, lf);
            Stmt.Emit(ilg);
            ilg.Emit(OpCodes.Br, end);
            ilg.MarkLabel(lf);
        }
    }

    internal partial class SwitchStmt : Stmt
    {
        internal override void EmitStmt(ILGenerator ilg)
        {
            Expr.Emit(ilg, true);
            SwitchValue.Declare(ilg);
            SwitchValue.EmitSet(ilg);
            var le = ilg.DefineLabel();
            foreach (var sb in SwitchBlocks)
            {
                sb.Emit(ilg, le);
            }
            ilg.MarkLabel(le);
        }
    }
    internal partial class SwitchBlock : Node
    {
        Label? _entry = null;
        Label? _exit = null;
        internal Label Entry(ILGenerator ilg) => _entry != null ? _entry.Value : (_entry = ilg.DefineLabel()).Value;
        internal Label Exit(ILGenerator ilg) => _exit != null ? _exit.Value : (_exit = ilg.DefineLabel()).Value;
        internal sealed override void Emit(ILGenerator ilg)
        {
            throw Error(ErrorCode.Internal);
        }
        internal virtual void Emit(ILGenerator ilg, Label end)
        {
            ilg.MarkLabel(Entry(ilg));
            if (Stmt != null)
            {
                Stmt?.Emit(ilg);
                ilg.Emit(OpCodes.Br, end);
            }
            else
            {
                ilg.Emit(OpCodes.Br, NextBlock.Entry(ilg));
            }
            ilg.MarkLabel(Exit(ilg));
        }
    }
    internal partial class SwitchBlockExpr : SwitchBlock
    {
        internal override void Emit(ILGenerator ilg, Label end)
        {
            Cond.Emit(ilg, true);
            ilg.Emit(OpCodes.Brfalse, Exit(ilg));
            if (When != null)
            {
                When.Emit(ilg, true);
                ilg.Emit(OpCodes.Brfalse, Exit(ilg));
            }
            base.Emit(ilg, end);
        }
    }
    internal partial class SwitchBlockType : SwitchBlock
    {
        internal override void Emit(ILGenerator ilg, Label end)
        {
            Cond.Emit(ilg, true);
            ilg.Emit(OpCodes.Brfalse, Exit(ilg));
            if (When != null)
            {
                When.Emit(ilg, true);
                ilg.Emit(OpCodes.Brfalse, Exit(ilg));
            }
            base.Emit(ilg, end);
        }
    }
    internal partial class ExitStmt : Stmt
    {
        internal override void EmitStmt(ILGenerator ilg)
        {
            Outer.EmitExit(ilg);
        }
    }
    internal partial class LoopStmt : Stmt
    {
        internal override void EmitStmt(ILGenerator ilg)
        {
            Outer.EmitLoop(ilg);
        }
    }
    internal partial class ThrowStmt : Stmt
    {
        internal override void EmitStmt(ILGenerator ilg)
        {
            if (Expr != null)
            {
                Expr.Emit(ilg, true);
                ilg.Emit(OpCodes.Throw);
            }
            else
            {
                ilg.Emit(OpCodes.Rethrow);
            }
        }
    }
    internal partial class BreakStmt : ThrowStmt
    {
        // Emit handled by parent
    }
    internal partial class QMarkStmt : Stmt
    {
        internal override void EmitStmt(ILGenerator ilg)
        {
            QOutCall.Emit(ilg, false);
        }
    }
    internal partial class QQMarkStmt : QMarkStmt
    {
    }
    internal partial class TryStmt : Stmt
    {
        Label? EndLabel => (TargetEntity as Script)?.End;
        Label? SaveEndLabel()
        {
            if (TargetEntity is Script s)
            {
                var e = s.End;
                s.End = null;
                return e;
            }
            return null;
        }
        void RestoreEndLabel(Label? e)
        {
            if (TargetEntity is Script s)
            {
                s.End = e;
            }
        }
        LocalBuilder retVal = null;
        void WrapEmit(Node n, ILGenerator ilg)
        {
            var oe = SaveEndLabel();
            n.Emit(ilg);
            if (EndLabel is Label ex)
            {
                if (retVal == null)
                    retVal = ilg.DeclareLocal(Compilation.Get(NativeType.Boolean).Type);
                var te = ilg.DefineLabel();
                ilg.Emit(OpCodes.Br_S, te);
                ilg.MarkLabel(ex);
                ilg.Emit(OpCodes.Ldc_I4_1);
                ilg.Emit(OpCodes.Stloc, retVal.LocalIndex);
                ilg.MarkLabel(te);
            }
            RestoreEndLabel(oe);
        }
        void UnwrapReturn(ILGenerator ilg)
        {
            if (retVal != null && TargetEntity is Script s)
            {
                ilg.Emit(OpCodes.Ldloc, retVal.LocalIndex);
                if (s.End == null)
                    s.End = ilg.DefineLabel();
                ilg.Emit(OpCodes.Brtrue, s.End.Value);
            }
        }
        internal override void EmitStmt(ILGenerator ilg)
        {
            ilg.BeginExceptionBlock();
            WrapEmit(Stmt, ilg);
            if (Catches.Length == 0 && Finally == null)
                ilg.BeginCatchBlock(Compilation.Get(NativeType.Object).Type);
            foreach (var c in Catches)
            {
                WrapEmit(c, ilg);
            }
            if (Finally != null)
                Finally.Emit(ilg);
            ilg.EndExceptionBlock();
            UnwrapReturn(ilg);
        }
    }
    internal partial class CatchBlock : Node
    {
        internal override void Emit(ILGenerator ilg)
        {
            ExVar.Declare(ilg);
            ilg.BeginCatchBlock(ExVar.Type.Type);
            ilg.Emit(OpCodes.Isinst, ExVar.Type.Type);
            ilg.Emit(OpCodes.Stloc, ExVar.Index);
            Stmt.Emit(ilg);
        }
    }
    internal partial class FinallyBlock : Node
    {
        internal override void Emit(ILGenerator ilg)
        {
            ilg.BeginFinallyBlock();
            Stmt.Emit(ilg);
        }
    }
    internal partial class SequenceStmt : Stmt
    {
        // Handled by TryStmt
    }

    internal partial class ScopeStmt : Stmt
    {
        internal override void EmitStmt(ILGenerator ilg)
        {
            Stmt.Emit(ilg);
        }
    }

    internal partial class LockStmt : Stmt
    {
        // Emitted as TRY statement
    }

    internal partial class UsingStmt : Stmt
    {
        // Emitted as TRY statement
    }
    internal partial class FixedStmt : Stmt
    {
    }
    internal partial class Script : Node
    {
        internal Label? End = null;
        internal override void Emit(ILGenerator ilg)
        {
            var result = ilg.DeclareLocal(((TypeSymbol)Symbol).Type);
            Symbol = new LocalSymbol((TypeSymbol)Symbol, result.LocalIndex);

            if (PCount != null)
            {
                PCount.Declare(ilg);
                ParamArray.EmitGet(ilg);
                ilg.Emit(OpCodes.Ldlen);
                ilg.Emit(OpCodes.Conv_I4);
                PCount.EmitSet(ilg);
            }

            Body.Emit(ilg);
            if (End != null)
                ilg.MarkLabel(End.Value);
            Symbol.EmitGet(ilg);
            ilg.Emit(OpCodes.Ret);
        }
    }
}
