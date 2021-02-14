using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection.Emit;

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
                if (FinallyClauses.Count > 0)
                {
                    ilg.BeginFinallyBlock();
                    foreach (var a in FinallyClauses) a();
                }
                ilg.EndExceptionBlock();
            }
        }
    }
    internal partial class ReturnStmt : Stmt
    {
        internal override void EmitStmt(ILGenerator ilg)
        {
            bool isVoid = true;
            if (Expr != null)
            {
                isVoid &= Expr.Datatype.NativeType == NativeType.Void;
                Expr.Emit(ilg, true);
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
    internal partial class DeclStmt : Stmt
    {
    }
    internal partial class VarDecl : Node
    {
    }
    internal partial class ImpliedVarDecl : VarDecl
    {
    }
    internal partial class FieldDeclStmt : Stmt
    {
    }
    internal partial class EmptyStmt : Stmt
    {
    }
    internal partial class WhileStmt : Stmt
    {
    }
    internal partial class RepeatStmt : WhileStmt
    {
    }
    internal partial class ForStmt : Stmt
    {
    }
    internal partial class ForeachStmt : Stmt
    {
    }
    internal partial class IfStmt : Stmt
    {
    }
    internal partial class DoCaseStmt : Stmt
    {
    }
    internal partial class CaseBlock : Node
    {
    }

    internal partial class SwitchStmt : Stmt
    {
    }
    internal partial class SwitchBlock : Node
    {
    }
    internal partial class SwitchBlockExpr : SwitchBlock
    {
    }
    internal partial class SwitchBlockType : SwitchBlock
    {
    }
    internal partial class ExitStmt : Stmt
    {
    }
    internal partial class LoopStmt : Stmt
    {
    }
    internal partial class BreakStmt : Stmt
    {
    }
    internal partial class ThrowStmt : Stmt
    {
    }
    internal partial class QMarkStmt : Stmt
    {
    }
    internal partial class QQMarkStmt : QMarkStmt
    {
    }
    internal partial class TryStmt : Stmt
    {
    }
    internal partial class CatchBlock : Node
    {
    }
    internal partial class FinallyBlock : Node
    {
    }
    internal partial class SequenceStmt : Stmt
    {
    }

    internal partial class ScopeStmt : Stmt
    {
    }

    internal partial class LockStmt : Stmt
    {
    }

    internal partial class UsingStmt : Stmt
    {
    }
    internal partial class FixedStmt : Stmt
    {
    }
    internal partial class Script : Node
    {
        internal override void Emit(ILGenerator ilg)
        {
            var temp = ilg.DeclareLocal(((TypeSymbol)Symbol).Type);
            Symbol = new LocalSymbol((TypeSymbol)Symbol, temp.LocalIndex);
            Body.Emit(ilg);
            if (Symbol != null) Symbol.EmitGet(ilg);
            ilg.Emit(OpCodes.Ret);
        }
    }
}
