using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.MacroCompiler
{
    internal partial class Binder
    {
        internal void Bind<T>(ref T node) where T: Syntax.Node
        {
            if (node.Symbol == null)
            {
                var b = node?.Bind(this);
                if (b != null) node = (T)b;
            }
        }

        internal void Bind<T>(ref T node, BindAffinity affinity) where T : Syntax.Expr
        {
            if (node != null)
            {
                node.Affinity = affinity;
                Bind(ref node);
            }
        }

        internal void Bind<T>(IList<T> nodes) where T: Syntax.Node
        {
            if (nodes != null)
            {
                for (var i = 0; i < nodes.Count; i++)
                {
                    if (nodes[i]?.Symbol == null)
                    {
                        var b = nodes[i]?.Bind(this);
                        if (b != null) nodes[i] = (T)b;
                    }
                }
            }
        }

        internal void BindStmt<T>(ref T node) where T : Syntax.Stmt
        {
            StmtStack.Push(node);
            var b = node?.Bind(this);
            if (b != null) node = (T)b;
            StmtStack.Pop();
        }

        internal Syntax.Expr Cache(ref Syntax.Expr expr)
        {
            return expr = Syntax.CachedExpr.Bound(this, expr);
        }
    }
}
