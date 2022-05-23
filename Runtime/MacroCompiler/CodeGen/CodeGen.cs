using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;
using System.Reflection.Emit;

namespace XSharp.MacroCompiler
{
    using Syntax;

    internal static partial class CodeGen
    {
        internal static R Emit<T,R>(this Binder<T,R> b, Node macro) where R: Delegate
        {
            macro.Emit(b.Method.GetILGenerator());
            return b.MakeMethodDelegate() as R;
        }

        internal static Delegate Emit(this Binder b, Node macro)
        {
            macro.Emit(b.Method.GetILGenerator());
            return b.MakeMethodDelegate();
        }
    }
}
