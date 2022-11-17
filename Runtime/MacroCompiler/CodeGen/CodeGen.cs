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
    using System.Runtime.CompilerServices;

    internal static partial class CodeGen
    {
        internal static R Emit<T,R>(this Binder<T,R> b, Node macro) where R: Delegate
        {
            macro.Emit(b.GetILGenerator());
            return b.GenerateDelegate() as R;
        }

        internal static Delegate Emit(this Binder b, Node macro)
        {
            macro.Emit(b.GetILGenerator());
            return b.GenerateDelegate();
        }
        internal static byte[] EmitAssembly(this Binder b, Node macro)
        {
            macro.Emit(b.GetILGenerator());
            return b.GetAssemblyBytes();
        }
        internal static void DeclareAutoLocals(this Binder b)
        {
            if (b.Locals != null)
                foreach (var l in b.Locals)
                    if (l.IsAuto) l.Declare(b.GetILGenerator());
        }
    }
}
