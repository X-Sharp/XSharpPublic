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
        internal static R Emit<T,R>(this Binder<T,R> b, Codeblock macro, string source) where R: class
        {
            var dm = b.CreateMethod(source);
            macro.Emit(dm.GetILGenerator());
            return b.CreateDelegate(dm) as R;
        }

        internal static Delegate Emit(this Binder b, Codeblock macro, string source)
        {
            var dm = b.CreateMethod(source);
            macro.Emit(dm.GetILGenerator());
            return b.CreateDelegate(dm);
        }
    }
}
