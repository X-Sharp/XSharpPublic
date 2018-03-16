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
        internal static R Emit<T,R>(this Binder<T,R> b, Codeblock macro) where R: class
        {
            var dm = new DynamicMethod("<macro>", typeof(T), new Type[] { typeof(T[]) });
            macro.Emit(dm.GetILGenerator());
            return dm.CreateDelegate(typeof(R)) as R;
        }
    }
}
