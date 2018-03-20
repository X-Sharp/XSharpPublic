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
        static bool TypesMatch(TypeSymbol t1, TypeSymbol t2)
        {
            return t1 == t2;
        }
    }
}