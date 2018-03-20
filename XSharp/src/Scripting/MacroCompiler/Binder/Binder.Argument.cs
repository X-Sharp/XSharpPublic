using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Reflection;

namespace XSharp.MacroCompiler
{
    using Syntax;

    internal partial class Binder
    {
        internal static ConversionSymbol ArgumentConversion(Arg arg, ParameterInfo param)
        {
            var conv = Conversion(arg.Expr, FindType(param.ParameterType));
            return conv;
        }
    }
}