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

    internal class OverloadResult
    {
        internal int TotalCost = 0;
        internal int ExtraValid = 0;
        internal bool Valid = true;
        internal readonly MethodSymbol Method;
        internal OverloadResult Equivalent = null;

        internal bool Exact { get { return Valid && TotalCost == 0; } }

        internal bool Unique { get { return Valid && ExtraValid == 0; } }

        internal ConversionSymbol[] Conversions;

        internal OverloadResult(MethodSymbol method, int nargs) { Method = method; Conversions = new ConversionSymbol[nargs]; }

        internal void ArgConversion(int index, ConversionSymbol conv)
        {
            Conversions[index] = conv;
            TotalCost += conv.Cost;
            Valid &= conv.IsImplicit;
        }

        internal static OverloadResult Create(MethodSymbol method, int nargs) { return new OverloadResult(method, nargs); }

        internal OverloadResult Better(OverloadResult other)
        {
            if (other?.Valid == true)
            {
                if (Valid && other.TotalCost == TotalCost)
                {
                    Equivalent = other;
                    ExtraValid += other.ExtraValid + 1;
                }
                else if (!Valid || other.TotalCost < TotalCost)
                {
                    return  other;
                }
            }
            return this;
        }
    }
}