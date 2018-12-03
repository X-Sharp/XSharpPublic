using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.MacroCompiler
{
    public class MacroOptions
    {
        public static readonly MacroOptions Default = new MacroOptions();

        public bool AllowFourLetterAbbreviations = true;
        public bool AllowOldStyleComments = true;
        public bool AllowSingleQuotedStrings = true;
        public bool AllowPackedDotOperators = true;

        public bool VOFloatConstants = true;
        public bool VODateConstants = true;

        public bool ArrayZero = false;
    }
}