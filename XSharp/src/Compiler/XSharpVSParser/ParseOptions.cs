using System.Collections.Generic;
using System;
using LanguageService.CodeAnalysis.XSharp;
using Microsoft.CodeAnalysis;

namespace XSharp.Parser
{
    public static class VSParseOptions 
    {
        public static XSharpParseOptions FromVsValues(IList<string> options)
        {
            var parser = new XSharpCommandLineParser();
            var diags = new List<Diagnostic>();
            parser.ResetXSharpCommandlineOptions();
            var defines = new List<string>();
            foreach (var opt in options)
            {
                string name, value;
                var pos = opt.IndexOf(":");
                if (pos > 0)
                {

                    name = opt.Substring(0, pos);
                    value = opt.Substring(pos + 1);
                }
                else
                {
                    name = opt;
                    value = "";
                }
                if (name == "d")
                {
                    var defs = value.Split(new char[] { ';' }, StringSplitOptions.RemoveEmptyEntries);
                    defines.AddRange(defs);
                }
                else
                {
                    parser.ParseXSharpArgument(ref name, ref value, "", diags);
                }
            }
            var xopts = parser.XSharpSpecificCompilationOptions;
            var result = new XSharpParseOptions().WithXSharpSpecificOptions(xopts);
            result.PreprocessorSymbols.AddRange(defines);
            return result;
        }

    }
}
