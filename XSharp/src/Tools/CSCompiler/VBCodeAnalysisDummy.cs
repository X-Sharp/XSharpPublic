using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Threading;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.VisualBasic
{
    internal class VisualBasicCommandLineParser: CSharp.CSharpCommandLineParser
    {

    }
    internal abstract class VisualBasicCompiler : CSharp.CSharpCompiler
    {
        internal VisualBasicCompiler(
            CSharp.CSharpCommandLineParser clp,
            string rspFile,
            string[] args,
            BuildPaths buildPaths,
            string libDirectory,
            IAnalyzerAssemblyLoader analyzerLoader)
            : base(clp, rspFile, args, buildPaths, "", analyzerLoader)
        {
        }
    }
}