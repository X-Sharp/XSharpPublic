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
        internal VisualBasicCompiler(CSharp.CSharpCommandLineParser parser, string? responseFile, string[] args, BuildPaths buildPaths, string? additionalReferenceDirectories, IAnalyzerAssemblyLoader assemblyLoader, GeneratorDriverCache? driverCache = null, ICommonCompilerFileSystem? fileSystem = null)
            : base(parser, responseFile, args, buildPaths, additionalReferenceDirectories, assemblyLoader, driverCache, fileSystem)
        {
        }

    }
}
