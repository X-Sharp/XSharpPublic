//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using XSharpModel;

namespace XSharp.ProjectSystem.LanguageService
{
    /// <summary>
    /// Converts the Xsc command line arguments of the design-time build (rule XSharpCompilerCommandLineArgs)
    /// into the inputs of the code model: the option list for <see cref="XParseOptions.FromVsValues"/>,
    /// the assembly references for <see cref="XProject.RefreshReferences"/> and the source files.
    /// </summary>
    /// <remarks>
    /// XParseOptions.FromVsValues expects the switches without the leading '/' and the defines as "d:",
    /// the same format that XSharpProjectOptions.BuildCommandLine (MPFproj) produces.
    /// Switches that are not X# parse options are ignored by FromVsValues.
    /// </remarks>
    internal sealed class XSharpCommandLine
    {
        public IList<string> ParseOptions { get; }
        public IList<string> References { get; }
        public IList<string> SourceFiles { get; }

        private XSharpCommandLine(IList<string> parseOptions, IList<string> references, IList<string> sourceFiles)
        {
            ParseOptions = parseOptions;
            References = references;
            SourceFiles = sourceFiles;
        }

        public static XSharpCommandLine Parse(IEnumerable<string> arguments, string projectFolder)
        {
            var options = new List<string>();
            var references = new List<string>();
            var sources = new List<string>();
            var includes = new List<string>();
            foreach (var argument in arguments)
            {
                if (string.IsNullOrWhiteSpace(argument))
                    continue;
                var arg = argument.Trim();
                if (arg[0] != '/' && arg[0] != '-')
                {
                    sources.Add(MakeFullPath(Unquote(arg), projectFolder));
                    continue;
                }
                arg = arg.Substring(1);
                string name, value;
                var pos = arg.IndexOf(':');
                if (pos > 0)
                {
                    name = arg.Substring(0, pos);
                    value = Unquote(arg.Substring(pos + 1));
                }
                else
                {
                    name = arg;
                    value = "";
                }
                switch (name.ToLowerInvariant())
                {
                    case "reference":
                    case "r":
                        var reference = StripAlias(value);
                        if (reference.Length > 0)
                            references.Add(MakeFullPath(reference, projectFolder));
                        break;
                    case "define":
                    case "d":
                        if (value.Length > 0)
                            options.Add("d:" + value);
                        break;
                    case "i":
                        if (value.Length > 0)
                            includes.Add(value);
                        break;
                    case "analyzer":
                    case "analyzerconfig":
                    case "additionalfile":
                        // paths for the Roslyn analyzers, not relevant for the code model
                        break;
                    default:
                        options.Add(arg);
                        break;
                }
            }
            // Like MPFproj: the default include folder is always part of the include path
            includes.Add(XParseOptions.DefaultIncludeDir);
            options.Add("i:" + string.Join(";", includes.Where(i => !string.IsNullOrEmpty(i))));
            return new XSharpCommandLine(options, references, sources);
        }

        private static string Unquote(string value)
        {
            value = value.Trim();
            if (value.Length >= 2 && value[0] == '"' && value[value.Length - 1] == '"')
                value = value.Substring(1, value.Length - 2);
            return value;
        }

        /// <summary>
        /// /reference:alias=path.dll -> path.dll
        /// </summary>
        private static string StripAlias(string value)
        {
            var pos = value.IndexOf('=');
            if (pos > 0 && value.IndexOfAny(new[] { '\\', '/', ':' }) > pos)
                value = value.Substring(pos + 1);
            return Unquote(value);
        }

        internal static string MakeFullPath(string path, string projectFolder)
        {
            try
            {
                if (!Path.IsPathRooted(path))
                    path = Path.Combine(projectFolder, path);
                return Path.GetFullPath(path);
            }
            catch (Exception)
            {
                return path;
            }
        }
    }
}
