//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.Project;
using System.Diagnostics;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell;

using System.IO;
using MSBuild = Microsoft.Build.Evaluation;
using System.Runtime.InteropServices;
using Microsoft.Win32;
using LanguageService.CodeAnalysis.XSharp;

namespace XSharp.Project
{
    /// <summary>
    /// This class adds X# specific project options and builds a command line for use in the intellisense
    /// </summary>
    /// 
    public class XSharpProjectOptions : ProjectOptions
    {

        static XSharpCommandLineParser xsCmdLineparser;

        private XSharpProjectNode _prjNode;
        internal ConfigCanonicalName ConfigCanonicalName { get; set; }
        public XSharpParseOptions ParseOptions { get; private set;}
        public XSharpParseOptions LexOptions { get; private set; }
        public XSharpProjectOptions(XSharpProjectNode prjNode) : base()
        {
            _prjNode = prjNode;

        }
        static string _includedirs;
        internal static string REG_KEY = @"HKEY_LOCAL_MACHINE\" + XSharp.Constants.RegistryKey;
        static XSharpProjectOptions()
        {
            xsCmdLineparser = XSharpCommandLineParser.Default;
            _includedirs = "";
            var path = (string)Registry.GetValue(REG_KEY, XSharp.Constants.RegistryValue, "");
            if (!string.IsNullOrEmpty(path))
            {
                if (!path.EndsWith("\\"))
                    path += @"\";
                path += @"Include\";
                _includedirs += path;
            }
            // Check for Vulcan path
            var key = @"HKEY_LOCAL_MACHINE\SOFTWARE\Grafx\Vulcan.NET";
            path = (string)Registry.GetValue(key, "InstallPath", "");
            if (!string.IsNullOrEmpty(path))
            {
                if (!path.EndsWith("\\"))
                    path += @"\";
                path += @"Include\";
                _includedirs += ";" + path;
            }
            XSharpSpecificCompilationOptions.SetDefaultIncludeDir(_includedirs);
            XSharpSpecificCompilationOptions.SetWinDir(Environment.GetFolderPath(Environment.SpecialFolder.Windows));
            XSharpSpecificCompilationOptions.SetSysDir(Environment.GetFolderPath(Environment.SpecialFolder.System));
        }

        public void BuildCommandLine()
        {
            List<String> args = new List<String>();
            try
            {
                args.Add("/dialect:" + _prjNode.GetProjectProperty("Dialect"));
                // Add pseudo references so the Vulcan/VO dialect will be allowed
                args.Add("/errorendlocation");
                args.Add("/r:vulcanrt.dll");
                args.Add("/r:vulcanrtfuncs.dll");
                if (String.Equals(ConfigCanonicalName.ConfigName, "DEBUG", StringComparison.OrdinalIgnoreCase))
                {
                    args.Add("/debug:full");
                }
                var tmp = "";

                foreach (var d in DefinedPreprocessorSymbols)
                {
                    tmp += ";" + d;
                }
                if (tmp.Length > 0)
                {
                    args.Add("/d:" + tmp.Substring(1));
                }
                tmp = _prjNode.GetProjectProperty("DisabledWarnings");
                if (tmp?.Length > 0)
                {
                    tmp = tmp.Replace(",", ";");
                    args.Add("/warningaserror-:" + tmp);
                }
                args.Add("/warn:" + WarningLevel.ToString());
                for (int i = 1; i < 16; i++)
                {
                    var sw = "vo" + i.ToString();
                    tmp = _prjNode.GetProjectProperty(sw);
                    if (!String.IsNullOrEmpty(tmp))
                    {
                        args.Add("/"+sw+  (tmp.ToLower() == "true" ? "+" : "-"));
                    }
                }
                var include = _prjNode.GetProjectProperty("IncludePaths");
                if (!String.IsNullOrEmpty(include))
                {
                    include = include + ";" + _includedirs;
                }
                else
                    include = _includedirs;
                args.Add("/i:" + include);
                tmp = _prjNode.GetProjectProperty("NoStandardDefs");
                if (!String.IsNullOrEmpty(tmp) && tmp.ToLower() == "true")
                    args.Add("/nostddefs");

                tmp = _prjNode.GetProjectProperty("INS");
                if (!String.IsNullOrEmpty(tmp) && tmp.ToLower() == "true")
                    args.Add("/ins");

                if (this.TreatWarningsAsErrors)
                    args.Add("/warnaserror");
            }
            finally
            {
                if (args.Count > 0)
                {
                    var cmdlineargs = xsCmdLineparser.Parse(args.ToArray(), null, null, null);
                    ParseOptions = cmdlineargs.ParseOptions;
                    cmdlineargs = xsCmdLineparser.Parse(args.ToArray(), null, null, null);
                    LexOptions = cmdlineargs.ParseOptions;
                }
                else
                {
                    var cmdlineargs = xsCmdLineparser.Parse(new string[0], null, null, null);
                    ParseOptions = cmdlineargs.ParseOptions;
                    cmdlineargs = xsCmdLineparser.Parse(new string[0], null, null, null);
                    LexOptions = cmdlineargs.ParseOptions;

                }
                ParseOptions.ParseLevel = ParseLevel.Parse;
                LexOptions.ParseLevel = ParseLevel.Lex;
            }
        }

    }
}
