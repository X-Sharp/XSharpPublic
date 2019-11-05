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
using System.Reflection;

namespace XSharp.Project
{
    /// <summary>
    /// This class adds X# specific project options and builds a command line for use in the intellisense
    /// </summary>
    ///
    public class XSharpProjectOptions : ProjectOptions
    {

        //static XSharpCommandLineParser xsCmdLineparser;

        private XSharpProjectNode _prjNode;
        internal ConfigCanonicalName ConfigCanonicalName { get; set; }
        public XSharpParseOptions ParseOptions { get; private set;}
        public XSharpProjectOptions(XSharpProjectNode prjNode) : base()
        {
            _prjNode = prjNode;

        }
        static string _includedirs;
        internal static string REG_KEY = @"HKEY_LOCAL_MACHINE\" + XSharp.Constants.RegistryKey;
        static XSharpProjectOptions()
        {
            //xsCmdLineparser = XSharpCommandLineParser.Default;
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
            //List<String> args = new List<String>();
            //try
            //{
            List<string> options = new List<string>();
            options.Add("dialect:" + _prjNode.GetProjectProperty("Dialect"));
            var asmNodes = new List<XSharpAssemblyReferenceNode>();
            _prjNode.FindNodesOfType<XSharpAssemblyReferenceNode>(asmNodes);
            foreach (var asmNode in asmNodes)
            {
                options.Add("r:" + asmNode.Url);
            }
            var prjNodes = new List<XSharpProjectReferenceNode>();
            _prjNode.FindNodesOfType<XSharpProjectReferenceNode>(prjNodes);
            foreach (var prjNode in prjNodes)
            {
                options.Add("r:" + prjNode.ReferencedProjectOutputPath);
            }
            var comNodes = new List<XSharpComReferenceNode>();
            _prjNode.FindNodesOfType<XSharpComReferenceNode>(comNodes);
            foreach (var comNode in comNodes)
            {
                options.Add("r:" + comNode.Url);
            }


            var defines = "";
            var value = "";
            foreach (var d in DefinedPreprocessorSymbols)
            {
                defines = defines + d + ";";
            }
            options.Add("d:" + defines);
            var include = _prjNode.GetProjectProperty("IncludePaths");
            if (!String.IsNullOrEmpty(include))
            {
                include = include + ";" + _includedirs;
            }
            else
            {
                include = _includedirs;
            }
            options.Add("i:" + include);

            var flags = new string[] {"vo1", "vo2" , "vo3" , "vo4" , "vo5" , "vo6" , "vo7" , "vo8" , "vo9" , "vo10" , "vo11" , "vo12", "vo13", "vo14", "vo15","vo16",
                "az","ins", "lb","memvar","namedargs","undeclared","unsafe","xpp1","xpp2","fox1"};
            foreach (var flag in flags)
            {
                value = _prjNode.GetProjectProperty(flag);
                if (value != null && value.ToLower() == "true")
                    options.Add(flag + "+");
                else
                    options.Add(flag + "-");
            }
            value = _prjNode.GetProjectProperty("StandardDefs");
            if (value != null && value.Trim().Length > 0)
            {
                options.Add("stddefs:" + value);
            }
            value = _prjNode.GetProjectProperty("NoStandardDefs");
            if (value != null  && value.ToLower() == "true")
            {
                options.Add("nostddefs+");
            }
            else
            {
                options.Add("nostddefs-");
            }

            ParseOptions = XSharpParseOptions.FromVsValues(options);
            if (this.ConfigCanonicalName != null && ConfigCanonicalName.ConfigName.ToUpper() == "DEBUG")
            {
                // dirty trick to set property with private setter
                PropertyInfo pi = ParseOptions.GetType().GetProperty("DebugEnabled");
                if (pi != null)
                {
                    pi.SetValue(ParseOptions, true);
                }

            }
        }

    }
}
