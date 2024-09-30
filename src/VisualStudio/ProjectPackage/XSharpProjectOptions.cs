//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.Project;
using Microsoft.Win32;
using System.Reflection;
using LanguageService.CodeAnalysis.XSharp;
using XSharp.Parser;
using XSharpModel;

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
        public XParseOptions ParseOptions { get; private set;}
        public XSharpProjectOptions(XSharpProjectNode prjNode) : base()
        {
            _prjNode = prjNode;

        }
        
        public void BuildCommandLine()
        {
            //List<String> args = new List<String>();
            //try
            //{
            Logger.Debug("ProjectOptions: BuildCommandLine");
            List<string> options = new List<string>();
            options.Add("dialect:" + _prjNode.GetProjectProperty(XSharpProjectFileConstants.Dialect));
            var asmNodes = new List<XSharpAssemblyReferenceNode>();
            _prjNode.FindNodesOfType(asmNodes);
            foreach (var asmNode in asmNodes)
            {
                options.Add("r:" + asmNode.Url);
            }
            var prjNodes = new List<XSharpProjectReferenceNode>();
            _prjNode.FindNodesOfType(prjNodes);
            foreach (var prjNode in prjNodes)
            {
                var path = prjNode.ReferencedProjectOutputPath;
                if (!string.IsNullOrEmpty(path))
                {
                    options.Add("r:" + path);
                }
            }
            var comNodes = new List<XSharpComReferenceNode>();
            _prjNode.FindNodesOfType(comNodes);
            foreach (var comNode in comNodes)
            {
                options.Add("r:" + comNode.Url);
            }

            var defines = "";
            if (DefinedPreprocessorSymbols != null)
            {
                foreach (var d in DefinedPreprocessorSymbols)
                {
                    defines = defines + d + ";";
                }
            }
            options.Add("d:" + defines);
            var include = _prjNode.GetProjectProperty(XSharpProjectFileConstants.IncludePaths);
            if (!string.IsNullOrEmpty(include))
            {
                include = include + ";" + XParseOptions.DefaultIncludeDir;
            }
            else
            {
                include = XParseOptions.DefaultIncludeDir;
            }
            options.Add("i:" + include);
            var ns = _prjNode.GetLogicProjectProperty(XSharpProjectFileConstants.NS);
            if (ns)
            {
                options.Add("ns:" + _prjNode.GetProjectProperty(XSharpProjectFileConstants.RootNamespace));
            }
            var flags = new string[] {XSharpProjectFileConstants.Vo1,
                XSharpProjectFileConstants.Vo2,
                XSharpProjectFileConstants.Vo3,
                XSharpProjectFileConstants.Vo4,
                XSharpProjectFileConstants.Vo5,
                XSharpProjectFileConstants.Vo6,
                XSharpProjectFileConstants.Vo7,
                XSharpProjectFileConstants.Vo8,
                XSharpProjectFileConstants.Vo9,
                XSharpProjectFileConstants.Vo10,
                XSharpProjectFileConstants.Vo11,
                XSharpProjectFileConstants.Vo12,
                XSharpProjectFileConstants.Vo13,
                XSharpProjectFileConstants.Vo14,
                XSharpProjectFileConstants.Vo15,
                XSharpProjectFileConstants.Vo16,
                XSharpProjectFileConstants.CS,
                XSharpProjectFileConstants.AZ,
                XSharpProjectFileConstants.INS,
                XSharpProjectFileConstants.LB,
                XSharpProjectFileConstants.MemVar,
                XSharpProjectFileConstants.NamedArgs,
                XSharpProjectFileConstants.Undeclared,
                XSharpProjectFileConstants.Unsafe,
                XSharpProjectFileConstants.Xpp1,
                XSharpProjectFileConstants.Xpp2,
                XSharpProjectFileConstants.Fox1,
                XSharpProjectFileConstants.Allowdot,
                XSharpProjectFileConstants.EnforceSelf,
                XSharpProjectFileConstants.EnforceOverride,
                XSharpProjectFileConstants.ModernSyntax,
                };
            foreach (var flag in flags)
            {
                if (_prjNode.HasProjectProperty(flag))
                {
                    if (_prjNode.GetLogicProjectProperty(flag))
                        options.Add(flag.ToLower() + "+");
                    else
                        options.Add(flag.ToLower() + "-");
                }
            }
            string value = _prjNode.GetProjectProperty(XSharpProjectFileConstants.StandardDefs);
            if (value != null && value.Trim().Length > 0)
            {
                options.Add("stddefs:" + value);
            }
            if (_prjNode.GetLogicProjectProperty(XSharpProjectFileConstants.NoStandardDefs))
            {
                options.Add("nostddefs+");
            }
            else
            {
                options.Add("nostddefs-");
            }
            ParseOptions = XParseOptions.FromVsValues(options);
            if (this.ConfigCanonicalName != null && ConfigCanonicalName.ConfigName.ToUpper() == "DEBUG")
            {
                // dirty trick to set property with private setter
                PropertyInfo pi = ParseOptions.GetType().GetProperty("DebugEnabled");
                if (pi != null)
                {
                    pi.SetValue(ParseOptions, true);
                }

            }
            _prjNode.ProjectModel.ResetParseOptions(ParseOptions);
        }

    }
}
