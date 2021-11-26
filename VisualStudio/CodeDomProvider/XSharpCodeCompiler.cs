//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace XSharp.CodeDom
{
    public partial class XSharpCodeGenerator
    {
        protected override string FileExtension
        {
            get
            {
                return ".prg";
            }
        }

        protected override string CompilerName
        {
            get
            {
                return "xsc.exe";
            }
        }

        protected override void ProcessCompilerOutputLine(CompilerResults results, string line)
        {
            Regex outputReg;
            outputReg = new Regex(@"(^([^(]+)(\(([0-9]+),([0-9]+)\))?: )?(error|warning) ([A-Z]+[0-9]+) ?: (.*)");
            Match match = outputReg.Match(line);
            if (match.Success)
            {
                CompilerError error = new CompilerError();
                if (match.Groups[3].Success)
                {
                    error.FileName = match.Groups[2].Value;
                    error.Line = int.Parse(match.Groups[4].Value, CultureInfo.InvariantCulture);
                    error.Column = int.Parse(match.Groups[5].Value, CultureInfo.InvariantCulture);
                }
                if (string.Compare(match.Groups[6].Value, "warning", true, CultureInfo.InvariantCulture) == 0)
                {
                    error.IsWarning = true;
                }
                error.ErrorNumber = match.Groups[7].Value;
                error.ErrorText = match.Groups[8].Value;
                results.Errors.Add(error);
            }
        }

        protected override string CmdArgsFromParameters(CompilerParameters options)
        {
            StringBuilder builder = new StringBuilder(0x80);

            if (options.GenerateExecutable)
            {
                builder.Append("/t:exe ");
            }
            else
            {
                builder.Append("/t:library ");
            }

            foreach (string str in options.ReferencedAssemblies)
            {
                builder.Append("/r:");
                builder.Append("\"");
                builder.Append(str);
                builder.Append("\" ");
            }

            builder.Append("/out:");
            builder.Append("\"");
            builder.Append(options.OutputAssembly);
            builder.Append("\" ");

            if (options.IncludeDebugInformation)
            {
                builder.Append("/d ");
            }

            if (options.Win32Resource != null)
            {
                builder.Append("/win32res:\"" + options.Win32Resource + "\" ");
            }

            foreach (string str2 in options.EmbeddedResources)
            {
                builder.Append("/res:\"");
                builder.Append(str2);
                builder.Append("\" ");
            }

            foreach (string str3 in options.LinkedResources)
            {
                builder.Append("/linkres:\"");
                builder.Append(str3);
                builder.Append("\" ");
            }

            if (options.TreatWarningsAsErrors)
            {
                builder.Append("/wx ");
            }

            if (options.CompilerOptions != null)
            {
                builder.Append(options.CompilerOptions + " ");
            }

            return builder.ToString();
        }
    }
}
