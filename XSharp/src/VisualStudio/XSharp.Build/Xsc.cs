using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Microsoft.Build.Framework;
using Microsoft.Build.Tasks;
using Microsoft.Build.Utilities;
using Microsoft.Win32;
using System.IO;
using System.Diagnostics;

namespace XSharp.Build
{
    public class Xsc : ManagedCompiler
    {
        // These are settings
        internal string REG_KEY = @"HKEY_LOCAL_MACHINE\" + Constants.RegistryKey;

        #region The values are set through .targets
        // The  fullpath to Compiler

        // Todo: store the values in base.bag

        public Boolean AZ { get; set; }
        public Boolean CS { get; set; }
        public Boolean LB { get; set; }
        public Boolean UnSafe { get; set; }
        public Boolean OVF { get; set; }
        public String DisabledWarnings { get; set; }
        public string DocumentationFile { get; set; }
        public Boolean GenerateFullPaths { get; set; }
        public Boolean PPO { get; set; }
        public Boolean NS { get; set; }
        public Boolean INS { get; set; }
        public String IncludePaths { get; set; }
        public Boolean NoStandardDefs { get; set; }
        public Boolean NoStandardLib { get; set; }
        public string RootNameSpace{ get; set; }
        public int WarningLevel { get; set; }
        public Boolean VO1 { get; set; }
        public Boolean VO2 { get; set; }
        public Boolean VO3 { get; set; }
        public Boolean VO4 { get; set; }
        public Boolean VO5 { get; set; }
        public Boolean VO6 { get; set; }
        public Boolean VO7 { get; set; }
        public Boolean VO8 { get; set; }
        public Boolean VO9 { get; set; }
        public Boolean VO10 { get; set; }
        public Boolean VO11{ get; set; }
        public Boolean VO12 { get; set; }
        public Boolean VO13 { get; set; }

        public string CompilerPath { get; set; }
        // Misc. (unknown at that time) CommandLine options
        public string CommandLineOption { get; set; }

       
        #endregion

        protected override string ToolName
        {
            get
            {
                return "xsc.exe";
            }
        }

        protected override string GenerateFullPathToTool()
        {
            return FindXsc(this.ToolName);
        }


        protected override void AddResponseFileCommands(CommandLineBuilderExtension commandLine)
        {
            try
            {
                AddResponseFileCommandsImpl(commandLine);
            }
            catch (Exception ex)
            {
                Trace.Assert(false, ex.ToString());
                throw;
            }
        }

        //protected override string GenerateCommandLineCommands()
        //{
        //    return "/shared";
        //}


        protected override int ExecuteTool(string pathToTool, string responseFileCommands, string commandLineCommands)
        {
            int iResult;
            DateTime start = DateTime.Now;
            iResult = base.ExecuteTool(pathToTool, responseFileCommands, commandLineCommands);
            var time = DateTime.Now - start;
            var timestring = time.ToString();
            Log.LogMessageFromText("XSharp Compilation time: "+timestring, MessageImportance.High);
            return iResult;
        }

        private string FindXsc(string toolName)
        {
            //System.Diagnostics.Debugger.Launch();
            if (string.IsNullOrEmpty(CompilerPath))
            {
                // If used after MSI Installer, value should be in the Registry
                string InstallPath = String.Empty;
                try
                {
                    InstallPath = (string)Registry.GetValue(REG_KEY, Constants.RegistryValue, "");
                    
                }
                catch (Exception) { }
                // Nothing in the Registry ?
                if (!string.IsNullOrEmpty(InstallPath))
                {
                    CompilerPath = AddSlash(InstallPath) + "Bin\\";
                }
                // Allow to override the path when developing.
                // Please note that this must be a complete path, for example "d:\Xsharp\Dev\XSharp\Binaries\Debug"
                string DevPath = System.Environment.GetEnvironmentVariable("XSHARPDEV");
                if (!string.IsNullOrEmpty(DevPath))
                {
                    CompilerPath = AddSlash(DevPath);
                }
                if (string.IsNullOrEmpty(CompilerPath))
                {
                    // get the path of the current DLL
                    CompilerPath = new Uri(typeof(Xsc).Assembly.CodeBase).LocalPath;
                }
            }
            // Search the compiler at the same place
            var xsc_file = Path.Combine(Path.GetDirectoryName(CompilerPath), toolName);
            if (File.Exists(xsc_file))
            {
                // The tool has been found.
                return xsc_file;
            }
            // Return the tool name itself.
            // Windows will search common paths for the tool.
            return toolName;
        }

        protected string AddSlash(string Path)
        {
            if (!String.IsNullOrEmpty(Path) && !Path.EndsWith("\\"))
                Path += "\\";
            return Path;

        }
        protected void AddResponseFileCommandsImpl(CommandLineBuilderExtension commandLine)
        {
            if (OutputAssembly == null && Sources != null && Sources.Length > 0 && ResponseFiles == null)
            {
                try
                {
                    OutputAssembly = new TaskItem(Path.GetFileNameWithoutExtension(Sources[0].ItemSpec));
                }
                catch (ArgumentException exception)
                {
                    throw new ArgumentException(exception.Message, "Sources", exception);
                }

                var outputAssembly = OutputAssembly;
                switch (TargetType.ToLowerInvariant())
                {
                    case "library":
                        outputAssembly.ItemSpec = outputAssembly.ItemSpec + ".dll";
                        break;

                    default:
                        outputAssembly.ItemSpec = outputAssembly.ItemSpec + ".exe";
                        break;
                }
            }

            // Add sources
            if (this.Sources != null)
            {
                commandLine.AppendFileNamesIfNotNull(this.Sources, "\n");
            }

            if (null != base.References)
            {
                foreach (var it in base.References)
                    commandLine.AppendSwitchIfNotNull("\n/reference:", it.ItemSpec);
            }
            // target and platform
            commandLine.AppendSwitch("\n/target:" + this.TargetType);
            commandLine.AppendSwitchIfNotNull("\n/platform:", this.Platform);
            // Debug ?

            commandLine.AppendSwitchIfNotNull("\n/debug", this.EmitDebugInformation ? "+" : "-");
            // Default Namespace
            if (NS)
            {
                commandLine.AppendSwitch("/ns:" + this.RootNameSpace);
            }
            else
            {
                commandLine.AppendSwitch("/ns:" );
            }
            if (WarningLevel < 0 || WarningLevel > 4)
            {
                WarningLevel = 4;
            }
            commandLine.AppendSwitch("/warn:" + WarningLevel.ToString());
            AppendSwitchIfTrue(commandLine, "/warnaserror", TreatWarningsAsErrors);
            if (!String.IsNullOrEmpty(DisabledWarnings))
            {
                string[] warnings = DisabledWarnings.Split(new char[] { ' ', ',', ';' });
                string warninglist = String.Empty;
                foreach (string s in warnings)
                {
                    if (warninglist.Length > 0)
                        warninglist += ";";
                    warninglist += s;
                }
                if (warninglist.Length > 0)
                    commandLine.AppendSwitch("/nowarn:" + warninglist);
            }
            // Compatibility
            AppendSwitchIfTrue(commandLine, "/az", AZ);
            AppendSwitchIfTrue(commandLine, "/cs", CS);
            AppendSwitchIfTrue(commandLine, "/ins", INS);
            AppendSwitchIfTrue(commandLine, "/lb", LB);
            AppendSwitchIfTrue(commandLine, "/ovf", OVF);
            AppendSwitchIfTrue(commandLine, "/ppo", PPO);
            AppendSwitchIfTrue(commandLine, "/vo1", VO1);
            AppendSwitchIfTrue(commandLine, "/vo2", VO2);
            AppendSwitchIfTrue(commandLine, "/vo3", VO3);
            AppendSwitchIfTrue(commandLine, "/vo4", VO4);
            AppendSwitchIfTrue(commandLine, "/vo5", VO5);
            AppendSwitchIfTrue(commandLine, "/vo6", VO6);
            AppendSwitchIfTrue(commandLine, "/vo7", VO7);
            AppendSwitchIfTrue(commandLine, "/vo8", VO8);
            AppendSwitchIfTrue(commandLine, "/vo9", VO9);
            AppendSwitchIfTrue(commandLine, "/vo10", VO10);
            AppendSwitchIfTrue(commandLine, "/vo11", VO11);
            AppendSwitchIfTrue(commandLine, "/vo12", VO12);
            AppendSwitchIfTrue(commandLine, "/vo13", VO13);

            // Output assembly name
            commandLine.AppendSwitchIfNotNull("\n/out:", OutputAssembly);
            // User-defined CommandLine Option (in order to support switches unknown at that time)
            // cannot use appendswitch because it will quote the string when there are embedded spaces
            if (!String.IsNullOrEmpty(this.CommandLineOption))
            {
                commandLine.AppendTextUnquoted("\n"+this.CommandLineOption);
            }

            //
        }

        protected void AppendSwitchIfTrue(CommandLineBuilderExtension commandLine, string Switch, Boolean Option)
        {
            
            if (Option)
            {
                commandLine.AppendSwitch(Switch+"+");
            }
            else
            {
                commandLine.AppendSwitch(Switch + "-");

            }
        }

        protected override void LogEventsFromTextOutput(string singleLine, MessageImportance messageImportance)
        {
            try
            {
                base.LogEventsFromTextOutput(singleLine, messageImportance);
            }
            catch (Exception e)
            {
                object[] messageArgs = new object[0];
                base.Log.LogMessage(MessageImportance.High, singleLine, messageArgs);
                base.Log.LogErrorFromException(e, true);
            }
        }
    }

}
