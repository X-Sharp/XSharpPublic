//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Text;
using Microsoft.Build.Tasks;

using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;
using Microsoft.Win32;
using System.IO;
using System.Diagnostics;
namespace XSharp.Build
{


    public class Xsc : ManagedCompiler
    {
        // These are settings

        #region VO Compatible properties

        public Boolean AZ
        {
            set { base.Bag[nameof(AZ)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(AZ), false); }
        }
        public Boolean CS
        {
            set { base.Bag[nameof(CS)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(CS), false); }
        }
        public Boolean LB
        {
            set { base.Bag[nameof(LB)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(LB), false); }
        }
        public Boolean OVF
        {
            set { base.Bag[nameof(OVF)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(OVF), false); }
        }
        public Boolean PPO
        {
            set { base.Bag[nameof(PPO)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(PPO), false); }
        }
        public Boolean NS
        {
            set { base.Bag[nameof(NS)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(NS), false); }
        }
        public Boolean INS
        {
            set { base.Bag[nameof(INS)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(INS), false); }
        }
        public string[] IncludePaths
        {
            set { base.Bag[nameof(IncludePaths)] = value; }
            get { return (string[])base.Bag[nameof(IncludePaths)]; }
        }

        public Boolean NoStandardDefs
        {
            set { base.Bag[nameof(NoStandardDefs)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(NoStandardDefs), false); }
        }
        public string RootNameSpace { get; set; }
        public Boolean VO1
        {
            set { base.Bag[nameof(VO1)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(VO1), false); }
        }
        public Boolean VO2
        {
            set { base.Bag[nameof(VO2)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(VO2), false); }
        }
        public Boolean VO3
        {
            set { base.Bag[nameof(VO3)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(VO3), false); }
        }
        public Boolean VO4
        {
            set { base.Bag[nameof(VO4)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(VO4), false); }
        }
        public Boolean VO5
        {
            set { base.Bag[nameof(VO5)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(VO5), false); }
        }
        public Boolean VO6
        {
            set { base.Bag[nameof(VO6)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(VO6), false); }
        }
        public Boolean VO7
        {
            set { base.Bag[nameof(VO7)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(VO7), false); }
        }
        public Boolean VO8
        {
            set { base.Bag[nameof(VO8)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(VO8), false); }
        }
        public Boolean VO9
        {
            set { base.Bag[nameof(VO9)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(VO9), false); }
        }
        public Boolean VO10
        {
            set { base.Bag[nameof(VO10)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(VO10), false); }
        }
        public Boolean VO11
        {
            set { base.Bag[nameof(VO11)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(VO11), false); }
        }
        public Boolean VO12
        {
            set { base.Bag[nameof(VO12)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(VO12), false); }
        }
        public Boolean VO13
        {
            set { base.Bag[nameof(VO13)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(VO13), false); }
        }
        public Boolean VO14
        {
            set { base.Bag[nameof(VO14)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(VO14), false); }
        }

        public Boolean VO15 {
            set { base.Bag[nameof(VO15)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(VO15), false); }
        }

        public Boolean VO16
        {
            set { base.Bag[nameof(VO16)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(VO16), false); }
        }
        public String CompilerPath
        {
            set { base.Bag[nameof(CompilerPath)] = value; }
            get { return (String)base.Bag[nameof(CompilerPath)]; }
        }
        // Misc. (unknown at that time) CommandLine options
        public String CommandLineOption
        {
            set { base.Bag[nameof(CommandLineOption)] = value; }
            get { return (String)base.Bag[nameof(CommandLineOption)]; }
        }
        #endregion

        #region XSharp specific properties
        public String Dialect
        {
            set { base.Bag[nameof(Dialect)] = value; }
            get { return (String)base.Bag[nameof(Dialect)]; }
        }
        #endregion
        #region properties copied from the csc task

        public Boolean AllowUnsafeBlocks
        {
            set { base.Bag[nameof(AllowUnsafeBlocks)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(AllowUnsafeBlocks), false); }
        }

        public String ApplicationConfiguration
        {
            set { base.Bag[nameof(ApplicationConfiguration)] = value; }
            get { return (String)base.Bag[nameof(ApplicationConfiguration)]; }
        }
        public String BaseAddress
        {
            set { base.Bag[nameof(BaseAddress)] = value; }
            get { return (String)base.Bag[nameof(BaseAddress)]; }
        }
        public Boolean CheckForOverflowUnderflow
        {
            set { base.Bag[nameof(CheckForOverflowUnderflow)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(CheckForOverflowUnderflow), false); }
        }
        public String DisabledWarnings
        {
            set { base.Bag[nameof(DisabledWarnings)] = value; }
            get { return (String)base.Bag[nameof(DisabledWarnings)]; }
        }
        public String DocumentationFile
        {
            set { base.Bag[nameof(DocumentationFile)] = value; }
            get { return (String)base.Bag[nameof(DocumentationFile)]; }
        }
        public Boolean ErrorEndLocation
        {
            set { base.Bag[nameof(ErrorEndLocation)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(ErrorEndLocation), false); }
        }

        public String ErrorReport
        {
            set { base.Bag[nameof(ErrorReport)] = value; }
            get { return (String)base.Bag[nameof(ErrorReport)]; }
        }
        public Boolean GenerateFullPaths
        {
            set { base.Bag[nameof(GenerateFullPaths)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(GenerateFullPaths), false); }
        }

        public String LangVersion
        {
            set { base.Bag[nameof(LangVersion)] = value; }
            get { return (String)base.Bag[nameof(LangVersion)]; }
        }
        public String ModuleAssemblyName
        {
            set { base.Bag[nameof(ModuleAssemblyName)] = value; }
            get { return (String)base.Bag[nameof(ModuleAssemblyName)]; }
        }

        public Boolean NoStandardLib
        {
            set { base.Bag[nameof(NoStandardLib)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(NoStandardLib), false); }
        }

        public String PdbFile
        {
            set { base.Bag[nameof(PdbFile)] = value; }
            get { return (String)base.Bag[nameof(PdbFile)]; }
        }

        /// <summary>
        /// Name of the language passed to "/preferreduilang" compiler option.
        /// </summary>
        /// <remarks>
        /// If set to null, "/preferreduilang" option is omitted, and csc.exe uses its default setting.
        /// Otherwise, the value is passed to "/preferreduilang" as is.
        /// </remarks>
        public String PreferredUILang
        {
            set { base.Bag[nameof(PreferredUILang)] = value; }
            get { return (String)base.Bag[nameof(PreferredUILang)]; }
        }
        public Boolean ReportAnalyzer
        {
            set { base.Bag[nameof(ReportAnalyzer)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(ReportAnalyzer), false); }
        }

        public bool VulcanCompatibleResources
        {
            set { base.Bag[nameof(VulcanCompatibleResources)] = value; }
            get { return (bool)base.Bag[nameof(VulcanCompatibleResources)]; }

        }
        public String VsSessionGuid
        {
            set { base.Bag[nameof(VsSessionGuid)] = value; }
            get { return (String)base.Bag[nameof(VsSessionGuid)]; }
        }


        // We do not support this
        //public Boolean UseHostCompilerIfAvailable
        //{
        //    set { base.Bag[nameof(UseHostCompilerIfAvailable)] = value; }
        //    get { return base.GetBoolParameterWithDefault(nameof(UseHostCompilerIfAvailable), false); }
        //}

        public int WarningLevel
        {
            set { base.Bag[nameof(WarningLevel)] = value; }
            get { return base.GetIntParameterWithDefault(nameof(WarningLevel), 4); }
        }

        public String WarningsAsErrors
        {
            set { base.Bag[nameof(WarningsAsErrors)] = value; }
            get { return (String)base.Bag[nameof(WarningsAsErrors)]; }
        }
        public String WarningsNotAsErrors
        {
            set { base.Bag[nameof(WarningsNotAsErrors)] = value; }
            get { return (String)base.Bag[nameof(WarningsNotAsErrors)]; }
        }

        #endregion

        #region properties copied from the Roslyn ManagedCompiler task

        public ITaskItem[] AdditionalFiles
        {
            set { base.Bag[nameof(AdditionalFiles)] = value; }
            get { return (ITaskItem[])base.Bag[nameof(AdditionalFiles)]; }
        }
        // AdditionalLibPaths in base
        // AddModules in base
        public ITaskItem[] Analyzers
        {
            set { base.Bag[nameof(Analyzers)] = value; }
            get { return (ITaskItem[])base.Bag[nameof(Analyzers)]; }
        }

        // We do not support BugReport because it always requires user interaction,
        // which will cause a hang.

        public String CodeAnalysisRuleSet
        {
            set { base.Bag[nameof(CodeAnalysisRuleSet)] = value; }
            get { return (String)base.Bag[nameof(CodeAnalysisRuleSet)]; }
        }
        // CodePage in base
        [Output]
        public ITaskItem[] CommandLineArgs
        {
            set { base.Bag[nameof(CommandLineArgs)] = value; }
            get { return (ITaskItem[])base.Bag[nameof(CommandLineArgs)]; }
        }
        // DebugType in base
        // DefineConstants
        // DelaySign
        public Boolean Deterministic
        {
            set { base.Bag[nameof(Deterministic)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(Deterministic), false); }
        }
        // EmitDebugInformation
        public String ErrorLog
        {
            set { base.Bag[nameof(ErrorLog)] = value; }
            get { return (String)base.Bag[nameof(ErrorLog)]; }
        }
        public String Features
        {
            set { base.Bag[nameof(Features)] = value; }
            get { return (String)base.Bag[nameof(Features)]; }
        }
        // FileAlignment
        // HighEntropyVA
        // KeyContainer
        // KeyFile
        // LinkResources
        // MainEntryPoint
        // NoConfig
        // NoLogo
        // NoWin32Manifest
        // Optimize
        // OutputAssembly
        public String PathMap
        {
            set { base.Bag[nameof(PathMap)] = value; }
            get { return (String)base.Bag[nameof(PathMap)]; }
        }
        // Platform
        // Prefer32Bit

        public Boolean ProvideCommandLineArgs
        {
            set { base.Bag[nameof(ProvideCommandLineArgs)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(ProvideCommandLineArgs), false); }
        }
        // References
        // Resources
        // ResponseFiles
        public Boolean SkipCompilerExecution
        {
            set { base.Bag[nameof(SkipCompilerExecution)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(SkipCompilerExecution), false); }
        }
        // Sources
        // SubsystemVersion
        // TargetType
        // TreatWarningsAsErrors

        /// <summary>
        /// If this property is true then the task will take every X#
        /// compilation which is queued by MSBuild and send it to the
        /// XSCompiler server instance, starting a new instance if necessary.
        /// If false, we will use the values from ToolPath/Exe.
        /// </summary>
        public Boolean UseSharedCompilation
        {
            set { base.Bag[nameof(UseSharedCompilation)] = value; }
            get { return base.GetBoolParameterWithDefault(nameof(UseSharedCompilation), false); }
        }


        // Utf8Output
        // Win32Icon
        // Win32Manifest
        // Win32Resource

        #endregion

        private bool useCRLF;
        private int errorCount;
        //private bool hasShownMaxErrorMsg;
        public Xsc() : base()
        {
            //System.Diagnostics.Debugger.Launch();
            useCRLF = !String.IsNullOrEmpty(System.Environment.GetEnvironmentVariable("XSHARPDEV"));
            errorCount = 0;
            //hasShownMaxErrorMsg = false;
            VulcanCompatibleResources = false;
            NoStandardDefs = false;
        }

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


        protected override string GenerateCommandLineCommands()
        {
            var commandLine = new XSharpCommandLineBuilder(false);
            commandLine.AppendWhenTrue("/noconfig", base.Bag, nameof(NoConfig));
            commandLine.AppendWhenTrue("/shared", base.Bag, nameof(UseSharedCompilation));
            return commandLine.ToString();
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

        protected override string GetResponseFileSwitch(string responseFilePath)
        {
            string newfile = Path.Combine(Path.GetDirectoryName(responseFilePath) , "LastXSharpResponseFile.Rsp");
            System.IO.File.Copy(responseFilePath, newfile, true);
            return base.GetResponseFileSwitch(responseFilePath);
        }

        protected override int ExecuteTool(string pathToTool, string responseFileCommands, string commandLineCommands)
        {
            int iResult;
            DateTime start = DateTime.Now;
            iResult = base.ExecuteTool(pathToTool, responseFileCommands, commandLineCommands);
            var time = DateTime.Now - start;
            var timestring = time.ToString();
            Log.LogMessageFromText("XSharp Compilation time: " + timestring, MessageImportance.High);
            return iResult;
        }

        private string FindXsc(string toolName)
        {
            if (string.IsNullOrEmpty(CompilerPath))
            {
                // If used after MSI Installer, value should be in the Registry
                string InstallPath = String.Empty;
                string node;
                if (IntPtr.Size == 4)
                    node = @"HKEY_LOCAL_MACHINE\" + XSharp.Constants.RegistryKey;
                else
                    node = @"HKEY_LOCAL_MACHINE\" + XSharp.Constants.RegistryKey64;

                try
                {
                    InstallPath = (string)Registry.GetValue(node, XSharp.Constants.RegistryValue, "");
                }
                catch (Exception)
                {
                    // Registry entry not found  x64 ?
                }
                if (string.IsNullOrEmpty(InstallPath))
                {
                    InstallPath = @"C:\Program Files (x86)\XSharp";
                }
                CompilerPath = Utilities.AddSlash(InstallPath) + "Bin\\";
                // Allow to override the path when developing.
                // Please note that this must be a complete path, for example "d:\Xsharp\Dev\XSharp\Binaries\Debug"

                string DevPath = System.Environment.GetEnvironmentVariable("XSHARPDEV");
                if (!string.IsNullOrEmpty(DevPath) )
                {
                    DevPath = Utilities.AddSlash(DevPath);
                    string testPath = Path.Combine(Path.GetDirectoryName(DevPath), toolName);
                    if (File.Exists(testPath))
                    {
                        CompilerPath = DevPath;
                    }
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


        /// <summary>
        /// The C# compiler (starting with Whidbey) supports assembly aliasing for references.
        /// See spec at http://devdiv/spectool/Documents/Whidbey/VCSharp/Design%20Time/M3%20DCRs/DCR%20Assembly%20aliases.doc.
        /// This method handles the necessary work of looking at the "Aliases" attribute on
        /// the incoming "References" items, and making sure to generate the correct
        /// command-line on csc.exe.  The syntax for aliasing a reference is:
        ///     csc.exe /reference:Foo=System.Xml.dll
        ///
        /// The "Aliases" attribute on the "References" items is actually a comma-separated
        /// list of aliases, and if any of the aliases specified is the string "global",
        /// then we add that reference to the command-line without an alias.
        /// </summary>

        internal static void AddReferencesToCommandLine(XSharpCommandLineBuilder commandLine, ITaskItem[] references, bool isInteractive = false)
        {
            // If there were no references passed in, don't add any /reference: switches
            // on the command-line.
            if (references == null)
            {
                return;
            }

            // Loop through all the references passed in.  We'll be adding separate
            // /reference: switches for each reference, and in some cases even multiple
            // /reference: switches per reference.
            foreach (ITaskItem reference in references)
            {
                // See if there was an "Alias" attribute on the reference.
                string aliasString = reference.GetMetadata("Aliases");


                string switchName = "/reference:";
                if (!isInteractive)
                {
                    bool embed = Utilities.TryConvertItemMetadataToBool(reference,
                                                                        "EmbedInteropTypes");

                    if (embed)
                    {
                        switchName = "/link:";
                    }
                }
                if (string.IsNullOrEmpty(aliasString))
                {
                    // If there was no "Alias" attribute, just add this as a global reference.
                    commandLine.AppendSwitchIfNotNull(switchName, reference.ItemSpec);
                }
                else
                {
                    // If there was an "Alias" attribute, it contains a comma-separated list
                    // of aliases to use for this reference.  For each one of those aliases,
                    // we're going to add a separate /reference: switch to the csc.exe
                    // command-line
                    string[] aliases = aliasString.Split(',');

                    foreach (string alias in aliases)
                    {
                        // Trim whitespace.
                        string trimmedAlias = alias.Trim();

                        if (alias.Length == 0)
                        {
                            continue;
                        }

                        // The alias should be a valid C# identifier.  Therefore it cannot
                        // contain comma, space, semicolon, or double-quote.  Let's check for
                        // the existence of those characters right here, and bail immediately
                        // if any are present.  There are a whole bunch of other characters
                        // that are not allowed in a C# identifier, but we'll just let csc.exe
                        // error out on those.  The ones we're checking for here are the ones
                        // that could seriously screw up the command-line parsing or could
                        // allow parameter injection.
                        if (trimmedAlias.IndexOfAny(new char[] { ',', ' ', ';', '"' }) != -1)
                        {
                            throw new Exception("Alias contains illegal characters :" + trimmedAlias);
                        }

                        // The alias called "global" is special.  It means that we don't
                        // give it an alias on the command-line.
                        if (string.Compare("global", trimmedAlias, StringComparison.OrdinalIgnoreCase) == 0)
                        {
                            commandLine.AppendSwitchIfNotNull(switchName, reference.ItemSpec);
                        }
                        else
                        {
                            // We have a valid (and explicit) alias for this reference.  Add
                            // it to the command-line using the syntax:
                            //      /reference:Foo=System.Xml.dll
                            commandLine.AppendSwitchAliased(switchName, trimmedAlias, reference.ItemSpec);
                        }
                    }
                }
            }
        }

        internal void AddVOCompatibilityCommands(XSharpCommandLineBuilder commandline)
        {
            // VO Compatibility switches

            if (NS)     // Add Default Namespace
            {
                commandline.AppendSwitch("/ns:" + this.RootNameSpace);
            }
            commandline.AppendPlusOrMinusSwitch("/az", base.Bag, nameof(AZ));
            //commandline.AppendPlusOrMinusSwitch("/cs", base.Bag, nameof(CS));
            commandline.AppendPlusOrMinusSwitch("/ins", base.Bag, nameof(INS));
            commandline.AppendPlusOrMinusSwitch("/lb", base.Bag, nameof(LB));
            commandline.AppendPlusOrMinusSwitch("/ovf", base.Bag, nameof(OVF));
            commandline.AppendPlusOrMinusSwitch("/ppo", base.Bag, nameof(PPO));
            commandline.AppendPlusOrMinusSwitch("/vo1", base.Bag, nameof(VO1));
            commandline.AppendPlusOrMinusSwitch("/vo2", base.Bag, nameof(VO2));
            commandline.AppendPlusOrMinusSwitch("/vo3", base.Bag, nameof(VO3));
            commandline.AppendPlusOrMinusSwitch("/vo4", base.Bag, nameof(VO4));
            commandline.AppendPlusOrMinusSwitch("/vo5", base.Bag, nameof(VO5));
            commandline.AppendPlusOrMinusSwitch("/vo6", base.Bag, nameof(VO6));
            commandline.AppendPlusOrMinusSwitch("/vo7", base.Bag, nameof(VO7));
            commandline.AppendPlusOrMinusSwitch("/vo8", base.Bag, nameof(VO8));
            commandline.AppendPlusOrMinusSwitch("/vo9", base.Bag, nameof(VO9));
            commandline.AppendPlusOrMinusSwitch("/vo10", base.Bag, nameof(VO10));
            //commandline.AppendPlusOrMinusSwitch("/vo11", base.Bag, nameof(VO11));
            commandline.AppendPlusOrMinusSwitch("/vo12", base.Bag, nameof(VO12));
            commandline.AppendPlusOrMinusSwitch("/vo13", base.Bag, nameof(VO13));
            commandline.AppendPlusOrMinusSwitch("/vo14", base.Bag, nameof(VO14));
            commandline.AppendPlusOrMinusSwitch("/vo15", base.Bag, nameof(VO15));
            commandline.AppendPlusOrMinusSwitch("/vo16", base.Bag, nameof(VO16));
            // User-defined CommandLine Option (in order to support switches unknown at that time)
            // cannot use appendswitch because it will quote the string when there are embedded spaces
            if (!String.IsNullOrEmpty(this.CommandLineOption))
            {
                commandline.AppendTextUnquoted( this.CommandLineOption);
            }
            if (this.IncludePaths?.Length > 0)
            {
                StringBuilder sb = new StringBuilder();
                foreach (var s in this.IncludePaths)
                {
                    if (sb.Length > 0)
                        sb.Append(';');
                    sb.Append(s);
                }
                string path = sb.ToString();
                path = path.Replace(@"\\", @"\");
                commandline.AppendTextUnquoted("/i:\"" + path + "\"");
            }
        }
        internal string PlatformWith32BitPreference
        {
            get
            {
                // no 32 bit preference for libraries
                string platform = this.Platform?.ToLower();
                if ((string.IsNullOrEmpty(platform) || platform.Equals("anycpu")) 
                    && this.Prefer32Bit
                    && TargetType.ToLower().Contains("exe"))
                {
                    platform = "anycpu32bitpreferred";
                }
                return platform;
            }
        }

        /// <summary>
        /// Mostly copied from the csc task in Roslyn
        /// </summary>
        /// <param name="commandLine"></param>
        internal void AddCscCompilerCommands(XSharpCommandLineBuilder commandLine)
        {
            commandLine.AppendSwitchIfNotNull("/lib:", AdditionalLibPaths, ",");
            commandLine.AppendPlusOrMinusSwitch("/unsafe", base.Bag, nameof(AllowUnsafeBlocks));
            commandLine.AppendPlusOrMinusSwitch("/checked", base.Bag, nameof(CheckForOverflowUnderflow));
            commandLine.AppendSwitchWithSplitting("/nowarn:", DisabledWarnings, ",", ';', ',');
            //commandLine.AppendWhenTrue("/fullpaths", base.Bag, nameof(GenerateFullPaths));
            commandLine.AppendSwitch("/fullpaths");
            commandLine.AppendSwitchIfNotNull("/langversion:", LangVersion);
            commandLine.AppendSwitchIfNotNull("/moduleassemblyname:", ModuleAssemblyName);
            commandLine.AppendSwitchIfNotNull("/pdb:", PdbFile);
            commandLine.AppendPlusOrMinusSwitch("/nostdlib", base.Bag, nameof(NoStandardLib));
            commandLine.AppendPlusOrMinusSwitch("/nostddefs", base.Bag, nameof(NoStandardDefs));
            commandLine.AppendSwitchIfNotNull("/platform:", PlatformWith32BitPreference);
            commandLine.AppendSwitchIfNotNull("/errorreport:", ErrorReport);
            commandLine.AppendSwitchWithInteger("/warn:", base.Bag, nameof(WarningLevel));
            commandLine.AppendSwitchIfNotNull("/doc:", DocumentationFile);
            commandLine.AppendSwitchIfNotNull("/baseaddress:", BaseAddress);
            commandLine.AppendSwitchUnquotedIfNotNull("/define:", Utilities.GetDefineConstantsSwitch(DefineConstants, Log));
            commandLine.AppendSwitchIfNotNull("/win32res:", Win32Resource);
            commandLine.AppendSwitchIfNotNull("/main:", MainEntryPoint);
            commandLine.AppendSwitchIfNotNull("/appconfig:", ApplicationConfiguration);
            commandLine.AppendWhenTrue("/errorendlocation", base.Bag, nameof(ErrorEndLocation));
            commandLine.AppendSwitchIfNotNull("/preferreduilang:", PreferredUILang);
            commandLine.AppendPlusOrMinusSwitch("/highentropyva", base.Bag, nameof(HighEntropyVA));
            //// If not design time build and the globalSessionGuid property was set then add a -globalsessionguid:<guid>
            //bool designTime = false;
            //if (HostObject != null)
            //{
            //    var csHost = HostObject as ICscHostObject;
            //    designTime = csHost.IsDesignTime();
            //}
            //if (!designTime)
            //{
            //    if (!string.IsNullOrWhiteSpace(VsSessionGuid))
            //    {
            //        commandLine.AppendSwitchIfNotNull("/sqmsessionguid:", VsSessionGuid);
            //    }
            //}

            AddReferencesToCommandLine(commandLine, References);
            AddManagedCompilerCommands(commandLine);
            // This should come after the "TreatWarningsAsErrors" flag is processed (in managedcompiler.cs).
            // Because if TreatWarningsAsErrors=false, then we'll have a /warnaserror- on the command-line,
            // and then any specific warnings that should be treated as errors should be specified with
            // /warnaserror+:<list> after the /warnaserror- switch.  The order of the switches on the command-line
            // does matter.
            //
            // Note that
            //      /warnaserror+
            // is just shorthand for:
            //      /warnaserror+:<all possible warnings>
            //
            // Similarly,
            //      /warnaserror-
            // is just shorthand for:
            //      /warnaserror-:<all possible warnings>
            commandLine.AppendSwitchWithSplitting("/warnaserror+:", WarningsAsErrors, ",", ';', ',');
            commandLine.AppendSwitchWithSplitting("/warnaserror-:", WarningsNotAsErrors, ",", ';', ',');

            // It's a good idea for the response file to be the very last switch passed, just
            // from a predictability perspective.  It also solves the problem that a dogfooder
            // ran into, which is described in an email thread attached to bug VSWhidbey 146883.
            // See also bugs 177762 and 118307 for additional bugs related to response file position.
            if (ResponseFiles != null)
            {
                foreach (ITaskItem response in ResponseFiles)
                {
                    commandLine.AppendSwitchIfNotNull("@", response.ItemSpec);
                }
            }

        }

#region Methods from ManagedCompiler in ROslyn
        /// <summary>
        /// Adds a "/features:" switch to the command line for each provided feature.
        /// </summary>
        internal static void AddFeatures(XSharpCommandLineBuilder commandLine, string features)
        {
            if (string.IsNullOrEmpty(features))
            {
                return;
            }
            // Todo: Implement /features commandline option
            //foreach (var feature in CompilerOptionParseUtilities.ParseFeatureFromMSBuild(features))
            //{
            //    commandLine.AppendSwitchIfNotNull("/features:", feature.Trim());
            //}
        }

        /// <summary>
        /// Adds a "/analyzer:" switch to the command line for each provided analyzer.
        /// </summary>
        internal static void AddAnalyzersToCommandLine(XSharpCommandLineBuilder commandLine, ITaskItem[] analyzers)
        {
            // If there were no analyzers passed in, don't add any /analyzer: switches
            // on the command-line.
            if (analyzers == null)
            {
                return;
            }

            foreach (ITaskItem analyzer in analyzers)
            {
                commandLine.AppendSwitchIfNotNull("/analyzer:", analyzer.ItemSpec);
            }
        }


        /// <summary>
        /// Adds a "/additionalfile:" switch to the command line for each additional file.
        /// </summary>
        private void AddAdditionalFilesToCommandLine(XSharpCommandLineBuilder commandLine)
        {
            // If there were no additional files passed in, don't add any /additionalfile: switches
            // on the command-line.
            if (AdditionalFiles == null)
            {
                return;
            }

            foreach (ITaskItem additionalFile in AdditionalFiles)
            {
                commandLine.AppendSwitchIfNotNull("/additionalfile:", additionalFile.ItemSpec);
            }
        }


        /// <summary>
        /// Configure the debug switches which will be placed on the compiler command-line.
        /// The matrix of debug type and symbol inputs and the desired results is as follows:
        ///
        /// Debug Symbols              DebugType   Desired Results
        ///          True               Full        /debug+ /debug:full
        ///          True               PdbOnly     /debug+ /debug:PdbOnly
        ///          True               None        /debug-
        ///          True               Blank       /debug+
        ///          False              Full        /debug- /debug:full
        ///          False              PdbOnly     /debug- /debug:PdbOnly
        ///          False              None        /debug-
        ///          False              Blank       /debug-
        ///          Blank              Full                /debug:full
        ///          Blank              PdbOnly             /debug:PdbOnly
        ///          Blank              None        /debug-
        /// Debug:   Blank              Blank       /debug+ //Microsoft.common.targets will set this
        /// Release: Blank              Blank       "Nothing for either switch"
        ///
        /// The logic is as follows:
        /// If debugtype is none  set debugtype to empty and debugSymbols to false
        /// If debugType is blank  use the debugsymbols "as is"
        /// If debug type is set, use its value and the debugsymbols value "as is"
        /// </summary>
        private void ConfigureDebugProperties()
        {
            // If debug type is set we need to take some action depending on the value. If debugtype is not set
            // We don't need to modify the EmitDebugInformation switch as its value will be used as is.
            if (base.Bag[nameof(DebugType)] != null)
            {
                // If debugtype is none then only show debug- else use the debug type and the debugsymbols as is.
                if (string.Compare((string)base.Bag[nameof(DebugType)], "none", StringComparison.OrdinalIgnoreCase) == 0)
                {
                    base.Bag[nameof(DebugType)] = null;
                    base.Bag[nameof(EmitDebugInformation)] = false;
                }
            }
        }
#endregion
        /// <summary>
        /// Mostly copied from the ManagedCompiler task in Roslyn
        /// </summary>
        /// <param name="commandLine"></param>

        internal void AddManagedCompilerCommands(XSharpCommandLineBuilder cmdline)
        {
            // If outputAssembly is not specified, then an "/out: <name>" option won't be added to
            // overwrite the one resulting from the OutputAssembly member of the CompilerParameters class.
            // In that case, we should set the outputAssembly member based on the first source file.
            XSharpCommandLineBuilder commandLine = (XSharpCommandLineBuilder)cmdline;
            if (
                    (OutputAssembly == null) &&
                    (Sources != null) &&
                    (Sources.Length > 0) &&
                    (ResponseFiles == null)    // The response file may already have a /out: switch in it, so don't try to be smart here.
                )
            {
                try
                {
                    OutputAssembly = new TaskItem(Path.GetFileNameWithoutExtension(Sources[0].ItemSpec));
                }
                catch (ArgumentException e)
                {
                    throw new ArgumentException(e.Message, "Sources");
                }
                if (string.Compare(TargetType, "library", StringComparison.OrdinalIgnoreCase) == 0)
                {
                    OutputAssembly.ItemSpec += ".dll";
                }
                else if (string.Compare(TargetType, "module", StringComparison.OrdinalIgnoreCase) == 0)
                {
                    OutputAssembly.ItemSpec += ".netmodule";
                }
                else
                {
                    OutputAssembly.ItemSpec += ".exe";
                }
            }
            commandLine.AppendSwitchIfNotNull("/addmodule:", AddModules, ",");
            commandLine.AppendSwitchWithInteger("/codepage:", base.Bag, nameof(CodePage));
            ConfigureDebugProperties();

            // The "DebugType" parameter should be processed after the "EmitDebugInformation" parameter
            // because it's more specific.  Order matters on the command-line, and the last one wins.
            // /debug+ is just a shorthand for /debug:full.  And /debug- is just a shorthand for /debug:none.

            commandLine.AppendPlusOrMinusSwitch("/debug", base.Bag, nameof(EmitDebugInformation));
            commandLine.AppendSwitchIfNotNull("/debug:", DebugType);

            commandLine.AppendPlusOrMinusSwitch("/delaysign", base.Bag, nameof(DelaySign));

            commandLine.AppendSwitchWithInteger("/filealign:", base.Bag, nameof(FileAlignment));
            commandLine.AppendSwitchIfNotNull("/keycontainer:", KeyContainer);
            commandLine.AppendSwitchIfNotNull("/keyfile:", KeyFile);
            // If the strings "LogicalName" or "Access" ever change, make sure to search/replace everywhere in vsproject.
            commandLine.AppendSwitchIfNotNull("/linkresource:", LinkResources, new string[] { "LogicalName", "Access" });
            commandLine.AppendWhenTrue("/nologo", base.Bag, nameof(NoLogo));
            commandLine.AppendWhenTrue("/nowin32manifest", base.Bag, nameof(NoWin32Manifest));
            commandLine.AppendPlusOrMinusSwitch("/optimize", base.Bag, nameof(Optimize));
            commandLine.AppendPlusOrMinusSwitch("/deterministic", base.Bag, nameof(Deterministic));
            commandLine.AppendSwitchIfNotNull("/pathmap:", PathMap);
            commandLine.AppendSwitchIfNotNull("/out:", OutputAssembly);
            commandLine.AppendSwitchIfNotNull("/ruleset:", CodeAnalysisRuleSet);
            commandLine.AppendSwitchIfNotNull("/errorlog:", ErrorLog);
            commandLine.AppendSwitchIfNotNull("/subsystemversion:", SubsystemVersion);
            commandLine.AppendWhenTrue("/reportanalyzer", base.Bag, nameof(ReportAnalyzer));
            // If the strings "LogicalName" or "Access" ever change, make sure to search/replace everywhere in vsproject.
            if (VulcanCompatibleResources)
                commandLine.AppendSwitchIfNotNull("/resource:", Resources, new string[] { });
            else
                commandLine.AppendSwitchIfNotNull("/resource:", Resources, new string[] { "LogicalName", "Access" });

            commandLine.AppendSwitchIfNotNull("/target:", TargetType);
            commandLine.AppendPlusOrMinusSwitch("/warnaserror", base.Bag, nameof(TreatWarningsAsErrors));
            commandLine.AppendWhenTrue("/utf8output", base.Bag, nameof(Utf8Output));
            commandLine.AppendSwitchIfNotNull("/win32icon:", Win32Icon);
            commandLine.AppendSwitchIfNotNull("/win32manifest:", Win32Manifest);

            AddFeatures(commandLine, Features);
            AddAnalyzersToCommandLine(commandLine, Analyzers);
            AddAdditionalFilesToCommandLine(commandLine);

            // Append the sources.
            
            commandLine.AppendFileNamesIfNotNull(Sources, useCRLF ? "\n " : " " );
            commandLine.AppendNewLine();

        }
        protected void AddResponseFileCommandsImpl(CommandLineBuilderExtension cmdline)
        {
            XSharpCommandLineBuilder commandLine = (XSharpCommandLineBuilder)cmdline;
            // The managed compiler command line options are called from the cscCompiler options
            if (this.Dialect?.Length > 0)
            {
                cmdline.AppendTextUnquoted("/dialect:" + this.Dialect);
            }
            AddCscCompilerCommands(commandLine);
            AddVOCompatibilityCommands(commandLine);

        }

        protected override string GenerateResponseFileCommands()
        {

            var commandLine = new XSharpCommandLineBuilder(useCRLF);
            this.AddResponseFileCommands(commandLine);
            return commandLine.ToString();
        }


        protected override void LogEventsFromTextOutput(string singleLine, MessageImportance messageImportance)
        {
            try
            {
                //if (errorCount < 500)
                //{
                base.LogEventsFromTextOutput(singleLine, messageImportance);
                //}
                //else if (! hasShownMaxErrorMsg)
                //{
                //    //hasShownMaxErrorMsg = true;
                //    // the line is in the format c:\....\file.prg (n,n,n,n): error/warning XSnnnn:
                //    string line = singleLine.Substring(0, singleLine.IndexOf(')')+2);
                //    line += " error XB9001:" + $"Truncating error list after at {errorCount} errors ";
                //    base.LogEventsFromTextOutput(line, MessageImportance.High);
                //}
                errorCount++;
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
