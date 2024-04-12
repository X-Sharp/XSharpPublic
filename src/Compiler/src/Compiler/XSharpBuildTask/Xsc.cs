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
using System.IO;
using System.Diagnostics;
using Roslyn.Utilities;

namespace XSharp.Build
{
    public class Xsc : ManagedCompiler
    {

        public Xsc() : base()
        {
            //System.Diagnostics.Debugger.Launch();
            useCRLF = !string.IsNullOrEmpty(System.Environment.GetEnvironmentVariable(Constants.EnvironmentXSharpDev));
            errorCount = 0;
        }


        // These are settings

        #region VO Compatible properties

        public bool AllowDot
        {
            set { _store[nameof(AllowDot)] = value; }
            get { return _store.GetOrDefault(nameof(AllowDot), false); }
        }
        public bool AllowOldStyleAssignments
        {
            set { _store[nameof(AllowOldStyleAssignments)] = value; }
            get { return _store.GetOrDefault(nameof(AllowOldStyleAssignments), false); }
        }
        public bool AZ
        {
            set { _store[nameof(AZ)] = value; }
            get { return _store.GetOrDefault(nameof(AZ), false); }
        }
        public bool CS
        {
            set { _store[nameof(CS)] = value; }
            get { return _store.GetOrDefault(nameof(CS), false); }
        }
        public bool EnforceOverride
        {
            set { _store[nameof(EnforceOverride)] = value; }
            get { return _store.GetOrDefault(nameof(EnforceOverride), false); }
        }
        public bool EnforceSelf
        {
            set { _store[nameof(EnforceSelf)] = value; }
            get { return _store.GetOrDefault(nameof(EnforceSelf), false); }
        }
        public bool NoInit
        {
            set { _store[nameof(NoInit)] = value; }
            get { return _store.GetOrDefault(nameof(NoInit), false); }
        }
        public bool LB
        {
            set { _store[nameof(LB)] = value; }
            get { return _store.GetOrDefault(nameof(LB), false); }
        }
        public bool OVF
        {
            set { _store[nameof(OVF)] = value; }
            get { return _store.GetOrDefault(nameof(OVF), false); }
        }
        public bool PPO
        {
            set { _store[nameof(PPO)] = value; }
            get { return _store.GetOrDefault(nameof(PPO), false); }
        }
        public bool MemVar
        {
            set { _store[nameof(MemVar)] = value; }
            get { return _store.GetOrDefault(nameof(MemVar), false); }
        }
        public bool ModernSyntax
        {
            set { _store[nameof(ModernSyntax)] = value; }
            get { return _store.GetOrDefault(nameof(ModernSyntax), false); }
        }
        public bool NamedArgs
        {
            set { _store[nameof(NamedArgs)] = value; }
            get { return _store.GetOrDefault(nameof(NamedArgs), false); }
        }
        public bool NS
        {
            set { _store[nameof(NS)] = value; }
            get { return _store.GetOrDefault(nameof(NS), false); }
        }
        public bool INS
        {
            set { _store[nameof(INS)] = value; }
            get { return _store.GetOrDefault(nameof(INS), false); }
        }
        public string[] IncludePaths
        {
            set { _store[nameof(IncludePaths)] = value; }
            get { return (string[])_store[nameof(IncludePaths)]; }
        }
        public bool InitLocals
        {
            set { _store[nameof(InitLocals)] = value; }
            get { return _store.GetOrDefault(nameof(InitLocals), false); }
        }

        public bool NoStandardDefs
        {
            set { _store[nameof(NoStandardDefs)] = value; }
            get { return _store.GetOrDefault(nameof(NoStandardDefs), false); }
        }

        public string StandardDefs
        {
            set { _store[nameof(StandardDefs)] = value; }
            get { return (string)_store[nameof(StandardDefs)]; }
        }
        public string RootNameSpace { get; set; }
        public bool Undeclared
        {
            set { _store[nameof(Undeclared)] = value; }
            get { return _store.GetOrDefault(nameof(Undeclared), false); }
        }

        public bool VO1
        {
            set { _store[nameof(VO1)] = value; }
            get { return _store.GetOrDefault(nameof(VO1), false); }
        }
        public bool VO2
        {
            set { _store[nameof(VO2)] = value; }
            get { return _store.GetOrDefault(nameof(VO2), false); }
        }
        public bool VO3
        {
            set { _store[nameof(VO3)] = value; }
            get { return _store.GetOrDefault(nameof(VO3), false); }
        }
        public bool VO4
        {
            set { _store[nameof(VO4)] = value; }
            get { return _store.GetOrDefault(nameof(VO4), false); }
        }
        public bool VO5
        {
            set { _store[nameof(VO5)] = value; }
            get { return _store.GetOrDefault(nameof(VO5), false); }
        }
        public bool VO6
        {
            set { _store[nameof(VO6)] = value; }
            get { return _store.GetOrDefault(nameof(VO6), false); }
        }
        public bool VO7
        {
            set { _store[nameof(VO7)] = value; }
            get { return _store.GetOrDefault(nameof(VO7), false); }
        }
        public bool VO8
        {
            set { _store[nameof(VO8)] = value; }
            get { return _store.GetOrDefault(nameof(VO8), false); }
        }
        public bool VO9
        {
            set { _store[nameof(VO9)] = value; }
            get { return _store.GetOrDefault(nameof(VO9), false); }
        }
        public bool VO10
        {
            set { _store[nameof(VO10)] = value; }
            get { return _store.GetOrDefault(nameof(VO10), false); }
        }
        public bool VO11
        {
            set { _store[nameof(VO11)] = value; }
            get { return _store.GetOrDefault(nameof(VO11), false); }
        }
        public bool VO12
        {
            set { _store[nameof(VO12)] = value; }
            get { return _store.GetOrDefault(nameof(VO12), false); }
        }
        public bool VO13
        {
            set { _store[nameof(VO13)] = value; }
            get { return _store.GetOrDefault(nameof(VO13), false); }
        }
        public bool VO14
        {
            set { _store[nameof(VO14)] = value; }
            get { return _store.GetOrDefault(nameof(VO14), false); }
        }

        public bool VO15
        {
            set { _store[nameof(VO15)] = value; }
            get { return _store.GetOrDefault(nameof(VO15), false); }
        }

        public bool VO16
        {
            set { _store[nameof(VO16)] = value; }
            get { return _store.GetOrDefault(nameof(VO16), false); }
        }
        public bool VO17
        {
            set { _store[nameof(VO17)] = value; }
            get { return _store.GetOrDefault(nameof(VO17), false); }
        }
        public bool XPP1
        {
            set { _store[nameof(XPP1)] = value; }
            get { return _store.GetOrDefault(nameof(XPP1), false); }
        }
        public bool FOX1
        {
            set { _store[nameof(FOX1)] = value; }
            get { return _store.GetOrDefault(nameof(FOX1), false); }
        }
        public bool FOX2
        {
            set { _store[nameof(FOX2)] = value; }
            get { return _store.GetOrDefault(nameof(FOX2), false); }
        }

        public string CompilerPath
        {
            set { _store[nameof(CompilerPath)] = value; }
            get { return (string)_store[nameof(CompilerPath)]; }
        }
        // Misc. (unknown at that time) CommandLine options
        public string CommandLineOption
        {
            set { _store[nameof(CommandLineOption)] = value; }
            get { return (string)_store[nameof(CommandLineOption)]; }
        }
        #endregion

        #region XSharp specific properties
        public string Dialect
        {
            set { _store[nameof(Dialect)] = value; }
            get { return (string)_store[nameof(Dialect)]; }
        }
        #endregion
        #region properties copied from the csc task

        public bool AllowUnsafeBlocks
        {
            set { _store[nameof(AllowUnsafeBlocks)] = value; }
            get { return _store.GetOrDefault(nameof(AllowUnsafeBlocks), false); }
        }

        public string ApplicationConfiguration
        {
            set { _store[nameof(ApplicationConfiguration)] = value; }
            get { return (string)_store[nameof(ApplicationConfiguration)]; }
        }
        public string BaseAddress
        {
            set { _store[nameof(BaseAddress)] = value; }
            get { return (string)_store[nameof(BaseAddress)]; }
        }
        public bool CheckForOverflowUnderflow
        {
            set { _store[nameof(CheckForOverflowUnderflow)] = value; }
            get { return _store.GetOrDefault(nameof(CheckForOverflowUnderflow), false); }
        }
        public string DocumentationFile
        {
            set { _store[nameof(DocumentationFile)] = value; }
            get { return (string)_store[nameof(DocumentationFile)]; }
        }

        public string DisabledWarnings
        {
            set { _store[nameof(DisabledWarnings)] = value; }
            get { return (string)_store[nameof(DisabledWarnings)]; }
        }

        public bool DisableSdkPath
        {
            set { _store[nameof(DisableSdkPath)] = value; }
            get { return _store.GetOrDefault(nameof(DisableSdkPath), false); }
        }

        public bool ErrorEndLocation
        {
            set { _store[nameof(ErrorEndLocation)] = value; }
            get { return _store.GetOrDefault(nameof(ErrorEndLocation), false); }
        }

        public string ErrorReport
        {
            set { _store[nameof(ErrorReport)] = value; }
            get { return (string)_store[nameof(ErrorReport)]; }
        }

        public string GeneratedFilesOutputPath
        {
            set { _store[nameof(GeneratedFilesOutputPath)] = value; }
            get { return (string)_store[nameof(GeneratedFilesOutputPath)]; }
        }

        public bool GenerateFullPaths
        {
            set { _store[nameof(GenerateFullPaths)] = value; }
            get { return _store.GetOrDefault(nameof(GenerateFullPaths), false); }
        }

        public string ModuleAssemblyName
        {
            set { _store[nameof(ModuleAssemblyName)] = value; }
            get { return (string)_store[nameof(ModuleAssemblyName)]; }
        }

        public bool NoStandardLib
        {
            set { _store[nameof(NoStandardLib)] = value; }
            get { return _store.GetOrDefault(nameof(NoStandardLib), false); }
        }

        public string PdbFile
        {
            set { _store[nameof(PdbFile)] = value; }
            get { return (string)_store[nameof(PdbFile)]; }
        }

        /// <summary>
        /// Name of the language passed to "/preferreduilang" compiler option.
        /// </summary>
        /// <remarks>
        /// If set to null, "/preferreduilang" option is omitted, and csc.exe uses its default setting.
        /// Otherwise, the value is passed to "/preferreduilang" as is.
        /// </remarks>
        public string PreferredUILang
        {
            set { _store[nameof(PreferredUILang)] = value; }
            get { return (string)_store[nameof(PreferredUILang)]; }
        }

        public bool UseNativeVersion
        {
            set { _store[nameof(UseNativeVersion)] = value; }
            get { return _store.GetOrDefault(nameof(UseNativeVersion),false); }

        }

        public string VsSessionGuid
        {
            set { _store[nameof(VsSessionGuid)] = value; }
            get { return (string)_store[nameof(VsSessionGuid)]; }
        }

        public bool UseHostCompilerIfAvailable
        {
            set { _store[nameof(UseHostCompilerIfAvailable)] = value; }
            get { return _store.GetOrDefault(nameof(UseHostCompilerIfAvailable), false); }
        }

        public bool VulcanCompatibleResources
        {
            set { _store[nameof(VulcanCompatibleResources)] = value; }
            get { return _store.GetOrDefault(nameof(VulcanCompatibleResources), false); }

        }

        public int WarningLevel
        {
            set { _store[nameof(WarningLevel)] = value; }
            get { return _store.GetOrDefault(nameof(WarningLevel), 4); }
        }

        public string WarningsAsErrors
        {
            set { _store[nameof(WarningsAsErrors)] = value; }
            get { return (string)_store[nameof(WarningsAsErrors)]; }
        }
        public string WarningsNotAsErrors
        {
            set { _store[nameof(WarningsNotAsErrors)] = value; }
            get { return (string)_store[nameof(WarningsNotAsErrors)]; }
        }

        public string Nullable
        {
            set
            {
                if (!string.IsNullOrEmpty(value))
                {
                    _store[nameof(Nullable)] = value;
                }
            }
            get { return (string)_store[nameof(Nullable)]; }
        }


        #endregion

        #region Properties that are in Targets but not used


        #endregion

        private bool useCRLF;
        private int errorCount;
        //private bool hasShownMaxErrorMsg;
        protected override string ToolNameWithoutExtension
        {
            get
            {
                return "xsc";
            }
        }

        protected override string GenerateFullPathToTool()
        {
            return FindXsc(this.ToolName);
        }

        protected internal override void AddCommandLineCommands(CommandLineBuilderExtension commandLine)
        {
            base.AddCommandLineCommands(commandLine);
            commandLine.AppendWhenTrue("/cs", _store, nameof(CS));
        }


        /// <summary>
        /// Mostly copied from the csc task in Roslyn
        /// </summary>
        /// <param name="commandLine"></param>
        internal void AddCscCompilerCommands(XSharpCommandLineBuilder commandLine)
        {
            commandLine.AppendSwitchIfNotNull("/lib:", AdditionalLibPaths, ",");
            commandLine.AppendPlusOrMinusSwitch("/unsafe", _store, nameof(AllowUnsafeBlocks));
            commandLine.AppendPlusOrMinusSwitch("/checked", _store, nameof(CheckForOverflowUnderflow));
            commandLine.AppendSwitchWithSplitting("/nowarn:", DisabledWarnings, ",", ';', ',');
            commandLine.AppendSwitchIfNotNull("/generatedfilesout:", GeneratedFilesOutputPath);
            commandLine.AppendWhenTrue("/fullpaths", _store, nameof(GenerateFullPaths));
            commandLine.AppendSwitchIfNotNull("/moduleassemblyname:", ModuleAssemblyName);
            commandLine.AppendSwitchIfNotNull("/pdb:", PdbFile);
            commandLine.AppendPlusOrMinusSwitch("/nostdlib", _store, nameof(NoStandardLib));
            commandLine.AppendSwitchIfNotNull("/platform:", PlatformWith32BitPreference);
            commandLine.AppendSwitchIfNotNull("/errorreport:", ErrorReport);
            commandLine.AppendSwitchWithInteger("/warn:", _store, nameof(WarningLevel));
            commandLine.AppendSwitchIfNotNull("/doc:", DocumentationFile);
            commandLine.AppendSwitchIfNotNull("/baseaddress:", BaseAddress);
            commandLine.AppendSwitchUnquotedIfNotNull("/define:", GetDefineConstantsSwitch(DefineConstants, Log));
            commandLine.AppendSwitchIfNotNull("/win32res:", Win32Resource);
            commandLine.AppendSwitchIfNotNull("/main:", MainEntryPoint);
            commandLine.AppendSwitchIfNotNull("/appconfig:", ApplicationConfiguration);
            commandLine.AppendWhenTrue("/errorendlocation", _store, nameof(ErrorEndLocation));
            commandLine.AppendSwitchIfNotNull("/preferreduilang:", PreferredUILang);
            commandLine.AppendPlusOrMinusSwitch("/highentropyva", _store, nameof(HighEntropyVA));
            commandLine.AppendSwitchIfNotNull("/nullable:", Nullable);
            commandLine.AppendWhenTrue("/nosdkpath", _store, nameof(DisableSdkPath));
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

        protected internal override void AddResponseFileCommands(CommandLineBuilderExtension commandLine)
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
            Utilities.CopyFileSafe(responseFilePath, newfile);
            return base.GetResponseFileSwitch(responseFilePath);
        }

        protected override int ExecuteTool(string pathToTool, string responseFileCommands, string commandLineCommands)
        {
            int iResult;
            DateTime start = DateTime.Now;
            iResult = base.ExecuteTool(pathToTool, responseFileCommands, commandLineCommands);
            var time = DateTime.Now - start;
            var timestring = time.ToString();
            Log.LogMessageFromText("XSharp Compilation time: " + timestring, MessageImportance.Normal);
            return iResult;
        }

        private string FindXsc(string toolName)
        {
            if (string.IsNullOrEmpty(CompilerPath))
            {
                // If used after MSI Installer, value should be in the Registry
                string InstallPath = Utilities.XSharpBinPath();
                CompilerPath = InstallPath;
                // Allow to override the path when developing.
                // Please note that this must be a complete path, for example "d:\Xsharp\Dev\XSharp\Binaries\Debug"

                string DevPath = System.Environment.GetEnvironmentVariable(Constants.EnvironmentXSharpDev);
                if (!string.IsNullOrEmpty(DevPath) )
                {
                    string testPath = Path.Combine(DevPath, toolName);
                    if (File.Exists(testPath))
                    {
                        CompilerPath = DevPath;
                    }
                }
            }
            // Search the compiler at the same place
            var xsc_file = Path.Combine(CompilerPath, toolName);
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
        ///     csc.exe /reference:Goo=System.Xml.dll
        ///
        /// The "Aliases" attribute on the "References" items is actually a comma-separated
        /// list of aliases, and if any of the aliases specified is the string "global",
        /// then we add that reference to the command-line without an alias.
        /// </summary>
        internal static void AddReferencesToCommandLine(
            XSharpCommandLineBuilder commandLine,
            ITaskItem[] references,
            bool isInteractive = false)
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

        /// <summary>
        /// Old VS projects had some pretty messed-up looking values for the
        /// "DefineConstants" property.  It worked fine in the IDE, because it
        /// effectively munged up the string so that it ended up being valid for
        /// the compiler.  We do the equivalent munging here now.
        ///
        /// Basically, we take the incoming string, and split it on comma/semicolon/space.
        /// Then we look at the resulting list of strings, and remove any that are
        /// illegal identifiers, and pass the remaining ones through to the compiler.
        ///
        /// Note that CSharp doesn't support assigning a value to the constants ... in
        /// other words, a constant is either defined or not defined ... it can't have
        /// an actual value.
        /// </summary>
        internal static string GetDefineConstantsSwitch(string originalDefineConstants, TaskLoggingHelper log)
        {
            if (originalDefineConstants == null)
            {
                return null;
            }

            StringBuilder finalDefineConstants = new StringBuilder();

            // Split the incoming string on comma/semicolon/space.
            string[] allIdentifiers = originalDefineConstants.Split(new char[] { ',', ';', ' ' });

            // Loop through all the parts, and for the ones that are legal C# identifiers,
            // add them to the outgoing string.
            foreach (string singleIdentifier in allIdentifiers)
            {
                if (UnicodeCharacterUtilities.IsValidIdentifier(singleIdentifier))
                {
                    // Separate them with a semicolon if there's something already in
                    // the outgoing string.
                    if (finalDefineConstants.Length > 0)
                    {
                        finalDefineConstants.Append(";");
                    }

                    finalDefineConstants.Append(singleIdentifier);
                }
                else if (singleIdentifier.Length > 0)
                {
                    log.LogWarningWithCodeFromResources("Csc_InvalidParameterWarning", "/define:", singleIdentifier);
                }
            }

            if (finalDefineConstants.Length > 0)
            {
                return finalDefineConstants.ToString();
            }
            else
            {
                // We wouldn't want to pass in an empty /define: switch on the csc.exe command-line.
                return null;
            }
        }


        internal void AddVOCompatibilityCommands(XSharpCommandLineBuilder commandline)
        {
            // VO Compatibility switches
            commandline.AppendPlusOrMinusSwitch("/nostddefs", _store, nameof(NoStandardDefs));
            if (!string.IsNullOrEmpty(StandardDefs))
            {
                commandline.AppendSwitchIfNotNull("/stddefs:", StandardDefs);
            }
            commandline.AppendWhenTrue("/usenativeversion", _store, nameof(UseNativeVersion));

            if (NS)     // Add Default Namespace
            {
                if (this.RootNameSpace.Contains(" "))
                {
                    this.RootNameSpace = '"' + this.RootNameSpace + '"';
                }
                commandline.AppendSwitch("/ns:" + this.RootNameSpace);
            }
            commandline.AppendPlusOrMinusSwitch("/allowdot", _store, nameof(AllowDot));
            commandline.AppendPlusOrMinusSwitch("/allowoldstyleassignments", _store, nameof(AllowOldStyleAssignments));
            commandline.AppendPlusOrMinusSwitch("/az", _store, nameof(AZ));
            commandline.AppendPlusOrMinusSwitch("/cs", _store, nameof(CS));
            commandline.AppendPlusOrMinusSwitch("/initlocals", _store, nameof(InitLocals));
            commandline.AppendPlusOrMinusSwitch("/ins", _store, nameof(INS));
            commandline.AppendPlusOrMinusSwitch("/lb", _store, nameof(LB));
            commandline.AppendPlusOrMinusSwitch("/namedarguments", _store, nameof(NamedArgs));
            if (Dialect.ToLower() != "core" && Dialect.ToLower() != "vulcan")
            {
                commandline.AppendPlusOrMinusSwitch("/memvar", _store, nameof(MemVar));
                commandline.AppendPlusOrMinusSwitch("/undeclared", _store, nameof(Undeclared));
            }
            commandline.AppendPlusOrMinusSwitch("/enforceself", _store, nameof(EnforceSelf));
            commandline.AppendPlusOrMinusSwitch("/enforceoverride", _store, nameof(EnforceOverride));
            commandline.AppendPlusOrMinusSwitch("/modernsyntax", _store, nameof(ModernSyntax));
            commandline.AppendPlusOrMinusSwitch("/noinit", _store, nameof(NoInit));
            commandline.AppendPlusOrMinusSwitch("/ovf", _store, nameof(OVF));
            commandline.AppendPlusOrMinusSwitch("/ppo", _store, nameof(PPO));
            commandline.AppendPlusOrMinusSwitch("/vo1", _store, nameof(VO1));
            commandline.AppendPlusOrMinusSwitch("/vo2", _store, nameof(VO2));
            commandline.AppendPlusOrMinusSwitch("/vo3", _store, nameof(VO3));
            commandline.AppendPlusOrMinusSwitch("/vo4", _store, nameof(VO4));
            commandline.AppendPlusOrMinusSwitch("/vo5", _store, nameof(VO5));
            commandline.AppendPlusOrMinusSwitch("/vo6", _store, nameof(VO6));
            commandline.AppendPlusOrMinusSwitch("/vo7", _store, nameof(VO7));
            commandline.AppendPlusOrMinusSwitch("/vo8", _store, nameof(VO8));
            commandline.AppendPlusOrMinusSwitch("/vo9", _store, nameof(VO9));
            commandline.AppendPlusOrMinusSwitch("/vo10", _store, nameof(VO10));
            commandline.AppendPlusOrMinusSwitch("/vo11", _store, nameof(VO11));
            commandline.AppendPlusOrMinusSwitch("/vo12", _store, nameof(VO12));
            commandline.AppendPlusOrMinusSwitch("/vo13", _store, nameof(VO13));
            commandline.AppendPlusOrMinusSwitch("/vo14", _store, nameof(VO14));
            commandline.AppendPlusOrMinusSwitch("/vo15", _store, nameof(VO15));
            commandline.AppendPlusOrMinusSwitch("/vo16", _store, nameof(VO16));
            commandline.AppendPlusOrMinusSwitch("/vo17", _store, nameof(VO17));
            if (Dialect.ToLower() == "xpp")
            {
                commandline.AppendPlusOrMinusSwitch("/xpp1", _store, nameof(XPP1));
            }
            if (Dialect.ToLower() == "foxpro")
            {
                commandline.AppendPlusOrMinusSwitch("/fox1", _store, nameof(FOX1));
                commandline.AppendPlusOrMinusSwitch("/fox2", _store, nameof(FOX2));
            }

            // User-defined CommandLine Option (in order to support switches unknown at that time)
            // cannot use appendswitch because it will quote the string when there are embedded spaces
            if (!string.IsNullOrEmpty(this.CommandLineOption))
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

        /// <summary>
        /// Mostly copied from the ManagedCompiler task in Roslyn
        /// </summary>
        /// <param name="cmdline"></param>

        internal void AddManagedCompilerCommands(XSharpCommandLineBuilder cmdline)
        {
            // If outputAssembly is not specified, then an "/out: <name>" option won't be added to
            // overwrite the one resulting from the OutputAssembly member of the CompilerParameters class.
            // In that case, we should set the outputAssembly member based on the first source file.
            XSharpCommandLineBuilder commandLine = (XSharpCommandLineBuilder)cmdline;
            var resources = Resources;
            var sources = Sources;
            Sources = null;
            if (VulcanCompatibleResources)
            {
                // Do not let the resources be handled by the managedcompiler class
                Resources = null;
            }
            base.AddResponseFileCommands(cmdline);
            Resources = resources;
            if (VulcanCompatibleResources)
            {
                Resources = resources;
                commandLine.AppendSwitchIfNotNull("/resource:", Resources, new string[] { });
            }

            // Append the sources.
            Sources = sources;
            commandLine.AppendFileNamesIfNotNull(Sources, useCRLF ? "\n " : " ");
            commandLine.AppendNewLine();

        }
        protected void AddResponseFileCommandsImpl(CommandLineBuilderExtension cmdline)
        {
            XSharpCommandLineBuilder commandLine = (XSharpCommandLineBuilder)cmdline;
            // The managed compiler command line options are called from the cscCompiler options
            if (this.Dialect?.Length > 0)
            {
                commandLine.AppendSwitchUnquotedIfNotNull("/dialect:" , this.Dialect);
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

        private static readonly string[] s_separators = { Environment.NewLine };

        internal override void LogMessages(string output, MessageImportance messageImportance)
        {
            var lines = output.Split(s_separators, StringSplitOptions.RemoveEmptyEntries);
            foreach (string line in lines)
            {
                string trimmedMessage = line.Trim();
                if (trimmedMessage != "")
                {
                    Log.LogMessageFromText(trimmedMessage, messageImportance);
                }
            }
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
