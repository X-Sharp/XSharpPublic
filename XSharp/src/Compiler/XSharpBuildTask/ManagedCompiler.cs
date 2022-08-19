//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Most of this code was 'inspired' by the C# build task
// Note that not all properties are in the correct alphabetical order.
// We try to keep them in the same order as in the ManagedCompiler class from Roslyn


using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text;
using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;
using Roslyn.Utilities;

namespace XSharp.Build
{
    /// <summary>Defines the properties and methods common to managed compiler tasks.</summary>
    public abstract class ManagedCompiler : ManagedToolTask
    {
        internal PropertyDictionary _store = new PropertyDictionary();
        //private bool hostCompilerSupportsAllParameters;

        /// <summary>Gets or sets the additional folders in which to look for assemblies.</summary>
        /// <returns>The additional folders in which to look for assemblies.</returns>
        #region Properties

        public string[] AdditionalLibPaths
        {
            set { _store[nameof(AdditionalLibPaths)] = value; }
            get { return (string[])_store[nameof(AdditionalLibPaths)]; }
        }


        /// <summary>Gets or sets the modules for the compiler to make available to the project you are currently compiling.</summary>
        /// <returns>The modules for the compiler to make available to the project you are currently compiling.</returns>
        public string[] AddModules
        {
            set { _store[nameof(AddModules)] = value; }
            get { return (string[])_store[nameof(AddModules)]; }
        }
        public ITaskItem[] AdditionalFiles
        {
            set { _store[nameof(AdditionalFiles)] = value; }
            get { return (ITaskItem[])_store[nameof(AdditionalFiles)]; }
        }

        public ITaskItem[] EmbeddedFiles
        {
            set { _store[nameof(EmbeddedFiles)] = value; }
            get { return (ITaskItem[])_store[nameof(EmbeddedFiles)]; }
        }

        public bool EmbedAllSources
        {
            set { _store[nameof(EmbedAllSources)] = value; }
            get { return _store.GetOrDefault(nameof(EmbedAllSources), false); }
        }

        public ITaskItem[] Analyzers
        {
            set { _store[nameof(Analyzers)] = value; }
            get { return (ITaskItem[])_store[nameof(Analyzers)]; }
        }

        // We do not support BugReport because it always requires user interaction,
        // which will cause a hang.
        public string ChecksumAlgorithm
        {
            set { _store[nameof(ChecksumAlgorithm)] = value; }
            get { return (string)_store[nameof(ChecksumAlgorithm)]; }
        }

        public string CodeAnalysisRuleSet
        {
            set { _store[nameof(CodeAnalysisRuleSet)] = value; }
            get { return (string)_store[nameof(CodeAnalysisRuleSet)]; }
        }

        /// <summary>Gets or sets the code page to use for all source code files in the compilation.</summary>
        /// <returns>The code page to use for all source code files in the compilation.</returns>
        public int CodePage
        {
            set { _store[nameof(CodePage)] = value; }
            get { return _store.GetOrDefault(nameof(CodePage), 0); }
        }
		
        [Output]
        public ITaskItem[] CommandLineArgs
        {
            set { _store[nameof(CommandLineArgs)] = value; }
            get { return (ITaskItem[])_store[nameof(CommandLineArgs)]; }
        }

        /// <summary>Gets or sets the debug type.</summary>
        /// <returns>The debug type.</returns>
        public string DebugType
        {
            set { _store[nameof(DebugType)] = value; }
            get { return (string)_store[nameof(DebugType)]; }
        }
		
        public string SourceLink
        {
            set { _store[nameof(SourceLink)] = value; }
            get { return (string)_store[nameof(SourceLink)]; }
        }

        /// <summary>Gets or sets the conditional compiler constants.</summary>
        /// <returns>The conditional compiler constants.</returns>
        public string DefineConstants
        {
            set { _store[nameof(DefineConstants)] = value; }
            get { return (string)_store[nameof(DefineConstants)]; }
        }

        /// <summary>Gets or sets a value indicating whether the public key is placed in the assembly.</summary>
        /// <returns>true if the public key is placed in the assembly; false if the assembly is fully signed.</returns>
        public bool DelaySign
        {
            set { _store[nameof(DelaySign)] = value; }
            get { return _store.GetOrDefault(nameof(DelaySign), false); }
        }
        public bool Deterministic
        {
            set { _store[nameof(Deterministic)] = value; }
            get { return _store.GetOrDefault(nameof(Deterministic), false); }
        }

        public bool PublicSign
        {
            set { _store[nameof(PublicSign)] = value; }
            get { return _store.GetOrDefault(nameof(PublicSign), false); }
        }

        public ITaskItem[] AnalyzerConfigFiles
        {
            set { _store[nameof(AnalyzerConfigFiles)] = value; }
            get { return (ITaskItem[])_store[nameof(AnalyzerConfigFiles)]; }
        }

        /// <summary>Gets or sets a value indicating whether the compiler generates debugging information.</summary>
        /// <returns>true if debugging information is generated; otherwise, false;</returns>
        public bool EmitDebugInformation
        {
            set { _store[nameof(EmitDebugInformation)] = value; }
            get { return _store.GetOrDefault(nameof(EmitDebugInformation), false); }
        }

        public string ErrorLog
        {
            set { _store[nameof(ErrorLog)] = value; }
            get { return (string)_store[nameof(ErrorLog)]; }
        }

        public string Features
        {
            set { _store[nameof(Features)] = value; }
            get { return (string)_store[nameof(Features)]; }
        }

        /// <summary>Gets or sets a value indicating where to align the sections of the output file.</summary>
        /// <returns>A value indicating where to align the sections of the output file.</returns>
        public int FileAlignment
        {
            set { _store[nameof(FileAlignment)] = value; }
            get { return _store.GetOrDefault(nameof(FileAlignment), 0); }
        }

        public bool HighEntropyVA
        {
            set { _store[nameof(HighEntropyVA)] = value; }
            get { return _store.GetOrDefault(nameof(HighEntropyVA), false); }
        }

        /// <summary>
        /// Specifies the list of instrumentation kinds to be used during compilation.
        /// </summary>
        public string Instrument
        {
            set { _store[nameof(Instrument)] = value; }
            get { return (string)_store[nameof(Instrument)]; }
        }

        /// <summary>Gets or sets the name of the cryptographic key container.</summary>
        /// <returns>The name of the cryptographic key container.</returns>
        public string KeyContainer
        {
            set { _store[nameof(KeyContainer)] = value; }
            get { return (string)_store[nameof(KeyContainer)]; }
        }

        /// <summary>Gets or sets the file name containing the cryptographic key.</summary>
        /// <returns>The file name containing the cryptographic key.</returns>
        public string KeyFile
        {
            set { _store[nameof(KeyFile)] = value; }
            get { return (string)_store[nameof(KeyFile)]; }
        }

        /// <summary>Gets or sets the .NET Framework resource files to link to the output file.</summary>
        /// <returns>The .NET Framework resource files to link to the output file.</returns>
        public ITaskItem[] LinkResources
        {
            set { _store[nameof(LinkResources)] = value; }
            get { return (ITaskItem[])_store[nameof(LinkResources)]; }
        }

        /// <summary>Gets or sets the class or module that contains the main entry point.</summary>
        /// <returns>The class or module that contains the main entry point.</returns>
        public string MainEntryPoint
        {
            set { _store[nameof(MainEntryPoint)] = value; }
            get { return (string)_store[nameof(MainEntryPoint)]; }
        }

        /// <summary>Gets or sets a value indicating whether the compiler should use the default response file.</summary>
        /// <returns>true if the compiler is not using the default response file; otherwise, false.</returns>
        public bool NoConfig
        {
            set { _store[nameof(NoConfig)] = value; }
            get { return _store.GetOrDefault(nameof(NoConfig), false); }
        }

        /// <summary>Gets or sets a value indicating whether to suppress the compiler banner information.</summary>
        /// <returns>true to suppress the compiler banner information; otherwise, false.</returns>
        public bool NoLogo
        {
            set { _store[nameof(NoLogo)] = value; }
            get { return _store.GetOrDefault(nameof(NoLogo), false); }
        }

        /// <summary>Gets or sets a Boolean value that specifies whether an external UAC manifest is generated for the application.</summary>
        /// <returns>true if an external UAC manifest is generated for the application; otherwise, false.</returns>
        public bool NoWin32Manifest
        {
            set { _store[nameof(NoWin32Manifest)] = value; }
            get { return _store.GetOrDefault(nameof(NoWin32Manifest), false); }
        }

        /// <summary>Gets or sets a value indicating whether to enable compiler optimizations.</summary>
        /// <returns>true to enable compiler optimizations; otherwise, false.</returns>
        public bool Optimize
        {
            set { _store[nameof(Optimize)] = value; }
            get { return _store.GetOrDefault(nameof(Optimize), false); }
        }

        /// <summary>Gets or sets the name of the output file.</summary>
        /// <returns>The name of the output file.</returns>
        [Output]
        public ITaskItem OutputAssembly
        {
            set { _store[nameof(OutputAssembly)] = value; }
            get { return (ITaskItem)_store[nameof(OutputAssembly)]; }
        }



		[Output]
        public ITaskItem OutputRefAssembly
        {
            set { _store[nameof(OutputRefAssembly)] = value; }
            get { return (ITaskItem)_store[nameof(OutputRefAssembly)]; }
        }
		
        public string Platform
        {
            set { _store[nameof(Platform)] = value; }
            get { return (string)_store[nameof(Platform)]; }
        }

        public ITaskItem[] PotentialAnalyzerConfigFiles
        {
            set { _store[nameof(PotentialAnalyzerConfigFiles)] = value; }
            get { return (ITaskItem[])_store[nameof(PotentialAnalyzerConfigFiles)]; }
        }

        public bool Prefer32Bit
        {
            set { _store[nameof(Prefer32Bit)] = value; }
            get { return _store.GetOrDefault(nameof(Prefer32Bit), false); }
        }

        public bool ProvideCommandLineArgs
        {
            set { _store[nameof(ProvideCommandLineArgs)] = value; }
            get { return _store.GetOrDefault(nameof(ProvideCommandLineArgs), false); }
        }

        public ITaskItem[] References
        {
            set { _store[nameof(References)] = value; }
            get { return (ITaskItem[])_store[nameof(References)]; }
        }

        public bool RefOnly
        {
            set { _store[nameof(RefOnly)] = value; }
            get { return _store.GetOrDefault(nameof(RefOnly), false); }
        }

        public bool ReportAnalyzer
        {
            set { _store[nameof(ReportAnalyzer)] = value; }
            get { return _store.GetOrDefault(nameof(ReportAnalyzer), false); }
        }

        /// <summary>Gets or sets the .NET Framework resources to embed in the output file.</summary>
        /// <returns>The .NET Framework resources to embed in the output file.</returns>
        public ITaskItem[] Resources
        {
            set { _store[nameof(Resources)] = value; }
            get { return (ITaskItem[])_store[nameof(Resources)]; }
        }

        public string RuntimeMetadataVersion
        {
            set { _store[nameof(RuntimeMetadataVersion)] = value; }
            get { return (string)_store[nameof(RuntimeMetadataVersion)]; }
        }

        /// <summary>Gets or sets the response files that contain commands for the task.</summary>
        /// <returns>The response files that contain commands for the task.</returns>
        public ITaskItem[] ResponseFiles
        {
            set { _store[nameof(ResponseFiles)] = value; }
            get { return (ITaskItem[])_store[nameof(ResponseFiles)]; }
        }


        public string SharedCompilationId
        {
            set { _store[nameof(SharedCompilationId)] = value; }
            get { return (string)_store[nameof(SharedCompilationId)]; }
        }

        public bool SkipAnalyzers
        {
            set { _store[nameof(SkipAnalyzers)] = value; }
            get { return _store.GetOrDefault(nameof(SkipAnalyzers), false); }
        }

        public bool SkipCompilerExecution
        {
            set { _store[nameof(SkipCompilerExecution)] = value; }
            get { return _store.GetOrDefault(nameof(SkipCompilerExecution), false); }
        }

        /// <summary>Gets or sets the source files to compile.</summary>
        /// <returns>The source files to compile.</returns>
        public ITaskItem[] Sources
        {
            set
            {
                if (UsedCommandLineTool)
                {
                    NormalizePaths(value);
                }

                _store[nameof(Sources)] = value;
            }
            get { return (ITaskItem[])_store[nameof(Sources)]; }
        }

        public string SubsystemVersion
        {
            set { _store[nameof(SubsystemVersion)] = value; }
            get { return (string)_store[nameof(SubsystemVersion)]; }
        }

        /// <summary>Gets or sets the file format of the output file.</summary>
        /// <returns>The file format of the output file.</returns>
        public string TargetType
        {
            set
            {
                _store[nameof(TargetType)] = value != null
                    ? CultureInfo.InvariantCulture.TextInfo.ToLower(value)
                    : null;
            }
            get { return (string)_store[nameof(TargetType)]; }
        }

        /// <summary>Gets or sets a value indicating whether warnings are treated as errors.</summary>
        /// <returns>true if warnings are treated as errors; otherwise, false.</returns>
        public bool TreatWarningsAsErrors
        {
            set { _store[nameof(TreatWarningsAsErrors)] = value; }
            get { return _store.GetOrDefault(nameof(TreatWarningsAsErrors), false); }
        }
        /// <summary>Gets or sets a value indicating whether compiler output is logged using UTF-8 encoding.</summary>
        /// <returns>true if compiler output is logged using UTF-8 encoding; otherwise, false.</returns>
        public bool Utf8Output
        {
            set { _store[nameof(Utf8Output)] = value; }
            get { return _store.GetOrDefault(nameof(Utf8Output), false); }
        }

        /// <summary>Gets or sets the icon file name.</summary>
        /// <returns>The icon file name.</returns>
        public string Win32Icon
        {
            set { _store[nameof(Win32Icon)] = value; }
            get { return (string)_store[nameof(Win32Icon)]; }
        }

        /// <summary>Gets or sets the Win32 manifest.</summary>
        /// <returns>The Win32 manifest.</returns>
        public string Win32Manifest
        {
            set { _store[nameof(Win32Manifest)] = value; }
            get { return (string)_store[nameof(Win32Manifest)]; }
        }

        /// <summary>Gets or sets a Win32 resource (.res) file to insert in the output file.</summary>
        /// <returns>The Win32 resource (.res) file to insert in the output file.</returns>
        public string Win32Resource
        {
            set { _store[nameof(Win32Resource)] = value; }
            get { return (string)_store[nameof(Win32Resource)]; }
        }

        public string PathMap
        {
            set { _store[nameof(PathMap)] = value; }
            get { return (string)_store[nameof(PathMap)]; }
        }

        /// <summary>
        /// If this property is true then the task will take every C# or VB
        /// compilation which is queued by MSBuild and send it to the
        /// XSCompiler server instance, starting a new instance if necessary.
        /// If false, we will use the values from ToolPath/Exe.
        /// </summary>
        public bool UseSharedCompilation
        {
            set { _store[nameof(UseSharedCompilation)] = value; }
            get { return _store.GetOrDefault(nameof(UseSharedCompilation), false); }
        }

        // Map explicit platform of "AnyCPU" or the default platform (null or ""), since it is commonly understood in the
        // managed build process to be equivalent to "AnyCPU", to platform "AnyCPU32BitPreferred" if the Prefer32Bit
        // property is set.
        internal string PlatformWith32BitPreference
        {
            get
            {
                string platform = Platform;
                if ((string.IsNullOrEmpty(platform) || platform.Equals("anycpu", StringComparison.OrdinalIgnoreCase)) && Prefer32Bit)
                {
                    platform = "anycpu32bitpreferred";
                }
                return platform;
            }
        }

        /// <summary>Gets or sets the encoding of the captured task standard output stream.</summary>
        /// <returns>The encoding of the captured task standard output stream.</returns>
        protected override Encoding StandardOutputEncoding
        {
            get
            {
                return (Utf8Output) ? Encoding.UTF8 : base.StandardOutputEncoding;
            }
        }

        public string LangVersion
        {
            set { _store[nameof(LangVersion)] = value; }
            get { return (string)_store[nameof(LangVersion)]; }
        }

        #endregion
        // ToolExe delegates back to ToolName if the override is not
        // set.  So, if ToolExe == ToolName, we know ToolExe is not
        // explicitly overriden.  So, if both ToolPath is unset and
        // ToolExe == ToolName, we know nothing is overridden, and
        // we can use our own csc.
        private bool HasToolBeenOverridden => !(string.IsNullOrEmpty(ToolPath) && ToolExe == ToolName);

        protected sealed override bool IsManagedTool => !HasToolBeenOverridden;

        /// <summary>
        /// Method for testing only
        /// </summary>
        public string GeneratePathToTool()
        {
            return GenerateFullPathToTool();
        }
        // sealed override string PathToManagedTool => Utilities.GenerateFullPathToTool(ToolName);

        //protected sealed override string PathToNativeTool => Path.Combine(ToolPath ?? "", ToolExe);

        //protected override int ExecuteTool(string pathToTool, string responseFileCommands, string commandLineCommands)
        //{
        //    if (ProvideCommandLineArgs)
        //    {
        //        CommandLineArgs = GetArguments(commandLineCommands, responseFileCommands)
        //            .Select(arg => new TaskItem(arg)).ToArray();
        //    }

        //    if (SkipCompilerExecution)
        //    {
        //        return 0;
        //    }

        //    try
        //    {
        //        if (!UseSharedCompilation ||
        //            HasToolBeenOverridden ||
        //            !BuildServerConnection.IsCompilerServerSupported)
        //        {
        //            return base.ExecuteTool(pathToTool, responseFileCommands, commandLineCommands);
        //        }

        //        using (_sharedCompileCts = new CancellationTokenSource())
        //        {

        //            CompilerServerLogger.Log($"CommandLine = '{commandLineCommands}'");
        //            CompilerServerLogger.Log($"BuildResponseFile = '{responseFileCommands}'");

        //            var clientDir = Path.GetDirectoryName(PathToManagedTool);

        //            // Note: we can't change the "tool path" printed to the console when we run
        //            // the Csc/Vbc task since MSBuild logs it for us before we get here. Instead,
        //            // we'll just print our own message that contains the real client location
        //            Log.LogMessage(ErrorString.UsingSharedCompilation, clientDir);

        //            var workingDir = CurrentDirectoryToUse();
        //            var buildPaths = new BuildPathsAlt(
        //                clientDir: clientDir,
        //                // MSBuild doesn't need the .NET SDK directory
        //                sdkDir: null,
        //                workingDir: workingDir,
        //                tempDir: BuildServerConnection.GetTempPath(workingDir));

        //            // Note: using ToolArguments here (the property) since
        //            // commandLineCommands (the parameter) may have been mucked with
        //            // (to support using the dotnet cli)
        //            var responseTask = BuildServerConnection.RunServerCompilation(
        //                Language,
        //                string.IsNullOrEmpty(SharedCompilationId) ? null : SharedCompilationId,
        //                GetArguments(ToolArguments, responseFileCommands).ToList(),
        //                buildPaths,
        //                keepAlive: null,
        //                libEnvVariable: LibDirectoryToUse(),
        //                cancellationToken: _sharedCompileCts.Token);

        //            responseTask.Wait(_sharedCompileCts.Token);

        //            var response = responseTask.Result;
        //            if (response != null)
        //            {
        //                ExitCode = HandleResponse(response, pathToTool, responseFileCommands, commandLineCommands);
        //            }
        //            else
        //            {
        //                Log.LogMessage(ErrorString.SharedCompilationFallback, pathToTool);

        //                ExitCode = base.ExecuteTool(pathToTool, responseFileCommands, commandLineCommands);
        //            }
        //        }
        //    }
        //    catch (OperationCanceledException)
        //    {
        //        ExitCode = 0;
        //    }
        //    catch (Exception e)
        //    {
        //        Log.LogErrorWithCodeFromResources("Compiler_UnexpectedException");
        //        LogErrorOutput(e.ToString());
        //        ExitCode = -1;
        //    }

        //    return ExitCode;
        //}

        //// <summary>
        //// Cancel the in-process build task.
        //// </summary>
        //public override void Cancel()
        //{
        //    base.Cancel();

        //    _sharedCompileCts?.Cancel();
        //}

        /// <summary>
        /// Get the current directory that the compiler should run in.
        /// </summary>
        private string CurrentDirectoryToUse()
        {
            // ToolTask has a method for this. But it may return null. Use the process directory
            // if ToolTask didn't override. MSBuild uses the process directory.
            string workingDirectory = GetWorkingDirectory();
            if (string.IsNullOrEmpty(workingDirectory))
            {
                workingDirectory = Directory.GetCurrentDirectory();
            }
            return workingDirectory;
        }

        /// <summary>
        /// Get the "LIB" environment variable, or NULL if none.
        /// </summary>
        private string LibDirectoryToUse()
        {
            // First check the real environment.
            string libDirectory = Environment.GetEnvironmentVariable("LIB");

            // Now go through additional environment variables.
            string[] additionalVariables = EnvironmentVariables;
            if (additionalVariables != null)
            {
                foreach (string var in EnvironmentVariables)
                {
                    if (var.StartsWith("LIB=", StringComparison.OrdinalIgnoreCase))
                    {
                        libDirectory = var.Substring(4);
                    }
                }
            }

            return libDirectory;
        }

        /// <summary>
        /// The return code of the compilation. Strangely, this isn't overridable from ToolTask, so we need
        /// to create our own.
        /// </summary>
        [Output]
        public new int ExitCode { get; private set; }

        /// <summary>
        /// Handle a response from the server, reporting messages and returning
        /// the appropriate exit code.
        /// </summary>
        //private int HandleResponse(BuildResponse response, string pathToTool, string responseFileCommands, string commandLineCommands)
        //{
        //    if (response.Type != BuildResponse.ResponseType.Completed)
        //    {
        //        ValidateBootstrapUtil.AddFailedServerConnection();
        //    }

        //    switch (response.Type)
        //    {
        //        case BuildResponse.ResponseType.Completed:
        //            var completedResponse = (CompletedBuildResponse)response;
        //            LogMessages(completedResponse.Output, StandardOutputImportanceToUse);

        //            if (LogStandardErrorAsError)
        //            {
        //                LogErrorOutput(completedResponse.ErrorOutput);
        //            }
        //            else
        //            {
        //                LogMessages(completedResponse.ErrorOutput, StandardErrorImportanceToUse);
        //            }

        //            return completedResponse.ReturnCode;

        //        case BuildResponse.ResponseType.MismatchedVersion:
        //            LogErrorOutput("Roslyn compiler server reports different protocol version than build task.");
        //            return base.ExecuteTool(pathToTool, responseFileCommands, commandLineCommands);

        //        case BuildResponse.ResponseType.Rejected:
        //        case BuildResponse.ResponseType.AnalyzerInconsistency:
        //            return base.ExecuteTool(pathToTool, responseFileCommands, commandLineCommands);

        //        default:
        //            LogErrorOutput($"Received an unrecognized response from the server: {response.Type}");
        //            return base.ExecuteTool(pathToTool, responseFileCommands, commandLineCommands);
        //    }
        //}

        private void LogErrorOutput(string output)
        {
            LogErrorOutput(output, Log);
        }

        internal static void LogErrorOutput(string output, TaskLoggingHelper log)
        {
            string[] lines = output.Split(new string[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries);
            foreach (string line in lines)
            {
                string trimmedMessage = line.Trim();
                if (trimmedMessage != "")
                {
                    log.LogError(trimmedMessage);
                }
            }
        }

        /// <summary>
        /// Log each of the messages in the given output with the given importance.
        /// We assume each line is a message to log.
        /// </summary>
        /// <remarks>
        /// Should be "private protected" visibility once it is introduced into C#.
        /// </remarks>
        internal abstract void LogMessages(string output, MessageImportance messageImportance);

        public string GenerateResponseFileContents()
        {
            return GenerateResponseFileCommands();
        }

        ///// <summary>
        ///// Get the command line arguments to pass to the compiler.
        ///// </summary>
        //private string[] GetArguments(string commandLineCommands, string responseFileCommands)
        //{
        //    var commandLineArguments =
        //        CommandLineUtilities.SplitCommandLineIntoArguments(commandLineCommands, removeHashComments: true);
        //    var responseFileArguments =
        //        CommandLineUtilities.SplitCommandLineIntoArguments(responseFileCommands, removeHashComments: true);
        //    return commandLineArguments.Concat(responseFileArguments).ToArray();
        //}

        /// <summary>
        /// Returns the command line switch used by the tool executable to specify the response file
        /// Will only be called if the task returned a non empty string from GetResponseFileCommands
        /// Called after ValidateParameters, SkipTaskExecution and GetResponseFileCommands
        /// </summary>
        protected override string GenerateResponseFileCommands()
        {
            CommandLineBuilderExtension commandLineBuilder = new CommandLineBuilderExtension();
            AddResponseFileCommands(commandLineBuilder);
            return commandLineBuilder.ToString();
        }

        /// <summary>
        /// Method for testing only
        /// </summary>
        public string GenerateCommandLine()
        {
            return GenerateCommandLineCommands();
        }

        protected sealed override string ToolArguments
        {
            get
            {
                var builder = new CommandLineBuilderExtension();
                AddCommandLineCommands(builder);
                return builder.ToString();
            }
        }

        /// <summary>
        /// Fills the provided CommandLineBuilderExtension with those switches and other information that can't go into a response file and
        /// must go directly onto the command line.
        /// </summary>
        protected internal virtual void AddCommandLineCommands(CommandLineBuilderExtension commandLine)
        {
            commandLine.AppendWhenTrue("/shared", _store, nameof(UseSharedCompilation));
            commandLine.AppendWhenTrue("/noconfig", _store, nameof(NoConfig));
        }

        /// <summary>
        /// Fills the provided CommandLineBuilderExtension with those switches and other information that can go into a response file.
        /// </summary>
        /// <param name="commandLine">Command line builder to add arguments to.</param>
        protected internal virtual void AddResponseFileCommands(CommandLineBuilderExtension commandLine)
        {
            // If outputAssembly is not specified, then an "/out: <name>" option won't be added to
            // overwrite the one resulting from the OutputAssembly member of the CompilerParameters class.
            // In that case, we should set the outputAssembly member based on the first source file.
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
            commandLine.AppendSwitchWithInteger("/codepage:", _store, nameof(CodePage));

            ConfigureDebugProperties();

            // The "DebugType" parameter should be processed after the "EmitDebugInformation" parameter
            // because it's more specific.  Order matters on the command-line, and the last one wins.
            // /debug+ is just a shorthand for /debug:full.  And /debug- is just a shorthand for /debug:none.

            commandLine.AppendPlusOrMinusSwitch("/debug", _store, nameof(EmitDebugInformation));
            commandLine.AppendSwitchIfNotNull("/debug:", DebugType);

            commandLine.AppendPlusOrMinusSwitch("/delaysign", _store, nameof(DelaySign));

            commandLine.AppendSwitchWithInteger("/filealign:", _store, nameof(FileAlignment));
            commandLine.AppendSwitchIfNotNull("/keycontainer:", KeyContainer);
            commandLine.AppendSwitchIfNotNull("/keyfile:", KeyFile);
            // If the strings "LogicalName" or "Access" ever change, make sure to search/replace everywhere in vsproject.
            commandLine.AppendSwitchIfNotNull("/linkresource:", LinkResources, new string[] { "LogicalName", "Access" });
            commandLine.AppendWhenTrue("/nologo", _store, nameof(NoLogo));
            commandLine.AppendWhenTrue("/nowin32manifest", _store, nameof(NoWin32Manifest));
            commandLine.AppendPlusOrMinusSwitch("/optimize", _store, nameof(Optimize));
            commandLine.AppendSwitchIfNotNull("/pathmap:", PathMap);
            commandLine.AppendSwitchIfNotNull("/out:", OutputAssembly);
            commandLine.AppendSwitchIfNotNull("/refout:", OutputRefAssembly);
            commandLine.AppendWhenTrue("/refonly", _store, nameof(RefOnly));
            commandLine.AppendSwitchIfNotNull("/ruleset:", CodeAnalysisRuleSet);
            commandLine.AppendSwitchIfNotNull("/errorlog:", ErrorLog);
            commandLine.AppendSwitchIfNotNull("/subsystemversion:", SubsystemVersion);
            commandLine.AppendWhenTrue("/reportanalyzer", _store, nameof(ReportAnalyzer));
            // If the strings "LogicalName" or "Access" ever change, make sure to search/replace everywhere in vsproject.
            commandLine.AppendSwitchIfNotNull("/resource:", Resources, new string[] { "LogicalName", "Access" });
            commandLine.AppendSwitchIfNotNull("/target:", TargetType);
            commandLine.AppendPlusOrMinusSwitch("/warnaserror", _store, nameof(TreatWarningsAsErrors));
            commandLine.AppendWhenTrue("/utf8output", _store, nameof(Utf8Output));
            commandLine.AppendSwitchIfNotNull("/win32icon:", Win32Icon);
            commandLine.AppendSwitchIfNotNull("/win32manifest:", Win32Manifest);

            AddResponseFileCommandsForSwitchesSinceInitialReleaseThatAreNeededByTheHost(commandLine);
            AddAnalyzersToCommandLine(commandLine, Analyzers);
            AddAdditionalFilesToCommandLine(commandLine);

            // Append the sources.
            commandLine.AppendFileNamesIfNotNull(Sources, " ");
        }

        internal void AddResponseFileCommandsForSwitchesSinceInitialReleaseThatAreNeededByTheHost(CommandLineBuilderExtension commandLine)
        {
            commandLine.AppendPlusOrMinusSwitch("/deterministic", _store, nameof(Deterministic));
            commandLine.AppendPlusOrMinusSwitch("/publicsign", _store, nameof(PublicSign));
            commandLine.AppendSwitchIfNotNull("/runtimemetadataversion:", RuntimeMetadataVersion);
            commandLine.AppendSwitchIfNotNull("/checksumalgorithm:", ChecksumAlgorithm);
            commandLine.AppendSwitchWithSplitting("/instrument:", Instrument, ",", ';', ',');
            commandLine.AppendSwitchIfNotNull("/sourcelink:", SourceLink);
            commandLine.AppendSwitchIfNotNull("/langversion:", LangVersion);
            commandLine.AppendPlusOrMinusSwitch("/skipanalyzers", _store, nameof(SkipAnalyzers));

            AddFeatures(commandLine, Features);
            AddEmbeddedFilesToCommandLine(commandLine);
            AddAnalyzerConfigFilesToCommandLine(commandLine);
        }
		
        /// <summary>
        /// Adds a "/features:" switch to the command line for each provided feature.
        /// </summary>
        internal static void AddFeatures(CommandLineBuilderExtension commandLine, string features)
        {
            if (string.IsNullOrEmpty(features))
            {
                return;
            }

            foreach (var feature in CompilerOptionParseUtilities.ParseFeatureFromMSBuild(features))
            {
                commandLine.AppendSwitchIfNotNull("/features:", feature.Trim());
            }
        }
		
        /// <summary>
        /// Adds a "/analyzer:" switch to the command line for each provided analyzer.
        /// </summary>
        internal static void AddAnalyzersToCommandLine(CommandLineBuilderExtension commandLine, ITaskItem[] analyzers)
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
        private void AddAdditionalFilesToCommandLine(CommandLineBuilderExtension commandLine)
        {
            if (AdditionalFiles != null)
            {
                foreach (ITaskItem additionalFile in AdditionalFiles)
                {
                    commandLine.AppendSwitchIfNotNull("/additionalfile:", additionalFile.ItemSpec);
                }
            }
        }

        /// <summary>
        /// Adds a "/embed:" switch to the command line for each pdb embedded file.
        /// </summary>
        private void AddEmbeddedFilesToCommandLine(CommandLineBuilderExtension commandLine)
        {
            if (EmbedAllSources)
            {
                commandLine.AppendSwitch("/embed");
            }

            if (EmbeddedFiles != null)
            {
                foreach (ITaskItem embeddedFile in EmbeddedFiles)
                {
                    commandLine.AppendSwitchIfNotNull("/embed:", embeddedFile.ItemSpec);
                }
            }
        }
		
        /// <summary>
        /// Adds a "/editorconfig:" switch to the command line for each .editorconfig file.
        /// </summary>
        private void AddAnalyzerConfigFilesToCommandLine(CommandLineBuilderExtension commandLine)
        {
            if (AnalyzerConfigFiles != null)
            {
                foreach (ITaskItem analyzerConfigFile in AnalyzerConfigFiles)
                {
                    commandLine.AppendSwitchIfNotNull("/analyzerconfig:", analyzerConfigFile.ItemSpec);
                }
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
            if (_store[nameof(DebugType)] != null)
            {
                // If debugtype is none then only show debug- else use the debug type and the debugsymbols as is.
                if (string.Compare((string)_store[nameof(DebugType)], "none", StringComparison.OrdinalIgnoreCase) == 0)
                {
                    _store[nameof(DebugType)] = null;
                    _store[nameof(EmitDebugInformation)] = false;
                }
            }
        }

        /// <summary>
        /// Validate parameters, log errors and warnings and return true if
        /// Execute should proceed.
        /// </summary>
        /// <returns>true if all parameters are valid; otherwise, false.</returns>
        protected override bool ValidateParameters()
        {
            return ListHasNoDuplicateItems(Resources, nameof(Resources), "LogicalName", Log) && ListHasNoDuplicateItems(Sources, nameof(Sources), Log);
        }

        /// <summary>
        /// Returns true if the provided item list contains duplicate items, false otherwise.
        /// </summary>
        /// <param name="itemList">The list of items to examine for duplicates.</param>
        /// <param name="parameterName">The name of the parameter that contains the <paramref name="itemList" />.</param>
        internal static bool ListHasNoDuplicateItems(ITaskItem[] itemList, string parameterName, TaskLoggingHelper log)
        {
            return ListHasNoDuplicateItems(itemList, parameterName, null, log);
        }

        /// <summary>
        /// Returns true if the provided item list contains duplicate items, false otherwise.
        /// </summary>
        /// <param name="itemList"></param>
        /// <param name="disambiguatingMetadataName">Optional name of metadata that may legitimately disambiguate items. May be null.</param>
        /// <param name="parameterName"></param>
        /// <param name="log"></param>
        private static bool ListHasNoDuplicateItems(ITaskItem[] itemList, string parameterName, string disambiguatingMetadataName, TaskLoggingHelper log)
        {
            if (itemList == null || itemList.Length == 0)
            {
                return true;
            }

            var alreadySeen = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
            foreach (ITaskItem item in itemList)
            {
                string key;
                string disambiguatingMetadataValue = null;
                if (disambiguatingMetadataName != null)
                {
                    disambiguatingMetadataValue = item.GetMetadata(disambiguatingMetadataName);
                }

                if (disambiguatingMetadataName == null || string.IsNullOrEmpty(disambiguatingMetadataValue))
                {
                    key = item.ItemSpec;
                }
                else
                {
                    key = item.ItemSpec + ":" + disambiguatingMetadataValue;
                }

                if (alreadySeen.ContainsKey(key))
                {
                    if (disambiguatingMetadataName == null || string.IsNullOrEmpty(disambiguatingMetadataValue))
                    {
                        log.LogError("MSB3105: The item \"{0}\" was specified more than once in the \"{1}\" parameter. Duplicate items are not supported by the \"{1}\" parameter.", item.ItemSpec, parameterName);
                    }
                    else
                    {
                        log.LogError("MSB3083: The item \"{0}\" was specified more than once in the \"{1}\" parameter and both items had the same value \"{2}\" for the \"{3}\" metadata. Duplicate items are not supported by the \"{1}\" parameter.", item.ItemSpec, parameterName, disambiguatingMetadataValue, disambiguatingMetadataName);
                    }
                    return false;
                }
                else
                {
                    alreadySeen[key] = string.Empty;
                }
            }

            return true;
        }

        /// <summary>
        /// Allows tool to handle the return code.
        /// This method will only be called with non-zero exitCode.
        /// </summary>
        protected override bool HandleTaskExecutionErrors()
        {
            // For managed compilers, the compiler should emit the appropriate
            // error messages before returning a non-zero exit code, so we don't
            // normally need to emit any additional messages now.
            //
            // If somehow the compiler DID return a non-zero exit code and didn't log an error, we'd like to log that exit code.
            // We can only do this for the command line compiler: if the inproc compiler was used,
            // we can't tell what if anything it logged as it logs directly to Visual Studio's output window.
            //
            if (!Log.HasLoggedErrors && UsedCommandLineTool)
            {
                // This will log a message "MSB3093: The command exited with code {0}."
                base.HandleTaskExecutionErrors();
            }

            return false;
        }

        /// <summary>
        /// Takes a list of files and returns the normalized locations of these files
        /// </summary>
        private void NormalizePaths(ITaskItem[] taskItems)
        {
            if (taskItems is null)
                return;

            foreach (var item in taskItems)
            {
                item.ItemSpec = Utilities.GetFullPathNoThrow(item.ItemSpec);
            }
        }

        /// <summary>
        /// Whether the command line compiler was invoked, instead
        /// of the host object compiler.
        /// </summary>
        protected bool UsedCommandLineTool
        {
            get;
            set;
        }

        private bool _hostCompilerSupportsAllParameters;
        protected bool HostCompilerSupportsAllParameters
        {
            get { return _hostCompilerSupportsAllParameters; }
            set { _hostCompilerSupportsAllParameters = value; }
        }

  //      /// <summary>
  //      /// Checks the bool result from calling one of the methods on the host compiler object to
  //      /// set one of the parameters.  If it returned false, that means the host object doesn't
  //      /// support a particular parameter or variation on a parameter.  So we log a comment,
  //      /// and set our state so we know not to call the host object to do the actual compilation.
  //      /// </summary>
  //      /// <owner>RGoel</owner>
		///// <param name="resultFromHostObjectSetOperation">true if the host compiler supports <paramref name="parameterName" />; otherwise, false</param>
  //      /// <param name="parameterName">The parameter name to set on the host compiler.</param>

        //protected void CheckHostObjectSupport
        //    (
        //    string parameterName,
        //    bool resultFromHostObjectSetOperation
        //    )
        //{
        //    if (!resultFromHostObjectSetOperation)
        //    {
        //        Log.LogMessage(MessageImportance.Normal, "The compiler does not support the \"{0}\" parameter.", parameterName);
        //        hostCompilerSupportsAllParameters = false;
        //    }
        //}

        //internal void InitializeHostObjectSupportForNewSwitches(ITaskHost hostObject, ref string param)
        //{
        //    var compilerOptionsHostObject = hostObject as ICompilerOptionsHostObject;

        //    if (compilerOptionsHostObject != null)
        //    {
        //        var commandLineBuilder = new CommandLineBuilderExtension();
        //        AddResponseFileCommandsForSwitchesSinceInitialReleaseThatAreNeededByTheHost(commandLineBuilder);
        //        param = "CompilerOptions";
        //        CheckHostObjectSupport(param, compilerOptionsHostObject.SetCompilerOptions(commandLineBuilder.ToString()));
        //    }
        //}
        ///// <summary>Verifies that all specified references exist on disk.</summary>
        /// <returns>true if all references exist on disk; otherwise, false.</returns>
        protected bool CheckAllReferencesExistOnDisk()
        {
            if (null == References)
            {
                // No references
                return true;
            }

            bool success = true;

            foreach (ITaskItem reference in References)
            {
                if (!File.Exists(reference.ItemSpec))
                {
                    success = false;
                    base.Log.LogError("MSB3104: The referenced assembly \"{0}\" was not found.", reference.ItemSpec);
                }
            }

            return success;
        }

        /// <summary>
        /// The IDE and command line compilers unfortunately differ in how win32
        /// manifests are specified.  In particular, the command line compiler offers a
        /// "/nowin32manifest" switch, while the IDE compiler does not offer analogous
        /// functionality. If this switch is omitted from the command line and no win32
        /// manifest is specified, the compiler will include a default win32 manifest
        /// named "default.win32manifest" found in the same directory as the compiler
        /// executable. Again, the IDE compiler does not offer analogous support.
        ///
        /// We'd like to imitate the command line compiler's behavior in the IDE, but
        /// it isn't aware of the default file, so we must compute the path to it if
        /// noDefaultWin32Manifest is false and no win32Manifest was provided by the
        /// project.
        ///
        /// This method will only be called during the initialization of the host object,
        /// which is only used during IDE builds.
        /// </summary>
        /// <returns>the path to the win32 manifest to provide to the host object</returns>
        internal string GetWin32ManifestSwitch
        (
            bool noDefaultWin32Manifest,
            string win32Manifest
        )
        {
            if (!noDefaultWin32Manifest)
            {
                if (string.IsNullOrEmpty(win32Manifest) && string.IsNullOrEmpty(Win32Resource))
                {
                    // We only want to consider the default.win32manifest if this is an executable
                    if (!string.Equals(TargetType, "library", StringComparison.OrdinalIgnoreCase)
                       && !string.Equals(TargetType, "module", StringComparison.OrdinalIgnoreCase))
            {
                        // We need to compute the path to the default win32 manifest
                        string pathToDefaultManifest = ToolLocationHelper.GetPathToDotNetFrameworkFile
                                                       (
                                                           "default.win32manifest",

                                                           // We are choosing to pass Version46 instead of VersionLatest. TargetDotNetFrameworkVersion
                                                           // is an enum, and VersionLatest is not some sentinel value but rather a constant that is
                                                           // equal to the highest version defined in the enum. Enum values, being constants, are baked
                                                           // into consuming assembly, so specifying VersionLatest means not the latest version wherever
                                                           // this code is running, but rather the latest version of the framework according to the
                                                           // reference assembly with which this assembly was built. As of this writing, we are building
                                                           // our bits on machines with Visual Studio 2015 that know about 4.6.1, so specifying
                                                           // VersionLatest would bake in the enum value for 4.6.1. But we need to run on machines with
                                                           // MSBuild that only know about Version46 (and no higher), so VersionLatest will fail there.
                                                           // Explicitly passing Version46 prevents this problem.
                                                           TargetDotNetFrameworkVersion.Version46
                                                       );

                        if (null == pathToDefaultManifest)
                        {
						// This is rather unlikely, and the inproc compiler seems to log an error anyway.
	                    Log.LogMessage("Expected file \"{ 0}\" does not exist", "default.win32manifest");
                        }

                        return pathToDefaultManifest;
                    }
                }
            }

            return win32Manifest;
        }
    }
}
