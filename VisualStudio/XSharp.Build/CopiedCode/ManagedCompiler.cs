// Microsoft.Build.Tasks.ManagedCompiler
using Microsoft.Build.Framework;
using Microsoft.Build.Tasks;
using Microsoft.Build.Utilities;
using System;
using System.Collections;
using System.Globalization;
using System.IO;
using System.Security.Permissions;
using System.Text;

/// <summary>Defines the properties and methods common to managed compiler tasks.</summary>

namespace XSharp.Build
{
    public abstract class ManagedCompiler : ToolTaskExtension
    {
        
        private bool hostCompilerSupportsAllParameters;

        /// <summary>Gets or sets the additional folders in which to look for assemblies.</summary>
        /// <returns>The additional folders in which to look for assemblies.</returns>
        public string[] AdditionalLibPaths
        {
            get
            {
                return (string[])base.Bag["AdditionalLibPaths"];
            }
            set
            {
                base.Bag["AdditionalLibPaths"] = value;
            }
        }

        /// <summary>Gets or sets the modules for the compiler to make available to the project you are currently compiling.</summary>
        /// <returns>The modules for the compiler to make available to the project you are currently compiling.</returns>
        public string[] AddModules
        {
            get
            {
                return (string[])base.Bag["AddModules"];
            }
            set
            {
                base.Bag["AddModules"] = value;
            }
        }

        /// <summary>Gets or sets the code page to use for all source code files in the compilation.</summary>
        /// <returns>The code page to use for all source code files in the compilation.</returns>
        public int CodePage
        {
            get
            {
                return GetIntParameterWithDefault("CodePage", 0);
            }
            set
            {
                base.Bag["CodePage"] = value;
            }
        }

        /// <summary>Gets or sets the debug type.</summary>
        /// <returns>The debug type.</returns>
        public string DebugType
        {
            get
            {
                return (string)base.Bag["DebugType"];
            }
            set
            {
                base.Bag["DebugType"] = value;
            }
        }

        /// <summary>Gets or sets the conditional compiler constants.</summary>
        /// <returns>The conditional compiler constants.</returns>
        public string DefineConstants
        {
            get
            {
                return (string)base.Bag["DefineConstants"];
            }
            set
            {
                base.Bag["DefineConstants"] = value;
            }
        }

        /// <summary>Gets or sets a value indicating whether the public key is placed in the assembly.</summary>
        /// <returns>true if the public key is placed in the assembly; false if the assembly is fully signed.</returns>
        public bool DelaySign
        {
            get
            {
                return GetBoolParameterWithDefault("DelaySign", defaultValue: false);
            }
            set
            {
                base.Bag["DelaySign"] = value;
            }
        }

        /// <summary>Gets or sets a value indicating whether the compiler generates debugging information.</summary>
        /// <returns>true if debugging information is generated; otherwise, false;</returns>
        public bool EmitDebugInformation
        {
            get
            {
                return GetBoolParameterWithDefault("EmitDebugInformation", defaultValue: false);
            }
            set
            {
                base.Bag["EmitDebugInformation"] = value;
            }
        }

        /// <summary>Gets or sets a value indicating where to align the sections of the output file.</summary>
        /// <returns>A value indicating where to align the sections of the output file.</returns>
        public int FileAlignment
        {
            get
            {
                return GetIntParameterWithDefault("FileAlignment", 0);
            }
            set
            {
                base.Bag["FileAlignment"] = value;
            }
        }

        public bool HighEntropyVA
        {
            get
            {
                return GetBoolParameterWithDefault("HighEntropyVA", defaultValue: false);
            }
            set
            {
                base.Bag["HighEntropyVA"] = value;
            }
        }

        /// <summary>Gets or sets the name of the cryptographic key container.</summary>
        /// <returns>The name of the cryptographic key container.</returns>
        public string KeyContainer
        {
            get
            {
                return (string)base.Bag["KeyContainer"];
            }
            set
            {
                base.Bag["KeyContainer"] = value;
            }
        }

        /// <summary>Gets or sets the file name containing the cryptographic key.</summary>
        /// <returns>The file name containing the cryptographic key.</returns>
        public string KeyFile
        {
            get
            {
                return (string)base.Bag["KeyFile"];
            }
            set
            {
                base.Bag["KeyFile"] = value;
            }
        }

        /// <summary>Gets or sets the .NET Framework resource files to link to the output file.</summary>
        /// <returns>The .NET Framework resource files to link to the output file.</returns>
        public ITaskItem[] LinkResources
        {
            get
            {
                return (ITaskItem[])base.Bag["LinkResources"];
            }
            set
            {
                base.Bag["LinkResources"] = value;
            }
        }

        /// <summary>Gets or sets the class or module that contains the main entry point.</summary>
        /// <returns>The class or module that contains the main entry point.</returns>
        public string MainEntryPoint
        {
            get
            {
                return (string)base.Bag["MainEntryPoint"];
            }
            set
            {
                base.Bag["MainEntryPoint"] = value;
            }
        }

        /// <summary>Gets or sets a value indicating whether the compiler should use the default response file.</summary>
        /// <returns>true if the compiler is not using the default response file; otherwise, false.</returns>
        public bool NoConfig
        {
            get
            {
                return GetBoolParameterWithDefault("NoConfig", defaultValue: false);
            }
            set
            {
                base.Bag["NoConfig"] = value;
            }
        }

        /// <summary>Gets or sets a value indicating whether to suppress the compiler banner information.</summary>
        /// <returns>true to suppress the compiler banner information; otherwise, false.</returns>
        public bool NoLogo
        {
            get
            {
                return GetBoolParameterWithDefault("NoLogo", defaultValue: false);
            }
            set
            {
                base.Bag["NoLogo"] = value;
            }
        }

        /// <summary>Gets or sets a Boolean value that specifies whether an external UAC manifest is generated for the application.</summary>
        /// <returns>true if an external UAC manifest is generated for the application; otherwise, false.</returns>
        public bool NoWin32Manifest
        {
            get
            {
                return GetBoolParameterWithDefault("NoWin32Manifest", defaultValue: false);
            }
            set
            {
                base.Bag["NoWin32Manifest"] = value;
            }
        }

        /// <summary>Gets or sets a value indicating whether to enable compiler optimizations.</summary>
        /// <returns>true to enable compiler optimizations; otherwise, false.</returns>
        public bool Optimize
        {
            get
            {
                return GetBoolParameterWithDefault("Optimize", defaultValue: false);
            }
            set
            {
                base.Bag["Optimize"] = value;
            }
        }

        /// <summary>Gets or sets the name of the output file.</summary>
        /// <returns>The name of the output file.</returns>
        [Output]
        public ITaskItem OutputAssembly
        {
            get
            {
                return (ITaskItem)base.Bag["OutputAssembly"];
            }
            set
            {
                base.Bag["OutputAssembly"] = value;
            }
        }

        public string Platform
        {
            get
            {
                return (string)base.Bag["Platform"];
            }
            set
            {
                base.Bag["Platform"] = value;
            }
        }

        public bool Prefer32Bit
        {
            get
            {
                return GetBoolParameterWithDefault("Prefer32Bit", defaultValue: false);
            }
            set
            {
                base.Bag["Prefer32Bit"] = value;
            }
        }

        /// <summary>Gets or sets the items from which the compiler will import public type information.</summary>
        /// <returns>The items from which the compiler will import public type information.</returns>
        public ITaskItem[] References
        {
            get
            {
                return (ITaskItem[])base.Bag["References"];
            }
            set
            {
                base.Bag["References"] = value;
            }
        }

        /// <summary>Gets or sets the .NET Framework resources to embed in the output file.</summary>
        /// <returns>The .NET Framework resources to embed in the output file.</returns>
        public ITaskItem[] Resources
        {
            get
            {
                return (ITaskItem[])base.Bag["Resources"];
            }
            set
            {
                base.Bag["Resources"] = value;
            }
        }

        /// <summary>Gets or sets the response files that contain commands for the task.</summary>
        /// <returns>The response files that contain commands for the task.</returns>
        public ITaskItem[] ResponseFiles
        {
            get
            {
                return (ITaskItem[])base.Bag["ResponseFiles"];
            }
            set
            {
                base.Bag["ResponseFiles"] = value;
            }
        }

        /// <summary>Gets or sets the source files to compile.</summary>
        /// <returns>The source files to compile.</returns>
        public ITaskItem[] Sources
        {
            get
            {
                return (ITaskItem[])base.Bag["Sources"];
            }
            set
            {
                base.Bag["Sources"] = value;
            }
        }

        public string SubsystemVersion
        {
            get
            {
                return (string)base.Bag["SubsystemVersion"];
            }
            set
            {
                base.Bag["SubsystemVersion"] = value;
            }
        }

        /// <summary>Gets or sets the file format of the output file.</summary>
        /// <returns>The file format of the output file.</returns>
        public string TargetType
        {
            get
            {
                return (string)base.Bag["TargetType"];
            }
            set
            {
                base.Bag["TargetType"] = value.ToLower(CultureInfo.InvariantCulture);
            }
        }

        /// <summary>Gets or sets a value indicating whether warnings are treated as errors.</summary>
        /// <returns>true if warnings are treated as errors; otherwise, false.</returns>
        public bool TreatWarningsAsErrors
        {
            get
            {
                return GetBoolParameterWithDefault("TreatWarningsAsErrors", defaultValue: false);
            }
            set
            {
                base.Bag["TreatWarningsAsErrors"] = value;
            }
        }

        /// <summary>Gets or sets a value indicating whether compiler output is logged using UTF-8 encoding.</summary>
        /// <returns>true if compiler output is logged using UTF-8 encoding; otherwise, false.</returns>
        public bool Utf8Output
        {
            get
            {
                return GetBoolParameterWithDefault("Utf8Output", defaultValue: false);
            }
            set
            {
                base.Bag["Utf8Output"] = value;
            }
        }

        /// <summary>Gets or sets the icon file name.</summary>
        /// <returns>The icon file name.</returns>
        public string Win32Icon
        {
            get
            {
                return (string)base.Bag["Win32Icon"];
            }
            set
            {
                base.Bag["Win32Icon"] = value;
            }
        }

        /// <summary>Gets or sets the Win32 manifest.</summary>
        /// <returns>The Win32 manifest.</returns>
        public string Win32Manifest
        {
            get
            {
                return (string)base.Bag["Win32Manifest"];
            }
            set
            {
                base.Bag["Win32Manifest"] = value;
            }
        }

        /// <summary>Gets or sets a Win32 resource (.res) file to insert in the output file.</summary>
        /// <returns>The Win32 resource (.res) file to insert in the output file.</returns>
        public string Win32Resource
        {
            get
            {
                return (string)base.Bag["Win32Resource"];
            }
            set
            {
                base.Bag["Win32Resource"] = value;
            }
        }

    

        /// <summary>Gets or sets the encoding of the captured task standard output stream.</summary>
        /// <returns>The encoding of the captured task standard output stream.</returns>
        protected override Encoding StandardOutputEncoding
        {
            get
            {
                if (!Utf8Output)
                {
                    return base.StandardOutputEncoding;
                }
                return Encoding.UTF8;
            }
        }

        /// <summary>Whether the command line compiler was invoked, instead of the host object compiler.</summary>
        protected bool UsedCommandLineTool
        {
            get;
            set;
        }

        /// <summary>Gets or sets a value indicating wheter the host compilter supports all task parameters.</summary>
        /// <returns>true if the host compiler supports all task parameters; otherwise, false.</returns>
        protected bool HostCompilerSupportsAllParameters
        {
            get
            {
                return hostCompilerSupportsAllParameters;
            }
            set
            {
                hostCompilerSupportsAllParameters = value;
            }
        }

        /// <summary>Fills the specified <paramref name="commandLine" /> parameter with the switches and other information that can go into a response file.</summary>
        /// <param name="commandLine">Command line builder to add arguments to.</param>
        protected internal override void AddResponseFileCommands(CommandLineBuilderExtension commandLine)
        {
            if (OutputAssembly == null && Sources != null && Sources.Length != 0 && ResponseFiles == null)
            {
                try
                {
                    OutputAssembly = new TaskItem(Path.GetFileNameWithoutExtension(Sources[0].ItemSpec));
                }
                catch (ArgumentException ex)
                {
                    throw new ArgumentException(ex.Message, "Sources");
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
            commandLine.AppendSwitchWithInteger("/codepage:", base.Bag, "CodePage");
            ConfigureDebugProperties();
            commandLine.AppendPlusOrMinusSwitch("/debug", base.Bag, "EmitDebugInformation");
            commandLine.AppendSwitchIfNotNull("/debug:", DebugType);
            commandLine.AppendPlusOrMinusSwitch("/delaysign", base.Bag, "DelaySign");
            commandLine.AppendSwitchWithInteger("/filealign:", base.Bag, "FileAlignment");
            commandLine.AppendSwitchIfNotNull("/keycontainer:", KeyContainer);
            commandLine.AppendSwitchIfNotNull("/keyfile:", KeyFile);
            commandLine.AppendSwitchIfNotNull("/linkresource:", LinkResources, new string[2]
            {
            "LogicalName",
            "Access"
            });
            commandLine.AppendWhenTrue("/nologo", base.Bag, "NoLogo");
            commandLine.AppendWhenTrue("/nowin32manifest", base.Bag, "NoWin32Manifest");
            commandLine.AppendPlusOrMinusSwitch("/optimize", base.Bag, "Optimize");
            commandLine.AppendSwitchIfNotNull("/out:", OutputAssembly);
            commandLine.AppendSwitchIfNotNull("/subsystemversion:", SubsystemVersion);
            commandLine.AppendSwitchIfNotNull("/resource:", Resources, new string[2]
            {
            "LogicalName",
            "Access"
            });
            commandLine.AppendSwitchIfNotNull("/target:", TargetType);
            commandLine.AppendPlusOrMinusSwitch("/warnaserror", base.Bag, "TreatWarningsAsErrors");
            commandLine.AppendWhenTrue("/utf8output", base.Bag, "Utf8Output");
            commandLine.AppendSwitchIfNotNull("/win32icon:", Win32Icon);
            commandLine.AppendSwitchIfNotNull("/win32manifest:", Win32Manifest);
            commandLine.AppendFileNamesIfNotNull(Sources, " ");
        }

        private void ConfigureDebugProperties()
        {
            if (base.Bag["DebugType"] != null && string.Compare((string)base.Bag["DebugType"], "none", StringComparison.OrdinalIgnoreCase) == 0)
            {
                base.Bag["DebugType"] = null;
                base.Bag["EmitDebugInformation"] = false;
            }
        }

        /// <summary>Generates command line arguments that the command line tool must run directly from the command line and not from a response file.</summary>
        /// <param name="commandLine">Command line builder to add arguments to.</param>
        protected internal override void AddCommandLineCommands(CommandLineBuilderExtension commandLine)
        {
            commandLine.AppendWhenTrue("/noconfig", base.Bag, "NoConfig");
        }

        /// <summary>If an alternate tool name or tool path was specified in the project file, then that tool is used rather than the host compiler for integrated development environment (IDE) builds.</summary>
        /// <returns>false if the host compiler should be used; otherwise, true.</returns>
        protected internal virtual bool UseAlternateCommandLineToolToExecute()
        {
            if (string.IsNullOrEmpty(base.ToolPath))
            {
                return !string.Equals(ToolName, ToolExe, StringComparison.OrdinalIgnoreCase);
            }
            return true;
        }

        /// <summary>Validates the task parameters.</summary>
        /// <returns>true if all parameters are valid; otherwise, false.</returns>
        protected override bool ValidateParameters()
        {
            if (ListHasNoDuplicateItems(Resources, "Resources", "LogicalName"))
            {
                return ListHasNoDuplicateItems(Sources, "Sources");
            }
            return false;
        }

        /// <summary>Determines if any duplicate items exist in the specified task parameter.</summary>
        /// <returns>true if the item list contains no duplicates; otherwise, false.</returns>
        /// <param name="itemList">The list of items to examine for duplicates.</param>
        /// <param name="parameterName">The name of the parameter that contains the <paramref name="itemList" />.</param>
        protected bool ListHasNoDuplicateItems(ITaskItem[] itemList, string parameterName)
        {
            return ListHasNoDuplicateItems(itemList, parameterName, null);
        }

        private bool ListHasNoDuplicateItems(ITaskItem[] itemList, string parameterName, string disambiguatingMetadataName)
        {
            if (itemList == null || itemList.Length == 0)
            {
                return true;
            }
            Hashtable hashtable = new Hashtable(StringComparer.OrdinalIgnoreCase);
            foreach (ITaskItem taskItem in itemList)
            {
                string text = null;
                if (disambiguatingMetadataName != null)
                {
                    text = taskItem.GetMetadata(disambiguatingMetadataName);
                }
                string key = (disambiguatingMetadataName != null && !string.IsNullOrEmpty(text)) ? (taskItem.ItemSpec + ":" + text) : taskItem.ItemSpec;
                if (hashtable.ContainsKey(key))
                {
                    if (disambiguatingMetadataName == null || string.IsNullOrEmpty(text))
                    {
                        base.Log.LogErrorWithCodeFromResources("General.DuplicateItemsNotSupported", taskItem.ItemSpec, parameterName);
                    }
                    else
                    {
                        base.Log.LogErrorWithCodeFromResources("General.DuplicateItemsNotSupportedWithMetadata", taskItem.ItemSpec, parameterName, text, disambiguatingMetadataName);
                    }
                    return false;
                }
                hashtable[key] = string.Empty;
            }
            return true;
        }

        /// <summary>Handles the return code from the compiler.</summary>
        /// <returns>true if the return code was handled successfully; otherwise, false.</returns>
        protected override bool HandleTaskExecutionErrors()
        {
            if (!base.Log.HasLoggedErrors && UsedCommandLineTool)
            {
                base.HandleTaskExecutionErrors();
            }
            return false;
        }

        /// <summary>Logs a message if the specified parameter is not supported by the host compiler.</summary>
        /// <param name="parameterName">The parameter name to set on the host compiler.</param>
        /// <param name="resultFromHostObjectSetOperation">true if the host compiler supports <paramref name="parameterName" />; otherwise, false</param>
        protected void CheckHostObjectSupport(string parameterName, bool resultFromHostObjectSetOperation)
        {
            if (!resultFromHostObjectSetOperation)
            {
                base.Log.LogMessageFromResources(MessageImportance.Normal, "General.ParameterUnsupportedOnHostCompiler", parameterName);
                hostCompilerSupportsAllParameters = false;
            }
        }

        /// <summary>Verifies that all specified references exist on disk.</summary>
        /// <returns>true if all references exist on disk; otherwise, false.</returns>
        protected bool CheckAllReferencesExistOnDisk()
        {
            if (References == null)
            {
                return true;
            }
            bool result = true;
            ITaskItem[] references = References;
            foreach (ITaskItem taskItem in references)
            {
                if (!File.Exists(taskItem.ItemSpec))
                {
                    result = false;
                    base.Log.LogErrorWithCodeFromResources("General.ReferenceDoesNotExist", taskItem.ItemSpec);
                }
            }
            return result;
        }

        internal string GetWin32ManifestSwitch(bool noDefaultWin32Manifest, string win32Manifest)
        {
            if (!noDefaultWin32Manifest && string.IsNullOrEmpty(win32Manifest) && string.IsNullOrEmpty(Win32Resource) && !string.Equals(TargetType, "library", StringComparison.OrdinalIgnoreCase) && !string.Equals(TargetType, "module", StringComparison.OrdinalIgnoreCase))
            {
                string pathToDotNetFrameworkFile = ToolLocationHelper.GetPathToDotNetFrameworkFile("default.win32manifest", TargetDotNetFrameworkVersion.Version45);
                if (pathToDotNetFrameworkFile == null)
                {
                    base.Log.LogMessageFromResources("General.ExpectedFileMissing", "default.win32manifest");
                }
                return pathToDotNetFrameworkFile;
            }
            return win32Manifest;
        }
    }

    public abstract class ToolTaskExtension : ToolTask
    {
        private Hashtable bag = new Hashtable();
        protected internal Hashtable Bag => bag;
        protected internal bool GetBoolParameterWithDefault(string parameterName, bool defaultValue)
        {
            object obj = bag[parameterName];
            if (obj != null)
            {
                return (bool)obj;
            }
            return defaultValue;
        }
        protected internal int GetIntParameterWithDefault(string parameterName, int defaultValue)
        {
            object obj = bag[parameterName];
            if (obj != null)
            {
                return (int)obj;
            }
            return defaultValue;
        }
        protected internal virtual void AddCommandLineCommands(CommandLineBuilderExtension commandLine)
        {
        }
        protected internal virtual void AddResponseFileCommands(CommandLineBuilderExtension commandLine)
        {
        }

    }
}