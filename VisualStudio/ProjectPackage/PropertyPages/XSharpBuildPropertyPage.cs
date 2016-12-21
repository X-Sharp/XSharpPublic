//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Runtime.Versioning;
using System.Windows.Forms;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Project;
using EnvDTE;
using EnvDTE80;
using System.ComponentModel;
using System.Globalization;

namespace XSharp.Project
{
    /// <summary>
    /// This class implements general property page for the project type.
    /// </summary>
    /// 
    [ComVisible(true)]
    [Guid("E994C210-9D6D-4CF4-A061-EBBEA2BC626B")]
    [ClassInterface(ClassInterfaceType.AutoDual)]
    public class XSharpBuildPropertyPage : XSharpSettingsPage
    {
        internal const string catEvents = "Build Events";
        internal const string catSigning = "Code Signing";
        internal const string catMisc = "Miscellaneous";
        internal const string catWarnings = "Warnings";
        internal const string catOutput = "Output";
        internal const string catAdvanced = "Advanced";
        internal const string CatPreprocessor = "Preprocessor";
        internal const string captOutputPath = "Output Path";
        internal const string descOutputPath = "Output Path (macros are allowed)";
        internal const string captIntermediateOutputPath = "Intermediate Output Path";
        internal const string descIntermediateOutputPath = "Intermediate Output Path  (macros are allowed)";
        internal const string captDocumentationFile = "Generate XML doc comments file";
        internal const string descDocumentationFile = "Generate XML doc comments file";
        internal const string captOptimize = "Optimize";
        internal const string descOptimize = "Should compiler optimize output?";
        internal const string captUseSharedCompilation = "Use Shared Compiler";
        internal const string descUseSharedCompilation = "Should the shared compiler be used to compile the project? (Faster, but may hide some compiler errors)";
        internal const string captPreBuildEvent = "Pre Build Event";
        internal const string descPreBuildEvent = "Pre Build Event Command Line (may use macros)";
        internal const string captPostBuildEvent = "Post Build Event";
        internal const string descPostBuildEvent = "Post Build Event Command Line (may use macros)";
        internal const string captRunPostBuildEvent = "Run the Post Build Event";
        internal const string descRunPostBuildEvent = "When to run the Post Build Event";
        internal const string captDisabledWarnings = "Suppress Warnings";
        internal const string descDisabledWarnings = "Specify a list of warnings to suppress (/nowarn)";
        internal const string captWarningLevel = "Warning Level";
        internal const string descWarningLevel = "Set the warning level to a value between 0 and 4 (/warn)";
        internal const string captTreatWarningsAsErrors = "Warnings As Errors";
        internal const string descTreatWarningsAsErrors = "Treat warnings as errors (/warnaserror)";
        internal const string captSignAssembly = "Sign the output assembly";
        internal const string descSignAssembly = "Sign the assembly  (/sign)";
        internal const string captDelaySign = "Delayed sign only";
        internal const string descDelaySign = "Delayed signing (/delaysign)";
        internal const string captAssemblyOriginatorKeyFile = "Code Signing KeyFile";
        internal const string descAssemblyOriginatorKeyFile = "Choose a code signing key file (/keyfile)";

        internal const string PPOCaption = "Generate preprocessor output";
        internal const string PPODescription = "Save the output from the preprocessor to .ppo files  (/ppo)";
        internal const string NoStdDefCaption = "Suppress standard header file";
        internal const string NoStdDefDescription = "Suppress inclusion of the standard header file in every file (/nostddef)";
        internal const string CmdLineCaption = "Extra Command Line Options";
        internal const string CmdLineDescription = "User-Defined Command Line options";
        internal const string DefCaption = "Defines for the preprocessor";
        internal const string DefDescription = "Defines for the preprocessor (/d)";
        internal const string INCCaption = "Additional Include paths";
        internal const string INCDescription = "Additional include paths for the preprocessor (it also looks through the folders set with the include environment variable) (/i)";


        internal const string defaultOutputPath = @"bin\$(Configuration)\";
        internal const string defaultIntermediatePath = @"obj\$(Configuration)\";

        #region Fields
        private bool warningAsErrors;
        private int warningLevel;
        private bool optimize;
        private bool signassembly;
        private bool delaysign;
        private string assemblyoriginatorkeyfile;
        private string disabledwarnings;
        private string prebuildevent;
        private string postbuildevent;
        private RunPostBuildEvent runpostbuildevent;
        private bool documentationfile;
        private string outputpath;
        private string intermediateoutputpath;
        private bool usesharedcompilation;
        private bool ppo;
        private string includepaths;
        private string defines;
        private bool nostandarddefs;
        private string commandlineoption;

        #endregion Fields


        #region Validation
        private int ValidateWarningLevel (int level)
        {
            if (level < 0)
                level = 0;
            else if (level > 4)
                level = 4;
            return level;

        }
        #endregion

        #region Constructors
        /// <summary>
        /// Explicitly defined default constructor.
        /// </summary>
        public XSharpBuildPropertyPage()
        {
            this.Name = "Build";
        }
        #endregion


        [Category(catAdvanced), DisplayName(CmdLineCaption), Description(CmdLineDescription)]
        public string CommandLineOption
        {
            get { return this.commandlineoption; }
            set { this.commandlineoption = value; this.IsDirty = true; }
        }


        [Category(catOutput)]
        [DisplayName(captOutputPath)]
        [Description(descOutputPath)]
        [Editor(typeof(XSharpSLEPropertyEditor), typeof(System.Drawing.Design.UITypeEditor))]
        public string OutputPath
        {
            get {
                if (String.IsNullOrEmpty(outputpath))
                    outputpath = defaultOutputPath;
                return this.outputpath; }
            set{
                if (String.IsNullOrEmpty(value))
                    value = defaultOutputPath;
                this.outputpath = AddSlash(value);
                this.IsDirty = true;}
        }
        [Category(catOutput)]
        [DisplayName(captIntermediateOutputPath)]
        [Description(descIntermediateOutputPath)]
        [Editor(typeof(XSharpSLEPropertyEditor), typeof(System.Drawing.Design.UITypeEditor))]
        public string IntermediateOutputPath
        {
            get {
                if (String.IsNullOrEmpty(intermediateoutputpath))
                    intermediateoutputpath = defaultIntermediatePath;
                return this.intermediateoutputpath;
            }
            set {
                if (String.IsNullOrEmpty(value))
                    value = defaultIntermediatePath;
                this.intermediateoutputpath = AddSlash(value);
                this.IsDirty = true; }
        }

        [Category(catOutput)]
        [DisplayName(captDocumentationFile)]
        [Description(descDocumentationFile)]
        public bool DocumentationFile
        {
            get { return this.documentationfile; }
            //set { this.documentationfile = value; this.IsDirty = true; }
        }

        [Category(catMisc)]
        [DisplayName(captOptimize)]
        [Description(descOptimize)]
        public bool Optimize
        {
            get { return this.optimize; }
            set { this.optimize = value; this.IsDirty = true; }
        }

        [Category(catMisc)]
        [DisplayName(captUseSharedCompilation)]
        [Description(descUseSharedCompilation)]
        public bool UseSharedCompilation
        {
            get { return this.usesharedcompilation; }
            set { this.usesharedcompilation = value; this.IsDirty = true; }
        }

        [Category(catEvents)]
        [DisplayName(captPreBuildEvent)]
        [Description(descPreBuildEvent)]
        [Editor(typeof(XSharpMLEPropertyEditor), typeof(System.Drawing.Design.UITypeEditor))]
        public string PreBuildEvent
        {
            get { return this.prebuildevent; }
            set { this.prebuildevent = value; this.IsDirty = true; }
        }

        [Category(catEvents)]
        [DisplayName(captPostBuildEvent)]
        [Description(descPostBuildEvent)]
        [Editor(typeof(XSharpMLEPropertyEditor), typeof(System.Drawing.Design.UITypeEditor))]
        public string PostBuildEvent
        {
            get { return this.postbuildevent; }
            set { this.postbuildevent = value; this.IsDirty = true; }
        }

        [Category(catEvents)]
        [DisplayName(captRunPostBuildEvent)]
        [Description(descRunPostBuildEvent)]
        public RunPostBuildEvent RunPostBuildEvent
        {
            get { return this.runpostbuildevent; }
            set { this.runpostbuildevent = value; this.IsDirty = true; }
        }
        [Category(catWarnings)]
        [DisplayName(captDisabledWarnings)]
        [Description(descDisabledWarnings)]
        public string DisabledWarnings
        {
            get { return this.disabledwarnings; }
            set { this.disabledwarnings = value; this.IsDirty = true; }
        }
        [Category(catWarnings)]
        [DisplayName(captWarningLevel)]
        [Description(descWarningLevel)]
        public int WarningLevel
        {
            get { return this.warningLevel; }
            set { this.warningLevel = ValidateWarningLevel (value); this.IsDirty = true; }
        }


        [Category(catWarnings)]
        [DisplayName(captTreatWarningsAsErrors)]
        [Description(descTreatWarningsAsErrors)]
        public bool TreatWarningsAsErrors
        {
            get { return this.warningAsErrors; }
            set { this.warningAsErrors = value; this.IsDirty = true; }
        }

        [Category(catSigning)]
        [DisplayName(captSignAssembly)]
        [Description(descSignAssembly)]
        public bool SignAssembly
        {
            get { return this.signassembly; }
            set { this.signassembly = value; this.IsDirty = true; }
        }

        [Category(catSigning)]
        [DisplayName(captDelaySign)]
        [Description(descDelaySign)]
        public bool DelaySign
        {
            get { return this.delaysign; }
            set { this.delaysign = value; this.IsDirty = true; }
        }
        [Category(catSigning)]
        [DisplayName(captAssemblyOriginatorKeyFile)]
        [Description(descAssemblyOriginatorKeyFile)]
        [XSharpFileNameEditorAttribute("Select Key File", "Key Files (*.snk; *.pfx)|*.snk;*.pfx|All files (*.*)|*.*", 0)]
        [EditorAttribute(typeof(XSharpFileNameEditor), typeof(System.Drawing.Design.UITypeEditor))]

        public string AssemblyOriginatorKeyFile
        {
            get { return this.assemblyoriginatorkeyfile; }
            set { this.assemblyoriginatorkeyfile = value; this.IsDirty = true; }
        }



        [Category(CatPreprocessor), DisplayName(PPOCaption), Description(PPODescription)]
        public bool PPO
        {
            get { return this.ppo; }
            set { this.ppo = value; this.IsDirty = true; }
        }

        [Category(CatPreprocessor), DisplayName(DefCaption), Description(DefDescription)]
        public string DefineConstants
        {
            get { return this.defines; }
            set { this.defines = value; this.IsDirty = true; }
        }

        [Category(CatPreprocessor), DisplayName(INCCaption), Description(INCDescription)]
        [Editor(typeof(XSharpSLEPropertyEditor), typeof(System.Drawing.Design.UITypeEditor))]
        public string IncludePaths
        {
            get { return this.includepaths; }
            set { this.includepaths = value; this.IsDirty = true; }
        }

        [Category(CatPreprocessor), DisplayName(NoStdDefCaption), Description(NoStdDefDescription)]
        public bool NoStandardDefs
        {
            get { return this.nostandarddefs; }
            set { this.nostandarddefs = value; this.IsDirty = true; }
        }


        #region Overriden Implementation
        /// <summary>
        /// Returns class FullName property value.
        /// </summary>
        public override string GetClassName()
        {
            return this.GetType().FullName;
        }

        /// <summary>
        /// Bind properties.
        /// </summary>
        protected override void BindProperties()
        {
            if (this.ProjectMgr == null)
            {
                return;
            }
            warningAsErrors = getCfgLogic(nameof(TreatWarningsAsErrors), false);
            outputpath = getCfgString(nameof(OutputPath), defaultOutputPath);
            outputpath = AddSlash(outputpath);
            intermediateoutputpath = getCfgString(nameof(IntermediateOutputPath),  defaultIntermediatePath);
            intermediateoutputpath = AddSlash(intermediateoutputpath);
            documentationfile= getCfgLogic(nameof(DocumentationFile), false);
            optimize = getCfgLogic(nameof(Optimize),  false);
            usesharedcompilation = getCfgLogic(nameof(UseSharedCompilation),  true);
            prebuildevent = getCfgString(nameof(PreBuildEvent),"");
            postbuildevent = getCfgString(nameof(PostBuildEvent), "");
            string temp= "";
            temp = getCfgString(nameof(RunPostBuildEvent),  "Always");
            runpostbuildevent = (RunPostBuildEvent) new RunPostBuildEventConverter().ConvertFromString(temp);
            disabledwarnings = getCfgString(nameof(DisabledWarnings),  "");
            warningLevel= getCfgInteger(nameof(WarningLevel), 4);
            warningLevel = ValidateWarningLevel(warningLevel);
            warningAsErrors = getCfgLogic(nameof(TreatWarningsAsErrors), true);
            signassembly = getCfgLogic(nameof(SignAssembly), false);
            delaysign = getCfgLogic(nameof(DelaySign), false);
            assemblyoriginatorkeyfile = getCfgString(nameof(AssemblyOriginatorKeyFile), "");

            commandlineoption= getCfgString(nameof(CommandLineOption),  "");
            ppo = getCfgLogic(nameof(PPO),  false);
            nostandarddefs = getCfgLogic(nameof(NoStandardDefs),  false);
            includepaths = getCfgString(nameof(IncludePaths),  "");
            defines = getCfgString(nameof(DefineConstants), "");
        }

        /// <summary>
        /// Apply Changes on project node.
        /// </summary>
        /// <returns>E_INVALIDARG if internal ProjectMgr is null, otherwise applies changes and return S_OK.</returns>
        protected override int ApplyChanges()
        {
            if (this.ProjectMgr == null)
            {
                return VSConstants.E_INVALIDARG;
            }

            this.SetConfigProperty(nameof(TreatWarningsAsErrors), this.warningAsErrors.ToString().ToLower());
            this.SetConfigProperty(nameof(OutputPath), this.outputpath?.ToString());
            this.SetConfigProperty(nameof(IntermediateOutputPath), this.intermediateoutputpath?.ToString());
            this.SetConfigProperty(nameof(DocumentationFile), this.documentationfile.ToString().ToLower());
            this.SetConfigProperty(nameof(Optimize), this.optimize.ToString().ToLower());
            this.SetConfigProperty(nameof(UseSharedCompilation), this.usesharedcompilation.ToString().ToLower());
            this.SetConfigProperty(nameof(PreBuildEvent), this.prebuildevent?.ToString());
            this.SetConfigProperty(nameof(PostBuildEvent), this.postbuildevent?.ToString());
            this.SetConfigProperty(nameof(RunPostBuildEvent), this.runpostbuildevent.ToString());
            this.SetConfigProperty(nameof(DisabledWarnings), this.disabledwarnings?.ToString());
            this.SetConfigProperty(nameof(WarningLevel), this.warningLevel.ToString().ToLower());
            this.SetConfigProperty(nameof(TreatWarningsAsErrors), this.warningAsErrors.ToString().ToLower());
            this.SetConfigProperty(nameof(SignAssembly), this.signassembly.ToString().ToLower());
            this.SetConfigProperty(nameof(DelaySign), this.delaysign.ToString().ToLower());
            this.SetConfigProperty(nameof(AssemblyOriginatorKeyFile), this.assemblyoriginatorkeyfile?.ToString().ToLower());

            this.SetConfigProperty(nameof(CommandLineOption), this.commandlineoption?.ToString().ToLower());
            this.SetConfigProperty(nameof(PPO), this.ppo.ToString().ToLower());
            this.SetConfigProperty(nameof(NoStandardDefs), this.nostandarddefs.ToString().ToLower());
            this.SetConfigProperty(nameof(IncludePaths), this.includepaths?.ToString());
            this.SetConfigProperty(nameof(DefineConstants), this.defines?.ToString());

            this.IsDirty = false;

            return VSConstants.S_OK;
        }

        #endregion
    }
}


