//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

namespace XSharp.Project
{
    using Microsoft.VisualStudio.PlatformUI;
    using System;
    using System.Diagnostics;
    using System.Drawing;
    using System.Text;
    using System.Collections.Generic;
    using System.Windows.Forms;
    using System.ComponentModel;
    using Microsoft.VisualStudio.Project;
    /// <summary>
    /// Property page contents for the Candle Settings page.
    /// </summary>
    internal partial class XBuildPropertyPagePanel : XPropertyPagePanel
    {
        #region Constants
        internal const string catSigning = "Code Signing";
        internal const string catMisc = "Miscellaneous";
        internal const string catWarnings = "Warnings";
        internal const string catOutput = "\tOutput";
        internal const string CatPreprocessor = "Preprocessor";
        internal const string catXML = "XML Output";
        internal const string captOutputPath = "Output Path";
        internal const string descOutputPath = "Output Path (macros are allowed)";
        internal const string captIntermediateOutputPath = "Intermediate Output Path";
        internal const string descIntermediateOutputPath = "Intermediate Output Path  (macros are allowed)";
        internal const string captDocumentationFile = "Generate XML doc comments file";
        internal const string descDocumentationFile = "Generate XML doc comments file";
        internal const string captDocumentationFileName = "XML doc comments file name";
        internal const string descDocumentationFileName = "XML doc comments file name";
        internal const string captOptimize = "Optimize";
        internal const string descOptimize = "Should compiler optimize output? (/optimize)";
        internal const string captUseSharedCompilation = "Use Shared Compiler";
        internal const string descUseSharedCompilation = "Should the shared compiler be used to compile the project? (Faster, but may hide some compiler errors) (/shared)";
        internal const string captDisabledWarnings = "Suppress Specific Warnings";
        internal const string descDisabledWarnings = "Specify a list of warnings to suppress (/nowarn)";
        internal const string captWarningLevel = "Warning Level";
        internal const string descWarningLevel = "Set the warning level to a value between 0 and 4 (/warn)";
        internal const string captTreatWarningsAsErrors = "Warnings As Errors";
        internal const string descTreatWarningsAsErrors = "Treat warnings as errors (/warnaserror)";
        internal const string captSignAssembly = "Sign the output assembly";
        internal const string descSignAssembly = "Sign the assembly  (/keyfile)";
        internal const string captDelaySign = "Delayed sign only";
        internal const string descDelaySign = "Delayed signing (/delaysign)";
        internal const string captAssemblyOriginatorKeyFile = "Code Signing KeyFile";
        internal const string descAssemblyOriginatorKeyFile = "Choose a code signing key file (/keyfile)";
        internal const string captRegisterForComInterop = "Register for COM Interop";
        internal const string descRegisterForComInterop = "Register the output assembly for COM Interop (requires administrator rights)";

        internal const string PPOCaption = "Generate preprocessor output";
        internal const string PPODescription = "Save the output from the preprocessor to .ppo files  (/ppo)";
        internal const string CmdLineCaption = "Extra Command Line Options";
        internal const string CmdLineDescription = "User-Defined Command Line options";
        internal const string DefCaption = "Defines for the preprocessor";
        internal const string DefDescription = "Defines for the preprocessor (/define)";
        internal const string captPrefer32Bit = "\tPrefer 32 Bit";
        internal const string descPrefer32Bit = "Prefer 32 bit when AnyCpu platform is selected. (/platform)";
        internal const string SuppressRCWarningsCaption = "Suppress Resource Compiler warnings";
        internal const string SuppressRCWarningsDescription = "Suppress warnings from the Native Resource Compiler about duplicate defines (RC4005)";
        internal const string captPlatFormTarget= "Platform Target";
        internal const string descPlatFormTarget = "Select the platform target when compiling this project. This should be AnyCPU, X86, x64,Arm or Itanium (/platform)";

        internal const string defaultOutputPath = @"bin\$(Configuration)\";
        internal const string defaultIntermediatePath = @"obj\$(Configuration)\";
        #endregion
        private void FillCombo(TypeConverter converter, System.Windows.Forms.ComboBox combo)
        {
            foreach (var enumvalue in converter.GetStandardValues(null))
            {
                var name = converter.ConvertTo(enumvalue, typeof(System.String));
                combo.Items.Add(name); // new comboItem ((int) enumvalue, name));
            }
        }


        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildEventsPropertyPagePanel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The parent property page to which this is bound.</param>
        public XBuildPropertyPagePanel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
            this.InitializeComponent();

            this.chkPPO.Text = PPOCaption;
            this.chkPPO.Tag = "PPO";
            this.toolTip1.SetToolTip(this.chkPPO, PPODescription);

            this.chkUseSharedCompilation.Text = captUseSharedCompilation;
            this.chkUseSharedCompilation.Tag = "UseSharedCompilation";
            this.toolTip1.SetToolTip(this.chkUseSharedCompilation, descUseSharedCompilation);

            this.chkPrefer32Bit.Text = captPrefer32Bit;
            this.chkPrefer32Bit.Tag = "Prefer32Bit";
            this.toolTip1.SetToolTip(this.chkPrefer32Bit, descPrefer32Bit);

            this.chkRegisterForComInterop.Text = captRegisterForComInterop;
            this.chkRegisterForComInterop.Tag = "RegisterForComInterop";
            this.toolTip1.SetToolTip(this.chkRegisterForComInterop, descRegisterForComInterop);

            this.chkXMLDocumentationFile.Text = captDocumentationFile;
            this.txtXMLDocumentationFile.Tag = "DocFile";
            this.toolTip1.SetToolTip(chkXMLDocumentationFile, descDocumentationFile);
            this.toolTip1.SetToolTip(txtXMLDocumentationFile, descDocumentationFileName);

            this.chkOptimize.Text = captOptimize;
            this.chkOptimize.Tag = "Optimize";
            this.toolTip1.SetToolTip(this.chkOptimize, descOptimize);

            this.chkSignAssembly.Text = captSignAssembly;
            this.chkSignAssembly.Tag = "SignAssembly";
            this.toolTip1.SetToolTip(this.chkSignAssembly, descSignAssembly);

            this.chkSuppressRCWarnings.Text = SuppressRCWarningsCaption;
            this.chkSuppressRCWarnings.Tag = "SuppressRCWarnings";
            this.toolTip1.SetToolTip(this.chkSuppressRCWarnings, SuppressRCWarningsDescription);

            this.chkDelaySign.Text = captDelaySign;
            this.chkDelaySign.Tag = "DelaySign";
            this.toolTip1.SetToolTip(this.chkDelaySign, descDelaySign);

            this.txtDefineConstants.Tag = "DefineConstants";
            this.lblDefineConstants.Text = DefCaption;
            this.toolTip1.SetToolTip(this.txtDefineConstants, DefDescription);
            this.toolTip1.SetToolTip(this.lblDefineConstants, DefDescription);

            this.txtCommandLineOption.Tag = "CommandLineOption";
            this.lblCommandLineOption.Text = CmdLineCaption;
            this.toolTip1.SetToolTip(txtCommandLineOption, CmdLineDescription);
            this.toolTip1.SetToolTip(lblCommandLineOption, CmdLineDescription);

            this.txtDisabledWarnings.Tag = "DisabledWarnings";
            this.lblDisabledWarnings.Text = captDisabledWarnings;
            this.toolTip1.SetToolTip(lblDisabledWarnings, descDisabledWarnings);
            this.toolTip1.SetToolTip(txtDisabledWarnings, descDisabledWarnings);

            this.txtOutputPath.Tag = "OutputPath";
            this.lblOutputPath.Text = captOutputPath;
            this.toolTip1.SetToolTip(txtOutputPath, descOutputPath);
            this.toolTip1.SetToolTip(lblOutputPath, descOutputPath);


            this.txtIntermediateOutputPath.Tag = "IntermediateOutputPath";
            this.lblIntermediateOutputPath.Text = captIntermediateOutputPath;
            this.toolTip1.SetToolTip(txtIntermediateOutputPath, descIntermediateOutputPath);
            this.toolTip1.SetToolTip(lblIntermediateOutputPath, descIntermediateOutputPath);

            this.txtAssemblyOriginatorKeyFile.Tag = "AssemblyOriginatorKeyFile";
            this.lblAssemblyOriginatorKeyFile.Text = captAssemblyOriginatorKeyFile;
            this.toolTip1.SetToolTip(txtAssemblyOriginatorKeyFile, descAssemblyOriginatorKeyFile);
            this.toolTip1.SetToolTip(lblAssemblyOriginatorKeyFile, descAssemblyOriginatorKeyFile);

            this.lblPlatformTarget.Text = captPlatFormTarget;
            this.cboPlatformTarget.Tag = "PlatformTarget";
            this.toolTip1.SetToolTip(lblPlatformTarget, descPlatFormTarget);
            this.toolTip1.SetToolTip(cboPlatformTarget, descPlatFormTarget);

            this.lblWarningLevel.Text = captWarningLevel;
            this.cboWarningLevel.Tag = "WarningLevel";
            this.toolTip1.SetToolTip(lblWarningLevel, descWarningLevel);
            this.toolTip1.SetToolTip(cboWarningLevel, descWarningLevel);

            this.txtSpecificWarnings.Tag = "SpecificWarnings";
            this.toolTip1.SetToolTip(txtSpecificWarnings, descPlatFormTarget);


            // hook up the form to both editors
            Color defaultBackground = SystemColors.ButtonFace;
            Color defaultForeground = SystemColors.WindowText;
            UpdateWindowColors(this, defaultBackground, defaultForeground);
        }
        /// <summary>
        /// Adjust the  color values. Adjusts the text color and text
        /// area background color
        /// </summary>
        /// <param name="clrBackground">The desired color for the background of the text area</param>
        /// <param name="clrForeground">The desired text color</param>
        static void UpdateWindowColors(Control control, Color clrBackground, Color clrForeground)
        {
            // Update the window background
            if (control is TextBox)
                control.BackColor = Color.White;
            else
                control.BackColor = clrBackground;
            control.ForeColor = clrForeground;

            // Also update the label
            foreach (Control child in control.Controls)
            {
                UpdateWindowColors(child, clrBackground, clrForeground);
            }
        }
      

    }
}
