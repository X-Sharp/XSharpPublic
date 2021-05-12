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
    using Microsoft.VisualStudio.Shell;
    using VSLangProj;

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

        internal const string descSpecificWarnings = "Specific Warnings To Treat As Errors";



        #endregion


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
            this.chkUseSharedCompilation.Tag = XSharpProjectFileConstants.UseSharedCompilation;
            this.toolTip1.SetToolTip(this.chkUseSharedCompilation, descUseSharedCompilation);

            this.chkPrefer32Bit.Text = captPrefer32Bit;
            this.chkPrefer32Bit.Tag = XSharpProjectFileConstants.Prefer32Bit;
            this.toolTip1.SetToolTip(this.chkPrefer32Bit, descPrefer32Bit);

            this.chkRegisterForComInterop.Text = captRegisterForComInterop;
            this.chkRegisterForComInterop.Tag = XSharpProjectFileConstants.RegisterForComInterop;
            this.toolTip1.SetToolTip(this.chkRegisterForComInterop, descRegisterForComInterop);

            this.chkXMLDocumentationFile.Text = captDocumentationFile;
            this.txtXMLDocumentationFile.Tag = XSharpProjectFileConstants.DocumentationFile;
            this.toolTip1.SetToolTip(chkXMLDocumentationFile, descDocumentationFile);
            this.toolTip1.SetToolTip(txtXMLDocumentationFile, descDocumentationFileName);

            this.chkOptimize.Text =captOptimize;
            this.chkOptimize.Tag = XSharpProjectFileConstants.Optimize;
            this.toolTip1.SetToolTip(this.chkOptimize, descOptimize);

            this.chkSignAssembly.Text = captSignAssembly;
            this.chkSignAssembly.Tag = XSharpProjectFileConstants.SignAssembly;
            this.toolTip1.SetToolTip(this.chkSignAssembly, descSignAssembly);

            this.chkSuppressRCWarnings.Text = SuppressRCWarningsCaption;
            this.chkSuppressRCWarnings.Tag = XSharpProjectFileConstants.SuppressRCWarnings;
            this.toolTip1.SetToolTip(this.chkSuppressRCWarnings, SuppressRCWarningsDescription);

            this.chkDelaySign.Text = captDelaySign;
            this.chkDelaySign.Tag = XSharpProjectFileConstants.DelaySign;
            this.toolTip1.SetToolTip(this.chkDelaySign, descDelaySign);

            this.txtDefineConstants.Tag = XSharpProjectFileConstants.DefineConstants;
            this.lblDefineConstants.Text = DefCaption;
            this.toolTip1.SetToolTip(this.txtDefineConstants, DefDescription);
            this.toolTip1.SetToolTip(this.lblDefineConstants, DefDescription);

            this.txtCommandLineOption.Tag = XSharpProjectFileConstants.CommandLineOption;
            this.lblCommandLineOption.Text = CmdLineCaption;
            this.toolTip1.SetToolTip(txtCommandLineOption, CmdLineDescription);
            this.toolTip1.SetToolTip(lblCommandLineOption, CmdLineDescription);

            this.txtDisabledWarnings.Tag = XSharpProjectFileConstants.DisabledWarnings;
            this.lblDisabledWarnings.Text = captDisabledWarnings;
            this.toolTip1.SetToolTip(lblDisabledWarnings, descDisabledWarnings);
            this.toolTip1.SetToolTip(txtDisabledWarnings, descDisabledWarnings);

            this.txtOutputPath.Tag = XSharpProjectFileConstants.OutputPath;
            this.lblOutputPath.Text = captOutputPath;
            this.toolTip1.SetToolTip(txtOutputPath, descOutputPath);
            this.toolTip1.SetToolTip(lblOutputPath, descOutputPath);


            this.txtIntermediateOutputPath.Tag = XSharpProjectFileConstants.IntermediateOutputPath;
            this.lblIntermediateOutputPath.Text = captIntermediateOutputPath;
            this.toolTip1.SetToolTip(txtIntermediateOutputPath, descIntermediateOutputPath);
            this.toolTip1.SetToolTip(lblIntermediateOutputPath, descIntermediateOutputPath);

            this.txtAssemblyOriginatorKeyFile.Tag = XSharpProjectFileConstants.AssemblyOriginatorKeyFile;
            this.lblAssemblyOriginatorKeyFile.Text = captAssemblyOriginatorKeyFile;
            this.toolTip1.SetToolTip(txtAssemblyOriginatorKeyFile, descAssemblyOriginatorKeyFile);
            this.toolTip1.SetToolTip(lblAssemblyOriginatorKeyFile, descAssemblyOriginatorKeyFile);

            this.lblPlatformTarget.Text = captPlatFormTarget;
            this.comboPlatformTarget.Tag = XSharpProjectFileConstants.PlatformTarget;
            this.toolTip1.SetToolTip(lblPlatformTarget, descPlatFormTarget);
            this.toolTip1.SetToolTip(comboPlatformTarget, descPlatFormTarget);

            this.lblWarningLevel.Text = captWarningLevel;
            this.cboWarningLevel.Tag = XSharpProjectFileConstants.WarningLevel;
            this.toolTip1.SetToolTip(lblWarningLevel, descWarningLevel);
            this.toolTip1.SetToolTip(cboWarningLevel, descWarningLevel);

            this.rbWarningAll.Tag = XSharpProjectFileConstants.TreatWarningsAsErrors + "|True";
            this.rbWarningNone.Tag = XSharpProjectFileConstants.TreatWarningsAsErrors + "|False";
            this.rbWarningSpecific.Tag = XSharpProjectFileConstants.TreatWarningsAsErrors + "|False";
            this.txtSpecificWarnings.Tag = XSharpProjectFileConstants.WarningsAsErrors;
            this.toolTip1.SetToolTip(txtSpecificWarnings, descSpecificWarnings);

            FillCombo(new PlatformConverter(), comboPlatformTarget);

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

        private void btnOutputPathBrowse_Click(object sender, EventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            showMacroDialog(txtOutputPath, descOutputPath);
        }

        private void btnIntermediateOutputPath_Click(object sender, EventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            showMacroDialog(txtIntermediateOutputPath, descIntermediateOutputPath);
        }
        internal void Project_OnProjectPropertyChanged(object sender, ProjectPropertyChangedArgs e)
        {
            if (e.OldValue != e.NewValue)
            {
                if (string.Compare(e.PropertyName, XSharpProjectFileConstants.PlatformTarget, true) == 0)
                {
                    chkPrefer32Bit.Enabled = e.NewValue.ToLower() == "anycpu";
                }
            }
        }

        private void chkXMLDocumentationFile_CheckedChanged(object sender, EventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            string documentationFile = "";
            if (chkXMLDocumentationFile.Checked)
            {
                var asmName = this.ParentPropertyPage.GetProperty(XSharpProjectFileConstants.AssemblyName) ?? "NoName";
                documentationFile = asmName + ".Xml";
            }
            var tag = "DocFile"; ;
            this.ParentPropertyPage.SetProperty(tag, documentationFile);
            this.txtXMLDocumentationFile.Text = documentationFile;
        }

        protected internal override void BindProperties()
        {
            base.BindProperties();
            this.chkXMLDocumentationFile.Checked = !string.IsNullOrEmpty(ParentPropertyPage.GetProperty("DocFile"));
            var platform = ParentPropertyPage.GetProperty(XSharpProjectFileConstants.PlatformTarget) ?? "anycpu";
            if (string.Compare(platform, "anycpu",true) == 0)
            {
                this.chkPrefer32Bit.Enabled = true;
            }
            else
            {
                this.chkPrefer32Bit.Enabled = false;
                this.chkPrefer32Bit.Checked = false;
            }
            if (! string.IsNullOrEmpty(txtSpecificWarnings.Text))
            {
                rbWarningSpecific.Checked = true;
                rbWarningAll.Checked = false;
                rbWarningNone.Checked = false;
                txtSpecificWarnings.Enabled = true;
            }
            else
            {
                var warn = ParentPropertyPage.GetProperty(XSharpProjectFileConstants.TreatWarningsAsErrors) ?? "false";
                warn = warn.ToLower();
                rbWarningSpecific.Checked = false;
                rbWarningAll.Checked = warn == "true";
                rbWarningNone.Checked = warn != "true";
                txtSpecificWarnings.Enabled = false;
            }
        }

        protected override void HandleControlValidated(object sender, EventArgs e)
        {
            base.HandleControlValidated(sender, e);
            if (ParentPropertyPage.IsActive)
            {
                if (sender is RadioButton button && button.Checked)
                {
                    // clear the specific warnings in the parent
                    var tag = (string)txtSpecificWarnings.Tag;
                    if (sender == rbWarningAll)
                    {
                        this.ParentPropertyPage.SetProperty(tag, " ");
                    }
                    else if (sender == rbWarningNone)
                    {
                        this.ParentPropertyPage.SetProperty(tag, " ");
                    }
                    else
                    {
                        this.ParentPropertyPage.SetProperty(tag, txtSpecificWarnings.Text);

                    }
                }
            }
        }

        private void btnKeyFile_Click(object sender, EventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            showMacroDialog(txtAssemblyOriginatorKeyFile, descAssemblyOriginatorKeyFile,
                "Key Files (*.snk; *.pfx)|*.snk;*.pfx|All files (*.*)|*.*");

        }

        private void enableControls()
        {
            if (ParentPropertyPage.IsActive)
            {
                txtSpecificWarnings.Enabled = rbWarningSpecific.Checked;
            }

        }
        private void rbWarningSpecific_CheckedChanged(object sender, EventArgs e)
        {
            enableControls();
        }

        private void rbWarningNone_CheckedChanged(object sender, EventArgs e)
        {
            enableControls();
        }

        private void rbWarningAll_CheckedChanged(object sender, EventArgs e)
        {
            enableControls();
        }
    }
}
