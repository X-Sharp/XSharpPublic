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
    using XSharpModel;

    /// <summary>
    /// Property page contents for the Candle Settings page.
    /// </summary>
    internal partial class XBuildPropertyPagePanel : XPropertyPagePanel
    {


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

            this.chkPPO.Text = BuildPropertyPagePanel.PPOCaption;
            this.chkPPO.Tag = "PPO";
            this.toolTip1.SetToolTip(this.chkPPO, BuildPropertyPagePanel.PPODescription);

            this.chkUseSharedCompilation.Text = BuildPropertyPagePanel.captUseSharedCompilation;
            this.chkUseSharedCompilation.Tag = XSharpProjectFileConstants.UseSharedCompilation;
            this.toolTip1.SetToolTip(this.chkUseSharedCompilation, BuildPropertyPagePanel.descUseSharedCompilation);

            this.chkPrefer32Bit.Text = BuildPropertyPagePanel.captPrefer32Bit;
            this.chkPrefer32Bit.Tag = XSharpProjectFileConstants.Prefer32Bit;
            this.toolTip1.SetToolTip(this.chkPrefer32Bit, BuildPropertyPagePanel.descPrefer32Bit);

            this.chkRegisterForComInterop.Text = BuildPropertyPagePanel.captRegisterForComInterop;
            this.chkRegisterForComInterop.Tag = XSharpProjectFileConstants.RegisterForComInterop;
            this.toolTip1.SetToolTip(this.chkRegisterForComInterop, BuildPropertyPagePanel.descRegisterForComInterop);

            this.chkXMLDocumentationFile.Text = BuildPropertyPagePanel.captDocumentationFile;
            this.txtXMLDocumentationFile.Tag = XSharpProjectFileConstants.DocumentationFile;
            this.toolTip1.SetToolTip(chkXMLDocumentationFile, BuildPropertyPagePanel.descDocumentationFile);
            this.toolTip1.SetToolTip(txtXMLDocumentationFile, BuildPropertyPagePanel.descDocumentationFileName);

            this.chkOptimize.Text = BuildPropertyPagePanel.captOptimize;
            this.chkOptimize.Tag = XSharpProjectFileConstants.Optimize;
            this.toolTip1.SetToolTip(this.chkOptimize, BuildPropertyPagePanel.descOptimize);

            this.chkSignAssembly.Text = BuildPropertyPagePanel.captSignAssembly;
            this.chkSignAssembly.Tag = XSharpProjectFileConstants.SignAssembly;
            this.toolTip1.SetToolTip(this.chkSignAssembly, BuildPropertyPagePanel.descSignAssembly);

            this.chkSuppressRCWarnings.Text = BuildPropertyPagePanel.SuppressRCWarningsCaption;
            this.chkSuppressRCWarnings.Tag = XSharpProjectFileConstants.SuppressRCWarnings;
            this.toolTip1.SetToolTip(this.chkSuppressRCWarnings, BuildPropertyPagePanel.SuppressRCWarningsDescription);

            this.chkDelaySign.Text = BuildPropertyPagePanel.captDelaySign;
            this.chkDelaySign.Tag = XSharpProjectFileConstants.DelaySign;
            this.toolTip1.SetToolTip(this.chkDelaySign, BuildPropertyPagePanel.descDelaySign);

            this.txtDefineConstants.Tag = XSharpProjectFileConstants.DefineConstants;
            this.lblDefineConstants.Text = BuildPropertyPagePanel.DefCaption;
            this.toolTip1.SetToolTip(this.txtDefineConstants, BuildPropertyPagePanel.DefDescription);
            this.toolTip1.SetToolTip(this.lblDefineConstants, BuildPropertyPagePanel.DefDescription);

            this.txtCommandLineOption.Tag = XSharpProjectFileConstants.CommandLineOption;
            this.lblCommandLineOption.Text = BuildPropertyPagePanel.CmdLineCaption;
            this.toolTip1.SetToolTip(txtCommandLineOption, BuildPropertyPagePanel.CmdLineDescription);
            this.toolTip1.SetToolTip(lblCommandLineOption, BuildPropertyPagePanel.CmdLineDescription);

            this.txtDisabledWarnings.Tag = XSharpProjectFileConstants.DisabledWarnings;
            this.lblDisabledWarnings.Text = BuildPropertyPagePanel.captDisabledWarnings;
            this.toolTip1.SetToolTip(lblDisabledWarnings, BuildPropertyPagePanel.descDisabledWarnings);
            this.toolTip1.SetToolTip(txtDisabledWarnings, BuildPropertyPagePanel.descDisabledWarnings);

            this.txtOutputPath.Tag = XSharpProjectFileConstants.OutputPath;
            this.lblOutputPath.Text = BuildPropertyPagePanel.captOutputPath;
            this.toolTip1.SetToolTip(txtOutputPath, BuildPropertyPagePanel.descOutputPath);
            this.toolTip1.SetToolTip(lblOutputPath, BuildPropertyPagePanel.descOutputPath);


            this.txtIntermediateOutputPath.Tag = XSharpProjectFileConstants.IntermediateOutputPath;
            this.lblIntermediateOutputPath.Text = BuildPropertyPagePanel.captIntermediateOutputPath;
            this.toolTip1.SetToolTip(txtIntermediateOutputPath, BuildPropertyPagePanel.descIntermediateOutputPath);
            this.toolTip1.SetToolTip(lblIntermediateOutputPath, BuildPropertyPagePanel.descIntermediateOutputPath);

            this.txtAssemblyOriginatorKeyFile.Tag = XSharpProjectFileConstants.AssemblyOriginatorKeyFile;
            this.lblAssemblyOriginatorKeyFile.Text = BuildPropertyPagePanel.captAssemblyOriginatorKeyFile;
            this.toolTip1.SetToolTip(txtAssemblyOriginatorKeyFile, BuildPropertyPagePanel.descAssemblyOriginatorKeyFile);
            this.toolTip1.SetToolTip(lblAssemblyOriginatorKeyFile, BuildPropertyPagePanel.descAssemblyOriginatorKeyFile);

            this.lblPlatformTarget.Text = BuildPropertyPagePanel.captPlatFormTarget;
            this.comboPlatformTarget.Tag = XSharpProjectFileConstants.PlatformTarget;
            this.toolTip1.SetToolTip(lblPlatformTarget, BuildPropertyPagePanel.descPlatFormTarget);
            this.toolTip1.SetToolTip(comboPlatformTarget, BuildPropertyPagePanel.descPlatFormTarget);

            this.lblWarningLevel.Text = BuildPropertyPagePanel.captWarningLevel;
            this.cboWarningLevel.Tag = XSharpProjectFileConstants.WarningLevel;
            this.toolTip1.SetToolTip(lblWarningLevel, BuildPropertyPagePanel.descWarningLevel);
            this.toolTip1.SetToolTip(cboWarningLevel, BuildPropertyPagePanel.descWarningLevel);

            this.rbWarningAll.Tag = XSharpProjectFileConstants.TreatWarningsAsErrors + "|True";
            this.rbWarningNone.Tag = XSharpProjectFileConstants.TreatWarningsAsErrors + "|False";
            this.rbWarningSpecific.Tag = XSharpProjectFileConstants.TreatWarningsAsErrors + "|False";
            this.txtSpecificWarnings.Tag = XSharpProjectFileConstants.WarningsAsErrors;
            this.toolTip1.SetToolTip(txtSpecificWarnings, BuildPropertyPagePanel.descSpecificWarnings);

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
            showMacroDialog(txtOutputPath, BuildPropertyPagePanel.descOutputPath);
        }

        private void btnIntermediateOutputPath_Click(object sender, EventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            showMacroDialog(txtIntermediateOutputPath, BuildPropertyPagePanel.descIntermediateOutputPath);
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
            this.ParentPropertyPage.SetProperty(XSharpProjectFileConstants.DocumentationFile, documentationFile);
            this.txtXMLDocumentationFile.Text = documentationFile;
        }

        protected internal override void BindProperties()
        {
            base.BindProperties();
            this.chkXMLDocumentationFile.Checked = !string.IsNullOrEmpty(ParentPropertyPage.GetProperty(XSharpProjectFileConstants.DocumentationFile));
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
            ThreadHelper.ThrowIfNotOnUIThread();
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
            showMacroDialog(txtAssemblyOriginatorKeyFile, BuildPropertyPagePanel.descAssemblyOriginatorKeyFile,
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
