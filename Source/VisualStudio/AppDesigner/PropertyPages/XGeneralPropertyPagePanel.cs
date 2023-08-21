//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using System;
using System.Drawing;
using System.Windows.Forms;
using XSharpModel;
using XSharp.Settings;

namespace XSharp.Project
{
    /// <summary>
    /// Property page contents for the Candle Settings page.
    /// </summary>
    internal partial class XGeneralPropertyPagePanel : XPropertyPagePanel
    {

        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildEventsPropertyPagePanel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The parent property page to which this is bound.</param>
        public XGeneralPropertyPagePanel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
            this.InitializeComponent();

            this.tbAssemblyName.Tag = XSharpProjectFileConstants.AssemblyName;
            this.tbDefaultNamespace.Tag = XSharpProjectFileConstants.RootNamespace;
            this.tbAppIcon.Tag = XSharpProjectFileConstants.ApplicationIcon;
            this.comboStartupObject.Tag = XSharpProjectFileConstants.StartupObject;
            this.chkPreferNativeVersion.Tag = XSharpProjectFileConstants.UseNativeVersion;
            this.chkSuppressDefaultManifest.Tag = XSharpProjectFileConstants.NoWin32Manifest;
            this.chkVulcanCompatibleResources.Tag = XSharpProjectFileConstants.VulcanCompatibleResources;
            this.chkAutoGenerateBindingRedirects.Tag = XSharpProjectFileConstants.AutoGenerateBindingRedirects;
            this.comboDialect.Tag = XSharpProjectFileConstants.Dialect;
            this.comboOutputType.Tag = XSharpProjectFileConstants.OutputType;
            this.comboTargetFramework.Tag = XSharpProjectFileConstants.TargetFrameworkVersion;

            lblDefaultNamespace.Text = GeneralPropertyPagePanel.captNamespace;
            toolTip1.SetToolTip(lblDefaultNamespace, GeneralPropertyPagePanel.descNamespace);
            toolTip1.SetToolTip(tbDefaultNamespace, GeneralPropertyPagePanel.descNamespace);

            lblApplicationName.Text = GeneralPropertyPagePanel.captAppName;
            toolTip1.SetToolTip(lblApplicationName, GeneralPropertyPagePanel.descAssembly);
            toolTip1.SetToolTip(tbAssemblyName, GeneralPropertyPagePanel.descAssembly);
            labelIcon.Text = GeneralPropertyPagePanel.captIcon;
            toolTip1.SetToolTip(labelIcon, GeneralPropertyPagePanel.descIcon);
            toolTip1.SetToolTip(tbAppIcon, GeneralPropertyPagePanel.descIcon);
            toolTip1.SetToolTip(btnIcon, GeneralPropertyPagePanel.descIcon);
            labelStartupObject.Text = GeneralPropertyPagePanel.captStartup;
            toolTip1.SetToolTip(labelStartupObject, GeneralPropertyPagePanel.descStartup);
            toolTip1.SetToolTip(comboStartupObject, GeneralPropertyPagePanel.descStartup);

            chkSuppressDefaultManifest.Text = GeneralPropertyPagePanel.captWin32Manifest;
            toolTip1.SetToolTip(chkSuppressDefaultManifest, GeneralPropertyPagePanel.descWin32Manifest);

            chkPreferNativeVersion.Text = GeneralPropertyPagePanel.captPreferNative;
            toolTip1.SetToolTip(chkPreferNativeVersion, GeneralPropertyPagePanel.descPreferNative);
            chkVulcanCompatibleResources.Text = GeneralPropertyPagePanel.captVulcanCompatibleResouces;
            toolTip1.SetToolTip(chkVulcanCompatibleResources, GeneralPropertyPagePanel.descVulcanCompatibleResouces);
            chkAutoGenerateBindingRedirects.Text = GeneralPropertyPagePanel.captBindingRedirects;
            toolTip1.SetToolTip(chkAutoGenerateBindingRedirects, GeneralPropertyPagePanel.descBindingRedirects);


            FillCombo(new DialectConverter(), comboDialect);
            toolTip1.SetToolTip(lblDialect, GeneralPropertyPagePanel.descDialect);
            toolTip1.SetToolTip(comboDialect, GeneralPropertyPagePanel.descDialect);
            FillCombo(new OutputTypeConverter(), comboOutputType);
            toolTip1.SetToolTip(lblOutputType, GeneralPropertyPagePanel.descOutputType);
            toolTip1.SetToolTip(comboOutputType, GeneralPropertyPagePanel.descOutputType);
            FillCombo(new FrameworkNameConverter(), comboTargetFramework);
            toolTip1.SetToolTip(lblTargetFramework, GeneralPropertyPagePanel.descFramework);
            toolTip1.SetToolTip(comboTargetFramework, GeneralPropertyPagePanel.descFramework);


            // hook up the form to both editors
            Color defaultBackground = SystemColors.ButtonFace;
            Color defaultForeground = SystemColors.WindowText;
            UpdateWindowColors(this, defaultBackground, defaultForeground);
        }

        void GetStartupClasses()
        {
            this.comboStartupObject.Items.Clear();
            this.comboStartupObject.Items.Add(GeneralPropertyPagePanel.DefaultValue);
            var prjfile = ParentPropertyPage.ProjectMgr.ProjectFile;
            var list = XDatabase.GetStartupClasses(System.IO.Path.GetFileName(prjfile));
            foreach (var item in list)
            {
                this.comboStartupObject.Items.Add(item);
            }
            if (comboStartupObject.Items.Count == 1)
            {
                comboStartupObject.SelectedItem = GeneralPropertyPagePanel.DefaultValue;
                this.comboStartupObject.Items.Add("");
                comboStartupObject.Enabled = false;
            }
        }

        void EnableApplicationIcon()
        {
            var prjfile = ParentPropertyPage.ProjectMgr.ProjectFile;
            var hasRcFiles = XDatabase.HasRCFiles(prjfile);
            this.labelIcon.Enabled = !hasRcFiles;
            this.tbAppIcon.Enabled = !hasRcFiles;
            this.btnIcon.Enabled = !hasRcFiles;
        }

        protected internal override void BindProperties()
        {
            this.GetStartupClasses();
            this.EnableApplicationIcon();
            base.BindProperties();
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

        bool resetting = false;
        protected override void HandleControlValidated(object sender, EventArgs e)
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                if (!resetting)
                {

                    base.HandleControlValidated(sender, e);
                }
            });
        }

        internal void resetFramework(string value)
        {
            int index = 0;
            resetting = true;
            foreach (string item in comboTargetFramework.Items)
            {
                if (String.Compare(item, value, true) == 0)
                {
                    comboTargetFramework.SelectedIndex = index;
                    break;
                }
                index++;
            }
            resetting = false;
        }
        private void btnIcon_Click(object sender, EventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            ShowOpenFileDialog(tbAppIcon, GeneralPropertyPagePanel.descIcon, "Icon Files (*.ico)|*.ico|All files (*.*)|*.*");
        }
    }
}
