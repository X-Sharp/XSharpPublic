//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

namespace XSharp.Project
{
    using Microsoft.VisualStudio.Project;
    using Microsoft.VisualStudio.Shell;
    using System;
    using System.Drawing;
    using System.Windows.Forms;
    using XSharpModel;

    /// <summary>
    /// Property page contents for the Candle Settings page.
    /// </summary>

    internal partial class XDialectPropertyPagePanel : XPropertyPagePanel
    {

        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildEventsPropertyPagePanel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The parent property page to which this is bound.</param>
        public XDialectPropertyPagePanel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
            this.InitializeComponent();
            this.chkVO1.Text = DialectPropertyPagePanel.VO1Caption;
            this.chkVO2.Text = DialectPropertyPagePanel.VO2Caption;
            this.chkVO3.Text = DialectPropertyPagePanel.VO3Caption;
            this.chkVO4.Text = DialectPropertyPagePanel.VO4Caption;
            this.chkVO5.Text = DialectPropertyPagePanel.VO5Caption;
            this.chkVO6.Text = DialectPropertyPagePanel.VO6Caption;
            this.chkVO7.Text = DialectPropertyPagePanel.VO7Caption;
            this.chkVO8.Text = DialectPropertyPagePanel.VO8Caption;
            this.chkVO9.Text = DialectPropertyPagePanel.VO9Caption;
            this.chkVO10.Text = DialectPropertyPagePanel.VO10Caption;
            this.chkVO11.Text = DialectPropertyPagePanel.VO11Caption;
            this.chkVO12.Text = DialectPropertyPagePanel.VO12Caption;
            this.chkVO13.Text = DialectPropertyPagePanel.VO13Caption;
            this.chkVO14.Text = DialectPropertyPagePanel.VO14Caption;
            this.chkVO15.Text = DialectPropertyPagePanel.VO15Caption;
            this.chkVO16.Text = DialectPropertyPagePanel.VO16Caption;
            this.chkVO17.Text = DialectPropertyPagePanel.VO17Caption;
            this.chkFox1.Text = DialectPropertyPagePanel.FOX1Caption;
            this.chkFox2.Text = DialectPropertyPagePanel.FOX2Caption;
            this.chkXPP1.Text = DialectPropertyPagePanel.XPP1Caption;
            this.chkVO1.Tag = XSharpProjectFileConstants.Vo1;
            this.chkVO2.Tag = XSharpProjectFileConstants.Vo2;
            this.chkVO3.Tag = XSharpProjectFileConstants.Vo3;
            this.chkVO4.Tag = XSharpProjectFileConstants.Vo4;
            this.chkVO5.Tag = XSharpProjectFileConstants.Vo5;
            this.chkVO6.Tag = XSharpProjectFileConstants.Vo6;
            this.chkVO7.Tag = XSharpProjectFileConstants.Vo7;
            this.chkVO8.Tag = XSharpProjectFileConstants.Vo8;
            this.chkVO9.Tag = XSharpProjectFileConstants.Vo9;
            this.chkVO10.Tag = XSharpProjectFileConstants.Vo10;
            this.chkVO11.Tag = XSharpProjectFileConstants.Vo11;
            this.chkVO12.Tag = XSharpProjectFileConstants.Vo12;
            this.chkVO13.Tag = XSharpProjectFileConstants.Vo13;
            this.chkVO14.Tag = XSharpProjectFileConstants.Vo14;
            this.chkVO15.Tag = XSharpProjectFileConstants.Vo15;
            this.chkVO16.Tag = XSharpProjectFileConstants.Vo16;
            this.chkVO17.Tag = XSharpProjectFileConstants.Vo17;
            this.chkFox1.Tag = XSharpProjectFileConstants.Fox1;
            this.chkFox2.Tag = XSharpProjectFileConstants.Fox2;
            this.chkXPP1.Tag = XSharpProjectFileConstants.Xpp1;
            toolTip1.SetToolTip(chkVO1, DialectPropertyPagePanel.VO1Description);
            toolTip1.SetToolTip(chkVO2, DialectPropertyPagePanel.VO2Description);
            toolTip1.SetToolTip(chkVO3, DialectPropertyPagePanel.VO3Description);
            toolTip1.SetToolTip(chkVO4, DialectPropertyPagePanel.VO4Description);
            toolTip1.SetToolTip(chkVO5, DialectPropertyPagePanel.VO5Description);
            toolTip1.SetToolTip(chkVO6, DialectPropertyPagePanel.VO6Description);
            toolTip1.SetToolTip(chkVO7, DialectPropertyPagePanel.VO7Description);
            toolTip1.SetToolTip(chkVO8, DialectPropertyPagePanel.VO8Description);
            toolTip1.SetToolTip(chkVO9, DialectPropertyPagePanel.VO9Description);
            toolTip1.SetToolTip(chkVO10, DialectPropertyPagePanel.VO10Description);
            toolTip1.SetToolTip(chkVO11, DialectPropertyPagePanel.VO11Description);
            toolTip1.SetToolTip(chkVO12, DialectPropertyPagePanel.VO12Description);
            toolTip1.SetToolTip(chkVO13, DialectPropertyPagePanel.VO13Description);
            toolTip1.SetToolTip(chkVO14, DialectPropertyPagePanel.VO14Description);
            toolTip1.SetToolTip(chkVO15, DialectPropertyPagePanel.VO15Description);
            toolTip1.SetToolTip(chkVO16, DialectPropertyPagePanel.VO16Description);
            toolTip1.SetToolTip(chkVO17, DialectPropertyPagePanel.VO17Description);

            toolTip1.SetToolTip(chkFox1, DialectPropertyPagePanel.FOX1Description);
            toolTip1.SetToolTip(chkFox2, DialectPropertyPagePanel.FOX2Description);
            toolTip1.SetToolTip(chkXPP1, DialectPropertyPagePanel.XPP1Description);
            this.lblAllDialects.Text = DialectPropertyPagePanel.CatCompatibility;
            this.lblNotInCore.Text = DialectPropertyPagePanel.CatNotCore;
            this.lblVFP.Text = DialectPropertyPagePanel.FOXCompatibility;
            this.lblXPP.Text = DialectPropertyPagePanel.XPPCompatibility;

            // hook up the form to both editors
            Color defaultBackground = SystemColors.ButtonFace;
            Color defaultForeground = SystemColors.WindowText;
            UpdateWindowColors(this, defaultBackground, defaultForeground);

        }

        internal void Project_OnProjectPropertyChanged(object sender, ProjectPropertyChangedArgs e)
        {
            try
            {
                if (e.OldValue != e.NewValue)
                {
                    if (e.PropertyName.ToLower() == "dialect")
                    {
                        if (e.NewValue.ToLower() == "foxpro")
                        {
                            chkFox1.Checked = true;
                            chkFox2.Checked = true;
                        }
                        else
                        {
                            chkFox1.Checked = false;
                            chkFox2.Checked = false;
                        }
                        EnableDialectOptions(e.NewValue);
                    }
                }
            }
            catch (Exception)
            {
            }
        }


        private void EnableDialectOptions(string dialect)
        {
            dialect = dialect.ToLower();
            chkFox1.Enabled = dialect == "foxpro";
            chkFox2.Enabled = dialect == "foxpro";
            chkXPP1.Enabled = dialect == "xpp";
            bool core = dialect == "core";
            chkVO5.Enabled = !core;
            chkVO6.Enabled = !core;
            chkVO7.Enabled = !core;
            chkVO11.Enabled = !core;
            chkVO12.Enabled = !core;
            chkVO13.Enabled = !core;
            chkVO14.Enabled = !core;
            chkVO15.Enabled = !core;
            chkVO16.Enabled = !core;
            chkVO17.Enabled = !core;
            if (core)
            {
                chkVO5.Checked = false;
                chkVO6.Checked = false;
                chkVO7.Checked = false;
                chkVO11.Checked = false;
                chkVO12.Checked = false;
                chkVO13.Checked = false;
                chkVO14.Checked = false;
                chkVO15.Checked = false;
                chkVO16.Checked = false;
                chkVO17.Checked = false;
                chkFox1.Checked = false;
                chkXPP1.Checked = false;
            }
            if (!chkFox1.Enabled)
                chkFox1.Checked = false;
            if (!chkFox2.Enabled)
                chkFox2.Checked = false;
            if (!chkXPP1.Enabled)
                chkXPP1.Checked = false;
        }
        internal void SetDialectOptions(string dialect)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            EnableDialectOptions(dialect);
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

        protected internal override void BindProperties()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            base.BindProperties();
            SetDialectOptions(ParentPropertyPage.GetProperty(XSharpProjectFileConstants.Dialect) ?? "Core");
        }
    }
}
