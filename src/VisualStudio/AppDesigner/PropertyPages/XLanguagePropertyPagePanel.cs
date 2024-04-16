//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Drawing;
using System.Windows.Forms;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using XSharp.Settings;

namespace XSharp.Project
{
    /// <summary>
    /// Property page contents for the Candle Settings page.
    /// </summary>
    internal partial class XLanguagePropertyPagePanel : XPropertyPagePanel
    {


        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildEventsPropertyPagePanel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The parent property page to which this is bound.</param>
        public XLanguagePropertyPagePanel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
            this.InitializeComponent();
            chkAZ.Text = LanguagePropertyPagePanel.AZCaption;
            chkAZ.Tag = XSharpProjectFileConstants.AZ;
            toolTip1.SetToolTip(chkAZ, LanguagePropertyPagePanel.AZDescription);
            chkCS.Text = LanguagePropertyPagePanel.CSCaption;
            chkCS.Tag = XSharpProjectFileConstants.CS;
            toolTip1.SetToolTip(chkCS, LanguagePropertyPagePanel.CSDescription);
            chkIns.Text = LanguagePropertyPagePanel.INSCaption;
            chkIns.Tag = XSharpProjectFileConstants.INS;
            toolTip1.SetToolTip(chkIns, LanguagePropertyPagePanel.INSDescription);
            chkInitLocals.Text = LanguagePropertyPagePanel.InitLocalsCaption;
            chkInitLocals.Tag = XSharpProjectFileConstants.InitLocals;
            toolTip1.SetToolTip(chkInitLocals, LanguagePropertyPagePanel.InitLocalsDescription);
            chkLB.Text = LanguagePropertyPagePanel.LBCaption;
            chkLB.Tag = XSharpProjectFileConstants.LB;
            toolTip1.SetToolTip(chkLB, LanguagePropertyPagePanel.LBDescription);
            chkMemVar.Text = LanguagePropertyPagePanel.MemVarCaption;
            chkMemVar.Tag = XSharpProjectFileConstants.MemVar;
            toolTip1.SetToolTip(chkMemVar, LanguagePropertyPagePanel.MemVarDescription);
            chkMemVar.CheckStateChanged += ChkMemVar_CheckStateChanged;
            chkNamedArgs.Text = LanguagePropertyPagePanel.NamedArgCaption;
            chkNamedArgs.Tag = XSharpProjectFileConstants.NamedArgs;
            toolTip1.SetToolTip(chkNamedArgs, LanguagePropertyPagePanel.NamedArgDescription);
            chkNoStandardDefs.Text = LanguagePropertyPagePanel.NoStdDefCaption;
            chkNoStandardDefs.Tag = XSharpProjectFileConstants.NoStandardDefs;
            toolTip1.SetToolTip(chkNoStandardDefs, LanguagePropertyPagePanel.NoStdDefDescription);
            chkNS.Text = LanguagePropertyPagePanel.NSCaption;
            chkNS.Tag = XSharpProjectFileConstants.NS;
            toolTip1.SetToolTip(chkNS, LanguagePropertyPagePanel.NSDescription);
            chkOvf.Text = LanguagePropertyPagePanel.OVFCaption;
            chkOvf.Tag = XSharpProjectFileConstants.OVF;
            toolTip1.SetToolTip(chkOvf, LanguagePropertyPagePanel.OVFDescription);
            chkUndefined.Text = LanguagePropertyPagePanel.UndeclaredCaption;
            chkUndefined.Tag = XSharpProjectFileConstants.Undeclared;
            toolTip1.SetToolTip(chkUndefined, LanguagePropertyPagePanel.UndeclaredDescription);
            chkUnsafe.Text = LanguagePropertyPagePanel.UnsafeCaption;
            chkUnsafe.Tag = XSharpProjectFileConstants.Unsafe;
            toolTip1.SetToolTip(chkUnsafe, LanguagePropertyPagePanel.UnsafeDescription);

            lblIncludePaths.Text = LanguagePropertyPagePanel.INCCaption;
            tbIncludePath.Tag = XSharpProjectFileConstants.IncludePaths;
            toolTip1.SetToolTip(lblIncludePaths, LanguagePropertyPagePanel.INCDescription);
            toolTip1.SetToolTip(tbIncludePath, LanguagePropertyPagePanel.INCDescription);
            toolTip1.SetToolTip(this.btnIncludePaths, LanguagePropertyPagePanel.INCDescription);
            lblStandardDefs.Text = LanguagePropertyPagePanel.StdDefCaption;
            tbStandardDefs.Tag = XSharpProjectFileConstants.StandardDefs;
            toolTip1.SetToolTip(lblStandardDefs, LanguagePropertyPagePanel.StdDefDescription);
            toolTip1.SetToolTip(tbStandardDefs, LanguagePropertyPagePanel.StdDefDescription);
            toolTip1.SetToolTip(btnStandardHeader, LanguagePropertyPagePanel.StdDefDescription);
            chkEnforceSelf.Text = LanguagePropertyPagePanel.enforceSelfCaption;
            chkEnforceSelf.Tag = XSharpProjectFileConstants.EnforceSelf;
            toolTip1.SetToolTip(chkEnforceSelf, LanguagePropertyPagePanel.EnforceSelfDescription);

            chkEnforceOverride.Text = LanguagePropertyPagePanel.EnforceOverrideCaption;
            chkEnforceOverride.Tag = XSharpProjectFileConstants.EnforceOverride;
            toolTip1.SetToolTip(chkEnforceOverride, LanguagePropertyPagePanel.EnforceOverrideDescription);

            chkAllowDot.Text = LanguagePropertyPagePanel.allowDotCaption;
            chkAllowDot.Tag = XSharpProjectFileConstants.Allowdot;
            toolTip1.SetToolTip(chkAllowDot, LanguagePropertyPagePanel.allowDotDescription);


            chkOldStyleAssignments.Text = LanguagePropertyPagePanel.allowOldStyleCaption;
            chkOldStyleAssignments.Tag = XSharpProjectFileConstants.AllowOldStyleAssignments;
            toolTip1.SetToolTip(chkOldStyleAssignments, LanguagePropertyPagePanel.allowOldStyleDescription);

            chkModernSyntax.Text = LanguagePropertyPagePanel.ModernSyntaxCaption;
            chkModernSyntax.Tag = XSharpProjectFileConstants.ModernSyntax;
            toolTip1.SetToolTip(chkAllowDot, LanguagePropertyPagePanel.ModernSyntaxDescription);

            // hook up the form to both editors
            Color defaultBackground = SystemColors.ButtonFace;
            Color defaultForeground = SystemColors.WindowText;
            UpdateWindowColors(this, defaultBackground, defaultForeground);

        }
        private void ChkMemVar_CheckStateChanged(object sender, EventArgs e)
        {
            EnableMemVars();
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
        internal void SetDialectOptions(string dialect)
        {
            bool Core = dialect == XSharpProjectFileConstants.DialectCore;
            bool FoxPro = dialect == XSharpProjectFileConstants.DialectFoxPro;
            ThreadHelper.ThrowIfNotOnUIThread();
            chkMemVar.Enabled = !Core;
            chkLB.Enabled = !Core;
            chkOldStyleAssignments.Checked = dialect == XSharpProjectFileConstants.DialectFoxPro; 

            if (!Core)
            {
                EnableMemVars(); // this sets Undefined
            }
            else
            {
                chkUndefined.Enabled = false; 
                chkMemVar.Checked = false;
                chkUndefined.Checked = false;
                chkLB.Checked = false;
                this.ParentPropertyPage.IsDirty = true;
            }
            var strAllowDot = this.ParentPropertyPage.GetProperty(XSharpProjectFileConstants.Allowdot);
            // 2 properties have a default of TRUE for some dialects and FALSE for others
            if (string.IsNullOrEmpty(strAllowDot))
            {
                if (Core || FoxPro)
                {
                    this.ParentPropertyPage.SetProperty(XSharpProjectFileConstants.Allowdot, "True");
                    chkAllowDot.Checked = true;
                }
            }
            var strNamedArgs = this.ParentPropertyPage.GetProperty(XSharpProjectFileConstants.NamedArgs);
            if (string.IsNullOrEmpty(strNamedArgs))
            {
                this.ParentPropertyPage.SetProperty(XSharpProjectFileConstants.NamedArgs, Core.ToString());
                chkNamedArgs.Checked = Core;

            }

        }

        internal void Project_OnProjectPropertyChanged(object sender, ProjectPropertyChangedArgs e)
        {
            try
            {
                ThreadHelper.ThrowIfNotOnUIThread();

                if (string.Compare(e.PropertyName, XSharpProjectFileConstants.Dialect, true) == 0)
                {
                    SetDialectOptions(e.NewValue);
                }
            }
            catch (Exception)
            {
            }
        }

        private void EnableMemVars()
        {
            chkUndefined.Enabled = chkMemVar.Checked;
        }

        protected internal override void BindProperties()
        {
            base.BindProperties();
            ThreadHelper.ThrowIfNotOnUIThread();
            SetDialectOptions(ParentPropertyPage.GetProperty(XSharpProjectFileConstants.Dialect) ?? "Core");
            EnabledisableStandardDefs();
        }

        private void btnStandardHeader_Click(object sender, EventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            ShowOpenFileDialog(tbStandardDefs, LanguagePropertyPagePanel.StdDefDescription, "Header Files (*.xh; *.vh; *.ch)|*.xh;*.vh;*.ch|All files (*.*)|*.*");
        }

        private void chkNoStandardDefs_CheckedChanged(object sender, EventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            EnabledisableStandardDefs();
        }
        private void EnabledisableStandardDefs()
        {
            //ThreadHelper.ThrowIfNotOnUIThread();
            //btnStandardHeader.Enabled = this.chkNoStandardDefs.Checked;
            //tbStandardDefs.Enabled = btnStandardHeader.Enabled;
        }

        private void chkIncludePaths_Click(object sender, EventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            showMacroDialog(this.tbIncludePath, LanguagePropertyPagePanel.INCDescription);
        }
    }
}
