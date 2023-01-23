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
    using XSharpModel;
    /// <summary>
    /// Property page contents for the Candle Settings page.
    /// </summary>

    internal partial class XDebugPropertyPagePanel : XPropertyPagePanel
    {
       

        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildEventsPropertyPagePanel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The parent property page to which this is bound.</param>
        public XDebugPropertyPagePanel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
            this.InitializeComponent();
            this.lblCommand.Text = DebugPropertyPagePanel.captDebuggerCommand;
            this.tbCommand.Tag = XSharpProjectFileConstants.DebuggerCommand;
            this.toolTip1.SetToolTip(lblCommand, DebugPropertyPagePanel.descDebuggerCommand);
            this.toolTip1.SetToolTip(tbCommand, DebugPropertyPagePanel.descDebuggerCommand);

            this.lblCommandArguments.Text = DebugPropertyPagePanel.captDebuggerCommandArguments;
            this.tbArguments.Tag = XSharpProjectFileConstants.DebuggerCommandArguments;
            this.toolTip1.SetToolTip(lblCommandArguments, DebugPropertyPagePanel.descDebuggerCommandArguments);
            this.toolTip1.SetToolTip(tbArguments, DebugPropertyPagePanel.descDebuggerCommandArguments);

            this.lblDebugInfo.Text = DebugPropertyPagePanel.captDebugType;
            this.comboDebugInfo.Tag = XSharpProjectFileConstants.DebugType;
            this.toolTip1.SetToolTip(this.lblDebugInfo, DebugPropertyPagePanel.descDebugType);
            this.toolTip1.SetToolTip(this.comboDebugInfo, DebugPropertyPagePanel.descDebugType);

            this.lblWorkDir.Text = DebugPropertyPagePanel.captDebuggerWorkingDirectory;
            this.tbWorkdir.Tag = XSharpProjectFileConstants.DebuggerWorkingDirectory;
            this.toolTip1.SetToolTip(this.lblWorkDir, DebugPropertyPagePanel.descDebuggerWorkingDirectory);
            this.toolTip1.SetToolTip(this.tbWorkdir, DebugPropertyPagePanel.descDebuggerWorkingDirectory);

            this.chkUnmanagedDebugging.Text = DebugPropertyPagePanel.captEnableUnmanagedDebugging;
            this.chkUnmanagedDebugging.Tag = XSharpProjectFileConstants.EnableUnmanagedDebugging;
            this.toolTip1.SetToolTip(chkUnmanagedDebugging, DebugPropertyPagePanel.descEnableUnmanagedDebugging);

            FillCombo(new DebugTypeConverter(), comboDebugInfo);

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


        private void btnDebuggerWorkingDirectory_Click(object sender, EventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            showMacroDialog(tbWorkdir, DebugPropertyPagePanel.descDebuggerWorkingDirectory);

        }

        private void btnCommand_Click(object sender, EventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            showMacroDialog(tbCommand, DebugPropertyPagePanel.descDebuggerCommand);

        }
    }
}
