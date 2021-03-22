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

    /// <summary>
    /// Property page contents for the Candle Settings page.
    /// </summary>

    internal partial class XDebugPropertyPagePanel : XPropertyPagePanel
    {
        #region Constants
        internal const string catGeneral = "General";
        internal const string catSpecial = "Special";
        internal const string captOutputPath = "Output Path";
        internal const string descOutputPath = "Output Path (macros are allowed)";
        internal const string captDebugType = "Generate Debug Information";
        internal const string descDebugType = "Generate Debug Information (none, full, pdbonly)";
        internal const string captDebuggerCommand = "Command";
        internal const string descDebuggerCommand = "The debug command to execute";
        internal const string captDebuggerCommandArguments = "Command Arguments";
        internal const string descDebuggerCommandArguments = "The command line arguments to pass to the application";
        internal const string captDebuggerWorkingDirectory = "Working Directory";
        internal const string descDebuggerWorkingDirectory = "The application's working directory. By default, the directory containing the project file.";
        internal const string captDebuggerAttach = "Attach";
        internal const string descDebuggerAttach = "Specifies whether the debugger should attempt to attach to an existing process when debugging starts.";
        internal const string captEnableUnmanagedDebugging = "Enable unmanaged debugging";
        internal const string descEnableUnmanagedDebugging = "Enable unmanaged debugging";
        internal const string captUseVSHostingProcess = "Enable the Visual Studio Hosting Process";
        internal const string descUseVSHostingProcess = "Enable the Visual Studio Hosting Process";

        #endregion

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
            this.lblCommand.Text = captDebuggerCommand;
            this.tbCommand.Tag = XSharpProjectFileConstants.DebuggerCommand;
            this.toolTip1.SetToolTip(lblCommand, descDebuggerCommand);
            this.toolTip1.SetToolTip(tbCommand, descDebuggerCommand);

            this.lblCommandArguments.Text = captDebuggerCommandArguments;
            this.tbArguments.Tag = XSharpProjectFileConstants.DebuggerCommandArguments;
            this.toolTip1.SetToolTip(lblCommandArguments, descDebuggerCommandArguments);
            this.toolTip1.SetToolTip(tbArguments, descDebuggerCommandArguments);

            this.lblDebugInfo.Text = captDebugType;
            this.comboDebugInfo.Tag = XSharpProjectFileConstants.DebugType;
            this.toolTip1.SetToolTip(this.lblDebugInfo, descDebugType);
            this.toolTip1.SetToolTip(this.comboDebugInfo, descDebugType);

            this.lblWorkDir.Text = captDebuggerWorkingDirectory;
            this.tbWorkdir.Tag = XSharpProjectFileConstants.DebuggerWorkingDirectory;
            this.toolTip1.SetToolTip(this.lblWorkDir, descDebuggerWorkingDirectory);
            this.toolTip1.SetToolTip(this.tbWorkdir, descDebuggerWorkingDirectory);

            this.chkUnmanagedDebugging.Text = captEnableUnmanagedDebugging;
            this.chkUnmanagedDebugging.Tag = XSharpProjectFileConstants.EnableUnmanagedDebugging;
            this.toolTip1.SetToolTip(chkUnmanagedDebugging, descEnableUnmanagedDebugging);

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
            showMacroDialog(tbWorkdir, descDebuggerWorkingDirectory);

        }

        private void btnCommand_Click(object sender, EventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            showMacroDialog(tbCommand, descDebuggerCommand);

        }
    }
}
