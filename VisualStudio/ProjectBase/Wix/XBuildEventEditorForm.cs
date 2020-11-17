//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

namespace Microsoft.VisualStudio.Project
{
    using System;
    using System.Collections.Generic;
    using System.ComponentModel;
    using System.Windows.Forms;

    /// <summary>
    /// Advanced editor form for build events.
    /// </summary>
    public partial class XBuildEventEditorForm : Form
    {
        private IServiceProvider serviceProvider;

        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildEventEditorForm"/> class.
        /// </summary>
        public XBuildEventEditorForm(IServiceProvider serviceProvider)
        {
            this.serviceProvider = serviceProvider;
            this.InitializeComponent();

            this.Font = XHelperMethods.GetDialogFont();

            //this.BackColor = XHelperMethods.GetVsColor(XHelperMethods.Vs2010Color.VSCOLOR_BUTTONFACE);
            XHelperMethods.SetControlTreeColors(this);
        }

        // =========================================================================================
        // Properties
        // =========================================================================================

        /// <summary>
        /// Gets or sets the editor's text.
        /// </summary>
        public string EditorText
        {
            get { return this.contentTextBox.Text; }
            set { this.contentTextBox.Text = value; }
        }

        // =========================================================================================
        // Methods
        // =========================================================================================

        /// <summary>
        /// Sets up the macros list view.
        /// </summary>
        /// <param name="buildMacros">The collection of build macros to add to the list.</param>
        public void InitializeMacroList(XBuildMacroCollection buildMacros)
        {
            this.macrosListView.Items.Clear();

            foreach (XBuildMacroCollection.MacroNameValuePair pair in buildMacros)
            {
                ListViewItem item = new ListViewItem(new string[] { pair.MacroName, pair.Value }, 0);
                this.macrosListView.Items.Add(item);
            }

            this.macrosListView.AutoResizeColumns(ColumnHeaderAutoResizeStyle.ColumnContent);
        }

        /// <summary>
        /// This method gets called whenever the dialog box is shown.
        /// </summary>
        /// <param name="e">Contains information about the event raised.</param>
        protected override void OnShown(EventArgs e)
        {
            // Set focus and cursor to text box.
            this.contentTextBox.Focus();
            this.contentTextBox.SelectionStart = this.contentTextBox.Text.Length;

            // Disable insert button as no macro is selected by default.
            this.insertButton.Enabled = false;
            base.OnShown(e);
        }

        /// <summary>
        /// Inserts the specified macro name into the text box at the current text selection.
        /// </summary>
        /// <param name="item">The <see cref="ListViewItem"/> that contains the macro name.</param>
        private void InsertMacro(ListViewItem item)
        {
            string macroName = item.Text;
            this.contentTextBox.SelectedText = "$(" + macroName + ")";
            this.contentTextBox.Focus();
        }

        // =========================================================================================
        // Event Handlers
        // =========================================================================================

        private void OnInsertButtonClick(object sender, EventArgs e)
        {
            if (this.macrosListView.SelectedItems.Count > 0)
            {
                this.InsertMacro(this.macrosListView.SelectedItems[0]);
            }
        }

        private void OnMacrosListViewMouseDoubleClick(object sender, MouseEventArgs e)
        {
            ListViewHitTestInfo hitTestInfo = this.macrosListView.HitTest(e.Location);

            if (hitTestInfo.Item != null)
            {
                this.InsertMacro(hitTestInfo.Item);
            }
        }

        private void OnMacrosListViewSelectedIndexChanged(object sender, EventArgs e)
        {
            this.insertButton.Enabled = this.macrosListView.SelectedItems.Count > 0;
        }

    }
}
