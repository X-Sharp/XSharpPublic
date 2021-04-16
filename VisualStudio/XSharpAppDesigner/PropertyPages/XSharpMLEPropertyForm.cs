//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Windows.Forms;
using Microsoft.VisualStudio.Project;

namespace XSharp.Project
{
    public partial class XSharpMLEPropertyForm : System.Windows.Forms.Form
    {
        public XSharpMLEPropertyForm()
        {
            InitializeComponent();
        }

        internal void SetMacros(XBuildMacroCollection mc)
        {
            MacrosList.BeginUpdate();
            MacrosList.Items.Clear();

            foreach (var p in mc)
            {
                ListViewItem i = new ListViewItem();

                i.Text = String.Concat("$(", p.MacroName, ")");
                i.SubItems.Add(p.Value);

                MacrosList.Items.Add(i);
            }

            MacrosList.Items[0].Selected = true;
            MacrosList.AutoResizeColumns(ColumnHeaderAutoResizeStyle.ColumnContent);
            MacrosList.EndUpdate();
        }


        private void InsertMacroButton_Click(object sender, EventArgs e)
        {
            PropertyText.Paste(MacrosList.SelectedItems[0].Text);
        }

        private void OKButton_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void MacrosList_DoubleClick(object sender, EventArgs e)
        {
            PropertyText.Paste(MacrosList.SelectedItems[0].Text);
        }
    }
}
