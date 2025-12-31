//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

namespace XSharp.Project
{
    using Microsoft.VisualStudio.Project;

    using System;
    using System.Collections.Generic;
    using System.Collections.ObjectModel;
    using System.Drawing;
    using System.Linq;
    using System.Windows.Forms;

    using XSharp.Settings;



    /// <summary>
    /// Property page contents for the Candle Settings page.
    /// </summary>
    internal partial class XGlobalUsingsPropertyPagePanel : XPropertyPagePanel
    {
        // =========================================================================================
        // Member Variables
        // =========================================================================================

        XProjectNode Project = null;
        ObservableCollection<UsingInfo> usingList = new ObservableCollection<UsingInfo>();
        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildEventsPropertyPagePanel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The parent property page to which this is bound.</param>
        public XGlobalUsingsPropertyPagePanel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
            this.InitializeComponent();
            this.grpImpicitGlobalUsings.Text = GlobalUsingsPropertyPagePanel.captImplicitUsings;
            this.chkImplicitUsings.Tag = XSharpProjectFileConstants.ImplicitUsings;
            this.chkImplicitUsings.Text = GlobalUsingsPropertyPagePanel.captEnableImplicitUsings;
            Color defaultBackground = SystemColors.ButtonFace;
            Color defaultForeground = SystemColors.WindowText;
            UpdateWindowColors(this, defaultBackground, defaultForeground);
            chkShowImported.Checked = true;
        }

        protected void ShowItems()
        {
            Project = this.ParentPropertyPage.ProjectMgr;
            if (Project == null)
            { return; }
            var items = Project.BuildProject.AllEvaluatedItems.Where(i => i.ItemType == "Using");
            usingList.Clear();
            foreach (var item in items)
            {
                if (item.IsImported && !chkShowImported.Checked)
                {
                    continue;
                }
                var usingInfo = new UsingInfo();
                usingInfo.Namespace = item.EvaluatedInclude;
                usingInfo.Alias = item.GetMetadataValue("Alias");
                var isStatic = item.GetMetadataValue("Static");
                usingInfo.Static = isStatic?.ToLower() == "true";
                usingInfo.Imported = item.IsImported;
                usingList.Add(usingInfo);
            }
            gridUsings.DataSource = usingList;

        }
        protected override void OnVisibleChanged(EventArgs e)
        {
            base.OnVisibleChanged(e);
            ShowItems();
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

        private void gridUsings_RowEnter(object sender, DataGridViewCellEventArgs e)
        {
            var row = gridUsings.Rows[e.RowIndex];
            var usingInfo = row.DataBoundItem as UsingInfo;
            if (usingInfo != null)
            {
                row.ReadOnly = usingInfo.Imported;
            }
        }

        private void chkShowImported_Click(object sender, EventArgs e)
        {
            ShowItems();
        }

    }
    public class UsingInfo
    {
        public string Namespace { get; set; }
        public string Alias { get; set; }
        public bool Static { get; set; }
        public bool Imported { get; set; }
        public string Delete => "Delete";
    }

}
