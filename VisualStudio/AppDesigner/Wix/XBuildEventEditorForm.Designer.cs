//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

namespace Microsoft.VisualStudio.Project
{
    partial class XBuildEventEditorForm
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.Windows.Forms.ColumnHeader nameColumnHeader;
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(XBuildEventEditorForm));
            System.Windows.Forms.ColumnHeader valueColumnHeader;
            this.okButton = new System.Windows.Forms.Button();
            this.cancelButton = new System.Windows.Forms.Button();
            this.macrosListView = new System.Windows.Forms.ListView();
            this.insertButton = new System.Windows.Forms.Button();
            this.centerPanel = new System.Windows.Forms.Panel();
            this.contentTextBoxPanel = new System.Windows.Forms.Panel();
            this.contentTextBox = new Microsoft.VisualStudio.Project.XBuildEventTextBox();
            this.bottomButtonTableLayoutPanel = new System.Windows.Forms.TableLayoutPanel();
            nameColumnHeader = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            valueColumnHeader = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.centerPanel.SuspendLayout();
            this.contentTextBoxPanel.SuspendLayout();
            this.bottomButtonTableLayoutPanel.SuspendLayout();
            this.SuspendLayout();
            // 
            // nameColumnHeader
            // 
            resources.ApplyResources(nameColumnHeader, "nameColumnHeader");
            // 
            // valueColumnHeader
            // 
            resources.ApplyResources(valueColumnHeader, "valueColumnHeader");
            // 
            // okButton
            // 
            resources.ApplyResources(this.okButton, "okButton");
            this.okButton.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.okButton.Name = "okButton";
            this.okButton.UseVisualStyleBackColor = true;
            // 
            // cancelButton
            // 
            resources.ApplyResources(this.cancelButton, "cancelButton");
            this.cancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.cancelButton.Name = "cancelButton";
            this.cancelButton.UseVisualStyleBackColor = true;
            // 
            // macrosListView
            // 
            this.macrosListView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            nameColumnHeader,
            valueColumnHeader});
            resources.ApplyResources(this.macrosListView, "macrosListView");
            this.macrosListView.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable;
            this.macrosListView.HideSelection = false;
            this.macrosListView.MultiSelect = false;
            this.macrosListView.Name = "macrosListView";
            this.macrosListView.Sorting = System.Windows.Forms.SortOrder.Ascending;
            this.macrosListView.UseCompatibleStateImageBehavior = false;
            this.macrosListView.View = System.Windows.Forms.View.Details;
            this.macrosListView.SelectedIndexChanged += new System.EventHandler(this.OnMacrosListViewSelectedIndexChanged);
            this.macrosListView.MouseDoubleClick += new System.Windows.Forms.MouseEventHandler(this.OnMacrosListViewMouseDoubleClick);
            // 
            // insertButton
            // 
            resources.ApplyResources(this.insertButton, "insertButton");
            this.insertButton.Name = "insertButton";
            this.insertButton.UseVisualStyleBackColor = true;
            this.insertButton.Click += new System.EventHandler(this.OnInsertButtonClick);
            // 
            // centerPanel
            // 
            resources.ApplyResources(this.centerPanel, "centerPanel");
            this.centerPanel.Controls.Add(this.contentTextBoxPanel);
            this.centerPanel.Controls.Add(this.macrosListView);
            this.centerPanel.Name = "centerPanel";
            // 
            // contentTextBoxPanel
            // 
            this.contentTextBoxPanel.Controls.Add(this.contentTextBox);
            resources.ApplyResources(this.contentTextBoxPanel, "contentTextBoxPanel");
            this.contentTextBoxPanel.Name = "contentTextBoxPanel";
            // 
            // contentTextBox
            // 
            resources.ApplyResources(this.contentTextBox, "contentTextBox");
            this.contentTextBox.Name = "contentTextBox";
            // 
            // bottomButtonTableLayoutPanel
            // 
            resources.ApplyResources(this.bottomButtonTableLayoutPanel, "bottomButtonTableLayoutPanel");
            this.bottomButtonTableLayoutPanel.Controls.Add(this.okButton, 0, 0);
            this.bottomButtonTableLayoutPanel.Controls.Add(this.cancelButton, 1, 0);
            this.bottomButtonTableLayoutPanel.Name = "bottomButtonTableLayoutPanel";
            // 
            // XBuildEventEditorForm
            // 
            this.AcceptButton = this.okButton;
            resources.ApplyResources(this, "$this");
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.cancelButton;
            this.Controls.Add(this.bottomButtonTableLayoutPanel);
            this.Controls.Add(this.centerPanel);
            this.Controls.Add(this.insertButton);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "XBuildEventEditorForm";
            this.ShowIcon = false;
            this.ShowInTaskbar = false;
            this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Show;
            this.centerPanel.ResumeLayout(false);
            this.contentTextBoxPanel.ResumeLayout(false);
            this.contentTextBoxPanel.PerformLayout();
            this.bottomButtonTableLayoutPanel.ResumeLayout(false);
            this.bottomButtonTableLayoutPanel.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private XBuildEventTextBox contentTextBox;
        private System.Windows.Forms.ListView macrosListView;
        private System.Windows.Forms.Button insertButton;
        private System.Windows.Forms.Panel centerPanel;
        private System.Windows.Forms.Panel contentTextBoxPanel;
        private System.Windows.Forms.TableLayoutPanel bottomButtonTableLayoutPanel;
        private System.Windows.Forms.Button okButton;
        private System.Windows.Forms.Button cancelButton;
    }
}
