//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

namespace Microsoft.VisualStudio.Project
{
    partial class XBuildEventEditor
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

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(XBuildEventEditor));
            this.editButton = new System.Windows.Forms.Button();
            this.contentTextBox = new Microsoft.VisualStudio.Project.XBuildEventTextBox();
            this.bottomButtonPanel = new System.Windows.Forms.Panel();
            this.bottomButtonPanel.SuspendLayout();
            this.SuspendLayout();
            // 
            // editButton
            // 
            resources.ApplyResources(this.editButton, "editButton");
            this.editButton.Name = "editButton";
            this.editButton.UseVisualStyleBackColor = true;
            this.editButton.Click += new System.EventHandler(this.OnEditButtonClick);
            // 
            // contentTextBox
            // 
            resources.ApplyResources(this.contentTextBox, "contentTextBox");
            this.contentTextBox.Name = "contentTextBox";
            // 
            // bottomButtonPanel
            // 
            this.bottomButtonPanel.Controls.Add(this.editButton);
            resources.ApplyResources(this.bottomButtonPanel, "bottomButtonPanel");
            this.bottomButtonPanel.Name = "bottomButtonPanel";
            // 
            // XBuildEventEditor
            // 
            resources.ApplyResources(this, "$this");
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.contentTextBox);
            this.Controls.Add(this.bottomButtonPanel);
            this.Name = "XBuildEventEditor";
            this.bottomButtonPanel.ResumeLayout(false);
            this.bottomButtonPanel.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private XBuildEventTextBox contentTextBox;
        private System.Windows.Forms.Button editButton;
        private System.Windows.Forms.Panel bottomButtonPanel;
    }
}
