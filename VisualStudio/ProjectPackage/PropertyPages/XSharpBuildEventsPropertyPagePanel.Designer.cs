//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

namespace XSharp.Project
{
    partial class XBuildEventsPropertyPagePanel
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(XBuildEventsPropertyPagePanel));
            XSharp.Project.XGroupBox preBuildGroupBox;
            XSharp.Project.XGroupBox postBuildGroupBox;
            System.Windows.Forms.Label runLabel;
            this.preBuildEditor = new XSharp.Project.XBuildEventEditor();
            this.postBuildEditor = new XSharp.Project.XBuildEventEditor();
            this.runPostBuildComboBox = new System.Windows.Forms.ComboBox();
            preBuildGroupBox = new XSharp.Project.XGroupBox();
            postBuildGroupBox = new XSharp.Project.XGroupBox();
            runLabel = new System.Windows.Forms.Label();
            preBuildGroupBox.SuspendLayout();
            postBuildGroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // preBuildEditor
            // 
            resources.ApplyResources(this.preBuildEditor, "preBuildEditor");
            this.preBuildEditor.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.preBuildEditor.Name = "preBuildEditor";
            // 
            // postBuildEditor
            // 
            resources.ApplyResources(this.postBuildEditor, "postBuildEditor");
            this.postBuildEditor.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.postBuildEditor.Name = "postBuildEditor";
            // 
            // runPostBuildComboBox
            // 
            resources.ApplyResources(this.runPostBuildComboBox, "runPostBuildComboBox");
            this.runPostBuildComboBox.BackColor = System.Drawing.Color.WhiteSmoke;
            this.runPostBuildComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.runPostBuildComboBox.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.runPostBuildComboBox.FormattingEnabled = true;
            this.runPostBuildComboBox.Items.AddRange(new object[] {
            resources.GetString("runPostBuildComboBox.Items"),
            resources.GetString("runPostBuildComboBox.Items1"),
            resources.GetString("runPostBuildComboBox.Items2")});
            this.runPostBuildComboBox.Name = "runPostBuildComboBox";
            // 
            // preBuildGroupBox
            // 
            preBuildGroupBox.Controls.Add(this.preBuildEditor);
            preBuildGroupBox.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            resources.ApplyResources(preBuildGroupBox, "preBuildGroupBox");
            preBuildGroupBox.Name = "preBuildGroupBox";
            // 
            // postBuildGroupBox
            // 
            postBuildGroupBox.Controls.Add(this.postBuildEditor);
            postBuildGroupBox.Controls.Add(this.runPostBuildComboBox);
            postBuildGroupBox.Controls.Add(runLabel);
            postBuildGroupBox.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            resources.ApplyResources(postBuildGroupBox, "postBuildGroupBox");
            postBuildGroupBox.Name = "postBuildGroupBox";
            // 
            // runLabel
            // 
            resources.ApplyResources(runLabel, "runLabel");
            runLabel.AutoEllipsis = true;
            runLabel.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            runLabel.Name = "runLabel";
            // 
            // XBuildEventsPropertyPagePanel
            // 
            resources.ApplyResources(this, "$this");
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(postBuildGroupBox);
            this.Controls.Add(preBuildGroupBox);
            this.Name = "XBuildEventsPropertyPagePanel";
            preBuildGroupBox.ResumeLayout(false);
            postBuildGroupBox.ResumeLayout(false);
            postBuildGroupBox.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private XSharp.Project.XBuildEventEditor preBuildEditor;
        private XSharp.Project.XBuildEventEditor postBuildEditor;
        private System.Windows.Forms.ComboBox runPostBuildComboBox;

    }
}
