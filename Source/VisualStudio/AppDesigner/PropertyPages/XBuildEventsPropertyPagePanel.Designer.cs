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
            Microsoft.VisualStudio.Project.XGroupBox preBuildGroupBox;
            Microsoft.VisualStudio.Project.XGroupBox postBuildGroupBox;
            System.Windows.Forms.Label runLabel;
            this.preBuildEditor = new Microsoft.VisualStudio.Project.XBuildEventEditor();
            this.postBuildEditor = new Microsoft.VisualStudio.Project.XBuildEventEditor();
            this.runPostBuildComboBox = new System.Windows.Forms.ComboBox();
            preBuildGroupBox = new Microsoft.VisualStudio.Project.XGroupBox();
            postBuildGroupBox = new Microsoft.VisualStudio.Project.XGroupBox();
            runLabel = new System.Windows.Forms.Label();
            preBuildGroupBox.SuspendLayout();
            postBuildGroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // preBuildGroupBox
            // 
            preBuildGroupBox.Controls.Add(this.preBuildEditor);
            preBuildGroupBox.Location = new System.Drawing.Point(3, 3);
            preBuildGroupBox.Name = "preBuildGroupBox";
            preBuildGroupBox.Padding = new System.Windows.Forms.Padding(20, 20, 20, 0);
            preBuildGroupBox.Size = new System.Drawing.Size(544, 152);
            preBuildGroupBox.TabIndex = 0;
            preBuildGroupBox.TabStop = false;
            preBuildGroupBox.Text = "P&re-build Event Command Line";
            // 
            // preBuildEditor
            // 
            this.preBuildEditor.ButtonText = "Ed&it Pre-build...";
            this.preBuildEditor.EditorFormText = "Pre-build Event Command Line";
            this.preBuildEditor.Location = new System.Drawing.Point(20, 20);
            this.preBuildEditor.Margin = new System.Windows.Forms.Padding(4);
            this.preBuildEditor.Name = "preBuildEditor";
            this.preBuildEditor.Size = new System.Drawing.Size(521, 132);
            this.preBuildEditor.TabIndex = 0;
            // 
            // postBuildGroupBox
            // 
            postBuildGroupBox.Controls.Add(this.postBuildEditor);
            postBuildGroupBox.Controls.Add(this.runPostBuildComboBox);
            postBuildGroupBox.Controls.Add(runLabel);
            postBuildGroupBox.Location = new System.Drawing.Point(3, 164);
            postBuildGroupBox.Name = "postBuildGroupBox";
            postBuildGroupBox.Padding = new System.Windows.Forms.Padding(20, 20, 20, 0);
            postBuildGroupBox.Size = new System.Drawing.Size(544, 197);
            postBuildGroupBox.TabIndex = 1;
            postBuildGroupBox.TabStop = false;
            postBuildGroupBox.Text = "P&ost-build Event Command Line";
            // 
            // postBuildEditor
            // 
            this.postBuildEditor.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.postBuildEditor.ButtonText = "Edit Post-b&uild...";
            this.postBuildEditor.EditorFormText = "Post-build Event Command Line";
            this.postBuildEditor.Location = new System.Drawing.Point(20, 20);
            this.postBuildEditor.Margin = new System.Windows.Forms.Padding(4);
            this.postBuildEditor.Name = "postBuildEditor";
            this.postBuildEditor.Size = new System.Drawing.Size(521, 132);
            this.postBuildEditor.TabIndex = 0;
            // 
            // runPostBuildComboBox
            // 
            this.runPostBuildComboBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.runPostBuildComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.runPostBuildComboBox.FormattingEnabled = true;
            this.runPostBuildComboBox.Location = new System.Drawing.Point(20, 171);
            this.runPostBuildComboBox.Name = "runPostBuildComboBox";
            this.runPostBuildComboBox.Size = new System.Drawing.Size(521, 21);
            this.runPostBuildComboBox.TabIndex = 2;
            // 
            // runLabel
            // 
            runLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            runLabel.AutoEllipsis = true;
            runLabel.AutoSize = true;
            runLabel.Location = new System.Drawing.Point(17, 155);
            runLabel.Name = "runLabel";
            runLabel.Size = new System.Drawing.Size(126, 13);
            runLabel.TabIndex = 1;
            runLabel.Text = "Ru&n the post-build event:";
            // 
            // XBuildEventsPropertyPagePanel
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(postBuildGroupBox);
            this.Controls.Add(preBuildGroupBox);
            this.Margin = new System.Windows.Forms.Padding(5, 5, 5, 5);
            this.Name = "XBuildEventsPropertyPagePanel";
            this.Size = new System.Drawing.Size(550, 364);
            preBuildGroupBox.ResumeLayout(false);
            postBuildGroupBox.ResumeLayout(false);
            postBuildGroupBox.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private Microsoft.VisualStudio.Project.XBuildEventEditor preBuildEditor;
        private Microsoft.VisualStudio.Project.XBuildEventEditor postBuildEditor;
        private System.Windows.Forms.ComboBox runPostBuildComboBox;

    }
}
