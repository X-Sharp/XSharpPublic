﻿namespace XSharp.LanguageService.OptionsPages
{
    partial class IndentingOptionsControl
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
            this.components = new System.ComponentModel.Container();
            System.Windows.Forms.Label label1;
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.panel1 = new System.Windows.Forms.Panel();
            this.label2 = new System.Windows.Forms.Label();
            this.codeSample = new System.Windows.Forms.RichTextBox();
            this.listIndentStyle = new System.Windows.Forms.ListView();
            label1 = new System.Windows.Forms.Label();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // label1
            // 
            label1.Location = new System.Drawing.Point(0, 0);
            label1.Name = "label1";
            label1.Size = new System.Drawing.Size(100, 23);
            label1.TabIndex = 0;
            // 
            // panel1
            // 
            this.panel1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.panel1.Controls.Add(this.label2);
            this.panel1.Controls.Add(this.codeSample);
            this.panel1.Controls.Add(this.listIndentStyle);
            this.panel1.Location = new System.Drawing.Point(0, 0);
            this.panel1.Margin = new System.Windows.Forms.Padding(4);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(560, 357);
            this.panel1.TabIndex = 0;
            // 
            // label2
            // 
            this.label2.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.label2.Location = new System.Drawing.Point(13, 12);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(460, 18);
            this.label2.TabIndex = 2;
            this.label2.Text = "These settings are ONLY used in the FormatDocument process";
            // 
            // codeSample
            // 
            this.codeSample.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.codeSample.BackColor = System.Drawing.SystemColors.Window;
            this.codeSample.Location = new System.Drawing.Point(16, 184);
            this.codeSample.Name = "codeSample";
            this.codeSample.ReadOnly = true;
            this.codeSample.Size = new System.Drawing.Size(526, 153);
            this.codeSample.TabIndex = 1;
            this.codeSample.Text = "";
            // 
            // listIndentStyle
            // 
            this.listIndentStyle.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.listIndentStyle.CheckBoxes = true;
            this.listIndentStyle.HideSelection = false;
            this.listIndentStyle.Location = new System.Drawing.Point(16, 32);
            this.listIndentStyle.Name = "listIndentStyle";
            this.listIndentStyle.Size = new System.Drawing.Size(526, 133);
            this.listIndentStyle.TabIndex = 0;
            this.listIndentStyle.UseCompatibleStateImageBehavior = false;
            this.listIndentStyle.View = System.Windows.Forms.View.List;
            this.listIndentStyle.ItemChecked += new System.Windows.Forms.ItemCheckedEventHandler(this.listIndentStyle_ItemChecked);
            this.listIndentStyle.SelectedIndexChanged += new System.EventHandler(this.listIndentStyle_SelectedIndexChanged);
            // 
            // IndentingOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoScroll = true;
            this.Controls.Add(this.panel1);
            this.Margin = new System.Windows.Forms.Padding(4);
            this.Name = "IndentingOptionsControl";
            this.Size = new System.Drawing.Size(580, 374);
            this.panel1.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.ToolTip toolTip1;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.ListView listIndentStyle;
        private System.Windows.Forms.RichTextBox codeSample;
        private System.Windows.Forms.Label label2;
    }
}
