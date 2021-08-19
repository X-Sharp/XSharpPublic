namespace XSharp.LanguageService.OptionsPages
{
    partial class OtherOptionsControl
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
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.chkSingleLineDividers = new System.Windows.Forms.CheckBox();
            this.chkShowDividers = new System.Windows.Forms.CheckBox();
            this.grpOther = new System.Windows.Forms.GroupBox();
            this.grpOther.SuspendLayout();
            this.SuspendLayout();
            // 
            // chkSingleLineDividers
            // 
            this.chkSingleLineDividers.AutoSize = true;
            this.chkSingleLineDividers.Location = new System.Drawing.Point(20, 50);
            this.chkSingleLineDividers.Name = "chkSingleLineDividers";
            this.chkSingleLineDividers.Size = new System.Drawing.Size(260, 17);
            this.chkSingleLineDividers.TabIndex = 1;
            this.chkSingleLineDividers.Text = "Also show &Entity Dividers for \"Single Line\" entities";
            this.toolTip1.SetToolTip(this.chkSingleLineDividers, "This applies to GLOBALs, DEFINEs, DELEGATES, CLASS Variables, MEMBERs of structures etc");
            this.chkSingleLineDividers.UseVisualStyleBackColor = true;
            this.chkSingleLineDividers.CheckedChanged += new System.EventHandler(this.chkSingleLineDividers_CheckedChanged);
            // 
            // chkShowDividers
            // 
            this.chkShowDividers.AutoSize = true;
            this.chkShowDividers.Location = new System.Drawing.Point(20, 27);
            this.chkShowDividers.Name = "chkShowDividers";
            this.chkShowDividers.Size = new System.Drawing.Size(247, 17);
            this.chkShowDividers.TabIndex = 0;
            this.chkShowDividers.Text = "Show Entity &Dividers in the Source Code Editor";
            this.toolTip1.SetToolTip(this.chkShowDividers, "The entity dividers will have the color that is defined in the Color setting as  " +
        "\"Outlining Margin Vertical Rule");
            this.chkShowDividers.UseVisualStyleBackColor = true;
            this.chkShowDividers.CheckedChanged += new System.EventHandler(this.chkShowDividers_CheckedChanged);
            // 
            // grpOther
            // 
            this.grpOther.Controls.Add(this.chkSingleLineDividers);
            this.grpOther.Controls.Add(this.chkShowDividers);
            this.grpOther.Location = new System.Drawing.Point(3, 3);
            this.grpOther.Name = "grpOther";
            this.grpOther.Size = new System.Drawing.Size(401, 252);
            this.grpOther.TabIndex = 1;
            this.grpOther.TabStop = false;
            this.grpOther.Text = "Other Options";
            // 
            // OtherOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoScroll = true;
            this.Controls.Add(this.grpOther);
            this.Name = "OtherOptionsControl";
            this.Size = new System.Drawing.Size(424, 278);
            this.grpOther.ResumeLayout(false);
            this.grpOther.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.ToolTip toolTip1;
        private System.Windows.Forms.GroupBox grpOther;
        private System.Windows.Forms.CheckBox chkSingleLineDividers;
        private System.Windows.Forms.CheckBox chkShowDividers;

    
    }
}
