namespace XSharp.Project.OptionsPages
{
    partial class IntellisenseOptionsControl
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
            this.chkCompletionListtabs = new System.Windows.Forms.CheckBox();
            this.grpCompletionListTabs = new System.Windows.Forms.GroupBox();
            this.grpCase = new System.Windows.Forms.GroupBox();
            this.rbNone = new System.Windows.Forms.RadioButton();
            this.rbUpper = new System.Windows.Forms.RadioButton();
            this.rbLower = new System.Windows.Forms.RadioButton();
            this.lblKeywordCase = new System.Windows.Forms.Label();
            this.chkIdentifierCase = new System.Windows.Forms.CheckBox();
            this.grpKeywordCase = new System.Windows.Forms.GroupBox();
            this.grpCompletionListTabs.SuspendLayout();
            this.grpCase.SuspendLayout();
            this.grpKeywordCase.SuspendLayout();
            this.SuspendLayout();
            // 
            // chkCompletionListtabs
            // 
            this.chkCompletionListtabs.AutoSize = true;
            this.chkCompletionListtabs.Location = new System.Drawing.Point(13, 20);
            this.chkCompletionListtabs.Name = "chkCompletionListtabs";
            this.chkCompletionListtabs.Size = new System.Drawing.Size(286, 17);
            this.chkCompletionListtabs.TabIndex = 0;
            this.chkCompletionListtabs.Text = "Organize completionlists in tabs with different item types";
            this.chkCompletionListtabs.UseVisualStyleBackColor = true;
            this.chkCompletionListtabs.CheckedChanged += new System.EventHandler(this.chkCompletionListtabs_CheckedChanged);
            // 
            // grpCompletionListTabs
            // 
            this.grpCompletionListTabs.Controls.Add(this.chkCompletionListtabs);
            this.grpCompletionListTabs.Location = new System.Drawing.Point(12, 12);
            this.grpCompletionListTabs.Name = "grpCompletionListTabs";
            this.grpCompletionListTabs.Size = new System.Drawing.Size(461, 46);
            this.grpCompletionListTabs.TabIndex = 0;
            this.grpCompletionListTabs.TabStop = false;
            this.grpCompletionListTabs.Text = "Completion Lists";
            // 
            // grpCase
            // 
            this.grpCase.Controls.Add(this.grpKeywordCase);
            this.grpCase.Controls.Add(this.chkIdentifierCase);
            this.grpCase.Controls.Add(this.lblKeywordCase);
            this.grpCase.Location = new System.Drawing.Point(12, 64);
            this.grpCase.Name = "grpCase";
            this.grpCase.Size = new System.Drawing.Size(461, 96);
            this.grpCase.TabIndex = 1;
            this.grpCase.TabStop = false;
            this.grpCase.Text = "Case Synchronization";
            // 
            // rbNone
            // 
            this.rbNone.AutoSize = true;
            this.rbNone.Location = new System.Drawing.Point(19, 12);
            this.rbNone.Name = "rbNone";
            this.rbNone.Size = new System.Drawing.Size(51, 17);
            this.rbNone.TabIndex = 0;
            this.rbNone.TabStop = true;
            this.rbNone.Text = "&None";
            this.rbNone.UseVisualStyleBackColor = true;
            // 
            // rbUpper
            // 
            this.rbUpper.AutoSize = true;
            this.rbUpper.Location = new System.Drawing.Point(80, 12);
            this.rbUpper.Name = "rbUpper";
            this.rbUpper.Size = new System.Drawing.Size(93, 17);
            this.rbUpper.TabIndex = 1;
            this.rbUpper.TabStop = true;
            this.rbUpper.Text = "&UPPER CASE";
            this.rbUpper.UseVisualStyleBackColor = true;
            // 
            // rbLower
            // 
            this.rbLower.AutoSize = true;
            this.rbLower.Location = new System.Drawing.Point(180, 12);
            this.rbLower.Name = "rbLower";
            this.rbLower.Size = new System.Drawing.Size(76, 17);
            this.rbLower.TabIndex = 2;
            this.rbLower.TabStop = true;
            this.rbLower.Text = "&lower case";
            this.rbLower.UseVisualStyleBackColor = true;
            // 
            // lblKeywordCase
            // 
            this.lblKeywordCase.AutoSize = true;
            this.lblKeywordCase.Location = new System.Drawing.Point(13, 21);
            this.lblKeywordCase.Name = "lblKeywordCase";
            this.lblKeywordCase.Size = new System.Drawing.Size(153, 13);
            this.lblKeywordCase.TabIndex = 0;
            this.lblKeywordCase.Text = "&Keyword Case Synchronization";
            // 
            // chkIdentifierCase
            // 
            this.chkIdentifierCase.AutoSize = true;
            this.chkIdentifierCase.Location = new System.Drawing.Point(13, 50);
            this.chkIdentifierCase.Name = "chkIdentifierCase";
            this.chkIdentifierCase.Size = new System.Drawing.Size(171, 17);
            this.chkIdentifierCase.TabIndex = 2;
            this.chkIdentifierCase.Text = "&Identifier Case Synchronization";
            this.chkIdentifierCase.UseVisualStyleBackColor = true;
            // 
            // grpKeywordCase
            // 
            this.grpKeywordCase.Controls.Add(this.rbNone);
            this.grpKeywordCase.Controls.Add(this.rbUpper);
            this.grpKeywordCase.Controls.Add(this.rbLower);
            this.grpKeywordCase.Location = new System.Drawing.Point(198, 10);
            this.grpKeywordCase.Name = "grpKeywordCase";
            this.grpKeywordCase.Size = new System.Drawing.Size(257, 36);
            this.grpKeywordCase.TabIndex = 1;
            this.grpKeywordCase.TabStop = false;
            // 
            // IntellisenseOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.grpCase);
            this.Controls.Add(this.grpCompletionListTabs);
            this.Name = "IntellisenseOptionsControl";
            this.Size = new System.Drawing.Size(486, 173);
            this.grpCompletionListTabs.ResumeLayout(false);
            this.grpCompletionListTabs.PerformLayout();
            this.grpCase.ResumeLayout(false);
            this.grpCase.PerformLayout();
            this.grpKeywordCase.ResumeLayout(false);
            this.grpKeywordCase.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.CheckBox chkCompletionListtabs;
        private System.Windows.Forms.GroupBox grpCompletionListTabs;
        private System.Windows.Forms.GroupBox grpCase;
        private System.Windows.Forms.GroupBox grpKeywordCase;
        private System.Windows.Forms.RadioButton rbNone;
        private System.Windows.Forms.RadioButton rbUpper;
        private System.Windows.Forms.RadioButton rbLower;
        private System.Windows.Forms.CheckBox chkIdentifierCase;
        private System.Windows.Forms.Label lblKeywordCase;
    }
}
