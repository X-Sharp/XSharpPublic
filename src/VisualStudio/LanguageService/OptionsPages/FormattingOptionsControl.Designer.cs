namespace XSharp.LanguageService.OptionsPages
{
    partial class FormattingOptionsControl
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FormattingOptionsControl));
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.chkTrimTrailngWhiteSpace = new System.Windows.Forms.CheckBox();
            this.chkInsertFinalNewLine = new System.Windows.Forms.CheckBox();
            this.chkSynchronizeUDCKeywords = new System.Windows.Forms.CheckBox();
            this.chkIdentifierCase = new System.Windows.Forms.CheckBox();
            this.panel1 = new System.Windows.Forms.Panel();
            this.grpCase = new System.Windows.Forms.GroupBox();
            this.tbExample = new System.Windows.Forms.TextBox();
            this.grpKeywordCase = new System.Windows.Forms.GroupBox();
            this.rbTitle = new System.Windows.Forms.RadioButton();
            this.rbNone = new System.Windows.Forms.RadioButton();
            this.rbUpper = new System.Windows.Forms.RadioButton();
            this.rbLower = new System.Windows.Forms.RadioButton();
            label1 = new System.Windows.Forms.Label();
            this.panel1.SuspendLayout();
            this.grpCase.SuspendLayout();
            this.grpKeywordCase.SuspendLayout();
            this.SuspendLayout();
            // 
            // label1
            // 
            label1.Location = new System.Drawing.Point(0, 0);
            label1.Name = "label1";
            label1.Size = new System.Drawing.Size(100, 23);
            label1.TabIndex = 0;
            // 
            // chkTrimTrailngWhiteSpace
            // 
            this.chkTrimTrailngWhiteSpace.AutoSize = true;
            this.chkTrimTrailngWhiteSpace.Location = new System.Drawing.Point(13, 105);
            this.chkTrimTrailngWhiteSpace.Name = "chkTrimTrailngWhiteSpace";
            this.chkTrimTrailngWhiteSpace.Size = new System.Drawing.Size(143, 17);
            this.chkTrimTrailngWhiteSpace.TabIndex = 4;
            this.chkTrimTrailngWhiteSpace.Text = "Trim Trailing Whitespace";
            this.toolTip1.SetToolTip(this.chkTrimTrailngWhiteSpace, "When you select this then all the lines will be \"trimmed\" when you save the file." +
        "");
            this.chkTrimTrailngWhiteSpace.UseVisualStyleBackColor = true;
            // 
            // chkInsertFinalNewLine
            // 
            this.chkInsertFinalNewLine.AutoSize = true;
            this.chkInsertFinalNewLine.Location = new System.Drawing.Point(13, 128);
            this.chkInsertFinalNewLine.Name = "chkInsertFinalNewLine";
            this.chkInsertFinalNewLine.Size = new System.Drawing.Size(122, 17);
            this.chkInsertFinalNewLine.TabIndex = 5;
            this.chkInsertFinalNewLine.Text = "Insert Final NewLine";
            this.toolTip1.SetToolTip(this.chkInsertFinalNewLine, "When you select this then the editor will add a CRLF after the last line of code " +
        "when needed.");
            this.chkInsertFinalNewLine.UseVisualStyleBackColor = true;
            // 
            // chkSynchronizeUDCKeywords
            // 
            this.chkSynchronizeUDCKeywords.AutoSize = true;
            this.chkSynchronizeUDCKeywords.Location = new System.Drawing.Point(13, 59);
            this.chkSynchronizeUDCKeywords.Name = "chkSynchronizeUDCKeywords";
            this.chkSynchronizeUDCKeywords.Size = new System.Drawing.Size(191, 17);
            this.chkSynchronizeUDCKeywords.TabIndex = 2;
            this.chkSynchronizeUDCKeywords.Text = "Sychronize case of &UDC Keywords";
            this.toolTip1.SetToolTip(this.chkSynchronizeUDCKeywords, "When you enable this option then User Defined keywords inside UDCs will follow th" +
        "e capitalization rules for the built-in keywords");
            this.chkSynchronizeUDCKeywords.UseVisualStyleBackColor = true;
            // 
            // chkIdentifierCase
            // 
            this.chkIdentifierCase.AutoSize = true;
            this.chkIdentifierCase.Location = new System.Drawing.Point(13, 82);
            this.chkIdentifierCase.Name = "chkIdentifierCase";
            this.chkIdentifierCase.Size = new System.Drawing.Size(171, 17);
            this.chkIdentifierCase.TabIndex = 3;
            this.chkIdentifierCase.Text = "&Identifier Case Synchronization";
            this.toolTip1.SetToolTip(this.chkIdentifierCase, resources.GetString("chkIdentifierCase.ToolTip"));
            this.chkIdentifierCase.UseVisualStyleBackColor = true;
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.grpCase);
            this.panel1.Location = new System.Drawing.Point(0, 0);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(381, 186);
            this.panel1.TabIndex = 0;
            // 
            // grpCase
            // 
            this.grpCase.Controls.Add(this.tbExample);
            this.grpCase.Controls.Add(this.chkSynchronizeUDCKeywords);
            this.grpCase.Controls.Add(this.chkInsertFinalNewLine);
            this.grpCase.Controls.Add(this.chkTrimTrailngWhiteSpace);
            this.grpCase.Controls.Add(this.grpKeywordCase);
            this.grpCase.Controls.Add(this.chkIdentifierCase);
            this.grpCase.Location = new System.Drawing.Point(3, 14);
            this.grpCase.Name = "grpCase";
            this.grpCase.Size = new System.Drawing.Size(373, 158);
            this.grpCase.TabIndex = 0;
            this.grpCase.TabStop = false;
            this.grpCase.Text = "Document Formatting";
            // 
            // tbExample
            // 
            this.tbExample.BackColor = System.Drawing.Color.White;
            this.tbExample.Font = new System.Drawing.Font("Courier New", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.tbExample.ForeColor = System.Drawing.Color.Red;
            this.tbExample.Location = new System.Drawing.Point(264, 27);
            this.tbExample.Name = "tbExample";
            this.tbExample.ReadOnly = true;
            this.tbExample.Size = new System.Drawing.Size(103, 22);
            this.tbExample.TabIndex = 1;
            this.tbExample.TabStop = false;
            this.tbExample.Text = "FUNCTION";
            // 
            // grpKeywordCase
            // 
            this.grpKeywordCase.Controls.Add(this.rbTitle);
            this.grpKeywordCase.Controls.Add(this.rbNone);
            this.grpKeywordCase.Controls.Add(this.rbUpper);
            this.grpKeywordCase.Controls.Add(this.rbLower);
            this.grpKeywordCase.Location = new System.Drawing.Point(7, 16);
            this.grpKeywordCase.Name = "grpKeywordCase";
            this.grpKeywordCase.Size = new System.Drawing.Size(237, 36);
            this.grpKeywordCase.TabIndex = 0;
            this.grpKeywordCase.TabStop = false;
            this.grpKeywordCase.Text = "&Keyword Case Synchronization";
            // 
            // rbTitle
            // 
            this.rbTitle.AutoSize = true;
            this.rbTitle.Location = new System.Drawing.Point(183, 15);
            this.rbTitle.Name = "rbTitle";
            this.rbTitle.Size = new System.Drawing.Size(45, 17);
            this.rbTitle.TabIndex = 3;
            this.rbTitle.TabStop = true;
            this.rbTitle.Text = "&Title";
            this.rbTitle.UseVisualStyleBackColor = true;
            this.rbTitle.CheckedChanged += new System.EventHandler(this.caseChanged);
            // 
            // rbNone
            // 
            this.rbNone.AutoSize = true;
            this.rbNone.Location = new System.Drawing.Point(6, 15);
            this.rbNone.Name = "rbNone";
            this.rbNone.Size = new System.Drawing.Size(51, 17);
            this.rbNone.TabIndex = 0;
            this.rbNone.TabStop = true;
            this.rbNone.Text = "&None";
            this.rbNone.UseVisualStyleBackColor = true;
            this.rbNone.CheckedChanged += new System.EventHandler(this.caseChanged);
            // 
            // rbUpper
            // 
            this.rbUpper.AutoSize = true;
            this.rbUpper.Location = new System.Drawing.Point(59, 15);
            this.rbUpper.Name = "rbUpper";
            this.rbUpper.Size = new System.Drawing.Size(62, 17);
            this.rbUpper.TabIndex = 1;
            this.rbUpper.TabStop = true;
            this.rbUpper.Text = "&UPPER";
            this.rbUpper.UseVisualStyleBackColor = true;
            this.rbUpper.CheckedChanged += new System.EventHandler(this.caseChanged);
            // 
            // rbLower
            // 
            this.rbLower.AutoSize = true;
            this.rbLower.Location = new System.Drawing.Point(127, 15);
            this.rbLower.Name = "rbLower";
            this.rbLower.Size = new System.Drawing.Size(50, 17);
            this.rbLower.TabIndex = 2;
            this.rbLower.TabStop = true;
            this.rbLower.Text = "&lower";
            this.rbLower.UseVisualStyleBackColor = true;
            this.rbLower.CheckedChanged += new System.EventHandler(this.caseChanged);
            // 
            // FormattingOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoScroll = true;
            this.Controls.Add(this.panel1);
            this.Name = "FormattingOptionsControl";
            this.Size = new System.Drawing.Size(389, 204);
            this.panel1.ResumeLayout(false);
            this.grpCase.ResumeLayout(false);
            this.grpCase.PerformLayout();
            this.grpKeywordCase.ResumeLayout(false);
            this.grpKeywordCase.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.ToolTip toolTip1;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.GroupBox grpCase;
        private System.Windows.Forms.GroupBox grpKeywordCase;
        private System.Windows.Forms.RadioButton rbTitle;
        private System.Windows.Forms.RadioButton rbNone;
        private System.Windows.Forms.RadioButton rbUpper;
        private System.Windows.Forms.RadioButton rbLower;
        private System.Windows.Forms.CheckBox chkIdentifierCase;
        private System.Windows.Forms.CheckBox chkInsertFinalNewLine;
        private System.Windows.Forms.CheckBox chkTrimTrailngWhiteSpace;
        private System.Windows.Forms.CheckBox chkSynchronizeUDCKeywords;
        private System.Windows.Forms.TextBox tbExample;
    }
}
