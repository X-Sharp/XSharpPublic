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
            this.label2 = new System.Windows.Forms.Label();
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.chkAlignMethod = new System.Windows.Forms.CheckBox();
            this.chkAlignDoCase = new System.Windows.Forms.CheckBox();
            this.chkTrimTrailngWhiteSpace = new System.Windows.Forms.CheckBox();
            this.chkInsertFinalNewLine = new System.Windows.Forms.CheckBox();
            this.panel1 = new System.Windows.Forms.Panel();
            this.grpCase = new System.Windows.Forms.GroupBox();
            this.chkSynchronizeUDCKeywords = new System.Windows.Forms.CheckBox();
            this.multiFactor = new System.Windows.Forms.TextBox();
            this.grpKeywordCase = new System.Windows.Forms.GroupBox();
            this.rbTitle = new System.Windows.Forms.RadioButton();
            this.rbNone = new System.Windows.Forms.RadioButton();
            this.rbUpper = new System.Windows.Forms.RadioButton();
            this.rbLower = new System.Windows.Forms.RadioButton();
            this.chkIdentifierCase = new System.Windows.Forms.CheckBox();
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
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(10, 147);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(192, 13);
            this.label2.TabIndex = 5;
            this.label2.Text = "MultiLine Statement Indentation factor :";
            this.toolTip1.SetToolTip(this.label2, "Specify the number of tab stops the editor should indent for multiline statements" +
        "");
            // 
            // chkAlignMethod
            // 
            this.chkAlignMethod.AutoSize = true;
            this.chkAlignMethod.Location = new System.Drawing.Point(13, 125);
            this.chkAlignMethod.Name = "chkAlignMethod";
            this.chkAlignMethod.Size = new System.Drawing.Size(316, 17);
            this.chkAlignMethod.TabIndex = 4;
            this.chkAlignMethod.Text = "Align inner content in METHOD, FUNCTION && PROCEDURE";
            this.toolTip1.SetToolTip(this.chkAlignMethod, "When you select this option then the statement lines inside a method or function " +
        "will have the same indent level as the method. Otherwise they will be indented o" +
        "ne level more.");
            this.chkAlignMethod.UseVisualStyleBackColor = true;
            this.chkAlignMethod.CheckedChanged += new System.EventHandler(this.chkAlignMethod_CheckedChanged);
            // 
            // chkAlignDoCase
            // 
            this.chkAlignDoCase.AutoSize = true;
            this.chkAlignDoCase.Location = new System.Drawing.Point(13, 103);
            this.chkAlignDoCase.Name = "chkAlignDoCase";
            this.chkAlignDoCase.Size = new System.Drawing.Size(241, 17);
            this.chkAlignDoCase.TabIndex = 3;
            this.chkAlignDoCase.Text = "Align inner content in DO CASE ... ENDCASE";
            this.toolTip1.SetToolTip(this.chkAlignDoCase, "When you select this then the CASE keywords will line up with the DO keyword from" +
        " a DO CASE statement or the SWITCH keyword from a SWITCH statement");
            this.chkAlignDoCase.UseVisualStyleBackColor = true;
            this.chkAlignDoCase.CheckedChanged += new System.EventHandler(this.chkAlignDoCase_CheckedChanged);
            // 
            // chkTrimTrailngWhiteSpace
            // 
            this.chkTrimTrailngWhiteSpace.AutoSize = true;
            this.chkTrimTrailngWhiteSpace.Location = new System.Drawing.Point(13, 168);
            this.chkTrimTrailngWhiteSpace.Name = "chkTrimTrailngWhiteSpace";
            this.chkTrimTrailngWhiteSpace.Size = new System.Drawing.Size(143, 17);
            this.chkTrimTrailngWhiteSpace.TabIndex = 7;
            this.chkTrimTrailngWhiteSpace.Text = "Trim Trailing Whitespace";
            this.toolTip1.SetToolTip(this.chkTrimTrailngWhiteSpace, "When you select this then all the lines will be \"trimmed\" when you save the file." +
        "");
            this.chkTrimTrailngWhiteSpace.UseVisualStyleBackColor = true;
            this.chkTrimTrailngWhiteSpace.CheckedChanged += new System.EventHandler(this.chkTrimTrailngWhiteSpace_CheckedChanged);
            // 
            // chkInsertFinalNewLine
            // 
            this.chkInsertFinalNewLine.AutoSize = true;
            this.chkInsertFinalNewLine.Location = new System.Drawing.Point(13, 191);
            this.chkInsertFinalNewLine.Name = "chkInsertFinalNewLine";
            this.chkInsertFinalNewLine.Size = new System.Drawing.Size(122, 17);
            this.chkInsertFinalNewLine.TabIndex = 8;
            this.chkInsertFinalNewLine.Text = "Insert Final NewLine";
            this.toolTip1.SetToolTip(this.chkInsertFinalNewLine, "When you select this then the editor will add a CRLF after the last line of code " +
        "when needed.");
            this.chkInsertFinalNewLine.UseVisualStyleBackColor = true;
            this.chkInsertFinalNewLine.CheckedChanged += new System.EventHandler(this.chkInsertFinalNewLine_CheckedChanged);
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.grpCase);
            this.panel1.Location = new System.Drawing.Point(0, 0);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(381, 251);
            this.panel1.TabIndex = 0;
            // 
            // grpCase
            // 
            this.grpCase.Controls.Add(this.chkSynchronizeUDCKeywords);
            this.grpCase.Controls.Add(this.chkInsertFinalNewLine);
            this.grpCase.Controls.Add(this.chkTrimTrailngWhiteSpace);
            this.grpCase.Controls.Add(this.label2);
            this.grpCase.Controls.Add(this.multiFactor);
            this.grpCase.Controls.Add(this.chkAlignMethod);
            this.grpCase.Controls.Add(this.chkAlignDoCase);
            this.grpCase.Controls.Add(this.grpKeywordCase);
            this.grpCase.Controls.Add(this.chkIdentifierCase);
            this.grpCase.Location = new System.Drawing.Point(3, 14);
            this.grpCase.Name = "grpCase";
            this.grpCase.Size = new System.Drawing.Size(373, 221);
            this.grpCase.TabIndex = 0;
            this.grpCase.TabStop = false;
            this.grpCase.Text = "Document Formatting";
            // 
            // chkSynchronizeUDCKeywords
            // 
            this.chkSynchronizeUDCKeywords.AutoSize = true;
            this.chkSynchronizeUDCKeywords.Location = new System.Drawing.Point(13, 59);
            this.chkSynchronizeUDCKeywords.Name = "chkSynchronizeUDCKeywords";
            this.chkSynchronizeUDCKeywords.Size = new System.Drawing.Size(191, 17);
            this.chkSynchronizeUDCKeywords.TabIndex = 1;
            this.chkSynchronizeUDCKeywords.Text = "Sychronize case of &UDC Keywords";
            this.toolTip1.SetToolTip(this.chkSynchronizeUDCKeywords, "When you enable this option then User Defined keywords inside UDCs will follow th" +
        "e capitalization rules for the built-in keywords");
            this.chkSynchronizeUDCKeywords.UseVisualStyleBackColor = true;
            this.chkSynchronizeUDCKeywords.CheckedChanged += new System.EventHandler(this.chkSynchronizeUDCKeywords_CheckedChanged);
            // 
            // multiFactor
            // 
            this.multiFactor.Location = new System.Drawing.Point(220, 143);
            this.multiFactor.Margin = new System.Windows.Forms.Padding(2);
            this.multiFactor.Name = "multiFactor";
            this.multiFactor.Size = new System.Drawing.Size(27, 20);
            this.multiFactor.TabIndex = 6;
            this.multiFactor.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.toolTip1.SetToolTip(this.multiFactor, "Specify the number of tab stops the editor should indent for multiline statements" +
        "");
            this.multiFactor.TextChanged += new System.EventHandler(this.multiFactor_TextChanged);
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
            this.rbTitle.CheckedChanged += new System.EventHandler(this.kwCaseChanged);
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
            this.rbNone.CheckedChanged += new System.EventHandler(this.kwCaseChanged);
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
            this.rbUpper.CheckedChanged += new System.EventHandler(this.kwCaseChanged);
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
            this.rbLower.CheckedChanged += new System.EventHandler(this.kwCaseChanged);
            // 
            // chkIdentifierCase
            // 
            this.chkIdentifierCase.AutoSize = true;
            this.chkIdentifierCase.Enabled = false;
            this.chkIdentifierCase.Location = new System.Drawing.Point(13, 82);
            this.chkIdentifierCase.Name = "chkIdentifierCase";
            this.chkIdentifierCase.Size = new System.Drawing.Size(171, 17);
            this.chkIdentifierCase.TabIndex = 2;
            this.chkIdentifierCase.Text = "&Identifier Case Synchronization";
            this.toolTip1.SetToolTip(this.chkIdentifierCase, resources.GetString("chkIdentifierCase.ToolTip"));
            this.chkIdentifierCase.UseVisualStyleBackColor = true;
            this.chkIdentifierCase.CheckedChanged += new System.EventHandler(this.chkIdentifierCase_CheckedChanged);
            // 
            // FormattingOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoScroll = true;
            this.Controls.Add(this.panel1);
            this.Name = "FormattingOptionsControl";
            this.Size = new System.Drawing.Size(389, 281);
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
        private System.Windows.Forms.TextBox multiFactor;
        private System.Windows.Forms.CheckBox chkAlignMethod;
        private System.Windows.Forms.CheckBox chkAlignDoCase;
        private System.Windows.Forms.GroupBox grpKeywordCase;
        private System.Windows.Forms.RadioButton rbTitle;
        private System.Windows.Forms.RadioButton rbNone;
        private System.Windows.Forms.RadioButton rbUpper;
        private System.Windows.Forms.RadioButton rbLower;
        private System.Windows.Forms.CheckBox chkIdentifierCase;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.CheckBox chkInsertFinalNewLine;
        private System.Windows.Forms.CheckBox chkTrimTrailngWhiteSpace;
        private System.Windows.Forms.CheckBox chkSynchronizeUDCKeywords;
    }
}
