namespace XSharp.LanguageService.OptionsPages
{
    partial class XSharpSpecialOptions
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
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.chkDisableXSharpProjectReferences = new System.Windows.Forms.CheckBox();
            this.label3 = new System.Windows.Forms.Label();
            this.chkDisableForeignProjectReferences = new System.Windows.Forms.CheckBox();
            this.label1 = new System.Windows.Forms.Label();
            this.chkDisableAssemblyReferences = new System.Windows.Forms.CheckBox();
            this.chkDisableClassViewObjectView = new System.Windows.Forms.CheckBox();
            this.chkDisableEditorDropdowns = new System.Windows.Forms.CheckBox();
            this.chkDisableEntityParsing = new System.Windows.Forms.CheckBox();
            this.chkDisableSyntaxColorization = new System.Windows.Forms.CheckBox();
            this.chkEnableParserLog = new System.Windows.Forms.CheckBox();
            this.chkEnableDatabaseLog = new System.Windows.Forms.CheckBox();
            this.btnOk = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.chkEnableBraceMatchLog = new System.Windows.Forms.CheckBox();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.chkEnableTypeLookupLog = new System.Windows.Forms.CheckBox();
            this.chkEnableReferenceLog = new System.Windows.Forms.CheckBox();
            this.chkEnableOutputPane = new System.Windows.Forms.CheckBox();
            this.chkEnableQuickInfoLog = new System.Windows.Forms.CheckBox();
            this.chkEnableParameterTipsLog = new System.Windows.Forms.CheckBox();
            this.chkEnableCodeCompletionLog = new System.Windows.Forms.CheckBox();
            this.groupBox4 = new System.Windows.Forms.GroupBox();
            this.chkLogToDebug = new System.Windows.Forms.CheckBox();
            this.chkLogToFile = new System.Windows.Forms.CheckBox();
            this.groupBox1.SuspendLayout();
            this.groupBox3.SuspendLayout();
            this.groupBox4.SuspendLayout();
            this.SuspendLayout();
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.chkDisableXSharpProjectReferences);
            this.groupBox1.Controls.Add(this.label3);
            this.groupBox1.Controls.Add(this.chkDisableForeignProjectReferences);
            this.groupBox1.Controls.Add(this.label1);
            this.groupBox1.Controls.Add(this.chkDisableAssemblyReferences);
            this.groupBox1.Controls.Add(this.chkDisableClassViewObjectView);
            this.groupBox1.Controls.Add(this.chkDisableEditorDropdowns);
            this.groupBox1.Controls.Add(this.chkDisableEntityParsing);
            this.groupBox1.Controls.Add(this.chkDisableSyntaxColorization);
            this.groupBox1.Location = new System.Drawing.Point(12, 12);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(373, 245);
            this.groupBox1.TabIndex = 4;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Enable/ Disable various intellisense options";
            // 
            // chkDisableXSharpProjectReferences
            // 
            this.chkDisableXSharpProjectReferences.AutoSize = true;
            this.chkDisableXSharpProjectReferences.Location = new System.Drawing.Point(18, 211);
            this.chkDisableXSharpProjectReferences.Name = "chkDisableXSharpProjectReferences";
            this.chkDisableXSharpProjectReferences.Size = new System.Drawing.Size(243, 17);
            this.chkDisableXSharpProjectReferences.TabIndex = 3;
            this.chkDisableXSharpProjectReferences.Text = "Disable Lookup in XSharp Project References";
            this.chkDisableXSharpProjectReferences.UseVisualStyleBackColor = true;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(15, 146);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(304, 13);
            this.label3.TabIndex = 14;
            this.label3.Text = "Most others require closing and reopening the editor window(s).";
            // 
            // chkDisableForeignProjectReferences
            // 
            this.chkDisableForeignProjectReferences.AutoSize = true;
            this.chkDisableForeignProjectReferences.Location = new System.Drawing.Point(18, 188);
            this.chkDisableForeignProjectReferences.Name = "chkDisableForeignProjectReferences";
            this.chkDisableForeignProjectReferences.Size = new System.Drawing.Size(232, 17);
            this.chkDisableForeignProjectReferences.TabIndex = 2;
            this.chkDisableForeignProjectReferences.Text = "Disable Loading \'Foreign\' project references";
            this.chkDisableForeignProjectReferences.UseVisualStyleBackColor = true;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(15, 130);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(341, 13);
            this.label1.TabIndex = 12;
            this.label1.Text = "Some options (such as ClassView and Objectview) require a VS restart ";
            // 
            // chkDisableAssemblyReferences
            // 
            this.chkDisableAssemblyReferences.AutoSize = true;
            this.chkDisableAssemblyReferences.Location = new System.Drawing.Point(18, 165);
            this.chkDisableAssemblyReferences.Name = "chkDisableAssemblyReferences";
            this.chkDisableAssemblyReferences.Size = new System.Drawing.Size(202, 17);
            this.chkDisableAssemblyReferences.TabIndex = 1;
            this.chkDisableAssemblyReferences.Text = "Disable Loading Assembly Reference";
            this.chkDisableAssemblyReferences.UseVisualStyleBackColor = true;
            // 
            // chkDisableClassViewObjectView
            // 
            this.chkDisableClassViewObjectView.AutoSize = true;
            this.chkDisableClassViewObjectView.Enabled = false;
            this.chkDisableClassViewObjectView.Location = new System.Drawing.Point(18, 87);
            this.chkDisableClassViewObjectView.Name = "chkDisableClassViewObjectView";
            this.chkDisableClassViewObjectView.Size = new System.Drawing.Size(169, 17);
            this.chkDisableClassViewObjectView.TabIndex = 10;
            this.chkDisableClassViewObjectView.Text = "Disable Class and  Objectview";
            this.chkDisableClassViewObjectView.UseVisualStyleBackColor = true;
            // 
            // chkDisableEditorDropdowns
            // 
            this.chkDisableEditorDropdowns.AutoSize = true;
            this.chkDisableEditorDropdowns.Location = new System.Drawing.Point(18, 64);
            this.chkDisableEditorDropdowns.Name = "chkDisableEditorDropdowns";
            this.chkDisableEditorDropdowns.Size = new System.Drawing.Size(148, 17);
            this.chkDisableEditorDropdowns.TabIndex = 9;
            this.chkDisableEditorDropdowns.Text = "Disable Editor Dropdowns";
            this.chkDisableEditorDropdowns.UseVisualStyleBackColor = true;
            // 
            // chkDisableEntityParsing
            // 
            this.chkDisableEntityParsing.AutoSize = true;
            this.chkDisableEntityParsing.Location = new System.Drawing.Point(18, 42);
            this.chkDisableEntityParsing.Name = "chkDisableEntityParsing";
            this.chkDisableEntityParsing.Size = new System.Drawing.Size(128, 17);
            this.chkDisableEntityParsing.TabIndex = 6;
            this.chkDisableEntityParsing.Text = "Disable Entity Parsing";
            this.chkDisableEntityParsing.UseVisualStyleBackColor = true;
            // 
            // chkDisableSyntaxColorization
            // 
            this.chkDisableSyntaxColorization.AutoSize = true;
            this.chkDisableSyntaxColorization.Location = new System.Drawing.Point(18, 19);
            this.chkDisableSyntaxColorization.Name = "chkDisableSyntaxColorization";
            this.chkDisableSyntaxColorization.Size = new System.Drawing.Size(153, 17);
            this.chkDisableSyntaxColorization.TabIndex = 5;
            this.chkDisableSyntaxColorization.Text = "Disable Syntax Colorization";
            this.chkDisableSyntaxColorization.UseVisualStyleBackColor = true;
            // 
            // chkEnableParserLog
            // 
            this.chkEnableParserLog.AutoSize = true;
            this.chkEnableParserLog.Location = new System.Drawing.Point(10, 60);
            this.chkEnableParserLog.Name = "chkEnableParserLog";
            this.chkEnableParserLog.Size = new System.Drawing.Size(136, 17);
            this.chkEnableParserLog.TabIndex = 6;
            this.chkEnableParserLog.Text = "Log background parser";
            this.chkEnableParserLog.UseVisualStyleBackColor = true;
            // 
            // chkEnableDatabaseLog
            // 
            this.chkEnableDatabaseLog.AutoSize = true;
            this.chkEnableDatabaseLog.Location = new System.Drawing.Point(10, 37);
            this.chkEnableDatabaseLog.Name = "chkEnableDatabaseLog";
            this.chkEnableDatabaseLog.Size = new System.Drawing.Size(143, 17);
            this.chkEnableDatabaseLog.TabIndex = 5;
            this.chkEnableDatabaseLog.Text = "Log database operations";
            this.chkEnableDatabaseLog.UseVisualStyleBackColor = true;
            // 
            // btnOk
            // 
            this.btnOk.Location = new System.Drawing.Point(434, 375);
            this.btnOk.Name = "btnOk";
            this.btnOk.Size = new System.Drawing.Size(75, 23);
            this.btnOk.TabIndex = 6;
            this.btnOk.Text = "&Ok";
            this.btnOk.UseVisualStyleBackColor = true;
            this.btnOk.Click += new System.EventHandler(this.btnOk_Click);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.Location = new System.Drawing.Point(536, 375);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(75, 23);
            this.btnCancel.TabIndex = 7;
            this.btnCancel.Text = "&Cancel";
            this.btnCancel.UseVisualStyleBackColor = true;
            // 
            // chkEnableBraceMatchLog
            // 
            this.chkEnableBraceMatchLog.AutoSize = true;
            this.chkEnableBraceMatchLog.Location = new System.Drawing.Point(10, 83);
            this.chkEnableBraceMatchLog.Name = "chkEnableBraceMatchLog";
            this.chkEnableBraceMatchLog.Size = new System.Drawing.Size(120, 17);
            this.chkEnableBraceMatchLog.TabIndex = 7;
            this.chkEnableBraceMatchLog.Text = "Log brace matching";
            this.chkEnableBraceMatchLog.UseVisualStyleBackColor = true;
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.chkEnableTypeLookupLog);
            this.groupBox3.Controls.Add(this.chkEnableReferenceLog);
            this.groupBox3.Controls.Add(this.chkEnableOutputPane);
            this.groupBox3.Controls.Add(this.chkEnableQuickInfoLog);
            this.groupBox3.Controls.Add(this.chkEnableParameterTipsLog);
            this.groupBox3.Controls.Add(this.chkEnableCodeCompletionLog);
            this.groupBox3.Controls.Add(this.chkEnableBraceMatchLog);
            this.groupBox3.Controls.Add(this.chkEnableDatabaseLog);
            this.groupBox3.Controls.Add(this.chkEnableParserLog);
            this.groupBox3.Location = new System.Drawing.Point(405, 25);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(222, 232);
            this.groupBox3.TabIndex = 8;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "Output pane Logging options";
            // 
            // chkEnableTypeLookupLog
            // 
            this.chkEnableTypeLookupLog.AutoSize = true;
            this.chkEnableTypeLookupLog.Location = new System.Drawing.Point(10, 198);
            this.chkEnableTypeLookupLog.Name = "chkEnableTypeLookupLog";
            this.chkEnableTypeLookupLog.Size = new System.Drawing.Size(110, 17);
            this.chkEnableTypeLookupLog.TabIndex = 13;
            this.chkEnableTypeLookupLog.Text = "Log Type Lookup";
            this.chkEnableTypeLookupLog.UseVisualStyleBackColor = true;
            // 
            // chkEnableReferenceLog
            // 
            this.chkEnableReferenceLog.AutoSize = true;
            this.chkEnableReferenceLog.Location = new System.Drawing.Point(10, 175);
            this.chkEnableReferenceLog.Name = "chkEnableReferenceLog";
            this.chkEnableReferenceLog.Size = new System.Drawing.Size(154, 17);
            this.chkEnableReferenceLog.TabIndex = 12;
            this.chkEnableReferenceLog.Text = "Log Handling of references";
            this.chkEnableReferenceLog.UseVisualStyleBackColor = true;
            // 
            // chkEnableOutputPane
            // 
            this.chkEnableOutputPane.AutoSize = true;
            this.chkEnableOutputPane.Location = new System.Drawing.Point(10, 17);
            this.chkEnableOutputPane.Name = "chkEnableOutputPane";
            this.chkEnableOutputPane.Size = new System.Drawing.Size(196, 17);
            this.chkEnableOutputPane.TabIndex = 11;
            this.chkEnableOutputPane.Text = "ENable output pane in Visual Studio";
            this.chkEnableOutputPane.UseVisualStyleBackColor = true;
            // 
            // chkEnableQuickInfoLog
            // 
            this.chkEnableQuickInfoLog.AutoSize = true;
            this.chkEnableQuickInfoLog.Location = new System.Drawing.Point(10, 152);
            this.chkEnableQuickInfoLog.Name = "chkEnableQuickInfoLog";
            this.chkEnableQuickInfoLog.Size = new System.Drawing.Size(173, 17);
            this.chkEnableQuickInfoLog.TabIndex = 10;
            this.chkEnableQuickInfoLog.Text = "Log QuickInfo (tooltip) handling";
            this.chkEnableQuickInfoLog.UseVisualStyleBackColor = true;
            // 
            // chkEnableParameterTipsLog
            // 
            this.chkEnableParameterTipsLog.AutoSize = true;
            this.chkEnableParameterTipsLog.Location = new System.Drawing.Point(10, 129);
            this.chkEnableParameterTipsLog.Name = "chkEnableParameterTipsLog";
            this.chkEnableParameterTipsLog.Size = new System.Drawing.Size(137, 17);
            this.chkEnableParameterTipsLog.TabIndex = 9;
            this.chkEnableParameterTipsLog.Text = "Log parameter handling";
            this.chkEnableParameterTipsLog.UseVisualStyleBackColor = true;
            // 
            // chkEnableCodeCompletionLog
            // 
            this.chkEnableCodeCompletionLog.AutoSize = true;
            this.chkEnableCodeCompletionLog.Location = new System.Drawing.Point(10, 106);
            this.chkEnableCodeCompletionLog.Name = "chkEnableCodeCompletionLog";
            this.chkEnableCodeCompletionLog.Size = new System.Drawing.Size(125, 17);
            this.chkEnableCodeCompletionLog.TabIndex = 8;
            this.chkEnableCodeCompletionLog.Text = "Log code completion";
            this.chkEnableCodeCompletionLog.UseVisualStyleBackColor = true;
            // 
            // groupBox4
            // 
            this.groupBox4.Controls.Add(this.chkLogToDebug);
            this.groupBox4.Controls.Add(this.chkLogToFile);
            this.groupBox4.Location = new System.Drawing.Point(12, 263);
            this.groupBox4.Name = "groupBox4";
            this.groupBox4.Size = new System.Drawing.Size(615, 72);
            this.groupBox4.TabIndex = 9;
            this.groupBox4.TabStop = false;
            this.groupBox4.Text = "Other logging (enables all options above)";
            // 
            // chkLogToDebug
            // 
            this.chkLogToDebug.AutoSize = true;
            this.chkLogToDebug.Location = new System.Drawing.Point(10, 42);
            this.chkLogToDebug.Name = "chkLogToDebug";
            this.chkLogToDebug.Size = new System.Drawing.Size(128, 17);
            this.chkLogToDebug.TabIndex = 15;
            this.chkLogToDebug.Text = "Log to &debug window";
            this.chkLogToDebug.UseVisualStyleBackColor = true;
            // 
            // chkLogToFile
            // 
            this.chkLogToFile.AutoSize = true;
            this.chkLogToFile.Location = new System.Drawing.Point(10, 19);
            this.chkLogToFile.Name = "chkLogToFile";
            this.chkLogToFile.Size = new System.Drawing.Size(72, 17);
            this.chkLogToFile.TabIndex = 14;
            this.chkLogToFile.Text = "Log to &file";
            this.chkLogToFile.UseVisualStyleBackColor = true;
            // 
            // XSharpSpecialOptions
            // 
            this.AcceptButton = this.btnOk;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(639, 410);
            this.Controls.Add(this.groupBox4);
            this.Controls.Add(this.groupBox3);
            this.Controls.Add(this.btnCancel);
            this.Controls.Add(this.btnOk);
            this.Controls.Add(this.groupBox1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Name = "XSharpSpecialOptions";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "XSharpSpecialOptions";
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.groupBox3.ResumeLayout(false);
            this.groupBox3.PerformLayout();
            this.groupBox4.ResumeLayout(false);
            this.groupBox4.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label1;
        internal System.Windows.Forms.CheckBox chkDisableClassViewObjectView;
        internal System.Windows.Forms.CheckBox chkDisableEditorDropdowns;
        internal System.Windows.Forms.CheckBox chkDisableEntityParsing;
        internal System.Windows.Forms.CheckBox chkDisableSyntaxColorization;
        internal System.Windows.Forms.CheckBox chkDisableXSharpProjectReferences;
        internal System.Windows.Forms.CheckBox chkDisableForeignProjectReferences;
        internal System.Windows.Forms.CheckBox chkDisableAssemblyReferences;
        private System.Windows.Forms.Button btnOk;
        private System.Windows.Forms.Button btnCancel;
        internal System.Windows.Forms.CheckBox chkEnableDatabaseLog;
        internal System.Windows.Forms.CheckBox chkEnableParserLog;
        internal System.Windows.Forms.CheckBox chkEnableBraceMatchLog;
        private System.Windows.Forms.GroupBox groupBox3;
        internal System.Windows.Forms.CheckBox chkEnableParameterTipsLog;
        internal System.Windows.Forms.CheckBox chkEnableCodeCompletionLog;
        internal System.Windows.Forms.CheckBox chkEnableQuickInfoLog;
        internal System.Windows.Forms.CheckBox chkEnableOutputPane;
        internal System.Windows.Forms.CheckBox chkEnableTypeLookupLog;
        internal System.Windows.Forms.CheckBox chkEnableReferenceLog;
        private System.Windows.Forms.GroupBox groupBox4;
        internal System.Windows.Forms.CheckBox chkLogToDebug;
        internal System.Windows.Forms.CheckBox chkLogToFile;
    }
}
