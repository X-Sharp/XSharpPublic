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
            this.chkShowAfterChar = new System.Windows.Forms.CheckBox();
            this.chkDotAsUniversalSelector = new System.Windows.Forms.CheckBox();
            this.grpCase = new System.Windows.Forms.GroupBox();
            this.chkAlignMethod = new System.Windows.Forms.CheckBox();
            this.chkAlignDoCase = new System.Windows.Forms.CheckBox();
            this.grpKeywordCase = new System.Windows.Forms.GroupBox();
            this.rbTitle = new System.Windows.Forms.RadioButton();
            this.rbNone = new System.Windows.Forms.RadioButton();
            this.rbUpper = new System.Windows.Forms.RadioButton();
            this.rbLower = new System.Windows.Forms.RadioButton();
            this.chkIdentifierCase = new System.Windows.Forms.CheckBox();
            this.grpNavigationBars = new System.Windows.Forms.GroupBox();
            this.chkSortNavBar = new System.Windows.Forms.CheckBox();
            this.chkIncludeFields = new System.Windows.Forms.CheckBox();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.chkDisableCodeCompletion = new System.Windows.Forms.CheckBox();
            this.label3 = new System.Windows.Forms.Label();
            this.label1 = new System.Windows.Forms.Label();
            this.chkDisableClassViewObjectView = new System.Windows.Forms.CheckBox();
            this.chkDisableEditorDropdowns = new System.Windows.Forms.CheckBox();
            this.chkDisableCaseSynchronization = new System.Windows.Forms.CheckBox();
            this.chkDisableRegions = new System.Windows.Forms.CheckBox();
            this.chkDisableEntityParsing = new System.Windows.Forms.CheckBox();
            this.chkDisableSyntaxColorization = new System.Windows.Forms.CheckBox();
            this.chkDisableQuickInfo = new System.Windows.Forms.CheckBox();
            this.chkDisablePeekDefinition = new System.Windows.Forms.CheckBox();
            this.chkDisableLigtBulb = new System.Windows.Forms.CheckBox();
            this.chkDisableHighlightWord = new System.Windows.Forms.CheckBox();
            this.chkBraceMatching = new System.Windows.Forms.CheckBox();
            this.chkDisableParameters = new System.Windows.Forms.CheckBox();
            this.grpCompletionListTabs.SuspendLayout();
            this.grpCase.SuspendLayout();
            this.grpKeywordCase.SuspendLayout();
            this.grpNavigationBars.SuspendLayout();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // chkCompletionListtabs
            // 
            this.chkCompletionListtabs.AutoSize = true;
            this.chkCompletionListtabs.Location = new System.Drawing.Point(13, 18);
            this.chkCompletionListtabs.Name = "chkCompletionListtabs";
            this.chkCompletionListtabs.Size = new System.Drawing.Size(286, 17);
            this.chkCompletionListtabs.TabIndex = 0;
            this.chkCompletionListtabs.Text = "Organize completionlists in tabs with different item types";
            this.chkCompletionListtabs.UseVisualStyleBackColor = true;
            this.chkCompletionListtabs.CheckedChanged += new System.EventHandler(this.chkCompletionListtabs_CheckedChanged);
            // 
            // grpCompletionListTabs
            // 
            this.grpCompletionListTabs.Controls.Add(this.chkShowAfterChar);
            this.grpCompletionListTabs.Controls.Add(this.chkDotAsUniversalSelector);
            this.grpCompletionListTabs.Controls.Add(this.chkCompletionListtabs);
            this.grpCompletionListTabs.Location = new System.Drawing.Point(12, 6);
            this.grpCompletionListTabs.Name = "grpCompletionListTabs";
            this.grpCompletionListTabs.Size = new System.Drawing.Size(373, 87);
            this.grpCompletionListTabs.TabIndex = 0;
            this.grpCompletionListTabs.TabStop = false;
            this.grpCompletionListTabs.Text = "Completion Lists";
            // 
            // chkShowAfterChar
            // 
            this.chkShowAfterChar.AutoSize = true;
            this.chkShowAfterChar.Location = new System.Drawing.Point(13, 59);
            this.chkShowAfterChar.Name = "chkShowAfterChar";
            this.chkShowAfterChar.Size = new System.Drawing.Size(243, 17);
            this.chkShowAfterChar.TabIndex = 2;
            this.chkShowAfterChar.Text = "Show Completion list after a character is typed";
            this.chkShowAfterChar.UseVisualStyleBackColor = true;
            this.chkShowAfterChar.CheckedChanged += new System.EventHandler(this.chkShowAfterChar_CheckedChanged);
            // 
            // chkDotAsUniversalSelector
            // 
            this.chkDotAsUniversalSelector.AutoSize = true;
            this.chkDotAsUniversalSelector.Location = new System.Drawing.Point(13, 38);
            this.chkDotAsUniversalSelector.Name = "chkDotAsUniversalSelector";
            this.chkDotAsUniversalSelector.Size = new System.Drawing.Size(241, 17);
            this.chkDotAsUniversalSelector.TabIndex = 1;
            this.chkDotAsUniversalSelector.Text = "Dot (.) as universal selector (Core dialect only)";
            this.chkDotAsUniversalSelector.UseVisualStyleBackColor = true;
            this.chkDotAsUniversalSelector.CheckedChanged += new System.EventHandler(this.chkDotAsUniversalSelector_CheckedChanged);
            // 
            // grpCase
            // 
            this.grpCase.Controls.Add(this.chkAlignMethod);
            this.grpCase.Controls.Add(this.chkAlignDoCase);
            this.grpCase.Controls.Add(this.grpKeywordCase);
            this.grpCase.Controls.Add(this.chkIdentifierCase);
            this.grpCase.Location = new System.Drawing.Point(12, 94);
            this.grpCase.Name = "grpCase";
            this.grpCase.Size = new System.Drawing.Size(373, 129);
            this.grpCase.TabIndex = 1;
            this.grpCase.TabStop = false;
            this.grpCase.Text = "Document Formatting";
            // 
            // chkAlignMethod
            // 
            this.chkAlignMethod.AutoSize = true;
            this.chkAlignMethod.Location = new System.Drawing.Point(13, 100);
            this.chkAlignMethod.Name = "chkAlignMethod";
            this.chkAlignMethod.Size = new System.Drawing.Size(316, 17);
            this.chkAlignMethod.TabIndex = 4;
            this.chkAlignMethod.Text = "Align inner content in METHOD, FUNCTION && PROCEDURE";
            this.chkAlignMethod.UseVisualStyleBackColor = true;
            this.chkAlignMethod.CheckedChanged += new System.EventHandler(this.chkAlignMethod_CheckedChanged);
            // 
            // chkAlignDoCase
            // 
            this.chkAlignDoCase.AutoSize = true;
            this.chkAlignDoCase.Location = new System.Drawing.Point(13, 78);
            this.chkAlignDoCase.Name = "chkAlignDoCase";
            this.chkAlignDoCase.Size = new System.Drawing.Size(241, 17);
            this.chkAlignDoCase.TabIndex = 3;
            this.chkAlignDoCase.Text = "Align inner content in DO CASE ... ENDCASE";
            this.chkAlignDoCase.UseVisualStyleBackColor = true;
            this.chkAlignDoCase.CheckedChanged += new System.EventHandler(this.chkAlignDoCase_CheckedChanged);
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
            this.grpKeywordCase.TabIndex = 1;
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
            this.chkIdentifierCase.Location = new System.Drawing.Point(13, 57);
            this.chkIdentifierCase.Name = "chkIdentifierCase";
            this.chkIdentifierCase.Size = new System.Drawing.Size(171, 17);
            this.chkIdentifierCase.TabIndex = 2;
            this.chkIdentifierCase.Text = "&Identifier Case Synchronization";
            this.chkIdentifierCase.UseVisualStyleBackColor = true;
            this.chkIdentifierCase.CheckedChanged += new System.EventHandler(this.chkIdentifierCase_CheckedChanged);
            // 
            // grpNavigationBars
            // 
            this.grpNavigationBars.Controls.Add(this.chkSortNavBar);
            this.grpNavigationBars.Controls.Add(this.chkIncludeFields);
            this.grpNavigationBars.Location = new System.Drawing.Point(12, 224);
            this.grpNavigationBars.Name = "grpNavigationBars";
            this.grpNavigationBars.Size = new System.Drawing.Size(373, 64);
            this.grpNavigationBars.TabIndex = 2;
            this.grpNavigationBars.TabStop = false;
            this.grpNavigationBars.Text = "Navigation Bars";
            // 
            // chkSortNavBar
            // 
            this.chkSortNavBar.AutoSize = true;
            this.chkSortNavBar.Location = new System.Drawing.Point(13, 38);
            this.chkSortNavBar.Name = "chkSortNavBar";
            this.chkSortNavBar.Size = new System.Drawing.Size(116, 17);
            this.chkSortNavBar.TabIndex = 1;
            this.chkSortNavBar.Text = "Sort Items by name";
            this.chkSortNavBar.UseVisualStyleBackColor = true;
            this.chkSortNavBar.CheckedChanged += new System.EventHandler(this.chkSortNavBar_CheckedChanged);
            // 
            // chkIncludeFields
            // 
            this.chkIncludeFields.AutoSize = true;
            this.chkIncludeFields.Location = new System.Drawing.Point(13, 17);
            this.chkIncludeFields.Name = "chkIncludeFields";
            this.chkIncludeFields.Size = new System.Drawing.Size(182, 17);
            this.chkIncludeFields.TabIndex = 0;
            this.chkIncludeFields.Text = "Include fields (instance variables)";
            this.chkIncludeFields.UseVisualStyleBackColor = true;
            this.chkIncludeFields.CheckedChanged += new System.EventHandler(this.chkIncludeFields_CheckedChanged);
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.chkDisableCodeCompletion);
            this.groupBox1.Controls.Add(this.label3);
            this.groupBox1.Controls.Add(this.label1);
            this.groupBox1.Controls.Add(this.chkDisableClassViewObjectView);
            this.groupBox1.Controls.Add(this.chkDisableEditorDropdowns);
            this.groupBox1.Controls.Add(this.chkDisableCaseSynchronization);
            this.groupBox1.Controls.Add(this.chkDisableRegions);
            this.groupBox1.Controls.Add(this.chkDisableEntityParsing);
            this.groupBox1.Controls.Add(this.chkDisableSyntaxColorization);
            this.groupBox1.Controls.Add(this.chkDisableQuickInfo);
            this.groupBox1.Controls.Add(this.chkDisablePeekDefinition);
            this.groupBox1.Controls.Add(this.chkDisableLigtBulb);
            this.groupBox1.Controls.Add(this.chkDisableHighlightWord);
            this.groupBox1.Controls.Add(this.chkBraceMatching);
            this.groupBox1.Controls.Add(this.chkDisableParameters);
            this.groupBox1.Location = new System.Drawing.Point(12, 294);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(373, 245);
            this.groupBox1.TabIndex = 3;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Enable/ Disable various intellisense options";
            // 
            // chkDisableCodeCompletion
            // 
            this.chkDisableCodeCompletion.AutoSize = true;
            this.chkDisableCodeCompletion.Location = new System.Drawing.Point(13, 168);
            this.chkDisableCodeCompletion.Name = "chkDisableCodeCompletion";
            this.chkDisableCodeCompletion.Size = new System.Drawing.Size(144, 17);
            this.chkDisableCodeCompletion.TabIndex = 15;
            this.chkDisableCodeCompletion.Text = "Disable Code Completion";
            this.chkDisableCodeCompletion.UseVisualStyleBackColor = true;
            this.chkDisableCodeCompletion.CheckedChanged += new System.EventHandler(this.Control_CheckedChanged);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(17, 215);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(304, 13);
            this.label3.TabIndex = 14;
            this.label3.Text = "Most others require closing and reopening the editor window(s).";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(17, 194);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(341, 13);
            this.label1.TabIndex = 12;
            this.label1.Text = "Some options (such as ClassView and Objectview) require a VS restart ";
            // 
            // chkDisableClassViewObjectView
            // 
            this.chkDisableClassViewObjectView.AutoSize = true;
            this.chkDisableClassViewObjectView.Location = new System.Drawing.Point(177, 122);
            this.chkDisableClassViewObjectView.Name = "chkDisableClassViewObjectView";
            this.chkDisableClassViewObjectView.Size = new System.Drawing.Size(169, 17);
            this.chkDisableClassViewObjectView.TabIndex = 10;
            this.chkDisableClassViewObjectView.Text = "Disable Class and  Objectview";
            this.chkDisableClassViewObjectView.UseVisualStyleBackColor = true;
            this.chkDisableClassViewObjectView.CheckedChanged += new System.EventHandler(this.Control_CheckedChanged);
            // 
            // chkDisableEditorDropdowns
            // 
            this.chkDisableEditorDropdowns.AutoSize = true;
            this.chkDisableEditorDropdowns.Location = new System.Drawing.Point(177, 99);
            this.chkDisableEditorDropdowns.Name = "chkDisableEditorDropdowns";
            this.chkDisableEditorDropdowns.Size = new System.Drawing.Size(148, 17);
            this.chkDisableEditorDropdowns.TabIndex = 9;
            this.chkDisableEditorDropdowns.Text = "Disable Editor Dropdowns";
            this.chkDisableEditorDropdowns.UseVisualStyleBackColor = true;
            this.chkDisableEditorDropdowns.CheckedChanged += new System.EventHandler(this.Control_CheckedChanged);
            // 
            // chkDisableCaseSynchronization
            // 
            this.chkDisableCaseSynchronization.AutoSize = true;
            this.chkDisableCaseSynchronization.Location = new System.Drawing.Point(177, 76);
            this.chkDisableCaseSynchronization.Name = "chkDisableCaseSynchronization";
            this.chkDisableCaseSynchronization.Size = new System.Drawing.Size(166, 17);
            this.chkDisableCaseSynchronization.TabIndex = 8;
            this.chkDisableCaseSynchronization.Text = "Disable Case Synchronization";
            this.chkDisableCaseSynchronization.UseVisualStyleBackColor = true;
            this.chkDisableCaseSynchronization.CheckedChanged += new System.EventHandler(this.Control_CheckedChanged);
            // 
            // chkDisableRegions
            // 
            this.chkDisableRegions.AutoSize = true;
            this.chkDisableRegions.Location = new System.Drawing.Point(177, 53);
            this.chkDisableRegions.Name = "chkDisableRegions";
            this.chkDisableRegions.Size = new System.Drawing.Size(103, 17);
            this.chkDisableRegions.TabIndex = 7;
            this.chkDisableRegions.Text = "Disable Regions";
            this.chkDisableRegions.UseVisualStyleBackColor = true;
            this.chkDisableRegions.CheckedChanged += new System.EventHandler(this.Control_CheckedChanged);
            // 
            // chkDisableEntityParsing
            // 
            this.chkDisableEntityParsing.AutoSize = true;
            this.chkDisableEntityParsing.Location = new System.Drawing.Point(177, 30);
            this.chkDisableEntityParsing.Name = "chkDisableEntityParsing";
            this.chkDisableEntityParsing.Size = new System.Drawing.Size(128, 17);
            this.chkDisableEntityParsing.TabIndex = 6;
            this.chkDisableEntityParsing.Text = "Disable Entity Parsing";
            this.chkDisableEntityParsing.UseVisualStyleBackColor = true;
            this.chkDisableEntityParsing.CheckedChanged += new System.EventHandler(this.Control_CheckedChanged);
            // 
            // chkDisableSyntaxColorization
            // 
            this.chkDisableSyntaxColorization.AutoSize = true;
            this.chkDisableSyntaxColorization.Location = new System.Drawing.Point(13, 145);
            this.chkDisableSyntaxColorization.Name = "chkDisableSyntaxColorization";
            this.chkDisableSyntaxColorization.Size = new System.Drawing.Size(153, 17);
            this.chkDisableSyntaxColorization.TabIndex = 5;
            this.chkDisableSyntaxColorization.Text = "Disable Syntax Colorization";
            this.chkDisableSyntaxColorization.UseVisualStyleBackColor = true;
            this.chkDisableSyntaxColorization.CheckedChanged += new System.EventHandler(this.Control_CheckedChanged);
            // 
            // chkDisableQuickInfo
            // 
            this.chkDisableQuickInfo.AutoSize = true;
            this.chkDisableQuickInfo.Location = new System.Drawing.Point(13, 122);
            this.chkDisableQuickInfo.Name = "chkDisableQuickInfo";
            this.chkDisableQuickInfo.Size = new System.Drawing.Size(113, 17);
            this.chkDisableQuickInfo.TabIndex = 4;
            this.chkDisableQuickInfo.Text = "Disable Quick Info";
            this.chkDisableQuickInfo.UseVisualStyleBackColor = true;
            this.chkDisableQuickInfo.CheckedChanged += new System.EventHandler(this.Control_CheckedChanged);
            // 
            // chkDisablePeekDefinition
            // 
            this.chkDisablePeekDefinition.AutoSize = true;
            this.chkDisablePeekDefinition.Location = new System.Drawing.Point(13, 99);
            this.chkDisablePeekDefinition.Name = "chkDisablePeekDefinition";
            this.chkDisablePeekDefinition.Size = new System.Drawing.Size(136, 17);
            this.chkDisablePeekDefinition.TabIndex = 3;
            this.chkDisablePeekDefinition.Text = "Disable Peek Definition";
            this.chkDisablePeekDefinition.UseVisualStyleBackColor = true;
            this.chkDisablePeekDefinition.CheckedChanged += new System.EventHandler(this.Control_CheckedChanged);
            // 
            // chkDisableLigtBulb
            // 
            this.chkDisableLigtBulb.AutoSize = true;
            this.chkDisableLigtBulb.Location = new System.Drawing.Point(13, 76);
            this.chkDisableLigtBulb.Name = "chkDisableLigtBulb";
            this.chkDisableLigtBulb.Size = new System.Drawing.Size(116, 17);
            this.chkDisableLigtBulb.TabIndex = 2;
            this.chkDisableLigtBulb.Text = "Disable Light Bulbs";
            this.chkDisableLigtBulb.UseVisualStyleBackColor = true;
            this.chkDisableLigtBulb.CheckedChanged += new System.EventHandler(this.Control_CheckedChanged);
            // 
            // chkDisableHighlightWord
            // 
            this.chkDisableHighlightWord.AutoSize = true;
            this.chkDisableHighlightWord.Location = new System.Drawing.Point(13, 53);
            this.chkDisableHighlightWord.Name = "chkDisableHighlightWord";
            this.chkDisableHighlightWord.Size = new System.Drawing.Size(134, 17);
            this.chkDisableHighlightWord.TabIndex = 1;
            this.chkDisableHighlightWord.Text = "Disable Highlight Word";
            this.chkDisableHighlightWord.UseVisualStyleBackColor = true;
            this.chkDisableHighlightWord.CheckedChanged += new System.EventHandler(this.Control_CheckedChanged);
            // 
            // chkBraceMatching
            // 
            this.chkBraceMatching.AutoSize = true;
            this.chkBraceMatching.Location = new System.Drawing.Point(13, 30);
            this.chkBraceMatching.Name = "chkBraceMatching";
            this.chkBraceMatching.Size = new System.Drawing.Size(139, 17);
            this.chkBraceMatching.TabIndex = 0;
            this.chkBraceMatching.Text = "Disable Brace Matching";
            this.chkBraceMatching.UseVisualStyleBackColor = true;
            this.chkBraceMatching.CheckedChanged += new System.EventHandler(this.Control_CheckedChanged);
            // 
            // chkDisableParameters
            // 
            this.chkDisableParameters.AutoSize = true;
            this.chkDisableParameters.Location = new System.Drawing.Point(177, 145);
            this.chkDisableParameters.Name = "chkDisableParameters";
            this.chkDisableParameters.Size = new System.Drawing.Size(135, 17);
            this.chkDisableParameters.TabIndex = 11;
            this.chkDisableParameters.Text = "Disable Parameter Tips";
            this.chkDisableParameters.UseVisualStyleBackColor = true;
            this.chkDisableParameters.CheckedChanged += new System.EventHandler(this.Control_CheckedChanged);
            // 
            // IntellisenseOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.grpNavigationBars);
            this.Controls.Add(this.grpCase);
            this.Controls.Add(this.grpCompletionListTabs);
            this.Name = "IntellisenseOptionsControl";
            this.Size = new System.Drawing.Size(397, 542);
            this.grpCompletionListTabs.ResumeLayout(false);
            this.grpCompletionListTabs.PerformLayout();
            this.grpCase.ResumeLayout(false);
            this.grpCase.PerformLayout();
            this.grpKeywordCase.ResumeLayout(false);
            this.grpKeywordCase.PerformLayout();
            this.grpNavigationBars.ResumeLayout(false);
            this.grpNavigationBars.PerformLayout();
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
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
        private System.Windows.Forms.RadioButton rbTitle;
        private System.Windows.Forms.CheckBox chkAlignMethod;
        private System.Windows.Forms.CheckBox chkAlignDoCase;
        private System.Windows.Forms.CheckBox chkDotAsUniversalSelector;
        private System.Windows.Forms.GroupBox grpNavigationBars;
        private System.Windows.Forms.CheckBox chkSortNavBar;
        private System.Windows.Forms.CheckBox chkIncludeFields;
        private System.Windows.Forms.CheckBox chkShowAfterChar;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.CheckBox chkDisableClassViewObjectView;
        private System.Windows.Forms.CheckBox chkDisableEditorDropdowns;
        private System.Windows.Forms.CheckBox chkDisableCaseSynchronization;
        private System.Windows.Forms.CheckBox chkDisableRegions;
        private System.Windows.Forms.CheckBox chkDisableEntityParsing;
        private System.Windows.Forms.CheckBox chkDisableSyntaxColorization;
        private System.Windows.Forms.CheckBox chkDisableQuickInfo;
        private System.Windows.Forms.CheckBox chkDisablePeekDefinition;
        private System.Windows.Forms.CheckBox chkDisableLigtBulb;
        private System.Windows.Forms.CheckBox chkDisableHighlightWord;
        private System.Windows.Forms.CheckBox chkBraceMatching;
        private System.Windows.Forms.CheckBox chkDisableParameters;
        private System.Windows.Forms.CheckBox chkDisableCodeCompletion;
    }
}
