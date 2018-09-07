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
            this.chkKeywordsInAll = new System.Windows.Forms.CheckBox();
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
            this.chkShowMembersOfCurrentType = new System.Windows.Forms.CheckBox();
            this.chkSortNavBar = new System.Windows.Forms.CheckBox();
            this.chkIncludeFields = new System.Windows.Forms.CheckBox();
            this.btnShowMeTheMagic = new System.Windows.Forms.Button();
            this.grpCompletionListTabs.SuspendLayout();
            this.grpCase.SuspendLayout();
            this.grpKeywordCase.SuspendLayout();
            this.grpNavigationBars.SuspendLayout();
            this.SuspendLayout();
            // 
            // chkCompletionListtabs
            // 
            this.chkCompletionListtabs.AutoSize = true;
            this.chkCompletionListtabs.Location = new System.Drawing.Point(17, 22);
            this.chkCompletionListtabs.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.chkCompletionListtabs.Name = "chkCompletionListtabs";
            this.chkCompletionListtabs.Size = new System.Drawing.Size(382, 21);
            this.chkCompletionListtabs.TabIndex = 0;
            this.chkCompletionListtabs.Text = "Organize completionlists in tabs with different item types";
            this.chkCompletionListtabs.UseVisualStyleBackColor = true;
            this.chkCompletionListtabs.CheckedChanged += new System.EventHandler(this.chkCompletionListtabs_CheckedChanged);
            // 
            // grpCompletionListTabs
            // 
            this.grpCompletionListTabs.Controls.Add(this.chkKeywordsInAll);
            this.grpCompletionListTabs.Controls.Add(this.chkShowAfterChar);
            this.grpCompletionListTabs.Controls.Add(this.chkDotAsUniversalSelector);
            this.grpCompletionListTabs.Controls.Add(this.chkCompletionListtabs);
            this.grpCompletionListTabs.Location = new System.Drawing.Point(16, 7);
            this.grpCompletionListTabs.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.grpCompletionListTabs.Name = "grpCompletionListTabs";
            this.grpCompletionListTabs.Padding = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.grpCompletionListTabs.Size = new System.Drawing.Size(497, 127);
            this.grpCompletionListTabs.TabIndex = 0;
            this.grpCompletionListTabs.TabStop = false;
            this.grpCompletionListTabs.Text = "Completion Lists";
            // 
            // chkKeywordsInAll
            // 
            this.chkKeywordsInAll.AutoSize = true;
            this.chkKeywordsInAll.Location = new System.Drawing.Point(17, 48);
            this.chkKeywordsInAll.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.chkKeywordsInAll.Name = "chkKeywordsInAll";
            this.chkKeywordsInAll.Size = new System.Drawing.Size(219, 21);
            this.chkKeywordsInAll.TabIndex = 3;
            this.chkKeywordsInAll.Text = "Add Keywords to the mainTab";
            this.chkKeywordsInAll.UseVisualStyleBackColor = true;
            this.chkKeywordsInAll.CheckedChanged += new System.EventHandler(this.chkKeywordsInAll_CheckedChanged);
            // 
            // chkShowAfterChar
            // 
            this.chkShowAfterChar.AutoSize = true;
            this.chkShowAfterChar.Location = new System.Drawing.Point(17, 98);
            this.chkShowAfterChar.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.chkShowAfterChar.Name = "chkShowAfterChar";
            this.chkShowAfterChar.Size = new System.Drawing.Size(321, 21);
            this.chkShowAfterChar.TabIndex = 2;
            this.chkShowAfterChar.Text = "Show Completion list after a character is typed";
            this.chkShowAfterChar.UseVisualStyleBackColor = true;
            this.chkShowAfterChar.CheckedChanged += new System.EventHandler(this.chkShowAfterChar_CheckedChanged);
            // 
            // chkDotAsUniversalSelector
            // 
            this.chkDotAsUniversalSelector.AutoSize = true;
            this.chkDotAsUniversalSelector.Location = new System.Drawing.Point(17, 73);
            this.chkDotAsUniversalSelector.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.chkDotAsUniversalSelector.Name = "chkDotAsUniversalSelector";
            this.chkDotAsUniversalSelector.Size = new System.Drawing.Size(323, 21);
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
            this.grpCase.Location = new System.Drawing.Point(16, 142);
            this.grpCase.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.grpCase.Name = "grpCase";
            this.grpCase.Padding = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.grpCase.Size = new System.Drawing.Size(497, 159);
            this.grpCase.TabIndex = 1;
            this.grpCase.TabStop = false;
            this.grpCase.Text = "Document Formatting";
            // 
            // chkAlignMethod
            // 
            this.chkAlignMethod.AutoSize = true;
            this.chkAlignMethod.Location = new System.Drawing.Point(17, 123);
            this.chkAlignMethod.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.chkAlignMethod.Name = "chkAlignMethod";
            this.chkAlignMethod.Size = new System.Drawing.Size(409, 21);
            this.chkAlignMethod.TabIndex = 4;
            this.chkAlignMethod.Text = "Align inner content in METHOD, FUNCTION && PROCEDURE";
            this.chkAlignMethod.UseVisualStyleBackColor = true;
            this.chkAlignMethod.CheckedChanged += new System.EventHandler(this.chkAlignMethod_CheckedChanged);
            // 
            // chkAlignDoCase
            // 
            this.chkAlignDoCase.AutoSize = true;
            this.chkAlignDoCase.Location = new System.Drawing.Point(17, 96);
            this.chkAlignDoCase.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.chkAlignDoCase.Name = "chkAlignDoCase";
            this.chkAlignDoCase.Size = new System.Drawing.Size(313, 21);
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
            this.grpKeywordCase.Location = new System.Drawing.Point(9, 20);
            this.grpKeywordCase.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.grpKeywordCase.Name = "grpKeywordCase";
            this.grpKeywordCase.Padding = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.grpKeywordCase.Size = new System.Drawing.Size(316, 44);
            this.grpKeywordCase.TabIndex = 1;
            this.grpKeywordCase.TabStop = false;
            this.grpKeywordCase.Text = "&Keyword Case Synchronization";
            // 
            // rbTitle
            // 
            this.rbTitle.AutoSize = true;
            this.rbTitle.Location = new System.Drawing.Point(244, 18);
            this.rbTitle.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.rbTitle.Name = "rbTitle";
            this.rbTitle.Size = new System.Drawing.Size(56, 21);
            this.rbTitle.TabIndex = 3;
            this.rbTitle.TabStop = true;
            this.rbTitle.Text = "&Title";
            this.rbTitle.UseVisualStyleBackColor = true;
            this.rbTitle.CheckedChanged += new System.EventHandler(this.kwCaseChanged);
            // 
            // rbNone
            // 
            this.rbNone.AutoSize = true;
            this.rbNone.Location = new System.Drawing.Point(8, 18);
            this.rbNone.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.rbNone.Name = "rbNone";
            this.rbNone.Size = new System.Drawing.Size(63, 21);
            this.rbNone.TabIndex = 0;
            this.rbNone.TabStop = true;
            this.rbNone.Text = "&None";
            this.rbNone.UseVisualStyleBackColor = true;
            this.rbNone.CheckedChanged += new System.EventHandler(this.kwCaseChanged);
            // 
            // rbUpper
            // 
            this.rbUpper.AutoSize = true;
            this.rbUpper.Location = new System.Drawing.Point(79, 18);
            this.rbUpper.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.rbUpper.Name = "rbUpper";
            this.rbUpper.Size = new System.Drawing.Size(76, 21);
            this.rbUpper.TabIndex = 1;
            this.rbUpper.TabStop = true;
            this.rbUpper.Text = "&UPPER";
            this.rbUpper.UseVisualStyleBackColor = true;
            this.rbUpper.CheckedChanged += new System.EventHandler(this.kwCaseChanged);
            // 
            // rbLower
            // 
            this.rbLower.AutoSize = true;
            this.rbLower.Location = new System.Drawing.Point(169, 18);
            this.rbLower.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.rbLower.Name = "rbLower";
            this.rbLower.Size = new System.Drawing.Size(62, 21);
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
            this.chkIdentifierCase.Location = new System.Drawing.Point(17, 70);
            this.chkIdentifierCase.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.chkIdentifierCase.Name = "chkIdentifierCase";
            this.chkIdentifierCase.Size = new System.Drawing.Size(225, 21);
            this.chkIdentifierCase.TabIndex = 2;
            this.chkIdentifierCase.Text = "&Identifier Case Synchronization";
            this.chkIdentifierCase.UseVisualStyleBackColor = true;
            this.chkIdentifierCase.CheckedChanged += new System.EventHandler(this.chkIdentifierCase_CheckedChanged);
            // 
            // grpNavigationBars
            // 
            this.grpNavigationBars.Controls.Add(this.chkShowMembersOfCurrentType);
            this.grpNavigationBars.Controls.Add(this.chkSortNavBar);
            this.grpNavigationBars.Controls.Add(this.chkIncludeFields);
            this.grpNavigationBars.Location = new System.Drawing.Point(16, 302);
            this.grpNavigationBars.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.grpNavigationBars.Name = "grpNavigationBars";
            this.grpNavigationBars.Padding = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.grpNavigationBars.Size = new System.Drawing.Size(497, 100);
            this.grpNavigationBars.TabIndex = 2;
            this.grpNavigationBars.TabStop = false;
            this.grpNavigationBars.Text = "Navigation Bars";
            // 
            // chkShowMembersOfCurrentType
            // 
            this.chkShowMembersOfCurrentType.AutoSize = true;
            this.chkShowMembersOfCurrentType.Location = new System.Drawing.Point(17, 71);
            this.chkShowMembersOfCurrentType.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.chkShowMembersOfCurrentType.Name = "chkShowMembersOfCurrentType";
            this.chkShowMembersOfCurrentType.Size = new System.Drawing.Size(444, 21);
            this.chkShowMembersOfCurrentType.TabIndex = 2;
            this.chkShowMembersOfCurrentType.Text = "Members combobox shows members of current selected type only";
            this.chkShowMembersOfCurrentType.UseVisualStyleBackColor = true;
            this.chkShowMembersOfCurrentType.CheckedChanged += new System.EventHandler(this.chkShowMembersOfCurrentType_CheckedChanged);
            // 
            // chkSortNavBar
            // 
            this.chkSortNavBar.AutoSize = true;
            this.chkSortNavBar.Location = new System.Drawing.Point(17, 47);
            this.chkSortNavBar.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.chkSortNavBar.Name = "chkSortNavBar";
            this.chkSortNavBar.Size = new System.Drawing.Size(151, 21);
            this.chkSortNavBar.TabIndex = 1;
            this.chkSortNavBar.Text = "Sort Items by name";
            this.chkSortNavBar.UseVisualStyleBackColor = true;
            this.chkSortNavBar.CheckedChanged += new System.EventHandler(this.chkSortNavBar_CheckedChanged);
            // 
            // chkIncludeFields
            // 
            this.chkIncludeFields.AutoSize = true;
            this.chkIncludeFields.Location = new System.Drawing.Point(17, 21);
            this.chkIncludeFields.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.chkIncludeFields.Name = "chkIncludeFields";
            this.chkIncludeFields.Size = new System.Drawing.Size(303, 21);
            this.chkIncludeFields.TabIndex = 0;
            this.chkIncludeFields.Text = "Include fields (instance variables) && defines";
            this.chkIncludeFields.UseVisualStyleBackColor = true;
            this.chkIncludeFields.CheckedChanged += new System.EventHandler(this.chkIncludeFields_CheckedChanged);
            // 
            // btnShowMeTheMagic
            // 
            this.btnShowMeTheMagic.Location = new System.Drawing.Point(361, 423);
            this.btnShowMeTheMagic.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.btnShowMeTheMagic.Name = "btnShowMeTheMagic";
            this.btnShowMeTheMagic.Size = new System.Drawing.Size(152, 28);
            this.btnShowMeTheMagic.TabIndex = 3;
            this.btnShowMeTheMagic.Text = "Open Sesame";
            this.btnShowMeTheMagic.UseVisualStyleBackColor = true;
            this.btnShowMeTheMagic.Click += new System.EventHandler(this.btnShowMeTheMagic_Click);
            // 
            // IntellisenseOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.btnShowMeTheMagic);
            this.Controls.Add(this.grpNavigationBars);
            this.Controls.Add(this.grpCase);
            this.Controls.Add(this.grpCompletionListTabs);
            this.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.Name = "IntellisenseOptionsControl";
            this.Size = new System.Drawing.Size(529, 469);
            this.grpCompletionListTabs.ResumeLayout(false);
            this.grpCompletionListTabs.PerformLayout();
            this.grpCase.ResumeLayout(false);
            this.grpCase.PerformLayout();
            this.grpKeywordCase.ResumeLayout(false);
            this.grpKeywordCase.PerformLayout();
            this.grpNavigationBars.ResumeLayout(false);
            this.grpNavigationBars.PerformLayout();
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
        private System.Windows.Forms.CheckBox chkKeywordsInAll;
        private System.Windows.Forms.Button btnShowMeTheMagic;
        private System.Windows.Forms.CheckBox chkShowMembersOfCurrentType;
    }
}
