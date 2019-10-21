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
            this.components = new System.ComponentModel.Container();
            System.Windows.Forms.Label label1;
            System.Windows.Forms.Label label2;
            this.chkCompletionListtabs = new System.Windows.Forms.CheckBox();
            this.grpCompletionListTabs = new System.Windows.Forms.GroupBox();
            this.chkAutoPairs = new System.Windows.Forms.CheckBox();
            this.commitChars = new System.Windows.Forms.TextBox();
            this.chkKeywordsInAll = new System.Windows.Forms.CheckBox();
            this.chkShowAfterChar = new System.Windows.Forms.CheckBox();
            this.chkDotAsUniversalSelector = new System.Windows.Forms.CheckBox();
            this.grpCase = new System.Windows.Forms.GroupBox();
            this.multiFactor = new System.Windows.Forms.TextBox();
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
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            label1 = new System.Windows.Forms.Label();
            label2 = new System.Windows.Forms.Label();
            this.grpCompletionListTabs.SuspendLayout();
            this.grpCase.SuspendLayout();
            this.grpKeywordCase.SuspendLayout();
            this.grpNavigationBars.SuspendLayout();
            this.SuspendLayout();
            // 
            // label1
            // 
            label1.AutoSize = true;
            label1.Location = new System.Drawing.Point(10, 99);
            label1.Name = "label1";
            label1.Size = new System.Drawing.Size(270, 13);
            label1.TabIndex = 6;
            label1.Text = "Commit completion list by typing the following characters";
            // 
            // label2
            // 
            label2.AutoSize = true;
            label2.Location = new System.Drawing.Point(10, 120);
            label2.Name = "label2";
            label2.Size = new System.Drawing.Size(192, 13);
            label2.TabIndex = 9;
            label2.Text = "MultiLine Statement Indentation factor :";
            // 
            // chkCompletionListtabs
            // 
            this.chkCompletionListtabs.AutoSize = true;
            this.chkCompletionListtabs.Location = new System.Drawing.Point(13, 18);
            this.chkCompletionListtabs.Name = "chkCompletionListtabs";
            this.chkCompletionListtabs.Size = new System.Drawing.Size(334, 17);
            this.chkCompletionListtabs.TabIndex = 0;
            this.chkCompletionListtabs.Text = "Organize in tabs with different item types (properties, methods etc)";
            this.toolTip1.SetToolTip(this.chkCompletionListtabs, "This organizes your comletionlists in tabs, to make it easier to find a method or" +
        " property in long completionlists");
            this.chkCompletionListtabs.UseVisualStyleBackColor = true;
            this.chkCompletionListtabs.CheckedChanged += new System.EventHandler(this.chkCompletionListtabs_CheckedChanged);
            // 
            // grpCompletionListTabs
            // 
            this.grpCompletionListTabs.Controls.Add(this.chkAutoPairs);
            this.grpCompletionListTabs.Controls.Add(this.commitChars);
            this.grpCompletionListTabs.Controls.Add(label1);
            this.grpCompletionListTabs.Controls.Add(this.chkKeywordsInAll);
            this.grpCompletionListTabs.Controls.Add(this.chkShowAfterChar);
            this.grpCompletionListTabs.Controls.Add(this.chkDotAsUniversalSelector);
            this.grpCompletionListTabs.Controls.Add(this.chkCompletionListtabs);
            this.grpCompletionListTabs.Location = new System.Drawing.Point(12, 6);
            this.grpCompletionListTabs.Name = "grpCompletionListTabs";
            this.grpCompletionListTabs.Size = new System.Drawing.Size(373, 162);
            this.grpCompletionListTabs.TabIndex = 0;
            this.grpCompletionListTabs.TabStop = false;
            this.grpCompletionListTabs.Text = "Completion Lists";
            // 
            // chkAutoPairs
            // 
            this.chkAutoPairs.AutoSize = true;
            this.chkAutoPairs.Location = new System.Drawing.Point(13, 140);
            this.chkAutoPairs.Name = "chkAutoPairs";
            this.chkAutoPairs.Size = new System.Drawing.Size(316, 17);
            this.chkAutoPairs.TabIndex = 8;
            this.chkAutoPairs.Text = "Auto Insert right parenthesis/bracket/curly after selecting item";
            this.toolTip1.SetToolTip(this.chkAutoPairs, "After selecting a function, method or type from the completionlist this will auto" +
        "matically inser a closing token");
            this.chkAutoPairs.UseVisualStyleBackColor = true;
            this.chkAutoPairs.CheckedChanged += new System.EventHandler(this.chkAutoPairs_CheckedChanged);
            // 
            // commitChars
            // 
            this.commitChars.Location = new System.Drawing.Point(14, 115);
            this.commitChars.Name = "commitChars";
            this.commitChars.Size = new System.Drawing.Size(286, 20);
            this.commitChars.TabIndex = 7;
            this.commitChars.Text = "{}[]().,:;+-*/%&|^!~=<>?@#\'\"\\";
            this.commitChars.TextChanged += new System.EventHandler(this.commitChars_TextChanged);
            // 
            // chkKeywordsInAll
            // 
            this.chkKeywordsInAll.AutoSize = true;
            this.chkKeywordsInAll.Location = new System.Drawing.Point(13, 39);
            this.chkKeywordsInAll.Name = "chkKeywordsInAll";
            this.chkKeywordsInAll.Size = new System.Drawing.Size(168, 17);
            this.chkKeywordsInAll.TabIndex = 3;
            this.chkKeywordsInAll.Text = "Add Keywords to the mainTab";
            this.toolTip1.SetToolTip(this.chkKeywordsInAll, "If you organize the Completion lists in tabs, this controls if keywords are added" +
        " to the main tab");
            this.chkKeywordsInAll.UseVisualStyleBackColor = true;
            this.chkKeywordsInAll.CheckedChanged += new System.EventHandler(this.chkKeywordsInAll_CheckedChanged);
            // 
            // chkShowAfterChar
            // 
            this.chkShowAfterChar.AutoSize = true;
            this.chkShowAfterChar.Location = new System.Drawing.Point(13, 80);
            this.chkShowAfterChar.Name = "chkShowAfterChar";
            this.chkShowAfterChar.Size = new System.Drawing.Size(246, 17);
            this.chkShowAfterChar.TabIndex = 2;
            this.chkShowAfterChar.Text = "Show Completion list after a character is typed.";
            this.toolTip1.SetToolTip(this.chkShowAfterChar, "When you have typed 3 or more characters then a completionlist will be shown with" +
        " keywords, type names, variable names etc based on the current location in the s" +
        "ource code editor");
            this.chkShowAfterChar.UseVisualStyleBackColor = true;
            this.chkShowAfterChar.CheckedChanged += new System.EventHandler(this.chkShowAfterChar_CheckedChanged);
            // 
            // chkDotAsUniversalSelector
            // 
            this.chkDotAsUniversalSelector.AutoSize = true;
            this.chkDotAsUniversalSelector.Location = new System.Drawing.Point(13, 59);
            this.chkDotAsUniversalSelector.Name = "chkDotAsUniversalSelector";
            this.chkDotAsUniversalSelector.Size = new System.Drawing.Size(342, 17);
            this.chkDotAsUniversalSelector.TabIndex = 1;
            this.chkDotAsUniversalSelector.Text = "Typing Dot (.) shows list that includes instance members (Core only)";
            this.toolTip1.SetToolTip(this.chkDotAsUniversalSelector, "The FoxPro dialect always uses instance members after a DOT and all other dialect" +
        "s except core will  only show static members after a dot and will show instance " +
        "members after a colon (:).");
            this.chkDotAsUniversalSelector.UseVisualStyleBackColor = true;
            this.chkDotAsUniversalSelector.CheckedChanged += new System.EventHandler(this.chkDotAsUniversalSelector_CheckedChanged);
            // 
            // grpCase
            // 
            this.grpCase.Controls.Add(this.multiFactor);
            this.grpCase.Controls.Add(label2);
            this.grpCase.Controls.Add(this.chkAlignMethod);
            this.grpCase.Controls.Add(this.chkAlignDoCase);
            this.grpCase.Controls.Add(this.grpKeywordCase);
            this.grpCase.Controls.Add(this.chkIdentifierCase);
            this.grpCase.Location = new System.Drawing.Point(12, 172);
            this.grpCase.Name = "grpCase";
            this.grpCase.Size = new System.Drawing.Size(373, 141);
            this.grpCase.TabIndex = 1;
            this.grpCase.TabStop = false;
            this.grpCase.Text = "Document Formatting";
            // 
            // multiFactor
            // 
            this.multiFactor.Location = new System.Drawing.Point(220, 118);
            this.multiFactor.Margin = new System.Windows.Forms.Padding(2);
            this.multiFactor.Name = "multiFactor";
            this.multiFactor.Size = new System.Drawing.Size(27, 20);
            this.multiFactor.TabIndex = 10;
            this.multiFactor.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.multiFactor.TextChanged += new System.EventHandler(this.multiFactor_TextChanged);
            // 
            // chkAlignMethod
            // 
            this.chkAlignMethod.AutoSize = true;
            this.chkAlignMethod.Location = new System.Drawing.Point(13, 100);
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
            this.chkAlignDoCase.Location = new System.Drawing.Point(13, 78);
            this.chkAlignDoCase.Name = "chkAlignDoCase";
            this.chkAlignDoCase.Size = new System.Drawing.Size(241, 17);
            this.chkAlignDoCase.TabIndex = 3;
            this.chkAlignDoCase.Text = "Align inner content in DO CASE ... ENDCASE";
            this.toolTip1.SetToolTip(this.chkAlignDoCase, "When you select this then the CASE keywords will line up with the DO keyword from" +
        " a DO CASE statement or the SWITCH keyword from a SWITCH statement");
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
            this.grpNavigationBars.Controls.Add(this.chkShowMembersOfCurrentType);
            this.grpNavigationBars.Controls.Add(this.chkSortNavBar);
            this.grpNavigationBars.Controls.Add(this.chkIncludeFields);
            this.grpNavigationBars.Location = new System.Drawing.Point(12, 314);
            this.grpNavigationBars.Name = "grpNavigationBars";
            this.grpNavigationBars.Size = new System.Drawing.Size(373, 81);
            this.grpNavigationBars.TabIndex = 2;
            this.grpNavigationBars.TabStop = false;
            this.grpNavigationBars.Text = "Navigation Bars";
            // 
            // chkShowMembersOfCurrentType
            // 
            this.chkShowMembersOfCurrentType.AutoSize = true;
            this.chkShowMembersOfCurrentType.Location = new System.Drawing.Point(13, 58);
            this.chkShowMembersOfCurrentType.Name = "chkShowMembersOfCurrentType";
            this.chkShowMembersOfCurrentType.Size = new System.Drawing.Size(335, 17);
            this.chkShowMembersOfCurrentType.TabIndex = 2;
            this.chkShowMembersOfCurrentType.Text = "Members combobox shows members of current selected type only";
            this.toolTip1.SetToolTip(this.chkShowMembersOfCurrentType, "If you select this then the combox on the right hand side will only show members " +
        "of the current selected type. Also the member names will no longer be prefixed w" +
        "ith the typenames.");
            this.chkShowMembersOfCurrentType.UseVisualStyleBackColor = true;
            this.chkShowMembersOfCurrentType.CheckedChanged += new System.EventHandler(this.chkShowMembersOfCurrentType_CheckedChanged);
            // 
            // chkSortNavBar
            // 
            this.chkSortNavBar.AutoSize = true;
            this.chkSortNavBar.Location = new System.Drawing.Point(13, 38);
            this.chkSortNavBar.Name = "chkSortNavBar";
            this.chkSortNavBar.Size = new System.Drawing.Size(116, 17);
            this.chkSortNavBar.TabIndex = 1;
            this.chkSortNavBar.Text = "Sort Items by name";
            this.toolTip1.SetToolTip(this.chkSortNavBar, "If you enable this then the items in the comboboxes on the navigation bar will be" +
        " sorted by name. Otherwise they will appear in the order in which they appear in" +
        " the source code.");
            this.chkSortNavBar.UseVisualStyleBackColor = true;
            this.chkSortNavBar.CheckedChanged += new System.EventHandler(this.chkSortNavBar_CheckedChanged);
            // 
            // chkIncludeFields
            // 
            this.chkIncludeFields.AutoSize = true;
            this.chkIncludeFields.Location = new System.Drawing.Point(13, 17);
            this.chkIncludeFields.Name = "chkIncludeFields";
            this.chkIncludeFields.Size = new System.Drawing.Size(228, 17);
            this.chkIncludeFields.TabIndex = 0;
            this.chkIncludeFields.Text = "Include fields (instance variables) && defines";
            this.toolTip1.SetToolTip(this.chkIncludeFields, "When you enable this then the combo box on the right hand side of the navigation " +
        "bar will also include fields and defines");
            this.chkIncludeFields.UseVisualStyleBackColor = true;
            this.chkIncludeFields.CheckedChanged += new System.EventHandler(this.chkIncludeFields_CheckedChanged);
            // 
            // btnShowMeTheMagic
            // 
            this.btnShowMeTheMagic.Location = new System.Drawing.Point(271, 413);
            this.btnShowMeTheMagic.Name = "btnShowMeTheMagic";
            this.btnShowMeTheMagic.Size = new System.Drawing.Size(114, 23);
            this.btnShowMeTheMagic.TabIndex = 3;
            this.btnShowMeTheMagic.Text = "Open Sesame";
            this.btnShowMeTheMagic.UseVisualStyleBackColor = true;
            this.btnShowMeTheMagic.Click += new System.EventHandler(this.btnShowMeTheMagic_Click);
            // 
            // IntellisenseOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.btnShowMeTheMagic);
            this.Controls.Add(this.grpNavigationBars);
            this.Controls.Add(this.grpCase);
            this.Controls.Add(this.grpCompletionListTabs);
            this.Name = "IntellisenseOptionsControl";
            this.Size = new System.Drawing.Size(397, 444);
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
        internal System.Windows.Forms.TextBox commitChars;
        private System.Windows.Forms.CheckBox chkAutoPairs;
        private System.Windows.Forms.TextBox multiFactor;
        private System.Windows.Forms.ToolTip toolTip1;
    }
}
