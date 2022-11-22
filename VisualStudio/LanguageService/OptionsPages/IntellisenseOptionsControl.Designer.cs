namespace XSharp.LanguageService.OptionsPages
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
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.chkShowMembersOfCurrentType = new System.Windows.Forms.CheckBox();
            this.chkSortNavBar = new System.Windows.Forms.CheckBox();
            this.chkIncludeFields = new System.Windows.Forms.CheckBox();
            this.chkKeywordsInAll = new System.Windows.Forms.CheckBox();
            this.chkCompletionListtabs = new System.Windows.Forms.CheckBox();
            this.chkExcludeMembersFromOtherfiles = new System.Windows.Forms.CheckBox();
            this.panel1 = new System.Windows.Forms.Panel();
            this.btnShowMeTheMagic = new System.Windows.Forms.Button();
            this.grpNavigationBars = new System.Windows.Forms.GroupBox();
            this.grpCompletionListTabs = new System.Windows.Forms.GroupBox();
            this.btnReset = new System.Windows.Forms.Button();
            this.commitChars = new System.Windows.Forms.TextBox();
            label1 = new System.Windows.Forms.Label();
            this.panel1.SuspendLayout();
            this.grpNavigationBars.SuspendLayout();
            this.grpCompletionListTabs.SuspendLayout();
            this.SuspendLayout();
            // 
            // label1
            // 
            label1.AutoSize = true;
            label1.Location = new System.Drawing.Point(10, 63);
            label1.Name = "label1";
            label1.Size = new System.Drawing.Size(270, 13);
            label1.TabIndex = 2;
            label1.Text = "Commit completion list by typing the following characters";
            // 
            // chkShowMembersOfCurrentType
            // 
            this.chkShowMembersOfCurrentType.AutoSize = true;
            this.chkShowMembersOfCurrentType.Location = new System.Drawing.Point(13, 57);
            this.chkShowMembersOfCurrentType.Name = "chkShowMembersOfCurrentType";
            this.chkShowMembersOfCurrentType.Size = new System.Drawing.Size(335, 17);
            this.chkShowMembersOfCurrentType.TabIndex = 2;
            this.chkShowMembersOfCurrentType.Text = "Members combobox shows members of current selected type only";
            this.toolTip1.SetToolTip(this.chkShowMembersOfCurrentType, "If you select this then the combox on the right hand side will only show members " +
        "of the current selected type. Also the member names will no longer be prefixed w" +
        "ith the typenames.");
            this.chkShowMembersOfCurrentType.UseVisualStyleBackColor = true;
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
            // 
            // chkKeywordsInAll
            // 
            this.chkKeywordsInAll.AutoSize = true;
            this.chkKeywordsInAll.Location = new System.Drawing.Point(13, 39);
            this.chkKeywordsInAll.Name = "chkKeywordsInAll";
            this.chkKeywordsInAll.Size = new System.Drawing.Size(168, 17);
            this.chkKeywordsInAll.TabIndex = 1;
            this.chkKeywordsInAll.Text = "Add &Keywords to the mainTab";
            this.toolTip1.SetToolTip(this.chkKeywordsInAll, "If you organize the Completion lists in tabs, this controls if keywords are added" +
        " to the main tab");
            this.chkKeywordsInAll.UseVisualStyleBackColor = true;
            // 
            // chkCompletionListtabs
            // 
            this.chkCompletionListtabs.AutoSize = true;
            this.chkCompletionListtabs.Location = new System.Drawing.Point(13, 18);
            this.chkCompletionListtabs.Name = "chkCompletionListtabs";
            this.chkCompletionListtabs.Size = new System.Drawing.Size(334, 17);
            this.chkCompletionListtabs.TabIndex = 0;
            this.chkCompletionListtabs.Text = "Organize in &tabs with different item types (properties, methods etc)";
            this.toolTip1.SetToolTip(this.chkCompletionListtabs, "This organizes your completionlists in tabs, to make it easier to find a method or" +
        " property in long completionlists");
            this.chkCompletionListtabs.UseVisualStyleBackColor = true;
            // 
            // chkExcludeMembersFromOtherfiles
            // 
            this.chkExcludeMembersFromOtherfiles.AutoSize = true;
            this.chkExcludeMembersFromOtherfiles.Location = new System.Drawing.Point(13, 76);
            this.chkExcludeMembersFromOtherfiles.Name = "chkExcludeMembersFromOtherfiles";
            this.chkExcludeMembersFromOtherfiles.Size = new System.Drawing.Size(270, 17);
            this.chkExcludeMembersFromOtherfiles.TabIndex = 3;
            this.chkExcludeMembersFromOtherfiles.Text = "Exclude members from other files (for partial classes)";
            this.toolTip1.SetToolTip(this.chkExcludeMembersFromOtherfiles, "If you select this then the combox on the right hand side will only show members " +
        "of the current selected type and only from the current file.");
            this.chkExcludeMembersFromOtherfiles.UseVisualStyleBackColor = true;
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.btnShowMeTheMagic);
            this.panel1.Controls.Add(this.grpNavigationBars);
            this.panel1.Controls.Add(this.grpCompletionListTabs);
            this.panel1.Location = new System.Drawing.Point(0, 0);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(381, 282);
            this.panel1.TabIndex = 0;
            // 
            // btnShowMeTheMagic
            // 
            this.btnShowMeTheMagic.Location = new System.Drawing.Point(262, 249);
            this.btnShowMeTheMagic.Name = "btnShowMeTheMagic";
            this.btnShowMeTheMagic.Size = new System.Drawing.Size(114, 23);
            this.btnShowMeTheMagic.TabIndex = 2;
            this.btnShowMeTheMagic.Text = "Open Sesame";
            this.btnShowMeTheMagic.UseVisualStyleBackColor = true;
            this.btnShowMeTheMagic.Click += new System.EventHandler(this.btnShowMeTheMagic_Click);
            // 
            // grpNavigationBars
            // 
            this.grpNavigationBars.Controls.Add(this.chkExcludeMembersFromOtherfiles);
            this.grpNavigationBars.Controls.Add(this.chkShowMembersOfCurrentType);
            this.grpNavigationBars.Controls.Add(this.chkSortNavBar);
            this.grpNavigationBars.Controls.Add(this.chkIncludeFields);
            this.grpNavigationBars.Location = new System.Drawing.Point(5, 121);
            this.grpNavigationBars.Name = "grpNavigationBars";
            this.grpNavigationBars.Size = new System.Drawing.Size(373, 101);
            this.grpNavigationBars.TabIndex = 1;
            this.grpNavigationBars.TabStop = false;
            this.grpNavigationBars.Text = "Navigation Bars";
            // 
            // grpCompletionListTabs
            // 
            this.grpCompletionListTabs.Controls.Add(this.btnReset);
            this.grpCompletionListTabs.Controls.Add(this.commitChars);
            this.grpCompletionListTabs.Controls.Add(label1);
            this.grpCompletionListTabs.Controls.Add(this.chkKeywordsInAll);
            this.grpCompletionListTabs.Controls.Add(this.chkCompletionListtabs);
            this.grpCompletionListTabs.Location = new System.Drawing.Point(3, 3);
            this.grpCompletionListTabs.Name = "grpCompletionListTabs";
            this.grpCompletionListTabs.Size = new System.Drawing.Size(373, 112);
            this.grpCompletionListTabs.TabIndex = 0;
            this.grpCompletionListTabs.TabStop = false;
            this.grpCompletionListTabs.Text = "Completion Lists";
            // 
            // btnReset
            // 
            this.btnReset.Location = new System.Drawing.Point(310, 80);
            this.btnReset.Name = "btnReset";
            this.btnReset.Size = new System.Drawing.Size(57, 23);
            this.btnReset.TabIndex = 4;
            this.btnReset.Text = "&Reset";
            this.btnReset.UseVisualStyleBackColor = true;
            this.btnReset.Click += new System.EventHandler(this.btnReset_Click);
            // 
            // commitChars
            // 
            this.commitChars.Location = new System.Drawing.Point(14, 79);
            this.commitChars.Name = "commitChars";
            this.commitChars.Size = new System.Drawing.Size(286, 20);
            this.commitChars.TabIndex = 3;
            // 
            // IntellisenseOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoScroll = true;
            this.Controls.Add(this.panel1);
            this.Name = "IntellisenseOptionsControl";
            this.Size = new System.Drawing.Size(388, 293);
            this.panel1.ResumeLayout(false);
            this.grpNavigationBars.ResumeLayout(false);
            this.grpNavigationBars.PerformLayout();
            this.grpCompletionListTabs.ResumeLayout(false);
            this.grpCompletionListTabs.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.ToolTip toolTip1;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Button btnShowMeTheMagic;
        private System.Windows.Forms.GroupBox grpNavigationBars;
        private System.Windows.Forms.CheckBox chkShowMembersOfCurrentType;
        private System.Windows.Forms.CheckBox chkSortNavBar;
        private System.Windows.Forms.CheckBox chkIncludeFields;
        private System.Windows.Forms.GroupBox grpCompletionListTabs;
        internal System.Windows.Forms.TextBox commitChars;
        private System.Windows.Forms.CheckBox chkKeywordsInAll;
        private System.Windows.Forms.CheckBox chkCompletionListtabs;
        private System.Windows.Forms.CheckBox chkExcludeMembersFromOtherfiles;
        private System.Windows.Forms.Button btnReset;
    }
}
