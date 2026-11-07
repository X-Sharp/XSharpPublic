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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(IntellisenseOptionsControl));
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.chkShowMembersOfCurrentType = new System.Windows.Forms.CheckBox();
            this.chkSortNavBar = new System.Windows.Forms.CheckBox();
            this.chkIncludeFields = new System.Windows.Forms.CheckBox();
            this.chkKeywordsInAll = new System.Windows.Forms.CheckBox();
            this.chkCompletionListtabs = new System.Windows.Forms.CheckBox();
            this.chkExcludeMembersFromOtherfiles = new System.Windows.Forms.CheckBox();
            this.chkUseMicrosoftSQLite = new System.Windows.Forms.CheckBox();
            this.chkFindReferencesLateBound = new System.Windows.Forms.CheckBox();
            this.grpFindReferences = new System.Windows.Forms.GroupBox();
            this.chkShowParameterHints = new System.Windows.Forms.CheckBox();
            this.chkShowTypeHints = new System.Windows.Forms.CheckBox();
            this.grpInlayHints = new System.Windows.Forms.GroupBox();
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
            this.grpFindReferences.SuspendLayout();
            this.grpInlayHints.SuspendLayout();
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
            this.toolTip1.SetToolTip(this.chkCompletionListtabs, "This organizes your completionlists in tabs, to make it easier to find a method o" +
        "r property in long completionlists");
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
            // chkUseMicrosoftSQLite
            // 
            this.chkUseMicrosoftSQLite.AutoSize = true;
            this.chkUseMicrosoftSQLite.Location = new System.Drawing.Point(13, 99);
            this.chkUseMicrosoftSQLite.Name = "chkUseMicrosoftSQLite";
            this.chkUseMicrosoftSQLite.Size = new System.Drawing.Size(339, 17);
            this.chkUseMicrosoftSQLite.TabIndex = 4;
            this.chkUseMicrosoftSQLite.Text = "Use the Microsoft.Data.SQLite engine for the intellisense database";
            this.toolTip1.SetToolTip(this.chkUseMicrosoftSQLite, resources.GetString("chkUseMicrosoftSQLite.ToolTip"));
            this.chkUseMicrosoftSQLite.UseVisualStyleBackColor = true;
            //
            // chkFindReferencesLateBound
            //
            this.chkFindReferencesLateBound.AutoSize = true;
            this.chkFindReferencesLateBound.Location = new System.Drawing.Point(13, 18);
            this.chkFindReferencesLateBound.Name = "chkFindReferencesLateBound";
            this.chkFindReferencesLateBound.Size = new System.Drawing.Size(309, 17);
            this.chkFindReferencesLateBound.TabIndex = 0;
            this.chkFindReferencesLateBound.Text = "Also list late-bound sends (o:Name) and symbols (#Name)";
            this.toolTip1.SetToolTip(this.chkFindReferencesLateBound, "Find All References on a method, property or field also lists the sends o:Name " +
        "that cannot be resolved (USUAL, OBJECT, unknown type) and the symbols #Name, in a group of their own.");
            this.chkFindReferencesLateBound.UseVisualStyleBackColor = true;
            //
            // grpFindReferences
            //
            this.grpFindReferences.Controls.Add(this.chkFindReferencesLateBound);
            this.grpFindReferences.Location = new System.Drawing.Point(5, 249);
            this.grpFindReferences.Name = "grpFindReferences";
            this.grpFindReferences.Size = new System.Drawing.Size(373, 45);
            this.grpFindReferences.TabIndex = 2;
            this.grpFindReferences.TabStop = false;
            this.grpFindReferences.Text = "Find All References";
            //
            // chkShowParameterHints
            //
            this.chkShowParameterHints.AutoSize = true;
            this.chkShowParameterHints.Location = new System.Drawing.Point(13, 18);
            this.chkShowParameterHints.Name = "chkShowParameterHints";
            this.chkShowParameterHints.Size = new System.Drawing.Size(178, 17);
            this.chkShowParameterHints.TabIndex = 0;
            this.chkShowParameterHints.Text = "Show parameter name hints";
            this.toolTip1.SetToolTip(this.chkShowParameterHints, "Shows the parameter name in front of arguments that do not tell what they mean: " +
        "literals such as TRUE, NIL or 3, and Type{} constructor calls.");
            this.chkShowParameterHints.UseVisualStyleBackColor = true;
            //
            // chkShowTypeHints
            //
            this.chkShowTypeHints.AutoSize = true;
            this.chkShowTypeHints.Location = new System.Drawing.Point(13, 39);
            this.chkShowTypeHints.Name = "chkShowTypeHints";
            this.chkShowTypeHints.Size = new System.Drawing.Size(158, 17);
            this.chkShowTypeHints.TabIndex = 1;
            this.chkShowTypeHints.Text = "Show inferred type hints";
            this.toolTip1.SetToolTip(this.chkShowTypeHints, "Shows the inferred type behind VAR and LOCAL IMPLIED variables. Double-click the " +
        "hint to write the type into the code.");
            this.chkShowTypeHints.UseVisualStyleBackColor = true;
            //
            // grpInlayHints
            //
            this.grpInlayHints.Controls.Add(this.chkShowParameterHints);
            this.grpInlayHints.Controls.Add(this.chkShowTypeHints);
            this.grpInlayHints.Location = new System.Drawing.Point(5, 300);
            this.grpInlayHints.Name = "grpInlayHints";
            this.grpInlayHints.Size = new System.Drawing.Size(373, 64);
            this.grpInlayHints.TabIndex = 3;
            this.grpInlayHints.TabStop = false;
            this.grpInlayHints.Text = "Inlay Hints";
            //
            // panel1
            //
            this.panel1.Controls.Add(this.grpInlayHints);
            this.panel1.Controls.Add(this.grpFindReferences);
            this.panel1.Controls.Add(this.btnShowMeTheMagic);
            this.panel1.Controls.Add(this.grpNavigationBars);
            this.panel1.Controls.Add(this.grpCompletionListTabs);
            this.panel1.Location = new System.Drawing.Point(0, 0);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(381, 403);
            this.panel1.TabIndex = 0;
            //
            // btnShowMeTheMagic
            //
            this.btnShowMeTheMagic.Location = new System.Drawing.Point(262, 370);
            this.btnShowMeTheMagic.Name = "btnShowMeTheMagic";
            this.btnShowMeTheMagic.Size = new System.Drawing.Size(114, 23);
            this.btnShowMeTheMagic.TabIndex = 4;
            this.btnShowMeTheMagic.Text = "Open Sesame";
            this.btnShowMeTheMagic.UseVisualStyleBackColor = true;
            this.btnShowMeTheMagic.Click += new System.EventHandler(this.btnShowMeTheMagic_Click);
            // 
            // grpNavigationBars
            // 
            this.grpNavigationBars.Controls.Add(this.chkUseMicrosoftSQLite);
            this.grpNavigationBars.Controls.Add(this.chkExcludeMembersFromOtherfiles);
            this.grpNavigationBars.Controls.Add(this.chkShowMembersOfCurrentType);
            this.grpNavigationBars.Controls.Add(this.chkSortNavBar);
            this.grpNavigationBars.Controls.Add(this.chkIncludeFields);
            this.grpNavigationBars.Location = new System.Drawing.Point(5, 121);
            this.grpNavigationBars.Name = "grpNavigationBars";
            this.grpNavigationBars.Size = new System.Drawing.Size(373, 122);
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
            this.Size = new System.Drawing.Size(388, 414);
            this.panel1.ResumeLayout(false);
            this.grpNavigationBars.ResumeLayout(false);
            this.grpNavigationBars.PerformLayout();
            this.grpCompletionListTabs.ResumeLayout(false);
            this.grpCompletionListTabs.PerformLayout();
            this.grpFindReferences.ResumeLayout(false);
            this.grpFindReferences.PerformLayout();
            this.grpInlayHints.ResumeLayout(false);
            this.grpInlayHints.PerformLayout();
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
        private System.Windows.Forms.CheckBox chkUseMicrosoftSQLite;
        private System.Windows.Forms.GroupBox grpFindReferences;
        private System.Windows.Forms.CheckBox chkFindReferencesLateBound;
        private System.Windows.Forms.GroupBox grpInlayHints;
        private System.Windows.Forms.CheckBox chkShowParameterHints;
        private System.Windows.Forms.CheckBox chkShowTypeHints;
    }
}
