namespace XSharp.LanguageService.OptionsPages
{
    partial class CompletionOptionsControl
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
            this.label1 = new System.Windows.Forms.Label();
            this.chkLocals = new System.Windows.Forms.CheckBox();
            this.chkFields = new System.Windows.Forms.CheckBox();
            this.chkInherited = new System.Windows.Forms.CheckBox();
            this.chkGlobalsProject = new System.Windows.Forms.CheckBox();
            this.chkFunctions = new System.Windows.Forms.CheckBox();
            this.chkFunctionsSource = new System.Windows.Forms.CheckBox();
            this.chkFunctionsExternal = new System.Windows.Forms.CheckBox();
            this.chkGlobalsSource = new System.Windows.Forms.CheckBox();
            this.chkGlobalsExtern = new System.Windows.Forms.CheckBox();
            this.chkKeywords = new System.Windows.Forms.CheckBox();
            this.chkSnippets = new System.Windows.Forms.CheckBox();
            this.rtfDescription = new System.Windows.Forms.RichTextBox();
            this.btnAll = new System.Windows.Forms.Button();
            this.btnNothing = new System.Windows.Forms.Button();
            this.lblChars = new System.Windows.Forms.Label();
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.tbChars = new System.Windows.Forms.NumericUpDown();
            this.chkNamespaces = new System.Windows.Forms.CheckBox();
            this.chkTypes = new System.Windows.Forms.CheckBox();
            ((System.ComponentModel.ISupportInitialize)(this.tbChars)).BeginInit();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(20, 12);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(0, 13);
            this.label1.TabIndex = 1;
            // 
            // chkLocals
            // 
            this.chkLocals.AutoSize = true;
            this.chkLocals.Location = new System.Drawing.Point(19, 166);
            this.chkLocals.Name = "chkLocals";
            this.chkLocals.Size = new System.Drawing.Size(122, 17);
            this.chkLocals.TabIndex = 4;
            this.chkLocals.Text = "&Locals && Parameters";
            this.toolTip1.SetToolTip(this.chkLocals, "Include Locals and Parameters in the completion list");
            this.chkLocals.UseVisualStyleBackColor = true;
            // 
            // chkFields
            // 
            this.chkFields.AutoSize = true;
            this.chkFields.Location = new System.Drawing.Point(19, 187);
            this.chkFields.Name = "chkFields";
            this.chkFields.Size = new System.Drawing.Size(176, 17);
            this.chkFields.TabIndex = 5;
            this.chkFields.Text = "&Fields and methods current type";
            this.toolTip1.SetToolTip(this.chkFields, "Include fields and methods of the current type in the completionlist");
            this.chkFields.UseVisualStyleBackColor = true;
            // 
            // chkInherited
            // 
            this.chkInherited.AutoSize = true;
            this.chkInherited.Location = new System.Drawing.Point(19, 208);
            this.chkInherited.Name = "chkInherited";
            this.chkInherited.Size = new System.Drawing.Size(161, 17);
            this.chkInherited.TabIndex = 6;
            this.chkInherited.Text = "&Inherited Fields and methods";
            this.toolTip1.SetToolTip(this.chkInherited, "Include fields and methods of parent type(s) in the completionlist");
            this.chkInherited.UseVisualStyleBackColor = true;
            // 
            // chkGlobalsProject
            // 
            this.chkGlobalsProject.AutoSize = true;
            this.chkGlobalsProject.Location = new System.Drawing.Point(195, 142);
            this.chkGlobalsProject.Name = "chkGlobalsProject";
            this.chkGlobalsProject.Size = new System.Drawing.Size(191, 17);
            this.chkGlobalsProject.TabIndex = 11;
            this.chkGlobalsProject.Text = "&Globals && Defines in current project";
            this.toolTip1.SetToolTip(this.chkGlobalsProject, "Include Globals and Defines from the current project in the completion list");
            this.chkGlobalsProject.UseVisualStyleBackColor = true;
            // 
            // chkFunctions
            // 
            this.chkFunctions.AutoSize = true;
            this.chkFunctions.Location = new System.Drawing.Point(195, 208);
            this.chkFunctions.Name = "chkFunctions";
            this.chkFunctions.Size = new System.Drawing.Size(154, 17);
            this.chkFunctions.TabIndex = 14;
            this.chkFunctions.Text = "&Functions in current project";
            this.toolTip1.SetToolTip(this.chkFunctions, "Include Functions from the current project in the completion list");
            this.chkFunctions.UseVisualStyleBackColor = true;
            // 
            // chkFunctionsSource
            // 
            this.chkFunctionsSource.AutoSize = true;
            this.chkFunctionsSource.Location = new System.Drawing.Point(195, 229);
            this.chkFunctionsSource.Name = "chkFunctionsSource";
            this.chkFunctionsSource.Size = new System.Drawing.Size(118, 17);
            this.chkFunctionsSource.TabIndex = 15;
            this.chkFunctionsSource.Text = "Functions in &source";
            this.toolTip1.SetToolTip(this.chkFunctionsSource, "Include Functions  from referenced X# projects in the completion list");
            this.chkFunctionsSource.UseVisualStyleBackColor = true;
            // 
            // chkFunctionsExternal
            // 
            this.chkFunctionsExternal.AutoSize = true;
            this.chkFunctionsExternal.Location = new System.Drawing.Point(195, 250);
            this.chkFunctionsExternal.Name = "chkFunctionsExternal";
            this.chkFunctionsExternal.Size = new System.Drawing.Size(137, 17);
            this.chkFunctionsExternal.TabIndex = 16;
            this.chkFunctionsExternal.Text = "Functions in assemblies";
            this.toolTip1.SetToolTip(this.chkFunctionsExternal, "Include Functions from referenced X# assemblies in the completion list. \r\nThis in" +
        "cludes Functions in the X# Runtime.");
            this.chkFunctionsExternal.UseVisualStyleBackColor = true;
            // 
            // chkGlobalsSource
            // 
            this.chkGlobalsSource.AutoSize = true;
            this.chkGlobalsSource.Location = new System.Drawing.Point(195, 166);
            this.chkGlobalsSource.Name = "chkGlobalsSource";
            this.chkGlobalsSource.Size = new System.Drawing.Size(155, 17);
            this.chkGlobalsSource.TabIndex = 12;
            this.chkGlobalsSource.Text = "G&lobals && Defines in source";
            this.toolTip1.SetToolTip(this.chkGlobalsSource, "Include Globals and Defines from referenced X# projects in the completion list");
            this.chkGlobalsSource.UseVisualStyleBackColor = true;
            // 
            // chkGlobalsExtern
            // 
            this.chkGlobalsExtern.AutoSize = true;
            this.chkGlobalsExtern.Location = new System.Drawing.Point(195, 187);
            this.chkGlobalsExtern.Name = "chkGlobalsExtern";
            this.chkGlobalsExtern.Size = new System.Drawing.Size(174, 17);
            this.chkGlobalsExtern.TabIndex = 13;
            this.chkGlobalsExtern.Text = "Gl&obals && Defines in assemblies";
            this.toolTip1.SetToolTip(this.chkGlobalsExtern, "Include Globals and Defines from referenced X# assemblies in the completion list." +
        "\r\nThis includes Globals and Defines in the X# Runtime.");
            this.chkGlobalsExtern.UseVisualStyleBackColor = true;
            // 
            // chkKeywords
            // 
            this.chkKeywords.AutoSize = true;
            this.chkKeywords.Location = new System.Drawing.Point(19, 250);
            this.chkKeywords.Name = "chkKeywords";
            this.chkKeywords.Size = new System.Drawing.Size(72, 17);
            this.chkKeywords.TabIndex = 9;
            this.chkKeywords.Text = "&Keywords";
            this.toolTip1.SetToolTip(this.chkKeywords, "Include keywords in the completionlist");
            this.chkKeywords.UseVisualStyleBackColor = true;
            // 
            // chkSnippets
            // 
            this.chkSnippets.AutoSize = true;
            this.chkSnippets.Location = new System.Drawing.Point(108, 249);
            this.chkSnippets.Name = "chkSnippets";
            this.chkSnippets.Size = new System.Drawing.Size(67, 17);
            this.chkSnippets.TabIndex = 10;
            this.chkSnippets.Text = "&Snippets";
            this.toolTip1.SetToolTip(this.chkSnippets, "Include snipprts in the completionlist");
            this.chkSnippets.UseVisualStyleBackColor = true;
            // 
            // rtfDescription
            // 
            this.rtfDescription.BackColor = System.Drawing.SystemColors.Control;
            this.rtfDescription.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.rtfDescription.BulletIndent = 5;
            this.rtfDescription.Location = new System.Drawing.Point(10, -3);
            this.rtfDescription.Name = "rtfDescription";
            this.rtfDescription.ReadOnly = true;
            this.rtfDescription.Size = new System.Drawing.Size(382, 139);
            this.rtfDescription.TabIndex = 0;
            this.rtfDescription.TabStop = false;
            this.rtfDescription.Text = "";
            // 
            // btnAll
            // 
            this.btnAll.Location = new System.Drawing.Point(350, 214);
            this.btnAll.Name = "btnAll";
            this.btnAll.Size = new System.Drawing.Size(44, 23);
            this.btnAll.TabIndex = 17;
            this.btnAll.Text = "&All";
            this.btnAll.UseVisualStyleBackColor = true;
            this.btnAll.Click += new System.EventHandler(this.btnAll_Click);
            // 
            // btnNothing
            // 
            this.btnNothing.Location = new System.Drawing.Point(350, 242);
            this.btnNothing.Name = "btnNothing";
            this.btnNothing.Size = new System.Drawing.Size(44, 23);
            this.btnNothing.TabIndex = 18;
            this.btnNothing.Text = "&None";
            this.btnNothing.UseVisualStyleBackColor = true;
            this.btnNothing.Click += new System.EventHandler(this.btnNothing_Click);
            // 
            // lblChars
            // 
            this.lblChars.AutoSize = true;
            this.lblChars.Location = new System.Drawing.Point(17, 149);
            this.lblChars.Name = "lblChars";
            this.lblChars.Size = new System.Drawing.Size(78, 13);
            this.lblChars.TabIndex = 2;
            this.lblChars.Text = "Chars required:";
            this.toolTip1.SetToolTip(this.lblChars, "Number of characters required to start Generic CompletionList");
            // 
            // tbChars
            // 
            this.tbChars.Location = new System.Drawing.Point(101, 147);
            this.tbChars.Minimum = new decimal(new int[] {
            3,
            0,
            0,
            0});
            this.tbChars.Name = "tbChars";
            this.tbChars.Size = new System.Drawing.Size(79, 20);
            this.tbChars.TabIndex = 3;
            this.toolTip1.SetToolTip(this.tbChars, "Number of characters required to start Generic CompletionList. This must be a num" +
        "ber > 1");
            this.tbChars.Value = new decimal(new int[] {
            3,
            0,
            0,
            0});
            // 
            // chkNamespaces
            // 
            this.chkNamespaces.AutoSize = true;
            this.chkNamespaces.Location = new System.Drawing.Point(19, 229);
            this.chkNamespaces.Name = "chkNamespaces";
            this.chkNamespaces.Size = new System.Drawing.Size(88, 17);
            this.chkNamespaces.TabIndex = 7;
            this.chkNamespaces.Text = "&Namespaces";
            this.toolTip1.SetToolTip(this.chkNamespaces, "Include namespaces from the current project and all referenced projects and assem" +
        "blies");
            this.chkNamespaces.UseVisualStyleBackColor = true;
            // 
            // chkTypes
            // 
            this.chkTypes.AutoSize = true;
            this.chkTypes.Location = new System.Drawing.Point(108, 229);
            this.chkTypes.Name = "chkTypes";
            this.chkTypes.Size = new System.Drawing.Size(55, 17);
            this.chkTypes.TabIndex = 8;
            this.chkTypes.Text = "&Types";
            this.toolTip1.SetToolTip(this.chkTypes, "Include types from the current assembly and all referenced projects and assemblie" +
        "s");
            this.chkTypes.UseVisualStyleBackColor = true;
            // 
            // CompletionOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoScroll = true;
            this.Controls.Add(this.chkTypes);
            this.Controls.Add(this.chkNamespaces);
            this.Controls.Add(this.tbChars);
            this.Controls.Add(this.lblChars);
            this.Controls.Add(this.btnNothing);
            this.Controls.Add(this.btnAll);
            this.Controls.Add(this.rtfDescription);
            this.Controls.Add(this.chkSnippets);
            this.Controls.Add(this.chkKeywords);
            this.Controls.Add(this.chkGlobalsExtern);
            this.Controls.Add(this.chkGlobalsSource);
            this.Controls.Add(this.chkFunctionsExternal);
            this.Controls.Add(this.chkFunctionsSource);
            this.Controls.Add(this.chkFunctions);
            this.Controls.Add(this.chkGlobalsProject);
            this.Controls.Add(this.chkInherited);
            this.Controls.Add(this.chkFields);
            this.Controls.Add(this.chkLocals);
            this.Controls.Add(this.label1);
            this.Name = "CompletionOptionsControl";
            this.Size = new System.Drawing.Size(403, 305);
            ((System.ComponentModel.ISupportInitialize)(this.tbChars)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.CheckBox chkLocals;
        private System.Windows.Forms.CheckBox chkFields;
        private System.Windows.Forms.CheckBox chkInherited;
        private System.Windows.Forms.CheckBox chkGlobalsProject;
        private System.Windows.Forms.CheckBox chkFunctions;
        private System.Windows.Forms.CheckBox chkFunctionsSource;
        private System.Windows.Forms.CheckBox chkFunctionsExternal;
        private System.Windows.Forms.CheckBox chkGlobalsSource;
        private System.Windows.Forms.CheckBox chkGlobalsExtern;
        private System.Windows.Forms.CheckBox chkKeywords;
        private System.Windows.Forms.CheckBox chkSnippets;
        private System.Windows.Forms.RichTextBox rtfDescription;
        private System.Windows.Forms.Button btnAll;
        private System.Windows.Forms.Button btnNothing;
        private System.Windows.Forms.Label lblChars;
        private System.Windows.Forms.ToolTip toolTip1;
        private System.Windows.Forms.NumericUpDown tbChars;
        private System.Windows.Forms.CheckBox chkNamespaces;
        private System.Windows.Forms.CheckBox chkTypes;
    }
}
