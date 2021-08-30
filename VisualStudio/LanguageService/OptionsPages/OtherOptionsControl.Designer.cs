namespace XSharp.LanguageService.OptionsPages
{
    partial class OtherOptionsControl
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(OtherOptionsControl));
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.chkSingleLineDividers = new System.Windows.Forms.CheckBox();
            this.chkShowDividers = new System.Windows.Forms.CheckBox();
            this.chkShowXMLComments = new System.Windows.Forms.CheckBox();
            this.lblPublic = new System.Windows.Forms.Label();
            this.rbPublic = new System.Windows.Forms.RadioButton();
            this.rbExport = new System.Windows.Forms.RadioButton();
            this.rbNone = new System.Windows.Forms.RadioButton();
            this.label1 = new System.Windows.Forms.Label();
            this.rbPrivate = new System.Windows.Forms.RadioButton();
            this.rbHidden = new System.Windows.Forms.RadioButton();
            this.grpOther = new System.Windows.Forms.GroupBox();
            this.grpCodeGenerator = new System.Windows.Forms.GroupBox();
            this.grpPrivate = new System.Windows.Forms.GroupBox();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.grpOther.SuspendLayout();
            this.grpCodeGenerator.SuspendLayout();
            this.grpPrivate.SuspendLayout();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // chkSingleLineDividers
            // 
            this.chkSingleLineDividers.AutoSize = true;
            this.chkSingleLineDividers.Location = new System.Drawing.Point(20, 47);
            this.chkSingleLineDividers.Name = "chkSingleLineDividers";
            this.chkSingleLineDividers.Size = new System.Drawing.Size(260, 17);
            this.chkSingleLineDividers.TabIndex = 1;
            this.chkSingleLineDividers.Text = "Also show &Entity Dividers for \"Single Line\" entities";
            this.toolTip1.SetToolTip(this.chkSingleLineDividers, "This applies to GLOBALs, DEFINEs, DELEGATES, CLASS Variables, MEMBERs of structur" +
        "es etc");
            this.chkSingleLineDividers.UseVisualStyleBackColor = true;
            this.chkSingleLineDividers.CheckedChanged += new System.EventHandler(this.chkSingleLineDividers_CheckedChanged);
            // 
            // chkShowDividers
            // 
            this.chkShowDividers.AutoSize = true;
            this.chkShowDividers.Location = new System.Drawing.Point(20, 23);
            this.chkShowDividers.Name = "chkShowDividers";
            this.chkShowDividers.Size = new System.Drawing.Size(247, 17);
            this.chkShowDividers.TabIndex = 0;
            this.chkShowDividers.Text = "Show Entity &Dividers in the Source Code Editor";
            this.toolTip1.SetToolTip(this.chkShowDividers, "The entity dividers will have the color that is defined in the Color setting as  " +
        "\"Outlining Margin Vertical Rule");
            this.chkShowDividers.UseVisualStyleBackColor = true;
            this.chkShowDividers.CheckedChanged += new System.EventHandler(this.chkShowDividers_CheckedChanged);
            // 
            // chkShowXMLComments
            // 
            this.chkShowXMLComments.AutoSize = true;
            this.chkShowXMLComments.Location = new System.Drawing.Point(20, 18);
            this.chkShowXMLComments.Name = "chkShowXMLComments";
            this.chkShowXMLComments.Size = new System.Drawing.Size(341, 17);
            this.chkShowXMLComments.TabIndex = 2;
            this.chkShowXMLComments.Text = "Show XML comments in generated source code for Goto Definition";
            this.toolTip1.SetToolTip(this.chkShowXMLComments, "The X# language service generates a temporary source file when you do a Goto Defition on a type that is found in an external assembly. When you select this option then the generate code will NOT contain XML comments if the external assembly comes with a matching XML documentation file");
            this.chkShowXMLComments.UseVisualStyleBackColor = true;
            this.chkShowXMLComments.CheckedChanged += new System.EventHandler(this.chkShowXMLComments_CheckedChanged);
            // 
            // lblPublic
            // 
            this.lblPublic.AutoSize = true;
            this.lblPublic.Location = new System.Drawing.Point(15, 50);
            this.lblPublic.Name = "lblPublic";
            this.lblPublic.Size = new System.Drawing.Size(143, 13);
            this.lblPublic.TabIndex = 3;
            this.lblPublic.Text = "Modifier for Public properties:";
            this.toolTip1.SetToolTip(this.lblPublic, "Specify the modifier the code generators should use for public properties");
            // 
            // rbPublic
            // 
            this.rbPublic.AutoSize = true;
            this.rbPublic.Location = new System.Drawing.Point(16, 8);
            this.rbPublic.Name = "rbPublic";
            this.rbPublic.Size = new System.Drawing.Size(54, 17);
            this.rbPublic.TabIndex = 4;
            this.rbPublic.TabStop = true;
            this.rbPublic.Text = "&Public";
            this.toolTip1.SetToolTip(this.rbPublic, "Use PUBLIC as modifier");
            this.rbPublic.UseVisualStyleBackColor = true;
            this.rbPublic.CheckedChanged += new System.EventHandler(this.rbPublic_CheckedChanged);
            // 
            // rbExport
            // 
            this.rbExport.AutoSize = true;
            this.rbExport.Location = new System.Drawing.Point(76, 8);
            this.rbExport.Name = "rbExport";
            this.rbExport.Size = new System.Drawing.Size(55, 17);
            this.rbExport.TabIndex = 5;
            this.rbExport.Text = "E&xport";
            this.toolTip1.SetToolTip(this.rbExport, "Use EXPORT as modifier");
            this.rbExport.UseVisualStyleBackColor = true;
            this.rbExport.CheckedChanged += new System.EventHandler(this.rbExport_CheckedChanged);
            // 
            // rbNone
            // 
            this.rbNone.AutoSize = true;
            this.rbNone.Location = new System.Drawing.Point(137, 8);
            this.rbNone.Name = "rbNone";
            this.rbNone.Size = new System.Drawing.Size(51, 17);
            this.rbNone.TabIndex = 6;
            this.rbNone.Text = "&None";
            this.toolTip1.SetToolTip(this.rbNone, "Use <no> modifier for public properties and methods");
            this.rbNone.UseVisualStyleBackColor = true;
            this.rbNone.CheckedChanged += new System.EventHandler(this.rbNone_CheckedChanged);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(15, 77);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(147, 13);
            this.label1.TabIndex = 7;
            this.label1.Text = "Modifier for Private properties:";
            this.toolTip1.SetToolTip(this.label1, "Specify the modifier the code generator should use for private properties");
            // 
            // rbPrivate
            // 
            this.rbPrivate.AutoSize = true;
            this.rbPrivate.Location = new System.Drawing.Point(20, 10);
            this.rbPrivate.Name = "rbPrivate";
            this.rbPrivate.Size = new System.Drawing.Size(58, 17);
            this.rbPrivate.TabIndex = 8;
            this.rbPrivate.TabStop = true;
            this.rbPrivate.Text = "P&rivate";
            this.toolTip1.SetToolTip(this.rbPrivate, "Use PRIVATE as modifier");
            this.rbPrivate.UseVisualStyleBackColor = true;
            this.rbPrivate.CheckedChanged += new System.EventHandler(this.rbPrivate_CheckedChanged);
            // 
            // rbHidden
            // 
            this.rbHidden.AutoSize = true;
            this.rbHidden.Location = new System.Drawing.Point(80, 10);
            this.rbHidden.Name = "rbHidden";
            this.rbHidden.Size = new System.Drawing.Size(59, 17);
            this.rbHidden.TabIndex = 9;
            this.rbHidden.Text = "&Hidden";
            this.toolTip1.SetToolTip(this.rbHidden, "Use HIDDEN as modifier");
            this.rbHidden.UseVisualStyleBackColor = true;
            this.rbHidden.CheckedChanged += new System.EventHandler(this.rbHidden_CheckedChanged);
            // 
            // grpOther
            // 
            this.grpOther.Controls.Add(this.chkSingleLineDividers);
            this.grpOther.Controls.Add(this.chkShowDividers);
            this.grpOther.Location = new System.Drawing.Point(3, 3);
            this.grpOther.Name = "grpOther";
            this.grpOther.Size = new System.Drawing.Size(401, 81);
            this.grpOther.TabIndex = 1;
            this.grpOther.TabStop = false;
            this.grpOther.Text = "Other Options";
            // 
            // grpCodeGenerator
            // 
            this.grpCodeGenerator.Controls.Add(this.grpPrivate);
            this.grpCodeGenerator.Controls.Add(this.groupBox1);
            this.grpCodeGenerator.Controls.Add(this.label1);
            this.grpCodeGenerator.Controls.Add(this.lblPublic);
            this.grpCodeGenerator.Controls.Add(this.chkShowXMLComments);
            this.grpCodeGenerator.Location = new System.Drawing.Point(3, 85);
            this.grpCodeGenerator.Name = "grpCodeGenerator";
            this.grpCodeGenerator.Size = new System.Drawing.Size(401, 138);
            this.grpCodeGenerator.TabIndex = 3;
            this.grpCodeGenerator.TabStop = false;
            this.grpCodeGenerator.Text = "Code Generator";
            this.grpCodeGenerator.Enter += new System.EventHandler(this.groupBox1_Enter);
            // 
            // grpPrivate
            // 
            this.grpPrivate.Controls.Add(this.rbPrivate);
            this.grpPrivate.Controls.Add(this.rbHidden);
            this.grpPrivate.Location = new System.Drawing.Point(165, 69);
            this.grpPrivate.Name = "grpPrivate";
            this.grpPrivate.Size = new System.Drawing.Size(230, 33);
            this.grpPrivate.TabIndex = 11;
            this.grpPrivate.TabStop = false;
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.rbPublic);
            this.groupBox1.Controls.Add(this.rbExport);
            this.groupBox1.Controls.Add(this.rbNone);
            this.groupBox1.Location = new System.Drawing.Point(169, 40);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(226, 29);
            this.groupBox1.TabIndex = 10;
            this.groupBox1.TabStop = false;
            // 
            // OtherOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoScroll = true;
            this.Controls.Add(this.grpCodeGenerator);
            this.Controls.Add(this.grpOther);
            this.Name = "OtherOptionsControl";
            this.Size = new System.Drawing.Size(424, 278);
            this.grpOther.ResumeLayout(false);
            this.grpOther.PerformLayout();
            this.grpCodeGenerator.ResumeLayout(false);
            this.grpCodeGenerator.PerformLayout();
            this.grpPrivate.ResumeLayout(false);
            this.grpPrivate.PerformLayout();
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.ToolTip toolTip1;
        private System.Windows.Forms.GroupBox grpOther;
        private System.Windows.Forms.CheckBox chkSingleLineDividers;
        private System.Windows.Forms.CheckBox chkShowDividers;
        private System.Windows.Forms.GroupBox grpCodeGenerator;
        private System.Windows.Forms.CheckBox chkShowXMLComments;
        private System.Windows.Forms.RadioButton rbNone;
        private System.Windows.Forms.RadioButton rbExport;
        private System.Windows.Forms.RadioButton rbPublic;
        private System.Windows.Forms.Label lblPublic;
        private System.Windows.Forms.GroupBox grpPrivate;
        private System.Windows.Forms.RadioButton rbPrivate;
        private System.Windows.Forms.RadioButton rbHidden;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Label label1;
    }
}
