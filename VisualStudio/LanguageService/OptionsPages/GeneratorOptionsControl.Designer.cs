namespace XSharp.LanguageService.OptionsPages
{
    partial class GeneratorOptionsControl
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
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.lblPublic = new System.Windows.Forms.Label();
            this.rbPublic = new System.Windows.Forms.RadioButton();
            this.rbExport = new System.Windows.Forms.RadioButton();
            this.rbNone = new System.Windows.Forms.RadioButton();
            this.label1 = new System.Windows.Forms.Label();
            this.rbPrivate = new System.Windows.Forms.RadioButton();
            this.rbHidden = new System.Windows.Forms.RadioButton();
            this.chkShowXMLComments = new System.Windows.Forms.CheckBox();
            this.grpCodeGenerator = new System.Windows.Forms.GroupBox();
            this.grpPrivate = new System.Windows.Forms.GroupBox();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.panel1 = new System.Windows.Forms.Panel();
            this.grpCodeGenerator.SuspendLayout();
            this.grpPrivate.SuspendLayout();
            this.groupBox1.SuspendLayout();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // lblPublic
            // 
            this.lblPublic.AutoSize = true;
            this.lblPublic.Location = new System.Drawing.Point(15, 50);
            this.lblPublic.Name = "lblPublic";
            this.lblPublic.Size = new System.Drawing.Size(143, 13);
            this.lblPublic.TabIndex = 1;
            this.lblPublic.Text = "Modifier for Public properties:";
            this.toolTip1.SetToolTip(this.lblPublic, "Specify the modifier the code generators should use for public properties");
            // 
            // rbPublic
            // 
            this.rbPublic.AutoSize = true;
            this.rbPublic.Location = new System.Drawing.Point(17, 8);
            this.rbPublic.Name = "rbPublic";
            this.rbPublic.Size = new System.Drawing.Size(54, 17);
            this.rbPublic.TabIndex = 0;
            this.rbPublic.TabStop = true;
            this.rbPublic.Text = "&Public";
            this.toolTip1.SetToolTip(this.rbPublic, "Use PUBLIC as modifier");
            this.rbPublic.UseVisualStyleBackColor = true;
            // 
            // rbExport
            // 
            this.rbExport.AutoSize = true;
            this.rbExport.Location = new System.Drawing.Point(78, 8);
            this.rbExport.Name = "rbExport";
            this.rbExport.Size = new System.Drawing.Size(55, 17);
            this.rbExport.TabIndex = 1;
            this.rbExport.Text = "E&xport";
            this.toolTip1.SetToolTip(this.rbExport, "Use EXPORT as modifier");
            this.rbExport.UseVisualStyleBackColor = true;
            // 
            // rbNone
            // 
            this.rbNone.AutoSize = true;
            this.rbNone.Location = new System.Drawing.Point(140, 9);
            this.rbNone.Name = "rbNone";
            this.rbNone.Size = new System.Drawing.Size(51, 17);
            this.rbNone.TabIndex = 2;
            this.rbNone.Text = "&None";
            this.toolTip1.SetToolTip(this.rbNone, "Use <no> modifier for public properties and methods");
            this.rbNone.UseVisualStyleBackColor = true;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(15, 77);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(147, 13);
            this.label1.TabIndex = 3;
            this.label1.Text = "Modifier for Private properties:";
            this.toolTip1.SetToolTip(this.label1, "Specify the modifier the code generator should use for private properties");
            // 
            // rbPrivate
            // 
            this.rbPrivate.AutoSize = true;
            this.rbPrivate.Location = new System.Drawing.Point(17, 10);
            this.rbPrivate.Name = "rbPrivate";
            this.rbPrivate.Size = new System.Drawing.Size(58, 17);
            this.rbPrivate.TabIndex = 0;
            this.rbPrivate.TabStop = true;
            this.rbPrivate.Text = "P&rivate";
            this.toolTip1.SetToolTip(this.rbPrivate, "Use PRIVATE as modifier");
            this.rbPrivate.UseVisualStyleBackColor = true;
            // 
            // rbHidden
            // 
            this.rbHidden.AutoSize = true;
            this.rbHidden.Location = new System.Drawing.Point(78, 10);
            this.rbHidden.Name = "rbHidden";
            this.rbHidden.Size = new System.Drawing.Size(59, 17);
            this.rbHidden.TabIndex = 1;
            this.rbHidden.Text = "&Hidden";
            this.toolTip1.SetToolTip(this.rbHidden, "Use HIDDEN as modifier");
            this.rbHidden.UseVisualStyleBackColor = true;
            // 
            // chkShowXMLComments
            // 
            this.chkShowXMLComments.AutoSize = true;
            this.chkShowXMLComments.Location = new System.Drawing.Point(20, 18);
            this.chkShowXMLComments.Name = "chkShowXMLComments";
            this.chkShowXMLComments.Size = new System.Drawing.Size(341, 17);
            this.chkShowXMLComments.TabIndex = 0;
            this.chkShowXMLComments.Text = "Show XML comments in generated source code for Goto Definition";
            this.chkShowXMLComments.UseVisualStyleBackColor = true;
            // 
            // grpCodeGenerator
            // 
            this.grpCodeGenerator.Controls.Add(this.grpPrivate);
            this.grpCodeGenerator.Controls.Add(this.groupBox1);
            this.grpCodeGenerator.Controls.Add(this.label1);
            this.grpCodeGenerator.Controls.Add(this.lblPublic);
            this.grpCodeGenerator.Controls.Add(this.chkShowXMLComments);
            this.grpCodeGenerator.Location = new System.Drawing.Point(3, 3);
            this.grpCodeGenerator.Name = "grpCodeGenerator";
            this.grpCodeGenerator.Size = new System.Drawing.Size(385, 119);
            this.grpCodeGenerator.TabIndex = 0;
            this.grpCodeGenerator.TabStop = false;
            this.grpCodeGenerator.Text = "Code Generator";
            // 
            // grpPrivate
            // 
            this.grpPrivate.Controls.Add(this.rbPrivate);
            this.grpPrivate.Controls.Add(this.rbHidden);
            this.grpPrivate.Location = new System.Drawing.Point(165, 69);
            this.grpPrivate.Name = "grpPrivate";
            this.grpPrivate.Size = new System.Drawing.Size(207, 33);
            this.grpPrivate.TabIndex = 4;
            this.grpPrivate.TabStop = false;
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.rbPublic);
            this.groupBox1.Controls.Add(this.rbExport);
            this.groupBox1.Controls.Add(this.rbNone);
            this.groupBox1.Location = new System.Drawing.Point(165, 40);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(203, 29);
            this.groupBox1.TabIndex = 2;
            this.groupBox1.TabStop = false;
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.grpCodeGenerator);
            this.panel1.Location = new System.Drawing.Point(0, 0);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(391, 158);
            this.panel1.TabIndex = 1;
            // 
            // GeneratorOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoScroll = true;
            this.Controls.Add(this.panel1);
            this.Name = "GeneratorOptionsControl";
            this.Size = new System.Drawing.Size(395, 173);
            this.grpCodeGenerator.ResumeLayout(false);
            this.grpCodeGenerator.PerformLayout();
            this.grpPrivate.ResumeLayout(false);
            this.grpPrivate.PerformLayout();
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.panel1.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.ToolTip toolTip1;
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
        private System.Windows.Forms.Panel panel1;
    }
}
