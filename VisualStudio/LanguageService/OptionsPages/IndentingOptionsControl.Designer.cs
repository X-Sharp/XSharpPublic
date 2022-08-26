namespace XSharp.LanguageService.OptionsPages
{
    partial class IndentingOptionsControl
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
            this.panel1 = new System.Windows.Forms.Panel();
            this.treeIndentStyle = new System.Windows.Forms.TreeView();
            this.codeSample = new System.Windows.Forms.RichTextBox();
            label1 = new System.Windows.Forms.Label();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // label1
            // 
            label1.Location = new System.Drawing.Point(0, 0);
            label1.Name = "label1";
            label1.Size = new System.Drawing.Size(100, 23);
            label1.TabIndex = 0;
            // 
            // panel1
            // 
            this.panel1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.panel1.Controls.Add(this.treeIndentStyle);
            this.panel1.Controls.Add(this.codeSample);
            this.panel1.Location = new System.Drawing.Point(0, 0);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(425, 311);
            this.panel1.TabIndex = 0;
            // 
            // treeIndentStyle
            // 
            this.treeIndentStyle.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.treeIndentStyle.FullRowSelect = true;
            this.treeIndentStyle.HideSelection = false;
            this.treeIndentStyle.Location = new System.Drawing.Point(12, 14);
            this.treeIndentStyle.Name = "treeIndentStyle";
            this.treeIndentStyle.ShowLines = false;
            this.treeIndentStyle.ShowRootLines = false;
            this.treeIndentStyle.Size = new System.Drawing.Size(401, 129);
            this.treeIndentStyle.TabIndex = 3;
            // 
            // codeSample
            // 
            this.codeSample.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.codeSample.BackColor = System.Drawing.SystemColors.Window;
            this.codeSample.Location = new System.Drawing.Point(12, 148);
            this.codeSample.Margin = new System.Windows.Forms.Padding(2);
            this.codeSample.Name = "codeSample";
            this.codeSample.ReadOnly = true;
            this.codeSample.Size = new System.Drawing.Size(401, 152);
            this.codeSample.TabIndex = 1;
            this.codeSample.Text = "";
            // 
            // IndentingOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoScroll = true;
            this.Controls.Add(this.panel1);
            this.Name = "IndentingOptionsControl";
            this.Size = new System.Drawing.Size(440, 327);
            this.panel1.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.ToolTip toolTip1;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.TreeView treeIndentStyle;
        private System.Windows.Forms.RichTextBox codeSample;
    }
}
