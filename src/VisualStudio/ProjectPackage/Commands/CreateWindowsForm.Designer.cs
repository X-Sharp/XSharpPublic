namespace XSharp.Project
{
    partial class CreateWindowsForm
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
            this.lblOriginalForm = new System.Windows.Forms.Label();
            this.lblNewForm = new System.Windows.Forms.Label();
            this.tbOriginalForm = new System.Windows.Forms.TextBox();
            this.tbNewForm = new System.Windows.Forms.TextBox();
            this.btnCancel = new System.Windows.Forms.Button();
            this.btnOk = new System.Windows.Forms.Button();
            this.lblFileName = new System.Windows.Forms.Label();
            this.lblFolder = new System.Windows.Forms.Label();
            this.lblFolderName = new System.Windows.Forms.Label();
            this.lblFileNames = new System.Windows.Forms.Label();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // lblOriginalForm
            // 
            this.lblOriginalForm.AutoSize = true;
            this.lblOriginalForm.Location = new System.Drawing.Point(15, 25);
            this.lblOriginalForm.Name = "lblOriginalForm";
            this.lblOriginalForm.Size = new System.Drawing.Size(104, 13);
            this.lblOriginalForm.TabIndex = 2;
            this.lblOriginalForm.Text = "Original class name :";
            // 
            // lblNewForm
            // 
            this.lblNewForm.AutoSize = true;
            this.lblNewForm.Location = new System.Drawing.Point(15, 60);
            this.lblNewForm.Name = "lblNewForm";
            this.lblNewForm.Size = new System.Drawing.Size(91, 13);
            this.lblNewForm.TabIndex = 0;
            this.lblNewForm.Text = "New class name :";
            // 
            // tbOriginalForm
            // 
            this.tbOriginalForm.Location = new System.Drawing.Point(134, 25);
            this.tbOriginalForm.Name = "tbOriginalForm";
            this.tbOriginalForm.ReadOnly = true;
            this.tbOriginalForm.Size = new System.Drawing.Size(341, 20);
            this.tbOriginalForm.TabIndex = 3;
            // 
            // tbNewForm
            // 
            this.tbNewForm.Location = new System.Drawing.Point(134, 60);
            this.tbNewForm.Name = "tbNewForm";
            this.tbNewForm.Size = new System.Drawing.Size(341, 20);
            this.tbNewForm.TabIndex = 1;
            this.tbNewForm.TextChanged += new System.EventHandler(this.tbNewForm_TextChanged);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.Location = new System.Drawing.Point(424, 193);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(91, 23);
            this.btnCancel.TabIndex = 2;
            this.btnCancel.Text = "&Cancel";
            this.btnCancel.UseVisualStyleBackColor = true;
            // 
            // btnOk
            // 
            this.btnOk.Location = new System.Drawing.Point(327, 193);
            this.btnOk.Name = "btnOk";
            this.btnOk.Size = new System.Drawing.Size(91, 23);
            this.btnOk.TabIndex = 1;
            this.btnOk.Text = "&OK";
            this.btnOk.UseVisualStyleBackColor = true;
            this.btnOk.Click += new System.EventHandler(this.btnOk_Click);
            // 
            // lblFileName
            // 
            this.lblFileName.AutoSize = true;
            this.lblFileName.Location = new System.Drawing.Point(134, 131);
            this.lblFileName.Name = "lblFileName";
            this.lblFileName.Size = new System.Drawing.Size(257, 13);
            this.lblFileName.TabIndex = 6;
            this.lblFileName.Text = "This will generate the file {0}.prg and {0}.designer.prg";
            // 
            // lblFolder
            // 
            this.lblFolder.AutoSize = true;
            this.lblFolder.Location = new System.Drawing.Point(15, 95);
            this.lblFolder.Name = "lblFolder";
            this.lblFolder.Size = new System.Drawing.Size(71, 13);
            this.lblFolder.TabIndex = 7;
            this.lblFolder.Text = "Folder name :";
            // 
            // lblFolderName
            // 
            this.lblFolderName.AutoSize = true;
            this.lblFolderName.Location = new System.Drawing.Point(134, 95);
            this.lblFolderName.Name = "lblFolderName";
            this.lblFolderName.Size = new System.Drawing.Size(64, 13);
            this.lblFolderName.TabIndex = 8;
            this.lblFolderName.Text = "FolderName";
            // 
            // lblFileNames
            // 
            this.lblFileNames.AutoSize = true;
            this.lblFileNames.Location = new System.Drawing.Point(15, 131);
            this.lblFileNames.Name = "lblFileNames";
            this.lblFileNames.Size = new System.Drawing.Size(63, 13);
            this.lblFileNames.TabIndex = 9;
            this.lblFileNames.Text = "File names :";
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.tbOriginalForm);
            this.groupBox1.Controls.Add(this.lblFileNames);
            this.groupBox1.Controls.Add(this.lblOriginalForm);
            this.groupBox1.Controls.Add(this.lblFolderName);
            this.groupBox1.Controls.Add(this.lblNewForm);
            this.groupBox1.Controls.Add(this.lblFolder);
            this.groupBox1.Controls.Add(this.tbNewForm);
            this.groupBox1.Controls.Add(this.lblFileName);
            this.groupBox1.Location = new System.Drawing.Point(12, 12);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(503, 174);
            this.groupBox1.TabIndex = 0;
            this.groupBox1.TabStop = false;
            // 
            // CreateWindowsForm
            // 
            this.AcceptButton = this.btnOk;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(528, 228);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.btnOk);
            this.Controls.Add(this.btnCancel);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "CreateWindowsForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Create Windows Form";
            this.VisibleChanged += new System.EventHandler(this.CreateWindowsForm_VisibleChanged);
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Label lblOriginalForm;
        private System.Windows.Forms.Label lblNewForm;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Button btnOk;
        internal System.Windows.Forms.TextBox tbOriginalForm;
        internal System.Windows.Forms.TextBox tbNewForm;
        private System.Windows.Forms.Label lblFileName;
        private System.Windows.Forms.Label lblFolder;
        private System.Windows.Forms.Label lblFolderName;
        private System.Windows.Forms.Label lblFileNames;
        private System.Windows.Forms.GroupBox groupBox1;
    }
}
