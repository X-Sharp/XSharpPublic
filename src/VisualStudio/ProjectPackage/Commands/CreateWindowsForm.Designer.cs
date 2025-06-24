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
            this.SuspendLayout();
            // 
            // lblOriginalForm
            // 
            this.lblOriginalForm.AutoSize = true;
            this.lblOriginalForm.Location = new System.Drawing.Point(29, 14);
            this.lblOriginalForm.Name = "lblOriginalForm";
            this.lblOriginalForm.Size = new System.Drawing.Size(98, 13);
            this.lblOriginalForm.TabIndex = 0;
            this.lblOriginalForm.Text = "Original ClassName";
            // 
            // lblNewForm
            // 
            this.lblNewForm.AutoSize = true;
            this.lblNewForm.Location = new System.Drawing.Point(29, 49);
            this.lblNewForm.Name = "lblNewForm";
            this.lblNewForm.Size = new System.Drawing.Size(85, 13);
            this.lblNewForm.TabIndex = 1;
            this.lblNewForm.Text = "New ClassName";
            // 
            // tbOriginalForm
            // 
            this.tbOriginalForm.Location = new System.Drawing.Point(148, 14);
            this.tbOriginalForm.Name = "tbOriginalForm";
            this.tbOriginalForm.ReadOnly = true;
            this.tbOriginalForm.Size = new System.Drawing.Size(288, 20);
            this.tbOriginalForm.TabIndex = 2;
            // 
            // tbNewForm
            // 
            this.tbNewForm.Location = new System.Drawing.Point(148, 49);
            this.tbNewForm.Name = "tbNewForm";
            this.tbNewForm.Size = new System.Drawing.Size(288, 20);
            this.tbNewForm.TabIndex = 3;
            this.tbNewForm.TextChanged += new System.EventHandler(this.tbNewForm_TextChanged);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.Location = new System.Drawing.Point(273, 150);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(75, 23);
            this.btnCancel.TabIndex = 4;
            this.btnCancel.Text = "&Cancel";
            this.btnCancel.UseVisualStyleBackColor = true;
            // 
            // btnOk
            // 
            this.btnOk.Location = new System.Drawing.Point(361, 150);
            this.btnOk.Name = "btnOk";
            this.btnOk.Size = new System.Drawing.Size(75, 23);
            this.btnOk.TabIndex = 5;
            this.btnOk.Text = "&Ok";
            this.btnOk.UseVisualStyleBackColor = true;
            this.btnOk.Click += new System.EventHandler(this.btnOk_Click);
            // 
            // lblFileName
            // 
            this.lblFileName.AutoSize = true;
            this.lblFileName.Location = new System.Drawing.Point(148, 120);
            this.lblFileName.Name = "lblFileName";
            this.lblFileName.Size = new System.Drawing.Size(257, 13);
            this.lblFileName.TabIndex = 6;
            this.lblFileName.Text = "This will generate the file {0}.prg and {0}.designer.prg";
            // 
            // lblFolder
            // 
            this.lblFolder.AutoSize = true;
            this.lblFolder.Location = new System.Drawing.Point(29, 84);
            this.lblFolder.Name = "lblFolder";
            this.lblFolder.Size = new System.Drawing.Size(64, 13);
            this.lblFolder.TabIndex = 7;
            this.lblFolder.Text = "FolderName";
            // 
            // lblFolderName
            // 
            this.lblFolderName.AutoSize = true;
            this.lblFolderName.Location = new System.Drawing.Point(148, 84);
            this.lblFolderName.Name = "lblFolderName";
            this.lblFolderName.Size = new System.Drawing.Size(64, 13);
            this.lblFolderName.TabIndex = 8;
            this.lblFolderName.Text = "FolderName";
            // 
            // lblFileNames
            // 
            this.lblFileNames.AutoSize = true;
            this.lblFileNames.Location = new System.Drawing.Point(29, 120);
            this.lblFileNames.Name = "lblFileNames";
            this.lblFileNames.Size = new System.Drawing.Size(56, 13);
            this.lblFileNames.TabIndex = 9;
            this.lblFileNames.Text = "FileNames";
            // 
            // CreateWindowsForm
            // 
            this.AcceptButton = this.btnOk;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(468, 197);
            this.Controls.Add(this.lblFileNames);
            this.Controls.Add(this.lblFolderName);
            this.Controls.Add(this.lblFolder);
            this.Controls.Add(this.lblFileName);
            this.Controls.Add(this.btnOk);
            this.Controls.Add(this.btnCancel);
            this.Controls.Add(this.tbNewForm);
            this.Controls.Add(this.tbOriginalForm);
            this.Controls.Add(this.lblNewForm);
            this.Controls.Add(this.lblOriginalForm);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "CreateWindowsForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Create Windows Form";
            this.VisibleChanged += new System.EventHandler(this.CreateWindowsForm_VisibleChanged);
            this.ResumeLayout(false);
            this.PerformLayout();

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
    }
}
