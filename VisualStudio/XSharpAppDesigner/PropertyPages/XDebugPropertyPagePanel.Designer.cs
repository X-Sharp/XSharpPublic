//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

namespace XSharp.Project
{
    partial class XDebugPropertyPagePanel
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
        // todo enable / disable options based on dialect

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel();
            this.lblCommand = new System.Windows.Forms.Label();
            this.lblCommandArguments = new System.Windows.Forms.Label();
            this.lblDebugInfo = new System.Windows.Forms.Label();
            this.lblWorkDir = new System.Windows.Forms.Label();
            this.chkUnmanagedDebugging = new System.Windows.Forms.CheckBox();
            this.tbCommand = new System.Windows.Forms.TextBox();
            this.tbArguments = new System.Windows.Forms.TextBox();
            this.tbWorkdir = new System.Windows.Forms.TextBox();
            this.comboDebugInfo = new System.Windows.Forms.ComboBox();
            this.btnCommand = new System.Windows.Forms.Button();
            this.btnDebuggerWorkingDirectory = new System.Windows.Forms.Button();
            this.tableLayoutPanel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // tableLayoutPanel1
            // 
            this.tableLayoutPanel1.ColumnCount = 3;
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.tableLayoutPanel1.Controls.Add(this.lblCommand, 0, 0);
            this.tableLayoutPanel1.Controls.Add(this.lblCommandArguments, 0, 1);
            this.tableLayoutPanel1.Controls.Add(this.lblDebugInfo, 0, 2);
            this.tableLayoutPanel1.Controls.Add(this.lblWorkDir, 0, 3);
            this.tableLayoutPanel1.Controls.Add(this.chkUnmanagedDebugging, 0, 4);
            this.tableLayoutPanel1.Controls.Add(this.tbCommand, 1, 0);
            this.tableLayoutPanel1.Controls.Add(this.tbArguments, 1, 1);
            this.tableLayoutPanel1.Controls.Add(this.tbWorkdir, 1, 3);
            this.tableLayoutPanel1.Controls.Add(this.comboDebugInfo, 1, 2);
            this.tableLayoutPanel1.Controls.Add(this.btnCommand, 2, 0);
            this.tableLayoutPanel1.Controls.Add(this.btnDebuggerWorkingDirectory, 2, 3);
            this.tableLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tableLayoutPanel1.ForeColor = System.Drawing.Color.Black;
            this.tableLayoutPanel1.Location = new System.Drawing.Point(0, 0);
            this.tableLayoutPanel1.Name = "tableLayoutPanel1";
            this.tableLayoutPanel1.RowCount = 13;
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tableLayoutPanel1.Size = new System.Drawing.Size(699, 457);
            this.tableLayoutPanel1.TabIndex = 0;
            // 
            // lblCommand
            // 
            this.lblCommand.AutoSize = true;
            this.lblCommand.ForeColor = System.Drawing.Color.Black;
            this.lblCommand.Location = new System.Drawing.Point(3, 0);
            this.lblCommand.Name = "lblCommand";
            this.lblCommand.Size = new System.Drawing.Size(54, 13);
            this.lblCommand.TabIndex = 0;
            this.lblCommand.Text = "Command";
            // 
            // lblCommandArguments
            // 
            this.lblCommandArguments.AutoSize = true;
            this.lblCommandArguments.ForeColor = System.Drawing.Color.Black;
            this.lblCommandArguments.Location = new System.Drawing.Point(3, 29);
            this.lblCommandArguments.Name = "lblCommandArguments";
            this.lblCommandArguments.Size = new System.Drawing.Size(57, 13);
            this.lblCommandArguments.TabIndex = 3;
            this.lblCommandArguments.Text = "Arguments";
            // 
            // lblDebugInfo
            // 
            this.lblDebugInfo.AutoSize = true;
            this.lblDebugInfo.ForeColor = System.Drawing.Color.Black;
            this.lblDebugInfo.Location = new System.Drawing.Point(3, 95);
            this.lblDebugInfo.Name = "lblDebugInfo";
            this.lblDebugInfo.Size = new System.Drawing.Size(141, 13);
            this.lblDebugInfo.TabIndex = 5;
            this.lblDebugInfo.Text = "Generate Debug Information";
            // 
            // lblWorkDir
            // 
            this.lblWorkDir.AutoSize = true;
            this.lblWorkDir.ForeColor = System.Drawing.Color.Black;
            this.lblWorkDir.Location = new System.Drawing.Point(3, 122);
            this.lblWorkDir.Name = "lblWorkDir";
            this.lblWorkDir.Size = new System.Drawing.Size(92, 13);
            this.lblWorkDir.TabIndex = 7;
            this.lblWorkDir.Text = "Working Directory";
            // 
            // chkUnmanagedDebugging
            // 
            this.chkUnmanagedDebugging.AutoSize = true;
            this.chkUnmanagedDebugging.ForeColor = System.Drawing.Color.Black;
            this.chkUnmanagedDebugging.Location = new System.Drawing.Point(3, 154);
            this.chkUnmanagedDebugging.Name = "chkUnmanagedDebugging";
            this.chkUnmanagedDebugging.Size = new System.Drawing.Size(139, 17);
            this.chkUnmanagedDebugging.TabIndex = 10;
            this.chkUnmanagedDebugging.Text = "Unmanaged Debugging";
            this.chkUnmanagedDebugging.UseVisualStyleBackColor = true;
            // 
            // tbCommand
            // 
            this.tbCommand.BackColor = System.Drawing.Color.White;
            this.tbCommand.ForeColor = System.Drawing.Color.Black;
            this.tbCommand.Location = new System.Drawing.Point(150, 3);
            this.tbCommand.Name = "tbCommand";
            this.tbCommand.Size = new System.Drawing.Size(300, 20);
            this.tbCommand.TabIndex = 1;
            // 
            // tbArguments
            // 
            this.tbArguments.BackColor = System.Drawing.Color.White;
            this.tbArguments.ForeColor = System.Drawing.Color.Black;
            this.tbArguments.Location = new System.Drawing.Point(150, 32);
            this.tbArguments.Multiline = true;
            this.tbArguments.Name = "tbArguments";
            this.tbArguments.Size = new System.Drawing.Size(300, 60);
            this.tbArguments.TabIndex = 4;
            // 
            // tbWorkdir
            // 
            this.tbWorkdir.BackColor = System.Drawing.Color.White;
            this.tbWorkdir.ForeColor = System.Drawing.Color.Black;
            this.tbWorkdir.Location = new System.Drawing.Point(150, 125);
            this.tbWorkdir.Name = "tbWorkdir";
            this.tbWorkdir.Size = new System.Drawing.Size(300, 20);
            this.tbWorkdir.TabIndex = 8;
            // 
            // comboDebugInfo
            // 
            this.comboDebugInfo.BackColor = System.Drawing.Color.White;
            this.comboDebugInfo.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboDebugInfo.ForeColor = System.Drawing.Color.Black;
            this.comboDebugInfo.FormattingEnabled = true;
            this.comboDebugInfo.Location = new System.Drawing.Point(150, 98);
            this.comboDebugInfo.Name = "comboDebugInfo";
            this.comboDebugInfo.Size = new System.Drawing.Size(300, 21);
            this.comboDebugInfo.TabIndex = 6;
            // 
            // btnCommand
            // 
            this.btnCommand.ForeColor = System.Drawing.Color.Black;
            this.btnCommand.Location = new System.Drawing.Point(456, 3);
            this.btnCommand.Name = "btnCommand";
            this.btnCommand.Size = new System.Drawing.Size(23, 23);
            this.btnCommand.TabIndex = 2;
            this.btnCommand.Text = "...";
            this.btnCommand.UseVisualStyleBackColor = true;
            this.btnCommand.Click += new System.EventHandler(this.btnCommand_Click);
            // 
            // btnDebuggerWorkingDirectory
            // 
            this.btnDebuggerWorkingDirectory.ForeColor = System.Drawing.Color.Black;
            this.btnDebuggerWorkingDirectory.Location = new System.Drawing.Point(456, 125);
            this.btnDebuggerWorkingDirectory.Name = "btnDebuggerWorkingDirectory";
            this.btnDebuggerWorkingDirectory.Size = new System.Drawing.Size(23, 23);
            this.btnDebuggerWorkingDirectory.TabIndex = 9;
            this.btnDebuggerWorkingDirectory.Text = "...";
            this.btnDebuggerWorkingDirectory.UseVisualStyleBackColor = true;
            this.btnDebuggerWorkingDirectory.Click += new System.EventHandler(this.btnDebuggerWorkingDirectory_Click);
            // 
            // XDebugPropertyPagePanel
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.tableLayoutPanel1);
            this.Margin = new System.Windows.Forms.Padding(5);
            this.Name = "XDebugPropertyPagePanel";
            this.Size = new System.Drawing.Size(699, 457);
            this.tableLayoutPanel1.ResumeLayout(false);
            this.tableLayoutPanel1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.ToolTip toolTip1;
        private System.Windows.Forms.TableLayoutPanel tableLayoutPanel1;
        private System.Windows.Forms.Label lblCommand;
        private System.Windows.Forms.Label lblCommandArguments;
        private System.Windows.Forms.Label lblDebugInfo;
        private System.Windows.Forms.Label lblWorkDir;
        private System.Windows.Forms.CheckBox chkUnmanagedDebugging;
        private System.Windows.Forms.TextBox tbCommand;
        private System.Windows.Forms.TextBox tbArguments;
        private System.Windows.Forms.TextBox tbWorkdir;
        private System.Windows.Forms.ComboBox comboDebugInfo;
        private System.Windows.Forms.Button btnCommand;
        private System.Windows.Forms.Button btnDebuggerWorkingDirectory;
    }
}
