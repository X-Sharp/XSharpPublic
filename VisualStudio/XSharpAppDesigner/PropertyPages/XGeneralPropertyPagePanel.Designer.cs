//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

namespace XSharp.Project
{
    partial class XGeneralPropertyPagePanel
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
            this.tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel();
            this.comboDialect = new System.Windows.Forms.ComboBox();
            this.lblDialect = new System.Windows.Forms.Label();
            this.comboOutputType = new System.Windows.Forms.ComboBox();
            this.tbDefaultNamespace = new System.Windows.Forms.TextBox();
            this.lblDefaultNamespace = new System.Windows.Forms.Label();
            this.lblApplicationName = new System.Windows.Forms.Label();
            this.lblTargetFramework = new System.Windows.Forms.Label();
            this.comboTargetFramework = new System.Windows.Forms.ComboBox();
            this.lblOutputType = new System.Windows.Forms.Label();
            this.tbAssemblyName = new System.Windows.Forms.TextBox();
            this.chkAutoGenerateBindingRedirects = new System.Windows.Forms.CheckBox();
            this.labelStartupObject = new System.Windows.Forms.Label();
            this.comboStartupObject = new System.Windows.Forms.ComboBox();
            this.lblResources = new System.Windows.Forms.Label();
            this.labelIcon = new System.Windows.Forms.Label();
            this.tbAppIcon = new System.Windows.Forms.TextBox();
            this.chkSuppressDefaultManifest = new System.Windows.Forms.CheckBox();
            this.chkPreferNativeVersion = new System.Windows.Forms.CheckBox();
            this.chkVulcanCompatibleResources = new System.Windows.Forms.CheckBox();
            this.btnIcon = new System.Windows.Forms.Button();
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.tableLayoutPanel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // tableLayoutPanel1
            // 
            this.tableLayoutPanel1.ColumnCount = 2;
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 50F));
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 50F));
            this.tableLayoutPanel1.Controls.Add(this.comboDialect, 0, 5);
            this.tableLayoutPanel1.Controls.Add(this.lblDialect, 0, 4);
            this.tableLayoutPanel1.Controls.Add(this.comboOutputType, 1, 3);
            this.tableLayoutPanel1.Controls.Add(this.tbDefaultNamespace, 1, 1);
            this.tableLayoutPanel1.Controls.Add(this.lblDefaultNamespace, 1, 0);
            this.tableLayoutPanel1.Controls.Add(this.lblApplicationName, 0, 0);
            this.tableLayoutPanel1.Controls.Add(this.lblTargetFramework, 0, 2);
            this.tableLayoutPanel1.Controls.Add(this.comboTargetFramework, 0, 3);
            this.tableLayoutPanel1.Controls.Add(this.lblOutputType, 1, 2);
            this.tableLayoutPanel1.Controls.Add(this.tbAssemblyName, 0, 1);
            this.tableLayoutPanel1.Controls.Add(this.chkAutoGenerateBindingRedirects, 0, 6);
            this.tableLayoutPanel1.Controls.Add(this.labelStartupObject, 0, 7);
            this.tableLayoutPanel1.Controls.Add(this.comboStartupObject, 0, 8);
            this.tableLayoutPanel1.Controls.Add(this.lblResources, 0, 9);
            this.tableLayoutPanel1.Controls.Add(this.labelIcon, 0, 10);
            this.tableLayoutPanel1.Controls.Add(this.tbAppIcon, 0, 11);
            this.tableLayoutPanel1.Controls.Add(this.chkSuppressDefaultManifest, 0, 12);
            this.tableLayoutPanel1.Controls.Add(this.chkPreferNativeVersion, 0, 13);
            this.tableLayoutPanel1.Controls.Add(this.chkVulcanCompatibleResources, 0, 14);
            this.tableLayoutPanel1.Controls.Add(this.btnIcon, 1, 11);
            this.tableLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Left;
            this.tableLayoutPanel1.ForeColor = System.Drawing.Color.Black;
            this.tableLayoutPanel1.Location = new System.Drawing.Point(0, 0);
            this.tableLayoutPanel1.Margin = new System.Windows.Forms.Padding(5);
            this.tableLayoutPanel1.Name = "tableLayoutPanel1";
            this.tableLayoutPanel1.RowCount = 15;
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tableLayoutPanel1.Size = new System.Drawing.Size(633, 341);
            this.tableLayoutPanel1.TabIndex = 0;
            // 
            // comboDialect
            // 
            this.comboDialect.BackColor = System.Drawing.Color.White;
            this.comboDialect.Dock = System.Windows.Forms.DockStyle.Fill;
            this.comboDialect.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboDialect.ForeColor = System.Drawing.Color.Black;
            this.comboDialect.FormattingEnabled = true;
            this.comboDialect.Location = new System.Drawing.Point(5, 98);
            this.comboDialect.Margin = new System.Windows.Forms.Padding(5, 3, 3, 0);
            this.comboDialect.Name = "comboDialect";
            this.comboDialect.Size = new System.Drawing.Size(308, 21);
            this.comboDialect.TabIndex = 9;
            // 
            // lblDialect
            // 
            this.lblDialect.AutoSize = true;
            this.lblDialect.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lblDialect.ForeColor = System.Drawing.Color.Black;
            this.lblDialect.Location = new System.Drawing.Point(5, 82);
            this.lblDialect.Margin = new System.Windows.Forms.Padding(5, 3, 3, 0);
            this.lblDialect.Name = "lblDialect";
            this.lblDialect.Size = new System.Drawing.Size(308, 13);
            this.lblDialect.TabIndex = 8;
            this.lblDialect.Text = "Dialect:";
            // 
            // comboOutputType
            // 
            this.comboOutputType.BackColor = System.Drawing.Color.White;
            this.comboOutputType.Dock = System.Windows.Forms.DockStyle.Fill;
            this.comboOutputType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboOutputType.ForeColor = System.Drawing.Color.Black;
            this.comboOutputType.FormattingEnabled = true;
            this.comboOutputType.Location = new System.Drawing.Point(321, 58);
            this.comboOutputType.Margin = new System.Windows.Forms.Padding(5, 3, 3, 0);
            this.comboOutputType.Name = "comboOutputType";
            this.comboOutputType.Size = new System.Drawing.Size(309, 21);
            this.comboOutputType.TabIndex = 7;
            // 
            // tbDefaultNamespace
            // 
            this.tbDefaultNamespace.BackColor = System.Drawing.Color.White;
            this.tbDefaultNamespace.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tbDefaultNamespace.ForeColor = System.Drawing.Color.Black;
            this.tbDefaultNamespace.Location = new System.Drawing.Point(321, 19);
            this.tbDefaultNamespace.Margin = new System.Windows.Forms.Padding(5, 3, 3, 0);
            this.tbDefaultNamespace.Name = "tbDefaultNamespace";
            this.tbDefaultNamespace.Size = new System.Drawing.Size(309, 20);
            this.tbDefaultNamespace.TabIndex = 3;
            // 
            // lblDefaultNamespace
            // 
            this.lblDefaultNamespace.AutoSize = true;
            this.lblDefaultNamespace.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lblDefaultNamespace.ForeColor = System.Drawing.Color.Black;
            this.lblDefaultNamespace.Location = new System.Drawing.Point(321, 3);
            this.lblDefaultNamespace.Margin = new System.Windows.Forms.Padding(5, 3, 3, 0);
            this.lblDefaultNamespace.Name = "lblDefaultNamespace";
            this.lblDefaultNamespace.Size = new System.Drawing.Size(309, 13);
            this.lblDefaultNamespace.TabIndex = 2;
            this.lblDefaultNamespace.Text = "Default Namespace:";
            // 
            // lblApplicationName
            // 
            this.lblApplicationName.AutoSize = true;
            this.lblApplicationName.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lblApplicationName.ForeColor = System.Drawing.Color.Black;
            this.lblApplicationName.Location = new System.Drawing.Point(5, 3);
            this.lblApplicationName.Margin = new System.Windows.Forms.Padding(5, 3, 3, 0);
            this.lblApplicationName.Name = "lblApplicationName";
            this.lblApplicationName.Size = new System.Drawing.Size(308, 13);
            this.lblApplicationName.TabIndex = 0;
            this.lblApplicationName.Text = "Application Name";
            // 
            // lblTargetFramework
            // 
            this.lblTargetFramework.AutoSize = true;
            this.lblTargetFramework.ForeColor = System.Drawing.Color.Black;
            this.lblTargetFramework.Location = new System.Drawing.Point(5, 42);
            this.lblTargetFramework.Margin = new System.Windows.Forms.Padding(5, 3, 3, 0);
            this.lblTargetFramework.Name = "lblTargetFramework";
            this.lblTargetFramework.Size = new System.Drawing.Size(96, 13);
            this.lblTargetFramework.TabIndex = 4;
            this.lblTargetFramework.Text = "Target Framework:";
            // 
            // comboTargetFramework
            // 
            this.comboTargetFramework.BackColor = System.Drawing.Color.White;
            this.comboTargetFramework.Dock = System.Windows.Forms.DockStyle.Fill;
            this.comboTargetFramework.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboTargetFramework.ForeColor = System.Drawing.Color.Black;
            this.comboTargetFramework.FormattingEnabled = true;
            this.comboTargetFramework.Location = new System.Drawing.Point(5, 58);
            this.comboTargetFramework.Margin = new System.Windows.Forms.Padding(5, 3, 3, 0);
            this.comboTargetFramework.Name = "comboTargetFramework";
            this.comboTargetFramework.Size = new System.Drawing.Size(308, 21);
            this.comboTargetFramework.TabIndex = 5;
            // 
            // lblOutputType
            // 
            this.lblOutputType.AutoSize = true;
            this.lblOutputType.ForeColor = System.Drawing.Color.Black;
            this.lblOutputType.Location = new System.Drawing.Point(321, 42);
            this.lblOutputType.Margin = new System.Windows.Forms.Padding(5, 3, 3, 0);
            this.lblOutputType.Name = "lblOutputType";
            this.lblOutputType.Size = new System.Drawing.Size(69, 13);
            this.lblOutputType.TabIndex = 6;
            this.lblOutputType.Text = "Output Type:";
            // 
            // tbAssemblyName
            // 
            this.tbAssemblyName.BackColor = System.Drawing.Color.White;
            this.tbAssemblyName.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tbAssemblyName.ForeColor = System.Drawing.Color.Black;
            this.tbAssemblyName.Location = new System.Drawing.Point(5, 19);
            this.tbAssemblyName.Margin = new System.Windows.Forms.Padding(5, 3, 3, 0);
            this.tbAssemblyName.Name = "tbAssemblyName";
            this.tbAssemblyName.Size = new System.Drawing.Size(308, 20);
            this.tbAssemblyName.TabIndex = 1;
            // 
            // chkAutoGenerateBindingRedirects
            // 
            this.chkAutoGenerateBindingRedirects.AutoSize = true;
            this.chkAutoGenerateBindingRedirects.ForeColor = System.Drawing.Color.Black;
            this.chkAutoGenerateBindingRedirects.Location = new System.Drawing.Point(5, 122);
            this.chkAutoGenerateBindingRedirects.Margin = new System.Windows.Forms.Padding(5, 3, 3, 0);
            this.chkAutoGenerateBindingRedirects.Name = "chkAutoGenerateBindingRedirects";
            this.chkAutoGenerateBindingRedirects.Size = new System.Drawing.Size(173, 17);
            this.chkAutoGenerateBindingRedirects.TabIndex = 10;
            this.chkAutoGenerateBindingRedirects.Text = "Auto-generate binding redirects";
            this.chkAutoGenerateBindingRedirects.UseVisualStyleBackColor = true;
            // 
            // labelStartupObject
            // 
            this.labelStartupObject.AutoSize = true;
            this.labelStartupObject.ForeColor = System.Drawing.Color.Black;
            this.labelStartupObject.Location = new System.Drawing.Point(5, 142);
            this.labelStartupObject.Margin = new System.Windows.Forms.Padding(5, 3, 3, 0);
            this.labelStartupObject.Name = "labelStartupObject";
            this.labelStartupObject.Size = new System.Drawing.Size(76, 13);
            this.labelStartupObject.TabIndex = 11;
            this.labelStartupObject.Text = "Startup object:";
            // 
            // comboStartupObject
            // 
            this.comboStartupObject.BackColor = System.Drawing.Color.White;
            this.comboStartupObject.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboStartupObject.ForeColor = System.Drawing.Color.Black;
            this.comboStartupObject.FormattingEnabled = true;
            this.comboStartupObject.Location = new System.Drawing.Point(5, 158);
            this.comboStartupObject.Margin = new System.Windows.Forms.Padding(5, 3, 3, 0);
            this.comboStartupObject.Name = "comboStartupObject";
            this.comboStartupObject.Size = new System.Drawing.Size(308, 21);
            this.comboStartupObject.TabIndex = 12;
            // 
            // lblResources
            // 
            this.lblResources.AutoSize = true;
            this.lblResources.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblResources.ForeColor = System.Drawing.Color.Black;
            this.lblResources.Location = new System.Drawing.Point(5, 182);
            this.lblResources.Margin = new System.Windows.Forms.Padding(5, 3, 3, 0);
            this.lblResources.Name = "lblResources";
            this.lblResources.Size = new System.Drawing.Size(71, 13);
            this.lblResources.TabIndex = 13;
            this.lblResources.Text = "Resources:";
            // 
            // labelIcon
            // 
            this.labelIcon.AutoSize = true;
            this.labelIcon.ForeColor = System.Drawing.Color.Black;
            this.labelIcon.Location = new System.Drawing.Point(5, 198);
            this.labelIcon.Margin = new System.Windows.Forms.Padding(5, 3, 3, 0);
            this.labelIcon.Name = "labelIcon";
            this.labelIcon.Size = new System.Drawing.Size(31, 13);
            this.labelIcon.TabIndex = 14;
            this.labelIcon.Text = "Icon:";
            // 
            // tbAppIcon
            // 
            this.tbAppIcon.BackColor = System.Drawing.Color.White;
            this.tbAppIcon.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tbAppIcon.ForeColor = System.Drawing.Color.Black;
            this.tbAppIcon.Location = new System.Drawing.Point(5, 214);
            this.tbAppIcon.Margin = new System.Windows.Forms.Padding(5, 3, 3, 0);
            this.tbAppIcon.Name = "tbAppIcon";
            this.tbAppIcon.Size = new System.Drawing.Size(308, 20);
            this.tbAppIcon.TabIndex = 15;
            // 
            // chkSuppressDefaultManifest
            // 
            this.chkSuppressDefaultManifest.AutoSize = true;
            this.chkSuppressDefaultManifest.ForeColor = System.Drawing.Color.Black;
            this.chkSuppressDefaultManifest.Location = new System.Drawing.Point(5, 243);
            this.chkSuppressDefaultManifest.Margin = new System.Windows.Forms.Padding(5, 3, 3, 0);
            this.chkSuppressDefaultManifest.Name = "chkSuppressDefaultManifest";
            this.chkSuppressDefaultManifest.Size = new System.Drawing.Size(178, 17);
            this.chkSuppressDefaultManifest.TabIndex = 17;
            this.chkSuppressDefaultManifest.Text = "Suppress default win32 manifest";
            this.chkSuppressDefaultManifest.UseVisualStyleBackColor = true;
            // 
            // chkPreferNativeVersion
            // 
            this.chkPreferNativeVersion.AutoSize = true;
            this.chkPreferNativeVersion.ForeColor = System.Drawing.Color.Black;
            this.chkPreferNativeVersion.Location = new System.Drawing.Point(5, 263);
            this.chkPreferNativeVersion.Margin = new System.Windows.Forms.Padding(5, 3, 3, 0);
            this.chkPreferNativeVersion.Name = "chkPreferNativeVersion";
            this.chkPreferNativeVersion.Size = new System.Drawing.Size(308, 17);
            this.chkPreferNativeVersion.TabIndex = 18;
            this.chkPreferNativeVersion.Text = "Prefer native version resource info over managed version info";
            this.chkPreferNativeVersion.UseVisualStyleBackColor = true;
            // 
            // chkVulcanCompatibleResources
            // 
            this.chkVulcanCompatibleResources.AutoSize = true;
            this.chkVulcanCompatibleResources.ForeColor = System.Drawing.Color.Black;
            this.chkVulcanCompatibleResources.Location = new System.Drawing.Point(5, 283);
            this.chkVulcanCompatibleResources.Margin = new System.Windows.Forms.Padding(5, 3, 3, 0);
            this.chkVulcanCompatibleResources.Name = "chkVulcanCompatibleResources";
            this.chkVulcanCompatibleResources.Size = new System.Drawing.Size(209, 17);
            this.chkVulcanCompatibleResources.TabIndex = 19;
            this.chkVulcanCompatibleResources.Text = "Vulcan compatible managed resources";
            this.chkVulcanCompatibleResources.UseVisualStyleBackColor = true;
            // 
            // btnIcon
            // 
            this.btnIcon.ForeColor = System.Drawing.Color.Black;
            this.btnIcon.Location = new System.Drawing.Point(319, 214);
            this.btnIcon.Name = "btnIcon";
            this.btnIcon.Size = new System.Drawing.Size(23, 23);
            this.btnIcon.TabIndex = 16;
            this.btnIcon.Text = "...";
            this.btnIcon.UseVisualStyleBackColor = true;
            this.btnIcon.Click += new System.EventHandler(this.btnIcon_Click);
            // 
            // XGeneralPropertyPagePanel
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.tableLayoutPanel1);
            this.Margin = new System.Windows.Forms.Padding(5);
            this.Name = "XGeneralPropertyPagePanel";
            this.Size = new System.Drawing.Size(633, 341);
            this.tableLayoutPanel1.ResumeLayout(false);
            this.tableLayoutPanel1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.TableLayoutPanel tableLayoutPanel1;
        private System.Windows.Forms.Label lblDefaultNamespace;
        private System.Windows.Forms.Label lblApplicationName;
        private System.Windows.Forms.Label lblTargetFramework;
        private System.Windows.Forms.Label lblOutputType;
        private System.Windows.Forms.TextBox tbDefaultNamespace;
        private System.Windows.Forms.TextBox tbAssemblyName;
        private System.Windows.Forms.ComboBox comboOutputType;
        private System.Windows.Forms.ComboBox comboTargetFramework;
        private System.Windows.Forms.CheckBox chkAutoGenerateBindingRedirects;
        private System.Windows.Forms.Label labelStartupObject;
        private System.Windows.Forms.ComboBox comboDialect;
        private System.Windows.Forms.Label lblDialect;
        private System.Windows.Forms.TextBox tbAppIcon;
        private System.Windows.Forms.Label labelIcon;
        private System.Windows.Forms.Label lblResources;
        private System.Windows.Forms.CheckBox chkSuppressDefaultManifest;
        private System.Windows.Forms.CheckBox chkPreferNativeVersion;
        private System.Windows.Forms.CheckBox chkVulcanCompatibleResources;
        private System.Windows.Forms.Button btnIcon;
        private System.Windows.Forms.ToolTip toolTip1;
        private System.Windows.Forms.ComboBox comboStartupObject;
    }
}
