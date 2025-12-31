//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

namespace XSharp.Project
{
    partial class XGlobalUsingsPropertyPagePanel
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
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle1 = new System.Windows.Forms.DataGridViewCellStyle();
            this.chkImplicitUsings = new System.Windows.Forms.CheckBox();
            this.grpImpicitGlobalUsings = new System.Windows.Forms.GroupBox();
            this.grpManageImplicit = new System.Windows.Forms.GroupBox();
            this.gridUsings = new System.Windows.Forms.DataGridView();
            this.colNamespace = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.colAlias = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.colStatic = new System.Windows.Forms.DataGridViewCheckBoxColumn();
            this.colDelete = new System.Windows.Forms.DataGridViewButtonColumn();
            this.colImported = new System.Windows.Forms.DataGridViewCheckBoxColumn();
            this.btnAdd = new System.Windows.Forms.Button();
            this.chkStatic = new System.Windows.Forms.CheckBox();
            this.tbAlias = new System.Windows.Forms.TextBox();
            this.lblAlias = new System.Windows.Forms.Label();
            this.tbUsing = new System.Windows.Forms.TextBox();
            this.lblUsing = new System.Windows.Forms.Label();
            this.chkShowImported = new System.Windows.Forms.CheckBox();
            this.grpImpicitGlobalUsings.SuspendLayout();
            this.grpManageImplicit.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.gridUsings)).BeginInit();
            this.SuspendLayout();
            // 
            // chkImplicitUsings
            // 
            this.chkImplicitUsings.AutoSize = true;
            this.chkImplicitUsings.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.chkImplicitUsings.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.chkImplicitUsings.Location = new System.Drawing.Point(16, 19);
            this.chkImplicitUsings.Name = "chkImplicitUsings";
            this.chkImplicitUsings.Size = new System.Drawing.Size(323, 17);
            this.chkImplicitUsings.TabIndex = 0;
            this.chkImplicitUsings.Text = "Enable implicit global usings to be declared by the project SDK.";
            this.chkImplicitUsings.UseVisualStyleBackColor = true;
            // 
            // grpImpicitGlobalUsings
            // 
            this.grpImpicitGlobalUsings.Controls.Add(this.chkImplicitUsings);
            this.grpImpicitGlobalUsings.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.grpImpicitGlobalUsings.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.grpImpicitGlobalUsings.Location = new System.Drawing.Point(17, 5);
            this.grpImpicitGlobalUsings.Name = "grpImpicitGlobalUsings";
            this.grpImpicitGlobalUsings.Size = new System.Drawing.Size(454, 51);
            this.grpImpicitGlobalUsings.TabIndex = 1;
            this.grpImpicitGlobalUsings.TabStop = false;
            this.grpImpicitGlobalUsings.Text = "Implicit Global Usings";
            // 
            // grpManageImplicit
            // 
            this.grpManageImplicit.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.grpManageImplicit.Controls.Add(this.gridUsings);
            this.grpManageImplicit.Controls.Add(this.btnAdd);
            this.grpManageImplicit.Controls.Add(this.chkStatic);
            this.grpManageImplicit.Controls.Add(this.tbAlias);
            this.grpManageImplicit.Controls.Add(this.lblAlias);
            this.grpManageImplicit.Controls.Add(this.tbUsing);
            this.grpManageImplicit.Controls.Add(this.lblUsing);
            this.grpManageImplicit.Controls.Add(this.chkShowImported);
            this.grpManageImplicit.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.grpManageImplicit.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.grpManageImplicit.Location = new System.Drawing.Point(17, 73);
            this.grpManageImplicit.Name = "grpManageImplicit";
            this.grpManageImplicit.Size = new System.Drawing.Size(521, 314);
            this.grpManageImplicit.TabIndex = 2;
            this.grpManageImplicit.TabStop = false;
            this.grpManageImplicit.Text = "Manage Implicit Globals";
            // 
            // gridUsings
            // 
            this.gridUsings.AllowUserToAddRows = false;
            this.gridUsings.AllowUserToDeleteRows = false;
            this.gridUsings.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.gridUsings.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.gridUsings.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.colNamespace,
            this.colAlias,
            this.colStatic,
            this.colDelete,
            this.colImported});
            this.gridUsings.Location = new System.Drawing.Point(19, 78);
            this.gridUsings.Name = "gridUsings";
            dataGridViewCellStyle1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.gridUsings.RowsDefaultCellStyle = dataGridViewCellStyle1;
            this.gridUsings.Size = new System.Drawing.Size(482, 230);
            this.gridUsings.TabIndex = 8;
            this.gridUsings.RowEnter += new System.Windows.Forms.DataGridViewCellEventHandler(this.gridUsings_RowEnter);
            // 
            // colNamespace
            // 
            this.colNamespace.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.AllCells;
            this.colNamespace.DataPropertyName = "Namespace";
            this.colNamespace.HeaderText = "Namespace";
            this.colNamespace.Name = "colNamespace";
            this.colNamespace.Width = 98;
            // 
            // colAlias
            // 
            this.colAlias.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.AllCells;
            this.colAlias.DataPropertyName = "Alias";
            this.colAlias.HeaderText = "Alias";
            this.colAlias.Name = "colAlias";
            this.colAlias.Width = 59;
            // 
            // colStatic
            // 
            this.colStatic.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.ColumnHeader;
            this.colStatic.DataPropertyName = "Static";
            this.colStatic.HeaderText = "Static";
            this.colStatic.Name = "colStatic";
            this.colStatic.Width = 46;
            // 
            // colDelete
            // 
            this.colDelete.DataPropertyName = "Delete";
            this.colDelete.HeaderText = "Delete";
            this.colDelete.Name = "colDelete";
            this.colDelete.Text = "Delete";
            // 
            // colImported
            // 
            this.colImported.HeaderText = "Imported";
            this.colImported.Name = "colImported";
            this.colImported.Visible = false;
            // 
            // btnAdd
            // 
            this.btnAdd.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.btnAdd.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnAdd.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.btnAdd.Location = new System.Drawing.Point(422, 19);
            this.btnAdd.Name = "btnAdd";
            this.btnAdd.Size = new System.Drawing.Size(79, 25);
            this.btnAdd.TabIndex = 7;
            this.btnAdd.Text = "Add Using";
            this.btnAdd.UseVisualStyleBackColor = true;
            // 
            // chkStatic
            // 
            this.chkStatic.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.chkStatic.AutoSize = true;
            this.chkStatic.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.chkStatic.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.chkStatic.Location = new System.Drawing.Point(359, 21);
            this.chkStatic.Name = "chkStatic";
            this.chkStatic.Size = new System.Drawing.Size(53, 17);
            this.chkStatic.TabIndex = 6;
            this.chkStatic.Text = "Static";
            this.chkStatic.UseVisualStyleBackColor = true;
            // 
            // tbAlias
            // 
            this.tbAlias.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.tbAlias.BackColor = System.Drawing.Color.WhiteSmoke;
            this.tbAlias.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.tbAlias.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.tbAlias.Location = new System.Drawing.Point(242, 18);
            this.tbAlias.Name = "tbAlias";
            this.tbAlias.Size = new System.Drawing.Size(100, 20);
            this.tbAlias.TabIndex = 5;
            // 
            // lblAlias
            // 
            this.lblAlias.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.lblAlias.AutoSize = true;
            this.lblAlias.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblAlias.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.lblAlias.Location = new System.Drawing.Point(199, 21);
            this.lblAlias.Name = "lblAlias";
            this.lblAlias.Size = new System.Drawing.Size(32, 13);
            this.lblAlias.TabIndex = 4;
            this.lblAlias.Text = "Alias:";
            // 
            // tbUsing
            // 
            this.tbUsing.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.tbUsing.BackColor = System.Drawing.Color.WhiteSmoke;
            this.tbUsing.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.tbUsing.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.tbUsing.Location = new System.Drawing.Point(59, 18);
            this.tbUsing.Name = "tbUsing";
            this.tbUsing.Size = new System.Drawing.Size(134, 20);
            this.tbUsing.TabIndex = 3;
            // 
            // lblUsing
            // 
            this.lblUsing.AutoSize = true;
            this.lblUsing.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblUsing.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.lblUsing.Location = new System.Drawing.Point(16, 21);
            this.lblUsing.Name = "lblUsing";
            this.lblUsing.Size = new System.Drawing.Size(37, 13);
            this.lblUsing.TabIndex = 2;
            this.lblUsing.Text = "Using:";
            // 
            // chkShowImported
            // 
            this.chkShowImported.AutoSize = true;
            this.chkShowImported.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.chkShowImported.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.chkShowImported.Location = new System.Drawing.Point(19, 44);
            this.chkShowImported.Name = "chkShowImported";
            this.chkShowImported.Size = new System.Drawing.Size(130, 17);
            this.chkShowImported.TabIndex = 1;
            this.chkShowImported.Text = "Show Imported usings";
            this.chkShowImported.UseVisualStyleBackColor = true;
            this.chkShowImported.Click += new System.EventHandler(this.chkShowImported_Click);
            // 
            // XGlobalUsingsPropertyPagePanel
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.grpManageImplicit);
            this.Controls.Add(this.grpImpicitGlobalUsings);
            this.Margin = new System.Windows.Forms.Padding(5);
            this.Name = "XGlobalUsingsPropertyPagePanel";
            this.grpImpicitGlobalUsings.ResumeLayout(false);
            this.grpImpicitGlobalUsings.PerformLayout();
            this.grpManageImplicit.ResumeLayout(false);
            this.grpManageImplicit.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.gridUsings)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.CheckBox chkImplicitUsings;
        private System.Windows.Forms.GroupBox grpImpicitGlobalUsings;
        private System.Windows.Forms.GroupBox grpManageImplicit;
        private System.Windows.Forms.CheckBox chkShowImported;
        private System.Windows.Forms.TextBox tbAlias;
        private System.Windows.Forms.Label lblAlias;
        private System.Windows.Forms.TextBox tbUsing;
        private System.Windows.Forms.Label lblUsing;
        private System.Windows.Forms.Button btnAdd;
        private System.Windows.Forms.CheckBox chkStatic;
        private System.Windows.Forms.DataGridView gridUsings;
        private System.Windows.Forms.DataGridViewTextBoxColumn colNamespace;
        private System.Windows.Forms.DataGridViewTextBoxColumn colAlias;
        private System.Windows.Forms.DataGridViewCheckBoxColumn colStatic;
        private System.Windows.Forms.DataGridViewButtonColumn colDelete;
        private System.Windows.Forms.DataGridViewCheckBoxColumn colImported;
    }
}
