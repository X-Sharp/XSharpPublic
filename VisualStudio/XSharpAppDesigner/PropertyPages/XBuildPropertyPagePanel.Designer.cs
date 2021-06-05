//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

namespace XSharp.Project
{
    partial class XBuildPropertyPagePanel
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
            this.mainPanel = new System.Windows.Forms.TableLayoutPanel();
            this.panelSigning = new System.Windows.Forms.TableLayoutPanel();
            this.lblSigningLine = new System.Windows.Forms.Label();
            this.lblSigning = new System.Windows.Forms.Label();
            this.outputTableLayoutPanel = new System.Windows.Forms.TableLayoutPanel();
            this.outputLineLabel = new System.Windows.Forms.Label();
            this.outputLabel = new System.Windows.Forms.Label();
            this.treatWarningsAsErrorsTableLayoutPanel = new System.Windows.Forms.TableLayoutPanel();
            this.treatWarningsAsErrorsLineLabel = new System.Windows.Forms.Label();
            this.treatWarningsAsErrorsLabel = new System.Windows.Forms.Label();
            this.lblWarningLevel = new System.Windows.Forms.Label();
            this.panelErrorsAndWarnings = new System.Windows.Forms.TableLayoutPanel();
            this.lblErrorsAndWarnings = new System.Windows.Forms.Label();
            this.lblErrorsAndWarningsLine = new System.Windows.Forms.Label();
            this.panelTop = new System.Windows.Forms.TableLayoutPanel();
            this.lblGeneral = new System.Windows.Forms.Label();
            this.lblGeneralLine = new System.Windows.Forms.Label();
            this.txtDefineConstants = new System.Windows.Forms.TextBox();
            this.lblDefineConstants = new System.Windows.Forms.Label();
            this.lblDisabledWarnings = new System.Windows.Forms.Label();
            this.cboWarningLevel = new System.Windows.Forms.ComboBox();
            this.txtDisabledWarnings = new System.Windows.Forms.TextBox();
            this.txtOutputPath = new System.Windows.Forms.TextBox();
            this.btnOutputPathBrowse = new System.Windows.Forms.Button();
            this.lblIntermediateOutputPath = new System.Windows.Forms.Label();
            this.txtXMLDocumentationFile = new System.Windows.Forms.TextBox();
            this.txtIntermediateOutputPath = new System.Windows.Forms.TextBox();
            this.btnIntermediateOutputPath = new System.Windows.Forms.Button();
            this.chkPPO = new System.Windows.Forms.CheckBox();
            this.chkXMLDocumentationFile = new System.Windows.Forms.CheckBox();
            this.lblPlatformTarget = new System.Windows.Forms.Label();
            this.chkOptimize = new System.Windows.Forms.CheckBox();
            this.chkUseSharedCompilation = new System.Windows.Forms.CheckBox();
            this.lblOutputPath = new System.Windows.Forms.Label();
            this.lblCommandLineOption = new System.Windows.Forms.Label();
            this.txtCommandLineOption = new System.Windows.Forms.TextBox();
            this.chkSignAssembly = new System.Windows.Forms.CheckBox();
            this.lblAssemblyOriginatorKeyFile = new System.Windows.Forms.Label();
            this.txtAssemblyOriginatorKeyFile = new System.Windows.Forms.TextBox();
            this.chkDelaySign = new System.Windows.Forms.CheckBox();
            this.chkRegisterForComInterop = new System.Windows.Forms.CheckBox();
            this.rbWarningNone = new System.Windows.Forms.RadioButton();
            this.rbWarningAll = new System.Windows.Forms.RadioButton();
            this.rbWarningSpecific = new System.Windows.Forms.RadioButton();
            this.chkSuppressRCWarnings = new System.Windows.Forms.CheckBox();
            this.txtSpecificWarnings = new System.Windows.Forms.TextBox();
            this.btnKeyFile = new System.Windows.Forms.Button();
            this.comboPlatformTarget = new System.Windows.Forms.ComboBox();
            this.chkPrefer32Bit = new System.Windows.Forms.CheckBox();
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.mainPanel.SuspendLayout();
            this.panelSigning.SuspendLayout();
            this.outputTableLayoutPanel.SuspendLayout();
            this.treatWarningsAsErrorsTableLayoutPanel.SuspendLayout();
            this.panelErrorsAndWarnings.SuspendLayout();
            this.panelTop.SuspendLayout();
            this.SuspendLayout();
            // 
            // mainPanel
            // 
            this.mainPanel.ColumnCount = 3;
            this.mainPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.mainPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.mainPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.mainPanel.Controls.Add(this.panelSigning, 0, 23);
            this.mainPanel.Controls.Add(this.outputTableLayoutPanel, 0, 15);
            this.mainPanel.Controls.Add(this.treatWarningsAsErrorsTableLayoutPanel, 0, 10);
            this.mainPanel.Controls.Add(this.lblWarningLevel, 0, 8);
            this.mainPanel.Controls.Add(this.panelErrorsAndWarnings, 0, 7);
            this.mainPanel.Controls.Add(this.panelTop, 0, 0);
            this.mainPanel.Controls.Add(this.txtDefineConstants, 1, 1);
            this.mainPanel.Controls.Add(this.lblDefineConstants, 0, 1);
            this.mainPanel.Controls.Add(this.lblDisabledWarnings, 0, 9);
            this.mainPanel.Controls.Add(this.cboWarningLevel, 1, 8);
            this.mainPanel.Controls.Add(this.txtDisabledWarnings, 1, 9);
            this.mainPanel.Controls.Add(this.txtOutputPath, 1, 16);
            this.mainPanel.Controls.Add(this.btnOutputPathBrowse, 2, 16);
            this.mainPanel.Controls.Add(this.lblIntermediateOutputPath, 0, 17);
            this.mainPanel.Controls.Add(this.txtXMLDocumentationFile, 1, 20);
            this.mainPanel.Controls.Add(this.txtIntermediateOutputPath, 1, 17);
            this.mainPanel.Controls.Add(this.btnIntermediateOutputPath, 2, 17);
            this.mainPanel.Controls.Add(this.chkPPO, 0, 2);
            this.mainPanel.Controls.Add(this.chkXMLDocumentationFile, 0, 20);
            this.mainPanel.Controls.Add(this.lblPlatformTarget, 0, 3);
            this.mainPanel.Controls.Add(this.chkOptimize, 0, 4);
            this.mainPanel.Controls.Add(this.chkUseSharedCompilation, 0, 5);
            this.mainPanel.Controls.Add(this.lblOutputPath, 0, 16);
            this.mainPanel.Controls.Add(this.lblCommandLineOption, 0, 6);
            this.mainPanel.Controls.Add(this.txtCommandLineOption, 1, 6);
            this.mainPanel.Controls.Add(this.chkSignAssembly, 0, 25);
            this.mainPanel.Controls.Add(this.lblAssemblyOriginatorKeyFile, 0, 24);
            this.mainPanel.Controls.Add(this.txtAssemblyOriginatorKeyFile, 1, 24);
            this.mainPanel.Controls.Add(this.chkDelaySign, 1, 25);
            this.mainPanel.Controls.Add(this.chkRegisterForComInterop, 0, 21);
            this.mainPanel.Controls.Add(this.rbWarningNone, 0, 11);
            this.mainPanel.Controls.Add(this.rbWarningAll, 0, 12);
            this.mainPanel.Controls.Add(this.rbWarningSpecific, 0, 13);
            this.mainPanel.Controls.Add(this.chkSuppressRCWarnings, 0, 14);
            this.mainPanel.Controls.Add(this.txtSpecificWarnings, 1, 13);
            this.mainPanel.Controls.Add(this.btnKeyFile, 2, 24);
            this.mainPanel.Controls.Add(this.comboPlatformTarget, 1, 3);
            this.mainPanel.Controls.Add(this.chkPrefer32Bit, 2, 3);
            this.mainPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.mainPanel.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.mainPanel.Location = new System.Drawing.Point(0, 0);
            this.mainPanel.Name = "mainPanel";
            this.mainPanel.RowCount = 27;
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.mainPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 5F));
            this.mainPanel.Size = new System.Drawing.Size(633, 618);
            this.mainPanel.TabIndex = 0;
            // 
            // panelSigning
            // 
            this.panelSigning.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.panelSigning.AutoSize = true;
            this.panelSigning.ColumnCount = 2;
            this.mainPanel.SetColumnSpan(this.panelSigning, 3);
            this.panelSigning.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.panelSigning.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.panelSigning.Controls.Add(this.lblSigningLine, 1, 0);
            this.panelSigning.Controls.Add(this.lblSigning, 0, 0);
            this.panelSigning.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.panelSigning.Location = new System.Drawing.Point(0, 467);
            this.panelSigning.Margin = new System.Windows.Forms.Padding(0);
            this.panelSigning.Name = "panelSigning";
            this.panelSigning.RowCount = 1;
            this.panelSigning.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.panelSigning.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.panelSigning.Size = new System.Drawing.Size(636, 13);
            this.panelSigning.TabIndex = 25;
            // 
            // lblSigningLine
            // 
            this.lblSigningLine.AccessibleRole = System.Windows.Forms.AccessibleRole.Separator;
            this.lblSigningLine.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.lblSigningLine.BackColor = System.Drawing.SystemColors.ControlDark;
            this.lblSigningLine.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.lblSigningLine.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.lblSigningLine.Location = new System.Drawing.Point(51, 6);
            this.lblSigningLine.Margin = new System.Windows.Forms.Padding(3, 0, 0, 0);
            this.lblSigningLine.Name = "lblSigningLine";
            this.lblSigningLine.Size = new System.Drawing.Size(585, 1);
            this.lblSigningLine.TabIndex = 1;
            // 
            // lblSigning
            // 
            this.lblSigning.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.lblSigning.AutoSize = true;
            this.lblSigning.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.lblSigning.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.lblSigning.Location = new System.Drawing.Point(3, 0);
            this.lblSigning.Name = "lblSigning";
            this.lblSigning.Size = new System.Drawing.Size(42, 13);
            this.lblSigning.TabIndex = 0;
            this.lblSigning.Text = "Signing";
            // 
            // outputTableLayoutPanel
            // 
            this.outputTableLayoutPanel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.outputTableLayoutPanel.AutoSize = true;
            this.outputTableLayoutPanel.ColumnCount = 2;
            this.mainPanel.SetColumnSpan(this.outputTableLayoutPanel, 3);
            this.outputTableLayoutPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.outputTableLayoutPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.outputTableLayoutPanel.Controls.Add(this.outputLineLabel, 1, 0);
            this.outputTableLayoutPanel.Controls.Add(this.outputLabel, 0, 0);
            this.outputTableLayoutPanel.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.outputTableLayoutPanel.Location = new System.Drawing.Point(0, 344);
            this.outputTableLayoutPanel.Margin = new System.Windows.Forms.Padding(0);
            this.outputTableLayoutPanel.Name = "outputTableLayoutPanel";
            this.outputTableLayoutPanel.RowCount = 1;
            this.outputTableLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.outputTableLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.outputTableLayoutPanel.Size = new System.Drawing.Size(636, 13);
            this.outputTableLayoutPanel.TabIndex = 24;
            // 
            // outputLineLabel
            // 
            this.outputLineLabel.AccessibleRole = System.Windows.Forms.AccessibleRole.Separator;
            this.outputLineLabel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.outputLineLabel.BackColor = System.Drawing.SystemColors.ControlDark;
            this.outputLineLabel.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.outputLineLabel.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.outputLineLabel.Location = new System.Drawing.Point(48, 6);
            this.outputLineLabel.Margin = new System.Windows.Forms.Padding(3, 0, 0, 0);
            this.outputLineLabel.Name = "outputLineLabel";
            this.outputLineLabel.Size = new System.Drawing.Size(588, 1);
            this.outputLineLabel.TabIndex = 1;
            // 
            // outputLabel
            // 
            this.outputLabel.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.outputLabel.AutoSize = true;
            this.outputLabel.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.outputLabel.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.outputLabel.Location = new System.Drawing.Point(3, 0);
            this.outputLabel.Name = "outputLabel";
            this.outputLabel.Size = new System.Drawing.Size(39, 13);
            this.outputLabel.TabIndex = 0;
            this.outputLabel.Text = "Output";
            // 
            // treatWarningsAsErrorsTableLayoutPanel
            // 
            this.treatWarningsAsErrorsTableLayoutPanel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.treatWarningsAsErrorsTableLayoutPanel.AutoSize = true;
            this.treatWarningsAsErrorsTableLayoutPanel.ColumnCount = 2;
            this.mainPanel.SetColumnSpan(this.treatWarningsAsErrorsTableLayoutPanel, 3);
            this.treatWarningsAsErrorsTableLayoutPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.treatWarningsAsErrorsTableLayoutPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.treatWarningsAsErrorsTableLayoutPanel.Controls.Add(this.treatWarningsAsErrorsLineLabel, 1, 0);
            this.treatWarningsAsErrorsTableLayoutPanel.Controls.Add(this.treatWarningsAsErrorsLabel, 0, 0);
            this.treatWarningsAsErrorsTableLayoutPanel.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.treatWarningsAsErrorsTableLayoutPanel.Location = new System.Drawing.Point(0, 233);
            this.treatWarningsAsErrorsTableLayoutPanel.Margin = new System.Windows.Forms.Padding(0);
            this.treatWarningsAsErrorsTableLayoutPanel.Name = "treatWarningsAsErrorsTableLayoutPanel";
            this.treatWarningsAsErrorsTableLayoutPanel.RowCount = 1;
            this.treatWarningsAsErrorsTableLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.treatWarningsAsErrorsTableLayoutPanel.Size = new System.Drawing.Size(636, 13);
            this.treatWarningsAsErrorsTableLayoutPanel.TabIndex = 18;
            // 
            // treatWarningsAsErrorsLineLabel
            // 
            this.treatWarningsAsErrorsLineLabel.AccessibleRole = System.Windows.Forms.AccessibleRole.Separator;
            this.treatWarningsAsErrorsLineLabel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.treatWarningsAsErrorsLineLabel.BackColor = System.Drawing.SystemColors.ControlDark;
            this.treatWarningsAsErrorsLineLabel.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.treatWarningsAsErrorsLineLabel.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.treatWarningsAsErrorsLineLabel.Location = new System.Drawing.Point(126, 6);
            this.treatWarningsAsErrorsLineLabel.Margin = new System.Windows.Forms.Padding(3, 0, 0, 0);
            this.treatWarningsAsErrorsLineLabel.Name = "treatWarningsAsErrorsLineLabel";
            this.treatWarningsAsErrorsLineLabel.Size = new System.Drawing.Size(510, 1);
            this.treatWarningsAsErrorsLineLabel.TabIndex = 1;
            // 
            // treatWarningsAsErrorsLabel
            // 
            this.treatWarningsAsErrorsLabel.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.treatWarningsAsErrorsLabel.AutoSize = true;
            this.treatWarningsAsErrorsLabel.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.treatWarningsAsErrorsLabel.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.treatWarningsAsErrorsLabel.Location = new System.Drawing.Point(0, 0);
            this.treatWarningsAsErrorsLabel.Margin = new System.Windows.Forms.Padding(0, 0, 3, 0);
            this.treatWarningsAsErrorsLabel.Name = "treatWarningsAsErrorsLabel";
            this.treatWarningsAsErrorsLabel.Size = new System.Drawing.Size(120, 13);
            this.treatWarningsAsErrorsLabel.TabIndex = 0;
            this.treatWarningsAsErrorsLabel.Text = "Treat warnings as errors";
            // 
            // lblWarningLevel
            // 
            this.lblWarningLevel.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.lblWarningLevel.AutoSize = true;
            this.lblWarningLevel.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.lblWarningLevel.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.lblWarningLevel.Location = new System.Drawing.Point(20, 184);
            this.lblWarningLevel.Margin = new System.Windows.Forms.Padding(20, 3, 3, 3);
            this.lblWarningLevel.Name = "lblWarningLevel";
            this.lblWarningLevel.Size = new System.Drawing.Size(75, 13);
            this.lblWarningLevel.TabIndex = 13;
            this.lblWarningLevel.Text = "W&arning level:";
            // 
            // panelErrorsAndWarnings
            // 
            this.panelErrorsAndWarnings.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.panelErrorsAndWarnings.ColumnCount = 2;
            this.mainPanel.SetColumnSpan(this.panelErrorsAndWarnings, 3);
            this.panelErrorsAndWarnings.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.panelErrorsAndWarnings.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.panelErrorsAndWarnings.Controls.Add(this.lblErrorsAndWarnings, 0, 0);
            this.panelErrorsAndWarnings.Controls.Add(this.lblErrorsAndWarningsLine, 1, 0);
            this.panelErrorsAndWarnings.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.panelErrorsAndWarnings.Location = new System.Drawing.Point(0, 164);
            this.panelErrorsAndWarnings.Margin = new System.Windows.Forms.Padding(0);
            this.panelErrorsAndWarnings.Name = "panelErrorsAndWarnings";
            this.panelErrorsAndWarnings.RowCount = 1;
            this.panelErrorsAndWarnings.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.panelErrorsAndWarnings.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.panelErrorsAndWarnings.Size = new System.Drawing.Size(636, 13);
            this.panelErrorsAndWarnings.TabIndex = 2;
            // 
            // lblErrorsAndWarnings
            // 
            this.lblErrorsAndWarnings.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.lblErrorsAndWarnings.AutoSize = true;
            this.lblErrorsAndWarnings.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.lblErrorsAndWarnings.Location = new System.Drawing.Point(0, 0);
            this.lblErrorsAndWarnings.Margin = new System.Windows.Forms.Padding(0, 0, 3, 0);
            this.lblErrorsAndWarnings.Name = "lblErrorsAndWarnings";
            this.lblErrorsAndWarnings.Size = new System.Drawing.Size(103, 13);
            this.lblErrorsAndWarnings.TabIndex = 0;
            this.lblErrorsAndWarnings.Text = "Errors and Warnings";
            // 
            // lblErrorsAndWarningsLine
            // 
            this.lblErrorsAndWarningsLine.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.lblErrorsAndWarningsLine.BackColor = System.Drawing.SystemColors.ControlDark;
            this.lblErrorsAndWarningsLine.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.lblErrorsAndWarningsLine.Location = new System.Drawing.Point(109, 6);
            this.lblErrorsAndWarningsLine.Name = "lblErrorsAndWarningsLine";
            this.lblErrorsAndWarningsLine.Size = new System.Drawing.Size(524, 1);
            this.lblErrorsAndWarningsLine.TabIndex = 1;
            // 
            // panelTop
            // 
            this.panelTop.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.panelTop.ColumnCount = 2;
            this.mainPanel.SetColumnSpan(this.panelTop, 3);
            this.panelTop.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.panelTop.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.panelTop.Controls.Add(this.lblGeneral, 0, 0);
            this.panelTop.Controls.Add(this.lblGeneralLine, 1, 0);
            this.panelTop.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.panelTop.Location = new System.Drawing.Point(0, 0);
            this.panelTop.Margin = new System.Windows.Forms.Padding(0);
            this.panelTop.Name = "panelTop";
            this.panelTop.RowCount = 1;
            this.panelTop.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.panelTop.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.panelTop.Size = new System.Drawing.Size(636, 13);
            this.panelTop.TabIndex = 0;
            // 
            // lblGeneral
            // 
            this.lblGeneral.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.lblGeneral.AutoSize = true;
            this.lblGeneral.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.lblGeneral.Location = new System.Drawing.Point(0, 0);
            this.lblGeneral.Margin = new System.Windows.Forms.Padding(0, 0, 3, 0);
            this.lblGeneral.Name = "lblGeneral";
            this.lblGeneral.Size = new System.Drawing.Size(44, 13);
            this.lblGeneral.TabIndex = 0;
            this.lblGeneral.Text = "General";
            // 
            // lblGeneralLine
            // 
            this.lblGeneralLine.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.lblGeneralLine.BackColor = System.Drawing.SystemColors.ControlDark;
            this.lblGeneralLine.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.lblGeneralLine.Location = new System.Drawing.Point(50, 6);
            this.lblGeneralLine.Name = "lblGeneralLine";
            this.lblGeneralLine.Size = new System.Drawing.Size(583, 1);
            this.lblGeneralLine.TabIndex = 1;
            // 
            // txtDefineConstants
            // 
            this.txtDefineConstants.BackColor = System.Drawing.Color.WhiteSmoke;
            this.txtDefineConstants.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.txtDefineConstants.Location = new System.Drawing.Point(198, 19);
            this.txtDefineConstants.Margin = new System.Windows.Forms.Padding(3, 6, 3, 3);
            this.txtDefineConstants.Name = "txtDefineConstants";
            this.txtDefineConstants.Size = new System.Drawing.Size(300, 20);
            this.txtDefineConstants.TabIndex = 3;
            // 
            // lblDefineConstants
            // 
            this.lblDefineConstants.AutoSize = true;
            this.lblDefineConstants.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.lblDefineConstants.Location = new System.Drawing.Point(23, 19);
            this.lblDefineConstants.Margin = new System.Windows.Forms.Padding(23, 6, 3, 3);
            this.lblDefineConstants.Name = "lblDefineConstants";
            this.lblDefineConstants.Size = new System.Drawing.Size(140, 13);
            this.lblDefineConstants.TabIndex = 4;
            this.lblDefineConstants.Text = "Defines for the preprocessor";
            // 
            // lblDisabledWarnings
            // 
            this.lblDisabledWarnings.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.lblDisabledWarnings.AutoSize = true;
            this.lblDisabledWarnings.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.lblDisabledWarnings.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.lblDisabledWarnings.Location = new System.Drawing.Point(20, 210);
            this.lblDisabledWarnings.Margin = new System.Windows.Forms.Padding(20, 3, 3, 6);
            this.lblDisabledWarnings.Name = "lblDisabledWarnings";
            this.lblDisabledWarnings.Size = new System.Drawing.Size(99, 13);
            this.lblDisabledWarnings.TabIndex = 15;
            this.lblDisabledWarnings.Text = "&Suppress warnings:";
            // 
            // cboWarningLevel
            // 
            this.cboWarningLevel.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.cboWarningLevel.BackColor = System.Drawing.Color.WhiteSmoke;
            this.cboWarningLevel.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboWarningLevel.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.cboWarningLevel.FormattingEnabled = true;
            this.cboWarningLevel.Items.AddRange(new object[] {
            "0",
            "1",
            "2",
            "3",
            "4",
            "5"});
            this.cboWarningLevel.Location = new System.Drawing.Point(198, 180);
            this.cboWarningLevel.Name = "cboWarningLevel";
            this.cboWarningLevel.Size = new System.Drawing.Size(125, 21);
            this.cboWarningLevel.TabIndex = 16;
            // 
            // txtDisabledWarnings
            // 
            this.txtDisabledWarnings.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.txtDisabledWarnings.BackColor = System.Drawing.Color.WhiteSmoke;
            this.txtDisabledWarnings.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.txtDisabledWarnings.Location = new System.Drawing.Point(198, 207);
            this.txtDisabledWarnings.Margin = new System.Windows.Forms.Padding(3, 3, 3, 6);
            this.txtDisabledWarnings.Name = "txtDisabledWarnings";
            this.txtDisabledWarnings.Size = new System.Drawing.Size(300, 20);
            this.txtDisabledWarnings.TabIndex = 17;
            // 
            // txtOutputPath
            // 
            this.txtOutputPath.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.txtOutputPath.BackColor = System.Drawing.Color.WhiteSmoke;
            this.txtOutputPath.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.txtOutputPath.Location = new System.Drawing.Point(198, 364);
            this.txtOutputPath.Margin = new System.Windows.Forms.Padding(3, 6, 3, 3);
            this.txtOutputPath.Name = "txtOutputPath";
            this.txtOutputPath.ReadOnly = true;
            this.txtOutputPath.Size = new System.Drawing.Size(300, 20);
            this.txtOutputPath.TabIndex = 27;
            // 
            // btnOutputPathBrowse
            // 
            this.btnOutputPathBrowse.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.btnOutputPathBrowse.AutoSize = true;
            this.btnOutputPathBrowse.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.btnOutputPathBrowse.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.btnOutputPathBrowse.Location = new System.Drawing.Point(504, 363);
            this.btnOutputPathBrowse.Margin = new System.Windows.Forms.Padding(3, 6, 0, 3);
            this.btnOutputPathBrowse.Name = "btnOutputPathBrowse";
            this.btnOutputPathBrowse.Size = new System.Drawing.Size(26, 23);
            this.btnOutputPathBrowse.TabIndex = 29;
            this.btnOutputPathBrowse.Text = "...";
            this.btnOutputPathBrowse.Click += new System.EventHandler(this.btnOutputPathBrowse_Click);
            // 
            // lblIntermediateOutputPath
            // 
            this.lblIntermediateOutputPath.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.lblIntermediateOutputPath.AutoSize = true;
            this.lblIntermediateOutputPath.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.lblIntermediateOutputPath.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.lblIntermediateOutputPath.Location = new System.Drawing.Point(20, 397);
            this.lblIntermediateOutputPath.Margin = new System.Windows.Forms.Padding(20, 3, 3, 3);
            this.lblIntermediateOutputPath.Name = "lblIntermediateOutputPath";
            this.lblIntermediateOutputPath.Size = new System.Drawing.Size(127, 13);
            this.lblIntermediateOutputPath.TabIndex = 31;
            this.lblIntermediateOutputPath.Text = "&Intermediate Output path:";
            // 
            // txtXMLDocumentationFile
            // 
            this.txtXMLDocumentationFile.AccessibleName = "XML Document File Path";
            this.txtXMLDocumentationFile.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.txtXMLDocumentationFile.BackColor = System.Drawing.Color.WhiteSmoke;
            this.txtXMLDocumentationFile.Enabled = false;
            this.txtXMLDocumentationFile.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.txtXMLDocumentationFile.Location = new System.Drawing.Point(198, 421);
            this.txtXMLDocumentationFile.Name = "txtXMLDocumentationFile";
            this.txtXMLDocumentationFile.Size = new System.Drawing.Size(300, 20);
            this.txtXMLDocumentationFile.TabIndex = 28;
            // 
            // txtIntermediateOutputPath
            // 
            this.txtIntermediateOutputPath.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.txtIntermediateOutputPath.BackColor = System.Drawing.Color.WhiteSmoke;
            this.txtIntermediateOutputPath.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.txtIntermediateOutputPath.Location = new System.Drawing.Point(198, 393);
            this.txtIntermediateOutputPath.Name = "txtIntermediateOutputPath";
            this.txtIntermediateOutputPath.ReadOnly = true;
            this.txtIntermediateOutputPath.Size = new System.Drawing.Size(300, 20);
            this.txtIntermediateOutputPath.TabIndex = 32;
            // 
            // btnIntermediateOutputPath
            // 
            this.btnIntermediateOutputPath.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.btnIntermediateOutputPath.AutoSize = true;
            this.btnIntermediateOutputPath.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.btnIntermediateOutputPath.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.btnIntermediateOutputPath.Location = new System.Drawing.Point(504, 392);
            this.btnIntermediateOutputPath.Margin = new System.Windows.Forms.Padding(3, 3, 0, 3);
            this.btnIntermediateOutputPath.Name = "btnIntermediateOutputPath";
            this.btnIntermediateOutputPath.Size = new System.Drawing.Size(26, 23);
            this.btnIntermediateOutputPath.TabIndex = 33;
            this.btnIntermediateOutputPath.Text = "...";
            this.btnIntermediateOutputPath.Click += new System.EventHandler(this.btnIntermediateOutputPath_Click);
            // 
            // chkPPO
            // 
            this.chkPPO.AutoSize = true;
            this.chkPPO.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.chkPPO.Location = new System.Drawing.Point(23, 45);
            this.chkPPO.Margin = new System.Windows.Forms.Padding(23, 3, 3, 3);
            this.chkPPO.Name = "chkPPO";
            this.chkPPO.Size = new System.Drawing.Size(169, 17);
            this.chkPPO.TabIndex = 5;
            this.chkPPO.Text = "Generate preprocessor Output";
            this.chkPPO.UseVisualStyleBackColor = true;
            // 
            // chkXMLDocumentationFile
            // 
            this.chkXMLDocumentationFile.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.chkXMLDocumentationFile.AutoSize = true;
            this.chkXMLDocumentationFile.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.chkXMLDocumentationFile.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.chkXMLDocumentationFile.Location = new System.Drawing.Point(23, 422);
            this.chkXMLDocumentationFile.Margin = new System.Windows.Forms.Padding(23, 3, 3, 3);
            this.chkXMLDocumentationFile.Name = "chkXMLDocumentationFile";
            this.chkXMLDocumentationFile.Size = new System.Drawing.Size(140, 17);
            this.chkXMLDocumentationFile.TabIndex = 26;
            this.chkXMLDocumentationFile.Text = "&XML documentation file:";
            this.chkXMLDocumentationFile.CheckedChanged += new System.EventHandler(this.chkXMLDocumentationFile_CheckedChanged);
            // 
            // lblPlatformTarget
            // 
            this.lblPlatformTarget.AutoSize = true;
            this.lblPlatformTarget.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.lblPlatformTarget.Location = new System.Drawing.Point(20, 71);
            this.lblPlatformTarget.Margin = new System.Windows.Forms.Padding(20, 6, 3, 3);
            this.lblPlatformTarget.Name = "lblPlatformTarget";
            this.lblPlatformTarget.Size = new System.Drawing.Size(79, 13);
            this.lblPlatformTarget.TabIndex = 7;
            this.lblPlatformTarget.Text = "Platform Target";
            // 
            // chkOptimize
            // 
            this.chkOptimize.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.chkOptimize.AutoSize = true;
            this.chkOptimize.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.chkOptimize.Location = new System.Drawing.Point(23, 95);
            this.chkOptimize.Margin = new System.Windows.Forms.Padding(23, 3, 3, 3);
            this.chkOptimize.Name = "chkOptimize";
            this.chkOptimize.Size = new System.Drawing.Size(66, 17);
            this.chkOptimize.TabIndex = 9;
            this.chkOptimize.Text = "Optimize";
            this.chkOptimize.UseVisualStyleBackColor = true;
            // 
            // chkUseSharedCompilation
            // 
            this.chkUseSharedCompilation.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.chkUseSharedCompilation.AutoSize = true;
            this.chkUseSharedCompilation.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.chkUseSharedCompilation.Location = new System.Drawing.Point(23, 118);
            this.chkUseSharedCompilation.Margin = new System.Windows.Forms.Padding(23, 3, 3, 3);
            this.chkUseSharedCompilation.Name = "chkUseSharedCompilation";
            this.chkUseSharedCompilation.Size = new System.Drawing.Size(125, 17);
            this.chkUseSharedCompilation.TabIndex = 34;
            this.chkUseSharedCompilation.Text = "Use Shared Compiler";
            this.chkUseSharedCompilation.UseVisualStyleBackColor = true;
            // 
            // lblOutputPath
            // 
            this.lblOutputPath.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.lblOutputPath.AutoSize = true;
            this.lblOutputPath.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.lblOutputPath.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.lblOutputPath.Location = new System.Drawing.Point(20, 368);
            this.lblOutputPath.Margin = new System.Windows.Forms.Padding(20, 6, 3, 3);
            this.lblOutputPath.Name = "lblOutputPath";
            this.lblOutputPath.Size = new System.Drawing.Size(66, 13);
            this.lblOutputPath.TabIndex = 25;
            this.lblOutputPath.Text = "&Output path:";
            // 
            // lblCommandLineOption
            // 
            this.lblCommandLineOption.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.lblCommandLineOption.AutoSize = true;
            this.lblCommandLineOption.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.lblCommandLineOption.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.lblCommandLineOption.Location = new System.Drawing.Point(20, 144);
            this.lblCommandLineOption.Margin = new System.Windows.Forms.Padding(20, 3, 3, 3);
            this.lblCommandLineOption.Name = "lblCommandLineOption";
            this.lblCommandLineOption.Size = new System.Drawing.Size(142, 13);
            this.lblCommandLineOption.TabIndex = 35;
            this.lblCommandLineOption.Text = "&Extra command Line Options";
            // 
            // txtCommandLineOption
            // 
            this.txtCommandLineOption.AccessibleName = "";
            this.txtCommandLineOption.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.txtCommandLineOption.BackColor = System.Drawing.Color.WhiteSmoke;
            this.txtCommandLineOption.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.txtCommandLineOption.Location = new System.Drawing.Point(198, 141);
            this.txtCommandLineOption.Name = "txtCommandLineOption";
            this.txtCommandLineOption.Size = new System.Drawing.Size(300, 20);
            this.txtCommandLineOption.TabIndex = 36;
            // 
            // chkSignAssembly
            // 
            this.chkSignAssembly.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.chkSignAssembly.AutoSize = true;
            this.chkSignAssembly.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.chkSignAssembly.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.chkSignAssembly.Location = new System.Drawing.Point(23, 512);
            this.chkSignAssembly.Margin = new System.Windows.Forms.Padding(23, 3, 3, 3);
            this.chkSignAssembly.Name = "chkSignAssembly";
            this.chkSignAssembly.Size = new System.Drawing.Size(145, 17);
            this.chkSignAssembly.TabIndex = 37;
            this.chkSignAssembly.Text = "Sign the output Assembly";
            // 
            // lblAssemblyOriginatorKeyFile
            // 
            this.lblAssemblyOriginatorKeyFile.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.lblAssemblyOriginatorKeyFile.AutoSize = true;
            this.lblAssemblyOriginatorKeyFile.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.lblAssemblyOriginatorKeyFile.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.lblAssemblyOriginatorKeyFile.Location = new System.Drawing.Point(20, 488);
            this.lblAssemblyOriginatorKeyFile.Margin = new System.Windows.Forms.Padding(20, 3, 3, 3);
            this.lblAssemblyOriginatorKeyFile.Name = "lblAssemblyOriginatorKeyFile";
            this.lblAssemblyOriginatorKeyFile.Size = new System.Drawing.Size(104, 13);
            this.lblAssemblyOriginatorKeyFile.TabIndex = 38;
            this.lblAssemblyOriginatorKeyFile.Text = "Code Signing Keyfile";
            // 
            // txtAssemblyOriginatorKeyFile
            // 
            this.txtAssemblyOriginatorKeyFile.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.txtAssemblyOriginatorKeyFile.BackColor = System.Drawing.Color.WhiteSmoke;
            this.txtAssemblyOriginatorKeyFile.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.txtAssemblyOriginatorKeyFile.Location = new System.Drawing.Point(198, 484);
            this.txtAssemblyOriginatorKeyFile.Name = "txtAssemblyOriginatorKeyFile";
            this.txtAssemblyOriginatorKeyFile.Size = new System.Drawing.Size(300, 20);
            this.txtAssemblyOriginatorKeyFile.TabIndex = 39;
            // 
            // chkDelaySign
            // 
            this.chkDelaySign.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.chkDelaySign.AutoSize = true;
            this.chkDelaySign.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.chkDelaySign.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.chkDelaySign.Location = new System.Drawing.Point(198, 512);
            this.chkDelaySign.Name = "chkDelaySign";
            this.chkDelaySign.Size = new System.Drawing.Size(113, 17);
            this.chkDelaySign.TabIndex = 40;
            this.chkDelaySign.Text = "Delayed Sign Only";
            // 
            // chkRegisterForComInterop
            // 
            this.chkRegisterForComInterop.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.chkRegisterForComInterop.AutoSize = true;
            this.chkRegisterForComInterop.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.chkRegisterForComInterop.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.chkRegisterForComInterop.Location = new System.Drawing.Point(23, 447);
            this.chkRegisterForComInterop.Margin = new System.Windows.Forms.Padding(23, 3, 3, 3);
            this.chkRegisterForComInterop.Name = "chkRegisterForComInterop";
            this.chkRegisterForComInterop.Size = new System.Drawing.Size(142, 17);
            this.chkRegisterForComInterop.TabIndex = 30;
            this.chkRegisterForComInterop.Text = "Register for &COM interop";
            // 
            // rbWarningNone
            // 
            this.rbWarningNone.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.rbWarningNone.AutoSize = true;
            this.rbWarningNone.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.rbWarningNone.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.rbWarningNone.Location = new System.Drawing.Point(23, 252);
            this.rbWarningNone.Margin = new System.Windows.Forms.Padding(23, 6, 3, 3);
            this.rbWarningNone.Name = "rbWarningNone";
            this.rbWarningNone.Size = new System.Drawing.Size(51, 17);
            this.rbWarningNone.TabIndex = 19;
            this.rbWarningNone.Text = "&None";
            this.rbWarningNone.CheckedChanged += new System.EventHandler(this.rbWarningNone_CheckedChanged);
            // 
            // rbWarningAll
            // 
            this.rbWarningAll.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.rbWarningAll.AutoSize = true;
            this.rbWarningAll.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.rbWarningAll.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.rbWarningAll.Location = new System.Drawing.Point(23, 275);
            this.rbWarningAll.Margin = new System.Windows.Forms.Padding(23, 3, 3, 3);
            this.rbWarningAll.Name = "rbWarningAll";
            this.rbWarningAll.Size = new System.Drawing.Size(36, 17);
            this.rbWarningAll.TabIndex = 20;
            this.rbWarningAll.Text = "A&ll";
            this.rbWarningAll.CheckedChanged += new System.EventHandler(this.rbWarningAll_CheckedChanged);
            // 
            // rbWarningSpecific
            // 
            this.rbWarningSpecific.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.rbWarningSpecific.AutoSize = true;
            this.rbWarningSpecific.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.rbWarningSpecific.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.rbWarningSpecific.Location = new System.Drawing.Point(23, 298);
            this.rbWarningSpecific.Margin = new System.Windows.Forms.Padding(23, 3, 3, 6);
            this.rbWarningSpecific.Name = "rbWarningSpecific";
            this.rbWarningSpecific.Size = new System.Drawing.Size(111, 17);
            this.rbWarningSpecific.TabIndex = 21;
            this.rbWarningSpecific.Text = "Specif&ic warnings:";
            this.rbWarningSpecific.CheckedChanged += new System.EventHandler(this.rbWarningSpecific_CheckedChanged);
            // 
            // chkSuppressRCWarnings
            // 
            this.chkSuppressRCWarnings.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.chkSuppressRCWarnings.AutoSize = true;
            this.chkSuppressRCWarnings.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.chkSuppressRCWarnings.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.chkSuppressRCWarnings.Location = new System.Drawing.Point(23, 324);
            this.chkSuppressRCWarnings.Margin = new System.Windows.Forms.Padding(23, 3, 3, 3);
            this.chkSuppressRCWarnings.Name = "chkSuppressRCWarnings";
            this.chkSuppressRCWarnings.Size = new System.Drawing.Size(136, 17);
            this.chkSuppressRCWarnings.TabIndex = 43;
            this.chkSuppressRCWarnings.Text = "Suppress RC Warnings";
            // 
            // txtSpecificWarnings
            // 
            this.txtSpecificWarnings.AccessibleName = "Specific Warnings To Treat As Errors";
            this.txtSpecificWarnings.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.txtSpecificWarnings.BackColor = System.Drawing.Color.WhiteSmoke;
            this.txtSpecificWarnings.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.txtSpecificWarnings.Location = new System.Drawing.Point(198, 298);
            this.txtSpecificWarnings.Name = "txtSpecificWarnings";
            this.txtSpecificWarnings.Size = new System.Drawing.Size(300, 20);
            this.txtSpecificWarnings.TabIndex = 22;
            // 
            // btnKeyFile
            // 
            this.btnKeyFile.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.btnKeyFile.AutoSize = true;
            this.btnKeyFile.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.btnKeyFile.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.btnKeyFile.Location = new System.Drawing.Point(504, 483);
            this.btnKeyFile.Margin = new System.Windows.Forms.Padding(3, 3, 0, 3);
            this.btnKeyFile.Name = "btnKeyFile";
            this.btnKeyFile.Size = new System.Drawing.Size(26, 23);
            this.btnKeyFile.TabIndex = 44;
            this.btnKeyFile.Text = "...";
            this.btnKeyFile.Click += new System.EventHandler(this.btnKeyFile_Click);
            // 
            // comboPlatformTarget
            // 
            this.comboPlatformTarget.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.comboPlatformTarget.BackColor = System.Drawing.Color.WhiteSmoke;
            this.comboPlatformTarget.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboPlatformTarget.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.comboPlatformTarget.FormattingEnabled = true;
            this.comboPlatformTarget.Location = new System.Drawing.Point(198, 68);
            this.comboPlatformTarget.Name = "comboPlatformTarget";
            this.comboPlatformTarget.Size = new System.Drawing.Size(133, 21);
            this.comboPlatformTarget.TabIndex = 7;
            // 
            // chkPrefer32Bit
            // 
            this.chkPrefer32Bit.AutoSize = true;
            this.chkPrefer32Bit.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(30)))), ((int)(((byte)(30)))), ((int)(((byte)(30)))));
            this.chkPrefer32Bit.Location = new System.Drawing.Point(504, 68);
            this.chkPrefer32Bit.Name = "chkPrefer32Bit";
            this.chkPrefer32Bit.Size = new System.Drawing.Size(83, 17);
            this.chkPrefer32Bit.TabIndex = 8;
            this.chkPrefer32Bit.Text = "Prefer 32-bit";
            this.chkPrefer32Bit.UseVisualStyleBackColor = true;
            // 
            // XBuildPropertyPagePanel
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.mainPanel);
            this.Margin = new System.Windows.Forms.Padding(5);
            this.Name = "XBuildPropertyPagePanel";
            this.Size = new System.Drawing.Size(633, 618);
            this.mainPanel.ResumeLayout(false);
            this.mainPanel.PerformLayout();
            this.panelSigning.ResumeLayout(false);
            this.panelSigning.PerformLayout();
            this.outputTableLayoutPanel.ResumeLayout(false);
            this.outputTableLayoutPanel.PerformLayout();
            this.treatWarningsAsErrorsTableLayoutPanel.ResumeLayout(false);
            this.treatWarningsAsErrorsTableLayoutPanel.PerformLayout();
            this.panelErrorsAndWarnings.ResumeLayout(false);
            this.panelErrorsAndWarnings.PerformLayout();
            this.panelTop.ResumeLayout(false);
            this.panelTop.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.TableLayoutPanel mainPanel;
        private System.Windows.Forms.TableLayoutPanel panelTop;
        private System.Windows.Forms.Label lblGeneralLine;
        private System.Windows.Forms.Label lblGeneral;
        internal System.Windows.Forms.TextBox txtDefineConstants;
        internal System.Windows.Forms.ComboBox comboPlatformTarget;
        private System.Windows.Forms.Label lblDefineConstants;
        private System.Windows.Forms.CheckBox chkPPO;
        private System.Windows.Forms.Label lblPlatformTarget;
        private System.Windows.Forms.CheckBox chkPrefer32Bit;
        private System.Windows.Forms.CheckBox chkOptimize;
        private System.Windows.Forms.TableLayoutPanel panelErrorsAndWarnings;
        private System.Windows.Forms.Label lblErrorsAndWarnings;
        private System.Windows.Forms.Label lblErrorsAndWarningsLine;
        internal System.Windows.Forms.TableLayoutPanel treatWarningsAsErrorsTableLayoutPanel;
        internal System.Windows.Forms.Label treatWarningsAsErrorsLineLabel;
        internal System.Windows.Forms.Label treatWarningsAsErrorsLabel;
        internal System.Windows.Forms.Label lblWarningLevel;
        internal System.Windows.Forms.Label lblDisabledWarnings;
        internal System.Windows.Forms.ComboBox cboWarningLevel;
        internal System.Windows.Forms.TextBox txtDisabledWarnings;
        internal System.Windows.Forms.RadioButton rbWarningNone;
        internal System.Windows.Forms.RadioButton rbWarningAll;
        internal System.Windows.Forms.RadioButton rbWarningSpecific;
        internal System.Windows.Forms.TextBox txtSpecificWarnings;
        internal System.Windows.Forms.TableLayoutPanel outputTableLayoutPanel;
        internal System.Windows.Forms.Label outputLineLabel;
        internal System.Windows.Forms.Label outputLabel;
        internal System.Windows.Forms.Label lblOutputPath;
        internal System.Windows.Forms.TextBox txtOutputPath;
        internal System.Windows.Forms.Button btnOutputPathBrowse;
        internal System.Windows.Forms.CheckBox chkRegisterForComInterop;
        internal System.Windows.Forms.CheckBox chkXMLDocumentationFile;
        internal System.Windows.Forms.Label lblIntermediateOutputPath;
        internal System.Windows.Forms.TextBox txtXMLDocumentationFile;
        internal System.Windows.Forms.TextBox txtIntermediateOutputPath;
        internal System.Windows.Forms.Button btnIntermediateOutputPath;
        private System.Windows.Forms.CheckBox chkUseSharedCompilation;
        internal System.Windows.Forms.Label lblCommandLineOption;
        internal System.Windows.Forms.TextBox txtCommandLineOption;
        internal System.Windows.Forms.TableLayoutPanel panelSigning;
        internal System.Windows.Forms.Label lblSigningLine;
        internal System.Windows.Forms.Label lblSigning;
        internal System.Windows.Forms.CheckBox chkSignAssembly;
        internal System.Windows.Forms.Label lblAssemblyOriginatorKeyFile;
        internal System.Windows.Forms.TextBox txtAssemblyOriginatorKeyFile;
        internal System.Windows.Forms.CheckBox chkDelaySign;
        private System.Windows.Forms.ToolTip toolTip1;
        internal System.Windows.Forms.CheckBox chkSuppressRCWarnings;
        internal System.Windows.Forms.Button btnKeyFile;
    }
}
