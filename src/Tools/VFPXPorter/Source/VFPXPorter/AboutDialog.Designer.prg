﻿//------------------------------------------------------------------------------
//  <auto-generated>
//     This code was generated by a tool.
//     Runtime version: 4.0.30319.42000
//     Generator      : XSharp.CodeDomProvider 2.13.2.2
//     Timestamp      : 16/09/2022 15:58:07
//     
//     Changes to this file may cause incorrect behavior and may be lost if
//     the code is regenerated.
//  </auto-generated>
//------------------------------------------------------------------------------
BEGIN NAMESPACE VFPXPorter
    PUBLIC PARTIAL CLASS AboutDialog	;
		INHERIT System.Windows.Forms.Form
        PRIVATE pictureBox1 AS System.Windows.Forms.PictureBox
        PRIVATE okButton AS System.Windows.Forms.Button
        PRIVATE labelInfo AS System.Windows.Forms.Label
        PRIVATE components	:=	NULL AS System.ComponentModel.IContainer
                                                                                                
        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected method Dispose(disposing as logic) as void strict

            if (disposing .AND. (components != null))
                components:Dispose()
            endif
            Super:Dispose(disposing)
            return

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        PRIVATE METHOD InitializeComponent() AS VOID STRICT
			LOCAL resources	:=	System.ComponentModel.ComponentResourceManager{typeof(AboutDialog)} AS System.ComponentModel.ComponentResourceManager
			SELF:pictureBox1	:=	System.Windows.Forms.PictureBox{}
			SELF:okButton	:=	System.Windows.Forms.Button{}
			SELF:labelInfo	:=	System.Windows.Forms.Label{}
			((System.ComponentModel.ISupportInitialize)(SELF:pictureBox1)):BeginInit()
			SELF:SuspendLayout()
			//	
			//	pictureBox1
			//	
			SELF:pictureBox1:Image	:=	((System.Drawing.Image)(resources:GetObject("pictureBox1.Image")))
			SELF:pictureBox1:Location	:=	System.Drawing.Point{12, 12}
			SELF:pictureBox1:Margin	:=	System.Windows.Forms.Padding{3, 2, 3, 2}
			SELF:pictureBox1:Name	:=	"pictureBox1"
			SELF:pictureBox1:Size	:=	System.Drawing.Size{59, 58}
			SELF:pictureBox1:SizeMode	:=	System.Windows.Forms.PictureBoxSizeMode.AutoSize
			SELF:pictureBox1:TabIndex	:=	0
			SELF:pictureBox1:TabStop	:=	false
			//	
			//	okButton
			//	
			SELF:okButton:Location	:=	System.Drawing.Point{296, 121}
			SELF:okButton:Margin	:=	System.Windows.Forms.Padding{3, 2, 3, 2}
			SELF:okButton:Name	:=	"okButton"
			SELF:okButton:Size	:=	System.Drawing.Size{93, 37}
			SELF:okButton:TabIndex	:=	1
			SELF:okButton:Text	:=	"&Ok"
			SELF:okButton:UseVisualStyleBackColor	:=	true
			SELF:okButton:Click	+=	System.EventHandler{ SELF, @okButton_Click() }
			//	
			//	labelInfo
			//	
			SELF:labelInfo:AutoSize	:=	true
			SELF:labelInfo:Location	:=	System.Drawing.Point{96, 12}
			SELF:labelInfo:Name	:=	"labelInfo"
			SELF:labelInfo:Size	:=	System.Drawing.Size{272, 48}
			SELF:labelInfo:TabIndex	:=	2
			SELF:labelInfo:Text	:=	e"VFP XPorter Application\r\nInspired by South West Fox 2019 - Gilbert, AZ\r\nFabrice Foray, XSharp DevTeam\r\n"
			//	
			//	AboutDialog
			//	
			SELF:AcceptButton	:=	SELF:okButton
			SELF:AutoScaleDimensions	:=	System.Drawing.SizeF{8, 16}
			SELF:AutoScaleMode	:=	System.Windows.Forms.AutoScaleMode.Font
			SELF:ClientSize	:=	System.Drawing.Size{404, 171}
			SELF:Controls:Add(SELF:labelInfo)
			SELF:Controls:Add(SELF:okButton)
			SELF:Controls:Add(SELF:pictureBox1)
			SELF:FormBorderStyle	:=	System.Windows.Forms.FormBorderStyle.FixedDialog
			SELF:Margin	:=	System.Windows.Forms.Padding{3, 2, 3, 2}
			SELF:MaximizeBox	:=	false
			SELF:MinimizeBox	:=	false
			SELF:Name	:=	"AboutDialog"
			SELF:StartPosition	:=	System.Windows.Forms.FormStartPosition.CenterParent
			SELF:Text	:=	"About"
			SELF:Load	+=	System.EventHandler{ SELF, @AboutDialog_Load() }
			((System.ComponentModel.ISupportInitialize)(SELF:pictureBox1)):EndInit()
			SELF:ResumeLayout(false)
			SELF:PerformLayout()
		END METHOD
                                
        #endregion
    
    END CLASS 
END NAMESPACE
