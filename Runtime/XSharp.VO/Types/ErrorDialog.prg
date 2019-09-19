//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

FUNCTION ErrorDialog( e AS Exception ) AS INT
   RETURN (INT) XSharp.ErrorDialog{ e }:ShowDialog()

FUNCTION ErrorDialog( txt AS STRING ) AS INT
   RETURN (INT) XSharp.ErrorDialog{ txt }:ShowDialog()

CLASS XSharp.ErrorDialog INHERIT System.Windows.Forms.Form

    EXPORT INSTANCE ErrorText AS System.Windows.Forms.TextBox
    EXPORT INSTANCE CloseButton AS System.Windows.Forms.Button
    EXPORT INSTANCE CopyButton AS System.Windows.Forms.Button

    CONSTRUCTOR( e AS Exception )
        SELF(e:ToString())
      RETURN

    CONSTRUCTOR( txt AS STRING )
      SUPER()
      SELF:InitializeComponent()
      SELF:SetLanguageStrings()
      SELF:Text += System.Reflection.Assembly.GetEntryAssembly():Location
      ErrorText:Text := txt
      ErrorText:Select( 0, 0 )
      RETURN

    PRIVATE METHOD SetLanguageStrings() as VOID
        SELF:Text               := __CavoStr(XSharp.VOErrors.ERRORDIALOG_TITLE)
        SELF:CloseButton:Text   := __CavoStr(XSharp.VOErrors.ERRORDIALOG_CLOSE)
        SELF:CopyButton:Text    := __CavoStr(XSharp.VOErrors.ERRORDIALOG_COPY)

    PRIVATE METHOD InitializeComponent() AS VOID
        SELF:ErrorText := System.Windows.Forms.TextBox{}
        SELF:CloseButton := System.Windows.Forms.Button{}
        SELF:CopyButton := System.Windows.Forms.Button{}
        SELF:SuspendLayout()
        //
        // ErrorText
        //
        SELF:ErrorText:Anchor := ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) ;
                    | System.Windows.Forms.AnchorStyles.Left) ;
                    | System.Windows.Forms.AnchorStyles.Right)))
        SELF:ErrorText:BackColor := System.Drawing.SystemColors.Window
        SELF:ErrorText:CausesValidation := FALSE
        SELF:ErrorText:Font := System.Drawing.Font{"Courier New", ((Single) 8)}
        SELF:ErrorText:Location := System.Drawing.Point{13, 12}
        SELF:ErrorText:Multiline := TRUE
        SELF:ErrorText:Name := "ErrorText"
        SELF:ErrorText:ReadOnly := TRUE
        SELF:ErrorText:ScrollBars := System.Windows.Forms.ScrollBars.Both
        SELF:ErrorText:Size := System.Drawing.Size{782, 279}
        SELF:ErrorText:TabIndex := 0
        SELF:ErrorText:WordWrap := FALSE
        //
        // CloseButton
        //
        SELF:CloseButton:Anchor := System.Windows.Forms.AnchorStyles.Bottom
        SELF:CloseButton:CausesValidation := FALSE
        SELF:CloseButton:DialogResult := System.Windows.Forms.DialogResult.OK
        SELF:CloseButton:Location := System.Drawing.Point{250, 297}
        SELF:CloseButton:Name := "CloseButton"
        SELF:CloseButton:Size := System.Drawing.Size{122, 23}
        SELF:CloseButton:TabIndex := 1
        SELF:CloseButton:Text := "Close"
        SELF:CloseButton:UseVisualStyleBackColor := TRUE
        //
        // CopyButton
        //
        SELF:CopyButton:Anchor := System.Windows.Forms.AnchorStyles.Bottom
        SELF:CopyButton:Location := System.Drawing.Point{410, 297}
        SELF:CopyButton:Name := "CopyButton"
        SELF:CopyButton:Size := System.Drawing.Size{122, 23}
        SELF:CopyButton:TabIndex := 2
        SELF:CopyButton:Text := "&Copy To Clipboard"
        SELF:CopyButton:UseVisualStyleBackColor := TRUE
        SELF:CopyButton:Click += System.EventHandler{ SELF, @CopyButton_Click() }
        //
        // ErrorDialog
        //
        SELF:AutoScaleDimensions := System.Drawing.SizeF{((Single) 6), ((Single) 13)}
        SELF:AutoScaleMode := System.Windows.Forms.AutoScaleMode.Font
        SELF:ClientSize := System.Drawing.Size{807, 332}
        SELF:Controls:Add(SELF:CopyButton)
        SELF:Controls:Add(SELF:CloseButton)
        SELF:Controls:Add(SELF:ErrorText)
        SELF:Name := "ErrorDialog"
        SELF:Text := "Application Error - "
        SELF:ResumeLayout(FALSE)
        SELF:PerformLayout()

    PRIVATE METHOD CopyButton_Click( sender AS OBJECT, e AS System.EventArgs ) AS VOID
       System.Windows.Forms.Clipboard.SetDataObject( ErrorText:Text, TRUE )
       RETURN
    
END CLASS
