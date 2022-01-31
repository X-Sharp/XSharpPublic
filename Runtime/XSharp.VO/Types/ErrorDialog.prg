//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Windows.Forms
USING System.Drawing

PROCEDURE InitializeErrorDialogHandler() _INIT1
   ShowErrorDialog_Handler := ErrorDialog

INTERNAL DEFINE IDABORT         := 3
INTERNAL DEFINE IDRETRY         := 4
INTERNAL DEFINE IDIGNORE        := 5
FUNCTION ErrorDialog( e AS Error ) AS INT
   LOCAL eResult AS DialogResult
   LOCAL nResult AS INT
   eResult := XSharp.ErrorDialog{ e }:ShowDialog()
   SWITCH eResult
   CASE DialogResult.Ignore
       nResult := IDIGNORE
   CASE DialogResult.Retry
       nResult := IDRETRY
   OTHERWISE
       nResult := IDABORT
   END SWITCH
   RETURN nResult

FUNCTION ErrorDialog( e AS Exception ) AS INT
   RETURN (INT) XSharp.ErrorDialog{ e }:ShowDialog()

FUNCTION ErrorDialog( txt AS STRING ) AS INT
   RETURN (INT) XSharp.ErrorDialog{ txt }:ShowDialog()

CLASS XSharp.ErrorDialog INHERIT System.Windows.Forms.Form

    EXPORT INSTANCE ErrorText AS System.Windows.Forms.TextBox
    EXPORT INSTANCE CloseButton AS System.Windows.Forms.Button
    EXPORT INSTANCE CopyButton AS System.Windows.Forms.Button
    EXPORT INSTANCE AbortButton AS System.Windows.Forms.Button
    EXPORT INSTANCE RetryButton AS System.Windows.Forms.Button
    EXPORT INSTANCE IgnoreButton AS System.Windows.Forms.Button

    PRIVATE lAbortRetryIgnoreMode := FALSE AS LOGIC

    CONSTRUCTOR( e AS Exception) 
        SELF( e:ToString() )
        IF e IS Error VAR oError
            SELF:CloseButton:Hide()
            SELF:AbortButton:Show()
            SELF:RetryButton:Show()
            SELF:IgnoreButton:Show()
            SELF:RetryButton:Enabled := oError:CanRetry
        
            SELF:ControlBox := FALSE
            SELF:lAbortRetryIgnoreMode := TRUE
            SELF:Text := IIF(oError:Severity == ES_WARNING, "WARNING", "ERROR")

            LOCAL y := SELF:CloseButton:Top AS INT
            LOCAL w := SELF:CloseButton:Width AS INT
            SELF:AbortButton:Location  := Point{SELF:Width / 2 - (w * 2 + 30) , y}
            SELF:RetryButton:Location  := Point{SELF:Width / 2 - (w + 10) , y}
            SELF:IgnoreButton:Location := Point{SELF:Width / 2 + 10 , y}
            SELF:CopyButton:Location   := Point{SELF:Width / 2 + w + 30 , y}
      ENDIF
      VAR threadState := System.Threading.Thread.CurrentThread:GetApartmentState()
      IF threadState != System.Threading.ApartmentState.STA
         SELF:CopyButton:Enabled := FALSE
      ENDIF
      RETURN


    CONSTRUCTOR( txt AS STRING )
      SUPER()
      SELF:InitializeComponent()
      SELF:SetLanguageStrings()
      SELF:Text += System.Reflection.Assembly.GetEntryAssembly():Location
      ErrorText:Text := txt
      ErrorText:Select( 0, 0 )
      RETURN

    PRIVATE METHOD SetLanguageStrings() AS VOID
        SELF:Text               := __CavoStr(XSharp.VOErrors.ERRORDIALOG_TITLE)
        SELF:CloseButton:Text   := __CavoStr(XSharp.VOErrors.ERRORDIALOG_CLOSE)
        SELF:CopyButton:Text    := __CavoStr(XSharp.VOErrors.ERRORDIALOG_COPY)
        SELF:AbortButton:Text   := __CavoStr(XSharp.VOErrors.ERRORDIALOG_ABORT)
        SELF:RetryButton:Text   := __CavoStr(XSharp.VOErrors.ERRORDIALOG_RETRY)
        SELF:IgnoreButton:Text  := __CavoStr(XSharp.VOErrors.ERRORDIALOG_IGNORE)

    PRIVATE METHOD InitializeComponent() AS VOID
        SELF:ErrorText := System.Windows.Forms.TextBox{}
        SELF:CloseButton := System.Windows.Forms.Button{}
        SELF:CopyButton := System.Windows.Forms.Button{}
        SELF:AbortButton := System.Windows.Forms.Button{}
        SELF:RetryButton := System.Windows.Forms.Button{}
        SELF:IgnoreButton := System.Windows.Forms.Button{}
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
        // AbortButton
        //
        SELF:AbortButton:Anchor := System.Windows.Forms.AnchorStyles.Bottom
        SELF:AbortButton:Location := System.Drawing.Point{510, 297}
        SELF:AbortButton:Name := "AbortButton"
        SELF:AbortButton:Size := System.Drawing.Size{122, 23}
        SELF:AbortButton:TabIndex := 2
        SELF:AbortButton:Text := "&Abort"
        SELF:AbortButton:UseVisualStyleBackColor := TRUE
        SELF:AbortButton:Visible := FALSE
        SELF:AbortButton:DialogResult := DialogResult.Abort
        //
        // RetryButton
        //
        SELF:RetryButton:Anchor := System.Windows.Forms.AnchorStyles.Bottom
        SELF:RetryButton:Location := System.Drawing.Point{510, 297}
        SELF:RetryButton:Name := "RetryButton"
        SELF:RetryButton:Size := System.Drawing.Size{122, 23}
        SELF:RetryButton:TabIndex := 2
        SELF:RetryButton:Text := "&Retry"
        SELF:RetryButton:UseVisualStyleBackColor := TRUE
        SELF:RetryButton:Visible := FALSE
        SELF:RetryButton:DialogResult := DialogResult.Retry
        //
        // IgnoreButton
        //
        SELF:IgnoreButton:Anchor := System.Windows.Forms.AnchorStyles.Bottom
        SELF:IgnoreButton:Location := System.Drawing.Point{510, 297}
        SELF:IgnoreButton:Name := "IgnoreButton"
        SELF:IgnoreButton:Size := System.Drawing.Size{122, 23}
        SELF:IgnoreButton:TabIndex := 2
        SELF:IgnoreButton:Text := "&Ignore"
        SELF:IgnoreButton:UseVisualStyleBackColor := TRUE
        SELF:IgnoreButton:Visible := FALSE
        SELF:IgnoreButton:DialogResult := DialogResult.Ignore
        //
        // ErrorDialog
        //
        SELF:AutoScaleDimensions := System.Drawing.SizeF{((Single) 6), ((Single) 13)}
        SELF:AutoScaleMode := System.Windows.Forms.AutoScaleMode.Font
        SELF:ClientSize := System.Drawing.Size{807, 332}
        SELF:Controls:Add(SELF:AbortButton)
        SELF:Controls:Add(SELF:RetryButton)
        SELF:Controls:Add(SELF:IgnoreButton)
        SELF:Controls:Add(SELF:CopyButton)
        SELF:Controls:Add(SELF:CloseButton)
        SELF:Controls:Add(SELF:ErrorText)
        SELF:Name := "ErrorDialog"
        SELF:Text := "Application Error - "
        SELF:ResumeLayout(FALSE)
        SELF:PerformLayout()

    PROTECTED OVERRIDE METHOD OnClosing( e AS System.ComponentModel.CancelEventArgs ) AS VOID
       IF SELF:lAbortRetryIgnoreMode .AND. SELF:DialogResult == DialogResult.Cancel // Alt+F4
           e:Cancel := TRUE
       ENDIF
       RETURN
    PRIVATE METHOD CopyButton_Click( sender AS OBJECT, e AS System.EventArgs ) AS VOID
       System.Windows.Forms.Clipboard.SetDataObject( ErrorText:Text, TRUE )
       RETURN
    
END CLASS
