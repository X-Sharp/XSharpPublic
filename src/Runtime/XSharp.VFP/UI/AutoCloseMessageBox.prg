//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// This class was inspired by the AutoClosingMessageBox on https://github.com/DmitryGaravsky/AutoClosingMessageBox

USING System
USING System.Collections.Generic
USING System.Text
USING System.Threading
USING System.Runtime.InteropServices
USING System.Windows.Forms

INTERNAL CLASS XSharp.VFP.AutoCloseMessageBox
    PRIVATE INITONLY _caption AS STRING
    PRIVATE INITONLY _result AS DialogResult
    
    PRIVATE CONSTRUCTOR (caption AS STRING , timeout AS INT , showMethod AS @@Func<STRING, MessageBoxButtons, DialogResult> ,;
        buttons AS MessageBoxButtons, nIcon   AS MessageBoxIcon, ;
        nDefault AS MessageBoxDefaultButton, defaultResult := DialogResult.None AS DialogResult ) 
        SELF:_caption := IIF( caption != NULL, caption,"")
        SELF:_result := buttons:ToDialogResult(defaultResult)
        BEGIN USING VAR timer := System.Threading.Timer {OnTimerElapsed, _result.ToDialogButtonId(buttons), timeout, Timeout.Infinite}
            SELF:_result := showMethod(SELF:_caption, buttons)
        END USING
        RETURN
    
    PRIVATE METHOD OnTimerElapsed(state AS OBJECT )  AS VOID
        SELF:CloseMessageBoxWindow((INT)state)
        RETURN
        
    PRIVATE METHOD CloseMessageBoxWindow(dlgButtonId AS INT ) AS VOID
        VAR hWndMsgBox := Win32.FindMessageBox(_caption)
        Win32.SendCommandToDlgButton(hWndMsgBox, dlgButtonId)
        RETURN
        
    PUBLIC STATIC METHOD Show(text AS STRING ,caption := "XSharp" AS STRING , timeout := 1000 AS INT,;
        buttons := MessageBoxButtons.OK AS MessageBoxButtons, nIcon   := MessageBoxIcon.None AS MessageBoxIcon, ;
        nDefault := MessageBoxDefaultButton.Button1 AS MessageBoxDefaultButton, ;
        defaultResult := DialogResult.None AS DialogResult ) AS DialogResult 
        
        RETURN AutoCloseMessageBox{caption, timeout,  { capt, btns => MessageBox.Show(text, capt, buttons, nIcon, nDefault) },;
            buttons, nIcon, nDefault, defaultResult }:_result
        
    PUBLIC STATIC METHOD Show(owner AS IWin32Window , text AS STRING , caption := "XSharp" AS STRING, timeout := 1000 AS INT, ;
        buttons := MessageBoxButtons.OK AS MessageBoxButtons, nIcon  := MessageBoxIcon.None AS MessageBoxIcon, ;
        nDefault := MessageBoxDefaultButton.Button1 AS MessageBoxDefaultButton, defaultResult := DialogResult.None AS DialogResult ) AS DialogResult  
        RETURN AutoCloseMessageBox{caption, timeout, {capt, btns => MessageBox.Show(owner, text, capt, buttons, nIcon, nDefault)} ,;
            buttons, nIcon, nDefault, defaultResult}:_result
        
        
END CLASS

INTERNAL STATIC CLASS MessageBoxButtonsExtension
	PUBLIC STATIC METHOD ToDialogResult(SELF buttons AS MessageBoxButtons , defaultResult AS DialogResult ) AS DialogResult
        LOCAL result AS DialogResult
        result := defaultResult
		BEGIN SWITCH buttons
		CASE MessageBoxButtons.OK
			result := DialogResult.OK
            
		CASE MessageBoxButtons.OKCancel
			IF result != DialogResult.Cancel
    			result := DialogResult.OK
            ENDIF
            
		CASE MessageBoxButtons.YesNo
			IF result != DialogResult.No
				result := DialogResult.Yes
			ENDIF
			
		CASE MessageBoxButtons.YesNoCancel
			IF result != DialogResult.No .AND. result != DialogResult.Cancel
				result := DialogResult.Yes
			ENDIF
			
		CASE MessageBoxButtons.RetryCancel
			IF result != DialogResult.Retry
				result := DialogResult.Cancel
			ENDIF
			
		CASE MessageBoxButtons.AbortRetryIgnore
			IF result != DialogResult.Abort .AND. result != DialogResult.Retry
				result := DialogResult.Ignore
			ENDIF
		END SWITCH
		RETURN result


	PUBLIC STATIC METHOD ToDialogButtonId(SELF result AS DialogResult , buttons AS MessageBoxButtons ) AS LONG
		IF buttons == MessageBoxButtons.OK
			RETURN 2
		ENDIF
		RETURN (LONG)result


END CLASS

