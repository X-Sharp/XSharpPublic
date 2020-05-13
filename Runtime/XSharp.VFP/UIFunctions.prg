//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Windows.Forms



/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/messagebox/*" />
FUNCTION MessageBox( eMessageText AS USUAL, nDialogBoxType := 0 AS LONG, cTitleBarText := "" AS STRING,nTimeOut := 0 AS LONG) AS LONG
    LOCAL cMessage AS STRING
    IF !IsString(eMessageText)
        cMessage := AsString(eMessageText)
    ELSE
        cMessage := eMessageText    
    ENDIF
    IF String.IsNullOrEmpty(cTitleBarText)
        cTitleBarText := System.IO.Path.GetFileNameWithoutExtension(System.Reflection.Assembly.GetEntryAssembly():Location)
    ENDIF
    LOCAL nButton := MessageBoxButtons.OK               AS MessageBoxButtons
    LOCAL nIcon   := MessageBoxIcon.None                AS MessageBoxIcon
    LOCAL nDefault := MessageBoxDefaultButton.Button1   AS MessageBoxDefaultButton
    nButton  := (MessageBoxButtons)         _AND(nDialogBoxType, 0x0F)
    nIcon    := (MessageBoxIcon)            _AND(nDialogBoxType, 0xF0)
    nDefault := (MessageBoxDefaultButton)   _AND(nDialogBoxType, 0xF00)
    IF nTimeOut >= 1
        RETURN XSharp.VFP.AutoCloseMessageBox.Show(cMessage, cTitleBarText, nTimeOut, nButton, nIcon, nDefault)
    ENDIF
    RETURN System.Windows.Forms.MessageBox.Show(cMessage, cTitleBarText, nButton, nIcon, nDefault)
        
