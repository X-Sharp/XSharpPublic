//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Windows.Forms

/// <include file="VFPDocs.xml" path="Runtimefunctions/messagebox/*" />
FUNCTION MessageBox( eMessageText , nDialogBoxType , cTitleBarText, nTimeout) AS LONG
    LOCAL cMessage, cTitle AS STRING
    IF IsNil(eMessageText)
          THROW Error{"Parameter eMessageText is missing. This parameter is mandatory"}
    ENDIF
    IF !IsString(eMessageText)
          THROW Error{"Parameter eMessageText should be of type STRING"}
    ENDIF
    cMessage := eMessageText
    IF IsString(cTitleBarText)
        cTitle := cTitleBarText
    ELSE
        cTitle := "XSharp"
    ENDIF
    LOCAL nButton := MessageBoxButtons.OK               AS MessageBoxButtons
    LOCAL nIcon   := MessageBoxIcon.None                AS MessageBoxIcon
    LOCAL nDefault := MessageBoxDefaultButton.Button1   AS MessageBoxDefaultButton
    IF IsNumeric(nDialogBoxType)
        LOCAL nType := nDialogBoxType AS LONG
        nButton  := (MessageBoxButtons)         _AND(nType, 0x0F)
        nIcon    := (MessageBoxIcon)            _AND(nType, 0xF0)
        nDefault := (MessageBoxDefaultButton)   _AND(nType, 0xF00)
    ENDIF
    IF IsNumeric(nTimeout)
        RETURN XSharp.VFP.AutoCloseMessageBox.Show(cMessage, cTitle, nTimeout, nButton, nIcon, nDefault)
    ENDIF
    RETURN System.Windows.Forms.MessageBox.Show(cMessage, cTitle, nButton, nIcon, nDefault)
        
