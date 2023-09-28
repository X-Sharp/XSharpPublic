//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Text
/// <include file="Gui.xml" path="doc/ResourceString/*" />
CLASS ResourceString INHERIT VObject
    PROTECT iLength AS INT
    PROTECT sBuffer AS STRING

    /// <include file="Gui.xml" path="doc/ResourceString.AsString/*" />
    METHOD AsString() AS STRING STRICT
        RETURN sBuffer

    /// <include file="Gui.xml" path="doc/ResourceString.ctor/*" />
    CONSTRUCTOR(xResourceID AS USUAL, nMaxLen := NIL AS USUAL)
        LOCAL hInst AS IntPtr
        LOCAL wID AS LONG
        LOCAL ptrBuffer AS StringBuilder
        LOCAL oResourceID as ResourceID

        SUPER()

        IF IsNumeric(xResourceID) .OR. IsSymbol(xResourceID)
            oResourceID := ResourceID{xResourceID} // , GetNatDLLHandle()}
        ELSEIF xResourceID IS ResourceID
            oResourceID := xResourceID
        ELSE
            WCError{#Init, #ResourceString, __WCSTypeError, xResourceID, 1}:Throw()
        ENDIF

        IF IsNil(nMaxLen)
            nMaxLen := 256
        ELSEIF !IsNumeric(nMaxLen)
            WCError{#Init, #ResourceString, __WCSTypeError, nMaxLen, 2}:Throw()
        ELSE
            NOP
        ENDIF

        hInst := oResourceID:Handle()

        IF String.IsNullOrEmpty(oResourceID:Name)
            wID := Val(oResourceID:Name)
        ELSE
            wID := oResourceID:ID
        ENDIF

        IF hInst == GetNatDllHandle()
            sBuffer := __CavoStr( (DWORD) wID )
            RETURN
        ENDIF

        ptrBuffer 	:= StringBuilder{(INT) nMaxLen+1}
        iLength 	:= GuiWin32.LoadString(hInst, wID, ptrBuffer, nMaxLen)
        IF iLength != 0
            sBuffer := ptrBuffer:ToString()
        ENDIF

        RETURN

    /// <include file="Gui.xml" path="doc/ResourceString.Length/*" />
    PROPERTY Length AS INT GET iLength

    /// <include file="Gui.xml" path="doc/ResourceString.Value/*" />
    PROPERTY Value AS STRING GET sBuffer

END CLASS

