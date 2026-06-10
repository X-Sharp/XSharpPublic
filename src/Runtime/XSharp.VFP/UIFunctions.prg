//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING XSharp.VFP

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/messagebox/*" />
[FoxProFunction("MESSAGEBOX", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION MessageBox( eMessageText AS USUAL, nDialogBoxType := NIL AS USUAL, cTitleBarText := NIL AS USUAL, nTimeOut := NIL AS USUAL) AS LONG
    LOCAL cMessage AS STRING
    LOCAL nRealType := 0 AS LONG
    LOCAL cRealTitle := "" AS STRING
    LOCAL nRealTimeout := 0 AS LONG
    LOCAL nNumericFound := 0 AS INT
    LOCAL lTitleSet := FALSE AS LOGIC

    cMessage := IIF(IsString(eMessageText), (STRING)eMessageText, AsString(eMessageText))

    LOCAL aParams := { nDialogBoxType, cTitleBarText, nTimeOut } AS ARRAY

    FOREACH VAR uParam IN aParams
        IF !IsNil(uParam)
            IF IsString(uParam) .AND. !lTitleSet
                cRealTitle := (STRING)uParam
                lTitleSet := TRUE
            ELSEIF IsNumeric(uParam)
                nNumericFound++
                IF nNumericFound == 1
                    nRealType := (LONG)uParam
                ELSEIF nNumericFound == 2
                    nRealTimeout := (LONG)uParam
                ENDIF
            ENDIF
        ENDIF
    NEXT

    IF String.IsNullOrEmpty(cRealTitle)
        cRealTitle := System.IO.Path.GetFileNameWithoutExtension(System.Reflection.Assembly.GetEntryAssembly()?:Location)
        IF String.IsNullOrEmpty(cRealTitle) ; cRealTitle := "Microsoft Visual FoxPro" ; ENDIF
    ENDIF

    RETURN VfpUIService.Provider:ShowMessageBox(cMessage, nRealType, cRealTitle, nRealTimeout)
END FUNCTION

/// <include file="VFPDocs.xml" path="Runtimefunctions/sysmetric/*" />
[FoxProFunction("SYSMETRIC", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION SysMetric( nScreenElement AS LONG) AS LONG
    RETURN VfpUIService.Provider:SysMetric(nScreenElement)
END FUNCTION

/// <include file="VFPDocs.xml" path="Runtimefunctions/fontmetric/*" />
[FoxProFunction("FONTMETRIC", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION FontMetric( nAttribute , cFontName, nFontSize , cFontStyle) AS LONG CLIPPER
    LOCAL nAttr := 0 AS LONG
    IF IsNumeric(nAttribute)
        nAttr := (LONG)nAttribute    
    ENDIF

    RETURN VfpUIService.Provider:FontMetric(nAttr, cFontName, nFontSize, cFontStyle)
