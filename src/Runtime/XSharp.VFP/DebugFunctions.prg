//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING XSharp.Internal
USING System.Collections.Generic
USING System.Text
using System.Diagnostics

/// <include file="VFPDocs.xml" path="Runtimefunctions/program/*" />
[FoxProFunction("PROGRAM", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Program( nLevel, lShowSignature) AS USUAL CLIPPER
    local frameCount as LONG
    local iLevel as LONG
    local result as string
    IF IsLong(nLevel)
        iLevel := nLevel
    ELSE
        iLevel := 0
    ENDIF
    IF IsNil(lShowSignature)
        lShowSignature := FALSE
    ENDIF
    frameCount := StackTrace{ FALSE }:FrameCount
    if (iLevel == -1)
        return frameCount-1 // subtract 1 for this function
    endif
    if ! IsLong(nLevel)
        result := ProcName(1, lShowSignature)
        if (result:StartsWith("FUNCTIONS."))
            result := result:Substring(10)
        endif
        return result
    ELSE
        if iLevel == 0
            iLevel := 1
        endif
        if iLevel >=0 .and. iLevel < frameCount
            result := ProcName(iLevel, lShowSignature)
            if (result:StartsWith("FUNCTIONS."))
                result := result:Substring(10)
            endif
            return result
        endif
    endif
    return ""

/// <include file="VFPDocs.xml" path="Runtimefunctions/lineno/*" />
[FoxProFunction("LINENO", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.LanguageCore, FoxFunctionStatus.Stub, FoxCriticality.Low)];
FUNCTION LineNo(nPos := 0 AS USUAL) AS LONG
    VAR nLevel := 1
    LOCAL oFrame := NULL AS StackFrame
    VAR oTrace := StackTrace{TRUE}

    IF oTrace:FrameCount > nLevel
        oFrame := oTrace:GetFrame(nLevel)
        RETURN oFrame:GetFileLineNumber()
    ENDIF

    RETURN 0

/// <include file="VFPDocs.xml" path="Runtimefunctions/astackinfo/*" />
[FoxArrayInputParameter(1)];
[FoxProFunction("ASTACKINFO", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION AStackInfo (ArrayName AS USUAL) AS DWORD
    LOCAL aFoxArray AS __FoxArray
    IF ArrayName IS __FoxArray VAR aFox
        aFoxArray := aFox
    ELSE
        VAR cMessage := __VfpStr(VFPErrors.VFP_VARIABLE_NOT_ARRAY, nameof(ArrayName))
        THROW ArgumentException{cMessage}
    ENDIF

    VAR oTrace := StackTrace{TRUE}
    VAR nFrames := oTrace:FrameCount

    VAR nLevels := nFrames - 1
    IF nLevels <= 0
        RETURN 0
    ENDIF

    aFoxArray:ReDim((DWORD)nLevels, 6)

    LOCAL nIdx := 0 AS DWORD
    FOR VAR i := 1 TO nLevels - 1
        VAR oFrame := oTrace:GetFrame(i)
        VAR oMethod := oFrame:GetMethod()
        VAR nBase := (INT)(nIdx * 6)

        aFoxArray.__SetElement(nIdx + 1, nBase)      // Col 1: Stack Level

        VAR cFile := oFrame:GetFileName()
        aFoxArray.__SetElement(IIF(cFile != NULL, cFile, ""), nBase + 1)  // Col 2: File

        VAR cModule := ""
        IF oMethod:DeclaringType != NULL
            cModule := oMethod:DeclaringType:Name + "." + oMethod:Name
        ELSE
            cModule := oMethod:Name
        ENDIF

        aFoxArray.__SetElement(cModule, nBase + 2)    // Col 3: Module/Object
        aFoxArray.__SetElement(IIF(cFile != NULL, cFile, ""), nBase + 3)  // Col 4: Source file
        aFoxArray.__SetElement(oFrame:GetFileLineNumber(), nBase + 4)      // Col 5: Line
        aFoxArray.__SetElement("", nBase + 5)          // Col 6: Source content (n/a)
        nIdx += 1
    NEXT

    RETURN (DWORD)nIdx
END FUNCTION
