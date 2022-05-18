//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text
using System.Diagnostics

/// <include file="VFPDocs.xml" path="Runtimefunctions/program/*" />
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
