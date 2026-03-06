
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING XSharp.VFP

/// <include file="VFPDocs.xml" path="Runtimefunctions/aprinters/*" />
[FoxProFunction("APRINTERS", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION APrinters( ArrayName , nValue ) AS INT CLIPPER
    LOCAL aPrintersFromService AS ARRAY
    LOCAL nRows AS DWORD
    LOCAL nCols AS DWORD

    Default(@nValue, 0)
    nCols := (DWORD)IIF((INT)nValue > 0, 5, 2)
    aPrintersFromService := VfpUIService.Provider:GetPrinters((INT)nValue)
    nRows := XSharp.VFP.Functions.ALen((__FoxArray)aPrintersFromService, 1)

    IF nRows > 0
        ArrayName := __FoxRedim(ArrayName, nRows, nCols)
        LOCAL faDest := (__FoxArray)ArrayName AS __FoxArray
        LOCAL faSrc  := (__FoxArray)aPrintersFromService AS __FoxArray

        FOR VAR i := 1 TO nRows
            FOR VAR j := 1 TO nCols
                faDest[i, j] := faSrc[i, j]
            NEXT
        NEXT
    ENDIF

    RETURN (INT)nRows
ENDFUNC
