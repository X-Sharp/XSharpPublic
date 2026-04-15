
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING XSharp.VFP
USING XSharp.Internal

/// <include file="VFPDocs.xml" path="Runtimefunctions/aprinters/*" />
[FoxArrayInputParameter(1)];
[FoxProFunction("APRINTERS", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION APrinters( ArrayName AS USUAL, nValue := 0 AS INT ) AS INT
    LOCAL aPrintersFromService AS ARRAY
    LOCAL nRows AS DWORD
    LOCAL nCols AS DWORD

    nCols := (DWORD)IIF(nValue > 0, 5, 2)
    aPrintersFromService := VfpUIService.Provider:GetPrinters(nValue)

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
    ELSE
        IF ArrayName IS __FoxArray VAR faDest
            faDest:ReDim(0, 0)
        ENDIF
    ENDIF

    RETURN (INT)nRows
ENDFUNC
