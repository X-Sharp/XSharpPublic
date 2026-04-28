//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/cntbar/*" />
[FoxProFunction("CNTBAR", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Stub, FoxCriticality.Medium)];
FUNCTION CntBar(cPopupName AS STRING) AS INT
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/cntpad/*" />
[FoxProFunction("CNTPAD", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Stub, FoxCriticality.Medium)];
FUNCTION CntPad(cMenuName AS STRING) AS INT
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/col/*" />
[FoxProFunction("COL", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION Col() AS INT
    RETURN XSharp.RT.Functions.Col()

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/row/*" />
[FoxProFunction("ROW", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION Row() AS INT
    RETURN XSharp.RT.Functions.Row()

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/mcol/*" />
[FoxProFunction("MCOL", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Stub, FoxCriticality.Medium)];
FUNCTION MCol(cWindowName := "" AS STRING) AS INT
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/mdown/*" />
[FoxProFunction("MDOWN", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Stub, FoxCriticality.Medium)];
FUNCTION MDown() AS LOGIC
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/chrsaw/*" />
[FoxProFunction("CHRSAW", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Stub, FoxCriticality.Low)];
FUNCTION ChrSaw(nSeconds := 0 AS REAL8) AS LOGIC
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/rgb/*" />
[FoxProFunction("RGB", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION RGB(nRedValue AS BYTE, nGreenValue AS BYTE, nBlueValue AS BYTE) AS DWORD
    RETURN XSharp.Core.Functions.RGB(nRedValue, nGreenValue, nBlueValue)

/// <include file="VFPDocs.xml" path="Runtimefunctions/loadpicture/*" />
[FoxProFunction("LOADPICTURE", FoxFunctionCategory.General, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION LoadPicture(cFileName := "" AS STRING) AS OBJECT
    IF IsNil(cFileName) .OR. Empty(cFileName)
        RETURN NULL_OBJECT
    ENDIF

    LOCAL cFile AS STRING
    cFile := (STRING) cFileName

    // use XSharp.Core.Functions.File to respect the SET PATH
    IF XSharp.Core.Functions.File(cFileName)
        cFile := XSharp.Core.Functions.FPathName()
    ELSE
        VAR err := Error.ArgumentError(__FUNCTION__, NAMEOF(cFileName), 1, "File does not exist: " + cFileName)
        THROW err
    ENDIF

    RETURN VfpUIService.Provider:LoadPicture(cFile)
