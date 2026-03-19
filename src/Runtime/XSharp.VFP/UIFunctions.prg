//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING XSharp.VFP

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/messagebox/*" />
[FoxProFunction("MESSAGEBOX", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.High)];
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

    RETURN VfpUIService.Provider:ShowMessageBox(cMessage, nDialogBoxType, cTitleBarText, nTimeOut)
END FUNCTION

/// <include file="VFPDocs.xml" path="Runtimefunctions/sysmetric/*" />
[FoxProFunction("SYSMETRIC", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION SysMetric( nScreenElement AS LONG) AS LONG
    RETURN VfpUIService.Provider:SysMetric(nScreenElement)
END FUNCTION
