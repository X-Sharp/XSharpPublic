//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING XSharp.VFP

INTERNAL DEFINE CF_NOSCRIPTSEL := 0x00800000L
INTERNAL DEFINE CF_PRINTERFONTS := 0x00000002L

/// <include file="VFPDocs.xml" path="Runtimefunctions/getcolor/*" />
[FoxProFunction("GETCOLOR", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION GetColor( nDefaultColorNumber ) AS INT CLIPPER
    RETURN VfpUIService.Provider:GetColor(nDefaultColorNumber)
END FUNCTION

/// <include file="VFPDocs.xml" path="Runtimefunctions/getfont/*" />
[FoxProFunction("GETFONT", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION GetFont(cFontName, nFontSize, cFontStyle, nFontCharSet) AS STRING CLIPPER
    RETURN VfpUIService.Provider:GetFont(cFontName, nFontSize, cFontStyle, nFontCharSet)
END FUNCTION

/// <include file="VFPDocs.xml" path="Runtimefunctions/getdir/*" />
[FoxProFunction("GETDIR", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION GetDir(cDirectory := "" AS STRING, cText := "" AS STRING, cCaption := "" AS STRING, nFlags := 0 AS LONG, lRootOnly := FALSE AS LOGIC) AS STRING
    RETURN VfpUIService.Provider:GetDir(cDirectory, cText, cCaption, nFlags, lRootOnly)
END FUNCTION

/// <include file="VFPDocs.xml" path="Runtimefunctions/getfile/*" />
[FoxProFunction("GETFILE", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION GetFile( cFileExtensions := "" AS STRING, cText := "" AS STRING, cOpenButtonCaption := "" AS STRING, nButtonType := 0 AS LONG, cTitleBarCaption := "" AS STRING) AS STRING
    RETURN VfpUIService.Provider:GetFile(cFileExtensions, cText, cOpenButtonCaption, nButtonType, cTitleBarCaption)
END FUNCTION

/// <include file="VFPDocs.xml" path="Runtimefunctions/getpict/*" />
[FoxProFunction("GETPICT", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION GetPict( cFileExtensions := "" AS STRING, cFileNameCaption := "" AS STRING, cOpenButtonCaption := "" AS STRING) AS STRING
    RETURN VfpUIService.Provider:GetPict(cFileExtensions, cFileNameCaption, cOpenButtonCaption)
END FUNCTION
