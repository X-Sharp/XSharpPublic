//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING XSharp.VFP

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

INTERNAL DEFINE CF_NOSCRIPTSEL := 0x00800000L
INTERNAL DEFINE CF_PRINTERFONTS := 0x00000002L
