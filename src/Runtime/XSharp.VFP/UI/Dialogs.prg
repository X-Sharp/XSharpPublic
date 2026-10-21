//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
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

/// <include file="VFPDocs.xml" path="Runtimefunctions/putfile/*" />
[FoxProFunction("PUTFILE", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION PutFile(cCustomText := "" AS STRING, cFileName := "" AS STRING, cFileExtensions := "" AS STRING) AS STRING
    RETURN VfpUIService.Provider:PutFile(cCustomText, cFileName, cFileExtensions)
END FUNCTION

/// <include file="VFPDocs.xml" path="Runtimefunctions/locfile/*" />
// Example: = LOCFILE("","PRG File:prg;Compiled:fxp;Backup:bak","Bestand")
[FoxProFunction("LOCFILE", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION LocFile(cFileName := "" AS STRING, cFileExtensions := "" AS STRING, cFileNameCaption := "" AS STRING) AS STRING

    // We'll look into the hard drive first (curdir + SET PATH + SET DEFAULT), no UI
    VAR cFound := __VfpLocFileSearch(cFileName, cFileExtensions)
    IF !String.IsNullOrEmpty(cFound)
        RETURN cFound
    ENDIF

    // If not found then we open a dialog (reuse GetFile with the same filter format)
    VAR cResult := VfpUIService.Provider:GetFile(cFileExtensions, "", "", 0, cFileNameCaption)
    IF !String.IsNullOrEmpty(cResult)
        RETURN cResult
    ENDIF

    // Cancel/Esc: VFP generates an error and LOCFILE doesn't return any value
    VAR err := Error{"File '" + cFileName + "' does not exist"}
    err:Gencode     := Gencode.EG_OPEN
    err:FuncSym     := "LOCFILE"
    err:Description := err:Message
    THROW err
END FUNCTION

// Search for the file as is, and if it doesn't have an extension, try the extensions from cFileExtensions
INTERNAL FUNCTION __VfpLocFileSearch(cFileName AS STRING, cFileExtensions AS STRING) AS STRING
    IF String.IsNullOrEmpty(cFileName)
        RETURN ""
    ENDIF
    IF File(cFileName)
        RETURN FPathName()
    ENDIF
    IF String.IsNullOrEmpty(System.IO.Path.GetExtension(cFileName))
        FOREACH cExt AS STRING IN __VfpExtractExtensions(cFileExtensions)
            IF File(cFileName + "." + cExt)
                RETURN FPathName()
            ENDIF
        NEXT
    ENDIF
    RETURN ""
END FUNCTION

// Extracts "bare" extensions from the format "Desc:ext;Desc:ext,ext2" (without descriptions or wildcards)
INTERNAL FUNCTION __VfpExtractExtensions(cFileExtensions AS STRING) AS List<STRING>
    VAR aResult := List<STRING>{}
    IF String.IsNullOrEmpty(cFileExtensions)
        RETURN aResult
    ENDIF
    FOREACH cGroup AS STRING IN cFileExtensions:Split(<CHAR>{c';'})
        VAR cExts  := cGroup
        VAR nColon := cGroup:IndexOf(":")
        IF nColon >= 0
            cExts := cGroup:Substring(nColon + 1)
        ENDIF
        FOREACH cRaw AS STRING IN cExts:Split(<CHAR>{c','})
            VAR cE := cRaw:Trim()
            IF cE:Length > 0 .AND. cE:IndexOfAny(<CHAR>{c'*', c'?'}) < 0
                aResult:Add(cE)
            ENDIF
        NEXT
    NEXT
    RETURN aResult
END FUNCTION

/// <include file="VFPDocs.xml" path="Runtimefunctions/inputbox/*" />
[FoxProFunction("INPUTBOX", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION InputBox(cInputPrompt := "" AS STRING, cDialogCaption := "" AS STRING, cDefaultValue := "" AS STRING, ;
                  nTimeout := 0 AS LONG, cTimeoutValue := "" AS STRING, cCancelValue := "" AS STRING) AS STRING
    RETURN VfpUIService.Provider:InputBox(cInputPrompt, cDialogCaption, cDefaultValue, nTimeout, cTimeoutValue, cCancelValue)
END FUNCTION
