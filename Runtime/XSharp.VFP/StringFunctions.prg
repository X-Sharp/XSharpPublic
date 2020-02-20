//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// String Functions


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO

INTERNAL STATIC CLASS PathHelpers
    INTERNAL STATIC PROPERTY PathChar AS STRING AUTO
    STATIC CONSTRUCTOR()
        PathChar := Path.DirectorySeparatorChar:ToString()
END CLASS


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/addbs/*" />
/// <seealso cref='M:XSharp.VFP.Functions.DefaultExt(System.String)' />
/// <seealso cref='M:XSharp.VFP.Functions.JustDrive(System.String)' />
/// <seealso cref='M:XSharp.VFP.Functions.JustExt(System.String)' />
/// <seealso cref='M:XSharp.VFP.Functions.JustFName(System.String)' />
/// <seealso cref='M:XSharp.VFP.Functions.JustPath(System.String)' />
/// <seealso cref='M:XSharp.VFP.Functions.JustStem(System.String)' />
FUNCTION AddBs (cPath AS STRING) AS STRING
    IF String.IsNullOrEmpty(cPath)
        RETURN ""
    ENDIF
    cPath := cPath:TrimEnd()
    IF ! cPath.EndsWith(PathHelpers.PathChar)
        cPath += PathHelpers.PathChar
    ENDIF
    RETURN cPath


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/forceext/*" />
FUNCTION ForceExt( cFileName AS STRING, cExtension AS STRING) AS STRING
    IF String.IsNullOrEmpty(cFileName)
        RETURN ""
    ENDIF
    VAR result := Path.ChangeExtension(cFileName,cExtension)
    RETURN result

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/forceext/*" />
FUNCTION ForceExt( cFileName AS STRING, cExtension AS STRING, tlOptAsVfp9 AS LOGIC) AS STRING
    *-- current take on matters is that the Dotnet-Version should be Default behaviour
    *-- as vfp9 version behaviour in edge cases could be seen as erroneous
    *-- work in progress and not tested, as existing code should only call 2-parameter overload should be safe
    if tlOptAsVfp9 == .f. 
        return ForceExt( cFileName, cExtension)
    endif
    cFileName := JustFName(cFileName)
    if cFileName:EndsWith(".")
        *-- if filename ends with dot, cut that
        *-- but only rightmost one, ending in several dots cuts still only 1
        cFileName := cFileName:Substring(0 , cFileName:Length-1)
    endif
    if cExtension:StartsWith(".")
        cExtension := cExtension:Substring(1)
    endif
    local cResult as STRING
    if Min(cFileName:Length, cExtension:Length)>0
        cResult := cFileName + "." + cExtension
    else
        cResult := ""
    endif
    RETURN cResult

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/forcepath/*" />
FUNCTION ForcePath( cFileName AS STRING, cPath AS STRING) AS STRING
    *-- check if path needs also check...
    IF String.IsNullOrEmpty(cFileName)
        RETURN ""
    ENDIF
    var cReturn := AddBs(cPath) + JustFName(cFileName)
    RETURN cReturn



/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justdrive/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justcommon/*" />
FUNCTION JustDrive(cPath AS STRING) AS STRING
    IF String.IsNullOrEmpty(cPath)
        RETURN ""
    ENDIF
    VAR result := System.IO.Directory.GetDirectoryRoot(cPath)
    result := result:Replace(PathHelpers.PathChar,"")
    RETURN result
    

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justext/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justcommon/*" />
FUNCTION JustExt(cPath AS STRING) AS STRING
    *-- Default for new parameter  lOptWithLeadingDot ist .f.
    *-- As returning all extensions with leading dot could lead to breaking changes
    return JustExt(cPath, .f.)
    
    
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justdrive/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justcommon/*" />
FUNCTION JustExt(cPath AS STRING, lOptWithLeadingDot AS LOGIC) AS STRING
    IF String.IsNullOrEmpty(cPath)
        RETURN ""
    ENDIF
    VAR result := Path.GetExtension(cPath)
    if lOptWithLeadingDot == .f. and result:StartsWith(".")
        result := result:Substring(1)
    endif
    RETURN result


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justfname/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justcommon/*" />
FUNCTION JustFName(cPath AS STRING) AS STRING
    IF String.IsNullOrEmpty(cPath)
        RETURN ""
    ENDIF
    VAR result := Path.GetFileName(cPath)
    RETURN result


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justpath/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justcommon/*" />
FUNCTION JustPath(cPath AS STRING) AS STRING
    IF String.IsNullOrEmpty(cPath)
        RETURN ""
    ENDIF
    LOCAL result := cPath AS STRING
    LOCAL nPos := result:LastIndexOf(PathHelpers.PathChar) as LONG
    IF  nPos >= 0
        result := result:Substring(0, nPos )
    ENDIF
    RETURN result


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/juststem/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justcommon/*" />
FUNCTION JustStem(cPath AS STRING) AS STRING
    IF String.IsNullOrEmpty(cPath)
        RETURN ""
    ENDIF
    VAR result := Path.GetFileNameWithoutExtension(cPath)
    RETURN result

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/leftc/*" />
FUNCTION LeftC( cExpression AS STRING, nExpression AS DWORD) AS STRING
    RETURN Left(cExpression, nExpression)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/lenc/*" />
FUNCTION LenC( cExpression AS STRING ) AS DWORD
    RETURN SLen(cExpression)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/likec/*" />
FUNCTION LikeC( cExpression1, cExpression2) AS LOGIC
    RETURN Like(cExpression1, cExpression2)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/rightc/*" />
FUNCTION RightC( cExpression AS STRING, nCharacters AS DWORD) AS STRING
    RETURN Right(cExpression, nCharacters)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/stuffc/*" />
FUNCTION StuffC( cExpression, nStartReplacement, nCharactersReplaced, cReplacement) AS STRING
    RETURN Stuff(cExpression, nStartReplacement, nCharactersReplaced, cReplacement)
    
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/substrc/*" />
FUNCTION SubStrC(cExpression, nStartPosition , nCharactersReturned ) AS STRING
    RETURN SubStr(cExpression, nStartPosition, nCharactersReturned)

