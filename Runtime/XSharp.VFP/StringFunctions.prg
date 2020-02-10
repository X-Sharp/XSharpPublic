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


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/addbs/*" />
/// <seealso cref='M:XSharp.VFP.Functions.DefaultExt(System.String)' />
/// <seealso cref='M:XSharp.VFP.Functions.JustDrive(System.String)' />
/// <seealso cref='M:XSharp.VFP.Functions.JustExt(System.String)' />
/// <seealso cref='M:XSharp.VFP.Functions.JustFName(System.String)' />
/// <seealso cref='M:XSharp.VFP.Functions.JustPath(System.String)' />
/// <seealso cref='M:XSharp.VFP.Functions.JustStem(System.String)' />
FUNCTION AddBs (cPath AS STRING)
    IF String.IsNullOrEmpty(cPath)
        RETURN ""
    ENDIF
    VAR delim := Path.DirectorySeparatorChar:ToString()
    cPath := cPath:TrimEnd()
    IF ! cPath.EndsWith(delim)
        cPath += delim
    ENDIF
    RETURN cPath



/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justdrive/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justcommon/*" />
FUNCTION JustDrive(cPath AS STRING)
    IF String.IsNullOrEmpty(cPath)
        RETURN ""
    ENDIF
    VAR result := System.IO.Directory.GetDirectoryRoot(cPath)
    result := result:Replace(Path.DirectorySeparatorChar:ToString(),"")
    RETURN result
    

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justdrive/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justcommon/*" />
FUNCTION JustExt(cPath AS STRING)
    IF String.IsNullOrEmpty(cPath)
        RETURN ""
    ENDIF
    VAR result := Path.GetExtension(cPath)
    RETURN result


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justfname/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justcommon/*" />
FUNCTION JustFName(cPath AS STRING)
    IF String.IsNullOrEmpty(cPath)
        RETURN ""
    ENDIF
    VAR result := Path.GetFileName(cPath)
    RETURN result


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justpath/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justcommon/*" />
FUNCTION JustPath(cPath AS STRING)
    IF String.IsNullOrEmpty(cPath)
        RETURN ""
    ENDIF
    LOCAL cPathChar := Path.DirectorySeparatorChar:ToString() AS STRING
    LOCAL result := cPath AS STRING
    IF result:IndexOf(cPathChar) >= 0
        result := result:Substring(0, result:LastIndexOf(cPathChar) -1)
    ENDIF
    RETURN result


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/juststem/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justcommon/*" />
FUNCTION JustStem(cPath AS STRING)
    IF String.IsNullOrEmpty(cPath)
        RETURN ""
    ENDIF
    VAR result := Path.GetFileNameWithoutExtension(cPath)
    RETURN result

/// <include file="Runtimefunctions.xml" path="Runtimefunctions/leftc/*" />
FUNCTION LeftC( cExpression AS STRING, nExpression AS DWORD) AS STRING
    RETURN Left(cExpression, nExpression)

/// <include file="Runtimefunctions.xml" path="Runtimefunctions/lenc/*" />
FUNCTION LenC( cExpression AS STRING ) AS DWORD
    RETURN SLen(cExpression)

/// <include file="Runtimefunctions.xml" path="Runtimefunctions/likec/*" />
FUNCTION LikeC( cExpression1, cExpression2) AS LOGIC
    RETURN Like(cExpression1, cExpression2)


/// <include file="Runtimefunctions.xml" path="Runtimefunctions/rightc/*" />
FUNCTION RightC( cExpression AS STRING, nCharacters AS DWORD) AS STRING
    RETURN Right(cExpression, nCharacters)

/// <include file="Runtimefunctions.xml" path="Runtimefunctions/stuffc/*" />
FUNCTION StuffC( cExpression, nStartReplacement, nCharactersReplaced, cReplacement) AS STRING
    RETURN Stuff(cExpression, nStartReplacement, nCharactersReplaced, cReplacement)
    
/// <include file="Runtimefunctions.xml" path="Runtimefunctions/substrc/*" />
FUNCTION SubStrC(cExpression, nStartPosition , nCharactersReturned )
    RETURN SubStr(cExpression, nStartPosition, nCharactersReturned)

