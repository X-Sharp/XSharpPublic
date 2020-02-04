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
#ifdef NOTREADY

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/at/*" />
FUNCTION At(cSearchExpression AS STRING, cExpressionSearched AS STRING, nOccurrence := 1 AS DWORD) AS DWORD
	LOCAL position := 0 AS DWORD
	IF ( cExpressionSearched != NULL .AND. cSearchExpression != NULL )
		IF cExpressionSearched:Length != 0 .AND. cSearchExpression:Length != 0
            DO WHILE nOccurrence  > 0
			    position := (DWORD) cExpressionSearched:IndexOf(cSearchExpression, (INT) position,StringComparison.Ordinal)
                nOccurrence -= 1
            ENDDO
		END IF
	ENDIF
	RETURN position
     
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/at_c/*" />
FUNCTION At_C(cSearchExpression AS STRING, cExpressionSearched AS STRING, nOccurrence := 1 AS DWORD) AS DWORD
	RETURN At(cSearchExpression, cExpressionSearched, nOccurrence)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/atc/*" />
FUNCTION AtC(cSearchExpression AS STRING, cExpressionSearched AS STRING, nOccurrence := 1 AS DWORD) AS DWORD
	LOCAL position := 0 AS DWORD
	IF ( cExpressionSearched != NULL .AND. cSearchExpression != NULL )
		IF cExpressionSearched:Length != 0 .AND. cSearchExpression:Length != 0
            DO WHILE nOccurrence  > 0
			    position := (DWORD) cExpressionSearched:IndexOf(cSearchExpression, (INT) position,StringComparison.OrdinalIgnoreCase)
                nOccurrence -= 1
            ENDDO
		END IF
	ENDIF
	RETURN position

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/atcc/*" />
FUNCTION AtCC(cSearchExpression AS STRING, cExpressionSearched AS STRING, nOccurrence := 1 AS DWORD) AS DWORD
	RETURN AtC(cSearchExpression, cExpressionSearched, nOccurrence)
#endif
