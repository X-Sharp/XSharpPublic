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
/// <seealso cref='O:XSharp.VFP.Functions.DefaultExt' />
/// <seealso cref='O:XSharp.VFP.Functions.JustDrive' />
/// <seealso cref='O:XSharp.VFP.Functions.JustExt' />
/// <seealso cref='O:XSharp.VFP.Functions.JustFName' />
/// <seealso cref='O:XSharp.VFP.Functions.JustPath' />
/// <seealso cref='O:XSharp.VFP.Functions.JustStem' />
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
    IF tlOptAsVfp9 == .f. 
        RETURN ForceExt( cFileName, cExtension)
    ENDIF
    cFileName := JustFName(cFileName)
    IF cFileName:EndsWith(".")
        *-- if filename ends with dot, cut that
        *-- but only rightmost one, ending in several dots cuts still only 1
        cFileName := cFileName:Substring(0 , cFileName:Length-1)
    ENDIF
    IF cExtension:StartsWith(".")
        cExtension := cExtension:Substring(1)
    ENDIF
    LOCAL cResult AS STRING
    IF Min(cFileName:Length, cExtension:Length)>0
        cResult := cFileName + "." + cExtension
    ELSE
        cResult := ""
    ENDIF
    RETURN cResult

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/forcepath/*" />
FUNCTION ForcePath( cFileName AS STRING, cPath AS STRING) AS STRING
    *-- check if path needs also check...
    IF String.IsNullOrEmpty(cFileName)
        RETURN ""
    ENDIF
    VAR cReturn := AddBs(cPath) + JustFName(cFileName)
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
    RETURN JustExt(cPath, .f.)
    
    
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justdrive/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justcommon/*" />
FUNCTION JustExt(cPath AS STRING, lOptWithLeadingDot AS LOGIC) AS STRING
    IF String.IsNullOrEmpty(cPath)
        RETURN ""
    ENDIF
    VAR result := Path.GetExtension(cPath)
    IF lOptWithLeadingDot == .f. AND result:StartsWith(".")
        result := result:Substring(1)
    ENDIF
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
    LOCAL nPos := result:LastIndexOf(PathHelpers.PathChar) AS LONG
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





/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ratc/*" />
FUNCTION RAtC(cSearchExpression AS STRING , cExpressionSearched AS STRING , nOccurrence := 1 AS INT ) AS INT	
	RETURN RAt(cSearchExpression , cExpressionSearched , nOccurrence )


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/rat/*" />
FUNCTION RAt(cSearchExpression AS STRING , cExpressionSearched AS STRING , nOccurrence := 1 AS INT ) AS INT
    LOCAL iPosition  AS INT
    LOCAL i , dwPosition, dwOccurred AS INT
    dwOccurred := 0
 	dwPosition := 0
	IF ! String.IsNullOrEmpty(cExpressionSearched) .AND. ! String.IsNullOrEmpty(cSearchExpression) 		
	
		iPosition := cExpressionSearched:Length 

		FOR i := 1 UPTO nOccurrence
				
     		IF ( iPosition := cExpressionSearched:LastIndexOf(cSearchExpression,iPosition,StringComparison.Ordinal) ) == -1 
				EXIT 
			ENDIF

			dwOccurred++ 
				

			IF dwOccurred == nOccurrence
				// Assign the found position before leaving the loop. 
				dwPosition := iPosition + 1
				EXIT 
			ENDIF
					
			//  Doing always a   
			//    
			//  iPosition--
			//    
			//	, like the c# sources do, is not correct. 
			//  That's only necessary if the len of the search string 	
			//  is 1.    

			IF cSearchExpression:Length == 1 .AND. --iPosition < 0 
				
				// something like:  
				//
				// RAt("a","abracadabra", 12 )   
				//
				// ends up here. If you do not check the iPosition value 
				// the next search loop would cause a exception.
				//
						 
				EXIT
			ENDIF
		NEXT 
	ENDIF 
	RETURN dwPosition		
							
	

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/chrtranc/*" />
FUNCTION ChrTranC( cSearchIn AS STRING , cSearchFor AS STRING, cReplaceWith  AS STRING ) AS STRING 
	RETURN ChrTran( cSearchIn , cSearchFor , cReplaceWith )  

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/chrtran/*" />
FUNCTION ChrTran( cSearchIn AS STRING , cSearchFor AS STRING, cReplaceWith  AS STRING ) AS STRING 
    LOCAL cRetVal ,cReplaceChar AS STRING
    LOCAL i AS INT  
	cRetVal := "" 
   
	IF  cSearchIn != NULL  .AND. cSearchFor != NULL .AND. cReplaceWith != NULL	   

		cRetVal := cSearchIn 
	
			
		FOR i := 1 UPTO cSearchFor:Length

			IF cReplaceWith:Length <= i-1
				cReplaceChar := ""
			ELSE
				cReplaceChar := cReplaceWith[i-1]:ToString()
			ENDIF 
			
		    cRetVal := cRetVal:Replace(cSearchFor[i-1]:ToString(), cReplaceChar )
	//	    cRetVal := StrTran(cRetVal , cSearchFor[i-1]:ToString(), cReplaceChar , , SLen ( cRetVal ) )

		NEXT           

	ENDIF						
	
	RETURN cRetVal

	


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/at/*" />
FUNCTION At(cSearchExpression AS STRING, cExpressionSearched AS STRING, nOccurrence := 1 AS DWORD) AS DWORD
	LOCAL position := 0 AS DWORD
	IF ( cExpressionSearched != NULL .AND. cSearchExpression != NULL )
		IF cExpressionSearched:Length != 0 .AND. cSearchExpression:Length != 0
            DO WHILE nOccurrence  > 0
                IF ( position := (DWORD) cExpressionSearched:IndexOf(cSearchExpression, (INT) position,StringComparison.Ordinal) + 1) == 0
                    EXIT
                ENDIF	
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
                IF ( position := (DWORD) cExpressionSearched:IndexOf(cSearchExpression, (INT) position,StringComparison.OrdinalIgnoreCase) + 1) == 0
                    EXIT
                ENDIF	
                nOccurrence -= 1
            ENDDO
		END IF
	ENDIF
	RETURN position

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/atcc/*" />
FUNCTION AtCC(cSearchExpression AS STRING, cExpressionSearched AS STRING, nOccurrence := 1 AS DWORD) AS DWORD
	RETURN AtC(cSearchExpression, cExpressionSearched, nOccurrence)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/alltrim/*" />
FUNCTION AllTrim(Expression AS STRING, Flags AS INT, TrimChars PARAMS STRING[]) AS STRING STRICT
	RETURN Trim_helper(.T., .T., Expression, Flags, TrimChars)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ltrim/*" />
FUNCTION LTrim(Expression AS STRING, Flags AS INT, TrimChars PARAMS STRING[]) AS STRING STRICT
	RETURN Trim_helper(.T., .F., Expression, Flags, TrimChars)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/rtrim/*" />
FUNCTION RTrim(Expression AS STRING, Flags AS INT, TrimChars PARAMS STRING[]) AS STRING STRICT
	RETURN Trim_helper(.F., .T., Expression, Flags, TrimChars)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/trim/*" />
FUNCTION Trim(Expression AS STRING, Flags AS INT, TrimChars PARAMS STRING[]) AS STRING STRICT
	RETURN Trim_helper(.F., .T., Expression, Flags, TrimChars)

STATIC FUNCTION Trim_helper(TrimLeft AS Boolean, TrimRight AS Boolean, Expression AS STRING, Flags AS INT, TrimChars PARAMS STRING[]) AS STRING STRICT

    LOCAL parmNdx AS INT
    LOCAL Trimmed = .T. AS Boolean
    LOCAL LRTrimmed AS INT
    LOCAL comparison = StringComparison.Ordinal AS System.StringComparison
    LOCAL compared AS STRING

    IF Expression = NULL
        RETURN Expression
    END IF

    IF Flags = 1
         comparison = StringComparison.OrdinalIgnoreCase
    END IF

    DO WHILE Trimmed

        Trimmed = .F.

        FOR parmNdx = 1 TO TrimChars:Length
 
            compared = TrimChars[parmNdx]

            IF TrimLeft
                LRTrimmed = 0
 
                DO WHILE String.Compare(Expression, LRTrimmed, compared, 0, compared:Length, comparison) = 0
                    LRTrimmed += compared:Length
                END DO
                IF LRTrimmed > 0
                    Expression = Expression:Substring(LRTrimmed)
                    Trimmed = .T.
                END IF
            END IF

            IF TrimRight
                LRTrimmed = Expression:Length - compared:Length

                DO WHILE LRTrimmed >= 0 AND String.Compare(Expression, LRTrimmed, compared, 0, compared:Length, comparison) = 0
                    LRTrimmed -= compared:Length
                END DO
                IF LRTrimmed < (Expression:Length - compared:Length)
                    Expression = Expression:Substring(0, LRTrimmed + compared:Length)
                    Trimmed = .T.
                END IF
            END IF

        NEXT
 
    END DO

    RETURN Expression

END FUNCTION

