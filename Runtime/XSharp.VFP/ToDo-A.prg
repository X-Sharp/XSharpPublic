//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/aclass/*" />
FUNCTION AClass(ArrayName, oExpression)
    THROW NotImplementedException{}
    //RETURN 0 
     
/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/adatabases/*" />
FUNCTION ADatabases(ArrayName) 
    THROW NotImplementedException{}
    //RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/adbobjects/*" />
FUNCTION ADBObjects(ArrayName, cSetting)
    THROW NotImplementedException{}
    //RETURN 0



/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/aelement/*" />
FUNCTION Aelement (ArrayName, nRowSubscript , nColumnSubscript)
    THROW NotImplementedException{}
    //RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/agetfileversion/*" />
FUNCTION AGetFileVersion (ArrayName, cFileName)
    THROW NotImplementedException{}
    //RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/alines/*" />
FUNCTION ALines (ArrayName, cExpression , nFlags , cParseChar , cParseChar2 )
    THROW NotImplementedException{}
    //RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/amembers/*" />
FUNCTION AMembers (ArrayName, oObjectNameOrClassName , nArrayContentsID , cFlags)
    THROW NotImplementedException{}
    //RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/anetresources/*" />
FUNCTION ANetResources (ArrayName, cNetworkName, nResourceType)
    THROW NotImplementedException{}
    //RETURN 0


/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/aprinters/*" />
FUNCTION APrinters (ArrayName , nValue)
    THROW NotImplementedException{}
    //RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/asessions/*" />
FUNCTION ASessions (ArrayName)
    THROW NotImplementedException{}
    //RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/asqlhandles/*" />
FUNCTION ASqlHandles (ArrayName , nStatementHandle)
    THROW NotImplementedException{}
    //RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/astackinfo/*" />
FUNCTION AStackInfo (ArrayName )
    THROW NotImplementedException{}
    //RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/asubscript/*" />
FUNCTION ASubScript (ArrayName, nElementNumber, nSubscript )
    THROW NotImplementedException{}
    //RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/ataginfo/*" />
FUNCTION ATagInfo (ArrayName ,cCDXName , uArea )
    THROW NotImplementedException{}
    //RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/aused/*" />
FUNCTION AUsed (ArrayName , nDataSessionNumber , cTableName )
    THROW NotImplementedException{}
    //RETURN 0



/// <summary>-- todo --</summary>
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
     
/// <summary>-- todo --</summary>
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/at_c/*" />
FUNCTION At_C(cSearchExpression AS STRING, cExpressionSearched AS STRING, nOccurrence := 1 AS DWORD) AS DWORD
	RETURN At(cSearchExpression, cExpressionSearched, nOccurrence)


/// <summary>-- todo --</summary>
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

/// <summary>-- todo --</summary>
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/atcc/*" />
FUNCTION AtCC(cSearchExpression AS STRING, cExpressionSearched AS STRING, nOccurrence := 1 AS DWORD) AS DWORD
	RETURN AtC(cSearchExpression, cExpressionSearched, nOccurrence)





/*
FALSE  Boolean
0.0.0  DATE
0.0    Decimal
{}     ARRAY
""     Character
0      Numeric
*/
