//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text


INTERNAL FUNCTION _DoInArea<T>(uArea as Usual, action as @@Func<T>, defaultValue as T, cFunction as STRING, nArg as DWORD) as T
    IF IsNil(uArea)
        RETURN (T) (OBJECT) IIF(Used(), action(), defaultValue)
    ELSEIF IsNumeric(uArea)
        RETURN (T) (OBJECT) (uArea)->(IIF(Used(), action(), defaultValue)) 
    ENDIF 
    VAR curArea := RuntimeState.CurrentWorkarea 
    VAR newArea := VoDb.SymSelect(uArea) 
    RuntimeState.CurrentWorkarea := curArea 
    IF newArea == 0 
        THROW Error.VoDbError( EG_ARG, EDB_BADALIAS, cFunction, nameof(uArea), nArg, <OBJECT>{uArea}  )
    ENDIF 
    RETURN (T) (Object) (newArea)->(action()) 

// The last 2 params in the function calls below determine the error message generated when the uArea parameter
// is an non existing alias
// The number should match the position of the uArea parameter in the parameter list of the original parameters list
//


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/cdx/*" />
FUNCTION  Cdx (nIndexNumber , uArea)  AS USUAL CLIPPER
    RETURN _DoInArea(uArea, { => (String) DbOrderInfo(DBOI_INDEXNAME, NIL, nIndexNumber) } , "",__FUNCTION__,2)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/cpdbf/*" />
FUNCTION CpDbf( uArea) AS LONG CLIPPER
    RETURN _DoInArea(uArea, { => (LONG) DbInfo(DBI_CODEPAGE) } , 0,__FUNCTION__,1)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/descending/*" />
FUNCTION Descending( uIndex, uArea) AS USUAL CLIPPER
    RETURN _DoInArea(uArea, { => (LOGIC) DbOrderInfo(DBOI_ISDESC, NIL, uIndex) } , FALSE,__FUNCTION__,2)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/field/*" />
FUNCTION Field( uField , uArea, nFlag) AS STRING CLIPPER
    LOCAL nInfo as LONG
    @@Default(@nFlag, 0)
    IF nFlag == 1
        nInfo := DBS_CAPTION
    ELSE
        nInfo := DBS_NAME
    ENDIF        
    IF IsString(uField)
        RETURN _DoInArea(uArea, { => (STRING) DbFieldInfo(nInfo, uField) } , "",__FUNCTION__,2)
    ELSEIF IsNumeric(uField)
        RETURN _DoInArea(uArea, { => (STRING) DbFieldInfo(nInfo, uField) } , "",__FUNCTION__,2)
    ELSE
        RETURN ""
    ENDIF

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/filter/*" />
FUNCTION Filter( uArea ) AS USUAL CLIPPER
    RETURN _DoInArea(uArea, { => (STRING) DbInfo(DBI_DBFILTER) } , "",__FUNCTION__,1)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/fldcount/*" />
FUNCTION FldCount( ) AS USUAL CLIPPER
    RETURN FCount()



/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/for/*" />
FUNCTION For( nIndexNumber , uArea) AS STRING CLIPPER
    RETURN _DoInArea(uArea, { => (String) DbOrderInfo(DBOI_CONDITION, NIL, nIndexNumber) } , "",__FUNCTION__,2)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/idxcollate/*" />
FUNCTION IdxCollate( uIndex, nIndex, uArea) AS STRING CLIPPER
    RETURN _DoInArea(uArea, { => (String) DbOrderInfo(DBOI_COLLATION, uIndex, nIndex) } , "",__FUNCTION__,3)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/isflocked/*" />
FUNCTION IsFlocked( uArea ) AS LOGIC CLIPPER
    RETURN _DoInArea(uArea, { => (LOGIC) DbInfo(DBI_ISFLOCK) } , FALSE,__FUNCTION__,1)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/isreadonly/*" />
FUNCTION IsReadOnly( uArea ) AS LOGIC CLIPPER
    RETURN _DoInArea(uArea, { => (LOGIC) DbInfo(DBI_READONLY) } , FALSE,__FUNCTION__,1)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/isrlocked/*" />
FUNCTION IsRlocked( nRecordNumber, uArea ) AS LOGIC CLIPPER
    RETURN _DoInArea(uArea, { => (LOGIC) DbRecordInfo(DBRI_LOCKED, nRecordNumber) } , FALSE,__FUNCTION__,2)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/key/*" />
FUNCTION Key( uIndex, uArea) AS STRING CLIPPER
    RETURN _DoInArea(uArea, { => (String) DbOrderInfo(DBOI_EXPRESSION,NIL ,uIndex) } , "",__FUNCTION__,2)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/mdx/*" />
FUNCTION Mdx( nIndexNumber , uArea) AS STRING CLIPPER
    RETURN _DoInArea(uArea, { => (String) DbOrderInfo(DBOI_INDEXNAME,NIL ,nIndexNumber) } , "",__FUNCTION__,2)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ndx/*" />
FUNCTION Ndx( nIndexNumber , uArea ) AS STRING CLIPPER
    RETURN _DoInArea(uArea, { => (String) DbOrderInfo(DBOI_INDEXNAME,NIL ,nIndexNumber) } , "",__FUNCTION__,2)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/order/*" />
FUNCTION Order( uArea, nPath) AS STRING CLIPPER
    IF IsNumeric(nPath)
        RETURN _DoInArea(uArea, { => (String) DbOrderInfo(DBOI_FULLPATH, 0) } , "",__FUNCTION__,1)
    ENDIF
    RETURN _DoInArea(uArea, { => (STRING) DbOrderInfo(DBOI_NAME, 0) } , "",__FUNCTION__,1)
    

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/relation/*" />
FUNCTION Relation( nRelationNumber , uArea) AS STRING CLIPPER
    RETURN _DoInArea(uArea, { =>  DbRelation(nRelationNumber) } , "",__FUNCTION__,2)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/tag/*" />
FUNCTION Tag( CDXFileName, nTagNumber, uArea) AS STRING CLIPPER
    RETURN _DoInArea(uArea, { => (String) DbOrderInfo(DBOI_NAME, CDXFileName, nTagNumber) } , "",__FUNCTION__,3)
    

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/tagcount/*" />
FUNCTION TagCount( CDXFileName , uArea) AS LONG CLIPPER
    RETURN _DoInArea(uArea, { => (LONG) DbOrderInfo(DBOI_ORDERCOUNT, CDXFileName) } , 0,__FUNCTION__,2)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/bagcount/*" />
FUNCTION BagCount( uArea) AS LONG CLIPPER
    RETURN _DoInArea(uArea, { => (LONG) DbOrderInfo(DBOI_BAGCOUNT) } , 0,__FUNCTION__,1)



/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/tagno/*" />
FUNCTION TagNo( IndexName , CDXFileName , uArea ) AS LONG CLIPPER
    RETURN _DoInArea(uArea, { => (LONG) DbOrderInfo(DBOI_NUMBER, CDXFileName, IndexName) } , 0,__FUNCTION__,3)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/target/*" />
FUNCTION Target( nRelationshipNumber , uArea ) AS STRING CLIPPER
    RETURN _DoInArea(uArea, { =>
        LOCAL nArea AS DWORD
        nArea := DbRSelect(nRelationshipNumber)
        IF nArea != 0
            RETURN Alias(nArea)
        ENDIF
        RETURN ""
        } , "",__FUNCTION__,2)




/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/unique/*" />
FUNCTION Unique(uArea ) AS LOGIC CLIPPER
    RETURN _DoInArea(uArea, { => (LOGIC) DbOrderInfo(DBOI_UNIQUE , NIL, NIL) } , FALSE,__FUNCTION__,1)
