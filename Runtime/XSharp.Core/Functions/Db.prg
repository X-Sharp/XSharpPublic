//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp
USING XSharp.RDD
USING XSharp.RDD.Enums
USING System.Collections.Generic
USING System.IO
USING System.Reflection
USING System.Linq
USING System.Text

FUNCTION Alias0() AS STRING
   LOCAL oRDD := RDDHelpers.CWA("Alias0") AS IRDD
    IF oRDD != NULL
        RETURN oRDD:Alias
    ENDIF                            
    RETURN String.Empty


FUNCTION BOF() AS LOGIC    
   RETURN VODBBof()

/// <summary>
/// Return the full path of the file
/// </summary>
/// <returns>
/// </returns>
FUNCTION DBF() AS STRING
    LOCAL oRDD := RDDHelpers.CWA("DBF") AS IRDD
    IF oRDD != NULL
        RETURN (STRING) oRDD:Info(DBI_FULLPATH, NULL)
    ENDIF                            
    RETURN String.Empty

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbPack() AS LOGIC STRICT
	
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBPack()
	
	IF !lRetCode
		//lRetCode := DoError(#DbPack)
	ENDIF
	
	RETURN lRetCode

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbRecall() AS LOGIC STRICT
	
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBRecall()
	
	IF !lRetCode
		//lRetCode := DoError(#DbRecall)
	ENDIF
	
	RETURN lRetCode

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbUnLock() AS LOGIC STRICT
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBUnlock(NULL_OBJECT)
	
	IF !lRetCode
		//lRetCode := DoError(#DbUnLock)
	ENDIF
	
	RETURN lRetCode

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbZap() AS LOGIC STRICT
	
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBZap()
	
	IF !lRetCode
		//lRetCode := DoError(#DbZap)
	ENDIF
	
	RETURN lRetCode


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbUnlockAll() AS LOGIC STRICT
	RETURN VODBUnlockAll()

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DELETED() AS LOGIC STRICT
	RETURN VODBDeleted()

FUNCTION EOF() AS LOGIC
   RETURN VODBEof()

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION FLOCK() AS LOGIC STRICT
	RETURN VODBFlock()


    /// <summary>
    /// Return the number of fields in the current database file.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION FCount() AS DWORD
    LOCAL oRDD := RDDHelpers.CWA("FCount") AS IRDD
    IF (oRDD != NULL)
        RETURN (DWORD) oRDD:FieldCount
    ENDIF
    RETURN 0
    
    
    /// <summary>
    /// Return the name of a field as a string.
    /// </summary>
    /// <param name="dwFieldPos"></param>
    /// <returns>
    /// </returns>
FUNCTION FieldName(dwFieldPos AS DWORD) AS STRING
    LOCAL oRDD := RDDHelpers.CWA("FieldName") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:FieldName((INT) dwFieldPos)
    ENDIF
    RETURN String.Empty   
    
    
    
    /// <summary>
    /// Return the position of a field.
    /// </summary>
    /// <param name="sFieldName"></param>
    /// <returns>
    /// </returns>
FUNCTION FieldPos(sFieldName AS STRING) AS DWORD
    LOCAL oRDD := RDDHelpers.CWA("FieldPos") AS IRDD
    IF (oRDD != NULL)
        RETURN (DWORD) oRDD:FieldIndex(sFieldName) 
    ENDIF
    RETURN 0   

/// <summary>
/// Return the position of a field.
/// </summary>
/// <param name="sFieldName"></param>
/// <param name="nArea"></param>
/// <returns>
/// </returns>
FUNCTION FieldPos(sFieldName AS STRING, nArea AS DWORD) AS DWORD
    LOCAL oRDD := RuntimeState.WorkAreas:GetRDD(nArea) AS IRDD
    IF oRDD != NULL_OBJECT
        RETURN (DWORD) oRDD:FieldIndex(sFieldName) 
    ENDIF
    RETURN 0   
    

    
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION Found() AS LOGIC 
	RETURN VODBFound()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION Header() AS LONG
    LOCAL oValue := NULL AS OBJECT
	VoDbInfo(DBI_GETHEADERSIZE, REF oValue)
    RETURN (LONG) oValue

/// <summary>
/// Return the number of records
/// </summary>
/// <returns>
/// </returns>
FUNCTION LastRec() AS DWORD
    LOCAL oRDD := RDDHelpers.CWA("LastRec") AS IRDD
    IF (oRDD != NULL)
        RETURN (DWORD) oRDD:RecCount
    ENDIF
    RETURN 0   




/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DBBUFFREFRESH  () AS LOGIC STRICT
	
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBBuffRefresh()
	
	IF !lRetCode
		//lRetCode := DoError(#DBBUFFREFRESH)
	ENDIF
	
	RETURN lRetCode

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DBCLEARFILTER  () AS LOGIC STRICT
	
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBClearFilter()
	
	IF !lRetCode
		//lRetCode := DoError(#DBCLEARFILTER)
	ENDIF
	
	RETURN lRetCode


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DBCLEARRELATION() AS LOGIC STRICT
	RETURN VODBClearRelation()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DBCLOSEALL     () AS LOGIC STRICT
	RETURN VODBCloseAll()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DBCLOSEAREA    () AS LOGIC STRICT
	RETURN VODBCloseArea()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DBCOMMIT       () AS LOGIC STRICT
	RETURN VODBCommit()

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DBCOMMITALL    () AS LOGIC STRICT
	RETURN VODBCommitAll()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbContinue() AS LOGIC STRICT
	LOCAL lRetCode AS LOGIC
	lRetCode := VODBContinue()
	IF !lRetCode
		//lRetCode := DoError(#DbContinue)
	ENDIF
	
	RETURN lRetCode



/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DBDRIVER() AS STRING STRICT
	RETURN RDDNAME()



/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbFIlter() AS STRING STRICT
	RETURN VODBFilter()

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbGetSelect() AS DWORD STRICT
	RETURN VODBGetSelect()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbGoBottom() AS LOGIC STRICT
	LOCAL lRetCode  AS LOGIC
	lRetCode := VODBGoBottom()
	IF !lRetCode
		//lRetCode := DoError(#DbGoBottom)
	ENDIF
	
	RETURN lRetCode



/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbGotop() AS LOGIC STRICT
	
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBGoTop()
	
	IF !lRetCode
		//lRetCode := DoError(#DbGotop)
	ENDIF
	
	RETURN lRetCode


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION RDDName        () AS STRING STRICT
	LOCAL cRet      AS STRING
	
	IF Used()
		cRet := VODBRddName()
	ELSE
		cRet := RddSetDefault()
	ENDIF
	
	RETURN cRet



FUNCTION RddSetDefault  () AS STRING STRICT
    RETURN RuntimeState.DefaultRDD
/// <summary>
/// </summary>
/// <returns>
/// </returns>

FUNCTION RddSetDefault  (cDriver AS STRING) AS STRING
	LOCAL cOld := RuntimeState.DefaultRDD AS STRING
    RuntimeState.DefaultRDD := cOld
	RETURN cOld


/// <summary>
/// Return the number of records
/// </summary>
/// <returns>
/// </returns>
FUNCTION RecCount() AS LONG
    RETURN VODBLastRec()



FUNCTION RecSize AS LONG
    LOCAL nSize := NULL AS OBJECT
    VODbInfo(DBInfo.DBI_GETRECSIZE, REF nSize)
    RETURN (LONG) nSize

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION RLock() AS LOGIC STRICT
	RETURN VODBRlock(NULL)


/// <summary>
/// Determine whether a database file is open.
/// </summary>
/// <returns>
/// </returns>
FUNCTION Used() AS LOGIC
    RETURN RuntimeState.Workareas:CurrentWorkArea != NULL
