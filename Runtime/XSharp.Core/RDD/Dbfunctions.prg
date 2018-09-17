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

/// <exclude />
FUNCTION _DbCallWithError(funcName AS STRING, resultToCheck AS LOGIC) AS LOGIC
    IF !resultToCheck
        resultToCheck := (LOGIC) DoError(funcName)
    ENDIF
    RETURN resultToCheck


FUNCTION Alias0() AS STRING
   LOCAL oRDD := RDDHelpers.CWA("Alias0") AS IRDD
    IF oRDD != NULL
        RETURN oRDD:Alias
    ENDIF                            
    RETURN String.Empty

    /// <summary>
    /// Determine when beginning-of-file is encountered.
    /// </summary>
    /// <returns>TRUE after an attempt to skip backward beyond the first logical record in a database file or if
    /// the current database file contains no records; otherwise, FALSE.  If there is no database file open in the
    /// current work area, BOF() returns TRUE.</returns>
    /// <remarks>BOF() is the same as VODBBOF().</remarks>
FUNCTION Bof() AS LOGIC    
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
	RETURN _DbCallWithError("DbPack", VODBPack())

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbRecall() AS LOGIC STRICT
	RETURN _DbCallWithError("DbRecall", VODbRecall())

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbUnLock() AS LOGIC STRICT
	RETURN _DbCallWithError("DbUnLock", VODBUnlock(NULL_OBJECT))

/// <summary>Remove all records from the current workarea./// </summary>
/// <returns>TRUE if successful; otherwise, FALSE./// </returns>
FUNCTION DbZap() AS LOGIC STRICT
	RETURN _DbCallWithError("DbZap", VODBZap())


/// <summary>Release all locks for all work areas.</summary>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
/// <remarks>DBUnlockAll() releases any record or file locks obtained by the current process for any work area.
/// DBUnlockAll() is only meaningful on a shared database.  It is equivalent to calling DBUnlock() on every occupied work area.</remarks>
FUNCTION DbUnlockAll() AS LOGIC STRICT
	RETURN VODBUnlockAll()

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION Deleted() AS LOGIC STRICT
	RETURN VODBDeleted()

     /// <summary>
    /// Determine when end-of-file is encountered.
    /// </summary>
    /// <returns>TRUE when an attempt is made to move the record pointer beyond the last logical record in a
    /// database file or if the current database file contains no records; otherwise, FALSE.  If there is no
    /// database file open in the current work area, EOF() returns TRUE.</returns>
    /// <remarks>EOF() is the same as VODBEOF().</remarks>
FUNCTION Eof() AS LOGIC
   RETURN VODBEof()

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION Flock() AS LOGIC STRICT
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
FUNCTION DBBuffRefresh() AS LOGIC STRICT
	RETURN _DbCallWithError("DBBuffRefresh", VODBBuffRefresh())

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DBClearFilter() AS LOGIC STRICT
	RETURN _DbCallWithError("DBClearFilter", VODBClearFilter())

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DBClearRelation() AS LOGIC STRICT
	RETURN VODBClearRelation()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DBCloseAll() AS LOGIC STRICT
	RETURN VODBCloseAll()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DBCloseArea () AS LOGIC STRICT
	RETURN VODBCloseArea()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DBCommit() AS LOGIC STRICT
	RETURN VODBCommit()

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DBCommitAll() AS LOGIC STRICT
	RETURN VODBCommitAll()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbContinue() AS LOGIC STRICT
	RETURN _DbCallWithError("DbContinue", VODbContinue())

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbDriver() AS STRING STRICT
	RETURN RDDNAME()



/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbFilter() AS STRING STRICT
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
	RETURN _DbCallWithError("DbGoBottom", VODbGoBottom())


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbGoTop() AS LOGIC STRICT
	RETURN _DbCallWithError("DbGoTop", VODbGoTop())

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


FUNCTION DoError (cSymFunc AS STRING, nTries:= 0 AS INT) AS OBJECT
	LOCAL oError    AS Error
    LOCAL bBlock    AS ICodeBlock
	oError := Error{RuntimeState.LastRDDError}
	oError:FuncSym := cSymFunc
    oError:Tries   := nTries
    bBlock := XSharp.RuntimeState.GetValue<ICodeBlock>(Set.ErrorBlock)
	RETURN bBlock:EvalBlock(oError)
