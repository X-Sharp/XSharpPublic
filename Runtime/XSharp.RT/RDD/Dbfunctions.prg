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
FUNCTION _DbThrowErrorOnFailure(funcName AS STRING, resultToCheck AS LOGIC) AS LOGIC
    IF !resultToCheck
        resultToCheck := (LOGIC) DoError(funcName)
    ENDIF
    RETURN resultToCheck


FUNCTION Alias0() AS STRING
   LOCAL oRDD := VoDb.CWA("Alias0") AS IRDD
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
    /// <remarks>BOF() is the same as CoreDbBOF().</remarks>
FUNCTION Bof() AS LOGIC    
   RETURN VoDb.Bof()

/// <summary>
/// Return the full path of the file
/// </summary>
/// <returns>
/// </returns>
FUNCTION DBF() AS STRING
    RETURN VoDb.DBF()
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbPack() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Pack())

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbRecall() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Recall())

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbUnLock() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Unlock(NULL_OBJECT))

/// <summary>Remove all records from the current workarea./// </summary>
/// <returns>TRUE if successful; otherwise, FALSE./// </returns>
FUNCTION DbZap() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Zap())


/// <summary>Release all locks for all work areas.</summary>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
/// <remarks>DBUnlockAll() releases any record or file locks obtained by the current process for any work area.
/// DBUnlockAll() is only meaningful on a shared database.  It is equivalent to calling DBUnlock() on every occupied work area.</remarks>
FUNCTION DbUnlockAll() AS LOGIC STRICT
	RETURN VoDb.UnlockAll()

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION Deleted() AS LOGIC STRICT
	RETURN VoDb.Deleted()

     /// <summary>
    /// Determine when end-of-file is encountered.
    /// </summary>
    /// <returns>TRUE when an attempt is made to move the record pointer beyond the last logical record in a
    /// database file or if the current database file contains no records; otherwise, FALSE.  If there is no
    /// database file open in the current work area, EOF() returns TRUE.</returns>
    /// <remarks>EOF() is the same as CoreDbEOF().</remarks>
FUNCTION Eof() AS LOGIC
   RETURN VoDb.Eof()

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION Flock() AS LOGIC STRICT
	RETURN VoDb.Flock()


    /// <summary>
    /// Return the number of fields in the current database file.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION FCount() AS DWORD
    RETURN VoDb.FCount()    
    
    /// <summary>
    /// Return the name of a field as a string.
    /// </summary>
    /// <param name="dwFieldPos">The position of the field in the database file structure.</param>
    /// <returns>The name of the specified field as a symbol.  If dwFieldPos does not correspond to an existing field in a database file
    /// or if no database file is open, FieldName() return an empty string.
    /// </returns>
FUNCTION FieldName(dwFieldPos AS DWORD) AS STRING
    IF dwFieldPos > 0 .AND. dwFieldPos <= VoDb.FCount() 
        RETURN VoDb.FieldName(dwFieldPos)
    ENDIF
    RETURN String.Empty
    

    /// <summary>
    /// Return the name of a field as a symbol.
    /// </summary>
    /// <param name="dwFieldPos">The position of the field in the database file structure.</param>
    /// <returns>The name of the specified field as a symbol.  If dwFieldPos does not correspond to an existing field in a database file
    /// or if no database file is open, FieldSym() returns NULL_SYMBOL.
    /// </returns>
FUNCTION FieldSym(dwFieldPos AS DWORD) AS SYMBOL
    IF dwFieldPos > 0 .AND. dwFieldPos <= VoDb.FCount() 
        RETURN (SYMBOL) VoDb.FieldName(dwFieldPos)
    ENDIF
    RETURN NULL_SYMBOL


    /// <summary>
    /// Return the position of a field.
    /// </summary>
    /// <param name="sFieldName"></param>
    /// <returns>
    /// </returns>
FUNCTION FieldPos(sFieldName AS STRING) AS DWORD
    RETURN VoDb.FieldPos(sFieldName)
    
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
	RETURN VoDb.Found()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION Header() AS LONG
    LOCAL oValue := NULL AS OBJECT
	VoDb.Info(DBI_GETHEADERSIZE, REF oValue)
    RETURN (LONG) oValue

/// <summary>
/// Return the number of records
/// </summary>
/// <returns>
/// </returns>
FUNCTION LastRec() AS DWORD
    RETURN (DWORD) VoDb.LastRec()

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbBuffRefresh() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.BuffRefresh())

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbClearFilter() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.ClearFilter())

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbClearRelation() AS LOGIC STRICT
	RETURN VoDb.ClearRelation()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbCloseAll() AS LOGIC STRICT
	RETURN VoDb.CloseAll()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbCloseArea () AS LOGIC STRICT
	RETURN VoDb.CloseArea()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbCommit() AS LOGIC STRICT
	RETURN VoDb.Commit()

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbCommitAll() AS LOGIC STRICT
	RETURN VoDb.CommitAll()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbContinue() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Continue())

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbDriver() AS STRING STRICT
	RETURN RddName()



/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbFilter() AS STRING STRICT
	RETURN VoDb.Filter()

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbGetSelect() AS DWORD STRICT
	RETURN VoDb.GetSelect()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbGoBottom() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.GoBottom())


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbGoTop() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.GoTop())

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION RddName        () AS STRING STRICT
	LOCAL cRet      AS STRING
	IF Used()
		cRet := VoDb.RddName()
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
    RETURN VoDb.LastRec()


/// <summary>
/// Return the number of records
/// </summary>
/// <returns>
/// </returns>
FUNCTION RecNo() AS DWORD
    RETURN VoDb.Recno()


FUNCTION RecSize AS LONG
    RETURN VoDb.RecSize()

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION RLock() AS LOGIC STRICT
	RETURN VoDb.Rlock(NULL)

/// <summary>
/// Determine whether a database file is open.
/// </summary>
/// <returns>
/// </returns>
FUNCTION Used() AS LOGIC
    RETURN RuntimeState.Workareas:CurrentWorkArea != NULL


FUNCTION DoError (cSymFunc AS STRING, nTries:= 0 AS INT) AS OBJECT
	LOCAL oError    AS Error
    LOCAL bBlock    AS ICodeblock
    IF RuntimeState.LastRDDError IS Error
	    oError         := (Error) RuntimeState.LastRDDError
    ELSEIF RuntimeState.LastRDDError != NULL_OBJECT
        oError         := Error{RuntimeState.LastRDDError}
    ELSE
        oError  := Error{"Unknown Error occurred" }
    ENDIF
	oError:FuncSym := cSymFunc
    oError:Tries   := nTries
    bBlock := XSharp.RuntimeState.GetValue<ICodeblock>(Set.ErrorBlock)
    IF bBlock != NULL
	    RETURN bBlock:EvalBlock(oError)
    ENDIF
    THROW oError
