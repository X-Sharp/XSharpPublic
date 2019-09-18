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

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/alias0/*" />
FUNCTION Alias0() AS STRING
   LOCAL oRDD := VoDb.CWA("Alias0", FALSE) AS IRDD
    IF oRDD != NULL
        RETURN oRDD:Alias
    ENDIF                            
    RETURN String.Empty

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/bof/*" />
FUNCTION Bof() AS LOGIC    
   RETURN VoDb.Bof()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbf/*" />
FUNCTION DBF() AS STRING
    RETURN VoDb.DBF()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbpack/*" />
FUNCTION DbPack() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Pack())

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbrecall/*" />
FUNCTION DbRecall() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Recall())

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbunlock/*" />
FUNCTION DbUnLock() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Unlock(NULL_OBJECT))

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbzap/*" />
FUNCTION DbZap() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Zap())


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbunlockall/*" />
FUNCTION DbUnlockAll() AS LOGIC STRICT
	RETURN VoDb.UnlockAll()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/deleted/*" />
FUNCTION Deleted() AS LOGIC STRICT
	RETURN VoDb.Deleted()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/eof/*" />
FUNCTION Eof() AS LOGIC
   RETURN VoDb.Eof()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/flock/*" />
FUNCTION Flock() AS LOGIC STRICT
	RETURN VoDb.Flock()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fcount/*" />
FUNCTION FCount() AS DWORD
    RETURN VoDb.FCount()    
    

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldname/*" />
FUNCTION FieldName(dwFieldPos AS DWORD) AS STRING
    IF dwFieldPos > 0 .AND. dwFieldPos <= VoDb.FCount() 
        RETURN VoDb.FieldName(dwFieldPos)
    ENDIF
    RETURN String.Empty
    

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldsym/*" />
FUNCTION FieldSym(dwFieldPos AS DWORD) AS SYMBOL
    IF dwFieldPos > 0 .AND. dwFieldPos <= VoDb.FCount() 
        RETURN (SYMBOL) VoDb.FieldName(dwFieldPos)
    ENDIF
    RETURN NULL_SYMBOL


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldpos/*" />
FUNCTION FieldPos(cFieldName AS STRING) AS DWORD
    RETURN VoDb.FieldPos(cFieldName)
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldpos/*" />
FUNCTION FieldPos(cFieldName AS STRING, nArea AS DWORD) AS DWORD
    LOCAL oRDD := RuntimeState.WorkAreas:GetRDD(nArea) AS IRDD
    IF oRDD != NULL_OBJECT
        RETURN (DWORD) oRDD:FieldIndex(cFieldName) 
    ENDIF
    RETURN 0   
    

    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/found/*" />
FUNCTION Found() AS LOGIC 
	RETURN VoDb.Found()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/header/*" />
FUNCTION Header() AS LONG
    LOCAL oValue := NULL AS OBJECT
	VoDb.Info(DBI_GETHEADERSIZE, REF oValue)
    RETURN (LONG) oValue

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/lastrec/*" />
FUNCTION LastRec() AS DWORD
    RETURN (DWORD) VoDb.LastRec()

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbBuffRefresh() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.BuffRefresh())

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbclearfilter/*" />
FUNCTION DbClearFilter() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.ClearFilter())

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbclearrelation/*" />
FUNCTION DbClearRelation() AS LOGIC STRICT
	RETURN VoDb.ClearRelation()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbcloseall/*" />
FUNCTION DbCloseAll() AS LOGIC STRICT
	RETURN VoDb.CloseAll()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbclosearea/*" />
FUNCTION DbCloseArea () AS LOGIC STRICT
	RETURN VoDb.CloseArea()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbcommit/*" />
FUNCTION DbCommit() AS LOGIC STRICT
	RETURN VoDb.Commit()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbcommitall/*" />
FUNCTION DbCommitAll() AS LOGIC STRICT
	RETURN VoDb.CommitAll()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbcontinue/*" />
FUNCTION DbContinue() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Continue())

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbdriver/*" />
FUNCTION DbDriver() AS STRING STRICT
	RETURN RddName()



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbfilter/*" />
FUNCTION DbFilter() AS STRING STRICT
	RETURN VoDb.Filter()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbgetselect/*" />
FUNCTION DbGetSelect() AS DWORD STRICT
	RETURN VoDb.GetSelect()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbgobottom/*" />
FUNCTION DbGoBottom() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.GoBottom())


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbgotop/*" />
FUNCTION DbGoTop() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.GoTop())

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rddname/*" />
FUNCTION RddName        () AS STRING STRICT
	LOCAL cRet      AS STRING
	IF Used()
		cRet := VoDb.RddName()
	ELSE
		cRet := RddSetDefault()
	ENDIF
	RETURN cRet


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rddsetdefault/*" />
FUNCTION RddSetDefault  () AS STRING STRICT
    RETURN VoDb.RddSetDefault(NULL_STRING)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rddsetdefault/*" />
FUNCTION RddSetDefault  (cNewSetting AS STRING) AS STRING
    RETURN VoDb.RddSetDefault(cNewSetting)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/reccount/*" />
FUNCTION RecCount() AS LONG
    RETURN VoDb.LastRec()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/recno/*" />
FUNCTION RecNo() AS DWORD
    RETURN VoDb.Recno()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/recsize/*" />
FUNCTION RecSize AS LONG
    RETURN VoDb.RecSize()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rlock/*" />
FUNCTION RLock() AS LOGIC STRICT
	RETURN VoDb.Rlock(NULL)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/used/*" />
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
