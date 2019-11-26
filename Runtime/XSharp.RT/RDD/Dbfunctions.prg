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

#command DOINAREA <uArea> <func>  => ;
    LOCAL nArea := _Select(<uArea>)  AS DWORD ; ; 
    IF nArea != 0; ;
        VAR nOld := RuntimeState.CurrentWorkArea; ;
        TRY ;  ;
            RuntimeState.CurrentWorkArea := nArea; ;
            RETURN <FUNC> ; ; 
        FINALLY;  ;
            RuntimeState.CurrentWorkArea := nOld ; ;
        END TRY; ;
    ENDIF; ;
    THROW _NoAlias( <uArea> )  
 
FUNCTION _NoAlias(uArea AS USUAL) AS Error
    VAR oErr := Error{EG_NOTABLE, "uArea", "Alias '"+uArea:ToString()+"' is not found"}
    oErr:FuncSym := ProcName(1):Replace("FUNCTIONS:","")
    RETURN oErr
     
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

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/bof/*" />
FUNCTION Bof(uArea AS USUAL) AS LOGIC    
    DOINAREA uArea Bof()
    

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbf/*" />
FUNCTION DBF() AS STRING
    RETURN VoDb.DBF()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbf/*" />
FUNCTION DBF(uArea AS USUAL) AS STRING
    DOINAREA uArea DBF()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbpack/*" />
FUNCTION DbPack() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Pack())

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbpack/*" />
FUNCTION DbPack(uArea AS USUAL) AS LOGIC STRICT
    DOINAREA uArea DbPack()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbrecall/*" />
FUNCTION DbRecall() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Recall())

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbrecall/*" />
FUNCTION DbRecall(uArea AS USUAL) AS LOGIC STRICT
    DOINAREA uArea DbRecall()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbunlock/*" />
FUNCTION DbUnLock() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Unlock(NULL_OBJECT))

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbunlock/*" />
FUNCTION DbUnLock(nRecord AS LONG) AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Unlock(nRecord))

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbzap/*" />
FUNCTION DbZap() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Zap())


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbzap/*" />
FUNCTION DbZap(uArea AS USUAL) AS LOGIC STRICT
    DOINAREA uArea DbZap()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbunlockall/*" />
FUNCTION DbUnLockAll() AS LOGIC STRICT
	RETURN VoDb.UnLockAll()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/deleted/*" />
FUNCTION Deleted() AS LOGIC STRICT
	RETURN VoDb.Deleted()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/deleted/*" />
FUNCTION Deleted(uArea AS USUAL) AS LOGIC STRICT
    DOINAREA uArea Deleted()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/eof/*" />
FUNCTION Eof() AS LOGIC
   RETURN VoDb.Eof()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/eof/*" />
FUNCTION Eof(uArea AS USUAL) AS LOGIC
    DOINAREA uArea Eof()  

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/flock/*" />
FUNCTION Flock() AS LOGIC STRICT
	RETURN VoDb.Flock()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/flock/*" />
FUNCTION Flock(uArea AS USUAL) AS LOGIC STRICT
    DOINAREA uArea Flock()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fcount/*" />
FUNCTION FCount() AS DWORD
    RETURN VoDb.FCount()    

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fcount/*" />
FUNCTION FCount(uArea AS USUAL) AS DWORD
    DOINAREA uArea FCount()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldname/*" />
FUNCTION FieldName(dwFieldPos AS DWORD) AS STRING
    IF dwFieldPos > 0 .AND. dwFieldPos <= VoDb.FCount() 
        RETURN VoDb.FieldName(dwFieldPos)
    ENDIF
    RETURN String.Empty
    

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldname/*" />
FUNCTION FieldName(dwFieldPos AS DWORD, uArea AS USUAL) AS STRING
    DOINAREA uArea FieldName(dwFieldPos)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldsym/*" />
FUNCTION FieldSym(dwFieldPos AS DWORD) AS SYMBOL
    IF dwFieldPos > 0 .AND. dwFieldPos <= VoDb.FCount() 
        RETURN (SYMBOL) VoDb.FieldName(dwFieldPos)
    ENDIF
    RETURN NULL_SYMBOL


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldsym/*" />
FUNCTION FieldSym(dwFieldPos AS DWORD, uArea AS USUAL) AS SYMBOL
    DOINAREA uArea FieldSym(dwFieldPos)


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

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/found/*" />
FUNCTION Found(uArea AS USUAL) AS LOGIC 
    DOINAREA uArea Found()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/header/*" />
FUNCTION Header() AS LONG
    LOCAL oValue := NULL AS OBJECT
	VoDb.Info(DBI_GETHEADERSIZE, REF oValue)
    RETURN (LONG) oValue

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/header/*" />
FUNCTION Header(uArea AS USUAL) AS LONG 
    DOINAREA uArea Header()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/lastrec/*" />
FUNCTION LastRec() AS DWORD
    RETURN (DWORD) VoDb.LastRec()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/lastrec/*" />
FUNCTION LastRec(uArea AS USUAL) AS DWORD
    DOINAREA uArea LastRec()

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbBuffRefresh() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.BuffRefresh())


FUNCTION DbBuffRefresh(uArea AS USUAL) AS LOGIC STRICT
    DOINAREA uArea DbBuffRefresh()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbclearfilter/*" />
FUNCTION DbClearFilter() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.ClearFilter())


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbclearfilter/*" />
FUNCTION DbClearFilter(uArea AS USUAL) AS LOGIC STRICT
    DOINAREA uArea DbClearFilter()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbclearrelation/*" />
FUNCTION DbClearRelation() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.ClearRelation())


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbclearrelation/*" />
FUNCTION DbClearRelation(uArea AS USUAL) AS LOGIC STRICT
    DOINAREA uArea DbClearRelation()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbcloseall/*" />
FUNCTION DbCloseAll() AS LOGIC STRICT
	RETURN VoDb.CloseAll()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbclosearea/*" />
FUNCTION DbCloseArea () AS LOGIC STRICT
	RETURN VoDb.CloseArea()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbclosearea/*" />
FUNCTION DbCloseArea (uArea AS USUAL) AS LOGIC STRICT
    DOINAREA uArea DbCloseArea()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbcommit/*" />
FUNCTION DbCommit() AS LOGIC STRICT
	RETURN VoDb.Commit()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbcommit/*" />
FUNCTION DbCommit(uArea AS USUAL) AS LOGIC STRICT
    DOINAREA uArea DbCommit()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbcommitall/*" />
FUNCTION DbCommitAll() AS LOGIC STRICT
	RETURN VoDb.CommitAll()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbcontinue/*" />
FUNCTION DbContinue(uArea AS USUAL) AS LOGIC STRICT
    DOINAREA uArea DbContinue()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbcontinue/*" />
FUNCTION DbContinue() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Continue())

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbdriver/*" />
FUNCTION DbDriver() AS STRING STRICT
	RETURN RddName()



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbfilter/*" />
FUNCTION DbFilter() AS STRING STRICT
	RETURN VoDb.Filter()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbfilter/*" />
FUNCTION DbFilter(uArea AS USUAL) AS STRING STRICT
    DOINAREA uArea DbFilter()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbgetselect/*" />
FUNCTION DbGetSelect() AS DWORD STRICT
	RETURN VoDb.GetSelect()



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbgobottom/*" />
FUNCTION DbGoBottom() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.GoBottom())

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbgobottom/*" />
FUNCTION DbGoBottom(uArea AS USUAL) AS LOGIC STRICT
    DOINAREA uArea DbGoBottom()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbgotop/*" />
FUNCTION DbGoTop() AS LOGIC STRICT
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.GoTop())

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbgotop/*" />
FUNCTION DbGoTop(uArea AS USUAL) AS LOGIC STRICT
    DOINAREA uArea DbGoTop()

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

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/reccount/*" />
FUNCTION RecCount(uArea AS USUAL) AS LONG
    DOINAREA uArea RecCount()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/recno/*" />
FUNCTION RecNo() AS DWORD
    RETURN VoDb.Recno()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/recno/*" />
FUNCTION RecNo(uArea AS USUAL) AS DWORD    
    DOINAREA uArea RecNo()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/recsize/*" />
FUNCTION RecSize AS LONG
    RETURN VoDb.RecSize()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/recsize/*" />
FUNCTION RecSize(uArea AS USUAL) AS LONG
    DOINAREA uArea RecSize()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rlock/*" />
FUNCTION RLock() AS LOGIC STRICT
	RETURN VoDb.Rlock(NULL)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rlock/*" />
FUNCTION RLock(uArea AS USUAL) AS LOGIC STRICT
    DOINAREA uArea RLock()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rlock/*" />
FUNCTION RLock(cRecordNumberList AS STRING, uArea AS USUAL) AS LOGIC STRICT
	LOCAL aRecords  AS STRING[]
    LOCAL nSelect   AS DWORD
    LOCAL lOk       AS LOGIC
    IF String.IsNullOrWhitespace(cRecordNumberList)
        RETURN FALSE
    ENDIF
    aRecords := cRecordNumberList:Split(<CHAR>{','},StringSplitoptions.RemoveEmptyEntries)
    nSelect := DbGetSelect()
    DbSelectArea(uArea)
    lOk := TRUE
    FOREACH VAR s IN aRecords
        LOCAL iRecno AS INT
        IF Int32.TryParse(s, OUT iRecno)
            IF ! VoDb.RLock(iRecno)
                lOk := FALSE
                EXIT
            ENDIF
        ELSE
            lOk := FALSE
            EXIT
        ENDIF
    NEXT
    DbSelectArea(nSelect)
    RETURN lOk

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/used/*" />
FUNCTION Used() AS LOGIC
    RETURN RuntimeState.Workareas:CurrentWorkArea != NULL


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/used/*" />
FUNCTION Used(uArea AS USUAL) AS LOGIC
    IF IsString(uArea)
        RETURN VODbGetSelect(uArea) != 0
    ENDIF
    RETURN Alias(uArea) != NULL_STRING

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
