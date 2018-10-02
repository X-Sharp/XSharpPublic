//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp.RDD
USING XSharp.RDD.Support
USING System.Collections.Generic
USING SYstem.Linq

/// <summary>The VoDb class extendes the CoreDb class with methods that take usual parameters or return usual values </summary>
PARTIAL CLASS XSharp.VoDb inherit XSharp.CoreDb
/// <inheritdoc/>
STATIC METHOD BlobInfo(nOrdinal AS DWORD,nPos AS DWORD,ptrRet REF USUAL) AS LOGIC
    LOCAL oRet := NULL AS OBJECT
    LOCAL result AS LOGIC
    result := CoreDb.BlobInfo(nOrdinal, nPos, REF oRet)
    ptrRet := oRet
    RETURN result

/// <inheritdoc/>
STATIC METHOD BlobInfo(nOrdinal AS DWORD,nPos AS DWORD,uValue AS USUAL) AS LOGIC
    RETURN CoreDb.BlobInfo(nOrdinal, nPos, (OBJECT) uValue)

/// <inheritdoc/>
STATIC METHOD FieldInfo(nOrdinal AS DWORD,nPos AS DWORD,ptrRet REF USUAL) AS LOGIC
    LOCAL oRet := NULL AS OBJECT
    LOCAL result AS LOGIC
    result := CoreDb.FieldInfo(nOrdinal, nPos, REF oRet)
    ptrRet := oRet
    RETURN result

/// <inheritdoc/>
STATIC METHOD FieldInfo(nOrdinal AS DWORD,nPos AS DWORD,uValue AS USUAL) AS LOGIC
    RETURN CoreDb.FieldInfo(nOrdinal, nPos, (OBJECT) uValue)
    
/// <inheritdoc/>
STATIC METHOD FieldGet(nPos AS DWORD,uRet REF USUAL) AS LOGIC
    LOCAL lResult AS LOGIC
    LOCAL oValue := uRet AS OBJECT
    lResult := CoreDb.FieldGet(nPos, REF oValue)
    uRet := oValue
    RETURN lResult

/// <inheritdoc/>
STATIC METHOD Info(nOrdinal AS DWORD,ptrRet REF USUAL) AS LOGIC
    LOCAL oRet := NULL AS OBJECT
    LOCAL result AS LOGIC
    result := CoreDb.Info(nOrdinal, REF oRet)
    ptrRet := oRet
    RETURN result

/// <inheritdoc/>
STATIC METHOD Info(nOrdinal AS DWORD,uValue AS USUAL) AS LOGIC
    RETURN CoreDb.Info(nOrdinal, (OBJECT) uValue)

/// <inheritdoc/>
STATIC METHOD OrderInfo(nOrdinal AS DWORD,cBagName AS STRING,uOrder AS OBJECT,uRet REF USUAL) AS LOGIC
    LOCAL oRet := NULL AS OBJECT   
    LOCAL result AS LOGIC
    result := CoreDb.OrderInfo(nOrdinal, cBagName,  uOrder, REF oRet)
    uRet := oRet
    RETURN result

/// <inheritdoc/>
STATIC METHOD OrderInfo(nOrdinal AS DWORD,cBagName AS STRING,uOrder AS OBJECT,uValue AS USUAL) AS LOGIC
    RETURN CoreDb.OrderInfo(nOrdinal, cBagName,  uOrder, (OBJECT) uValue)

/// <inheritdoc/>
STATIC METHOD RddInfo(nOrdinal AS DWORD,uRet REF USUAL) AS LOGIC
    LOCAL oValue AS OBJECT
    oValue := uRet
    LOCAL result := CoreDb.RddInfo(nOrdinal, REF oValue) AS LOGIC
    uRet := oValue
    RETURN result

/// <inheritdoc/>
STATIC METHOD RddInfo(nOrdinal AS DWORD,uValue AS USUAL) AS LOGIC
    RETURN CoreDb.RddInfo(nOrdinal, (OBJECT) uValue) 

/// <inheritdoc/>
STATIC METHOD RecordInfo(nOrdinal AS DWORD,uRecId AS USUAL,uRet REF USUAL) AS LOGIC
    LOCAL oRet := NULL AS OBJECT
    LOCAL lResult AS LOGIC
    lResult := CoreDb.RecordInfo(nOrdinal, uRecID, REF oRet)
    uRet := oRet
    RETURN lResult
    
/// <inheritdoc/>
STATIC METHOD RecordInfo(nOrdinal AS DWORD,uRecId AS USUAL,uRet AS USUAL) AS LOGIC
    RETURN CoreDb.RecordInfo(nOrdinal, uRecID,  (OBJECT) uRet)

/// <inheritdoc/>
STATIC METHOD Relation(nPos AS DWORD, uRel REF USUAL) AS LOGIC
    LOCAL cRel := "" AS STRING
    LOCAL lResult AS LOGIC
    lResult := CoreDb.Relation(nPos, REF cRel)
    uRel := cRel
    RETURN lResult
    
/// <inheritdoc/>
STATIC METHOD Select(nNew AS DWORD,riOld REF USUAL) AS LOGIC
    LOCAL nOld := 0 AS DWORD
    LOCAL lResult AS LOGIC
    lResult := CoreDb.Select(nNew, REF nOld)
    riOld := nOld
    RETURN lResult

    INTERNAL STATIC METHOD ParamError(cFuncSym AS STRING, dwArgNum  AS DWORD ,   dwArgType AS DWORD) AS Error 
    
        LOCAL oError    AS Error
        oError := Error{RuntimeState.LastRDDError}
        oError:SubSystem    := "DBCMD"
        oError:GenCode      := EG_ARG
        oError:Severity     := ES_ERROR
        oError:CanDefault   := .F.
        oError:CanRetry     := .T.
        oError:CanSubstitute := .F.
        oError:ArgType      := dwArgType
        oError:ArgNum       := dwArgNum
        oError:FuncSym      := cFuncSym
        RETURN oError
        
    INTERNAL STATIC METHOD DBCMDError(cFuncSym AS STRING)  AS Error 
        LOCAL oError    AS Error
        oError := Error{RuntimeState.LastRDDError}	
        oError:GenCode      := EG_NOTABLE
        oError:SubCode      := EDB_NOTABLE
        oError:SubSystem    := "DBCMD"
        oError:Severity     := ES_ERROR
        oError:FuncSym      := cFuncSym
        oError:CanDefault   := .T.
        RETURN oError
        
    INTERNAL STATIC METHOD AllocFieldNames(aStru AS ARRAY) AS _FieldNames
        VAR aNames := List<STRING>{}
        FOREACH aField AS USUAL IN aStru
            IF IsArray(aField)
                aNames:Add(Upper(aField[DBS_NAME]))
            ELSE
                aNames:Add(upper(aField))
            ENDIF
        NEXT
        RETURN _FieldNames{aNames}
        
    INTERNAL STATIC METHOD TargetFields  (cAlias AS STRING, aNames AS ARRAY, oJoinList OUT _JoinList) AS ARRAY 
    
        LOCAL aNew      AS ARRAY
        LOCAL cName     AS STRING
        LOCAL aStruct   AS ARRAY
        LOCAL adbStruct AS ARRAY
        LOCAL nFields, i AS INT
        LOCAL siPos     AS DWORD
        LOCAL siSelect  AS DWORD
        LOCAL aFldList  AS ARRAY
        
        adbStruct := DbStruct()
        aStruct   := {}
        aFldList := {}
        
        IF ( Empty(aNames) )
        
            aNames     := {}
            nFields    := (INT) FCount()
            siSelect   := VoDb.GetSelect()
            FOR i := 1 TO nFields
                cName := adbStruct[i, DBS_NAME]
                AAdd(aFldList, {siSelect, FieldPos(cName)})
                AAdd(aStruct, aDbStruct[i])
                AAdd(aNames, cName)
            NEXT
        ELSE
            nFields := (INT)Len(aNames)
            aNew := {}
            FOR i := 1 TO nFields
                AAdd(aNew, AllTrim(Upper(aNames[i])))
            NEXT
            aNames := aNew
            nFields := (INT)FCount()
            siSelect := VoDb.GetSelect()
            FOR i := 1 TO nFields
                cName := adbStruct[i, DBS_NAME]
                IF AScan(aNames, {|c| c == cName}) > 0
                    AAdd(aFldList, {siSelect, FieldPos(cName)})
                    AAdd(aStruct, aDbStruct[i])
                ENDIF
            NEXT
        ENDIF
        siSelect := SELECT(cAlias)
        aDbStruct := DbStruct()
        nFields := (INT)Len(aNames)
        
        FOR i := 1 TO nFields
            IF "->" $ aNames[i]
                cName := SubStr(aNames[i], At(">", aNames[i]) + 1)
            ELSE
                cName :=  aNames[i]
            ENDIF
            
            siPos := AScan(aDbStruct, {|a| a[DBS_NAME] == cName})
            IF siPos > 0 .AND. (AScan( aStruct, {|c|c[DBS_NAME]== cName }) == 0)
                AAdd(aFldList, {siSelect, FieldPos(cName)})
                AAdd(aStruct, aDbStruct[siPos])
            ENDIF
        NEXT
        nFields := (INT)ALen(aStruct)
        oJoinList := _JoinList{nFields}
        FOR i := 1 TO nFields
            oJoinList:Fields[i]:Area := aFldList[i,1]
            oJoinList:Fields[i]:Pos  := aFldList[i,2] - 1
        NEXT
        RETURN aStruct
        
    INTERNAL STATIC METHOD  RddList( xDriver AS USUAL, aHidden AS USUAL ) AS ARRAY
    
        LOCAL   nType   AS DWORD
        LOCAL   aRdds  := NULL_ARRAY AS ARRAY
        LOCAL   n       AS DWORD
        LOCAL   i       AS DWORD
        LOCAL   lBlob  := FALSE AS LOGIC
        LOCAL   lDbf    AS LOGIC
        
        IF IsArray(xDriver)
            nType := ARRAY
        ELSEIF IsString(xDriver)
            IF SLen(xDriver) = 0
                xDriver := RDDSetDefault()
            ENDIF
            nType := STRING
        ELSE
            xDriver := RDDSetDefault()
            nType := STRING
        ENDIF
        
        IF nType == ARRAY
            aRdds := xDriver
        ELSEIF nType == STRING
            aRdds := {}
            xDriver := upper(xDriver)
            DO CASE
            CASE xDriver = "DBFNTX"
                lDbf := .T.
            CASE xDriver = "_DBFCDX"
                lDbf := .T.
            CASE xDriver = "DBFCDX"
                    lBlob := .T.
                    lDbf  := .T.
                xDriver := "_DBFCDX"
            CASE xDriver = "DBFMDX"
                lDbf := .T.
            OTHERWISE
                    lDbf := .F.
                lBlob := .F.
            ENDCASE
            
            IF lDbf
                AAdd(aRdds, "DBF")  
            ENDIF
            
            AAdd(aRdds, xDriver)
            
            IF lBlob
                AAdd(aRdds, "DBFCDX")
            ENDIF
            
        ENDIF
        
        IF UsualType(aHidden) == ARRAY
            n := ALen(aHidden)
            FOR i := 1 TO n
                AAdd(aRdds, aHidden[i])
            NEXT
        ENDIF
        RETURN aRdds
        
    INTERNAL STATIC METHOD AllocRddList(aNames AS ARRAY) AS _RddList
        VAR aList := List<STRING>{}
        FOREACH cName AS STRING IN aNames
            aList:Add(cName)
        NEXT
        RETURN _RddList{aList:ToArray()}
    INTERNAL STATIC METHOD ArrayToFieldInfo(aStruct AS ARRAY) AS RddFieldInfo[]
        VAR oList := List<RddFieldInfo>{}
        FOREACH aField AS USUAL IN aStruct
            VAR oFld := RddFieldInfo{(STRING) aField[DBS_NAME], (STRING) aField[DBS_TYPE], (LONG)aField[DBS_LEN], (LONG)aField[DBS_DEC]}
            oList:Add(oFld)
        NEXT
        RETURN oList:ToArray()
        
    INTERNAL STATIC METHOD OrdScopeNum(nScope)  AS INT CLIPPER
        IF !IsNumeric( nScope )
            nScope := 0
        ENDIF
        nScope := INT(nScope)
        IF nScope < 0
            nScope := 0
        ENDIF
        IF nScope > 1
            nScope := 1
        ENDIF
        RETURN nScope
        
    INTERNAL STATIC METHOD WithoutAlias(cName AS STRING) AS STRING 
        cName   := SubStr(cName, At(">", cName) + 1 )
        cName   := Trim(Upper(cName))
        RETURN cName

    INTERNAL STATIC METHOD ValidBlock(uBlock AS USUAL) AS ICodeBlock
        LOCAL oBlock    := uBlock   AS OBJECT
        IF oBlock IS ICodeBlock
            RETURN (ICodeBlock) oBlock
        ENDIF
        RETURN NULL  

INTERNAL STATIC METHOD  FieldList(aStruct AS ARRAY, aNames AS ARRAY, aMatch AS ARRAY) AS ARRAY 
	
	LOCAL aNew      AS ARRAY
	LOCAL cobScan   AS CODEBLOCK
	LOCAL cName     AS STRING
	LOCAL n, i, j   AS DWORD
	LOCAL lMatch	AS LOGIC
	
	
	IF Empty(aNames)
		RETURN (aStruct)
	ENDIF
	
	//	UH 11/30/1998
	IF Empty(aMatch)
		lMatch := .F.
	ELSE
		lMatch := .T.
	ENDIF
	
	aNew:= {}
	n   := Len(aNames)
	
	FOR i := 1 TO n
		AAdd(aNew, VoDb.WithoutAlias(AllTrim(aNames[i])))
	NEXT
	
	aNames  := aNew
	cName   := ""
	aNew    := {}
	cobScan := {|aFld| aFld[DBS_NAME] == cName}
	
	FOR i := 1 TO n
		cName := aNames[i]
		j := AScan(aStruct, cobScan)
		
		IF j > 0
			IF lMatch
				IF aMatch[i, DBS_TYPE] == aStruct[j, DBS_TYPE]
					AAdd(aNew, aStruct[j])
				ENDIF
			ELSE
				AAdd(aNew, aStruct[j])
			ENDIF
		ENDIF
	NEXT
	
	RETURN aNew


END CLASS    
