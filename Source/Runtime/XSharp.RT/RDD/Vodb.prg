//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharp.RDD
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING System.Collections.Generic
USING System.Linq

/// <summary>The VoDb class extends the CoreDb class with methods that take usual parameters or return usual values.<br/>
/// All other methods are identical and inherited from the CoreDb class.</summary>
PARTIAL CLASS XSharp.VoDb INHERIT XSharp.CoreDb
PRIVATE STATIC METHOD DecodeResult(oValue as OBJECT) AS USUAL
    IF oValue == DBNull.Value
        RETURN NIL
    ENDIF
    RETURN oValue

/// <inheritdoc cref='CoreDb.BlobInfo'/>



STATIC METHOD BlobInfo(nOrdinal AS DWORD,nPos AS DWORD,ptrRet REF USUAL) AS LOGIC
    LOCAL oRet := ptrRet AS OBJECT
    LOCAL result AS LOGIC
    result := CoreDb.BlobInfo(nOrdinal, nPos, REF oRet)
    ptrRet := DecodeResult(oRet)
    RETURN result

/// <inheritdoc cref='CoreDb.BlobInfo'/>
STATIC METHOD BlobInfo(nOrdinal AS DWORD,nPos AS DWORD,uValue AS USUAL) AS LOGIC
    RETURN CoreDb.BlobInfo(nOrdinal, nPos, (OBJECT) uValue)

/// <inheritdoc cref='CoreDb.FieldInfo'/>
STATIC METHOD FieldInfo(nOrdinal AS DWORD,nFldPos AS DWORD,oValue REF USUAL) AS LOGIC
    LOCAL oRet := oValue AS OBJECT
    LOCAL result AS LOGIC
    result := CoreDb.FieldInfo(nOrdinal, nFldPos, REF oRet)
    oValue := DecodeResult(oRet)
    RETURN result

/// <inheritdoc cref="CoreDb.FieldInfo"/>
STATIC METHOD FieldInfo(nOrdinal AS DWORD,nFldPos AS DWORD,oValue AS USUAL) AS LOGIC
    RETURN CoreDb.FieldInfo(nOrdinal, nFldPos, (OBJECT) oValue)

/// <inheritdoc cref="CoreDb.FieldGet"/>
STATIC METHOD FieldGet(nPos AS DWORD,uRet REF USUAL) AS LOGIC
    LOCAL lResult AS LOGIC
    LOCAL oValue := uRet AS OBJECT
    lResult := CoreDb.FieldGet(nPos, REF oValue)
    // Note: FieldGet SHOULD return DBNull.Value when needed
    uRet := oValue
    RETURN lResult

/// <inheritdoc cref="CoreDb.Info"/>
/// <remarks> <inheritdoc cref="CoreDb.Info"/>
/// <br/><br/> <note type="tip">The difference between VoDb.Info and CoreDb.Info is that VoDb.Info takes a USUAL parameter</note></remarks>
STATIC METHOD Info(nOrdinal AS DWORD,oValue REF USUAL) AS LOGIC
    LOCAL oRet := oValue AS OBJECT
    LOCAL result AS LOGIC
    IF oValue:IsArray
        LOCAL aValue := oValue AS ARRAY
        oRet := (OBJECT[]) aValue
    ENDIF
    result := CoreDb.Info(nOrdinal, REF oRet)
    oValue := DecodeResult(oRet)
    RETURN result

/// <inheritdoc cref='CoreDb.Info'/>
/// <remarks> <inheritdoc cref='CoreDb.Info'/>
/// <br/><br/> <note type="tip">The difference between VoDb.Info and CoreDb.Info is that VoDb.Info takes a USUAL parameter</note></remarks>
STATIC METHOD Info(nOrdinal AS DWORD,oValue AS USUAL) AS LOGIC
    IF oValue:IsArray
        LOCAL aValue := oValue AS ARRAY
        oValue := (OBJECT[]) aValue
    ENDIF

    RETURN CoreDb.Info(nOrdinal, (OBJECT) oValue)

/// <inheritdoc cref='CoreDb.OrderInfo'/>
/// <remarks> <inheritdoc cref='CoreDb.OrderInfo'/>
/// <br/><br/> <note type="tip">The difference between VoDb.OrderInfo and CoreDb.OrderInfo is that VoDb.Info takes a USUAL parameter</note></remarks>
STATIC METHOD OrderInfo(nOrdinal AS DWORD,cBagName AS STRING,uOrder AS OBJECT,oValue REF USUAL) AS LOGIC
    LOCAL oRet := oValue AS OBJECT
    LOCAL result AS LOGIC
    result := CoreDb.OrderInfo(nOrdinal, cBagName,  uOrder, REF oRet)
    IF oRet == NULL
        oValue := NIL
    ELSE
        oValue := DecodeResult(oRet)
    ENDIF
    RETURN result

/// <inheritdoc cref="CoreDb.OrderInfo" />
/// <remarks> <inheritdoc cref='CoreDb.OrderInfo'/>
/// <br/><br/> <note type="tip">The difference between VoDb.OrderInfo and CoreDb.OrderInfo is that VoDb.Info takes a USUAL parameter</note></remarks>
STATIC METHOD OrderInfo(nOrdinal AS DWORD,cBagName AS STRING,uOrder AS OBJECT,oValue AS USUAL) AS LOGIC
    RETURN CoreDb.OrderInfo(nOrdinal, cBagName,  uOrder, (OBJECT) oValue)

/// <inheritdoc cref='CoreDb.RddInfo'/>
/// <remarks> <inheritdoc cref='CoreDb.RddInfo'/>
/// <br/><br/> <note type="tip">The difference between VoDb.RddInfo and CoreDb.RddInfo is that VoDb.RddInfo takes a USUAL parameter</note></remarks>
STATIC METHOD RddInfo(nOrdinal AS DWORD,uRet REF USUAL) AS LOGIC
    LOCAL oValue AS OBJECT
    oValue := uRet
    LOCAL result := CoreDb.RddInfo(nOrdinal, REF oValue) AS LOGIC
    uRet := DecodeResult(oValue)
    RETURN result

/// <inheritdoc cref='CoreDb.RddInfo'/>
/// <remarks> <inheritdoc cref='CoreDb.RddInfo'/>
/// <br/><br/> <note type="tip">The difference between VoDb.RddInfo and CoreDb.RddInfo is that VoDb.RddInfo takes a USUAL parameter</note></remarks>
STATIC METHOD RddInfo(nOrdinal AS DWORD,uValue AS USUAL) AS LOGIC
    RETURN CoreDb.RddInfo(nOrdinal, (OBJECT) uValue)

/// <inheritdoc cref='CoreDb.RecordInfo'/>
/// <remarks> <inheritdoc cref='CoreDb.RecordInfo'/>
/// <br/><br/> <note type="tip">The difference between VoDb.RecordInfo and CoreDb.RecordInfo is that VoDb.RecordInfo takes a USUAL parameter</note></remarks>
STATIC METHOD RecordInfo(nOrdinal AS DWORD,oRecID AS USUAL,oValue REF USUAL) AS LOGIC
    LOCAL oRet := oValue AS OBJECT
    LOCAL lResult AS LOGIC
    lResult := CoreDb.RecordInfo(nOrdinal, oRecID, REF oRet)
    oValue := DecodeResult(oRet)
    RETURN lResult

/// <inheritdoc cref='CoreDb.RecordInfo'/>
/// <remarks> <inheritdoc cref='CoreDb.RecordInfo'/>
/// <br/><br/> <note type="tip">The difference between VoDb.RecordInfo and CoreDb.RecordInfo is that VoDb.RecordInfo takes a USUAL parameter</note></remarks>
STATIC METHOD RecordInfo(nOrdinal AS DWORD,oRecID AS USUAL,oValue AS USUAL) AS LOGIC
    RETURN CoreDb.RecordInfo(nOrdinal, oRecID,  (OBJECT) oValue)


/// <inheritdoc cref='CoreDb.Select'/>
/// <remarks> <inheritdoc cref='CoreDb.Select'/>
/// <br/><br/> <note type="tip">The difference between VoDb.Select and CoreDb.Select is that VoDb.Select takes a USUAL parameter</note></remarks>
STATIC METHOD Select(nNew AS DWORD,riOld OUT USUAL) AS LOGIC
    LOCAL nOld := 0 AS DWORD
    LOCAL lResult AS LOGIC
    lResult := CoreDb.Select(nNew, OUT nOld)
    riOld := nOld
    RETURN lResult


/// <inheritdoc cref='CoreDb.SetFilter'/>
/// <remarks> <note type="tip">The difference between VoDb.SetFilter and CoreDb.SetFilter is that VoDb.SetFilter takes a USUAL parameter</note></remarks>
STATIC METHOD SetFilter(oBlock AS USUAL,cFilter AS STRING) AS LOGIC
    LOCAL cb AS ICodeblock
    IF oBlock:IsCodeblock
       cb := (ICodeblock) oBlock
    ELSE
        cb := NULL
    ENDIF
    RETURN CoreDb.SetFilter(cb, cFilter)


    INTERNAL STATIC METHOD ParamError(cFuncSym AS STRING, dwArgNum  AS DWORD , dwArgType AS DWORD, cArgName := "" AS STRING) AS Error

        LOCAL oError    AS Error
        oError := Error{RuntimeState.LastRddError}
        oError:SubSystem    := "DBCMD"
        oError:Gencode      := EG_ARG
        oError:Severity     := ES_ERROR
        oError:CanDefault   := .F.
        oError:CanRetry     := .T.
        oError:CanSubstitute := .F.
        oError:ArgType      := dwArgType
        oError:ArgNum       := dwArgNum
        oError:Arg          := cArgName
        oError:FuncSym      := cFuncSym
        RETURN oError

    INTERNAL STATIC METHOD DbCmdError(cFuncSym AS STRING)  AS Error
        LOCAL oError    AS Error
        if RuntimeState.LastRddError != NULL
            oError := Error{RuntimeState.LastRddError}
        else
            oError := Error{}
        endif
        oError:Gencode      := EG_NOTABLE
        oError:SubCode      := EDB_NOTABLE
        oError:SubSystem    := "DBCMD"
        oError:Severity     := ES_ERROR
        oError:FuncSym      := cFuncSym
        oError:CanDefault   := .T.
        RETURN oError


    INTERNAL STATIC METHOD AllocFieldNames(aStru AS ARRAY) AS _FieldNames
        VAR aNames := List<STRING>{}
        FOREACH aField AS USUAL IN aStru
            IF aField:IsArray
                aNames:Add(Upper(aField[DBS_NAME]))
            ELSE
                aNames:Add(Upper(aField))
            ENDIF
        NEXT
        RETURN _FieldNames{aNames}

    INTERNAL STATIC METHOD TargetFields  (cAlias AS STRING, aNames AS ARRAY, oJoinList OUT _JoinList) AS ARRAY

        LOCAL aNew      AS ARRAY
        LOCAL cName     AS STRING
        LOCAL aStruct   AS ARRAY
        LOCAL aDbStruct AS ARRAY
        LOCAL nFields, i AS INT
        LOCAL siPos     AS DWORD
        LOCAL siSelect  AS DWORD
        LOCAL aFldList  AS ARRAY

        aDbStruct := DbStruct()
        aStruct   := {}
        aFldList := {}

        IF ( Empty(aNames) )

            aNames     := {}
            nFields    := (INT) VoDb.FCount()
            siSelect   := VoDb.GetSelect()
            FOR i := 1 TO nFields
                cName := aDbStruct[i, DBS_NAME]
                AAdd(aFldList, {siSelect, VoDb.FieldPos(cName)})
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
            nFields := (INT)VoDb.FCount()
            siSelect := VoDb.GetSelect()
            FOR i := 1 TO nFields
                cName := aDbStruct[i, DBS_NAME]
                IF AScan(aNames, {|c| c == cName}) > 0
                    AAdd(aFldList, {siSelect, VoDb.FieldPos(cName)})
                    AAdd(aStruct, aDbStruct[i])
                ENDIF
            NEXT
        ENDIF
        siSelect := @@Select(cAlias)
        aDbStruct := DbStruct()
        nFields := (INT)Len(aNames)

        FOR i := 1 TO nFields
            IF "->" $ aNames[i]
                cName := SubStr2(aNames[i], At(">", aNames[i]) + 1)
            ELSE
                cName :=  aNames[i]
            ENDIF

            siPos := AScan(aDbStruct, {|a| a[DBS_NAME] == cName})
            IF siPos > 0 .AND. (AScan( aStruct, {|c|c[DBS_NAME]== cName }) == 0)
                AAdd(aFldList, {siSelect, VoDb.FieldPos(cName)})
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
        LOCAL   lFPT  := FALSE AS LOGIC
        LOCAL   lDbf    AS LOGIC

        IF xDriver:IsArray
            nType := ARRAY
        ELSEIF xDriver:IsString
            IF SLen(xDriver) = 0
                xDriver := RddSetDefault()
            ENDIF
            nType := STRING
        ELSE
            xDriver := RddSetDefault()
            nType := STRING
        ENDIF

        IF nType == ARRAY
            aRdds := xDriver
        ELSEIF nType == STRING
            aRdds := {}
            xDriver := Upper(xDriver)
            SWITCH (STRING) xDriver
            CASE "DBFNTX"
            CASE "DBFMEMO"
            CASE "DBFDBT"
                lDbf := .T.
            CASE "_DBFCDX"
            CASE "DBFFPT"
                xDriver := "DBFFPT"
                lDbf := .T.
            CASE "DBFCDX"
                lFPT := .T.
                lDbf  := .T.
                xDriver := "DBFFPT"
            CASE "DBFMDX"
                lDbf := .T.
            OTHERWISE
                lDbf := .F.
                lFPT := .F.
            END SWITCH

            IF lDbf
                AAdd(aRdds, "DBF")
            ENDIF

            AAdd(aRdds, xDriver)

            IF lFPT
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
        VAR nullable := 0
        VAR hasnullfield := FALSE
        FOREACH aField AS ARRAY IN aStruct
            VAR oFld := RddFieldInfo{(STRING) aField[DBS_NAME], (STRING) aField[DBS_TYPE], (LONG)aField[DBS_LEN], (LONG)aField[DBS_DEC]}
            IF ALen(aField) >= DBS_FLAGS
                oFld:Flags := (DBFFieldFlags) (LONG) aField[DBS_FLAGS]
            ENDIF
            IF oFld:IsNullable
                nullable += 1
            ENDIF
            IF oFld:FieldType == DbFieldType.NullFlags
                hasnullfield := TRUE
            ENDIF
            IF ALen(aField) >= DBS_ALIAS
                oFld:Alias := aField[DBS_ALIAS]
            ENDIF
            oList:Add(oFld)
        NEXT
        IF nullable != 0 .AND. ! hasnullfield
            VAR oFld := RddFieldInfo{_NULLFLAGS,"0", (nullable+7) % 8, 0}
            oList:Add(oFld)
        ENDIF
        RETURN oList:ToArray()

    INTERNAL STATIC METHOD OrdScopeNum(nScope)  AS INT CLIPPER
        IF !nScope:IsNumeric
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

    INTERNAL STATIC METHOD ValidBlock(uBlock AS USUAL, bDef := NULL_CODEBLOCK AS ICodeblock) AS ICodeblock
        IF uBlock:IsCodeblock .or. uBlock:IsObject
            LOCAL oBlock    := uBlock   AS OBJECT
            IF oBlock IS ICodeblock
                RETURN (ICodeblock) oBlock
            ENDIF
        ENDIF
        RETURN bDef

INTERNAL STATIC METHOD  FieldList(aStruct AS ARRAY, aNames AS ARRAY, aMatch AS ARRAY) AS ARRAY

	LOCAL aNew      AS ARRAY
	LOCAL cName     AS STRING
	LOCAL n, i, j   AS DWORD
	LOCAL lMatch	AS LOGIC


	IF Empty(aNames)
		RETURN (aStruct)
	ENDIF

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
    // Convert AStruct to List<String>
    VAR aFields := Dictionary<String, DWORD>{StringComparer.OrdinalIgnoreCase}
    FOR i := 1 to ALen(aStruct)
        local aFld := aStruct[i] AS ARRAY
        aFields:Add(Trim(aFld[DBS_NAME]), i )
    NEXT

	FOR i := 1 TO n
		cName := Trim(aNames[i])
        IF aFields:ContainsKey(cName)
            j := aFields[cName]
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
