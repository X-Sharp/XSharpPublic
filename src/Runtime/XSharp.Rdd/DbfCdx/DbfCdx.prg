//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharp.RDD.Support
USING XSharp.RDD.CDX
USING XSharp.RDD.Enums
USING System.Collections.Generic
USING System.IO
USING System.Diagnostics

//#define TESTCDX

#ifdef TESTCDX
    GLOBAL LOGGING := TRUE AS LOGIC
    GLOBAL VALIDATETREE := FALSE AS LOGIC

#endif

BEGIN NAMESPACE XSharp.RDD
    // Inherits all standard DBF and Memo behavior
    // Only adds Order Handling
    /// <summary>DBFCDX RDD. For DBF/FPT/CDX.</summary>
    [DebuggerDisplay("DBFCDX ({Alias,nq})")];
    CLASS DBFCDX INHERIT DBFFPT
        INTERNAL _indexList  AS CdxOrderBagList
        INTERNAL _dirtyRead AS LOGIC
        INTERNAL PROPERTY CurrentOrder AS CdxTag GET _indexList:CurrentOrder
        OVERRIDE PROPERTY Driver  AS STRING GET nameof(DBFCDX)
        INTERNAL PROPERTY MustForceRel AS LOGIC GET _RelInfoPending != NULL
        // this is used to monitor which fields are using during index creation
        // so we can deduce if there is a nullable field in the index
        INTERNAL _fieldList AS List<INT>

        CONSTRUCTOR()
            SUPER()
            _indexList := CdxOrderBagList{SELF}
            _dirtyRead := !RuntimeState.GetValue<LOGIC>(Set.StrictRead)
            RETURN


            #ifdef TESTCDX
            PUBLIC STATIC METHOD StartLogging AS VOID
                LOGGING := TRUE
                RETURN

            PUBLIC STATIC METHOD StopLogging AS VOID
                LOGGING := FALSE
                RETURN

            PUBLIC STATIC METHOD ValidateTree AS VOID
                VALIDATETREE := TRUE
                CoreDb.GoTop()
                VALIDATETREE := FALSE
                RETURN
            #endif


            #region Order Support
            // GetValue is overridden so we can keep track of the fields
            // that are using in calculating an index expression.
            // _fieldList will only be set when the code to evaluate index expressions
            // is running
            OVERRIDE METHOD GetValue(nFldPos as LONG) AS OBJECT
                IF SELF:_fieldList != NULL
                    SELF:_fieldList:Add(nFldPos)
                ENDIF
                RETURN SUPER:GetValue(nFldPos)
            OVERRIDE METHOD Info(nOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
	            LOCAL oResult AS OBJECT
                oResult := NULL
                BEGIN SWITCH nOrdinal
                CASE DbInfo.DBI_DIRTYREAD
                    oResult := SELF:_dirtyRead
                    IF oNewValue IS LOGIC VAR lValue
                        SELF:_dirtyRead := lValue
                    ENDIF
                CASE DbInfo.DBI_STRICTREAD
                    oResult := !SELF:_dirtyRead
                    IF oNewValue IS LOGIC VAR lValue
                        SELF:_dirtyRead := !lValue
                    ENDIF
                OTHERWISE
                    oResult := SUPER:Info(nOrdinal, oNewValue)
                END SWITCH
                RETURN oResult
            OVERRIDE METHOD OrderCreate(orderInfo AS DbOrderCreateInfo ) AS LOGIC
                VAR result := SELF:_indexList:Create(orderInfo)
                if result
                    SELF:MarkDbfHeader(SELF:_FileName,TRUE)
                endif
                RETURN result

            OVERRIDE METHOD OrderDestroy(orderInfo AS DbOrderInfo ) AS LOGIC
                RETURN SELF:_indexList:Destroy(orderInfo)

            OVERRIDE METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
                RETURN SUPER:OrderCondition(info)

            OVERRIDE METHOD OrderListAdd( orderInfo AS DbOrderInfo) AS LOGIC
                BEGIN LOCK SELF
                    SELF:GoCold()
                    LOCAL fullPath AS STRING
                    fullPath := orderInfo:BagName
                    IF File(fullPath)
                        fullPath := FPathName()
                    ELSEIF String.IsNullOrEmpty(Path.GetExtension(fullPath))
                        fullPath := Path.ChangeExtension(fullPath, CdxOrderBag.CDX_EXTENSION)
                        IF File(fullPath)
                            fullPath := FPathName()
                            orderInfo:BagName := fullPath
                        ENDIF
                    ENDIF
                    IF String.IsNullOrEmpty(System.IO.Path.GetDirectoryName(fullPath))
                        fullPath := System.IO.Path.Combine(System.IO.Path.GetDirectoryName(SELF:_FileName), fullPath)
                        orderInfo:BagName := fullPath
                    ENDIF
                    LOCAL lOk := FALSE AS LOGIC
                    IF SELF:_indexList:FindOrderBag(orderInfo:BagName) == NULL
                        lOk := SELF:_indexList:Add(orderInfo)
                    ELSE
                        // Already open, do nothing
                        lOk := TRUE
                    ENDIF
                    IF lOk .and. SELF:CurrentOrder == NULL
                        orderInfo:Order := 1
                        lOk := SELF:OrderListFocus(orderInfo)
                    ENDIF
                    RETURN lOk
                END LOCK


            METHOD _CloseAllIndexes(orderInfo AS DbOrderInfo, lCloseStructural AS LOGIC) AS LOGIC
                RETURN SELF:_indexList:Delete(orderInfo, lCloseStructural)

            OVERRIDE METHOD OrderListDelete(orderInfo AS DbOrderInfo) AS LOGIC
                BEGIN LOCK SELF
                    SELF:GoCold()
                    RETURN SELF:_CloseAllIndexes(orderInfo, FALSE)
                END LOCK

            OVERRIDE METHOD OrderListFocus(orderInfo AS DbOrderInfo) AS LOGIC
                BEGIN LOCK SELF
                    SELF:GoCold()
                    RETURN SELF:_indexList:Focus(orderInfo)
                END LOCK

            OVERRIDE METHOD OrderListRebuild() AS LOGIC
                BEGIN LOCK SELF
                    IF SELF:Shared
                        // Error !! Cannot be written !
                        SELF:_dbfError( ERDD.SHARED, XSharp.Gencode.EG_SHARED )
                        RETURN FALSE
                    ENDIF
                    IF SELF:_ReadOnly
                        SELF:_dbfError( ERDD.READONLY, XSharp.Gencode.EG_READONLY)
                        RETURN FALSE
                    ENDIF

                    SELF:GoCold()
                    RETURN SELF:_indexList:Rebuild()
                END LOCK

            OVERRIDE METHOD OrderInfo(nOrdinal AS DWORD , info AS DbOrderInfo ) AS OBJECT
                LOCAL result AS LONG
                LOCAL isOk := FALSE AS LOGIC
                LOCAL oBag := NULL AS CdxOrderBag
                LOCAL hasBagName := FALSE AS LOGIC
                result := 0
                SELF:_indexList:FindOrder(info, OUT VAR workOrder)
                IF ! String.IsNullOrEmpty(info:BagName)
                     hasBagName := TRUE
                     oBag := SELF:_indexList:FindOrderBag(info:BagName)
                ENDIF

                IF workOrder == NULL .AND. info:IsEmpty
                    workOrder := SELF:CurrentOrder
                ENDIF

                BEGIN SWITCH nOrdinal
                CASE DBOI_DEFBAGEXT
                    info:Result := CdxOrderBag.CDX_EXTENSION
                CASE DBOI_CONDITION
                    IF workOrder != NULL
                        info:Result := workOrder:Condition
                    ELSE
                        info:Result := ""
                    ENDIF
                CASE DBOI_EXPRESSION
                    IF workOrder != NULL
                        info:Result := workOrder:Expression
                    ELSE
                        info:Result := ""
                    ENDIF
                CASE DBOI_ORDERCOUNT
                    IF oBag == NULL
                        if hasBagName
                            info:Result := 0
                        else
                            info:Result := SELF:_indexList:Count
                        endif
                    ELSE
                        info:Result := oBag:Tags:Count
                    ENDIF
                CASE DBOI_POSITION
                CASE DBOI_RECNO
                    VAR oState := SELF:_GetState()
                    IF workOrder == NULL
                        info:Result := SELF:RecNo
                    ELSE
                        isOk := workOrder:_getRecPos( REF result)
                        IF isOk
                            info:Result := result
                        ENDIF
                    ENDIF
                    SELF:_SetState(oState)
                CASE DBOI_KEYCOUNT
                    result := 0
                    VAR oState := SELF:_GetState()
                    IF workOrder != NULL
                        info:Result := 0
                        isOk := workOrder:_CountRecords(REF result)
                    ELSE
                        isOk := TRUE
                    ENDIF
                    IF isOk
                        info:Result := result
                    ENDIF
                    SELF:_SetState(oState)
                CASE DBOI_NUMBER
                    info:Result := SELF:_indexList:OrderPos(workOrder)
                CASE DBOI_BAGEXT
                    // according to the docs this should always return the default extension and not the actual extension
                    IF workOrder != NULL
                        info:Result := System.IO.Path.GetExtension(workOrder:OrderBag:FullPath)
                    ELSE
                        info:Result := CdxOrderBag.CDX_EXTENSION
                    ENDIF
                CASE DBOI_FULLPATH
                    IF workOrder != NULL
                        info:Result := workOrder:OrderBag:FullPath
                    ELSE
                        info:Result := String.Empty
                    ENDIF
                CASE DBOI_BAGCOUNT
                    info:Result := SELF:_indexList:BagCount
                CASE DBOI_BAGNAME
                    //CASE DBOI_INDEXNAME // alias
                    IF info:Order IS LONG VAR nOrder
                        info:Result := SELF:_indexList:BagName(nOrder)
                    ELSEIF workOrder != NULL
                        info:Result := workOrder:FileName
                    ELSE
                        info:Result :=String.Empty
                    ENDIF

                CASE DBOI_NAME
                    IF workOrder != NULL
                        info:Result := workOrder:_orderName
                    ELSE
                        info:Result := String.Empty
                    ENDIF
                CASE DBOI_COLLATION
                     info:Result := ""
                     IF workOrder != NULL
                        LOCAL collation as VfpCollation
                        collation := workOrder:Collation
                        if collation  != NULL
                            info:Result := collation:Name
                        ENDIF
                    ENDIF

                CASE DBOI_FILEHANDLE
                    IF workOrder != NULL
                        info:Result := workOrder:OrderBag:Handle
                    ELSE
                        info:Result := IntPtr.Zero
                    ENDIF
                CASE DBOI_FILESTREAM
                    IF workOrder != NULL
                        info:Result := workOrder:OrderBag:Stream
                    ELSE
                        info:Result := NULL
                    ENDIF
                CASE DBOI_ISDESC
                    IF workOrder != NULL
                        VAR oldValue  := workOrder:Descending
                        IF info:Result IS LOGIC VAR descend
                            workOrder:Descending := descend
                        ENDIF
                        info:Result := oldValue
                    ELSE
                        info:Result := FALSE
                    ENDIF
                CASE DBOI_ISCOND
                    IF workOrder != NULL
                        info:Result := workOrder:Conditional
                    ELSE
                        info:Result := FALSE
                    ENDIF
                CASE DBOI_KEYTYPE
                    IF workOrder != NULL
                        info:Result := workOrder:KeyType
                    ELSE
                        info:Result := 0
                    ENDIF
                CASE DBOI_KEYSIZE
                    IF workOrder != NULL
                        info:Result := workOrder:KeyLength
                    ELSE
                        info:Result := 0
                    ENDIF
                CASE DBOI_KEYDEC
                    info:Result := 0
                CASE DBOI_HPLOCKING
                    info:Result := FALSE
                CASE DBOI_UNIQUE
                    IF workOrder != NULL
                        info:Result := workOrder:Unique
                    ELSE
                        info:Result := FALSE
                    ENDIF
                CASE DBOI_LOCKOFFSET
                    IF workOrder != NULL
                        info:Result := workOrder:OrderBag:_LockOffSet
                    ELSE
                        info:Result := 0
                    ENDIF
                CASE DBOI_SETCODEBLOCK
                    IF workOrder != NULL
                        info:Result := workOrder:KeyCodeBlock
                    ENDIF
                CASE DBOI_KEYVAL
                    IF workOrder != NULL
                        isOk := TRUE
                        TRY
                            info:Result := SELF:EvalBlock(workOrder:KeyCodeBlock)
                        CATCH ex AS Exception
                            isOk := FALSE
                            SELF:_dbfError(ex, Subcodes.EDB_EXPRESSION, Gencode.EG_SYNTAX, "DBFCDX.OrderInfo")
                        END TRY
                        IF !isOk
                            info:Result := DBNull.Value
                        ENDIF
                    ELSE
                        info:Result := DBNull.Value
                    ENDIF
                CASE DBOI_SCOPETOPCLEAR
                CASE DBOI_SCOPEBOTTOMCLEAR
                    IF workOrder != NULL
                        workOrder:SetOrderScope(info:Result, (DbOrder_Info) nOrdinal)
                    ENDIF
                    info:Result := NULL
                CASE DBOI_SCOPETOP
                CASE DBOI_SCOPEBOTTOM
                    IF workOrder != NULL
                        LOCAL oldValue as OBJECT
                        IF nOrdinal == DBOI_SCOPETOP
                            oldValue := workOrder:TopScope
                        ELSEIF nOrdinal == DBOI_SCOPEBOTTOM
                            oldValue := workOrder:BottomScope
                        ELSE
                            oldValue := DBNull.Value
                        ENDIF
                        IF info:Result != NULL
                            workOrder:SetOrderScope(info:Result, (DbOrder_Info) nOrdinal)
                        ENDIF
                        info:Result := oldValue
                    ELSE
                        info:Result := DBNull.Value
                    ENDIF
                CASE DBOI_KEYADD
                    IF workOrder != NULL
                        info:Result := workOrder:AddKey(SELF:RecNo)
                    ELSE
                        info:Result := FALSE
                    ENDIF
                CASE DBOI_KEYDELETE
                    IF workOrder != NULL
                        info:Result := workOrder:DeleteKey(SELF:RecNo)
                    ELSE
                        info:Result := FALSE
                    ENDIF
                CASE DBOI_CUSTOM
                    IF workOrder != NULL
                        LOCAL lOld AS LOGIC
                        lOld := workOrder:Custom
                        IF info:Result IS LOGIC VAR custom
                            IF custom
                                workOrder:SetCustom()
                            ENDIF
                        ENDIF
                        info:Result := lOld
                    ELSE
                        info:Result := FALSE
                    ENDIF

                CASE DBOI_USER + 42
                CASE DBOI_DUMP
                    // Dump Cdx to Txt file
                    VAR oState := SELF:_GetState()
                    IF workOrder != NULL
                        workOrder:_dump()
                    ENDIF
                    SELF:_SetState(oState)
                CASE DBOI_VALIDATE
                    // Validate integrity of the current Order
                    VAR oState := SELF:_GetState()
                    IF workOrder != NULL
                        info:Result := workOrder:_validate()
                    ENDIF
                    SELF:_SetState(oState)
                CASE DBOI_SKIPUNIQUE
                    IF workOrder != NULL
                        local nToSkip := 1 as LONG
                        if info:Result IS LONG VAR nNum
                            nToSkip := nNum
                        endif
                        info:Result := workOrder:SkipUnique(nToSkip)
                    ENDIF
                OTHERWISE
                    SUPER:OrderInfo(nOrdinal, info)
                END SWITCH
                RETURN info:Result

            #endregion
        #region relations
        OVERRIDE METHOD ForceRel() AS LOGIC
            LOCAL isOk    := TRUE AS LOGIC
            IF SELF:_RelInfoPending != NULL
                // Save the current context
                LOCAL currentRelation := SELF:_RelInfoPending AS DbRelInfo
                SELF:_RelInfoPending := NULL
                if currentRelation:Parent:EoF
                    //
                    isOk := SELF:GoTo( 0 )
                ELSE
                    isOk := super:_RelSeek(currentRelation )
                ENDIF
            endif

            RETURN isOk
            #endregion
        #region Pack, Zap
        OVERRIDE METHOD Pack() AS LOGIC
            LOCAL isOk AS LOGIC

            isOk := SUPER:Pack()
            IF isOk
                isOk := SELF:OrderListRebuild()
            ENDIF
            RETURN isOk

        OVERRIDE METHOD Zap() AS LOGIC
            LOCAL isOk AS LOGIC

            isOk := SUPER:Zap()
            IF isOk
                isOk := SELF:OrderListRebuild()
            ENDIF
            RETURN isOk

        #endregion

        #region Open, Close, Create

        OVERRIDE METHOD Close() AS LOGIC
            LOCAL orderInfo AS DbOrderInfo
            BEGIN LOCK SELF
                SELF:GoCold()
                orderInfo := DbOrderInfo{}
                orderInfo:AllTags := TRUE
                SELF:_CloseAllIndexes(orderInfo, TRUE)
                RETURN SUPER:Close()
            END LOCK

        OVERRIDE METHOD Create( openInfo AS DbOpenInfo ) AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL lMemo := FALSE AS LOGIC
            isOk := SUPER:Create(openInfo)
            IF isOk
                FOREACH VAR fld IN SELF:_Fields
                    IF fld:FieldType:IsMemo()
                        lMemo := TRUE
                        EXIT
                    ENDIF
                NEXT
                /*
                RvdH 2021-08-16 DO not delete the file. VO also does not DO that
                LOCAL cIndex AS STRING
                // Delete index because it may have incorrect index expressions
                cIndex := System.IO.Path.ChangeExtension(SELF:FullPath, ".CDX")
                IF System.IO.File.Exists(cIndex)
                    System.IO.File.Delete(cIndex)
                ENDIF
                */
                IF lMemo
                    SELF:_Header:Version := DBFVersion.FoxPro2WithMemo
                ELSE
                    SELF:_Header:Version := DBFVersion.FoxBaseDBase3NoMemo
                ENDIF
            ENDIF
            RETURN isOk


        OVERRIDE METHOD Open(info AS DbOpenInfo) AS LOGIC
            LOCAL lOk AS LOGIC
            lOk := SUPER:Open(info)
            IF lOk
                // Open structural index
                IF RuntimeState.AutoOpen
                    SELF:OpenProductionIndex(info)
                ENDIF
                SELF:GoTop()
            ENDIF
            RETURN lOk

        PROTECTED METHOD OpenProductionIndex(info AS DbOpenInfo) AS VOID
            VAR cExt  := CdxOrderBag.GetIndexExtFromDbfExt(info:FullName)
            IF ! String.IsNullOrEmpty(cExt)
                VAR cCdxFileName := System.IO.Path.ChangeExtension(info:FullName, cExt)
                IF System.IO.File.Exists(cCdxFileName)
                    LOCAL orderinfo := DbOrderInfo{} AS DbOrderInfo
                    orderinfo:BagName := cCdxFileName
                    SELF:_indexList:Add(orderinfo, TRUE)
                ENDIF
                SELF:MarkDbfHeader(info:FullName,FALSE)
            ENDIF

         PROTECTED METHOD MarkDbfHeader(cFileName as STRING, lForce as LOGIC) AS VOID
            VAR cExt  := CdxOrderBag.GetIndexExtFromDbfExt(cFileName)
            IF ! String.IsNullOrEmpty(cExt)
                VAR cCdxFileName := System.IO.Path.ChangeExtension(cFileName, cExt)
                var wanted := SELF:Header:TableFlags
                IF System.IO.File.Exists(cCdxFileName)
                    wanted |= DBFTableFlags.HasStructuralCDX
                ELSE
                    wanted &= _NOT(DBFTableFlags.HasStructuralCDX)
                ENDIF
                IF wanted != SELF:Header:TableFlags .or. lForce
                    IF ! SELF:_ReadOnly
                        SELF:Header:TableFlags := wanted
                        SELF:Header:Write()
                    ENDIF
                ENDIF
            ENDIF

        #endregion

        #REGION Move

        INTERNAL METHOD ReadRecord() AS LOGIC
            RETURN SELF:_readRecord()

        OVERRIDE METHOD Seek(seekInfo AS DbSeekInfo ) AS LOGIC
            LOCAL isOk AS LOGIC

            isOk := FALSE
            BEGIN LOCK SELF
                VAR index := SELF:CurrentOrder
                IF index != NULL
                    isOk := index:Seek(seekInfo)
                ENDIF
                IF  !isOk
                    SELF:_dbfError(Subcodes.ERDD_DATATYPE, Gencode.EG_NOORDER )
                ENDIF
                SELF:_CheckEofBof()
            END LOCK
            RETURN isOk

        OVERRIDE METHOD GoBottom() AS LOGIC
            BEGIN LOCK SELF
                LOCAL result AS LOGIC
                IF SELF:CurrentOrder != NULL
                    result := SELF:CurrentOrder:GoBottom()
                    if (! result)
                        SELF:_SetEOF(TRUE)
                        SELF:_SetBOF(TRUE)
                    ELSE
                        SELF:_CheckEofBof()
                    ENDIF
                ELSE
                    result := SUPER:GoBottom()
                ENDIF
                RETURN result
            END LOCK

        OVERRIDE METHOD GoTop() AS LOGIC
            BEGIN LOCK SELF
                LOCAL result AS LOGIC
                IF SELF:CurrentOrder != NULL
                    result := SELF:CurrentOrder:GoTop()
                    if (! result)
                        SELF:_SetEOF(TRUE)
                        SELF:_SetBOF(TRUE)
                        result := TRUE
                    ELSE
                        SELF:_CheckEofBof()
                    ENDIF
                ELSE
                    result := SUPER:GoTop()
                ENDIF
                RETURN result
            END LOCK

        METHOD __Goto(nRec AS LONG) AS LOGIC
            // Skip without reset of stack
            RETURN SUPER:GoTo(nRec)

        OVERRIDE METHOD GoTo(nRec AS LONG) AS LOGIC
            LOCAL result AS LOGIC
            SELF:GoCold()
            IF SELF:CurrentOrder != NULL
                SELF:CurrentOrder:ClearStack() // force to reseek later
            ENDIF
            result := SUPER:GoTo(nRec)
            RETURN result

        OVERRIDE METHOD SkipRaw( move AS LONG ) AS LOGIC
            BEGIN LOCK SELF
                LOCAL result AS LOGIC
                IF move == 0 .AND. (SUPER:_fLocked .OR. SUPER:_Locks:Contains(SUPER:RecNo)) .AND. SUPER:_BufferValid
                    return self:GoCold()
                ELSEIF SELF:CurrentOrder != NULL
                    result := SELF:CurrentOrder:SkipRaw(move)
                    SELF:_CheckEofBof()
                ELSE
                    result := SUPER:SkipRaw(move)
                ENDIF
                RETURN result
            END LOCK

        #ENDREGION

        #REGION GoCold, GoHot, Flush
        OVERRIDE METHOD GoCold() AS LOGIC
            LOCAL isOk AS LOGIC

            isOk := TRUE
            BEGIN LOCK SELF
                IF !SELF:IsHot
                    RETURN isOk
                ENDIF
                isOk := SELF:_indexList:GoCold()
                IF !isOk
                    RETURN isOk
                ENDIF
                RETURN SUPER:GoCold()
            END LOCK

         OVERRIDE METHOD GoHot() AS LOGIC
            LOCAL isOk AS LOGIC

            isOk := TRUE
            BEGIN LOCK SELF
                isOk := SUPER:GoHot()
                IF !isOk
                    RETURN isOk
                ENDIF
                RETURN SELF:_indexList:GoHot()
            END LOCK

        OVERRIDE METHOD Flush() AS LOGIC
            LOCAL isOk AS LOGIC

            isOk := TRUE
            BEGIN LOCK SELF
                isOk := SUPER:Flush()
                RETURN SELF:_indexList:Flush() .AND. isOk
            END LOCK

        #endregion

        INTERNAL METHOD _GetState() AS CdxState
            RETURN CdxState{} {EoF := SELF:EoF, BoF := SELF:BoF, RecNo := SELF:RecNo}

        INTERNAL METHOD _SetState(oState AS CdxState) AS VOID
            SELF:__Goto(oState:RecNo)
            SELF:_SetEOF(oState:EoF)
            SELF:_SetBOF(oState:BoF)

        INTERNAL CLASS CdxState
            PROPERTY EoF AS LOGIC AUTO
            PROPERTY BoF AS LOGIC AUTO
            PROPERTY RecNo AS LONG AUTO
        END CLASS
    END CLASS

END NAMESPACE



