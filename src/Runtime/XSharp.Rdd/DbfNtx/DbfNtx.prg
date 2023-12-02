//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING XSharp.RDD.Support
USING XSharp.RDD.NTX
USING XSharp.RDD.Enums
USING System.IO
USING System.Diagnostics

BEGIN NAMESPACE XSharp.RDD
    // Inherits all standard DBF and Memo behavior
    // Only adds Order Handling
    /// <summary>DBFNTX RDD. For DBF/DBT/NTX.</summary>
    [DebuggerDisplay("DBFNTX ({Alias,nq})")];
    CLASS DBFNTX INHERIT DBFDBT
        INTERNAL _indexList AS NtxOrderList
        INTERNAL PROPERTY CurrentOrder AS NtxOrder GET _indexList:CurrentOrder
        OVERRIDE PROPERTY Driver AS STRING GET nameof(DBFNTX)
        CONSTRUCTOR()
            SUPER()
            SELF:_indexList := NtxOrderList{SELF}
            SELF:_oIndex 	:= NULL
            RETURN


            #REGION Order Support
        OVERRIDE METHOD OrderCreate(orderInfo AS DbOrderCreateInfo ) AS LOGIC
            VAR result :=  SELF:_indexList:Create(orderInfo)
            RETURN result

        OVERRIDE METHOD OrderDestroy(orderInfo AS DbOrderInfo ) AS LOGIC
            RETURN SUPER:OrderDestroy(orderInfo)

        OVERRIDE METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
            RETURN SUPER:OrderCondition(info)

        OVERRIDE METHOD OrderListAdd( orderInfo AS DbOrderInfo) AS LOGIC
            BEGIN LOCK SELF
                SELF:GoCold()
                LOCAL fullPath AS STRING
                fullPath := orderInfo:BagName
                IF String.IsNullOrEmpty(System.IO.Path.GetDirectoryName(fullPath))
                    fullPath := System.IO.Path.Combine(System.IO.Path.GetDirectoryName(SELF:_FileName), fullPath)
                ENDIF
                VAR lOk := SELF:_indexList:Add(orderInfo, fullPath)
                SELF:_oIndex := SELF:_indexList:CurrentOrder
                IF RuntimeState.LastRddError != null
                    lOk := FALSE
                ENDIF
                RETURN lOk
            END LOCK

        OVERRIDE METHOD OrderListDelete(orderInfo AS DbOrderInfo) AS LOGIC

            BEGIN LOCK SELF

                SELF:GoCold()
                RETURN SELF:_indexList:CloseAll() .and. RuntimeState.LastRddError == null
            END LOCK

        OVERRIDE METHOD OrderListFocus(orderInfo AS DbOrderInfo) AS LOGIC
            BEGIN LOCK SELF
                SELF:GoCold()
                RETURN SELF:_indexList:SetFocus(orderInfo)  .and. RuntimeState.LastRddError == null
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
                local current := SELF:CurrentOrder as NtxOrder
                var lOk := SELF:_indexList:Rebuild()  .and. RuntimeState.LastRddError == null
                IF lOk .and. current != null
                    var orderInfo := DbOrderInfo{}
                    orderInfo:BagName := current:FullPath
                    orderInfo:Order   := current:OrderName
                    SELF:OrderListFocus(orderInfo)
                ENDIF
                RETURN lOk
            END LOCK

        OVERRIDE METHOD OrderInfo(nOrdinal AS DWORD , info AS DbOrderInfo ) AS OBJECT
            LOCAL result AS LONG
            LOCAL workOrder AS NtxOrder
            LOCAL orderPos AS LONG
            LOCAL isOk := FALSE AS LOGIC

            result := 0
            workOrder := NULL
            orderPos := SELF:_indexList:FindOrder(info)
            IF orderPos <= 0
                IF info:IsEmpty .OR. orderPos == 0
                    workOrder := SELF:CurrentOrder
                ELSE
                    workOrder := NULL
                ENDIF
            ELSE
                workOrder := SELF:_indexList[orderPos - 1]
            ENDIF

            BEGIN SWITCH nOrdinal
            CASE DBOI_CONDITION
                IF workOrder != NULL
                    info:Result := workOrder:Condition
                ELSE
                    info:Result := DBNull.Value
                ENDIF
            CASE DBOI_EXPRESSION
                IF workOrder != NULL
                    info:Result := workOrder:Expression
                ELSE
                    info:Result := DBNull.Value
                ENDIF
            CASE DBOI_ORDERCOUNT
            CASE DBOI_BAGCOUNT
                info:Result := SELF:_indexList:Count
            CASE DBOI_POSITION
                IF workOrder == NULL
                    info:Result := 0
                ELSE
                    isOk := workOrder:_getRecPos( REF result)
                    IF isOk
                        info:Result := result
                    ENDIF
                ENDIF
            CASE DBOI_KEYCOUNT
                result := 0
                IF workOrder != NULL
                    info:Result := 0
                    isOk := workOrder:_CountRecords(REF result)
                ELSE
                    isOk := TRUE
                ENDIF
                IF isOk
                    info:Result := result
                ENDIF
            CASE DBOI_NUMBER
                info:Result := SELF:_indexList:OrderPos(workOrder)

            CASE DBOI_DEFBAGEXT
                // according to the docs this should always return the default extension and not the actual extension
                info:Result := NtxOrder.NTX_EXTENSION
            CASE DBOI_BAGEXT
                // according to the docs this should always return the default extension and not the actual extension
                IF workOrder != NULL
                    info:Result := System.IO.Path.GetExtension(workOrder:FullPath)
                ELSE
                    info:Result := NtxOrder.NTX_EXTENSION
                ENDIF
            CASE DBOI_FULLPATH
                IF workOrder != NULL
                    info:Result := workOrder:FullPath
                ELSE
                    info:Result := DBNull.Value
                ENDIF
            CASE DBOI_BAGNAME
                //CASE DBOI_INDEXNAME // alias
                IF workOrder != NULL
                    info:Result := System.IO.Path.GetFileName(workOrder:FullPath)
                ELSE
                    info:Result := DBNull.Value
                ENDIF
            CASE DBOI_NAME
                IF workOrder != NULL
                    info:Result := workOrder:OrderName
                ELSE
                    info:Result := DBNull.Value
                ENDIF
            CASE DBOI_FILEHANDLE
                IF workOrder != NULL
                    info:Result := workOrder:Handle
                ELSE
                    info:Result := IntPtr.Zero
                ENDIF
            CASE DBOI_FILESTREAM
                IF workOrder != NULL
                    info:Result := workOrder:Stream
                ELSE
                    info:Result := NULL
                ENDIF
            CASE DBOI_ISDESC
                IF workOrder != NULL
                    info:Result := workOrder:Descending
                    // Note NTX cannot change descending flag on the fly like DBFCDX can.
                ELSE
                    info:Result := FALSE
                ENDIF
            CASE DBOI_ISCOND
                IF workOrder != NULL
                    info:Result := workOrder:Conditional
                ELSE
                    info:Result := DBNull.Value
                ENDIF
            CASE DBOI_KEYTYPE
                IF workOrder != NULL
                    info:Result := workOrder:KeyExprType
                ELSE
                    info:Result := DBNull.Value
                ENDIF
            CASE DBOI_KEYSIZE
                IF workOrder != NULL
                    info:Result := workOrder:KeyLength
                ELSE
                    info:Result := DBNull.Value
                ENDIF
            CASE DBOI_KEYDEC
                IF workOrder != NULL
                    info:Result := workOrder:KeyDecimals
                ELSE
                    info:Result := 0
                ENDIF
            CASE DBOI_HPLOCKING
                IF workOrder != NULL
                    info:Result := workOrder:HPLocking
                ELSE
                    info:Result := FALSE
                ENDIF
            CASE DBOI_UNIQUE
                IF workOrder != NULL
                    info:Result := workOrder:Unique
                ELSE
                    info:Result := FALSE
                ENDIF
            CASE DBOI_LOCKOFFSET
                IF workOrder != NULL
                    info:Result := workOrder:_LockOffset
                ELSE
                    info:Result := 0
                ENDIF
            CASE DBOI_SETCODEBLOCK
                IF workOrder != NULL
                    info:Result := workOrder:KeyCodeBlock
                ELSE
                    info:Result := DBNull.Value
                ENDIF
            CASE DBOI_KEYVAL
                IF workOrder != NULL
                    isOk := TRUE
                    TRY
                        info:Result := SELF:EvalBlock(workOrder:KeyCodeBlock)
                    CATCH ex AS Exception
                        isOk := FALSE
                        SELF:_dbfError(ex, Subcodes.EDB_EXPRESSION, Gencode.EG_SYNTAX, "DBFNTX.OrderInfo")
                    END TRY
                    IF !isOk
                        info:Result := DBNull.Value
                    ENDIF
                ELSE
                    info:Result := NULL
                ENDIF
            CASE DBOI_SCOPETOPCLEAR
            CASE DBOI_SCOPEBOTTOMCLEAR
                IF workOrder != NULL
                    workOrder:SetOrderScope(info:Result, (DbOrder_Info) nOrdinal)
                ENDIF
                info:Result := DBNull.Value
            CASE DBOI_SCOPETOP
            CASE DBOI_SCOPEBOTTOM
                IF workOrder != NULL
                    LOCAL oldValue AS OBJECT
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
                // Not Supported
                info:Result := FALSE
            CASE DBOI_KEYDELETE
                // Not Supported
                info:Result := FALSE
            CASE DBOI_CUSTOM
                // Not Supported
                info:Result := FALSE
            CASE DBOI_USER + 42
                // Dump Ntx to Txt file
                IF workOrder != NULL
                    workOrder:_dump()
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
                else
                    isOk := super:_RelSeek(currentRelation )
                endif
            ENDIF

            RETURN isOk
            #endregion
        #region Pack, Zap
        OVERRIDE METHOD Pack() AS LOGIC
            LOCAL isOk AS LOGIC

            isOk := SUPER:Pack()
            IF isOk
                isOk := SELF:OrderListRebuild()  .and. RuntimeState.LastRddError == null
            ENDIF
            RETURN isOk

        OVERRIDE METHOD Zap() AS LOGIC
            LOCAL isOk AS LOGIC

            isOk := SUPER:Zap()
            IF isOk
                isOk := SELF:OrderListRebuild()  .and. RuntimeState.LastRddError == null
            ENDIF
            RETURN isOk

        #endregion

        #REGION Open, Close, Create

        OVERRIDE METHOD Close() AS LOGIC
            LOCAL orderInfo AS DbOrderInfo
            BEGIN LOCK SELF
                SELF:GoCold()
                orderInfo := DbOrderInfo{}
                orderInfo:AllTags := TRUE
                SELF:OrderListDelete(orderInfo)
                RETURN SUPER:Close()
            END LOCK

        OVERRIDE METHOD Create( openInfo AS DbOpenInfo ) AS LOGIC
            LOCAL isOk AS LOGIC

            isOk := SUPER:Create(openInfo)
            LOCAL lSupportAnsi := FALSE AS LOGIC
            SWITCH RuntimeState.Dialect
                CASE XSharpDialect.VO
                CASE XSharpDialect.Vulcan
                CASE XSharpDialect.Core
                    lSupportAnsi := TRUE
                OTHERWISE
                    lSupportAnsi := FALSE
            END SWITCH
            IF  XSharp.RuntimeState.Ansi .AND. isOk .and. lSupportAnsi
                VAR sig := SELF:_Header:Version
                //SET bit TO Force ANSI Signature
                sig := sig |4
                //Should we also SET the CodePage ??
                //SELF:_Header:DbfCodePage := DbfCodePage.CP_DBF_WIN_ANSI
                SELF:_Header:Version := sig
            ENDIF
        RETURN isOk
        OVERRIDE METHOD Open(info AS DbOpenInfo) AS LOGIC
            LOCAL lOk AS LOGIC
            lOk := SUPER:Open(info)
            RETURN lOk

            #ENDREGION

        #REGION Move

        INTERNAL METHOD ReadRecord() AS LOGIC
            RETURN SELF:_readRecord()


        OVERRIDE METHOD Seek(seekInfo AS DbSeekInfo ) AS LOGIC
            LOCAL isOk AS LOGIC

            isOk := FALSE
            BEGIN LOCK SELF
                var index := SELF:CurrentOrder
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
                local result as LOGIC
                IF SELF:CurrentOrder != NULL
                    result := SELF:CurrentOrder:GoBottom()
                    SELF:_CheckEofBof()
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
                    SELF:_CheckEofBof()
                ELSE
                    result := SUPER:GoTop()
                ENDIF
                RETURN result
            END LOCK

        METHOD __Goto(nRec AS LONG) AS LOGIC
            // Skip without reset of stack
            RETURN SUPER:GoTo(nRec)

        OVERRIDE METHOD GoTo(nRec AS LONG) AS LOGIC
            local result as LOGIC
            SELF:GoCold()
            IF SELF:CurrentOrder != NULL
                SELF:CurrentOrder:ClearStack() // force to reseek later
            ENDIF
            result := SUPER:GoTo(nRec)
            RETURN result


        OVERRIDE METHOD SkipRaw( move AS LONG ) AS LOGIC
            BEGIN LOCK SELF
                local result as LOGIC
                IF SELF:CurrentOrder != NULL
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

        #ENDREGION
    END CLASS

END NAMESPACE
