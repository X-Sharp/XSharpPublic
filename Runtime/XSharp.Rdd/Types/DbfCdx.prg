//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp.RDD.Support
USING XSharp.RDD.CDX
USING XSharp.RDD.Enums
USING System.IO
BEGIN NAMESPACE XSharp.RDD
    // Inherits all standard DBF and Memo behavior
    // Only adds Order Handling
    /// <summary>DBFCDX RDD. For DBF/FPT/CDX.</summary>
    CLASS DBFCDX INHERIT DBFFPT
        INTERNAL _indexList  AS CdxOrderBagList
        INTERNAL PROPERTY CurrentOrder AS CdxTag GET _indexList:CurrentOrder
        
            CONSTRUCTOR()
                SUPER()
                _indexList := CdxOrderBagList{SELF}
                RETURN
                
            PROPERTY SysName AS STRING GET "DBFCDX"
            #REGION Order Support
            
            VIRTUAL METHOD OrderCreate(orderInfo AS DbOrderCreateInfo ) AS LOGIC
                RETURN SELF:_indexList:Create(orderInfo)
                
            VIRTUAL METHOD OrderDestroy(orderInfo AS DbOrderInfo ) AS LOGIC
                RETURN SELF:_indexList:Destroy(orderInfo)

		    METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
                RETURN SUPER:OrderCondition(info)
                
            VIRTUAL METHOD OrderListAdd( orderInfo AS DbOrderInfo) AS LOGIC
                BEGIN LOCK SELF
                    SELF:GoCold()
                    LOCAL fullPath AS STRING
                    fullPath := orderInfo:BagName
                    IF File(fullPath)
                        fullPath := FPathName()
                    ELSEIF ! String.IsNullOrEmpty(Path.GetExtension(fullPath)) 
                        fullPath := Path.ChangeExtension(fullPath, CdxOrderbag.CDX_EXTENSION)
                        IF File(fullPath)
                            fullPath := FPathName()
                        ENDIF
                    ENDIF
                    IF String.IsNullOrEmpty(System.IO.Path.GetDirectoryName(fullPath))
                        fullPath := System.IO.Path.Combine(SYstem.IO.Path.GetDirectoryName(SELF:_FileName), fullPath)
                        orderinfo:BagName := fullPath
                    ENDIF
                    LOCAL lOk := FALSE AS LOGIC
                    IF SELF:_indexList:FindOrderBag(orderInfo:BagName) == NULL
                        lOk := SELF:_indexList:Add(orderInfo)
                    ELSE
                        // Already open, do nothing
                        lOk := TRUE
                    ENDIF
                    IF lOk
                         orderInfo:Order := 1
                         lOk := SELF:OrderListFocus(orderInfo)
                    ENDIF
                    RETURN lOk
                END LOCK


            METHOD _CloseAllIndexes(orderInfo AS DbOrderInfo, lCloseStructural AS LOGIC) AS LOGIC
                    SELF:GoCold()
                    RETURN SELF:_indexList:Delete(orderInfo, lCloseStructural)

            VIRTUAL METHOD OrderListDelete(orderInfo AS DbOrderInfo) AS LOGIC
                BEGIN LOCK SELF
                    RETURN SELF:_CloseAllIndexes(orderInfo, FALSE)
                END LOCK
                
            VIRTUAL METHOD OrderListFocus(orderInfo AS DbOrderInfo) AS LOGIC
            
                BEGIN LOCK SELF
                    SELF:GoCold()
                    RETURN SELF:_indexList:Focus(orderInfo)
                END LOCK
                
            VIRTUAL METHOD OrderListRebuild() AS LOGIC
                BEGIN LOCK SELF
                    SELF:GoCold()
                    RETURN SELF:_indexList:Rebuild()
                END LOCK
                
                
            OVERRIDE METHOD OrderInfo(nOrdinal AS DWORD , info AS DbOrderInfo ) AS OBJECT
                LOCAL result AS LONG
                LOCAL workOrder AS CdxTag
                LOCAL isOk := FALSE AS LOGIC
                
                result := 0
                workOrder := SELF:_indexList:FindOrder(info)
                IF workOrder == NULL
                    workOrder := SELF:CurrentOrder
                ENDIF
                
                BEGIN SWITCH nOrdinal
            CASE DBOI_CONDITION
                IF workOrder != NULL
                    info:Result := workOrder:Condition
                ENDIF
            CASE DBOI_EXPRESSION
                IF workOrder != NULL
			        info:Result := workOrder:Expression
        	    ENDIF
            CASE DBOI_ORDERCOUNT
                info:Result := SELF:_indexList:Count
            CASE DBOI_POSITION
                IF workOrder == NULL
                    info:Result := SELF:RecNo
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
            CASE DBOI_BAGEXT
                // according to the docs this should always return the default extension and not the actual extension
                info:Result := CdxOrderBag.CDX_EXTENSION
            CASE DBOI_FULLPATH
                IF workOrder != NULL
                    info:Result := workOrder:OrderBag:FullPath
                ELSE
                    info:Result := ""
                ENDIF
            CASE DBOI_BAGNAME
                IF workOrder != NULL
                    info:Result := workOrder:FileName
                ELSE
                    info:Result := ""
                ENDIF
            CASE DBOI_NAME
                IF workOrder != NULL
                    info:Result := workOrder:_orderName
                ELSE
                    info:Result := ""
                ENDIF
            CASE DBOI_FILEHANDLE
                IF workOrder != NULL
                    info:Result := workOrder:OrderBag:Handle
                ELSE
                    info:Result := IntPtr.Zero
                ENDIF
            CASE DBOI_ISDESC
                IF workOrder != NULL
                    info:Result := workOrder:Descending
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
            CASE DBOI_UNIQUE
                IF workOrder != NULL
                    info:Result := workOrder:Unique
                ELSE
                    info:Result := FALSE
                ENDIF
            CASE DBOI_LOCKOFFSET
                IF workOrder != NULL
                    info:Result := workOrder:OrderBag:_lockOffset
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
                        SELF:_dbfError(ex, SubCodes.EDB_EXPRESSION, GenCode.EG_SYNTAX, "DBFCDX.OrderInfo")
                    END TRY
                    IF !isOk
                        info:Result := NULL
                    ENDIF
                ELSE
                    info:Result := NULL
                ENDIF
            CASE DBOI_SCOPETOP
            CASE DBOI_SCOPEBOTTOM
            CASE DBOI_SCOPETOPCLEAR
            CASE DBOI_SCOPEBOTTOMCLEAR
                IF workOrder != NULL
                    IF info:Result != NULL
                        workOrder:SetOrderScope(info:Result, (DbOrder_Info) nOrdinal)
                    ENDIF
                    IF nOrdinal == DBOI_SCOPETOP
                        info:Result := workOrder:TopScope
                    ELSEIF nOrdinal == DBOI_SCOPEBOTTOM
                        info:Result := workOrder:BottomScope
                    ELSE
                        info:Result := NULL
                    ENDIF
                ELSE
                    info:Result := NULL
                ENDIF
            CASE DBOI_USER + 42
                // Dump Cdx to Txt file
                IF workOrder != NULL
                    workOrder:_dump()
                ENDIF
                    
            OTHERWISE
                SUPER:OrderInfo(nOrdinal, info)
            END SWITCH
            RETURN info:Result
            
            #endregion

        #region Pack, Zap
        METHOD Pack() AS LOGIC
            LOCAL isOk AS LOGIC
            isOk := SUPER:Pack()
            IF isOk
                isOk := SELF:OrderListRebuild()
            ENDIF
            RETURN isOk
            
        PUBLIC METHOD Zap() AS LOGIC
            LOCAL isOk AS LOGIC
            isOk := SUPER:Zap()
            IF isOk
                isOk := SELF:OrderListRebuild()
            ENDIF
            RETURN isOk
            
            #endregion
        #REGION Open, Close, Create
        PUBLIC OVERRIDE METHOD Close() AS LOGIC
            LOCAL orderInfo AS DbOrderInfo
            BEGIN LOCK SELF
                orderInfo := DbOrderInfo{}
                orderInfo:AllTags := TRUE
                SELF:_CloseAllIndexes(orderInfo, TRUE)
                RETURN SUPER:Close()
            END LOCK

            
        PUBLIC OVERRIDE METHOD Create( openInfo AS DbOpenInfo ) AS LOGIC
            LOCAL isOk AS LOGIC
            
            isOk := SUPER:Create(openInfo)
            IF  XSharp.RuntimeState.Ansi .AND. isOk
                VAR sig := SELF:_Header:Version
                //SET bit TO Force ANSI Signature
                sig := sig |4
                //Should we also SET the CodePage ??
                //SELF:_Header:DbfCodePage := DbfCodePage.CP_DBF_WIN_ANSI
                SELF:_Header:Version := sig
            ENDIF
            RETURN isOk
            
        METHOD Open(info AS DbOpenInfo) AS LOGIC
            LOCAL lOk AS LOGIC
            lOk := SUPER:Open(info)
            IF lOk
                
                IF RuntimeState.AutoOpen
                    VAR cCdxFileName := System.IO.Path.ChangeExtension(info:FileName, ".CDX")
                    IF System.IO.File.Exists(cCdxFileName)
                        LOCAL orderinfo := DbOrderInfo{} AS DbOrderInfo
                        orderInfo:BagName := cCdxFileName
                        SELF:_indexList:Add(orderInfo, TRUE)
                    ENDIF
                ENDIF
            ENDIF
            RETURN lOk
            #ENDREGION
            
        #REGION Move
        
        INTERNAL METHOD ReadRecord() AS LOGIC
            RETURN SELF:_ReadRecord()
            
            
        PUBLIC METHOD Seek(seekInfo AS DBSEEKINFO ) AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL ntxIndex AS CdxTag
            
            isOk := FALSE
            BEGIN LOCK SELF
                ntxIndex := SELF:CurrentOrder
                IF ntxIndex != NULL
                    isOk := ntxIndex:Seek(seekInfo)
                ENDIF
                IF  !isOk 
                    SELF:_DbfError(SubCodes.ERDD_DATATYPE, GenCode.EG_NOORDER )
                ENDIF
            END LOCK
            RETURN isOk
            
        PUBLIC METHOD GoBottom() AS LOGIC
            BEGIN LOCK SELF
                IF SELF:CurrentOrder != NULL
                    RETURN SELF:CurrentOrder:GoBottom()
                ELSE
                    RETURN SUPER:GoBottom()
                ENDIF
            END LOCK
            
        PUBLIC METHOD GoTop() AS LOGIC
            BEGIN LOCK SELF
                IF SELF:CurrentOrder != NULL
                    RETURN SELF:CurrentOrder:GoTop()
                ELSE
                    RETURN SUPER:GoTop()
                ENDIF
            END LOCK
            
        METHOD __Goto(nRec AS LONG) AS LOGIC
            // Skip without reset of topstack
            RETURN SUPER:Goto(nRec)
            
        METHOD GoTo(nRec AS LONG) AS LOGIC
            SELF:GoCold()
            IF SELF:CurrentOrder != NULL
                SELF:CurrentOrder:ClearStack() // force to reseek later
            ENDIF
            RETURN SUPER:Goto(nRec)
            
            
        PUBLIC METHOD SkipRaw( move AS LONG ) AS LOGIC
            BEGIN LOCK SELF
                IF SELF:CurrentOrder != NULL
                    RETURN SELF:CurrentOrder:SkipRaw(move)
                ELSE
                    RETURN SUPER:SkipRaw(move)
                ENDIF
            END LOCK
            
            #ENDREGION
            
        #REGION GoCold, GoHot, Flush
        PUBLIC OVERRIDE METHOD GoCold() AS LOGIC
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
            
        PUBLIC OVERRIDE METHOD GoHot() AS LOGIC
            LOCAL isOk AS LOGIC
            
            isOk := TRUE
            BEGIN LOCK SELF
                isOk := SUPER:GoHot()
                IF !isOk
                    RETURN isOk
                ENDIF
                RETURN SELF:_indexList:GoHot()
            END LOCK
            
        PUBLIC OVERRIDE METHOD Flush() AS LOGIC
            LOCAL isOk AS LOGIC
            
            isOk := TRUE
            BEGIN LOCK SELF
                isOk := SUPER:Flush()
                RETURN SELF:_indexList:Flush()
            END LOCK
            
        #ENDREGION
    END CLASS    
    
END NAMESPACE
