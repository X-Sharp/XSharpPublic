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
        VIRTUAL PROPERTY Driver AS STRING GET "DBFNTX"
            CONSTRUCTOR()
                SUPER()
                SELF:_indexList := NtxOrderList{SELF}
                SELF:_oIndex 	:= NULL
                RETURN
                
            
            #REGION Order Support 
            VIRTUAL METHOD OrderCreate(orderInfo AS DBORDERCREATEINFO ) AS LOGIC
                RETURN SELF:_indexList:Create(orderInfo)
                
            VIRTUAL METHOD OrderDestroy(orderInfo AS DBORDERINFO ) AS LOGIC
                RETURN SUPER:OrderDestroy(orderInfo)
                
            METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
                RETURN SUPER:OrderCondition(info)
                
            VIRTUAL METHOD OrderListAdd( orderInfo AS DbOrderInfo) AS LOGIC
                BEGIN LOCK SELF
                    SELF:GoCold()
                    LOCAL fullPath AS STRING
                    fullPath := orderInfo:BagName
                    IF String.IsNullOrEmpty(System.IO.Path.GetDirectoryName(fullPath))
                        fullPath := System.IO.Path.Combine(System.IO.Path.GetDirectoryName(SELF:_FileName), fullPath)
                    ENDIF
                    VAR lOk := SELF:_indexList:Add(orderInfo, fullPath)
                    SELF:_oIndex := SELF:_indexList:CurrentOrder
                    RETURN lOk
                END LOCK
                
            VIRTUAL METHOD OrderListDelete(orderInfo AS DbOrderInfo) AS LOGIC
            
                BEGIN LOCK SELF
                
                    SELF:GoCold()
                    RETURN SELF:_indexList:CloseAll()
                END LOCK            
                
            VIRTUAL METHOD OrderListFocus(orderInfo AS DbOrderInfo) AS LOGIC
                BEGIN LOCK SELF
                    SELF:GoCold()
                    RETURN SELF:_indexList:SetFocus(orderInfo)
                END LOCK
                
            VIRTUAL METHOD OrderListRebuild() AS LOGIC
                BEGIN LOCK SELF
                    IF SELF:Shared 
                        // Error !! Cannot be written !
                        SELF:_DbfError( ERDD.SHARED, XSharp.Gencode.EG_SHARED )
                        RETURN FALSE
                    ENDIF
	        IF SELF:_Readonly
	            SELF:_DbfError( ERDD.READONLY, XSharp.Gencode.EG_READONLY)
	            RETURN FALSE
	        ENDIF
                    SELF:GoCold()
                    RETURN SELF:_indexList:Rebuild()
                END LOCK
                
            OVERRIDE METHOD OrderInfo(nOrdinal AS DWORD , info AS DBORDERINFO ) AS OBJECT
                LOCAL result AS LONG
                LOCAL workOrder AS NtxOrder
                LOCAL orderPos AS LONG
                LOCAL isOk := FALSE AS LOGIC
                
                result := 0
                workOrder := NULL
                orderPos := SELF:_indexList:FindOrder(info)
                IF orderPos <= 0
                    IF info:IsEmpty
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
                info:Result := NtxOrder.NTX_EXTENSION
            CASE DBOI_FULLPATH
                IF workOrder != NULL
                    info:Result := workOrder:FullPath
                ELSE
                    info:Result := ""
                ENDIF
            CASE DBOI_BAGNAME
                //CASE DBOI_INDEXNAME // alias
                IF workOrder != NULL
                    info:Result := System.IO.Path.GetFileName(workOrder:FullPath)
                ELSE
                    info:Result := ""
                ENDIF
            CASE DBOI_NAME
                IF workOrder != NULL
                    info:Result := workOrder:OrderName
                ELSE
                    info:Result := ""
                ENDIF
            CASE DBOI_FILEHANDLE
                IF workOrder != NULL
                    info:Result := workOrder:Handle
                ELSE
                    info:Result := IntPtr.Zero
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
                        info:Result := FALSE
                ENDIF
            CASE DBOI_KEYTYPE
                    IF workOrder != NULL
                        info:Result := workOrder:KeyExprType
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
                        info:Result := workOrder:_lockOffset
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
                            SELF:_dbfError(ex, SubCodes.EDB_EXPRESSION, GenCode.EG_SYNTAX, "DBFNTX.OrderInfo")
                        END TRY
                        IF !isOk
                            info:Result := NULL
                        ENDIF
                    ELSE
                        info:Result := NULL
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
                        IF info:Result != NULL 
                            workOrder:SetOrderScope(info:Result, (DbOrder_Info) nOrdinal)
                        ENDIF
                        IF nOrdinal == DBOI_SCOPETOP
                            info:Result := workOrder:TopScope
                        ELSEIF nOrdinal == DBOI_SCOPEBOTTOM
                            info:Result := workOrder:BottomScope
                        ENDIF
                    ELSE
                        info:Result := NULL
                ENDIF
    CASE DBOI_KEYADD
		 info:Result := FALSE
    CASE DBOI_KEYDELETE
		 info:Result := FALSE
    CASE DBOI_CUSTOM
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
        METHOD ForceRel() AS LOGIC
            LOCAL isOk    := TRUE AS LOGIC
            IF SELF:_RelInfoPending != NULL
                // Save the current context
                LOCAL currentRelation := SELF:_RelInfoPending AS DbRelInfo
                SELF:_RelInfoPending := NULL
                VAR oParent := (DBF) currentRelation:Parent 
                IF oParent:_EOF
                    //
                    isOk := SELF:Goto( 0 )
                ELSE
                    isOk := SELF:RelEval( currentRelation )

                    IF isOk .AND. !((DBF)currentRelation:Parent):_Eof
                        TRY
                            LOCAL seekInfo AS DBSEEKINFO
                            seekInfo := DbSeekInfo{}
                            seekInfo:Value := SELF:_EvalResult
                            seekInfo:SoftSeek := FALSE
                            isOk := SELF:Seek(seekInfo)
                            
                        CATCH ex AS InvalidCastException
                            SELF:_dbfError(ex, SubCodes.ERDD_DATATYPE,GenCode.EG_DATATYPE,  "DBFNTX.ForceRel") 

                        END TRY
                    ENDIF
                ENDIF
            ENDIF
   
        RETURN isOk
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
                SELF:GoCold()
                orderInfo := DbOrderInfo{}
                orderInfo:AllTags := TRUE
                SELF:OrderListDelete(orderInfo)
                RETURN SUPER:Close()
            END LOCK
            
        PUBLIC OVERRIDE METHOD Create( openInfo AS DBOPENINFO ) AS LOGIC
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
            RETURN lOk

        #ENDREGION
        
        #REGION Move
        
        INTERNAL METHOD ReadRecord() AS LOGIC
            RETURN SELF:_ReadRecord()
            
            
        PUBLIC METHOD Seek(seekInfo AS DBSEEKINFO ) AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL ntxIndex AS NtxOrder
            
            isOk := FALSE
            BEGIN LOCK SELF
                ntxIndex := SELF:CurrentOrder
                IF ntxIndex != NULL
                    isOk := ntxIndex:Seek(seekInfo)
                ENDIF
                IF  !isOk 
                    SELF:_DbfError(SubCodes.ERDD_DATATYPE, GenCode.EG_NOORDER )
                ENDIF
        SELF:_CheckEofBof()
            END LOCK
            RETURN isOk
            
        PUBLIC METHOD GoBottom() AS LOGIC
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
            
        PUBLIC METHOD GoTop() AS LOGIC
            BEGIN LOCK SELF
        local result as LOGIC    
        IF SELF:CurrentOrder != NULL
            result := SELF:CurrentOrder:GoTop()
	    SELF:_CheckEofBof()
        ELSE
            result := SUPER:GoTop()
        ENDIF
        RETURN result
            END LOCK
            
        METHOD __Goto(nRec AS LONG) AS LOGIC
            // Skip without reset of topstack
            RETURN SUPER:Goto(nRec)
            
        METHOD GoTo(nRec AS LONG) AS LOGIC
    local result as LOGIC    
    SELF:GoCold()
    IF SELF:CurrentOrder != NULL
        SELF:CurrentOrder:ClearStack() // force to reseek later
    ENDIF
    result := SUPER:Goto(nRec)
    RETURN result
            
            
        PUBLIC METHOD SkipRaw( move AS LONG ) AS LOGIC
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
                RETURN SELF:_indexList:Flush() .AND. isOk
            END LOCK
            
        #ENDREGION
    END CLASS
    
END NAMESPACE
