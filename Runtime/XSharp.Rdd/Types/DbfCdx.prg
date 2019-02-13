//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp.RDD.Support
USING XSharp.RDD.CDX
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
            
        PROPERTY SysName AS STRING GET typeof(DBFCDX):ToString()
		#REGION Order Support

		VIRTUAL METHOD OrderCreate(orderInfo AS DBORDERCREATEINFO ) AS LOGIC
			RETURN SELF:_indexList:Create(orderInfo)

		VIRTUAL METHOD OrderDestroy(orderInfo AS DBORDERINFO ) AS LOGIC
			RETURN SUPER:OrderDestroy(orderInfo)

		VIRTUAL METHOD OrderListAdd( orderInfo AS DbOrderInfo) AS LOGIC
			BEGIN LOCK SELF
				SELF:GoCold()
				LOCAL fullPath AS STRING
				fullPath := orderInfo:BagName
				IF String.IsNullOrEmpty(System.IO.Path.GetDirectoryName(fullPath))
					fullPath := System.IO.Path.Combine(SYstem.IO.Path.GetDirectoryName(SELF:_FileName), fullPath)
                    orderinfo:BagName := fullPath
				ENDIF
				VAR lOk := SELF:_indexList:Add(orderInfo)
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
				SELF:GoCold()
				RETURN SELF:_indexList:Rebuild()
			END LOCK


		OVERRIDE METHOD OrderInfo(nOrdinal AS DWORD , info AS DBORDERINFO ) AS OBJECT
			LOCAL isOk AS LOGIC
			LOCAL result AS LONG
			LOCAL workOrder AS CdxTag
			LOCAL oldvalue AS OBJECT

			isOk := TRUE
			result := 0
			workOrder := SELF:_indexList:FindOrder(info)

			BEGIN SWITCH nOrdinal
			CASE DBOI_CONDITION
				IF workOrder != NULL
					info:Result := workOrder:ForExpression
				ENDIF
			CASE DBOI_EXPRESSION
				IF workOrder != NULL
					info:Result := workOrder:KeyExpression
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
				ENDIF
				IF isOk
					info:Result := result
				ENDIF
			CASE DBOI_NUMBER
				info:Result := SELF:_indexList:OrderPos(workOrder)
			CASE DBOI_BAGEXT
                IF workOrder != NULL
                    info:Result := System.IO.Path.GetExtension(workOrder:OrderBag:FileName)
                ELSE
				    info:Result := ".CDX"
                ENDIF
			CASE DBOI_FULLPATH
				IF workOrder != NULL
					info:Result := workOrder:FileName
				ENDIF
			CASE DBOI_BAGNAME
				IF workOrder != NULL
					info:Result := System.IO.Path.GetFileNameWithoutExtension(workOrder:FileName)
				ENDIF
			CASE DBOI_NAME
				IF workOrder != NULL
					info:Result := workOrder:_orderName
				ENDIF
			CASE DBOI_FILEHANDLE
				IF workOrder != NULL
					info:Result := workOrder:OrderBag:Handle
				ENDIF
			CASE DBOI_ISDESC
				IF workOrder != NULL
					info:Result := workOrder:_Descending
				ENDIF
			CASE DBOI_ISCOND
				IF workOrder != NULL
					info:Result := workOrder:Conditional
				ENDIF
			CASE DBOI_KEYTYPE
				IF workOrder != NULL
					info:Result := workOrder:_KeyExprType
				ENDIF
			CASE DBOI_KEYSIZE
				IF workOrder != NULL
					info:Result := workOrder:_keySize
				ENDIF
			CASE DBOI_KEYDEC
				IF workOrder != NULL
					info:Result := workOrder:keyDecimals
				ENDIF
			CASE DBOI_UNIQUE
				IF workOrder != NULL
					info:Result := workOrder:Unique
				ENDIF
			CASE DBOI_LOCKOFFSET
				IF workOrder != NULL
					info:Result := workOrder:OrderBag:_lockOffset
				ENDIF
			CASE DBOI_SETCODEBLOCK
				IF workOrder != NULL
					oldvalue := workOrder:KeyBlock
					IF info:Result != NULL
						workOrder:KeyBlock := (ICodeblock)info:Result
					ENDIF
					info:Result := oldvalue
				ENDIF
			CASE DBOI_KEYVAL
				IF workOrder != NULL
					isOk := TRUE
					TRY
						info:Result := SELF:EvalBlock(workOrder:KeyBlock)
					CATCH
						isOk := FALSE
					END TRY
					IF !isOk
						info:Result := NULL
					ENDIF
				ENDIF
			CASE DBOI_SCOPETOP
				IF workOrder != NULL
					IF info:Result != NULL
						workOrder:SetOrderScope(info:Result, XSharp.RDD.Enums.DbOrder_Info.DBOI_SCOPETOP)
					ENDIF
					info:Result := workOrder:_topScope
				ENDIF
			CASE DBOI_SCOPEBOTTOM
				IF workOrder != NULL
					IF info:Result != NULL
						workOrder:SetOrderScope(info:Result, XSharp.RDD.Enums.DbOrder_Info.DBOI_SCOPEBOTTOM)
					ENDIF
					info:Result := workOrder:_bottomScope
				ENDIF
			CASE DBOI_SCOPETOPCLEAR
				IF workOrder != NULL
					workOrder:_hasTopScope := FALSE
					workOrder:_topScope := NULL
				ENDIF
			CASE DBOI_SCOPEBOTTOMCLEAR
				IF workOrder != NULL
					workOrder:_hasBottomScope := FALSE
					workOrder:_bottomScope := NULL
				ENDIF
            CASE DBOI_USER + 42
                // Dump Ntx to Txt file
				IF workOrder != NULL
					workOrder:_dump()
				ENDIF

			OTHERWISE
				isOk := (LOGIC)SUPER:OrderInfo(nOrdinal, info)
			END SWITCH
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

			orderInfo := DbOrderInfo{}
			orderInfo:AllTags := TRUE
			SELF:OrderListDelete(orderInfo)
			RETURN SUPER:Close()


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

	        METHOD Open(info AS XSharp.RDD.Support.DbOpenInfo) AS LOGIC
	            LOCAL lOk AS LOGIC
	            lOk := SUPER:Open(info)
	            IF lOk
	                VAR cCdxFileName := System.IO.Path.ChangeExtension(info:FileName, ".CDX")
	                IF RuntimeState.AutoOpen .AND. System.IO.File.Exists(cCdxFileName)
                        LOCAL orderinfo := DbOrderInfo{} AS XSharp.RDD.Support.DbOrderInfo
                        orderInfo:BagName := cCdxFileName
	                    lOk := _indexList:Add(orderinfo)
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
                SELF:CurrentOrder:_TopStack := 0    // force to reseek later
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
