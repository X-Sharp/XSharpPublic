//
// Copyright (c) B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp.RDD.Support
USING XSharp.RDD.NTX


BEGIN NAMESPACE XSharp.RDD
	/// <summary>DBFNTX RDD. For DBF/DBT/NTX.</summary>
	CLASS DBFNTX INHERIT DBFDBT
	
		INTERNAL _indexList AS NtxOrderList
		INTERNAL PROPERTY CurrentOrder AS NtxOrder GET _indexList:CurrentOrder
		CONSTRUCTOR()
			SUPER()
			SELF:_indexList := NtxOrderList{SELF}
			SELF:_oIndex 	:= NULL
			RETURN
			
		PROPERTY SysName AS STRING GET TYPEOF(DbfNtx):ToString()	
		
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
				SELF:GoCold()
				RETURN SELF:_indexList:Rebuild()
			END LOCK
			
		OVERRIDE METHOD OrderInfo(nOrdinal AS DWORD , info AS DBORDERINFO ) AS OBJECT
			LOCAL isOk AS LOGIC
			LOCAL result AS LONG
			LOCAL workOrder AS NtxOrder
			LOCAL orderPos AS LONG
			LOCAL oldvalue AS OBJECT

			isOk := TRUE
			result := 0
			workOrder := NULL
			orderPos := SELF:_indexList:FindOrder(info)
			IF orderPos <= 0 
				workOrder := SELF:CurrentOrder
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
				ENDIF
				IF isOk
					info:Result := result
				ENDIF
			CASE DBOI_NUMBER
                info:Result := SELF:_indexList:OrderPos(workOrder)

			CASE DBOI_BAGEXT
				info:Result := ".NTX"
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
					info:Result := workOrder:_hFile
				ENDIF
			CASE DBOI_ISDESC
				IF workOrder != NULL
					info:Result := workOrder:_Descending
				ENDIF
			CASE DBOI_ISCOND
				IF workOrder != NULL
					info:Result := workOrder:_Conditional
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
					info:Result := workOrder:_keyDecimals
				ENDIF
			CASE DBOI_HPLOCKING
				IF workOrder != NULL
					info:Result := workOrder:_HPLocking
				ENDIF
			CASE DBOI_UNIQUE
				IF workOrder != NULL
					info:Result := workOrder:_Unique
				ENDIF
			CASE DBOI_LOCKOFFSET
				IF workOrder != NULL
					info:Result := workOrder:_lockOffset
				ENDIF
			CASE DBOI_SETCODEBLOCK
				IF workOrder != NULL
					oldvalue := workOrder:_KeyCodeBlock
					IF info:Result != NULL
						workOrder:_KeyCodeBlock := (ICodeblock)info:Result
					ENDIF
					info:Result := oldvalue
				ENDIF
			CASE DBOI_KEYVAL
				IF workOrder != NULL
					isOk := TRUE
					TRY
						info:Result := SELF:EvalBlock(workOrder:_KeyCodeBlock)
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
