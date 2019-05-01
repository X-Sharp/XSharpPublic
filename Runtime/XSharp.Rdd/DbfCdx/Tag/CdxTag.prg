//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Collections
USING System.Collections.Generic
USING System.Diagnostics
USING System.Globalization
USING System.IO
USING System.Reflection
USING System.Text
USING System.Threading
USING XSharp.RDD.Enums
USING XSharp.RDD.Support

BEGIN NAMESPACE XSharp.RDD.CDX

    DELEGATE CompareFunc(aLHS AS BYTE[], aRHS AS BYTE[], nLength AS LONG) AS LONG


    [DebuggerDisplay("Tag: {OrderName}, Key: {Expression}, For: {Condition}")];
    INTERNAL PARTIAL CLASS CdxTag
        #region constants
        PRIVATE CONST MAX_KEY_LEN       := 240  AS WORD
        #endregion
        #region fields
        PRIVATE _Encoding AS Encoding
        PRIVATE _Hot AS LOGIC
        PRIVATE _Conditional AS LOGIC
        PRIVATE _Descending AS LOGIC
        PRIVATE _Unique AS LOGIC
        PRIVATE _Custom AS LOGIC
        PRIVATE _SingleField AS LONG            // 0 based FieldIndex
        PRIVATE _SourceIndex AS LONG
        PRIVATE _KeyCodeBlock AS ICodeblock
        PRIVATE _ForCodeBlock AS ICodeblock
        PRIVATE _KeyExpr AS STRING
        PRIVATE _ForExpr AS STRING
        PRIVATE _currentvalue AS RddKeyData
        INTERNAL _newvalue     AS RddKeyData
        PRIVATE _newKeyLen AS LONG
        PRIVATE _KeyExprType AS LONG
        PRIVATE _keySize AS WORD
        PRIVATE _rootPage AS LONG
        PRIVATE _ordCondInfo AS DbOrderCondInfo
        //INTERNAL _tagNumber AS INT
        INTERNAL _orderName AS STRING
        INTERNAL _Ansi AS LOGIC
        // Scopes
        PRIVATE DIM _Scopes[2] AS ScopeInfo
        // siblings
        PRIVATE _oRdd   AS DbfCdx
        PRIVATE _Header AS CdxTagHeader

        PRIVATE _stack          AS CdxPageStack
        INTERNAL __Compare       AS CompareFunc

        PRIVATE _bag            AS CdxOrderBag
        PRIVATE getKeyValue     AS ValueBlock       // Delegate to calculate the key

        PRIVATE _maxKeysPerPage AS WORD

#endregion



#region Properties
        INTERNAL PROPERTY Expression        AS STRING GET _KeyExpr
        INTERNAL PROPERTY Encoding          AS Encoding GET _Encoding
        INTERNAL PROPERTY RDD               AS Dbfcdx GET _oRDD
        INTERNAL PROPERTY Condition         AS STRING GET _ForExpr
        INTERNAL PROPERTY OrderName         AS STRING GET _orderName
	    INTERNAL PROPERTY Shared 	        AS LOGIC GET _bag:Shared
        INTERNAL PROPERTY _Recno 	        AS LONG GET _oRDD:Recno
        INTERNAL PROPERTY FileName 	        AS STRING GET _bag:FullPath
        INTERNAL PROPERTY OrderBag          AS CdxOrderBag GET SELF:_bag
        INTERNAL PROPERTY Page              AS Int32 AUTO
        INTERNAL PROPERTY Descending        AS LOGIC GET _Descending SET _Descending := VALUE
        INTERNAL PROPERTY IsConditional     AS LOGIC GET Options:HasFlag(CdxOptions.HasFor)
        INTERNAL PROPERTY IsHot             AS LOGIC GET _Hot
        INTERNAL PROPERTY Header            AS CdxTagHeader GET _Header
        INTERNAL PROPERTY KeyType        	AS INT GET SELF:_KeyExprType
        INTERNAL PROPERTY KeyCodeBlock      AS ICodeBlock GET _KeyCodeBlock
        INTERNAL PROPERTY KeyLength         AS WORD GET SELF:_keySize
        INTERNAL PROPERTY Partial        	AS LOGIC GET SELF:Custom
        INTERNAL PROPERTY Conditional       AS LOGIC GET !String.IsNullOrEmpty(_ForExpr)
        INTERNAL PROPERTY Custom         	AS LOGIC GET Options:HasFlag(CdxOptions.Custom)
        INTERNAL PROPERTY Unique         	AS LOGIC GET Options:HasFlag(CdxOptions.Unique)
        INTERNAL PROPERTY Signature      	AS BYTE AUTO
        INTERNAL PROPERTY Options        	AS CdxOptions AUTO
        INTERNAL PROPERTY LockOffSet     	AS LONG AUTO
        INTERNAL PROPERTY CurrentStack      AS CdxStackEntry GET  SELF:_stack:Top
        INTERNAL PROPERTY RootPage          AS LONG AUTO
        INTERNAL PROPERTY MaxKeysPerPage    AS WORD GET _maxKeysPerPage
        INTERNAL PROPERTY Stack             AS CdxPageStack GET _stack

        // Scopes
        INTERNAL PROPERTY HasTopScope AS LOGIC GET _Scopes[IIF(DESCENDING,BOTTOMSCOPE, TOPSCOPE)]:IsSet
        INTERNAL PROPERTY TopScope AS OBJECT GET _Scopes[IIF(DESCENDING,BOTTOMSCOPE, TOPSCOPE)]:Value
        INTERNAL PROPERTY HasBottomScope AS LOGIC GET _Scopes[IIF(DESCENDING,TOPSCOPE, BOTTOMSCOPE)]:IsSet
        INTERNAL PROPERTY BottomScope AS OBJECT GET _Scopes[IIF(DESCENDING,TOPSCOPE, BOTTOMSCOPE)]:VALUE
        INTERNAL PROPERTY HasScope AS LOGIC GET _Scopes[0]:IsSet .OR. _Scopes[1]:IsSet
        PRIVATE PROPERTY TopScopeNo AS LONG GET IIF(SELF:Descending, BOTTOMSCOPE, TOPSCOPE)
        PRIVATE PROPERTY BottomScopeNo AS LONG GET IIF(SELF:Descending, TOPSCOPE, BOTTOMSCOPE)

#endregion


        PRIVATE METHOD _InitFields(oBag AS CdxOrderBag) AS VOID
            SELF:_bag           := oBag
            SELF:_oRDD          := oBag:_oRDD
            SELF:_stack         := CdxPageStack{SELF}
            SELF:_Encoding      := _oRDD:_Encoding
  
            SELF:_SingleField   := -1
            SELF:_currentValue := RddKeyData{MAX_KEY_LEN}
            SELF:_newValue     := RddKeyData{MAX_KEY_LEN}


        // Constructor for Creation of tags
        INTERNAL CONSTRUCTOR (oBag AS CdxOrderBag)
	        SUPER()
            _InitFields(oBag)



        // Constructor for Opening of tags
        INTERNAL CONSTRUCTOR (oBag AS CdxOrderBag, nPage AS Int32, cName AS STRING)
	        SUPER()
            _InitFields(oBag)
            SELF:_orderName := cName
            SELF:Page := nPage
            SELF:_Header := CdxTagHeader{oBag, nPage,SELF:OrderName}
            SELF:_Bag:SetPage(SELF:_Header)
            SELF:Open()


	    METHOD Open() AS LOGIC
            SELF:_Header:Read()
            SELF:_KeyExpr := SELF:_Header:KeyExpression
            SELF:_ForExpr := SELF:_Header:ForExpression
            SELF:_oRdd:GoTo(1)
            SELF:_Hot := FALSE
            SELF:ClearStack()
            SELF:_keySize       := SELF:_Header:KeySize
            SELF:Options        := SELF:_Header:Options
            SELF:Signature      := SELF:_Header:Signature
            SELF:_Descending    := SELF:_Header:Descending
            SELF:_rootPage      := SELF:_Header:RootPage
            IF ! SELF:EvaluateExpressions()
                RETURN FALSE
            ENDIF
            SELF:AllocateBuffers()
            RETURN TRUE

        INTERNAL METHOD AllocateBuffers() AS VOID
            SELF:_newValue          := RddKeyData{_keySize}
            SELF:_currentValue      := RddKeyData{_keySize}
            SELF:_Scopes[0]:SetBuffer(_keySize)
            SELF:_Scopes[1]:SetBuffer(_keySize)
            RETURN


        INTERNAL METHOD SetLeafProperties(page as CdxLeafPage) as VOID
            VAR numRecs     := SELF:RDD:RecCount
            page:ClearRecordsAndKeys()
            page:KeyLength      := SELF:KeyLength
            VAR bits            := CdxHelpers.GetBits(SELF:KeyLength)
            DO CASE
            CASE numRecs < 2^12
                page:RecordBits     := 12
            CASE numRecs < 2^16
                page:RecordBits     := 16
            CASE numRecs < 2^24
                page:RecordBits     := 24
            OTHERWISE
                page:RecordBits     := 32
            ENDCASE
            var totalBits       := page:RecordBits + bits + bits
            DO CASE
            CASE totalBits    <= 24
                page:DataBytes := 3
            CASE totalBits    <= 32
                page:DataBytes := 4
            CASE totalBits    <= 40
                page:DataBytes := 5
            OTHERWISE
                page:DataBytes := 6
            ENDCASE
            
            page:DuplicateBits  := bits
            page:TrailingBits   := bits
            page:TrailingMask   := (BYTE) (( 1 << bits  ) - 1)
            page:DuplicateMask  := (BYTE) (( 1 << bits  ) - 1)
            page:RecnoMask      := (1 << page:RecordBits) -1
            RETURN

        INTERNAL METHOD EvaluateExpressions() AS LOGIC
            LOCAL evalOk AS LOGIC
            LOCAL oKey AS OBJECT
            evalOk := TRUE
            TRY
                SELF:_KeyCodeBlock := SELF:_oRdd:Compile(SELF:_KeyExpr)
            CATCH ex AS Exception
                SELF:_oRdd:_dbfError( ex, SubCodes.EDB_EXPRESSION, GenCode.EG_SYNTAX,"DBFCDX.Compile")
                RETURN FALSE
            END TRY

            TRY
                oKey := SELF:_oRdd:EvalBlock(SELF:_KeyCodeBlock)
            CATCH ex AS Exception
                SELF:_oRdd:_dbfError( ex, SubCodes.EDB_EXPRESSION, GenCode.EG_SYNTAX, "DBFCDX.Compile")
                evalOk := FALSE
                oKey := NULL
            END TRY
            IF !evalOk
                RETURN FALSE
            ENDIF
            SELF:_KeyExprType := SELF:_oRdd:_getUsualType(oKey)
            IF SELF:_KeyExprType == __UsualType.String
                SELF:__Compare := _compareText
            ELSE
                SELF:__Compare := _compareBin
            ENDIF

            // If the Key Expression contains only a Field Name
            SELF:_SingleField := SELF:_oRDD:FieldIndex(SELF:_KeyExpr) -1
            SELF:_SourceIndex := 0
            LOCAL isOk AS LOGIC
            IF SELF:_SingleField >= 0
                SELF:_SourceIndex   := SELF:_oRdd:_fields[_SingleField]:OffSet
                VAR fType           := SELF:_oRdd:_fields[_SingleField]:FieldType
                SWITCH fType
                CASE DbFieldType.Number
                    SELF:_keySize   := 8
                    SELF:getKeyValue := _getNumFieldValue
                CASE DbFieldType.Date
                    // CDX converts the Date to a Julian Number and stores that as a Real8 in the index
                    SELF:_keySize   := 8
                    SELF:getKeyValue := _getDateFieldValue
                OTHERWISE
                    SELF:_keySize   :=  (WORD) SELF:_oRdd:_fields[_SingleField]:Length
                    SELF:getKeyValue := _getFieldValue
                END SWITCH
                isOk := TRUE
            ELSE
                SELF:_keySize    := 0
                SELF:getKeyValue := _getExpressionValue
                isOk             := SELF:_determineSize(oKey)
            ENDIF
            IF ! isOk
                RETURN FALSE
            ENDIF
            SELF:AllocateBuffers()
            SELF:_maxKeysPerPage    := CdxBranchPage.MaxKeysPerPage(_keySize)
            SELF:_Conditional       := FALSE
            IF SELF:_ForExpr:Length > 0
                TRY
                    SELF:_ForCodeBlock := SELF:_oRdd:Compile(SELF:_ForExpr)
                CATCH ex AS Exception
                    SELF:_oRdd:_dbfError( ex, SubCodes.EDB_EXPRESSION, GenCode.EG_SYNTAX,"DBFNTX.Compile")
                    RETURN FALSE
                END TRY
                SELF:_oRdd:GoTo(1)
                evalOk := TRUE
                TRY
                    VAR oValue := SELF:_oRdd:EvalBlock(SELF:_ForCodeBlock)
                    evalOk     := SELF:_oRdd:_getUsualType(oValue) == __UsualType.Logic
                CATCH ex AS Exception
                    SELF:_oRdd:_dbfError(ex, SubCodes.EDB_EXPRESSION,GenCode.EG_SYNTAX,  "DBFNTX.Compile") 
                    evalOk := FALSE
                END TRY
                IF !evalOk
                    RETURN FALSE
                ENDIF
                SELF:_Conditional := TRUE
            ENDIF
 

            RETURN isOk

        PUBLIC METHOD Flush() AS LOGIC
            /*
            IF !SELF:_Shared .AND. SELF:_Hot 
                SELF:GoCold()
                SELF:_PageList:Flush(TRUE)
                SELF:_Header:IndexingVersion        := 1
                SELF:_Header:NextUnusedPageOffset   := SELF:_nextUnusedPageOffset
                SELF:_Header:FirstPageOffset        := SELF:_rootPage
                SELF:_Header:Write( )
            ENDIF
            FFlush( SELF:_hFile )
            */
            RETURN TRUE

        PUBLIC METHOD Commit() AS LOGIC
            /*
            SELF:GoCold()
            IF !SELF:_Shared .AND. SELF:_Hot .AND. SELF:_hFile != F_ERROR
                SELF:_Header:IndexingVersion        := 1
                SELF:_Header:NextUnusedPageOffset   := SELF:_nextUnusedPageOffset
                SELF:_Header:FirstPageOffset        := SELF:_rootPage
                SELF:_Header:Write( )
            ENDIF
            FFlush( SELF:_hFile )
            */ 
            RETURN TRUE
            
        PUBLIC METHOD Close() AS LOGIC
            LOCAL lOk AS LOGIC
            lOk := SELF:GoCold()
            IF lOk
                lOk := SELF:Flush()
            ENDIF
            RETURN TRUE

        PUBLIC METHOD GoCold() AS LOGIC
            IF SELF:_oRDD:IsHot
                RETURN SELF:_KeyUpdate( SELF:_RecNo, SELF:_oRDD:IsNewRecord )
            ENDIF
            RETURN TRUE

        INTERNAL METHOD GoHot() AS LOGIC
            // Is called to save the current for value and key value
            RETURN _saveCurrentKey(SELF:_oRdd:RecNo, SELF:_currentValue)

        INTERNAL METHOD GetPage(nPage AS LONG) AS CdxTreePage
            IF nPage == -1
                RETURN NULL
            ENDIF
            VAR page := SELF:_bag:GetPage(nPage, SELF:_KeySize, SELF)
            page:Tag := SELF
            RETURN page ASTYPE CdxTreePage

        INTERNAL STATIC METHOD _compareText(aLHS AS BYTE[], aRHS AS BYTE[], nLength AS LONG) AS LONG
            IF aRHS == NULL
                RETURN 0
            ENDIF
            RETURN RuntimeState.StringCompare(aLHS, aRHS, nLength)

        INTERNAL STATIC METHOD _compareBin(aLHS AS BYTE[], aRHS AS BYTE[], nLength AS LONG) AS LONG
            IF aRHS == NULL
                RETURN 0
            ENDIF
            FOR VAR nI := 0 TO nLength -1
                VAR bL := aLHS[nI]
                VAR bR := aRHS[nI]
                IF bL < bR
                    RETURN -1
                ELSEIF bl > bR
                    RETURN 1
                ENDIF
            NEXT
            RETURN 0

        PUBLIC METHOD SetOffLine() AS VOID
            SELF:ClearStack()

        PRIVATE METHOD _saveCurrentRecord( node AS CdxNode) AS VOID
            IF SELF:_currentValue:Recno != node:Recno
                SELF:_currentValue:Recno := node:Recno
                Array.Copy(node:KeyBytes, SELF:_currentValue:Key, _keySize)
            ENDIF

        PRIVATE STATIC METHOD _toJulian(dt AS DateTime) AS LONG
            VAR baseDate  := DateTime{1901, 1, 1 }
            VAR days      := dt:Subtract( baseDate )
            RETURN Convert.ToInt32( days:TotalDays ) + 2415386

        PRIVATE METHOD _ToString( toConvert AS OBJECT , sLen AS LONG ,  buffer AS BYTE[] ) AS LOGIC    
            LOCAL resultLength AS LONG
            resultLength := 0
            RETURN SELF:_ToString( toConvert, sLen, buffer,  REF resultLength)

        PRIVATE METHOD _ToString( toConvert AS OBJECT , sLen AS LONG ,  buffer AS BYTE[] ,  resultLength REF LONG ) AS LOGIC
            LOCAL text AS STRING
            LOCAL typeCde AS TypeCode
            text := NULL
            IF (toConvert ASTYPE IFloat) != NULL // Float Value ?
                typeCde   := TypeCode.Double
            ELSEIF (toConvert ASTYPE IDate) != NULL // Date Value
                VAR valueDate := (IDate)toConvert
                toconvert := DateTime{valueDate:Year, valueDate:Month, valueDate:Day}
                typeCde   := TypeCode.DateTime
            ELSE
                typeCde := Type.GetTypeCode(toConvert:GetType())
            ENDIF
            IF typeCde == TypeCode.DateTime
                // Convert to Julian number and then process as normal numeric value
                toConvert     := _toJulian((DateTime) toConvert)
                typeCde       := TypeCode.Int32
            ENDIF
            SWITCH typeCde
            CASE TypeCode.Int16
            CASE TypeCode.Int32
            CASE TypeCode.Int64
            CASE TypeCode.Single
            CASE TypeCode.Double
            CASE TypeCode.Decimal
                LOCAL ds := DoubleStruct{} AS DoubleStruct
                ds:doubleValue := Convert.ToDouble(toConvert)
                ds:SaveToIndex(buffer)
                resultLength := 8
                RETURN TRUE
            CASE TypeCode.String
                text := (STRING)toConvert
            CASE TypeCode.Boolean
                text := "F"
                IF (LOGIC) toConvert
                    text := "T"
                ENDIF
            END SWITCH
            IF sLen > text:Length
                sLen := text:Length
            ENDIF
            resultLength := sLen
            SELF:_oRDD:_Encoding:GetBytes( text, 0, slen, buffer, 0)
            RETURN TRUE
            
        METHOD SetOrderScope(itmScope AS OBJECT , uiScope AS DBOrder_Info ) AS LOGIC
            LOCAL uiRealLen AS LONG
            LOCAL result AS LOGIC
            
            uiRealLen := 0
            result := TRUE
            SWITCH uiScope
            CASE DBOrder_Info.DBOI_SCOPETOPCLEAR
                SELF:_Scopes[SELF:TopScopeNo]:Clear()
            CASE DBOrder_Info.DBOI_SCOPEBOTTOMCLEAR
                SELF:_Scopes[SELF:BottomScopeNo]:Clear()
            CASE DBOrder_Info.DBOI_SCOPETOP
                SELF:_Scopes[SELF:TopScopeNo]:Value := itmScope
                IF itmScope != NULL
                    SELF:_ToString(itmScope, SELF:_keySize,  SELF:_Scopes[SELF:TopScopeNo]:Buffer, REF uiRealLen)
                    SELF:_Scopes[SELF:TopScopeNo]:Size := uiRealLen
                ENDIF
            CASE DBOrder_Info.DBOI_SCOPEBOTTOM
                SELF:_Scopes[SELF:BottomScopeNo]:Value   := itmScope
                IF itmScope != NULL
                    SELF:_ToString(itmScope, SELF:_keySize, SELF:_Scopes[SELF:BottomScopeNo]:Buffer, REF uiRealLen)
                    SELF:_Scopes[SELF:BottomScopeNo]:Size := uiRealLen
                ENDIF
            OTHERWISE
                result := FALSE
            END SWITCH
            RETURN result

        INTERNAL METHOD _CountRecords(records REF LONG ) AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL oldRec AS LONG
            LOCAL recno AS LONG
            LOCAL last AS LONG
            LOCAL count AS LONG
            
            isOk := TRUE
            SELF:_bag:Flush()
            SELF:_oRdd:GoCold()
            oldRec := SELF:_Recno
            IF SELF:Shared
                isOk := SELF:SLock()
                IF !isOk
                    RETURN FALSE
                ENDIF
            ENDIF
            IF SELF:HasScope
                SELF:_ScopeSeek(DBOrder_Info.DBOI_SCOPEBOTTOM)
                records := SELF:_getScopePos()
            ELSE
                IF  XSharp.RuntimeState.Deleted  .OR. SELF:_oRdd:_FilterInfo:Active
                    SELF:_oRdd:SkipFilter(1)
                    oldRec := SELF:_Recno
                    records := 0
                    IF !SELF:_oRdd:_Eof
                        recno := SELF:_locateKey(NULL, 0, SearchMode.Top)
                        isOk := SELF:_oRdd:__Goto(recno)
                        IF isOk
                            isOk := SELF:_oRdd:SkipFilter(1)
                        ENDIF
                        recno := SELF:_Recno
                        last := SELF:_oRdd:RecCount + 1
                        count := 0
                        DO WHILE recno != 0 .AND. recno < last
                            count++
                            recno := SELF:_ScopeSkip(1)
                        ENDDO
                        records := count
                    ENDIF
                ELSE
                     records := 0
                    IF SELF:GoTop()
                        VAR topStack := SELF:CurrentStack
                        VAR page     := topStack:Page
                        DO WHILE TRUE
                            IF page == NULL
                                EXIT
                            ENDIF
                            records += page:NumKeys
                            IF SELF:_Descending
                                IF page:HasLeft
                                    VAR pageNo := page:LeftPtr
                                    page := SELF:GetPage(pageNo)
                                ELSE
                                    EXIT
                                ENDIF
                            ELSE
                                IF page:HasRight
                                    VAR pageNo := page:RightPtr
                                    page := SELF:GetPage(pageNo)
                                ELSE
                                    EXIT
                                ENDIF
                            ENDIF
                        ENDDO
                    ENDIF
                ENDIF
            ENDIF
            SELF:_oRdd:__Goto(oldRec)
            IF SELF:Shared
                isOk := SELF:UnLock()
            ENDIF
            RETURN isOk


        INTERNAL METHOD _getRecPos(record REF LONG ) AS LOGIC
            LOCAL oldRec AS LONG
            LOCAL recno AS LONG
            LOCAL count AS LONG
            
            SELF:_oRdd:GoCold()
            oldRec := SELF:_Recno
            IF !SELF:SLock()
                RETURN FALSE
            ENDIF
            recno := record
            IF recno == 0
                recno := SELF:_Recno
            ENDIF
            IF SELF:Stack:Empty
                SELF:_GoToRecno(recno)
            ENDIF
            IF SELF:HasScope
                record := SELF:_getScopePos()
            ELSE
                IF XSharp.RuntimeState.Deleted .OR. SELF:_oRdd:_FilterInfo:Active
                    SELF:_oRdd:SkipFilter(1)
                    oldRec := SELF:_Recno
                    record := 0
                    IF !SELF:_oRdd:_Eof
                        recno := SELF:_locateKey(NULL, 0, SearchMode.Top)
                        IF SELF:_oRdd:__Goto(recno)
                            SELF:_oRdd:SkipFilter(1)
                        ENDIF
                        recno := SELF:_Recno
                        count := 1
                        DO WHILE recno != 0 .AND. recno != oldRec
                            count++
                            recno := SELF:_ScopeSkip(1)
                        ENDDO
                        record := count
                    ENDIF
                ELSE
                    record := 1
                    DO WHILE SELF:_findItemPos(REF record, FALSE)
                    ENDDO
                ENDIF
            ENDIF
            SELF:_oRdd:__Goto(oldRec)
            RETURN SELF:UnLock()
            
        PRIVATE METHOD _nextKey( keyMove AS LONG ) AS LONG
            LOCAL recno			AS LONG
            LOCAL moveDirection	AS SkipDirection
            IF keyMove == 1
                recno := SELF:_getNextKey(SkipDirection.Forward)
            ELSE
                IF keyMove < 0
                    keyMove := -keyMove
                    moveDirection := SkipDirection.Backward
                ELSE
                    moveDirection := SkipDirection.Forward
                ENDIF
                IF keyMove != 0
                    REPEAT
                        recno := SELF:_getNextKey( moveDirection)
                        keyMove--
                    UNTIL !(recno != 0 .AND. keyMove != 0)
                ELSE
                    recno := 0
                ENDIF
            ENDIF
            RETURN recno
            
        INTERNAL METHOD PopPage() AS CdxStackEntry
            SELF:_Stack:Pop()
            RETURN SELF:_Stack:Top

        INTERNAL METHOD PushPage(oPage AS CdxTreePage) AS VOID
            SELF:_Stack:Push(oPage, 0)
            RETURN

        INTERNAL PROPERTY CurrentLeaf AS CdxLeafPage GET SELF:_Stack:Top:Page ASTYPE  CdxLeafPage

        INTERNAL PROPERTY CurrentTop  AS CdxTreePage GET SELF:_Stack:Top:Page 

        INTERNAL METHOD InsertOnTop(oPage AS CdxTreePage) AS LOGIC
            SELF:_stack:InsertOnTop(oPage)
            RETURN TRUE

        INTERNAL METHOD AdjustStack(originalPage AS CdxTreePage, oPage AS CdxTreePage, nPos AS WORD) AS VOID
            SELF:_stack:Replace(originalPage, oPage, nPos)
            RETURN


        INTERNAL METHOD PushPage(oPage AS CdxTreePage , nPos AS WORD) AS VOID
            SELF:_Stack:Push(oPage, nPos)
            RETURN 

        INTERNAL METHOD ClearStack() AS VOID
            SELF:_stack:Clear()
            RETURN
            
        INTERNAL METHOD _dump() AS VOID
	    
            LOCAL hDump     AS IntPtr
            LOCAL cFile     AS STRING
            LOCAL sBlock    AS STRING
            LOCAL sName AS STRING
            sName := SELF:_bag:FullPath+"_"+SELF:OrderName
            VAR sRecords := System.Text.StringBuilder{}
            cFile := sName+".DMP"
            hDump := FCreate(cFile)
            IF hDump != F_ERROR
                SELF:_bag:_PageList:DumpHandle := hDump
                SELF:_bag:_PageList:Flush(FALSE)
                sBlock := SELF:_Header:Dump("Filedump for:"+sName)
                FWrite(hDump, sBlock)
                _oRdd:Gotop()
                sRecords:AppendLine("------------------------------")
                sRecords:AppendLine("List of Records in Index order")
                sRecords:AppendLine("------------------------------")
                sRecords:AppendLine("Recno      KeyValue")
                sRecords:AppendLine("------------------------------")
                DO WHILE ! _oRdd:EOF
                    VAR key := _oRdd:EvalBlock(SELF:_KeyCodeBlock)
                    IF key IS IDate
                        VAR d := key ASTYPE IDate
                        key := DateTime{d:Year, d:Month, d:Day}:ToString("yyyyMMdd")
                    ELSEIF key IS IFLoat
                        VAR f := key ASTYPE IFloat
                        key   :=  f:Value:ToString("F"+f:Decimals:ToString())
                    ENDIF
                    sRecords:AppendLine(String.Format("{0,10} {1}", _oRdd:Recno, key))
                    _oRdd:Skip(1)
                ENDDO
                // Collect Recycled pages
                LOCAL sbFree AS StringBuilder
                sbFree := StringBuilder{}
                LOCAL nPage := SELF:OrderBag:Root:FreeList AS LONG
                sbFree:AppendLine("-------------------------------")
                sbFree:AppendLine("List of Free pages (Bag Level) ")
                sbFree:AppendLine("-------------------------------")
                sbFree:AppendLine("First free: "+ nPage:ToString("X8"))
                // stop dumping pages because from now on we only dump the page numbers
                SELF:_bag:_PageList:DumpHandle := IntPtr.Zero
                DO WHILE nPage != 0 .AND. nPage != -1
                    VAR oPage := SELF:GetPage(nPage) ASTYPE CdxTreePage
                    IF oPage != NULL
                        nPage := oPage:NextFree
                        sbFree:AppendLine("Next free : "+ nPage:ToString("X8"))
                    ELSE
                        EXIT
                    ENDIF
                ENDDO
                FWrite(hDump, sbFree:ToString())
                FWrite(hDump, sRecords:ToString())
                FClose(hDump)
                
            ENDIF
            RETURN

        // Three methods to calculate keys. We have split these to optimize index creating
        PRIVATE METHOD _getNumFieldValue(sourceIndex AS LONG, byteArray AS BYTE[]) AS LOGIC
            LOCAL r8 := DoubleStruct{} AS DoubleStruct
            LOCAL oValue := SELF:_oRdd:GetValue(_SingleField+1) AS OBJECT
            r8:DoubleValue := Convert.ToDouble(oValue)
            r8:SaveToIndex(byteArray)
            RETURN TRUE
            
        PRIVATE METHOD _getDateFieldValue(sourceIndex AS LONG, byteArray AS BYTE[]) AS LOGIC
            LOCAL oValue := SELF:_oRdd:GetValue(_SingleField+1) AS OBJECT
            IF oValue IS IDate
                VAR valueDate := (IDate)oValue
                IF !valueDate:IsEmpty
                    oValue := DateTime{valueDate:Year, valueDate:Month, valueDate:Day}
                ELSE
                    oValue := DateTime.MinValue
                ENDIF
            ENDIF
            IF oValue IS DateTime
                LOCAL r8 := DoubleStruct{} AS DoubleStruct
                VAR longValue  := _toJulian((DateTime) oValue)
                r8:DoubleValue := Convert.ToDouble(longValue)
                r8:SaveToIndex(byteArray)
                RETURN TRUE
            ENDIF
            RETURN FALSE
        
        PRIVATE METHOD _getFieldValue(sourceIndex AS LONG, byteArray AS BYTE[]) AS LOGIC
            SELF:_oRDD:Validate()
            Array.Copy(SELF:_oRdd:_RecordBuffer, sourceIndex, byteArray, 0, SELF:_keySize)
            RETURN TRUE
            
        PRIVATE METHOD _getExpressionValue(sourceIndex AS LONG, byteArray AS BYTE[]) AS LOGIC
            LOCAL result := TRUE AS LOGIC
            TRY
                SELF:_oRDD:Validate()
                VAR oKeyValue := SELF:_oRdd:EvalBlock(SELF:_KeyCodeBlock)
                LOCAL uiRealLen := 0 AS LONG
                result := SELF:_ToString(oKeyValue, SELF:_keySize,  byteArray,  REF uiRealLen)
            CATCH Ex AS Exception
                SELF:_oRdd:_dbfError(ex, SubCodes.EDB_EXPRESSION,GenCode.EG_SYNTAX,  "DBFCDX._GetExpressionValue") 
                result := FALSE
            END TRY
            RETURN result

    END CLASS
END NAMESPACE
