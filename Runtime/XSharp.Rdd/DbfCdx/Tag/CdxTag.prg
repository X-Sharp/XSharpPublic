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
    [DebuggerDisplay("Tag: {OrderName}, Key: {Expression}, For: {Condition}")];
    INTERNAL PARTIAL CLASS CdxTag
        #region constants
        PRIVATE CONST MAX_KEY_LEN       := 256  AS WORD
        PRIVATE CONST NTX_COUNT         := 16    AS WORD
        PRIVATE CONST MIN_BYTE          := 0x01 AS BYTE
        PRIVATE CONST MAX_BYTE          := 0xFF AS BYTE
        PRIVATE CONST MAX_TRIES         := 50 AS WORD
        PRIVATE CONST STACK_DEPTH       := 20 AS LONG
        PRIVATE CONST LOCKOFFSET_OLD    := 1000000000 AS LONG
        PRIVATE CONST LOCKOFFSET_NEW    := -1 AS LONG
        #endregion
        #region fields
        INTERNAL _Encoding AS Encoding
        INTERNAL _Hot AS LOGIC
        INTERNAL _Conditional AS LOGIC
        INTERNAL _Descending AS LOGIC
        INTERNAL _Partial AS LOGIC
        INTERNAL _SingleField AS LONG
        INTERNAL _SourceIndex AS LONG
        INTERNAL _KeyCodeBlock AS ICodeblock
        INTERNAL _ForCodeBlock AS ICodeblock
        INTERNAL _KeyExpr AS STRING
        INTERNAL _ForExpr AS STRING
        INTERNAL _currentRecno AS LONG
        INTERNAL _currentKeyBuffer AS BYTE[]
        INTERNAL _newKeyBuffer AS BYTE[]
        INTERNAL _newKeyLen AS LONG
        INTERNAL _version AS DWORD
        INTERNAL _KeyExprType AS LONG
        INTERNAL _keySize AS WORD
        INTERNAL _keyDecimals AS WORD
        INTERNAL _TopStack AS LONG
        INTERNAL _firstPageOffset AS LONG
        INTERNAL _stack AS RddStack[]
        INTERNAL _tagNumber AS INT
        INTERNAL _orderName AS STRING
        INTERNAL _Ansi AS LOGIC
        INTERNAL _hasTopScope AS LOGIC
        INTERNAL _hasBottomScope AS LOGIC
        INTERNAL _topScopeBuffer AS BYTE[]
        INTERNAL _bottomScopeBuffer AS BYTE[]
        INTERNAL _topScope AS OBJECT
        INTERNAL _bottomScope AS OBJECT
        INTERNAL _topScopeSize AS LONG
        INTERNAL _bottomScopeSize AS LONG
        INTERNAL _oRdd   AS DbfCdx
        INTERNAL _Header AS CdxTagHeader
        INTERNAL _oneItem AS CdxNode
        PRIVATE _midItem AS CdxNode
        PRIVATE _outPageNo AS LONG      // has to move to OrderBag later

        PRIVATE _bag    AS CdxOrderBag
        PRIVATE getKeyValue AS ValueBlock       // Delegate to calculate the key
#endregion

#region Properties
        INTERNAL PROPERTY Expression AS STRING GET _KeyExpr
        
        INTERNAL PROPERTY Condition AS STRING GET _ForExpr
        INTERNAL PROPERTY OrderName AS STRING GET _orderName
	    INTERNAL PROPERTY Shared 	 AS LOGIC GET _bag:Shared
        INTERNAL PROPERTY _Recno 	 AS LONG GET _oRDD:Recno
        INTERNAL PROPERTY FileName 	 AS STRING GET _bag:FileName
        INTERNAL PROPERTY OrderBag       AS CdxOrderBag GET SELF:_bag
        INTERNAL PROPERTY Page           AS Int32 AUTO
        INTERNAL PROPERTY Descending     AS LOGIC GET _Descending
        INTERNAL PROPERTY IsConditional  AS LOGIC GET Options:HasFlag(CdxOptions.HasFor)
        INTERNAL PROPERTY IsHot          AS LOGIC GET _Hot
        PROPERTY KeyType        	AS INT GET SELF:_KeyExprType
        PROPERTY Partial        	AS LOGIC GET SELF:Custom
        PROPERTY Conditional        AS LOGIC GET !String.IsNullOrEmpty(_ForExpr)
        PROPERTY Custom         	AS LOGIC GET Options:HasFlag(CdxOptions.IsCustom)
        PROPERTY Unique         	AS LOGIC GET Options:HasFlag(CdxOptions.IsUnique)
        PROPERTY Signature      	AS BYTE AUTO
        PROPERTY FieldIndex     	AS INT AUTO             // 1 based FieldIndex
        PROPERTY Options        	AS CdxOptions AUTO
        PROPERTY LockOffSet     	AS LONG AUTO
       
#endregion


        INTERNAL CONSTRUCTOR (oBag AS CdxOrderBag, nPage AS Int32, buffer AS BYTE[], cName AS STRING)
	    SUPER()
            LOCAL i AS LONG

            SELF:_currentKeyBuffer := BYTE[]{ MAX_KEY_LEN+1 }
            SELF:_newKeyBuffer  := BYTE[]{ MAX_KEY_LEN+1 }
            SELF:_bag := oBag
            SELF:_oRDD := oBag:_oRDD
            SELF:_orderName := cName
            SELF:_stack         := RddStack[]{ STACK_DEPTH }
            SELF:_Encoding      := _oRDD:_Encoding
            //Init
            FOR i := 0 TO STACK_DEPTH - 1 
                SELF:_stack[i] := RddStack{}
            NEXT
            SELF:Page := nPage
            SELF:FieldIndex  := 0
            SELF:_SingleField := -1
            SELF:_Header := CdxTagHeader{oBag, nPage, buffer, SELF:OrderName}
            SELF:Open()


	    METHOD Open() AS LOGIC
            
            SELF:_KeyExpr := SELF:_Header:KeyExpression
            SELF:_ForExpr := SELF:_Header:ForExpression
            SELF:_oRdd:GoTo(1)
            IF ! SELF:EvaluateExpressions()
                RETURN FALSE
            ENDIF

            SELF:_Hot := FALSE
            SELF:ClearStack()
            SELF:_keySize       := SELF:_Header:KeySize
            //SELF:_keyDecimals   := SELF:_Header:KeyDecimals
            SELF:Options        := SELF:_Header:Options
            SELF:Signature      := SELF:_Header:Signature
            SELF:_Descending    := SELF:_Header:Descending
            SELF:_firstPageOffset := SELF:_Header:RootPage

            SELF:_Version  := SELF:_Header:Version
            SELF:_midItem := CdxNode{SELF:_keySize}
            SELF:_oneItem := CdxNode{SELF:_keySize}
            RETURN TRUE

        DESTRUCTOR()
            Close()
            
        INTERNAL METHOD EvaluateExpressions() AS LOGIC
            LOCAL evalOk AS LOGIC
            LOCAL oKey AS OBJECT
            evalOk := TRUE
            TRY
                SELF:_KeyCodeBlock := SELF:_oRdd:Compile(SELF:_KeyExpr)
            CATCH
                SELF:_oRdd:_dbfError( SubCodes.EDB_EXPRESSION, GenCode.EG_SYNTAX,"DBFNTX.Compile")
                RETURN FALSE
            END TRY

            TRY
                oKey := SELF:_oRdd:EvalBlock(SELF:_KeyCodeBlock)
            CATCH
                evalOk := FALSE
                oKey := NULL
            END TRY
            IF !evalOk
                SELF:_oRdd:_dbfError( SubCodes.EDB_EXPRESSION, GenCode.EG_SYNTAX, "DBFNTX.Compile")
                RETURN FALSE
            ENDIF
            SELF:_KeyExprType := SELF:_oRdd:_getUsualType(oKey)

            // If the Key Expression contains only a Field Name
            SELF:_SingleField := SELF:_oRDD:FieldIndex(SELF:_KeyExpr) -1
            SELF:_SourceIndex := 0
            LOCAL isOk AS LOGIC
            IF SELF:_SingleField >= 0
                SELF:_keyDecimals   := (WORD) SELF:_oRdd:_fields[_SingleField]:Decimals
                SELF:_SourceIndex   := (WORD) SELF:_oRdd:_fields[_SingleField]:OffSet
                VAR fType           := SELF:_oRdd:_fields[_SingleField]:FieldType
                SWITCH fType
                CASE DbFieldType.Number
                    SELF:_keySize   := 8
                    SELF:getKeyValue := _getNumFieldValue
                CASE DbFieldType.Date
                    SELF:_keySize   := 8
                    SELF:getKeyValue := _getDateFieldValue
                OTHERWISE
                    SELF:_keySize   := (WORD) SELF:_oRdd:_fields[_SingleField]:Length
                    SELF:getKeyValue := _getFieldValue
                END SWITCH
                isOk := TRUE
            ELSE
                SELF:_keyDecimals := 0
                SELF:_keySize := 0
                SELF:getKeyValue := _getExpressionValue
                isOk := SELF:_determineSize(oKey)
            ENDIF
            IF ! isOk
                RETURN FALSE
            ENDIF
            SELF:_Conditional := FALSE
            IF SELF:_ForExpr:Length > 0
                TRY
                    SELF:_ForCodeBlock := SELF:_oRdd:Compile(SELF:_ForExpr)
                CATCH
                    SELF:_oRdd:_dbfError( SubCodes.EDB_EXPRESSION, GenCode.EG_SYNTAX,"DBFNTX.Compile")
                    RETURN FALSE
                END TRY
                SELF:_oRdd:GoTo(1)
                evalOk := TRUE
                TRY
                    VAR oValue := SELF:_oRdd:EvalBlock(SELF:_ForCodeBlock)
                    evalOk     := SELF:_oRdd:_getUsualType(oValue) == __UsualType.Logic
                CATCH
                    evalOk := FALSE
                END TRY
                IF !evalOk
                    SELF:_oRdd:_dbfError(SubCodes.EDB_EXPRESSION,GenCode.EG_SYNTAX,  "DBFNTX.Compile") 
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
                SELF:_Header:FirstPageOffset        := SELF:_firstPageOffset
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
                SELF:_Header:FirstPageOffset        := SELF:_firstPageOffset
                SELF:_Header:Write( )
            ENDIF
            FFlush( SELF:_hFile )
            */ 
            RETURN TRUE
            
        PUBLIC METHOD Close() AS LOGIC
            /*
            SELF:Flush()
            TRY
                IF SELF:_hFile != F_ERROR
                    FClose( SELF:_hFile )
                    SELF:_hFile := F_ERROR
                ENDIF
                
            FINALLY
                SELF:_hFile := F_ERROR
            END TRY
            */
            RETURN TRUE

        PUBLIC METHOD GoCold() AS LOGIC
            IF SELF:_oRDD:IsHot
                RETURN SELF:_KeyUpdate( SELF:_RecNo, SELF:_oRDD:IsNewRecord )
            ENDIF
            RETURN TRUE


        INTERNAL METHOD __Compare( aLHS AS BYTE[], aRHS AS BYTE[], nLength AS LONG) AS LONG
            IF aRHS == NULL
                RETURN 0
            ENDIF
            RETURN RuntimeState.StringCompare(aLHS, aRHS, nLength)


        PUBLIC METHOD SetOffLine() AS VOID
            SELF:ClearStack()
        PRIVATE METHOD _PutHeader() AS LOGIC
	    /*
            LOCAL ntxSignature AS NtxHeaderFlags
            
            ntxSignature := NtxHeaderFlags.Default
            IF SELF:_Conditional .OR. SELF:_Descending
                ntxSignature |= NtxHeaderFlags.Conditional
            ENDIF
            IF SELF:Custom
                ntxSignature |= NtxHeaderFlags.Partial
            ENDIF
            IF SELF:_HPLocking
                ntxSignature |= NtxHeaderFlags.HpLock
            ENDIF
            IF  _LockOffSet == LOCKOFFSET_NEW
                ntxSignature |= NtxHeaderFlags.NewLock
            ENDIF
            SELF:_Header:Signature              := ntxSignature
            SELF:_Header:Version        := SELF:_Version
            SELF:_Header:FirstPageOffset        := SELF:_firstPageOffset
            SELF:_Header:NextUnusedPageOffset   := SELF:_nextUnusedPageOffset
            System.Diagnostics.Debug.WriteLine(SELF:_Header:Dump("After Update"))
            
            RETURN SELF:_Header:Write()
	    */
	    RETURN TRUE
            
            // Save informations about the "current" Item	
        PRIVATE METHOD _saveCurrentRecord( node AS CdxNode ) AS VOID
            SELF:_currentRecno := node:Recno
            Array.Copy(node:KeyBytes, SELF:_currentKeyBuffer, SELF:_keySize)


        PRIVATE METHOD _ToString( toConvert AS OBJECT , sLen AS LONG , nDec AS LONG , buffer AS BYTE[] , isAnsi AS LOGIC ) AS LOGIC    
            LOCAL resultLength AS LONG
            resultLength := 0
            RETURN SELF:_ToString( toConvert, sLen, nDec, buffer, isAnsi, REF resultLength)

        PRIVATE METHOD _ToString( toConvert AS OBJECT , sLen AS LONG , nDec AS LONG , buffer AS BYTE[] , isAnsi AS LOGIC , resultLength REF LONG ) AS LOGIC
            LOCAL text AS STRING
            LOCAL chkDigits AS LOGIC
            LOCAL typeCde AS TypeCode
            LOCAL valueFloat AS IFloat
            LOCAL sBuilder AS StringBuilder
            LOCAL valueDate AS IDate
            LOCAL formatInfo AS NumberFormatInfo
            
            formatInfo := NumberFormatInfo{}
            formatInfo:NumberDecimalSeparator := "."
            
            text := NULL
            chkDigits := FALSE
            // Float Value ?
            IF (toConvert ASTYPE IFloat) != NULL
                valueFloat := (IFloat)toConvert
                toConvert := valueFloat:Value
                formatInfo:NumberDecimalDigits := valueFloat:Decimals
                typeCde := TypeCode.Double
                text := valueFloat:Value:ToString("F", formatInfo)
                // Too long ?
                IF text:Length > sLen
                    sBuilder := StringBuilder{}
                    text := sBuilder:Insert(0, "*", sLen):ToString()
                    SELF:_oRDD:_Encoding:GetBytes( text, 0, slen, buffer, 0)
                    resultLength := text:Length
                    RETURN FALSE
                ENDIF
                IF text:Length < sLen
                    text := text:PadLeft(sLen, ' ')
                    chkDigits := TRUE
                ENDIF
            ELSE
                IF (toConvert ASTYPE IDate) != NULL
                    valueDate := (IDate)toConvert
                    text := valueDate:Value:ToString("yyyyMMdd")
                    typeCde := TypeCode.String
                ELSE
                    typeCde := Type.GetTypeCode(toConvert:GetType())
                ENDIF
            ENDIF
            IF text == NULL
                SWITCH typeCde
                CASE TypeCode.String
                    text := (STRING)toConvert
                CASE TypeCode.Int16
                CASE TypeCode.Int32
            CASE TypeCode.Int64
                CASE TypeCode.Single
                CASE TypeCode.Double
                CASE TypeCode.Decimal
                        formatInfo:NumberDecimalDigits := nDec
                        SWITCH typeCde
                        CASE TypeCode.Int32
                            text := ((LONG)toConvert):ToString("F", formatInfo)
                        CASE TypeCode.Double
                            text := ((REAL8)toConvert):ToString("F", formatInfo)
                        OTHERWISE
                            text := ((Decimal)toConvert):ToString("F", formatInfo)
                        END SWITCH
                        VAR length := text:Length
                        IF length > sLen
                            sBuilder := StringBuilder{}
                            text := sBuilder:Insert(0, "*", sLen):ToString()
                            SELF:_oRDD:_Encoding:GetBytes( text, 0, slen, buffer, 0)
                            resultLength := text:Length
                            RETURN FALSE
                        ENDIF
                        text := text:PadLeft(sLen, ' ')
                    chkDigits := TRUE
                CASE TypeCode.DateTime
                    text := ((DateTime)toConvert):ToString("yyyyMMdd")
                CASE TypeCode.Boolean
                    text := (IIF(((LOGIC)toConvert) , "T" , "F"))
                OTHERWISE
                    RETURN FALSE
                END SWITCH
            ENDIF
            IF sLen > text:Length
                sLen := text:Length
            ENDIF
            SELF:_oRDD:_Encoding:GetBytes( text, 0, slen, buffer, 0)
            IF chkDigits
                SELF:_checkDigits(buffer, SELF:_keySize, SELF:_keyDecimals )
            ENDIF
            resultLength := text:Length
            RETURN TRUE
            
            
            
        PRIVATE METHOD _checkDigits(buffer AS BYTE[] , length AS LONG , decimals AS LONG ) AS VOID
            LOCAL i := 0 AS LONG
            // Transform starting spaces with zeros
            DO WHILE buffer[i] == 32 .AND. i < length
                buffer[i] := (BYTE)'0'
                i++
            ENDDO
            IF i == length  // all spaces , now converted to all zeroes.
                // It must be a decimal value ?
                IF decimals != 0
                    buffer[length - decimals - 1] := (BYTE)'.'
                ENDIF
            ENDIF
            IF buffer[i] == '-'
                i++
                FOR VAR j := 0 TO i-1 STEP 1
                    buffer[j] := 44 // ,
                NEXT
                FOR VAR j := i TO length -1 STEP 1
                    buffer[j] := (BYTE) (92 - buffer[j])
                NEXT
            ENDIF
            RETURN
        METHOD SetOrderScope(itmScope AS OBJECT , uiScope AS DBOrder_Info ) AS LOGIC
            LOCAL uiRealLen AS LONG
            LOCAL result AS LOGIC
            
            uiRealLen := 0
            result := TRUE
            SWITCH uiScope
            CASE DBOrder_Info.DBOI_SCOPETOP
                SELF:_topScope      := itmScope
                SELF:_hasTopScope   := (itmScope != NULL)
                IF itmScope != NULL
                    SELF:_topScopeBuffer := BYTE[]{ MAX_KEY_LEN+1 }
                    SELF:_ToString(itmScope, SELF:_keySize, SELF:_keyDecimals, SELF:_topScopeBuffer, SELF:_Ansi, REF uiRealLen)
                    SELF:_topScopeSize := uiRealLen
                ENDIF
            CASE DBOrder_Info.DBOI_SCOPEBOTTOM
                SELF:_bottomScope    := itmScope
                SELF:_hasBottomScope := (itmScope != NULL)
                IF itmScope != NULL
                    SELF:_bottomScopeBuffer := BYTE[]{ MAX_KEY_LEN+1 }
                    SELF:_ToString(itmScope, SELF:_keySize, SELF:_keyDecimals, SELF:_bottomScopeBuffer, SELF:_Ansi, REF uiRealLen)
                    SELF:_bottomScopeSize := uiRealLen
                ENDIF
            OTHERWISE
                result := FALSE
            END SWITCH
            RETURN result
            // Todo
        INTERNAL METHOD _CountRecords(records REF LONG ) AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL oldRec AS LONG
            LOCAL recno AS LONG
            LOCAL last AS LONG
            LOCAL count AS LONG
            
            isOk := TRUE
            SELF:_oRdd:GoCold()
            oldRec := SELF:_Recno
            IF SELF:Shared
                isOk := SELF:SLock()
                IF !isOk
                    RETURN FALSE
                ENDIF
            ENDIF
            IF SELF:_hasTopScope .OR. SELF:_hasBottomScope
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
                    SELF:_oRdd:GoBottom()
                    records := 0
                    IF !SELF:_oRdd:_Eof
                        records := 1
                        DO WHILE SELF:_findItemPos(REF records, FALSE)
                            NOP
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
            IF SELF:_TopStack == 0
                SELF:_GoToRecno(recno)
            ENDIF
            IF SELF:_hasTopScope .OR. SELF:_hasBottomScope
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
                recno := SELF:_getNextKey(FALSE, SkipDirection.Forward)
            ELSE
                IF keyMove < 0
                    keyMove := -keyMove
                    moveDirection := SkipDirection.Backward
                ELSE
                    moveDirection := SkipDirection.Forward
                ENDIF
                IF keyMove != 0
                    REPEAT
                        recno := SELF:_getNextKey(FALSE, moveDirection)
                        keyMove--
                    UNTIL !(recno != 0 .AND. keyMove != 0)
                ELSE
                    recno := 0
                ENDIF
            ENDIF
            RETURN recno
            
        PRIVATE METHOD PopPage() AS VOID
            IF SELF:_TopStack != 0
                SELF:_stack[SELF:_TopStack]:Clear()
                SELF:_TopStack--
            ENDIF
            
        PRIVATE METHOD ClearStack() AS VOID
        
            FOREACH entry AS RddStack IN SELF:_stack 
                entry:Clear()
            NEXT
            SELF:_TopStack := 0
            
        INTERNAL METHOD _dump() AS VOID
	    /*
            LOCAL hDump     AS IntPtr
            LOCAL cFile     AS STRING
            LOCAL sBlock    AS STRING
            VAR sRecords := System.Text.StringBuilder{}
            cFile := SELF:_fileName+".DMP"
            hDump := FCreate(cFile)
            IF hDump != F_ERROR
                SELF:_PageList:DumpHandle := hDump
                sBlock := SELF:_Header:Dump("Filedump for:"+SELF:_FileName)
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
                FWrite(hDump, sRecords:ToString())
                sRecords:Clear()
                sRecords:AppendLine("------------------------------")
                sRecords:AppendLine("List of Unused Pages")
                sRecords:AppendLine("------------------------------")
                LOCAL nPage AS LONG
                nPage := SELF:_nextUnusedPageOffset
                SELF:_PageList:DumpHandle := IntPtr.Zero
                SELF:_PageList:Flush(FALSE)
                DO WHILE nPage != 0
                    sRecords:AppendLine(nPage:ToString())
                    VAR page := SELF:_pageList:Read(nPage)
                    nPage := page:NextPage
                ENDDO
                FWrite(hDump, sRecords:ToString())
                FClose(hDump)
                
            ENDIF
        */
            RETURN
        // Three methods to calculate keys. We have split these to optimize index creating
        PRIVATE METHOD _getNumFieldValue(sourceIndex AS LONG, byteArray AS BYTE[]) AS LOGIC
            //Todo
            RETURN TRUE
            
        PRIVATE METHOD _getDateFieldValue(sourceIndex AS LONG, byteArray AS BYTE[]) AS LOGIC
            //Todo
            RETURN TRUE
        PRIVATE METHOD _getFieldValue(sourceIndex AS LONG, byteArray AS BYTE[]) AS LOGIC
            Array.Copy(SELF:_oRdd:_RecordBuffer, sourceIndex, byteArray, 0, SELF:_keySize)
            RETURN TRUE
            
        PRIVATE METHOD _getExpressionValue(sourceIndex AS LONG, byteArray AS BYTE[]) AS LOGIC
            LOCAL result := TRUE AS LOGIC
            TRY
                VAR oKeyValue := SELF:_oRdd:EvalBlock(SELF:_KeyCodeBlock)
                LOCAL uiRealLen := 0 AS LONG
                result := SELF:_ToString(oKeyValue, SELF:_keySize, SELF:_keyDecimals, byteArray, SELF:_Ansi, REF uiRealLen)
            CATCH
                result := FALSE
            END TRY
            RETURN result

    END CLASS
END NAMESPACE
