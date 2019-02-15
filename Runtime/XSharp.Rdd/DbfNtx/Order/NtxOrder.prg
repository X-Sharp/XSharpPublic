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
USING System.Runtime.CompilerServices

BEGIN NAMESPACE XSharp.RDD.NTX

    [DebuggerDisplay("Order {OrderName}: {Expression}")];
    INTERNAL PARTIAL SEALED CLASS NtxOrder INHERIT BaseIndex IMPLEMENTS IRddSortWriter
        #region constants
        PRIVATE CONST MAX_KEY_LEN       := 256  AS WORD
        PRIVATE CONST BUFF_SIZE	        := 1024  AS WORD 
        PRIVATE CONST NTX_COUNT         := 16    AS WORD
        PRIVATE CONST STACK_DEPTH       := 20    AS WORD
        PRIVATE CONST MIN_BYTE          := 0x01 AS BYTE
        PRIVATE CONST MAX_BYTE          := 0xFF AS BYTE
        PRIVATE CONST MAX_TRIES         := 50 AS WORD
        PRIVATE CONST LOCKOFFSET_OLD    := 1000000000 AS LONG
        PRIVATE CONST LOCKOFFSET_NEW    := -1 AS LONG
        INTERNAL CONST NTX_EXTENSION     := ".NTX" AS STRING
        #endregion
        #region fields
        INTERNAL _hFile AS IntPtr
        INTERNAL _Encoding AS Encoding
        INTERNAL _Shared AS LOGIC
        INTERNAL _ReadOnly       AS LOGIC
        INTERNAL _Hot AS LOGIC
        INTERNAL _Unique AS LOGIC
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
        INTERNAL _indexVersion AS WORD
        INTERNAL _nextUnusedPageOffset AS LONG
        INTERNAL _entrySize AS WORD
        INTERNAL _KeyExprType AS LONG
        INTERNAL _keySize AS WORD
        INTERNAL _keyDecimals AS WORD
        INTERNAL _MaxEntry AS WORD
        INTERNAL _halfPage AS WORD
        INTERNAL _TopStack AS LONG
        INTERNAL _firstPageOffset AS LONG
        INTERNAL _fileSize AS LONG
        INTERNAL _stack AS RddStack[]
        INTERNAL _HPLocking AS LOGIC
        INTERNAL _readLocks AS LONG
        INTERNAL _writeLocks AS LONG
        INTERNAL _tagNumber AS INT
        INTERNAL _maxLockTries AS INT
        INTERNAL _orderName AS STRING
        INTERNAL _fileName AS STRING
        INTERNAL _fullPath AS STRING
        INTERNAL _Ansi AS LOGIC
        INTERNAL _hasTopScope AS LOGIC
        INTERNAL _hasBottomScope AS LOGIC
        INTERNAL _topScopeBuffer AS BYTE[]
        INTERNAL _bottomScopeBuffer AS BYTE[]
        INTERNAL _topScope AS OBJECT
        INTERNAL _bottomScope AS OBJECT
        INTERNAL _topScopeSize AS LONG
        INTERNAL _bottomScopeSize AS LONG
        INTERNAL _oRdd AS DBFNTX
        INTERNAL _Header AS NtxHeader
        INTERNAL _oneItem AS NtxNode
        INTERNAL _PageList AS NtxPageList
        PRIVATE _levels AS NtxLevel[]
        PRIVATE _levelsCount AS LONG
        PRIVATE _midItem AS NtxNode
        PRIVATE _outPageNo AS LONG
        PRIVATE _parkPlace AS LONG
        PRIVATE getKeyValue AS ValueBlock       // Delegate to calculate the key
        
        INTERNAL _lockOffSet AS LONG
        #endregion
        #region properties
        INTERNAL PROPERTY Expression AS STRING GET _KeyExpr
        
        INTERNAL PROPERTY Condition AS STRING GET _ForExpr
        
        INTERNAL PROPERTY OrderName AS STRING GET _orderName
	INTERNAL PROPERTY Shared    AS LOGIC GET _Shared
        INTERNAL PROPERTY _Recno AS LONG GET _oRdd:Recno

        INTERNAL PROPERTY FullPath AS STRING GET _fullPath

        INTERNAL PROPERTY FileName AS STRING
            GET
                RETURN SELF:_fileName
            END GET
            SET
                SELF:_FileName := VALUE
                IF  String.IsNullOrEmpty( SELF:_FileName )
                    // When empty then take same name as DBF but with NTX extension
                    SELF:_FileName := SELF:_oRDD:_FileName
                    // force extenstion here, so change DBF to NTX
                    SELF:_FileName := Path.ChangeExtension(SELF:_FileName, NTX_EXTENSION)
                ENDIF
                SELF:_fullPath := SELF:_FileName
                // Check that we have a FullPath
                IF Path.GetDirectoryName(SELF:_fullPath):Length == 0
                    // Get the path from the RDD's open info
                    VAR cPath := Path.GetDirectoryName(SELF:_oRDD:_OpenInfo:FileName)
                    IF String.IsNullOrEmpty(cPath)
                        cPath := (STRING) SELF:_oRDD:Info(DBI_FULLPATH,NULL)
                        cPath := Path.GetDirectoryName(cPath)
                    ENDIF
                    SELF:_fullPath := Path.Combine(cPath, SELF:_fullPath)
                ELSE
                    SELF:_fullPath := SELF:_fileName
                ENDIF
                // and be sure to have an extension
                IF String.IsNullOrEmpty(Path.GetExtension(_fullPath))
                    SELF:_fullPath := Path.ChangeExtension(_fullPath, NTX_EXTENSION)
                ENDIF
            END SET
        END PROPERTY
        
        #endregion
        
        INTERNAL CONSTRUCTOR(oRDD AS DBFNTX )
            SUPER( oRdd )
            
            LOCAL i AS LONG
            
            SELF:_currentKeyBuffer := BYTE[]{ MAX_KEY_LEN+1 }
            SELF:_newKeyBuffer  := BYTE[]{ MAX_KEY_LEN+1 }
            SELF:_fileName      := NULL
            SELF:_hFile         := IntPtr.Zero
            SELF:_oRdd          := oRDD
            SELF:_Header        := NULL 
            SELF:_stack         := RddStack[]{ STACK_DEPTH }
            SELF:_Encoding      := oRDD:_Encoding
            SELF:_tagNumber     := 1
            SELF:_maxLockTries  := 1
            //Init
            FOR i := 0 TO STACK_DEPTH - 1 
                SELF:_stack[i] := RddStack{}
            NEXT
            
        INTERNAL CONSTRUCTOR(oRDD AS DBFNTX , filePath AS STRING )
            SELF(oRDD)
            SELF:FileName := filePath
        
        INTERNAL METHOD Open(dbordInfo AS DBORDERINFO ) AS LOGIC
            LOCAL isOk AS LOGIC
            isOk := FALSE
            SELF:_oRdd:GoCold()
            SELF:_Shared := SELF:_oRDD:_Shared
            SELF:_ReadOnly := SELF:_oRDD:_ReadOnly
            SELF:_hFile    := Fopen(SELF:FullPath, SELF:_oRDD:_OpenInfo:FileMode) 
            IF SELF:_hFile == F_ERROR 
                SELF:_oRDD:_dbfError( ERDD.OPEN_ORDER, GenCode.EG_OPEN, SELF:fileName)
                RETURN FALSE
            ENDIF
            SELF:_Header := NtxHeader{ SELF:_hFile }
            IF !SELF:_Header:Read()
                SELF:_oRDD:_dbfError(ERDD.OPEN_ORDER, GenCode.EG_OPEN, SELF:fileName)
                RETURN FALSE
            ENDIF
            
            SELF:_PageList := NtxPageList{SELF}
            SELF:_Ansi := SELF:_oRdd:_Ansi
            // Key & For Expression
            SELF:_KeyExpr := SELF:_Header:KeyExpression
            SELF:_ForExpr := SELF:_Header:ForExpression
             
            SELF:_oRdd:GoTo(1)
            IF ! SELF:EvaluateExpressions()
                RETURN FALSE
            ENDIF
            // For Condition
            SELF:_Shared := SELF:_oRdd:_Shared
            FSeek3( SELF:_hFile, 0, FS_END )
            SELF:_fileSize  := (LONG) FTell( SELF:_hFile ) 
            SELF:_Hot := FALSE
            SELF:ClearStack()
            SELF:_entrySize := SELF:_Header:EntrySize
            SELF:_keySize := SELF:_Header:KeySize
            SELF:_keyDecimals := SELF:_Header:KeyDecimals
            SELF:_MaxEntry := SELF:_Header:MaxItem
            SELF:_halfPage := SELF:_Header:HalfPage
            SELF:_indexVersion := SELF:_Header:IndexingVersion
            SELF:_firstPageOffset := SELF:_Header:FirstPageOffset
            SELF:_nextUnusedPageOffset := SELF:_Header:NextUnusedPageOffset
            SELF:_Unique := SELF:_Header:Unique
            SELF:_Descending := SELF:_Header:Descending
            
            SELF:_midItem := NtxNode{SELF:_keySize}
            SELF:_oneItem := NtxNode{SELF:_keySize}
            IF string.IsNullOrEmpty(SELF:_Header:OrdName)
                SELF:_orderName := Path.GetFileNameWithoutExtension(SELF:fileName)
            ELSE
                SELF:_orderName := SELF:_Header:OrdName:ToUpper()
            ENDIF
            IF SELF:_Header:Signature:HasFlag(NtxHeaderFlags.HpLock)
                SELF:_HPLocking := TRUE
            ENDIF
            
            // Copy locking scheme from DBF.
            
            // Except
            IF SELF:_Header:Signature:HasFlag(NtxHeaderFlags.NewLock)
                SELF:_LockOffSet := LOCKOFFSET_NEW
            ELSE
                SELF:_LockOffSet:= LOCKOFFSET_OLD
            ENDIF
            // NTX has only one Tag index
            SELF:_tagNumber := 1
            SELF:_maxLockTries := 99 //(LONG)XSharp.RuntimeState.LockTries
            SELF:_readLocks := 0
            
            isOk := TRUE
            IF SELF:_HPLocking .AND. SELF:Shared
                //DO
                REPEAT
                    IF !SELF:_LockInit()
                        SELF:_oRDD:_dbfError( ERDD.INIT_LOCK, GenCode.EG_LOCK, "DBFNTX.LockInit", SELF:_fileName)
                        isOk := FALSE
                    ENDIF
                    //WHILE (!isOk)
                UNTIL isOk
            ENDIF
            IF !isOk
                SELF:Flush()
                SELF:Close()
            ENDIF
            RETURN isOk
            
        DESTRUCTOR()
            Close()
            

        PRIVATE METHOD EvaluateExpressions() AS LOGIC
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
                SELF:_keySize       := (WORD) SELF:_oRdd:_fields[_SingleField]:Length
                SELF:_keyDecimals   := (WORD) SELF:_oRdd:_fields[_SingleField]:Decimals
                SELF:_SourceIndex   := (WORD) SELF:_oRdd:_fields[_SingleField]:OffSet
                VAR fType           := SELF:_oRdd:_fields[_SingleField]:FieldType
                IF fType ==  DbFieldType.Number
                    SELF:getKeyValue := _getNumFieldValue
                ELSE
                    SELF:getKeyValue := _getFieldValue
                ENDIF
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
            IF !SELF:Shared .AND. SELF:_Hot .AND. SELF:_hFile != F_ERROR
                SELF:GoCold()
                SELF:_PageList:Flush(TRUE)
                SELF:_Header:IndexingVersion        := 1
                SELF:_Header:NextUnusedPageOffset   := SELF:_nextUnusedPageOffset
                SELF:_Header:FirstPageOffset        := SELF:_firstPageOffset
                SELF:_Header:Write( )
            ENDIF
            FFlush( SELF:_hFile )
            RETURN TRUE
            
        PUBLIC METHOD Commit() AS LOGIC
            SELF:GoCold()
            IF !SELF:Shared .AND. SELF:_Hot .AND. SELF:_hFile != F_ERROR
                SELF:_Header:IndexingVersion        := 1
                SELF:_Header:NextUnusedPageOffset   := SELF:_nextUnusedPageOffset
                SELF:_Header:FirstPageOffset        := SELF:_firstPageOffset
                SELF:_Header:Write( )
            ENDIF
            FFlush( SELF:_hFile )
            RETURN TRUE
            
        PUBLIC METHOD Close() AS LOGIC
            SELF:Flush()
            TRY
                IF SELF:Shared .AND. SELF:_HPLocking
                    SELF:_LockExit()
                ENDIF
                IF SELF:_hFile != F_ERROR
                    FClose( SELF:_hFile )
                    SELF:_hFile := F_ERROR
                ENDIF
                
            FINALLY
                SELF:_HPLocking := FALSE
                SELF:_hFile := F_ERROR
            END TRY
            RETURN TRUE
            
        PUBLIC METHOD GoCold() AS LOGIC
            IF SELF:_oRDD:IsHot
                RETURN SELF:_KeyUpdate( SELF:_RecNo, SELF:_oRDD:IsNewRecord )
            ENDIF
            RETURN TRUE

        [MethodImpl(MethodImplOptions.AggressiveInlining)];
        INTERNAL METHOD __Compare( aLHS AS BYTE[], aRHS AS BYTE[], nLength AS LONG) AS LONG
            IF aRHS == NULL
                RETURN 0
            ENDIF
            RETURN RuntimeState.StringCompare(aLHS, aRHS, nLength)


        PUBLIC METHOD SetOffLine() AS VOID
            SELF:ClearStack()
            

            
            
        PRIVATE METHOD _PutHeader() AS LOGIC
            LOCAL ntxSignature AS NtxHeaderFlags
            
            ntxSignature := NtxHeaderFlags.Default
            IF SELF:_Conditional .OR. SELF:_Descending
                ntxSignature |= NtxHeaderFlags.Conditional
            ENDIF
            IF SELF:_Partial
                ntxSignature |= NtxHeaderFlags.Partial
            ENDIF
            IF SELF:_HPLocking
                ntxSignature |= NtxHeaderFlags.HpLock
            ENDIF
            IF  _LockOffSet == LOCKOFFSET_NEW
                ntxSignature |= NtxHeaderFlags.NewLock
            ENDIF
            SELF:_Header:Signature              := ntxSignature
            SELF:_Header:IndexingVersion        := SELF:_indexVersion
            SELF:_Header:FirstPageOffset        := SELF:_firstPageOffset
            SELF:_Header:NextUnusedPageOffset   := SELF:_nextUnusedPageOffset
            System.Diagnostics.Debug.WriteLine(SELF:_Header:Dump("After Update"))
            
            RETURN SELF:_Header:Write()
            
            // Save informations about the "current" Item	
        PRIVATE METHOD _saveCurrentRecord( node AS NtxNode ) AS VOID
            SELF:_currentRecno := node:Recno
            Array.Copy(node:KeyBytes, SELF:_currentKeyBuffer, SELF:_keySize)
            
            
        PRIVATE METHOD _ToString( toConvert AS OBJECT , sLen AS LONG , nDec AS LONG , buffer AS BYTE[] , isAnsi AS LOGIC ) AS LOGIC    
            LOCAL resultLength AS LONG
            resultLength := 0
            RETURN SELF:_ToString( toConvert, sLen, nDec, buffer, isAnsi, REF resultLength)
            
        PRIVATE METHOD _ToString( toConvert AS OBJECT , sLen AS LONG , nDec AS LONG , buffer AS BYTE[] , isAnsi AS LOGIC , resultLength REF LONG ) AS LOGIC
            LOCAL text AS STRING
            LOCAL chkDigits AS LOGIC
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
            ENDIF
            VAR type := SELF:_oRdd:_getUsualType(toConvert)
            SWITCH type
            CASE __UsualType.Date
                valueDate := (IDate)toConvert
                text := valueDate:Value:ToString("yyyyMMdd")
            CASE __UsualType.String
                text := (STRING)toConvert
            CASE __UsualType.DateTime
                text := ((DateTime)toConvert):ToString("yyyyMMdd")
            CASE __UsualType.Logic
                text := (IIF(((LOGIC)toConvert) , "T" , "F"))
            CASE __UsualType.Float
            CASE __UsualType.Long
                formatInfo:NumberDecimalDigits := nDec
                IF type == __UsualType.Float
                    text := ((REAL8)toConvert):ToString("F", formatInfo)
                ELSE
                    text := ((LONG)toConvert):ToString("F", formatInfo)
                ENDIF
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
            END SWITCH
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
            LOCAL last := length -1 AS LONG
            // Transform starting spaces with zeros
            FOR i := 0 TO length -1
                IF buffer[i] ==  32
                    buffer[i] := (BYTE)'0'
                ELSE
                    last := i
                    EXIT
                ENDIF
            NEXT
            IF last == length -1 // all spaces , now converted to all zeroes.
                // It must be a decimal value ?
                IF decimals != 0
                    buffer[length - decimals - 1] := (BYTE)'.'
                ENDIF
            ENDIF
            IF buffer[last] == '-'
                i++
                FOR VAR j := 0 TO last-1 STEP 1
                    buffer[j] := 44 // ,
                NEXT
                FOR VAR j := last TO length -1 STEP 1
                    buffer[j] := (BYTE) (92 - buffer[j])
                NEXT
            ENDIF
            RETURN
            
            
        PUBLIC METHOD SetOrderScope(itmScope AS OBJECT , uiScope AS DBOrder_Info ) AS LOGIC
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
                isOk := SELF:_lockForRead()
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
                isOk := SELF:_unLockForRead()
            ENDIF
            RETURN isOk
            
            
        INTERNAL METHOD _getRecPos(record REF LONG ) AS LOGIC
            LOCAL oldRec AS LONG
            LOCAL recno AS LONG
            LOCAL count AS LONG
            
            SELF:_oRdd:GoCold()
            oldRec := SELF:_Recno
            IF !SELF:_lockForRead()
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
            RETURN SELF:_unLockForRead()
            
            
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
        
            FOREACH VAR entry IN SELF:_stack 
                entry:Clear()
            NEXT
            SELF:_TopStack := 0
            
            
        PRIVATE METHOD AllocPage() AS NtxPage
            LOCAL ntxPage AS NtxPage
            LOCAL nextPage AS LONG
            
            IF SELF:_nextUnusedPageOffset > 0
                nextPage := SELF:_nextUnusedPageOffset
                ntxPage := SELF:_PageList:Update(nextPage)
                SELF:_nextUnusedPageOffset := ntxPage[0]:PageNo
            ELSE
                nextPage := SELF:_fileSize
                SELF:_fileSize += BUFF_SIZE
                ntxPage := SELF:_PageList:Append(nextPage)
            ENDIF
            RETURN ntxPage
            
            
            
            
            
            
        
            
        INTERNAL METHOD _dump() AS VOID
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
            RETURN
        // Three methods to calculate keys. We have split these to optimize index creating
        PRIVATE METHOD _getNumFieldValue(sourceIndex AS LONG, byteArray AS BYTE[]) AS LOGIC
            SELF:_oRDD:Validate()
            Array.Copy(SELF:_oRdd:_RecordBuffer, sourceIndex, byteArray, 0, SELF:_keySize)
            SELF:_checkDigits(byteArray, SELF:_keySize, SELF:_keyDecimals)
            RETURN TRUE
            
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
                result := SELF:_ToString(oKeyValue, SELF:_keySize, SELF:_keyDecimals, byteArray, SELF:_Ansi, REF uiRealLen)
            CATCH
                result := FALSE
            END TRY
            RETURN result

    END CLASS
    
END NAMESPACE


