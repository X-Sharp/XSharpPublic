// NtxOrder.prg
// Created by    : fabri
// Creation Date : 7/9/2018 7:10:50 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO

BEGIN NAMESPACE XSharp.RDD

    /// <summary>
    /// The NtxOrder class.
    /// </summary>
    CLASS NtxOrder
    
        PROTECT _oRDD           AS DBFNTX
        PROTECT _Header         AS NtxHeader
        PROTECT _PageList       AS NtxPageList
        PROTECT _fileName       AS STRING
        PROTECT _hFile          AS IntPtr
        PROTECT _Shared         AS LOGIC
        PROTECT _ReadOnly       AS LOGIC
        PROTECT _Conditional    AS LOGIC
        PROTECT _SingleField    AS INT
        PROTECT _Hot            AS LOGIC
        PROTECT _OrderName      AS STRING
        PROTECT _HPLocking      AS LOGIC
        
        PROTECT _KeyExpr        AS STRING
        PROTECT _ForExpr        AS STRING
        PROTECT _cbKeyExpr      AS ICodeBlock
        PROTECT _cbForExpr      AS ICodeBlock
        
        PROTECT _lockScheme     AS Dbf.DbfLocking
        
        /// <inheritdoc />
        CONSTRUCTOR(oRDD AS DBFNTX)
            _oRDD := oRDD
            
        CONSTRUCTOR(oRDD AS DBFNTX, filePath AS STRING)
            SELF(oRDD)   
            //_oRDD := oRDD
            // Get the FileName
            SELF:FileName := filePath
            
            
        PROPERTY FileName AS STRING
            GET
                RETURN SELF:_fileName
            END GET
            SET
                SELF:_FileName := VALUE
                IF ( String.IsNullOrEmpty( SELF:_FileName ) )
                    SELF:_FileName := SELF:_oRDD:_FileName
                    //SELF:_FileName := Path.ChangeExtension(SELF:_FileName, ".ntx")
                ENDIF
                // and be sure to have NTX
                SELF:_FileName := Path.ChangeExtension(SELF:_FileName, ".ntx")
                // Check that we have a FullPath
                IF (Path.GetDirectoryName(SELF:_FileName):Length == 0)
                    SELF:_FileName := Path.GetDirectoryName(SELF:_oRDD:_FileName) + Path.DirectorySeparatorChar + SELF:_FileName
                ENDIF
            END SET
        END PROPERTY
        
        
        METHOD Open( dbordInfo AS DBORDERINFO ) AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL i AS LONG
            //
            isOk := FALSE
            SELF:_oRDD:GoCold()
            //
            SELF:_Shared := SELF:_oRDD:_Shared
            SELF:_ReadOnly := SELF:_oRDD:_ReadOnly
            //
            SELF:_hFile    := Fopen(SELF:_FileName, SELF:_oRDD:_OpenInfo:FileMode) 
            IF ( SELF:_hFile == F_ERROR )
                SELF:_oRDD:_dbfError( ERDD.OPEN_ORDER, GenCode.EG_OPEN, SELF:_fileName)
                RETURN FALSE
            ENDIF
            //
            SELF:_Header := NtxHeader{ SELF:_hFile }
            IF (!SELF:_Header:Read())
                SELF:_oRDD:_dbfError(ERDD.OPEN_ORDER, GenCode.EG_OPEN, SELF:_fileName)
                RETURN FALSE
            ENDIF
            SELF:_PageList := NtxPageList{SELF}
            // Index Key Expression
            SELF:_KeyExpr := SELF:_Header:Bytes:KeyExpression
            IF (!SELF:_oRDD:Compile(SELF:_KeyExpr))
                SELF:_oRDD:_dbfError(SubCodes.EDB_EXPRESSION, GenCode.EG_SYNTAX, "DBFNTX.Compile")
                RETURN FALSE
            ENDIF
            SELF:_cbKeyExpr := (ICodeblock)SELF:_oRDD:_LastCodeBlock
            // Move to first position
            SELF:_oRDD:GoTo(1)
            //
            SELF:_oRDD:EvalBlock(SELF:_cbKeyExpr)
            SELF:_KeyExprType := SELF:_getTypeCode(SELF:_oRDD:_EvalResult)
            // For Expression ?
            SELF:_Conditional := FALSE
            SELF:_ForExpr := SELF:_Header:Bytes:ForExpression
            IF (SELF:_ForExpr:Length > 0)
                IF (!SELF:_oRDD:Compile(SELF:_ForExpr))
                    SELF:_oRDD:_dbfError(SubCodes.EDB_EXPRESSION, GenCode.EG_SYNTAX, "DBFNTX.Compile")
                    RETURN FALSE
                ENDIF
                SELF:_cbForExpr := (ICodeblock)SELF:_oRDD:_LastCodeBlock
                SELF:_oRDD:GoTo(1)
                SELF:_oRDD:EvalBlock(SELF:_cbForExpr)
                SELF:_Conditional := TRUE
            ENDIF
            // If the Key Expression contains only a Field Name
            SELF:_SingleField := -1
            FOR i := 1 TO SELF:_oRDD:FieldCount
                IF (string.Compare(SELF:_KeyExpr, SELF:_oRDD:FieldName(i), TRUE) == 0)
                    SELF:_SingleField := i
                    EXIT
                ENDIF
            NEXT
            SELF:_Hot := FALSE
            SELF:ClearStack()
            //
            SELF:m_nodeMid := NtxItem{SELF:m_uiKeySize, SELF:_oRDD}
            SELF:m_lpNode := NtxItem{SELF:m_uiKeySize, SELF:_oRDD}
            //
            IF ( !String.IsNullOrEmpty(SELF:_Header:Bytes:OrdName))
                SELF:_OrderName := SELF:_Header:Bytes:OrdName
            ELSE
                SELF:_OrderName := Path.GetFileNameWithoutExtension(SELF:_fileName)
            ENDIF
            SELF:_OrderName := SELF:_OrderName:ToUpper()
            //
            IF ((SELF:_Header:Bytes:Signature & HP_LOCK) != 0)
                SELF:_HPLocking := TRUE
            ENDIF
            // Standard Locking Scheme
            SELF:_lockScheme:Initialize( DbfLockingModel.Clipper52 )
            // Except
            IF ((SELF:_Header:Bytes:Signature & NEW_LOCK ) != 0)
                SELF:_lockScheme:Offset := -1
            ENDIF
            //
            isOk := TRUE
            IF (SELF:_HPLocking .AND. SELF:_Shared)
                IF (!SELF:_lockInit())
                    SELF:_oRDD:_dbfError(SubCodes.ERDD_INIT_LOCK, GenCodes.EG_LOCK, "DBFNTX.LockInit", SELF:_fileName)
                    isOk := FALSE
                ENDIF
            ENDIF
            IF (!isOk)
                SELF:Flush()
                SELF:Close()
            ENDIF
            RETURN isOk
            
        PUBLIC METHOD Flush() AS LOGIC
            SELF:GoCold()
            IF (((!SELF:_Shared) .AND. (SELF:_Hot)) .AND. (SELF:_hFile != F_ERROR))
                SELF:_PageList:Flush(TRUE)
                SELF:_Header:Bytes:IndexingVersion := 1
                SELF:_Header:Bytes:NextUnusedPageOffset := (DWORD)SELF:m_lNextPage
                SELF:_Header:Bytes:FirstPageOffset := (DWORD)SELF:m_lRoot
                SELF:_Header:Write( )
            ENDIF
            FFlush( SELF:_hFile )
            RETURN TRUE
            
        PUBLIC METHOD Commit() AS LOGIC
            SELF:GoCold()
            IF (((!SELF:_Shared) .AND. (SELF:_Hot)) .AND. (SELF:_hFile != F_ERROR))
                SELF:_Header:Bytes:IndexingVersion := 1
                SELF:_Header:Bytes:NextUnusedPageOffset := (DWORD)SELF:m_lNextPage
                SELF:_Header:Bytes:FirstPageOffset := (DWORD)SELF:m_lRoot
                SELF:_Header:Write( )
            ENDIF
            FFlush( SELF:_hFile )
            RETURN TRUE
            
        PUBLIC METHOD GoCold() AS LOGIC
            IF (SELF:_oRDD:Hot)
                RETURN SELF:_KeyUpdate(SELF:_oRDD:RecNo, SELF:_oRDD:IsNewRecord )
            ENDIF
            RETURN TRUE
            
        PUBLIC METHOD Close() AS LOGIC
            SELF:Flush()
            TRY
                IF ((SELF:_Shared) .AND. (SELF:_HPLocking))
                    SELF:LockExit()
                ENDIF
                IF (SELF:_hFile != F_ERROR)
                    FClose( SELF:_hFile )
                    SELF:_hFile := F_ERROR
                ENDIF
                
            FINALLY
                SELF:_HPLocking := FALSE
                SELF:_hFile := F_ERROR
            END TRY
            RETURN TRUE
            
            
            
            // Vulcan.RDD.DBFNTX/NtxOrder
            USING System
            USING Vulcan.Runtime
            
        PRIVATE METHOD _KeyUpdate( recno AS DWORD , isNew AS LOGIC ) AS LOGIC
            LOCAL flag AS LOGIC
            LOCAL flag2 AS LOGIC
            LOCAL num AS DWORD
            LOCAL flag3 AS LOGIC
            LOCAL num2 AS LONG
            LOCAL num3 AS DWORD
            LOCAL errorInfo AS ErrorInfo
            //
            flag := FALSE
            flag2 := TRUE
            num := 0u
            flag3 := TRUE
            num2 := 0
            flag3 := TRUE
            num3 := 0u
            IF (SELF:_Shared)
            IF (SELF:_HPLocking)
                num3 := SELF:m_uiReadLocks
                WHILE SELF:m_uiReadLocks != 0
                    flag3 := SELF:_ReadUnLock()
                    IF (!flag3)
                        num2 := 2
                        EXIT
                    ENDIF
                END WHILE
            ENDIF
            IF ((flag3) .AND. (!SELF:_WriteLock()))
                num2 := 2
                GOTOENDIF
            ENDIF
            IF (SELF:m_fConditional)
            IF (!SELF:m_Area:EvalBlock(SELF:m_itmCobFor))
                num2 := 1
                GOTOENDIF
                TRY
                    flag2 := (LOGIC)SELF:m_Area:m_valResult
                    
                CATCH innerExcept AS Exception
                    errorInfo := SELF:m_Area:PostError(GenCodes.EG_DATATYPE, SubCodes.ERDD_KEY_EVAL, SELF:m_strFileName)
                    errorInfo:ArgTypeType := TYPEOF(LOGIC)
                    errorInfo:Args := <OBJECT>;
                    {SELF:m_Area:m_valResult;
                    }
                    errorInfo:InnerExcept := innerExcept
                END TRY
            ENDIF
            IF (!SELF:m_Area:EvalBlock(SELF:m_itmCobExpr))
                num2 := 1
            ELSE
                IF (!SELF:__ValToStr(SELF:m_Area:m_valResult, SELF:m_uiKeySize, SELF:m_uiKeyDec, SELF:m_abNewKey, SELF:m_fAnsi))
                    num2 := 1
                ELSE
                    IF (!isNew)
                        flag := (TCMP(SELF:m_abNewKey, SELF:m_abKnownKey, SELF:m_uiKeySize) != 0)
                        IF (flag)
                            SELF:m_uiTop := 0u
                        ENDIF
                        num := SELF:__GoRecord(SELF:m_abKnownKey, SELF:m_uiKeySize, recno)
                        IF ((((SELF:m_uiTop != 0) .AND. (!SELF:m_fConditional))) .OR. (num != 0))
                            IF ((flag) .OR. (!flag2))
                                SELF:__DeleteKey()
                            ENDIF
                        ELSE
                            IF (((!SELF:m_fUnique) .AND. (!SELF:m_fConditional)) .AND. (!SELF:m_fPartial))
                                errorInfo := SELF:m_Area:PostError(GenCodes.EG_DATATYPE, SubCodes.ERDD_KEY_NOT_FOUND, SELF:m_strFileName)
                                errorInfo:FileName := SELF:m_strFileName
                                errorInfo:CanDefault := TRUE
                                errorInfo:Tries := 1u
                                IF (flag2)
                                    isNew := TRUE
                                ENDIF
                            ENDIF
                        ENDIF
                    ENDIF
                    IF ((((isNew) .OR. (flag))) .AND. (flag2))
                        SELF:m_nodeMid:KeyBytes := SELF:m_abNewKey
                        SELF:m_nodeMid:PageNo := 0L
                        SELF:m_nodeMid:Recno := recno
                        SELF:m_uiTop := 0u
                        IF (SELF:m_fUnique)
                            IF (SELF:__Locate(SELF:m_nodeMid:KeyBytes, SELF:m_uiKeySize, NtxSearchMode.Left, SELF:m_lRoot) == 0)
                                SELF:__AddKey()
                            ELSE
                                SELF:m_uiTop := 0u
                            ENDIF
                        ELSE
                            SELF:__Locate(SELF:m_nodeMid:KeyBytes, SELF:m_uiKeySize, NtxSearchMode.Right, SELF:m_lRoot)
                            SELF:__AddKey()
                        ENDIF
                        SELF:m_uiTop := 0u
                        SELF:m_fHot := TRUE
                    ENDIF
                    Array.Copy(SELF:m_abNewKey, SELF:m_abKnownKey, SELF:m_uiKeySize + 1)
                    SELF:m_lKnownRec := recno
                    num2 := 0
                ENDIF
            ENDIF

            GOTO.
            IF (num2 <= 1)
                IF (SELF:_Shared)
                    SELF:m_PageList:Flush(TRUE)
                    SELF:m_uiVersion++
                    SELF:__PutHeader()
                    SELF:m_fHot := FALSE
                    SELF:m_FileStream:Flush()
                    SELF:__WriteUnLock()
                    IF (SELF:_HPLocking)
                        WHILE (num3 != 0) .AND. (!SELF:__ReadLock())
                            num3--
                        END WHILE
                    ENDIF
                ENDIF
                RETURN TRUE
            ENDIF
            IF (num2 == 2)
                SELF:m_Area:PostError(GenCodes.EG_DATATYPE, SubCodes.ERDD_KEY_EVAL)
                RETURN FALSE
            ENDIF
            RETURN TRUE
            
            
            
        PRIVATE METHOD _getTypeCode(oValue AS OBJECT ) AS TypeCode
            LOCAL typeCode AS TypeCode
            //
            IF (oValue == NULL)
                typeCode := TypeCode.Empty
            ELSE
                IF (oValue ASTYPE IDate)
                    typeCode := TypeCode.DateTime
                ELSE
                    IF (oValue ASTYPE IFloat)
                        typeCode := TypeCode.Double
                    ELSE
                        typeCode := Type.GetTypeCode(oValue:GetType())
                        SWITCH typeCode
                        CASE TypeCode.SByte
                            CASE TypeCode.Byte
                            CASE TypeCode.Int16
                            CASE TypeCode.UInt16
                            CASE TypeCode.Int32
                                typeCode := TypeCode.Int32
                                
                            CASE TypeCode.UInt32
                            CASE TypeCode.Int64
                            CASE TypeCode.UInt64
                            
                            CASE TypeCode.Single
                            CASE TypeCode.Double
                                typeCode := TypeCode.Double
                            CASE TypeCode.Boolean
                                typeCode := TypeCode.Boolean
                            CASE TypeCode.String
                                typeCode := TypeCode.String
                            CASE TypeCode.DateTime
                                typeCode := TypeCode.DateTime
                            END SWITCH
                    ENDIF
                ENDIF
            ENDIF
            RETURN typeCode            
            
        PRIVATE METHOD _genSeed() AS LONG
            LOCAL dateTime AS DateTime
            //
            dateTime := DateTime{Environment.TickCount}
            RETURN ((dateTime:Hour * 60 + dateTime:Minute) * 60 + dateTime:Second) * 100 + dateTime:Millisecond
            
            
        PROTECTED METHOD _lockBytes( nOffset AS UINT64, nLong AS LONG  ) AS LOGIC
            LOCAL locked AS LOGIC
            //
            TRY
                locked := FFLock( SELF:_hFile, (DWORD)nOffset, (DWORD)nLong )
            CATCH ex AS Exception
                Trace.WriteLine("Lock Error:" + ex:Message)
                locked := FALSE
            END TRY
            //
            RETURN locked
            
        PROTECTED METHOD _unlockBytes( nOffset AS UINT64, nLong AS LONG  ) AS LOGIC
            LOCAL unlocked AS LOGIC
            //
            TRY
                unlocked := FFUnLock( SELF:_hFile, (DWORD)nOffset, (DWORD)nLong )
            CATCH ex AS Exception
                Trace.WriteLine("UnLock Error:" + ex:Message)
                unlocked := FALSE
            END TRY
            //
            RETURN unlocked
            
        PRIVATE METHOD _lockInit() AS LOGIC
            LOCAL tries AS LONG
            LOCAL seed AS LONG
            //
            tries := 0
            seed := 0
            SELF:_parkPlace := 0
            seed := _genSeed()
            // MAX_TRIES := 50 
            WHILE (tries++ < MAX_TRIES ) .AND. (SELF:_parkPlace == 0)
                IF (seed <= 0)
                    seed := 1
                ENDIF
                seed := (seed * 1221 + 1) % PARKLOT_SIZE //1025
                SELF:_parkPlace := seed
                IF !SELF:_lockBytes( ~(SELF:_parkPlace + TOKEN_AREA), 1)
                    RETURN FALSE
                ENDIF
            ENDDO
            //
            RETURN TRUE
            
        PRIVATE METHOD _lockExit() AS LOGIC
            RETURN SELF:_unLockBytes( ~(SELF:m_lockInfo:liSpace + 1), 1)
            
            
            /// <inheritdoc />
        METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
            THROW NotImplementedException{}
            /// <inheritdoc />
        METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC
            THROW NotImplementedException{}
            /// <inheritdoc />
        METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC
            THROW NotImplementedException{}
            /// <inheritdoc />
        METHOD OrderInfo(nOrdinal AS INT) AS OBJECT
            THROW NotImplementedException{}
            /// <inheritdoc />
        METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC
            THROW NotImplementedException{}
            /// <inheritdoc />
        METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC
            THROW NotImplementedException{}
            /// <inheritdoc />
        METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC
            THROW NotImplementedException{}
            /// <inheritdoc />
        METHOD OrderListRebuild( ) AS LOGIC
            THROW NotImplementedException{}
            /// <inheritdoc />
        METHOD Seek(info AS DbSeekInfo) AS LOGIC		
            THROW NotImplementedException{}
            /// <inheritdoc />
        VIRTUAL PROPERTY Found AS LOGIC	
            GET
                THROW NotImplementedException{}
            END GET
        END PROPERTY     
        
        
        
        
        
        
        
        
        
    END CLASS
END NAMESPACE // XSharp.RDD