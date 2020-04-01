//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Text
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING XSharp.RDD.CDX
USING System.Diagnostics
USING System.IO
BEGIN NAMESPACE XSharp.RDD
    /// <summary>DBFFPT RDD. For DBF/FPT. No index support at this level</summary>
    CLASS DBFFPT INHERIT DBF 
        PRIVATE _oFptMemo AS FPTMemo
        PROTECTED INTERNAL _iExportMode AS LONG
        CONSTRUCTOR   
            SUPER()
            SELF:_Memo := _oFptMemo := FPTMemo{SELF}
            SELF:_iExportMode := BLOB_EXPORT_APPEND
            
            /// <inheritdoc />	
        PROPERTY Driver AS STRING GET "DBFFPT"


        INTERNAL METHOD DecodeFlexArray(nType AS FlexFieldType, bData AS BYTE[], nOffset REF LONG) AS OBJECT
            LOCAL iLen AS Int32
            LOCAL aValues AS OBJECT[]
            IF nType == FlexFieldType.Array16 // 16 bits
                iLen := BitConverter.ToInt16(bData, nOffset)
                nOffset += 2
            ELSEIF nType == FlexFieldType.Array32
                iLen := BitConverter.ToInt32(bData, nOffset)
                nOffset += 4
            ELSE
                RETURN NULL
            ENDIF
            aValues := OBJECT[]{iLen}
            FOR VAR i := 0 TO iLen-1
                VAR nFldType := bData[nOffset]
                LOCAL element AS OBJECT
                LOCAL length AS LONG
                nOffset += 1
                VAR nArrType := (FlexArrayTypes) nFldType
                SWITCH nArrType
                CASE FlexArrayTypes.NIL
                    element := DBNull.Value
                CASE FlexArrayTypes.Char
                    element := (SByte) bData[ nOffset]
                    nOffset += 1
                CASE FlexArrayTypes.UChar
                    element := (BYTE) bData[ nOffset]
                    nOffset += 1
                CASE FlexArrayTypes.Short
                    element := BitConverter.ToInt16(bData,nOffset)
                    nOffset += 2
                CASE FlexArrayTypes.UShort
                    element := BitConverter.ToUInt16(bData,nOffset)
                    nOffset += 2
                CASE FlexArrayTypes.Long     
                    element := BitConverter.ToInt32(bData,nOffset)
                    nOffset += 4
                CASE FlexArrayTypes.String32 
                    length := BitConverter.ToInt32(bData,nOffset)
                    nOffset += 4
                    element := _Encoding:GetString(bData, nOffset, length)
                    nOffset += length
                CASE FlexArrayTypes.String16 
                    length := BitConverter.ToInt16(bData,nOffset)
                    nOffset += 2
                    element := _Encoding:GetString(bData, nOffset, length)
                    nOffset += length
                CASE FlexArrayTypes.Float    
                    element := 0.0
                    nOffset += 10
                CASE FlexArrayTypes.Double 
                    element := BitConverter.ToDouble(bData, nOffset)
                    nOffset += 8
                CASE FlexArrayTypes.Date     
                    element := BitConverter.ToInt32(bData, nOffset)
                    nOffset += 4
                CASE FlexArrayTypes.Logic    
                    element := bData[nOffset] != 0
                    nOffset += 1
                CASE FlexArrayTypes.Array    
                    element := DecodeFlexArray(FlexFieldType.Array16, bData, REF nOffset)

                CASE FlexArrayTypes.CodeBlock
                    element := NULL
                    SELF:_dbfError(NULL, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)

                CASE FlexArrayTypes.DateJ    
                    element := BitConverter.ToInt32(bData, nOffset)
                    nOffset += 4

                CASE FlexArrayTypes.Double2  
                    element := BitConverter.ToDouble(bData, nOffset)
                    nOffset += 6

                CASE FlexArrayTypes.Cyclic
                    element := NULL
                    SELF:_dbfError(NULL, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)

                CASE FlexArrayTypes.UCHar1  
                    element := (SByte) bData[ nOffset]
                    nOffset += 2

                CASE FlexArrayTypes.Char1    
                    element := (BYTE) bData[ nOffset]
                    nOffset += 2

                CASE FlexArrayTypes.Short1   
                    element := BitConverter.ToInt16(bData, nOffset)
                    nOffset += 3
                CASE FlexArrayTypes.UShort1  
                    element := BitConverter.ToUInt16(bData, nOffset)
                    nOffset += 3
                CASE FlexArrayTypes.Long1  
                    element := BitConverter.ToInt32(bData, nOffset)
                    nOffset += 5
                CASE FlexArrayTypes.Unused
                    element := NULL
                    SELF:_dbfError(NULL, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)

                CASE FlexArrayTypes.Object
                    element := NULL
                    SELF:_dbfError(NULL, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)

                CASE FlexArrayTypes.Null     
                    element := String.Empty

                CASE FlexArrayTypes.True     
                    element := TRUE

                CASE FlexArrayTypes.False    
                    element := FALSE

                CASE FlexArrayTypes.LDouble
                    element := NULL
                    SELF:_dbfError(NULL, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)
                CASE FlexArrayTypes.UCHar2   
                    element := (SByte) bData[ nOffset]
                    nOffset += 3
                CASE FlexArrayTypes.CHar2    
                    element := (BYTE) bData[ nOffset]
                    nOffset += 3
                CASE FlexArrayTypes.Short2   
                    element := BitConverter.ToInt16(bData, nOffset)
                    nOffset += 4
                CASE FlexArrayTypes.UShort2  
                    element := BitConverter.ToUInt16(bData, nOffset)
                    nOffset += 4
                CASE FlexArrayTypes.Long2    
                    element := BitConverter.ToInt32(bData, nOffset)
                    nOffset += 6
                CASE FlexArrayTypes.ULong2   
                    element := BitConverter.ToUInt32(bData, nOffset)
                    nOffset += 6
                OTHERWISE
                    element := NULL
                    SELF:_dbfError(NULL, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)
                END SWITCH
                aValues[i] := element
            NEXT
            RETURN aValues

        INTERNAL METHOD EncodeValue(oValue AS OBJECT) AS BYTE[]
            LOCAL token AS FtpMemoToken
            LOCAL otc AS TypeCode
            LOCAL oType AS System.Type
            LOCAL bData AS BYTE[]
            IF oValue == NULL
                RETURN NULL
            ENDIF
            oType := oValue:GetType()
            otc   := System.Type.GetTypeCode(oType)
            SWITCH otc
            CASE TypeCode.String
                VAR sValue := (STRING) oValue
                bData := BYTE[] { sValue:Length+8}
                token := FtpMemoToken{bData}
                token:DataType := FlexFieldType.String
                token:Length   := (DWORD) sValue:Length
                VAR bytes := SELF:_Encoding:GetBytes(sValue)
                System.Array.Copy(bytes,0, bData,8, bytes:Length)
                RETURN bData
            CASE TypeCode.Boolean
                VAR lValue := (LOGIC) oValue
                bData := BYTE[] { 8}
                token := FtpMemoToken{bData}
                token:Length := 0
                token:DataType := IIF(lValue, FlexFieldType.LogicTrue, FlexFieldType.LogicFalse)
                RETURN bData
            END SWITCH
            RETURN NULL


        INTERNAL METHOD DecodeValue(bData AS BYTE[]) AS OBJECT
            // bData includes the header
            LOCAL encoding  AS Encoding
            LOCAL token AS FtpMemoToken
            LOCAL offset AS LONG
            token := FtpMemoToken{bData}
            encoding := SELF:_Encoding //ASCIIEncoding{}
            SWITCH token:DataType
            CASE FlexFieldType.Array16
            CASE FlexFieldType.Array32
                offset := 8
                RETURN DecodeFlexArray(token:DataType, bData, REF offset)
            CASE FlexFieldType.Picture
            CASE FlexFieldType.OleObject
                VAR buffer := BYTE[]{ bData:Length - 8}
                Array.Copy(bData,8, buffer, 0, buffer:Length)
                RETURN buffer
            CASE FlexFieldType.String
            CASE FlexFieldType.StringLong
               // Some drivers are stupid enough to allocate blocks in the FPT with a zero length..        
                IF token:Length > 0
                    IF bData[bData:Length-1] == 0
                        RETURN encoding:GetString(bData,8, bData:Length-9)
                    ELSE
                        RETURN encoding:GetString(bData,8, bData:Length-8)
                    ENDIF
                ENDIF
                RETURN ""
            CASE FlexFieldType.IndexBlock
            CASE FlexFieldType.Delete
            CASE FlexFieldType.Object16
            CASE FlexFieldType.Object32
            CASE FlexFieldType.Nil
                RETURN NULL_OBJECT
            CASE FlexFieldType.LogicTrue
                RETURN TRUE
            CASE FlexFieldType.LogicFalse
                RETURN FALSE
            CASE FlexFieldType.JDate
                RETURN FALSE
            CASE FlexFieldType.SByte
                RETURN (SByte) bData[8]
            CASE FlexFieldType.Byte
                RETURN (BYTE) bData[8]
            CASE FlexFieldType.Short
                RETURN FoxToShort(bData, 8)
            CASE FlexFieldType.Word
                RETURN FoxToWord(bData, 8)
            CASE FlexFieldType.Long
                RETURN FoxToLong(bData, 8)
            CASE FlexFieldType.Dword
                RETURN FoxToDword(bData, 8)
            CASE FlexFieldType.Double
                RETURN BitConverter.ToDouble(bData, 8)
            CASE FlexFieldType.Double10
                RETURN 0.0
            CASE FlexFieldType.Compressed
                RETURN ""
             CASE FlexFieldType.CompressedLong
                RETURN ""
            CASE FlexFieldType.ItemClipper
                RETURN NULL
            CASE FlexFieldType.LogicLong
                RETURN FoxToLong(bData, 8) != 0
            CASE FlexFieldType.StringEmpty
                RETURN ""
            CASE FlexFieldType.Illegal
                RETURN NIL
            END SWITCH
            RETURN bData

        
        METHOD GetValue(nFldPos AS LONG) AS OBJECT
            IF SELF:_isMemoField( nFldPos )
                // At this level, the return value is the raw Data, in BYTE[]
                VAR rawData := (BYTE[])SUPER:GetValue(nFldPos)
                IF rawData != NULL
                    // So, extract the "real" Data
                    RETURN SELF:DecodeValue(rawData)
                ELSE
                    RETURN String.Empty
                ENDIF
            ENDIF
            RETURN SUPER:GetValue(nFldPos)

        METHOD PutValue(nFldPos AS LONG, oValue AS OBJECT) AS LOGIC
            IF SELF:_ReadOnly
                SELF:_dbfError(ERDD.READONLY, XSharp.Gencode.EG_READONLY )
            ENDIF
            SELF:ForceRel()
            IF SELF:_readRecord()
                // GoHot() must be called first because this saves the current index values
                IF ! SELF:_Hot
                    SELF:GoHot()
                ENDIF
                VAR oColumn := SELF:_GetColumn(nFldPos)
                IF oColumn != NULL
                    IF oColumn:IsMemo
                        IF SELF:HasMemo
                            IF oValue == NULL .OR. oValue IS STRING VAR cValue .AND. cValue:Length == 0
                                VAR oldValue := oColumn:GetValue(SELF:_RecordBuffer)
                                IF oldValue != NULL
                                    SELF:_oFptMemo:DeleteBlock((INT) oldValue)
                                ENDIF
                                RETURN oColumn:PutValue(NULL, SELF:_RecordBuffer)
                            ENDIF
                            LOCAL bData AS BYTE[]
                            bData := SELF:EncodeValue(oValue)
                            IF SELF:_oFptMemo:PutValue(nFldPos, bData)
                                // Update the Field Info with the new MemoBlock Position
                                RETURN oColumn:PutValue(SELF:_oFptMemo:LastWrittenBlockNumber, SELF:_RecordBuffer)
                            ENDIF
                        ELSE
                            RETURN SUPER:PutValue(nFldPos, oValue)
                        ENDIF
                    ELSE
                        RETURN oColumn:PutValue(oValue, SELF:_RecordBuffer)
                    ENDIF
                 ENDIF
            ENDIF
            RETURN FALSE

 
            
            /// <inheritdoc />
        VIRTUAL METHOD Info(nOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
            LOCAL oResult := NULL AS OBJECT
            SWITCH nOrdinal
            CASE DbInfo.DBI_MEMOHANDLE
                IF ( SELF:_oFptMemo != NULL .AND. SELF:_oFptMemo:IsOpen)
                    oResult := SELF:_oFptMemo:_hFile
                ELSE
                    oResult := IntPtr.Zero
                ENDIF
                    
            CASE DbInfo.DBI_MEMOEXT
                IF ( SELF:_oFptMemo != NULL .AND. SELF:_oFptMemo:IsOpen)
                    oResult := System.IO.Path.GetExtension(SELF:_oFptMemo:FileName)
                ELSE
                    oResult := FPTMemo.DefExt
                ENDIF
                IF oNewValue IS STRING VAR strExt
                    FPTMemo.DefExt := strExt
                ENDIF
            CASE DbInfo.DBI_MEMOBLOCKSIZE
                oResult := SELF:_oFptMemo:BlockSize
                IF oNewValue != NULL
                    TRY
                        LOCAL size AS LONG
                        size:= Convert.ToInt32(oNewValue)
                        SELF:_oFptMemo:BlockSize := (WORD) size
                    CATCH ex AS Exception
                        oResult := ""   
                        SELF:_dbfError(ex, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)
                    END TRY
                ENDIF

            CASE DbInfo.DBI_MEMOFIELD
                oResult := ""
                IF oNewValue != NULL
                    TRY
                        LOCAL fldPos AS LONG
                        fldPos := Convert.ToInt32(oNewValue)
                        oResult := SELF:GetValue(fldPos)
                    CATCH ex AS Exception
                        oResult := ""   
                        SELF:_dbfError(ex, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)
                    END TRY
                ENDIF
            CASE DbInfo.DBI_MEMOTYPE
                oResult := DB_MEMO_FPT
            CASE DbInfo.DBI_MEMOVERSION
                oResult := DB_MEMOVER_STD
            CASE DbInfo.BLOB_GET
                // oNewValue should be object[] with 3 elements
                TRY
                    IF oNewValue IS OBJECT[] VAR oArray
                        IF oArray:Length >= 3
                            VAR nFld    := Convert.ToInt32(oArray[0])
                            VAR nOffset := Convert.ToInt32(oArray[1])
                            VAR nLen    := Convert.ToInt32(oArray[2])
                            VAR rawData := (BYTE[])SUPER:GetValue(nFld)
                            IF rawData != NULL .AND. rawData:Length > 8 // 1st 8 bytes are the header
                                VAR nDataLen := rawData:Length -8
                                nOffset += 8
                                IF nOffset <= rawData:Length 
                                    VAR nToCopy := nLen
                                    IF nToCopy == 0
                                        nToCopy := nDataLen
                                    ELSEIF nToCopy > nDataLen - nOffset + 1
                                        nToCopy := nDataLen - nOffset + 1
                                    ENDIF
                                    VAR result  := BYTE[]{nToCopy}
                                    System.Array.Copy(rawData, nOffset, result,0, nToCopy)
                                    oResult := SELF:_Encoding:GetString(result,0, nToCopy)
                                ENDIF
                            ENDIF
                        ENDIF
                    ENDIF
                CATCH ex AS Exception
                    SELF:_dbfError(ex, Subcodes.ERDD_READ, Gencode.EG_CORRUPTION, "DBFFPT.BlobGet")
                END TRY
            CASE DbInfo.BLOB_NMODE
                IF oNewValue IS LONG VAR iExportMode
                    SELF:_iExportMode := iExportMode
                ENDIF
            OTHERWISE
                oResult := SUPER:Info(nOrdinal, oNewValue)
            END SWITCH
            RETURN oResult
            
        VIRTUAL METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
           IF SELF:_ReadOnly
                SELF:_dbfError(ERDD.READONLY, XSharp.Gencode.EG_READONLY )
            ENDIF
            SELF:ForceRel()
            IF SELF:_readRecord()
                // GoHot() must be called first because this saves the current index values
                IF ! SELF:IsHot
                    SELF:GoHot()
                ENDIF
                RETURN SELF:_oFptMemo:PutValueFile(nFldPos, fileName)
            ENDIF
            RETURN FALSE
    END CLASS
            
    
 END NAMESPACE
