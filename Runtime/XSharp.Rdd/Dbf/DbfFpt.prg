//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Text
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING XSharp.RDD.CDX


INTERNAL ENUM FlexArrayTypes
    MEMBER NIL      := 0
    MEMBER UChar    := 1       // 1 byte
    MEMBER Char     := 2       // 1 byte
    MEMBER Short    := 3       // 2 bytes
    MEMBER UShort   := 4      // 2 bytes
    MEMBER Long     := 5      // 4 bytes
    MEMBER String32 := 6    // 4 bytes length followed by string
    MEMBER String16 := 7   // 2 bytes length followed by string
    MEMBER Float    := 8      // 10 bytes 
    MEMBER Double   := 9      // 8 bytes
    MEMBER Date     := 10     // 4 bytes
    MEMBER Logic    := 11    // 1 byte
    MEMBER Array    := 12    // 2 bytes length followed by array
    MEMBER CodeBlock:= 13    // ?
    MEMBER DateJ    := 14    // 4 bytes
    MEMBER Double2  := 15       // len, dec, 8 byte double
    MEMBER Cyclic   := 16       // Cyclic array, stored how ?
    MEMBER UCHar1   := 17       // byte, dec
    MEMBER Char1    := 18       // char, dec
    MEMBER Short1   := 19       // 2, followed by len
    MEMBER UShort1  := 20       // 2, followed by len
    MEMBER Long1    := 21       // 4, followed by len
    MEMBER Unused   := 22       // no data
    MEMBER Object   := 23       // ?
    MEMBER Null     := 24       // no data
    MEMBER True     := 25
    MEMBER False    := 26
    MEMBER LDouble  := 27       
    MEMBER UCHar2   := 28       // byte[1], len, dec 
    MEMBER CHar2    := 29       // byte[1], len, dec 
    MEMBER Short2   := 30       // short[2], len, dec 
    MEMBER UShort2  := 31       // ushort[2], len, dec 
    MEMBER Long2    := 32       // long[4], len, dec
    MEMBER ULong2   := 33       // ulong[4], len, dec
END ENUM


#region Conversion Functions
INTERNAL FUNCTION ShortToBuff(siValue AS SHORT, buffer AS BYTE[], nOffSet AS LONG) AS VOID
    LOCAL nValue := WordStruct{} AS WordStruct
    nValue:shortValue := siValue
    buffer[nOffSet+0]   := nValue:b1
    buffer[nOffSet+1]   := nValue:b2
    
INTERNAL FUNCTION BuffToShort(buffer AS BYTE[], nOffSet AS LONG) AS SHORT
    LOCAL nValue := WordStruct{} AS WordStruct
    nValue:b1 := buffer[nOffSet+0]   
    nValue:b2 := buffer[nOffSet+1]
    RETURN nValue:shortValue
    
INTERNAL FUNCTION ShortToFox(siValue AS SHORT, buffer AS BYTE[], nOffSet AS LONG) AS VOID
    LOCAL nValue := WordStruct{} AS WordStruct
    nValue:shortValue := siValue
    buffer[nOffSet+1]   := nValue:b1
    buffer[nOffSet+0]   := nValue:b2
    
INTERNAL FUNCTION FoxToShort(buffer AS BYTE[], nOffSet AS LONG) AS SHORT
    LOCAL nValue := WordStruct{} AS WordStruct
    nValue:b1 := buffer[nOffSet+1]   
    nValue:b2 := buffer[nOffSet+0]
    RETURN nValue:shortValue

INTERNAL FUNCTION FoxToWord(buffer AS BYTE[], nOffSet AS LONG) AS WORD
    LOCAL nValue := WordStruct{} AS WordStruct
    nValue:b1 := buffer[nOffSet+1]   
    nValue:b2 := buffer[nOffSet+0]
    RETURN nValue:wordValue
        
    
INTERNAL FUNCTION BuffToLong(buffer AS BYTE[], nOffSet AS LONG) AS LONG
    LOCAL nValue := LongStruct{} AS LongStruct
    nValue:b1 := buffer[nOffSet+0]
    nValue:b2 := buffer[nOffSet+1]
    nValue:b3 := buffer[nOffSet+2]
    nValue:b4 := buffer[nOffSet+3]
    RETURN nValue:longValue

    
INTERNAL FUNCTION LongToBuff(liValue AS LONG, buffer AS BYTE[], nOffSet AS LONG) AS LONG
    LOCAL nValue := LongStruct{} AS LongStruct
    nValue:LongValue := liValue
    buffer[nOffSet+0] := nValue:b1
    buffer[nOffSet+1] := nValue:b2
    buffer[nOffSet+2] := nValue:b3
    buffer[nOffSet+3] := nValue:b4
    RETURN liValue
    
    
INTERNAL FUNCTION FoxToLong(buffer AS BYTE[], nOffSet AS LONG) AS LONG
    LOCAL nValue := LongStruct{} AS LongStruct
    nValue:b4 := buffer[nOffSet+0]
    nValue:b3 := buffer[nOffSet+1]
    nValue:b2 := buffer[nOffSet+2]
    nValue:b1 := buffer[nOffSet+3]
    RETURN nValue:longValue

INTERNAL FUNCTION FoxToDword(buffer AS BYTE[], nOffSet AS LONG) AS DWORD
    LOCAL nValue := LongStruct{} AS LongStruct
    nValue:b4 := buffer[nOffSet+0]
    nValue:b3 := buffer[nOffSet+1]
    nValue:b2 := buffer[nOffSet+2]
    nValue:b1 := buffer[nOffSet+3]
    RETURN nValue:dwordValue

    
INTERNAL FUNCTION LongToFox(liValue AS LONG, buffer AS BYTE[], nOffSet AS LONG) AS LONG
    LOCAL nValue := LongStruct{} AS LongStruct
    nValue:LongValue := liValue
    buffer[nOffSet+0] := nValue:b4
    buffer[nOffSet+1] := nValue:b3
    buffer[nOffSet+2] := nValue:b2
    buffer[nOffSet+3] := nValue:b1
    RETURN liValue
#endregion    
    
    BEGIN NAMESPACE XSharp.RDD
    /// <summary>DBFFPT RDD. For DBF/FPT. No index support at this level</summary>
    CLASS DBFFPT INHERIT DBF 
        PRIVATE _oFptMemo AS FptMemo
        CONSTRUCTOR   
            SUPER()
            SELF:_oMemo := _oFptMemo := FptMemo{SELF}
            /// <inheritdoc />	
        PROPERTY Driver AS STRING GET "DBFFPT"



    
        /*
        METHOD DecodeFlexArray(nType as LONG, bData as byte[]) as object
            local iLen as Int32
            local aValues as Object[]
            LOCAL nOffSet   as Int32
            local nDim as Int16
            nDim    := bData[ 0]
            nOffSet := 1
            IF nType == 1002 // 16 bits
                iLen := FoxToShort(bData, nOffSet)
                nOffSet += 2
            ELSE
                iLen := FoxToLong(bData, nOffSet)
                nOffSet += 4
            ENDIF
            aValues := OBJECT[]{iLen}
            FOR VAR i := 0 to iLen-1
                var nFldType := bData[nOffSet]
                local element as Object
                local length as Long
                nOffSet += 1
                VAR nArrType := (FlexArrayTypes) nFldType
                SWITCH nArrType
                CASE FlexArrayTypes.NIL
                    element := NULL
                CASE FlexArrayTypes.Char
                    element := (sByte) bData[ nOffSet]
                    nOffSet += 1
                CASE FlexArrayTypes.UChar
                    element := (Byte) bData[ nOffSet]
                    nOffSet += 1
                CASE FlexArrayTypes.Short
                    element := FoxToShort(bData,nOffSet)
                    nOffSet += 2
                CASE FlexArrayTypes.UShort
                    element := FoxToWord(bData,nOffSet)
                    nOffSet += 2
                CASE FlexArrayTypes.Long     
                    element := BitConverter.ToUInt32(bData,nOffSet)
                    nOffSet += 4
                CASE FlexArrayTypes.String32 
                    length := BitConverter.ToUInt32(bData,nOffSet)
                    nOffSet += 4
                    element := _encoding:GetString(bData, nOffSet, length)
                    nOffSet += length
                CASE FlexArrayTypes.String16 
                    length := BitConverter.ToUInt16(bData,nOffSet)
                    nOffSet += 2
                    element := _encoding:GetString(bData, nOffSet, length)
                    nOffSet += length
                CASE FlexArrayTypes.Float    
                    element := 0.0
                    nOffSet += 10
                CASE FlexArrayTypes.Double 
                    element := BitConverter.ToDouble(bData, nOffSet)
                    nOffSet += 8
                CASE FlexArrayTypes.Date     
                    element := BitConverter.ToInt32(bData, nOffSet)
                    nOffSet += 4
                CASE FlexArrayTypes.Logic    
                    element := bData[nOffSet] != 0
                    nOffSet += 1
                CASE FlexArrayTypes.Array    
                    element := bData[nOffSet] != 0
                    nOffSet += 1

                CASE FlexArrayTypes.CodeBlock 
                    SELF:_dbfError(null, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, "DBFDBT.Info")

                CASE FlexArrayTypes.DateJ    
                    element := BitConverter.ToInt32(bData, nOffSet)
                    nOffSet += 4
                CASE FlexArrayTypes.Double2  
                    element := BitConverter.ToDouble(bData, nOffSet)
                    nOffSet += 6

                CASE FlexArrayTypes.Cyclic   
                    SELF:_dbfError(null, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, "DBFDBT.Info")
                CASE FlexArrayTypes.UCHar1  
                    element := (sByte) bData[ nOffSet]
                    nOffSet += 2
                CASE FlexArrayTypes.Char1    
                    element := (Byte) bData[ nOffSet]
                    nOffSet += 2
                CASE FlexArrayTypes.Short1   
                    element := FoxToShort(bData, nOffSet)
                    nOffSet += 3
                CASE FlexArrayTypes.UShort1  
                    element := FoxToWord(bData, nOffSet)
                    nOffSet += 3
                CASE FlexArrayTypes.Long1  
                    element := FoxToLong(bData, nOffSet)
                    nOffSet += 5
                CASE FlexArrayTypes.Unused   
                    SELF:_dbfError(null, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, "DBFDBT.Info")
                CASE FlexArrayTypes.Object   
                    SELF:_dbfError(null, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, "DBFDBT.Info")
                CASE FlexArrayTypes.Null     
                    element := NULL
                CASE FlexArrayTypes.True     
                    element := TRUE
                CASE FlexArrayTypes.False    
                    element := FALSE
                CASE FlexArrayTypes.LDouble
                    SELF:_dbfError(null, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, "DBFDBT.Info")
                CASE FlexArrayTypes.UCHar2   
                    element := (SByte) bData[ nOffSet]
                    nOffSet += 3
                CASE FlexArrayTypes.CHar2    
                    element := (Byte) bData[ nOffSet]
                    nOffSet += 3
                CASE FlexArrayTypes.Short2   
                    element := FoxToShort(bData, nOffSet)
                    nOffSet += 4
                CASE FlexArrayTypes.UShort2  
                    element := FoxToWord(bData, nOffSet)
                    nOffSet += 4
                CASE FlexArrayTypes.Long2    
                    element := FoxToLong(bData, nOffSet)
                    nOffSet += 6
                CASE FlexArrayTypes.ULong2   
                    element := FoxToDWord(bData, nOffSet)
                    nOffSet += 6
                OTHERWISE
                    SELF:_dbfError(null, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, "DBFDBT.Info")
                END SWITCH
                aValues[i] := element
            NEXT
            RETURN aValues
            */

        METHOD DecodeValue(nType as LONG, bData as BYTE[]) as object
            LOCAL encoding  as Encoding
             encoding := SELF:_Encoding //ASCIIEncoding{}
             IF nType == 0
                return bData
            ELSEIF nType == 1
                return encoding:GetString(bData)
            ELSE
                IF SELF:_oFptMemo:isFlex
                    SWITCH nType
                    CASE 1000  // indexblock
                        return NULL
                    CASE 1001  // Deleted
                        return NULL
                    CASE 1002  // 16 bit array
                        //return DecodeFlexArray(nType, bData)
                        return bData
                    CASE 1003  // Clipper Object
                        return NULL_OBJECT
                    CASE 1004  // 32 bit array
                        //return DecodeFlexArray(nType, bData)
                        return bData
                    CASE 1005  // VO Object
                    CASE 1006  // NIL
                        return NULL_OBJECT
                    CASE 1007  // TRUE
                        return TRUE
                    CASE 1008  // FALSE
                        return FALSE
                    CASE 1009  // JDATE
                        return FALSE
                    CASE 1010  // SByte
                        return (Sbyte) bData[0]
                    CASE 1011  // Byte
                        return (byte) bData[0]
                    CASE 1012  // Short
                        return FoxToShort(bData, 0)
                    CASE 1013  // Word
                        return FoxToWord(bData, 0)
                    CASE 1014  // Int32
                        return FoxToLong(bData, 0)
                    CASE 1015  // DWORD
                        return FoxToDword(bData, 0)
                    CASE 1016  // 8 byte double
                        return BitConverter.ToDouble(bData, 0)
                    CASE 1017  // 10 byte double
                        return 0.0
                    CASE 1018  // Compressed Characters
                        return ""
                    CASE 1019  // String Longer > 32 K
                        return encoding:GetString(bData)
                    CASE 1020  // Compressed string > 32 K
                        return ""
                    case 10000  // 14 byte clipper item with a double
                        return 0.0
                    case 10001  // Logical stored as 4 byte Long
                        return FoxToLong(bData, 0) != 0
                    case 10002  // empty string
                        return ""
                    case -1     // Illegal
                        return NIL
                    end switch
                ENDIF
            ENDIF
            RETURN bData


        
        METHOD GetValue(nFldPos AS LONG) AS OBJECT
            LOCAL nType AS LONG
            IF SELF:_isMemoField( nFldPos )
                // At this level, the return value is the raw Data, in BYTE[]
                var rawData := (BYTE[])SUPER:GetValue(nFldPos)
                IF rawData != NULL
                    // So, extract the "real" Data
                    VAR buffer := BYTE[]{ rawData:Length - 8}
                    Array.Copy(rawData,8, buffer, 0, buffer:Length)
                    // Get the Underlying type from the MemoBlock
                    // 0 : Picture (on MacOS); 1: Memo; 2 : Object
                    nType := FoxToLong( rawData, 0)
                    RETURN SELF:DecodeValue(nType, buffer)
                ELSE
                    RETURN String.Empty
                ENDIF
            ENDIF
            RETURN SUPER:GetValue(nFldPos)
            
            /// <inheritdoc />
        VIRTUAL METHOD Info(nOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
            LOCAL oResult AS OBJECT
            SWITCH nOrdinal
            CASE DbInfo.DBI_MEMOHANDLE
                IF ( SELF:_oFptMemo != NULL .AND. SELF:_oFptMemo:_Open)
                    oResult := SELF:_oFptMemo:_hFile
                ELSE
                    oResult := IntPtr.Zero
                ENDIF
                    
            CASE DbInfo.DBI_MEMOEXT
                    IF ( SELF:_oFptMemo != NULL .AND. SELF:_oFptMemo:_Open)
                        oResult := System.IO.Path.GetExtension(SELF:_oFptMemo:_FileName)
                    ELSE
                        oResult := FptMemo.DefExt
                    ENDIF
                    IF oNewValue IS STRING
                        FptMemo.DefExt := (STRING) oNewValue
                    ENDIF
            CASE DbInfo.DBI_MEMOBLOCKSIZE
                oResult := SELF:_oFptMemo:BlockSize
                IF oNewValue != NULL
                    TRY
                        LOCAL size AS LONG
                        size:= Convert.ToInt32(oNewValue)
                        SELF:_oFptMemo:BlockSize := (SHORT) size
                    CATCH ex AS exception
                        oResult := ""   
                        SELF:_dbfError(ex, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, "DBFDBT.Info")
                    END TRY
                ENDIF

            CASE DBInfo.DBI_MEMOFIELD
                oResult := ""
                IF oNewValue != NULL
                    TRY
                        LOCAL fldPos AS LONG
                        fldPos := Convert.ToInt32(oNewValue)
                        oResult := SELF:GetValue(fldPos)
                    CATCH ex AS exception
                        oResult := ""   
                        SELF:_dbfError(ex, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, "DBFDBT.Info")
                    END TRY
                ENDIF
            CASE DbInfo.DBI_MEMOTYPE
                oResult := DB_MEMO_FPT
            CASE DbInfo.DBI_MEMOVERSION
                oResult := DB_MEMOVER_STD
            OTHERWISE
                oResult := SUPER:Info(nOrdinal, oNewValue)
            END SWITCH
            RETURN oResult
            
            
            END CLASS
            
            
    /// <summary>FPT Memo class. Implements the FTP support.</summary>
    // To also support FPT files created with FlexFile we also need to read the FlexFile header and decode that.

    INTERNAL CLASS FPTMemo INHERIT BaseMemo IMPLEMENTS IMemo
        INTERNAL _hFile	    AS IntPtr
        INTERNAL _FileName  AS STRING
        INTERNAL _Open      AS LOGIC
        PROTECT _Shared     AS LOGIC
        PROTECT _ReadOnly   AS LOGIC
        PROTECT _oRDD       AS DBF
        PROTECT _isFlex     AS LOGIC
        PROTECT _nextFree   AS LONG
        PROTECT _blockSize  AS SHORT
        PROTECT _verMajor   as BYTE
        PROTECT _verMinor   as BYTE
        PROTECT _indexHosed as BYTE
        PROTECT _indexLen as LONG
        PROTECT _indexLoc as LONG
        PROTECT _updateCnt as LONG
        PROTECT _rootPtr as LONG
        PROTECT _updCount as LONG
        PROTECT _lockScheme AS DbfLocking
        PROTECT _altBlockSize as SHORT
        PROPERTY Shared AS LOGIC GET _Shared
        STATIC PROPERTY DefExt AS STRING AUTO
        PROTECTED PROPERTY IsOpen AS LOGIC GET SELF:_hFile != F_ERROR  .AND. SELF:_hFile != IntPtr.Zero      
        INTERNAL PROPERTY IsFlex as LOGIC GET _isFlex
        STATIC CONSTRUCTOR
            DefExt := FPT_MEMOEXT
            
        PRIVATE METHOD GetMemoExtFromDbfExt(cDbfName AS STRING) AS STRING
            SWITCH System.IO.Path.GetExtension(cDbfName:ToLower())
            CASE ".vcx"         // Control Library
                RETURN ".VCT"
            CASE ".scx"         // Screen
                RETURN ".SCT"
            CASE ".pjx"         // Project
                RETURN ".PJT"
            CASE ".frx"         // Report
                RETURN ".FRT"
            CASE ".mnx"         // Menu
                RETURN ".MNT"
            CASE ".dbc"         // database container
                RETURN ".dct"
            END SWITCH
            RETURN DefExt
            
            
        CONSTRUCTOR (oRDD AS DBF)
            SUPER(oRDD)
            SELF:_oRdd := oRDD
            SELF:_hFile := IntPtr.Zero
            SELF:_Shared := SELF:_oRDD:_Shared
            SELF:_ReadOnly := SELF:_oRdd:_ReadOnly
            SELF:_Encoding := SELF:_oRdd:_Encoding
            
            
        VIRTUAL PROTECTED METHOD _initContext() AS VOID
            SELF:_lockScheme:Initialize( DbfLockingModel.FoxPro )
            SELF:ReadHeader()
            IF ( SELF:BlockSize == 0 )
                //SELF:BlockSize := FPT_DEFBLOCKSIZE
                SELF:_BlockSize := (SHORT) XSharp.RuntimeState.MemoBlockSize
                SELF:_WriteHeader()
            ENDIF
            
            
            /// <inheritdoc />
        METHOD Flush() 			AS LOGIC		
            LOCAL isOk AS LOGIC
            isOk := SELF:IsOpen
            IF isOk
                isOk := FFlush( SELF:_hFile )
            ENDIF
            RETURN isOk
            
            /// <inheritdoc />
        METHOD GetValue(nFldPos AS INT) AS OBJECT
            LOCAL blockNbr AS LONG
            LOCAL blockLen := 0 AS LONG
            LOCAL memoBlock := NULL AS BYTE[]
            //
            blockNbr := SELF:_oRDD:_getMemoBlockNumber( nFldPos )
            IF ( blockNbr > 0 )
                // Get the raw Length of the Memo
                blockLen := SELF:_getValueLength( nFldPos )
                IF blockLen > 0 
                    memoBlock := BYTE[]{ blockLen }
                    // Where do we start ?
                    LOCAL iOffset := blockNbr * SELF:BlockSize AS LONG
                    //
                    FSeek3( SELF:_hFile, iOffset, FS_SET )
                    // Max 512 Blocks
                    LOCAL isOk AS LOGIC
                    isOk := ( FRead3( SELF:_hFile, memoBlock, (DWORD)blockLen ) == blockLen )
                    IF ( !isOk )
                        memoBlock := NULL
                    ENDIF
                ENDIF
            ENDIF
            // At this level, the return value is the raw Data, in BYTE[]
            RETURN memoBlock
            
            /// <inheritdoc />
        METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
            THROW NotImplementedException{}
            
            /// <inheritdoc />
        METHOD GetValueLength(nFldPos AS INT) AS INT
            LOCAL blockLen := 0 AS LONG
            blockLen := SELF:_getValueLength( nFldPos )
            // Don't forget to remove the 8 Bytes
            blockLen := blockLen - 8
            RETURN blockLen
            
            // Get the raw data length
        VIRTUAL PROTECTED METHOD _getValueLength(nFldPos AS INT) AS INT
            // In FPT :
            // The first 4 Bytes contains the type of Memo Data
            // The next 4 Bytes contains the length of Memo data, including the first 8 bytes
            LOCAL blockNbr AS LONG
            LOCAL blockLen := 0 AS LONG
            // Where does the block start ?
            blockNbr := SELF:_oRDD:_getMemoBlockNumber( nFldPos )
            IF ( blockNbr > 0 ) 
                // File Open ?
                LOCAL isOk := ( SELF:_hFile != F_ERROR ) AS LOGIC
                IF isOk
                    //
                    LOCAL blockInfo AS BYTE[]
                    blockInfo := BYTE[]{8}
                    // Where do we start ?
                    LOCAL iOffset := blockNbr * SELF:_blockSize AS LONG
                    // Go to the blockNbr position
                    FSeek3( SELF:_hFile, iOffset, FS_SET )
                    // 
                    FRead3( SELF:_hFile, blockInfo, (DWORD) blockInfo:Length )
                    blockLen     := FoxToLong(blockInfo,4) +8
                    // The raw length include the 8 Bytes included in the Memo Block
                ENDIF
            ENDIF
            RETURN blockLen
            
            /// <inheritdoc />
        VIRTUAL METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
            // We receive the raw Data : We now have to put extra information on it
            // 4 Bytes for the "type" of memo data
            // 4 Bytes for the length of block
            LOCAL objType AS System.Type
            LOCAL objTypeCode AS System.TypeCode
            LOCAL str AS STRING
            LOCAL rawData AS BYTE[]
            LOCAL memoBlock AS BYTE[]
            //
            IF !SELF:IsOpen
                RETURN FALSE
            ENDIF
            //
            objType := oValue:GetType()
            objTypeCode := Type.GetTypeCode( objType )
            //
            IF ( objTypeCode == TypeCode.Char )
                str := STRING{ (CHAR)oValue, 1 }
            ELSEIF ( objTypeCode == TypeCode.String )
                str := oValue ASTYPE STRING
            ELSE
                str := NULL
            ENDIF
            // Not a Char, Not a String
            IF ( str == NULL )
                rawData := (BYTE[])oValue
            ELSE
                LOCAL encoding AS Encoding //ASCIIEncoding
                rawData := BYTE[]{str:Length}
                encoding := SELF:_Encoding //ASCIIEncoding{}
                encoding:GetBytes( str, 0, str:Length, rawData, 0 )
            ENDIF
            // Add the extra Info
            memoBlock := BYTE[]{ rawData:Length + 4 + 4 }
            // Put the Data
            Array.Copy(rawData,0, memoBlock, 8, rawData:Length)
            // now, put the length
            LongToFox(memoBlock:Length, memoBlock,4)
            // And finally, put the Data Type
            // 0 : Picture (on MacOS); 1: Memo; 2 : Object
            LOCAL nType := 2 AS LONG
            var column := SELF:_oRdd:_GetColumn(nFldPos)
            IF ( column:FieldType == DbFieldType.Memo )
                nType := 1
            ELSEIF ( column:FieldType == DbFieldType.Picture )
                nType := 0
            ENDIF
            LongToFox(nType, memoBlock,0)
            // Ok, we now have a FPT Memoblock, save
            // TODO: write the value to the file, including:
            // - check if the current block exists
            // - check if the current block has enough space
            // - add the current block to the 'deleted blocks' list when not enough space
            // - allocate a new block from either the list of deleted blocks or at the end of the file
            // - and of course locking the file.
            RETURN SUPER:PutValue( nFldPos, memoBlock )
            
            /// <inheritdoc />
        VIRTUAL METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
            THROW NotImplementedException{}
            
            /// <inheritdoc />
        VIRTUAL METHOD CloseMemFile( ) AS LOGIC
            LOCAL isOk := FALSE AS LOGIC
            IF SELF:IsOpen
                //
                TRY
                    isOk := FClose( SELF:_hFile )
                    
                CATCH ex AS Exception
                    isOk := FALSE
                    SELF:_oRDD:_dbfError(ex, SubCodes.ERDD_CLOSE_MEMO, GenCode.EG_CLOSE, "DBFDBT.CloseMemFile")
                    
                END TRY
                SELF:_hFile := F_ERROR
                SELF:_Open  := FALSE
            ENDIF
            RETURN isOk
            
            /// <inheritdoc />
        VIRTUAL METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC
            LOCAL isOk AS LOGIC
            SELF:_FileName  := info:FileName
            VAR cExt        := GetMemoExtFromDbfExt(SELF:_FileName)
            SELF:_FileName  := System.IO.Path.ChangeExtension( SELF:_FileName, cExt )
            SELF:_Shared    := info:Shared
            SELF:_ReadOnly  := info:ReadOnly
            //
            SELF:_hFile     := FCreate( SELF:_FileName) 
            isOk := SELF:IsOpen
            IF isOk
                // Per default, Header Block Size if 512
                LOCAL memoHeader AS BYTE[]
                LOCAL nextBlock AS LONG
                
                memoHeader := BYTE[]{ DBTMemo.HeaderSize }
                nextBlock := 1
                Array.Copy(BitConverter.GetBytes(nextBlock),0, memoHeader, 0, SIZEOF(LONG))
                
                FWrite3( SELF:_hFile, memoHeader, DBTMemo.HeaderSize )
                
                SELF:_initContext()
            ELSE
                SELF:_oRDD:_DbfError( ERDD.CREATE_MEMO, XSharp.Gencode.EG_CREATE )
            ENDIF
            
            RETURN isOk
            
            /// <inheritdoc />
        VIRTUAL METHOD OpenMemFile(info AS DbOpenInfo ) AS LOGIC
            LOCAL isOk AS LOGIC
            SELF:_FileName  := info:FileName
            VAR cExt        := GetMemoExtFromDbfExt(SELF:_FileName)
            SELF:_FileName  := System.IO.Path.ChangeExtension( SELF:_FileName, cExt )
            SELF:_Shared    := info:Shared
            SELF:_ReadOnly  := info:ReadOnly
            //
            SELF:_hFile     := Fopen(SELF:_FileName, info:FileMode)
            isOk := SELF:IsOpen
            IF isOk
                // Per default, Block Size if 512
                SELF:_initContext()
            ELSE
                SELF:_oRDD:_DbfError( ERDD.OPEN_MEMO, XSharp.Gencode.EG_OPEN )
                isOk := FALSE
            ENDIF
            //
            RETURN isOk
            
        VIRTUAL PROPERTY BlockSize 	 AS SHORT GET _BlockSize SET _blockSize := value

       
        // Place a lock : <nOffset> indicate where the lock should be; <nLong> indicate the number bytes to lock
        // If it fails, the operation is tried <nTries> times, waiting 1ms between each operation.
        // Return the result of the operation
        PROTECTED METHOD _tryLock( nOffset AS UINT64, nLong AS LONG, nTries AS LONG  ) AS LOGIC
            LOCAL locked AS LOGIC
            IF ! SELF:IsOpen
                RETURN FALSE
            ENDIF
            REPEAT
                TRY
                    locked := FFLock( SELF:_hFile, (DWORD)nOffset, (DWORD)nLong )
                CATCH ex AS Exception
                    locked := FALSE
                    SELF:_oRDD:_dbfError(ex, SubCodes.ERDD_INIT_LOCK, GenCode.EG_LOCK_ERROR, "DBFDBT._tryLock")
                END TRY
                IF ( !locked )
                    nTries --
                    IF ( nTries > 0 )
                        System.Threading.Thread.Sleep( 1 )
                    ENDIF
                ENDIF
            UNTIL ( locked .OR. (nTries==0) )
            //
            RETURN locked
            
        PROTECTED METHOD _unlock( nOffset AS UINT64, nLong AS LONG ) AS LOGIC
            LOCAL unlocked AS LOGIC
            IF ! SELF:IsOpen
                RETURN FALSE
            ENDIF

            TRY
                unlocked := FFUnLock( SELF:_hFile, (DWORD)nOffset, (DWORD)nLong )
            CATCH ex AS Exception
                unlocked := FALSE
                SELF:_oRDD:_dbfError(ex, SubCodes.ERDD_UNLOCKED, GenCode.EG_UNLOCKED, "DBFDBT._unlock")
                
            END TRY
            RETURN unlocked
            
        VIRTUAL METHOD Zap() AS LOGIC
            THROW NotImplementedException{}

        PRIVATE METHOD ReadHeader() AS LOGIC
            // FoxPro memo Header:
            // Byte offset  Description
            // 00 ? 03      Location of next free block1
            // 04 ? 05      Unused
            // 06 ? 07      Block size (bytes per block)1
            // 08 ? 511     Unused
            // FlexFile Header starts as 512. Following positions are relative
            // 00 - 08      Header "FlexFile3"
            // 09 - 09      Version Major
            // 10 - 10      Version Minor
            // 11 - 11      Index = defect
            // 12 - 15      Index Length
            // 16 - 19      Index location
            // 20 - 23      Root Pointer
            // 24 - 27      Update Count
            // 28 - 29      Alternative Block Size (allows block sizes < 32)

            VAR buffer := BYTE[]{512}
            LOCAL savedPos := FSeek3(SELF:_hFile, 0, FS_RELATIVE ) AS LONG
            LOCAL nFileLen as LONG
            nFileLen := FSeek3(SELF:_hFile, 0, FS_END)
            FSeek3(SELF:_hFile, 0, FS_SET)
            Fread3( SELF:_hFile,buffer, 512)
            _nextFree  := BitConverter.ToInt32(buffer, 0)
            _blockSize := BitConverter.ToInt16(buffer, 6)
            // read Flex Header
            if nFileLen >= 1024
                Fread3( SELF:_hFile,buffer, 512)
                _isFlex := buffer[0] == 70  ;// F
                            .and. buffer[1] == 108; // l
                            .and. buffer[2] == 101; // e
                            .and. buffer[3] == 120; // x
                            .and. buffer[4] == 70 ; // F
                            .and. buffer[5] == 105; // i
                            .and. buffer[6] == 108; // l
                            .and. buffer[7] == 101;  // e
                            .and. buffer[8] == 51   // 3
                IF _isFlex
                    SELF:_verMajor   :=  buffer[9]
                    SELF:_verMinor   :=  buffer[10]
                    SELF:_indexHosed := buffer[11]
                    self:_indexLen   := BitConverter.ToInt32(buffer, 12)
                    self:_indexLoc   := BitConverter.ToInt32(buffer, 16)
                    self:_rootPtr    := BitConverter.ToInt32(buffer, 20)
                    self:_updCount   := BitConverter.ToInt32(buffer, 24)
                    SELF:_altBlockSize := BitConverter.ToInt16(buffer, 28)
                    SELF:_blockSize   := _altBlockSize
                ENDIF
            ELSE
                _isFlex := FALSE
                SELF:_verMajor   := 0
                SELF:_verMinor   := 0
                SELF:_indexHosed := 0
                self:_indexLen   := 0
                self:_indexLoc   := 0
                self:_updateCnt  := 0
                self:_rootPtr    := 0
                SELF:_altBlockSize := 0
            ENDIF
            FSeek3(SELF:_hFile, savedPos, FS_SET )
            RETURN TRUE
        METHOD _WriteHeader() AS VOID
            IF SELF:IsOpen
                LOCAL _sizeOfBlock AS BYTE[]
                _sizeOfBlock := BYTE[]{2}
                ShortToFox( SELF:_blockSize, _sizeOfBlock,0)
                LOCAL savedPos := FSeek3(SELF:_hFile, 0, FS_RELATIVE ) AS LONG
                FSeek3( SELF:_hFile, 6, FS_SET )
                FWrite3( SELF:_hFile, _sizeOfBlock, 2 )
                FSeek3(SELF:_hFile, savedPos, FS_SET )
            ENDIF
            RETURN
   

    END CLASS    
    
    
    END NAMESPACE
