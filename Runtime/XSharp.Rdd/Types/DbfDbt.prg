//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Text
USING XSharp.RDD.Enums

BEGIN NAMESPACE XSharp.RDD
    /// <summary>DBFDBT RDD. For DBF/DBT. No index support at this level</summary>
    CLASS DBFDBT INHERIT DBF
        CONSTRUCTOR
            SUPER()
            SELF:_oMemo := DBTMemo{SELF}
            
        VIRTUAL PROPERTY SysName AS STRING GET TYPEOF(DbfDbt):ToString()
        
        
        METHOD GetValue(nFldPos AS LONG) AS OBJECT
            LOCAL fieldType AS DbFieldType
            LOCAL cType AS STRING
            LOCAL buffer AS BYTE[]
            //
            cType := (STRING)SELF:FieldInfo( nFldPos, DbFieldInfo.DBS_TYPE, NULL )
            fieldType := (DbFieldType) Char.ToUpper(cType[0])
            IF ( fieldType == DbFieldType.Memo )
                // At this level, the return value is the raw Data, in BYTE[]
                buffer := (BYTE[])SUPER:GetValue(nFldPos)
                LOCAL encoding AS ASCIIEncoding
                LOCAL str AS STRING
                encoding := ASCIIEncoding{}
                str :=  encoding:GetString(buffer)
                // Convert to String and return
                RETURN str
            ENDIF
            RETURN SUPER:GetValue(nFldPos)
            
            /// <summary>DBT Memo class. Implements the DBT support.</summary>
            // See https://www.clicketyclick.dk/databases/xbase/format/dbt.html#DBT_STRUCT/
            //
        CLASS DBTMemo INHERIT BaseMemo IMPLEMENTS IMemo
            PROTECT _hFile	    AS IntPtr
            PROTECT _FileName   AS STRING
            PROTECT _Shared     AS LOGIC
            PROTECT _ReadOnly   AS LOGIC
            PROTECT _oRDD       AS DBF
            PROTECT _blockSize  AS LONG
            
            
            CONSTRUCTOR (oRDD AS DBF)
                SUPER(oRDD)
                SELF:_oRdd := oRDD
                SELF:_blockSize := 512
                SELF:_hFile := F_ERROR
                
                
                /// <inheritdoc />
            METHOD Flush() 			AS LOGIC
                LOCAL isOk AS LOGIC
                isOk := ( SELF:_hFile != F_ERROR )
                IF isOk
                    isOk := FFlush( SELF:_hFile )
                ENDIF
                RETURN isOk
                
                /// <inheritdoc />
            METHOD GetValue(nFldPos AS INT) AS OBJECT
                LOCAL blockNbr AS LONG
                LOCAL blockLen := 0 AS LONG
                LOCAL memoBlock AS BYTE[]
                //
                blockNbr := SELF:_oRDD:_getMemoBlockNumber( nFldPos )
                IF ( blockNbr > 0 ) 
                    blockLen := SELF:GetValueLength( nFldPos )
                    IF blockLen > 0 
                        memoBlock := BYTE[]{ blockLen }
                        // Where do we start ?
                        LOCAL iOffset := blockNbr * SELF:_blockSize AS LONG
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
                LOCAL blockNbr AS LONG
                LOCAL blockLen := 0 AS LONG
                // Where does the block start ?
                blockNbr := SELF:_oRDD:_getMemoBlockNumber( nFldPos )
                IF ( blockNbr > 0 ) 
                    // File Open ?
                    LOCAL isOk := ( SELF:_hFile != F_ERROR ) AS LOGIC
                    IF isOk
                        // We don't need to store all the data, just read block by block
                        // in a temporary buffer, to count the needed size
                        LOCAL memoBlock AS BYTE[]
                        memoBlock := BYTE[]{SELF:_blockSize}
                        // Where do we start ?
                        LOCAL iOffset := blockNbr * SELF:_blockSize AS LONG
                        // Go to the blockNbr position
                        FSeek3( SELF:_hFile, iOffset, FS_SET )
                        // 
                        LOCAL sizeRead := 1 AS LONG
                        LOCAL endPos := -1 AS LONG
                        WHILE ( sizeRead > 0 ) .AND. ( endPos == - 1 )
                            sizeRead := (LONG)FRead3( SELF:_hFile, memoBlock, (DWORD)SELF:_blockSize )
                            IF ( sizeRead > 0 )
                                // Search for one field terminator (1Ah)
                                endPos := Array.FindIndex<BYTE>( memoBlock, { x => x == 0x1A } )
                                IF ( endPos == - 1 )
                                    blockLen += sizeRead
                                ELSE
                                    // Some docs says that Memo ends with two field teminator ???
                                    // https://www.clicketyclick.dk/databases/xbase/format/dbt.html
                                    // but ok, let say only one.
                                    blockLen += endPos
                                ENDIF
                            ENDIF
                        ENDDO
                    ENDIF
                ENDIF
                RETURN blockLen
                
                /// <inheritdoc />
                
            VIRTUAL METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
                LOCAL objType AS System.Type
                LOCAL objTypeCode AS System.TypeCode
                LOCAL str AS STRING
                LOCAL memoBlock AS BYTE[]
                LOCAL isOk := FALSE AS LOGIC
                // 
                objType := oValue:GetType()
                objTypeCode := Type.GetTypeCode( objType )
                //
                IF ( objTypeCode == TypeCode.Char )
                    str := STRING{ (CHAR)oValue, 1 }
                ELSEIF ( objTypeCode == TypeCode.String )
                    str := oValue ASTYPE STRING
                ENDIF
                // Not a Char, Not a String
                IF ( str == NULL )
                    memoBlock := (BYTE[])oValue
                ELSE
                    LOCAL encoding AS ASCIIEncoding
                    memoBlock := BYTE[]{str:Length}
                    encoding := ASCIIEncoding{}
                    encoding:GetBytes( str, 0, str:Length, memoBlock, 0 )
                ENDIF
                // Now, calculate where we will write the Datas
                LOCAL blockNbr AS LONG
                LOCAL blockLen := 0 AS LONG
                LOCAL newBlock AS LOGIC
                // Suppose we will have a new start of block
                newBlock := TRUE
                // Where are currently stored the datas ?
                blockNbr := SELF:_oRDD:_getMemoBlockNumber( nFldPos )
                IF ( blockNbr > 0 ) 
                    // Existing block ? What is it's current length ?
                    blockLen := SELF:GetValueLength( nFldPos )
                    IF blockLen > 0 
                        // Compare the number of Blocks : Do we need more blocks ?
                        newBlock := ( memoBlock:Length / SELF:_blockSize ) > ( blockLen / SELF:_blockSize )
                    ENDIF
                ENDIF
                IF newBlock
                    // Go to the end of end, where we will add the new data
                    FSeek3( SELF:_hFile, 0, FS_END )
                    LOCAL fileSize := (LONG)FTell( SELF:_hFile ) AS LONG
                    // But first, check that the Size of Memo file is multiple of BlockSize
                    LOCAL toAdd := fileSize % SELF:_blockSize AS LONG
                    // No ! Fill the gap
                    IF ( toAdd > 0 )
                        toAdd := SELF:_blockSize - toAdd
                        LOCAL dummy AS BYTE[]
                        dummy := BYTE[]{ toAdd }
                        FWrite3( SELF:_hfile, dummy, (DWORD)toAdd )
                        // so, new size is
                        fileSize := (LONG)FTell( SELF:_hFile )
                    ENDIF
                    // The new block Number to write to is (I supposed we should use NextAvailableBlock, no ?)
                    blockNbr := fileSize / SELF:_blockSize
                ELSE
                    // Go to the position of the blockNbr
                    FSeek3( SELF:_hFile, blockNbr * SELF:_blockSize, FS_SET )
                ENDIF
                // Ok, now write the Datas
                isok := ( FWrite3( SELF:_hfile, memoBlock, (DWORD)memoBlock:Length ) == memoBlock:Length )
                IF isOk
                    // Don't forget to write an End Of Block terminator (1Ah) (Should it be two ??)
                    LOCAL eob := <BYTE>{0x1A} AS BYTE[]
                    isOk := ( FWrite3( SELF:_hFile, eob, 1 ) == 1 )
                    IF isOk .AND. newBlock
                        // We have to update the next block info
                        LOCAL newPos AS LONG // FTell might do the job ?
                        newPos := blockNbr * SELF:_blockSize + memoBlock:Length + 1
                        newPos += SELF:_blockSize - newPos % SELF:_blockSize
                        SELF:NextAvailableBlock := newPos / SELF:_blockSize
                    ENDIF
                    IF isOk
                        isOk := SELF:Flush()
                    ENDIF
                ENDIF
                //
                IF !isOk
                    SELF:_oRDD:_DbfError( ERDD.WRITE, XSharp.Gencode.EG_WRITE )
                ENDIF
                // Now, update the information in the DBF, look at PutValue() in the DbfDbt Class
                SELF:LastWrittenBlockNumber := blockNbr
                RETURN isOk
                
                /// <inheritdoc />
            VIRTUAL METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
                THROW NotImplementedException{}
                
                /// <inheritdoc />
            VIRTUAL METHOD CloseMemFile( ) AS LOGIC
                LOCAL isOk := FALSE AS LOGIC
                IF ( SELF:_hFile != NULL )
                    //
                    TRY
                        isOk := FClose( SELF:_hFile )
                    CATCH
                        isOk := FALSE
                    END TRY
                    SELF:_hFile := NULL
                ENDIF
                RETURN isOk
                
                /// <inheritdoc />
            VIRTUAL METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC
                LOCAL isOk AS LOGIC
                SELF:_FileName  := info:FileName
                SELF:_FileName  := System.IO.Path.ChangeExtension( SELF:_FileName, DBT_MEMOEXT )
                SELF:_Shared    := info:Shared
                SELF:_ReadOnly  := info:ReadOnly
                //
                SELF:_hFile     := FCreate( SELF:_FileName) 
                isOk := ( SELF:_hFile != F_ERROR )
                IF isOk
                    // Per default, Header Block Size if 512
                    LOCAL memoHeader AS BYTE[]
                    LOCAL nextBlock AS LONG
                    //
                    memoHeader := BYTE[]{ 512 }
                    nextBlock := 1
                    Array.Copy(BitConverter.GetBytes(nextBlock),0, memoHeader, 0, SIZEOF(LONG))
                    //
                    FWrite3( SELF:_hFile, memoHeader, 512 )
                ELSE
                    SELF:_oRDD:_DbfError( ERDD.CREATE_MEMO, XSharp.Gencode.EG_CREATE )
                ENDIF
                //
                RETURN isOk
                
                /// <inheritdoc />
            VIRTUAL METHOD OpenMemFile(info AS DbOpenInfo ) AS LOGIC
                LOCAL isOk AS LOGIC
                SELF:_FileName  := info:FileName
                SELF:_FileName  := System.IO.Path.ChangeExtension( SELF:_FileName, DBT_MEMOEXT )
                SELF:_Shared    := info:Shared
                SELF:_ReadOnly  := info:ReadOnly
                //
                SELF:_hFile     := Fopen(SELF:_FileName, info:FileMode)
                isOk := ( SELF:_hFile != F_ERROR )
                IF isOk
                    // Per default, Block Size if 512
                ELSE
                    SELF:_oRDD:_DbfError( ERDD.OPEN_MEMO, XSharp.Gencode.EG_OPEN )
                    isOk := FALSE
                ENDIF
                //
                RETURN isOk
                
            PROPERTY NextAvailableBlock 	 AS LONG
                GET 
                    LOCAL nBlock := 0 AS LONG
                    LOCAL isOk AS LOGIC
                    isOk := ( SELF:_hFile != F_ERROR )
                    IF isOk
                        LOCAL _memoBlock AS BYTE[]
                        // NextBlock is at the beginning of MemoFile
                        _memoBlock := BYTE[]{4}
                        LOCAL savedPos := FSeek3(SELF:_hFile, 0, FS_RELATIVE ) AS LONG
                        FSeek3( SELF:_hFile, 0, FS_SET )
                        IF ( FRead3( SELF:_hFile, _memoBlock, 4 ) == 4 )
                            nBlock := BitConverter.ToInt32( _memoBlock, 0)
                        ENDIF
                        FSeek3(SELF:_hFile, savedPos, FS_SET )
                    ENDIF
                    RETURN nBlock
                END GET
                
                SET 
                    LOCAL nBlock := 0 AS LONG
                    LOCAL isOk AS LOGIC
                    isOk := ( SELF:_hFile != F_ERROR )
                    IF isOk
                        LOCAL _memoBlock AS BYTE[]
                        _memoBlock := BYTE[]{4}
                        Array.Copy(BitConverter.GetBytes(VALUE),0, _memoBlock, 0, SIZEOF(LONG))
                        LOCAL savedPos := FSeek3(SELF:_hFile, 0, FS_RELATIVE ) AS LONG
                        FSeek3( SELF:_hFile, 0, FS_SET )
                        FWrite3( SELF:_hFile, _memoBlock, 4 )
                        FSeek3(SELF:_hFile, savedPos, FS_SET )
                    ENDIF
                END SET
                
            END PROPERTY
            
            
        END CLASS    
        
    END CLASS
END NAMESPACE
