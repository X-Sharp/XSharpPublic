//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Text
USING XSharp.RDD.Enums

BEGIN NAMESPACE XSharp.RDD
    /// <summary>DBFFPT RDD. For DBF/FPT. No index support at this level</summary>
    CLASS DBFFPT INHERIT DBF 
        CONSTRUCTOR
            SUPER()
            SELF:_oMemo := FptMemo{SELF}
            /// <inheritdoc />	
        VIRTUAL PROPERTY SysName AS STRING GET TYPEOF(DBFFPT):ToString()
        
        METHOD GetValue(nFldPos AS LONG) AS OBJECT
            LOCAL rawData AS BYTE[]
            LOCAL buffer AS BYTE[]
            LOCAL nType AS LONG
            // At this level, the return value is the raw Data, in BYTE[]
            rawData := (BYTE[])SUPER:GetValue(nFldPos)
            // So, extract the "real" Data
            buffer := BYTE[]{ rawData:Length - 8}
            Array.Copy(rawData,8, buffer, 0, buffer:Length)
            // Get the Underlying type from the MemoBlock
            // 0 : Picture (on MacOS); 1: Memo; 2 : Object
            nType := BitConverter.ToInt32( rawData, 0)
            IF SELF:_isMemoField( nFldPos ) .AND. ( nType == 1 )
                LOCAL encoding AS ASCIIEncoding
                LOCAL str AS STRING
                encoding := ASCIIEncoding{}
                str :=  encoding:GetString(buffer)
                // Convert to String and return
                RETURN str
            ENDIF
            RETURN buffer
            
            // Indicate if a Field is a Memo; Called by GetValue() in Parent Class
            // At DbfFpt Level, TRUE for DbFieldType.Memo, DbFieldType.Picture, DbFieldType.Object
        INTERNAL VIRTUAL METHOD _isMemoFieldType( fieldType AS DbFieldType ) AS LOGIC
            RETURN ( ( fieldType == DbFieldType.Memo ) .OR. ( fieldType == DbFieldType.Picture ) .OR. ( fieldType == DbFieldType.Ole ) )
            
            
            END CLASS
            
            
    /// <summary>FPT Memo class. Implements the FTP support.</summary>
    INTERNAL CLASS FptMemo INHERIT DBTMemo 
    
        CONSTRUCTOR (oRDD AS DBF)
            SUPER(oRDD)
            //
            SELF:DefExt := FPT_MEMOEXT
            
            // Called from CreateMemFile : The Header has been filled with 0, so the BlockSize is 0
            // Called from OpenMemFile : The Header contains the Size of Block to use
        VIRTUAL PROTECTED METHOD _initContext() AS VOID
            SELF:_lockScheme:Initialize( DBF.DbfLockingModel.FoxPro )
            IF ( SELF:BlockSize == 0 )
                //SELF:BlockSize := FPT_DEFBLOCKSIZE
                SELF:BlockSize := (SHORT) XSharp.RuntimeState.MemoBlockSize
            ENDIF
            
            
            /// <inheritdoc />
        METHOD Flush() 			AS LOGIC		
            RETURN SUPER:Flush()
            
            /// <inheritdoc />
        METHOD GetValue(nFldPos AS INT) AS OBJECT
            // At this level, the return value is the raw Data, in BYTE[]
            // This will ALSO include the 8 Bytes of FPT
            // The first 4 Bytes contains the tpye of Memo Data
            // The next 4 Bytes contains the length of Memo data, including the first 8 bytes            
            RETURN SELF:GetValue( nFldPos )
            
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
            // The first 4 Bytes contains the tpye of Memo Data
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
                    LOCAL memoType AS BYTE[]
                    LOCAL memoLen  AS BYTE[]
                    memoType := BYTE[]{4}
                    memoLen := BYTE[]{4}                    
                    // Where do we start ?
                    LOCAL iOffset := blockNbr * SELF:_blockSize AS LONG
                    // Go to the blockNbr position
                    FSeek3( SELF:_hFile, iOffset, FS_SET )
                    // 
                    FRead3( SELF:_hFile, memoType, 4 )
                    FRead3( SELF:_hFile, memoLen, 4 )
                    blockLen := BitConverter.ToInt32( memoLen, 0)
                    // The raw length include the 8 Bytes included in the Memo Block
                ENDIF
            ENDIF
            RETURN blockLen
            
            /// <inheritdoc />
        VIRTUAL METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
            // We receive hea the raw Data : We now have to put extra information on it
            // 4 Bytes for the "type" of memo data
            // 4 Bytes for the length of block
            LOCAL objType AS System.Type
            LOCAL objTypeCode AS System.TypeCode
            LOCAL str AS STRING
            LOCAL rawData AS BYTE[]
            LOCAL memoBlock AS BYTE[]
            //
            IF ( SELF:_hFile == F_ERROR )
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
                LOCAL encoding AS ASCIIEncoding
                rawData := BYTE[]{str:Length}
                encoding := ASCIIEncoding{}
                encoding:GetBytes( str, 0, str:Length, rawData, 0 )
            ENDIF
            // Add the extra Info
            memoBlock := BYTE[]{ rawData:Length + 4 + 4 }
            // Put the Data
            Array.Copy(rawData,0, memoBlock, 8, rawData:Length)
            // now, put the length
            Array.Copy(BitConverter.GetBytes(memoBlock:Length),0, memoBlock, 4, SIZEOF(LONG))
            // And finally, put the Data Type
            // 0 : Picture (on MacOS); 1: Memo; 2 : Object
            LOCAL nType := 2 AS LONG
            IF ( SELF:_oRdd:_FieldType( nFldPos ) == DbFieldType.Memo )
                nType := 1
            ELSEIF ( SELF:_oRdd:_FieldType( nFldPos ) == DbFieldType.Picture )
                nType := 0
            ENDIF
            Array.Copy(BitConverter.GetBytes( nType ),0, memoBlock, 0, SIZEOF(LONG))
            // Ok, we now have a FPT Memoblock, save
            RETURN SUPER:PutValue( nFldPos, memoBlock )
            
            /// <inheritdoc />
        VIRTUAL METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
            THROW NotImplementedException{}
            
            /// <inheritdoc />
        VIRTUAL METHOD CloseMemFile( ) AS LOGIC
            RETURN SUPER:CloseMemFile()
            
            /// <inheritdoc />
        VIRTUAL METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC
            RETURN SUPER:CreateMemFile( info )
            
            /// <inheritdoc />
        VIRTUAL METHOD OpenMemFile(info AS DbOpenInfo ) AS LOGIC
            RETURN SUPER:OpenMemFile( info )
            
            
        Virtual PROPERTY BlockSize 	 AS SHORT
            GET 
                LOCAL nSize := 0 AS SHORT
                LOCAL isOk AS LOGIC
                isOk := ( SELF:_hFile != F_ERROR )
                IF isOk
                    LOCAL _sizeOfBlock AS BYTE[]
                    // _sizeOfBlock is at the beginning of MemoFile, at posiion 6
                    _sizeOfBlock := BYTE[]{2}
                    LOCAL savedPos := FSeek3(SELF:_hFile, 0, FS_RELATIVE ) AS LONG
                    FSeek3( SELF:_hFile, 6, FS_SET )
                    IF ( FRead3( SELF:_hFile, _sizeOfBlock, 2 ) == 2 )
                        nSize := BitConverter.ToInt16( _sizeOfBlock, 0)
                    ENDIF
                    FSeek3(SELF:_hFile, savedPos, FS_SET )
                    SELF:_blockSize := nSize
                ENDIF
                RETURN nSize
            END GET
            
            SET 
                LOCAL isOk AS LOGIC
                isOk := ( SELF:_hFile != F_ERROR )
                IF isOk
                    LOCAL _sizeOfBlock AS BYTE[]
                    _sizeOfBlock := BYTE[]{2}
                    Array.Copy(BitConverter.GetBytes(VALUE),0, _sizeOfBlock, 0, SIZEOF(SHORT))
                    LOCAL savedPos := FSeek3(SELF:_hFile, 0, FS_RELATIVE ) AS LONG
                    FSeek3( SELF:_hFile, 6, FS_SET )
                    FWrite3( SELF:_hFile, _sizeOfBlock, 2 )
                    FSeek3(SELF:_hFile, savedPos, FS_SET )
                    SELF:_blockSize := VALUE
                ENDIF
            END SET
            
        END PROPERTY
        
        
        
    END CLASS    
    
    
END NAMESPACE
