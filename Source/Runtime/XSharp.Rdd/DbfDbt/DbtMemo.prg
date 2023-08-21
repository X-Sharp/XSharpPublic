// AbstractMemo.prg
// Created by    : robert
// Creation Date : 4/1/2020 10:21:25 AM
// Created for   :
// WorkStation   : ARTEMIS


USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING System.IO

BEGIN NAMESPACE XSharp.RDD
      /// <summary>DBT Memo class. Implements the DBT support.</summary>
    // See https://www.clicketyclick.dk/databases/xbase/format/dbt.html#DBT_STRUCT/
    //
    INTERNAL CLASS DBTMemo INHERIT AbstractMemo IMPLEMENTS IMemo

        STATIC PROPERTY DefExt AS STRING AUTO
        STATIC CONSTRUCTOR
            DefExt := DBT_MEMOEXT

        INTERNAL CONST HeaderSize := 512 AS LONG

        CONSTRUCTOR (oRDD AS DBF)
            SUPER(oRDD)
            SELF:_oRdd := oRDD

        VIRTUAL PROTECTED METHOD _initContext() AS VOID
            SELF:_blockSize := DBT_DEFBLOCKSIZE
            SELF:_lockScheme:Initialize( DbfLockingModel.Clipper52 )

            /// <inheritdoc />
        OVERRIDE METHOD GetValue(nFldPos AS INT) AS OBJECT
            LOCAL blockNbr AS LONG
            LOCAL blockLen := 0 AS LONG
            LOCAL memoBlock := NULL AS BYTE[]
            //
            blockNbr := SELF:_oRdd:_getMemoBlockNumber( nFldPos )
            IF ( blockNbr > 0 )
                // Get the raw Length of the Memo
                blockLen := SELF:_getValueLength( nFldPos )
                IF blockLen > 0
                    memoBlock := BYTE[]{ blockLen }
                    // Where do we start ?
                    LOCAL iOffset := blockNbr * SELF:BlockSize AS LONG
                    //
                    _oStream:SafeSetPos( iOffset)
                    // Max 512 Blocks
                    LOCAL isOk AS LOGIC
                    isOk := _oStream:SafeRead(memoBlock)
                    IF ( !isOk )
                        memoBlock := NULL
                    ENDIF
                ENDIF
            ENDIF
            // At this level, the return value is the raw Data, in BYTE[]
            RETURN memoBlock

            /// <inheritdoc />
        OVERRIDE METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
            THROW NotImplementedException{}

            /// <inheritdoc />
        OVERRIDE METHOD GetValueLength(nFldPos AS INT) AS INT
            RETURN SELF:_getValueLength( nFldPos )

            // Do the calculation for the MemoLength
        VIRTUAL PROTECTED METHOD _getValueLength(nFldPos AS INT) AS INT
            LOCAL blockNbr AS LONG
            LOCAL blockLen := 0 AS LONG
            // Where does the block start ?
            blockNbr := SELF:_oRdd:_getMemoBlockNumber( nFldPos )
            IF ( blockNbr > 0 )
                // File Open ?
                LOCAL isOk := ( SELF:_hFile != F_ERROR ) AS LOGIC
                IF isOk
                    // We don't need to store all the data, just read block by block
                    // in a temporary buffer, to count the needed size
                    LOCAL memoBlock AS BYTE[]
                    memoBlock := BYTE[]{SELF:BlockSize}
                    // Where do we start ?
                    LOCAL iOffset := blockNbr * SELF:BlockSize AS LONG
                    // Go to the blockNbr position
                    _oStream:SafeSetPos( iOffset)
                    //
                    LOCAL sizeRead := 1 AS LONG
                    LOCAL endPos := -1 AS LONG
                    WHILE ( sizeRead > 0 ) .AND. ( endPos == - 1 )
                        _oStream:SafeRead(memoBlock, SELF:BlockSize, OUT sizeRead)
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
        OVERRIDE METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
            LOCAL objType AS System.Type
            LOCAL objTypeCode AS System.TypeCode
            LOCAL str := NULL AS STRING
            LOCAL memoBlock AS BYTE[]
            LOCAL isOk := FALSE AS LOGIC
            LOCAL locked := FALSE AS LOGIC
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
            ENDIF
            // Not a Char, Not a String
            IF ( str == NULL )
                memoBlock := (BYTE[])oValue
            ELSE
                memoBlock := BYTE[]{str:Length}
                SELF:Encoding:GetBytes( str, 0, str:Length, memoBlock, 0 )
            ENDIF
            // Now, calculate where we will write the Datas
            LOCAL blockNbr AS INT64
            LOCAL blockLen := 0 AS LONG
            LOCAL newBlock AS LOGIC
            // Suppose we will have a new start of block
            newBlock := TRUE
            // Where are currently stored the datas ?
            blockNbr := SELF:_oRdd:_getMemoBlockNumber( nFldPos )
            IF ( blockNbr > 0 )
                // Existing block ? What is it's current raw length ?
                blockLen := SELF:_getValueLength( nFldPos )
                IF blockLen > 0
                    // Compare the number of Blocks : Do we need more blocks ?
                    newBlock := ( memoBlock:Length / SELF:_blockSize ) > ( blockLen / SELF:BlockSize )
                ENDIF
            ENDIF
            IF newBlock
                IF SELF:Shared
                    locked := SELF:_tryLock( SELF:_lockScheme:Offset, 1, (LONG)XSharp.RuntimeState.LockTries )
                ENDIF
                // Go to the end of end, where we will add the new data
                _oStream:Seek(0, SeekOrigin.End)
                VAR fileSize := _oStream:Length
                // But first, check that the Size of Memo file is multiple of BlockSize
                LOCAL toAdd := (INT) (fileSize % SELF:BlockSize) AS INT
                // No ! Fill the gap
                IF ( toAdd > 0 )
                    toAdd := SELF:BlockSize - toAdd
                    LOCAL filler AS BYTE[]
                    filler := BYTE[]{ toAdd }
                    _oStream:SafeWrite(filler)
                    // so, new size is
                    fileSize := (LONG)_oStream:Length

                ENDIF
                // The new block Number to write to is (I supposed we should use NextAvailableBlock, no ?)
                blockNbr := fileSize / SELF:BlockSize
            ELSE
                // Go to the position of the blockNbr
                _oStream:SafeSetPos( blockNbr * SELF:BlockSize)
            ENDIF
            // Ok, now write the Datas
            isOk := _oStream:SafeWrite(memoBlock)
            IF isOk
                // Don't forget to write an End Of Block terminator (1Ah) (Should it be two ??)
                isOk := _oStream:SafeWriteByte(0x1A)
                IF isOk .AND. newBlock
                    // We have to update the next block info
                    LOCAL newPos AS LONG // FTell might do the job ?
                    newPos := (LONG) blockNbr * SELF:BlockSize + memoBlock:Length + 1
                    newPos += (LONG) SELF:BlockSize - newPos % SELF:BlockSize
                    SELF:NextAvailableBlock := newPos / SELF:BlockSize
                ENDIF
                IF isOk
                    isOk := SELF:Flush()
                ENDIF
            ENDIF
            IF ( locked )
                isOk := SELF:_unlock( SELF:_lockScheme:Offset, 1 )
            ENDIF
            //
            IF !isOk
                SELF:Error( FException(), ERDD.WRITE, XSharp.Gencode.EG_WRITE ,"DBTMemo.PutValue")
            ENDIF
            // Now, update the information in the DBF, look at PutValue() in the DbfDbt Class
            SELF:LastWrittenBlockNumber := (LONG) blockNbr
            RETURN isOk

            /// <inheritdoc />
        OVERRIDE METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
            THROW NotImplementedException{}

        /// <summary>Return the extension set with Set.MemoExt or default extension</summary>
        PRIVATE METHOD _GetExtension() AS STRING
            VAR cExt := RuntimeState.GetValue<STRING>(Set.MemoExt)
            IF ! String.IsNullOrEmpty(cExt)
                IF ! cExt.StartsWith(".")
                    cExt := "."+cExt
                    RuntimeState.SetValue(Set.MemoExt, cExt)
                ENDIF
                RETURN cExt
            ENDIF
            RETURN DefExt

            /// <inheritdoc />
        OVERRIDE METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC
            LOCAL isOk      AS LOGIC
            SELF:Extension := SELF:_GetExtension()
            isOk := SUPER:CreateMemFile(info)
            IF isOk

                // Per default, Header Block Size if 512
                LOCAL memoHeader AS BYTE[]
                LOCAL nextBlock AS LONG
                //
                memoHeader := BYTE[]{ DBTMemo.HeaderSize }
                nextBlock := 1
                Array.Copy(BitConverter.GetBytes(nextBlock),0, memoHeader, 0, SIZEOF(LONG))
                //
                _oStream:SafeWrite(memoHeader)
                //
                SELF:_initContext()
            ELSE
                SELF:Error( FException(), ERDD.CREATE_MEMO, XSharp.Gencode.EG_CREATE ,"DBTMemo.CreateMemFile")
            ENDIF
            //
            RETURN isOk

            /// <inheritdoc />
        OVERRIDE METHOD OpenMemFile(info AS DbOpenInfo ) AS LOGIC
            LOCAL isOk AS LOGIC
            SELF:Extension  := SELF:_GetExtension()
            isOk := SUPER:OpenMemFile(info)
            IF isOk
                SELF:_initContext()
            ELSE
                SELF:Error( FException(), ERDD.OPEN_MEMO, XSharp.Gencode.EG_OPEN ,"DBTMemo.OpenMemFile")
                isOk := FALSE
            ENDIF
            //
            RETURN isOk

        PROPERTY NextAvailableBlock 	 AS LONG
            GET
               LOCAL nBlock := 0 AS LONG
               IF SELF:IsOpen
                    LOCAL _memoBlock AS BYTE[]
                    // NextBlock is at the beginning of MemoFile
                    _memoBlock := BYTE[]{4}
                    VAR savedPos := _oStream:Position
                    _oStream:SafeSetPos(0)

                    IF  _oStream:SafeRead(_memoBlock )
                        nBlock := BitConverter.ToInt32( _memoBlock, 0)
                    ENDIF
                    _oStream:SafeSetPos(savedPos)
                ENDIF
                RETURN nBlock
            END GET

            SET
                 IF SELF:IsOpen
                    LOCAL _memoBlock AS BYTE[]
                    _memoBlock := BYTE[]{4}
                    Array.Copy(BitConverter.GetBytes(value),0, _memoBlock, 0, SIZEOF(LONG))
                    VAR savedPos := _oStream:Position
                    _oStream:SafeSetPos(0)
                    _oStream:SafeWrite(_memoBlock )
                    _oStream:SafeSetPos(savedPos)
                ENDIF
            END SET

        END PROPERTY

        // Place a lock : <nOffset> indicate where the lock should be; <nLong> indicate the number bytes to lock
        // If it fails, the operation is tried <nTries> times, waiting 1ms between each operation.
        // Return the result of the operation
        PROTECTED METHOD _tryLock( nOffset AS INT64, nLong AS LONG, nTries AS LONG  ) AS LOGIC
            LOCAL locked AS LOGIC
            IF ! SELF:IsOpen
                RETURN FALSE
            ENDIF

            REPEAT
                locked := _oStream:SafeLock(nOffset, nLong)
                IF ! locked
                    SELF:Error(FException(), Subcodes.ERDD_INIT_LOCK, Gencode.EG_LOCK_ERROR, "DBTMemo._tryLock")
                ENDIF
                IF ( !locked )
                    nTries --
                    IF ( nTries > 0 )
                        System.Threading.Thread.Sleep( 1 )
                    ENDIF
                ENDIF
            UNTIL ( locked .OR. (nTries==0) )
            //
            RETURN locked

        PROTECTED METHOD _unlock( nOffset AS INT64, nLong AS LONG ) AS LOGIC
            LOCAL unlocked AS LOGIC
            //
            IF ! SELF:IsOpen
                RETURN FALSE
            ENDIF
            unlocked := _oStream:SafeUnlock( nOffset, nLong )
            IF ! unlocked
                SELF:Error(FException(), Subcodes.ERDD_UNLOCKED, Gencode.EG_UNLOCKED, "DBFDBT._unlock")
            ENDIF
            RETURN unlocked

        OVERRIDE METHOD Zap() AS LOGIC
            IF SELF:IsOpen
                IF SELF:Shared
                    SELF:Error(FException(), Subcodes.ERDD_SHARED, Gencode.EG_LOCK, "DBTMemo.Zap")
                ENDIF
                IF ! _oStream:SafeSetLength(DBTMemo.HeaderSize)
                    SELF:Error(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "DBTMemo.Zap")
                ENDIF
                SELF:NextAvailableBlock := 1
                RETURN SELF:Flush()
            ELSE
                SELF:Error(FException(), Subcodes.EDB_NOTABLE, Gencode.EG_NOTABLE, "DBTMemo.Zap")
                RETURN FALSE
            ENDIF


      END CLASS

END NAMESPACE

