//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/*
LEAF Page
  From the FoxPro Docs, where this is called "Compact Index Exterior Node Record"
  Byte offset   Description
  ===============================
  00 - 01       Node attributes (any of the following numeric values or their sums):
                0 - index node
                1 - root node
                2 - leaf node
                4 - created in batch process
  02 - 03       Number of keys present (0, 1 or many)
  04 - 07       Pointer to the node directly to the left of current node (on same level; -1 if not present)
  08 - 11       Pointer to the node directly to right of the current node (on same level; -1 if not present)
  12 - 13       Available free space in node
  14 - 17       Record number mask
  18            Duplicate byte count mask
  19            Trailing byte count mask
  20            Number of bits used for record number
  21            Number of bits used for duplicate count
  22            Number of bits used for trail count
  23            Number of bytes holding record number, duplicate count and trailing count
  24 - 511      Index keys and information
                Each entry consists of the record number, duplicate byte count and trailing byte count, all compacted.
                The key text is placed at the logical end of the node, working backwards, allowing for previous key entries.

- The List of CDX tags is a special leaf page with type 3 (Root + Leaf)
- It starts with a fixed block of 24 bytes:
  The remaining 488 bytes are used by the list of key descriptors (which starts immediately after this header) and the keys
  (which are stored from the end of the page forward)
  Each key descriptor consists of 3 numbers
  1) The record number
  2) The number of Duplicate bytes (so for 2 keys following eachother with the same start bytes only the different bytes for the )
     2nd one are stored
  3) The number of Trailing bytes (trailing spaces are trimmed)
  The header describes how many bits each of these elements occupies and how many bytes the 3 together
  (often 4 bytes, but for larger CDX files more bytes may be needed for the recno, so more bytes will be used for the key descriptors)
  The header also contains "masks" that help to mask out the record number, duplicate and trailing bytes counts
  This is stored in the file as dddddtttttrrrrrrrrrr  where ddd are the duplicate bits, tttt the trail bits and rrrr the recno bits

  Key data is stored as:
  Character: ASCII
  Date     : Julian date converted to number
  Number   : IEEE double (8 bytes), swapped the order of bytes. When negative invert all bits, otherwise only highest order bit.
             This is complicated but allows to use memcmp to compare numeric values
  Character fields are assumed to have trailing ' ', numeric field have trailing '\0'



*/

#include "CdxDebug.xh"

USING System.Runtime.CompilerServices
USING System.Runtime.InteropServices
USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Diagnostics
USING STATIC XSharp.Conversions

BEGIN NAMESPACE XSharp.RDD.CDX
    [DebuggerDisplay("{DebugString,nq}")];
    INTERNAL SEALED CLASS CdxLeaf
        INTERNAL Recno  AS LONG
        INTERNAL Key    AS BYTE[]
        INTERNAL Trail  AS BYTE
        INTERNAL Dup    AS BYTE
        INTERNAL KeyBinary AS LOGIC
        INTERNAL PROPERTY KeyText AS STRING GET SELF:Key:ToAscii(KeyBinary)
        INTERNAL PROPERTY DebugString AS STRING => SELF:KeyText:Trim() +" # "+SELF:Recno:ToString()
        CONSTRUCTOR (nRecno AS LONG, bKey AS BYTE[], nDup AS BYTE, nTrail AS BYTE, @@binary AS LOGIC)
            SELF:Recno := nRecno
            SELF:Key   := (BYTE[]) bKey:Clone()
            SELF:Dup   := nDup
            SELF:Trail := nTrail
            SELF:KeyBinary := @@binary
            RETURN
         OVERRIDE METHOD ToString() AS STRING => DebugString
    END CLASS
	/// <summary>
	/// CdxLeaf page. this class maps the Leaf page from the file in memory
    /// Manipulating the page is implemented in the CdxTag class
	/// </summary>
	INTERNAL CLASS CdxLeafPage INHERIT CdxTreePage
#region Fields
        // Private fields that map the various fixed values in a page
        // The values are cached here so they won't have to be decoded from the page everytime they are used
        PRIVATE _freeSpace      AS WORD
        PRIVATE _recnoMask      AS LONG
        PRIVATE _duplicateMask  AS BYTE
        PRIVATE _trailingMask   AS BYTE
        PRIVATE _recordBits     AS BYTE
        PRIVATE _dupBits        AS BYTE
        PRIVATE _trailBits      AS BYTE
        PRIVATE _dataBytes      AS BYTE

        // Other fields
        PRIVATE _leaves    AS List<CdxLeaf>
        INTERNAL aClear     as BYTE[]

#endregion
#region constants
		PRIVATE CONST CDXLEAF_OFFSET_FREESPACE	    := 12	AS WORD // 2 WORD		: Free space in this key
		PRIVATE CONST CDXLEAF_OFFSET_RECNOMASK	    := 14	AS WORD // 4 LONGINT	: Bit mask for record number
		PRIVATE CONST CDXLEAF_OFFSET_DUPMASK		:= 18	AS WORD // 1 Bit mask for duplicate byte count
		PRIVATE CONST CDXLEAF_OFFSET_TRAILMASK      := 19	AS WORD // 1 Bit mask for trailing byte count
		PRIVATE CONST CDXLEAF_OFFSET_RECNUMBITS	    := 20	AS WORD // 1 Number of bits used for record number
		PRIVATE CONST CDXLEAF_OFFSET_DUPCOUNTBITS   := 21	AS WORD // 1 Number of bits used for duplicate count
		PRIVATE CONST CDXLEAF_OFFSET_TRAILINGBITS   := 22	AS WORD // 1 Number of bits used for trailing count
		PRIVATE CONST CDXLEAF_OFFSET_DATABYTES	    := 23	AS WORD // 1 Bytes needed for recno+dups+trailing (sum of 20,21 & 22)
        PRIVATE CONST CDXLEAF_HEADERLEN             := 24   AS WORD // length of the page header
        PRIVATE CONST CDXLEAF_BYTESFREE             := 488  AS WORD // # of bytes 512 - 24
#endregion

        INTERNAL PROPERTY Binary AS LOGIC
            GET
                IF SELF:Tag != NULL
                    RETURN SELF:Tag:Binary
                ENDIF
                RETURN FALSE
            END GET
        END PROPERTY


        INTERNAL CONSTRUCTOR( bag AS CdxOrderBag, page AS CdxPage)
            SELF(bag, page:PageNo, page:Buffer, (WORD) IIF(page:Tag != NULL, page:Tag:KeyLength,0))

	    INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , nPage AS Int32 , buffer AS BYTE[], nKeyLen AS WORD)
            SUPER(bag, nPage, buffer)
            KeyLength   := nKeyLen
            _leaves     := NULL
            TrailByte   := 0
            SELF:_getValues()
            SELF:InitTrailKey()

            RETURN

        PROTECTED OVERRIDE METHOD _clear() AS VOID
            SUPER:_clear()
            SELF:_freeSpace      := 0
            SELF:_recnoMask      := 0
            SELF:_duplicateMask  := 0
            SELF:_trailingMask   := 0
            SELF:_recordBits     := 0
            SELF:_dupBits        := 0
            SELF:_trailBits      := 0
            SELF:_dataBytes      := 0

        INTERNAL OVERRIDE METHOD Clear() AS VOID
            SUPER:Clear()
            SELF:_clear()

        INTERNAL METHOD InitTrailKey() AS VOID
            SELF:aClear := BYTE[]{SELF:KeyLength}
            IF TrailByte != 0
                FOR VAR nI := 0 to KeyLength-1
                    aClear[nI] := TrailByte
                NEXT
            ENDIF

        INTERNAL OVERRIDE METHOD InitBlank(oTag AS CdxTag) AS VOID
            SELF:Tag    := oTag
            SELF:Initialize(KeyLength)
            IF SELF IS CdxTagList
                TrailByte := 0
            ELSEIF Tag != NULL
                IF Tag:Collation != NULL
                    TrailByte := 0
                ELSE
                    TrailByte := (BYTE) (IIF(Tag:KeyType == __UsualType.String, 32, 0) )
                ENDIF
            ELSE
                TrailByte := 32
            ENDIF
            SELF:InitTrailKey()

        INTERNAL OVERRIDE METHOD _setTag(newTag AS CdxTag) AS VOID
            SUPER:_setTag(newTag)
            IF SELF IS CdxTagList
                TrailByte := 0
            ELSEIF newTag != NULL
                IF _tag:Collation != NULL
                    TrailByte := 0
                ELSE
                    TrailByte := (BYTE) (IIF(_tag:KeyType == __UsualType.String, 32, 0) )
                ENDIF
            ENDIF
            SELF:InitTrailKey()

        INTERNAL VIRTUAL METHOD Initialize(nKeyLength AS WORD) AS VOID
            VAR wasRoot := SELF:IsRoot
            SELF:PageType       := CdxPageType.Leaf
            IF wasRoot
                SELF:SetRoot()
            ENDIF

            IF SELF:Tag != NULL
                SELF:Tag:SetLeafProperties(SELF)
            ELSE
                SELF:SetProperties()
            ENDIF
            SELF:LenShift      := (nKeyLength << 8 ) | (8 - SELF:DuplicateBits)
            SELF:InitTrailKey()
            RETURN

        PRIVATE METHOD SetProperties() AS VOID
            SELF:ClearRecordsAndKeys()
            VAR bits            := CdxHelpers.GetBits(SELF:KeyLength)
            // base dupCountMask, trailCountMNask, numbitsRecno and other info are based on keylength
            SELF:DataBytes      := (BYTE) IIF (bits > 12, 5, IIF( bits > 8, 4, IIF(bits > 1, 3,2)))
            SELF:RecordBits     := (BYTE) ((SELF:DataBytes << 3) - (bits << 1))
            SELF:DuplicateBits  := SELF:TrailingBits  := bits
            SELF:TrailingMask   := SELF:DuplicateMask := (BYTE) (( 1 << bits  ) - 1)
            SELF:RecnoMask      := (1 << SELF:RecordBits) -1
            RETURN

        INTERNAL METHOD ClearRecordsAndKeys() AS VOID
            SELF:Freespace  := CDXLEAF_BYTESFREE
            SELF:NumKeys    := 0
            SELF:_leaves    := NULL
            MemSet(SELF:Buffer,CDXLEAF_HEADERLEN,CDXLEAF_BYTESFREE,0)
            RETURN

        INTERNAL OVERRIDE METHOD Read() AS LOGIC
			VAR Ok := SUPER:Read()
            System.Diagnostics.Debug.Assert (SELF:PageType:HasFlag(CdxPageType.Leaf))
            IF Ok
                SELF:_getValues()
            ENDIF
            RETURN Ok

        INTERNAL PROPERTY ValidKeys AS LOGIC GET _leaves != NULL .AND. _leaves:Count == SELF:NumKeys
#region ICdxKeyValue

        INTERNAL OVERRIDE METHOD GetRecno(nPos AS Int32) AS Int32
            IF SELF:ValidKeys
                RETURN _leaves[nPos]:Recno
            ENDIF
            LOCAL nOffSet   AS Int32
            LOCAL nRecno    AS Int32
            System.Diagnostics.Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nOffSet     := CDXLEAF_HEADERLEN + nPos * SELF:DataBytes
            IF SELF:RecordBits <= 16
                nRecno      := SELF:_GetWord(nOffSet)
            ELSE
                nRecno      := SELF:_GetLong(nOffSet)
            ENDIF
            nRecno      := _AND( nRecno , SELF:RecnoMask)
            RETURN nRecno

        INTERNAL OVERRIDE METHOD GetChildPage(nPos AS Int32) AS Int32
            RETURN 0

        INTERNAL OVERRIDE METHOD GetChildren as IList<LONG>
            RETURN List<LONG>{}

        INTERNAL OVERRIDE METHOD GetKey(nPos AS Int32) AS BYTE[]
            System.Diagnostics.Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            IF nPos >= 0 .AND. nPos < SELF:NumKeys
                SELF:_ExpandKeys(FALSE)
                RETURN _leaves[nPos]:Key
            ENDIF
            RETURN NULL
#endregion

         // For Debugging we calculate all Recnos and KeyBytes
         // Later we will remove this
         PRIVATE METHOD _ExpandKeys(lForce := FALSE AS LOGIC)  AS LOGIC
            IF ! lForce .AND. ValidKeys
                RETURN TRUE
            ENDIF
            LOCAL nOffSet   AS Int32
            LOCAL aBytes := BYTE[]{KeyLength} AS BYTE[]
            LOCAL nRecno    AS Int32
            LOCAL nDup, nTrail AS BYTE
            LOCAL nCopy     AS Int32
            LOCAL nStart    AS Int32
            LOCAL nStep     AS Int32
            LOCAL nLast     AS Int32

            // First key starts at end of page
            nStart := CDXPAGE_SIZE
            _leaves := List<CdxLeaf>{}
            IF SELF IS CdxTagList
                SELF:aClear := BYTE[]{KeyLength}
            ENDIF
            nOffSet := CDXLEAF_HEADERLEN
            nStep := SELF:DataBytes
            IF SELF:NumKeys > 0
                nLast := SELF:NumKeys-1
                FOR VAR nI := 0 TO nLast
                    LOCAL iTemp AS WORD
                    IF SELF:RecordBits <= 16
                        nRecno  := SELF:_GetWord(nOffSet)
                    ELSE
                        nRecno  := SELF:_GetLong(nOffSet)
                    ENDIF
                    nRecno  := _AND( nRecno , SELF:RecnoMask)
                    iTemp   := SELF:_GetWord(nOffSet + SELF:DataBytes - 2)
                    SELF:_getDupTrail(iTemp, OUT nDup, OUT nTrail)
                    nCopy    := SELF:KeyLength - nTrail - nDup
                    IF nTrail > 0 .and. nTrail <= SELF:KeyLength
                       System.Array.Copy(aClear, 0,  aBytes, SELF:KeyLength - nTrail, nTrail)
                    ENDIF
                    // Copy to aBytes from pos nDup
                    IF nCopy > 0 .and. nCopy <= nStart
                        System.Array.Copy(_buffer, nStart - nCopy, aBytes, nDup, nCopy)
                        nStart := nStart - nCopy
                    ENDIF
                    var leaf := CdxLeaf{ nRecno, aBytes,nDup, nTrail,SELF:Binary}
                    _leaves.Add( leaf)
                    nOffSet      += nStep
                NEXT
            ENDIF
            RETURN TRUE

        INTERNAL OVERRIDE METHOD Write() AS LOGIC
#ifdef TESTCDX
            IF ! SELF:ValidateKeys()
               SELF:_tag:ThrowException(Subcodes.ERDD_WRITE_NTX,Gencode.EG_CORRUPTION,  "CdxLeafPage.Write")
            ENDIF
#endif
            SELF:Compress()
            RETURN SUPER:Write()


        PRIVATE METHOD _getValues AS VOID
            _freeSpace      := SELF:_GetWord(CDXLEAF_OFFSET_FREESPACE)
            _recnoMask      := SELF:_GetLong(CDXLEAF_OFFSET_RECNOMASK)
            _duplicateMask  := _buffer[CDXLEAF_OFFSET_DUPMASK]
            _trailingMask   := _buffer[CDXLEAF_OFFSET_TRAILMASK]
            _recordBits     := _buffer[CDXLEAF_OFFSET_RECNUMBITS]
            _dupBits        := _buffer[CDXLEAF_OFFSET_DUPCOUNTBITS]
            _trailBits      := _buffer[CDXLEAF_OFFSET_TRAILINGBITS]
            _dataBytes      := _buffer[CDXLEAF_OFFSET_DATABYTES]
            LenShift       := (SELF:KeyLength << 8 ) | (8 - SELF:DuplicateBits)


#region Properties
        // We read the values from our cache but write back to the cache and the buffer at the same time
        // The _Set.. methods set the IsHot flag of the page automatically

		INTERNAL PROPERTY Freespace AS WORD GET _freeSpace ;
			SET SELF:_SetWord(CDXLEAF_OFFSET_FREESPACE, value),  _freeSpace := value

		INTERNAL PROPERTY RecnoMask AS LONG GET _recnoMask;
			SET SELF:_SetLong(CDXLEAF_OFFSET_RECNOMASK, value),  _recnoMask := value

		INTERNAL PROPERTY DuplicateMask	AS BYTE	GET _duplicateMask;
			SET _buffer[CDXLEAF_OFFSET_DUPMASK]      := _duplicateMask := value, _hot := TRUE

		INTERNAL PROPERTY TrailingMask AS BYTE	GET _trailingMask ;
			SET _buffer[CDXLEAF_OFFSET_TRAILMASK]    := _trailingMask := value, _hot := TRUE

		INTERNAL PROPERTY RecordBits AS BYTE GET _recordBits;
			SET _buffer[CDXLEAF_OFFSET_RECNUMBITS]   := _recordBits := value, _hot := TRUE

		INTERNAL PROPERTY DuplicateBits AS BYTE	GET _dupBits;
			SET _buffer[CDXLEAF_OFFSET_DUPCOUNTBITS] := _dupBits := value, _hot := TRUE

		INTERNAL PROPERTY TrailingBits  AS BYTE	GET _trailBits;
			SET _buffer[CDXLEAF_OFFSET_TRAILINGBITS] := _trailBits := value, _hot := TRUE

		INTERNAL PROPERTY DataBytes	AS BYTE	GET _dataBytes;
			SET _buffer[CDXLEAF_OFFSET_DATABYTES] := _dataBytes := value, _hot := TRUE

        INTERNAL PROPERTY TrailByte    AS BYTE AUTO
        INTERNAL PROPERTY KeyLength    AS WORD AUTO
        INTERNAL PROPERTY LenShift     AS INT AUTO


#endregion

#region Other properties

        // Retrieve an index node in the current Page, at the specified position
        // return CdxLeagPageNode which always returns 0 for the ChildPage
        INTERNAL OVERRIDE PROPERTY SELF[ index AS WORD ] AS CdxPageNode
            GET
                RETURN CdxLeafPageNode{ SELF:KeyLength, SELF, index }
            END GET
        END PROPERTY

        INTERNAL OVERRIDE PROPERTY LastNode AS CdxPageNode GET IIF(SELF:NumKeys == 0, NULL, SELF[(WORD) (SELF:NumKeys-1)])
        INTERNAL PROPERTY Keys    AS IList<CdxLeaf>
            GET
                SELF:_ExpandKeys(FALSE)
                RETURN SELF:_leaves
            END GET
        END PROPERTY
#endregion
        INTERNAL METHOD SetRecordBits(numRecs AS LONG) AS VOID
            VAR bits    := CdxHelpers.GetBits(SELF:KeyLength)
            VAR totbits := bits * 2


            DO CASE
            CASE numRecs < 2^(16 - totbits) -1
                SELF:RecordBits     := (BYTE) (16-totbits)
            CASE numRecs < 2^(24 - totbits) -1
                SELF:RecordBits     := (BYTE) (24-totbits)
            CASE numRecs < 2^(32 - totbits) -1
                SELF:RecordBits     := (BYTE) (32-totbits)
            OTHERWISE
                SELF:RecordBits     := 32
            ENDCASE
            IF SELF:RecordBits < 8
                SELF:RecordBits := 8
            ENDIF
            var totalBits       := SELF:RecordBits + bits + bits
            DO CASE
            CASE totalBits    <= 16
                SELF:DataBytes := 2
            CASE totalBits    <= 24
                SELF:DataBytes := 3
            CASE totalBits    <= 32
                SELF:DataBytes := 4
            CASE totalBits    <= 40
                SELF:DataBytes := 5
            OTHERWISE
                SELF:DataBytes := 6
            ENDCASE

            SELF:DuplicateBits  := bits
            SELF:TrailingBits   := bits
            SELF:TrailingMask   := (BYTE) (( 1 << bits  ) - 1)
            SELF:DuplicateMask  := (BYTE) (( 1 << bits  ) - 1)
            IF SELF:RecordBits == 32
                SELF:RecnoMask      := -1   // all bit set
            ELSE
                SELF:RecnoMask      := (1 << SELF:RecordBits) -1
            ENDIF

        INTERNAL METHOD GetKeys() AS IList<CdxLeaf>
            SELF:_ExpandKeys(TRUE)
            RETURN SELF:_leaves



        // This method assumes keys are added in the right order.
        INTERNAL METHOD Add(recno AS LONG, key AS BYTE[]) AS CdxAction
            LOCAL nTrailCount AS BYTE
            LOCAL nDupCount   AS BYTE
            //System.Diagnostics.Trace.WriteLine(i"CdxLeafPage:Add({recno})")
            IF _AND( recno, SELF:RecnoMask) != recno
                //DUMP("rec",recno:ToString("X"), "keys", SELF:NumKeys, "free before", SELF:Freespace)
               // DUMP( "triggers ExpandRecnos", "Rec", recno)
                SELF:Write()
                RETURN CdxAction.ExpandRecnos(SELF, recno, key, -1)
            ENDIF
            SELF:_ExpandKeys(FALSE)
            nTrailCount := SELF:_getTrailCount(key)
            IF SELF:NumKeys == 0
                nDupCount := 0
            ELSE
                VAR prevkey := _leaves[SELF:NumKeys-1]:Key
                nDupCount   := SELF:_getDupCount(prevkey, key, nTrailCount)
            ENDIF
            LOCAL nBytesNeeded := SELF:KeyLength - nDupCount - nTrailCount  + SELF:DataBytes  AS WORD
            IF SELF:Freespace < nBytesNeeded
                //DUMP("rec",recno:ToString("X"), "keys", SELF:NumKeys, "free before", SELF:Freespace)
                //DUMP( "triggers SplitLeaf", "Rec", recno)
                SELF:Write()
                RETURN CdxAction.AddLeaf(SELF, recno, key)
            ENDIF
            VAR leaf := CdxLeaf{recno, key,nDupCount, nTrailCount,SELF:Binary}

            _leaves:Add( leaf)
#ifdef TESTCDX
            IF _leaves:Count > 1
                ValidateKeys(_leaves[_leaves:Count-2], leaf)
            ENDIF
#endif
            SELF:NumKeys += 1
            SELF:Freespace -= nBytesNeeded
            //Debug("rec",recno, "keys", SELF:NumKeys, "free after", SELF:Freespace)
            //SELF:Write()
            RETURN CdxAction.Ok


        INTERNAL METHOD SetKeys(list AS IList<CdxLeaf>, nStart AS LONG, nCount AS LONG) AS CdxAction
            LOCAL action AS CdxAction
            DUMP(nStart, nCount, "keys", SELF:NumKeys, "free", SELF:Freespace)
            SELF:ClearRecordsAndKeys()
            FOR VAR nKey := nStart TO nStart+nCount-1
                LOCAL key := list[nKey] AS CdxLeaf
                action := SELF:Add(key:Recno, key:Key)
            NEXT
            action := SELF:Compress()
            SELF:Write()
            RETURN action

        INTERNAL METHOD Insert(nPos AS LONG, recno AS LONG, key AS BYTE[]) AS CdxAction

            // A quick calculation if we have enough room, ignoring the duplicate count
            LOCAL nTrailCount AS BYTE
            LOCAL nBytesNeeded AS WORD
            LOCAL last := FALSE AS LOGIC
            LOCAL adjustNext := FALSE AS LOGIC
            LOCAL leaf AS CdxLeaf
            IF _AND( recno, SELF:RecnoMask) != recno
                DUMP("Pos",nPos, "Rec", recno, "keys", SELF:NumKeys, "free before", SELF:Freespace)
                DUMP(  "triggers ExpandRecnos", SELF:PageNoX, "Rec", recno)
                RETURN CdxAction.ExpandRecnos(SELF,recno, key, nPos)
            ENDIF
            nTrailCount := SELF:_getTrailCount(key)
            // we calculate if a key fits without looking at the Duplicate bytes to make things easier
            // it might fit when the # of duplicate bytes is large, but we simply don't allow it
            nBytesNeeded := SELF:DataBytes + SELF:KeyLength - nTrailCount
            IF SELF:Freespace < nBytesNeeded
                DUMP("Pos",nPos, "Rec", recno, "keys", SELF:NumKeys, "free before", SELF:Freespace)
                DUMP( "triggers SplitLeaf",  "Rec", recno)
                RETURN CdxAction.SplitLeaf(SELF, recno,key, nPos)
            ENDIF
            // Todo: optimize. We are now expanding the leaves which could be overkill.
            SELF:_ExpandKeys(FALSE)
            // compare position in page
            LOCAL nDupCount AS BYTE
            IF _leaves:Count == 0
                leaf := CdxLeaf{recno, key, 0, nTrailCount,SELF:Binary}
                _leaves:Add(leaf)
                SELF:Freespace -= nBytesNeeded
                last := TRUE
            ELSEIF nPos < 0 .OR. nPos >= _leaves:Count
                VAR prevLeaf := _leaves[_leaves:Count-1]
                VAR prevkey := prevLeaf:Key
                nDupCount := SELF:_getDupCount(prevkey, key,  nTrailCount)
                leaf := CdxLeaf{recno, key,  nDupCount, nTrailCount,SELF:Binary}
                _leaves:Add(leaf)
#ifdef TESTCDX
                ValidateKeys(prevLeaf, leaf)
#endif
                SELF:Freespace := SELF:Freespace - (nBytesNeeded-nDupCount)
                last := TRUE
            ELSEIF nPos == 0
                nDupCount       := 0
                SELF:Freespace  := SELF:Freespace - nBytesNeeded
                leaf := CdxLeaf{recno, key,nDupCount,nTrailCount,SELF:Binary}
                _leaves:Insert(nPos, leaf)
                adjustNext      := TRUE
            ELSE // nPos > 0 .AND. nPos < _leaves:Count
                VAR prevLeaf    := _leaves[nPos-1]
                nDupCount       := SELF:_getDupCount(prevLeaf:Key, key,  nTrailCount)
                SELF:Freespace  := SELF:Freespace- (nBytesNeeded-nDupCount)
                leaf := CdxLeaf{recno, key, nDupCount,nTrailCount,SELF:Binary}

                _leaves:Insert(nPos, leaf)
#ifdef TESTCDX
                ValidateKeys(prevLeaf, leaf)
#endif
                adjustNext      := TRUE
            ENDIF
            IF adjustNext
                VAR nextLeaf := _leaves[nPos+1]
                nDupCount    := SELF:_getDupCount(key, nextLeaf:Key,  nextLeaf:Trail)
                IF nDupCount != nextLeaf:Dup
                    DUMP("Adjusted next leaf dup from ", nextLeaf:Dup, "to", nDupCount)
                    VAR diff        := nDupCount - nextLeaf:Dup
                    SELF:Freespace  += (WORD) diff
                    nextLeaf:Dup    := nDupCount
                ENDIF
#ifdef TESTCDX
                ValidateKeys(leaf, nextLeaf)
#endif
            ENDIF
            SELF:NumKeys := (WORD) _leaves:Count

            VAR result := CdxAction.Ok
            IF last .AND. ! SELF:IsRoot
                DUMP("Pos",nPos, "Rec", recno, "keys", SELF:NumKeys, "free after", SELF:Freespace)
                DUMP( "triggers ChangeParent")
                result := CdxAction.ChangeParent(SELF)
            ENDIF
            VAR compResult := SELF:Compress()
            IF compResult:Type != CdxActionType.Ok
                RETURN compResult
            ENDIF
            SELF:Write()
            RETURN result

#ifdef TESTCDX
        METHOD ValidateSiblings AS LOGIC
            LOCAL lOk := TRUE AS LOGIC
            IF SELF:NumKeys > 0
                IF SELF:HasLeft
                    VAR leftPage  := (CdxLeafPage) SELF:_tag:GetPage(SELF:LeftPtr)
                    VAR leftNode  := leftPage:Keys[leftPage:NumKeys -1]
                    VAR rightNode  := SELF:Keys[0]
                    IF ! ValidateKeys(leftNode, rightNode)
                         SELF:Debug("Corruption detected: Last key on left page",leftPage:PageNoX," is not < our first key", leftNode:DebugString,  rightNode:DebugString)
                         lOk := FALSE
                    ENDIF
                ENDIF
                IF SELF:HasRight
                    VAR leftNode  := SELF:Keys[SELF:NumKeys-1]
                    VAR rightPage := (CdxLeafPage) SELF:_tag:GetPage(SELF:RightPtr)
                    IF rightPage:NumKeys > 0
                        VAR rightNode := rightPage:Keys[0]
                        IF ! ValidateKeys(leftNode, rightNode)
                           SELF:Debug("Corruption detected: Last key on our page is not < first key on right page", rightPage:PageNoX, leftNode:DebugString, rightNode:DebugString)
                           lOk := FALSE
                        ENDIF
//                    ELSE
//                       System.Diagnostics.Debugger.Break()
                    ENDIF
                ENDIF
            ENDIF
            RETURN lOk

        METHOD ValidateLevel() AS LOGIC
            LOCAL lOk := TRUE AS LOGIC
            VAR page := (CdxLeafPage) SELF:FirstPageOnLevel
            DO WHILE page != NULL
                page:ValidateKeys()
                IF page:HasRight
                    VAR leftNode  := page:Keys[page:NumKeys-1]
                    VAR rightPage := (CdxLeafPage) SELF:_tag:GetPage(page:RightPtr)
                    VAR rightNode := rightPage:Keys[0]
                    ValidateKeys(leftNode, rightNode)
                    VAR nDiff := SELF:Tag:__Compare(leftNode:Key, rightNode:Key, leftNode:Key:Length, leftNode:Recno, rightNode:Recno)
                    IF nDiff > 0
                        SELF:Debug(page:PageNoX, "Keys in wrong order", leftNode:DebugString, rightNode:DebugString)
                        lOk := FALSE
                        SELF:DumpKeys()
                        rightPage:DumpKeys()
                    ENDIF
                    page := rightPage
                ELSE
                    page := NULL
                ENDIF
            ENDDO
            RETURN lOk

        INTERNAL METHOD ValidateKeys(oLeft AS CdxLeaf, oRight AS CdxLeaf) AS LOGIC
//            LOCAL nDiff AS LONG
//            IF SELF:Tag == NULL
//                RETURN TRUE
//            ENDIF
//            nDiff := SELF:Tag:__Compare(oLeft:Key, oRight:Key, oLeft:Key:Length, oLeft:Recno, oRight:Recno)
//            IF nDiff == -1
//                RETURN TRUE
//            ELSEIF nDiff == 0 .AND. oLeft:Recno < oRight:Recno
//                RETURN TRUE
//            ENDIF
//            RETURN FALSE
            RETURN TRUE

        METHOD ValidateKeys() AS LOGIC
            LOCAL lOk := TRUE AS LOGIC
            FOR VAR i := 0  TO SELF:Keys:Count-2
                VAR leaf1 := SELF:Keys[i]
                VAR leaf2 := SELF:Keys[i+1]
                IF ! ValidateKeys(leaf1, leaf2)
                    SELF:Debug("Corruption detected: incorrect order of Keys L:", leaf1:DebugString, "R", leaf2:DebugString)
                    lOk := FALSE
                ENDIF
            NEXT
            RETURN lOk

        METHOD Validate() AS VOID
            SELF:_ExpandKeys(FALSE)
            IF SELF:NumKeys > 0
                ValidateSiblings()
            ENDIF
#endif
        INTERNAL METHOD Split(oPageR AS CdxLeafPage, action AS CdxAction) AS CdxAction
            DUMP("New", oPageR:PageNoX)
            VAR list      := _leaves
            IF action:Recno > 0
                list := (List<CdxLeaf>) SELF:GetKeys()
                VAR nTrailCount := SELF:_getTrailCount(action:Key)
                LOCAL nDupCount AS BYTE
                VAR nPos := SELF:FindKey(action:Key,action:Recno,action:Key:Length)
                LOCAL leaf AS CdxLeaf
                IF nPos == 0
                    nDupCount := 0
                    leaf := CdxLeaf{action:Recno, action:Key, 0, nTrailCount,SELF:Binary}
                    list:Insert(0, leaf)
#ifdef TESTCDX
                    IF list:Count > 1
                        ValidateKeys(leaf, list[1])
                    ENDIF
#endif
                ELSEIF nPos > 0 .and. nPos < list:Count
                    VAR nextLeaf := list[nPos]
                    VAR nextKey := nextLeaf:Key
                    nDupCount := SELF:_getDupCount(nextKey, action:Key, nTrailCount)
                    leaf := CdxLeaf{action:Recno, action:Key, nDupCount, nTrailCount,SELF:Binary}
                    list:Insert(nPos, leaf)
#ifdef TESTCDX
                    ValidateKeys(leaf, nextLeaf)
                    IF nPos > 0
                        ValidateKeys( list[nPos-1], leaf)
                    ENDIF
#endif
                ELSE
                    // append at the end
                    VAR prevLeaf := list[list:Count-1]
                    VAR prevKey := prevLeaf:Key
                    nDupCount := SELF:_getDupCount(prevKey, action:Key, nTrailCount)
                    leaf := CdxLeaf{action:Recno, action:Key, nDupCount, nTrailCount,SELF:Binary}
                    list:Add(leaf)
#ifdef TESTCDX
                    ValidateKeys(prevLeaf, leaf)
#endif

                ENDIF

            ENDIF
            VAR half := list:Count /2
            //? "Writing to page 1", oPageR:PageNoX
            SELF:SetKeys(list, 0, half)
            oPageR:SetKeys(list, half, list:Count - half)
            //? "Writing to page 2", oPageR:PageNoX
            SELF:Write()
            oPageR:Write()
            SELF:Tag:AdjustStack(SELF, oPageR, oPageR:NumKeys)
#ifdef TESTCDX
            SELF:Write()
#endif
            RETURN CdxAction.Ok


        INTERNAL METHOD Delete(nPos AS LONG) AS CdxAction
             // Todo: optimize. We are now expanding the leaves which could be overkill.
            IF nPos >= 0 .AND. nPos < SELF:NumKeys
                SELF:_ExpandKeys(FALSE)
                VAR result := CdxAction.Ok
                IF ! SELF:IsRoot
                    IF nPos == SELF:NumKeys -1 .AND. SELF:NumKeys > 1
                        //SELF:Tag:SetChildToProcess(SELF:PageNo)
                        DUMP(  "updates last key, triggers ChangeParent")
                        result := CdxAction.ChangeParent(SELF)
                    ENDIF
                ENDIF
                // we must adjust the dupcount for the element AFTER the element that will be deleted
                if nPos > 0 .and. nPos < SELF:NumKeys-1
                    var nextLeaf := _leaves[nPos+1]
                    var prevLeaf := _leaves[nPos-1]
                    nextLeaf:Dup := SELF:_getDupCount(prevLeaf:Key, nextLeaf:Key,nextLeaf:Trail)
                ELSEIF nPos == 0 .AND. _leaves:Count > 1
                    _leaves[1]:Dup := 0
                ENDIF
                _leaves:RemoveAt(nPos)
                SELF:NumKeys -= 1
                IF SELF:NumKeys = 0 .AND. ! SELF:IsRoot
                    RETURN CdxAction.DeletePage(SELF)
                ENDIF
                LOCAL result2 :=  SELF:Compress() AS CdxAction
                IF result2.Type == CdxActionType.Ok
                    SELF:Write()
                    RETURN result
                ELSE
                    RETURN result2
                ENDIF
            ENDIF
            RETURN CdxAction.OutOfBounds(SELF)

        INTERNAL METHOD Replace(nPos AS LONG, node AS CdxNode) AS CdxAction
            // Todo: optimize. We are now expanding the leaves which could be overkill.
            IF nPos >= 0 .AND. nPos < SELF:NumKeys
                SELF:_ExpandKeys(FALSE)
                VAR nTrailCount := SELF:_getTrailCount(node:KeyBytes)
                LOCAL nDupCount AS BYTE
                IF nPos == 0
                    nDupCount := 0
                ELSE
                    nDupCount := SELF:_getDupCount(SELF:_leaves[nPos-1]:Key, node:KeyBytes,nTrailCount)
                ENDIF
                _leaves[nPos] := CdxLeaf{node:Recno, node:KeyBytes, nDupCount, nTrailCount,SELF:Binary}
                RETURN SELF:Compress()
            ENDIF
            RETURN CdxAction.OutOfBounds(SELF)

        INTERNAL METHOD Compress() AS CdxAction
            // Todo: optimize. We are now expanding and compressing the leaves which could be overkill.
            // Also the buffer is saved (because we may discover a 'page full' and we need to restore then
            // maybe we can restore from disk ?
            IF SELF:NumKeys == 0 .OR. ! SELF:ValidKeys
                RETURN CdxAction.Ok
            ENDIF
            #ifdef TESTCDX
            //Debug("keys", SELF:NumKeys, "free", SELF:Freespace)
            #endif
            VAR list  := SELF:_leaves
            SELF:ClearRecordsAndKeys()

            LOCAL nKey   := 0 AS INT
            LOCAL nStart := (WORD) (CDXLEAF_HEADERLEN + SELF:Freespace)  AS WORD
            FOREACH leaf AS CdxLeaf IN list
                VAR nDup   := leaf:Dup
                VAR nTrail := leaf:Trail
                IF nKey == 0
                    nDup := 0  // The first key cannot have any duplicates
		        ELSE
		            NOP // The dup and trail values are already stored in the leaves
                ENDIF
                LOCAL nBytesToCopy := SELF:KeyLength - nDup - nTrail AS WORD
                IF SELF:Freespace < (SELF:DataBytes + nBytesToCopy)
                    VAR action := CdxAction.SplitLeaf(SELF, -1, NULL, 0)
                    SELF:_leaves := list
                    RETURN SELF:Tag:DoAction(action)
                ENDIF
                SELF:_placeRecno(nKey, leaf:Recno, SELF:_makeDupTrail(nDup, nTrail))
                IF nBytesToCopy != 0
                    System.Array.Copy(leaf:Key, nDup, _buffer, nStart-nBytesToCopy,  nBytesToCopy)
                ENDIF
                SELF:Freespace := (WORD)  (SELF:Freespace -  (nBytesToCopy + SELF:DataBytes))
                nStart  -= nBytesToCopy
                nKey    += 1
            NEXT
            SELF:NumKeys := (WORD) list:Count
            SELF:_leaves := list
            RETURN CdxAction.Ok


       PRIVATE METHOD _getTrailCount(data AS BYTE[]) AS BYTE
           LOCAL iLastTrail AS LONG
           iLastTrail  := data:Length
           FOR VAR i := data:Length -1 DOWNTO 0
                IF data[i] != TrailByte
                    EXIT
                ENDIF
                iLastTrail := i
           NEXT
           RETURN (BYTE)  (data:Length - iLastTrail)

        PRIVATE METHOD _getDupCount(prevdata AS BYTE[], data AS BYTE[], trailCount AS LONG) AS BYTE
           BEGIN UNCHECKED
               LOCAL last := data:Length - trailCount  AS LONG
               LOCAL dup AS LONG
               dup := 0
               DO WHILE dup < last
                   if data[dup] != prevdata[dup]
                       EXIT
                   ENDIF
                   dup++
               ENDDO
               RETURN (BYTE) dup
           END UNCHECKED
        #pragma warnings (170, off)
       PRIVATE METHOD _makeDupTrail(dupCount AS BYTE, trailCount AS BYTE) AS WORD
            BEGIN UNCHECKED
                LOCAL shift AS INT
                LOCAL wValue AS __WordStruct
                shift     := SELF:LenShift & 0xFF
                wValue:b1 := (BYTE) (dupCount << shift)
                wValue:b2 := trailCount
                wValue:wordValue := (WORD) (wValue:wordValue << shift)
                RETURN wValue:wordValue
            END UNCHECKED
        #pragma warnings (170, DEFAULT)
       #pragma warnings (170, off)
       PRIVATE METHOD _getDupTrail(wData AS WORD, dupCount OUT BYTE, trailCount OUT BYTE) AS VOID
            LOCAL shift AS INT
            LOCAL wValue AS __WordStruct
            shift := SELF:LenShift & 0xFF
            wValue:wordValue := (WORD) (wData >> shift)
            trailCount := wValue:b2
            dupCount   := (BYTE) (wValue:b1 >> shift)
            RETURN

       PRIVATE METHOD _placeRecno(nIndex AS INT, recno AS LONG, dupLen AS WORD) AS VOID
            BEGIN UNCHECKED
                LOCAL nOffset AS LONG
                LOCAL liValue AS __LongStruct
                nOffset           := CDXLEAF_HEADERLEN + nIndex * SELF:DataBytes
                liValue:longValue := recno
	            _buffer[nOffset]   :=  liValue:b1
                _buffer[nOffset+1] :=  liValue:b2
                IF SELF:DataBytes > 2
                    _buffer[nOffset+2] :=  liValue:b3
                    IF SELF:DataBytes > 3
                        _buffer[nOffset+3] :=  liValue:b4
                    ENDIF
                ENDIF
                nOffset := nOffset + SELF:DataBytes - 2
                LOCAL wValue := SELF:_GetWord(nOffset) AS WORD
                wValue |= dupLen
                SELF:_SetWord(nOffset , wValue)
            END UNCHECKED
            RETURN

       #pragma warnings (170, DEFAULT)
       INTERNAL OVERRIDE METHOD FindKey(key AS BYTE[], recno AS LONG, nSearchLen AS LONG) AS WORD
            SELF:_ExpandKeys(FALSE)
            IF SELF:NumKeys > 0
                LOCAL nI AS WORD
                FOR nI := 0 TO SELF:NumKeys -1
                    VAR leaf    := _leaves[nI]
                    VAR nDiff   := SELF:Tag:__Compare(leaf:Key, key, nSearchLen, leaf:Recno, recno)
                    IF nDiff > 0
                        // insert before this key
                        RETURN nI
                    ENDIF
               NEXT
           ENDIF
           RETURN (WORD) (SELF:NumKeys+1)

       INTERNAL OVERRIDE METHOD Dump AS STRING
            LOCAL sb AS StringBuilder
            LOCAL iLen AS WORD
            VAR lTagList := SELF IS CdxTagList
            IF lTagList
                iLen := 10
            ELSE
                iLen := SELF:Tag:KeyLength
            ENDIF
            sb := StringBuilder{}
            sb:AppendLine("")
            sb:AppendLine("--------------------------")
            if lTagList
                sb:AppendLine(String.Format("{0} Page {1:X6}, # of tags: {2}, Free Bytes {3}", "TagList", SELF:PageNo, SELF:NumKeys, SELF:Freespace))
            else
                sb:AppendLine(String.Format("{0} Page {1:X6}, # of keys: {2}, Free Bytes {3}", SELF:PageType, SELF:PageNo, SELF:NumKeys, SELF:Freespace))
            endif
            sb:AppendLine(String.Format("     RecnoMask: 0x{0:X}, DuplicateMask: 0x{1:X}, TrailingMask: 0x{2:X}",SELF:RecnoMask, SELF:DuplicateMask, SELF:TrailingMask))
            sb:AppendLine(String.Format("     DataBytes: {0}, RecordBits: {1}, DuplicateBits: {2}, TrailBit: {3}", SELF:DataBytes, SELF:RecordBits, SELF:DuplicateBits, SELF:TrailingBits))
            sb:AppendLine(String.Format("Left page reference {0:X6}", SELF:LeftPtr))
            IF SELF:NumKeys > 0
               SELF:_ExpandKeys(FALSE)
               VAR nPos := 0
               FOREACH VAR leaf IN _leaves
                    IF lTagList
                        sb:AppendLine(String.Format("Item {0,2}, Page {1:X8}, Data {2,3}, Dup {3,3}, Trail {4,3} : {5} ", nPos,  leaf:Recno, iLen-leaf:Dup-leaf:Trail, leaf:Dup, leaf:Trail, leaf:KeyText))
                    ELSE
                        sb:AppendLine(String.Format("Item {0,2}, Record {1,5}, Data {2,3}, Dup {3,3}, Trail {4,3} : {5} ", nPos,  leaf:Recno, iLen-leaf:Dup-leaf:Trail, leaf:Dup, leaf:Trail, leaf:KeyText))
                    ENDIF
                    nPos++
                NEXT
            ENDIF
            sb:AppendLine(String.Format("Right page reference {0:X6}", SELF:RightPtr))
            sb:AppendLine("")
            RETURN sb:ToString()

        METHOD DumpKeys AS VOID
        Debug("Dump keys for leaf page", SELF:PageNoX)
        FOREACH VAR Leaf IN SELF:Keys
            Debug("Rec", Leaf:Recno, Leaf:KeyText:Trim())
        NEXT

    END CLASS

END NAMESPACE


