//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
/*
LEAF Page
- A Leaf page has type 2. The List of CDX tags is a special leaf page with type 3 (Root + Leaf)
- It starts with a fixed block of 24 bytes:
  BYTE     attr    [ 2 ];    node type 
  BYTE     nKeys   [ 2 ];    number of keys 
  BYTE     leftPtr [ 4 ];    offset of left node or -1 
  BYTE     rightPtr[ 4 ];    offset of right node or -1
  BYTE     freeSpc [ 2 ];    free space available in a page in bytes
  BYTE     recMask [ 4 ];    record number mask 
  BYTE     dupMask;          duplicate bytes count mask 
  BYTE     trlMask;          trailing bytes count mask 
  BYTE     recBits;          number of bits for record number 
  BYTE     dupBits;          number of bits for duplicate count
  BYTE     trlBits;          number of bits for trailing count 
  BYTE     keyBytes;         total number of bytes for recno/dup/trail info 
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

USING System.Runtime.CompilerServices
USING System.Runtime.InteropServices
USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Diagnostics

BEGIN NAMESPACE XSharp.RDD.CDX
    [DebuggerDisplay("{Recno} {KeyText}")];
    INTERNAL STRUCTURE CdxLeaf
        INTERNAL Recno  AS LONG
        INTERNAL Key    AS BYTE[]
        INTERNAL Trail  as BYTE
        INTERNAL Dup    as BYTE
        INTERNAL PROPERTY KeyText AS STRING GET SELF:Key:ToAscii()
        CONSTRUCTOR (nRecno AS LONG, bKey AS BYTE[], nDup as byte, nTrail as byte)
            SELF:Recno := nRecno
            SELF:Key   := (BYTE[]) bKey:Clone()
            self:Dup   := nDup
            SELF:Trail := nTrail
            RETURN
    END STRUCTURE
	/// <summary>
	/// CdxLeaf page. this class maps the Leaf page from the file in memory
    /// Manipulating the page is implemented in the CdxTag class
	/// </summary>
	INTERNAL CLASS CdxLeafPage INHERIT CdxTreePage
#region Fields
        // Private fields that map the various fixed values in a page
        // The values are cached here so they won't have to be decoded from the page everytime they are used
        PRIVATE _numKeys        AS WORD
        PRIVATE _leftPtr        AS LONG
        PRIVATE _rightPtr       AS LONG
        PRIVATE _freeSpace      AS WORD
        PRIVATE _recnoMask      AS LONG
        PRIVATE _duplicateMask  AS BYTE
        PRIVATE _trailingMask   AS BYTE
        PRIVATE _recordBits     AS BYTE
        PRIVATE _dupBits        AS BYTE
        PRIVATE _trailBits      AS BYTE
        PRIVATE _dataBytes      AS BYTE

        // Other fields 
		PRIVATE _keyLen    AS WORD
        PRIVATE _lenShift  AS INT
        PRIVATE _leaves    AS List<CdxLeaf>
        PROTECTED _bTrail    AS BYTE


#endregion
#region constants				
		PRIVATE CONST CDXLEAF_OFFSET_NUMKEYS		:= 2	AS WORD // 2 WORD
		PRIVATE CONST CDXLEAF_OFFSET_LEFTPTR		:= 4	AS WORD // 4 LONGINT
		PRIVATE CONST CDXLEAF_OFFSET_RIGHTPTR 	    := 8	AS WORD // 4 LONGINT
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


        INTERNAL CONSTRUCTOR( bag AS CdxOrderBag, page AS CdxPage)
            SELF(bag, page:PageNo, page:Buffer, (WORD) IIF(page:Tag != NULL, page:Tag:KeyLength,0))

	    INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , nPage AS Int32 , buffer AS BYTE[], nKeyLen AS WORD)
            SUPER(bag, nPage, buffer)
            _keyLen     := nKeyLen
            _leaves     := NULL
            _bTrail     := 0
            SELF:_getValues()

            RETURN
        INTERNAL METHOD InitBlank(oTag AS CdxTag) AS VOID
            SELF:Tag    := oTag
            SELF:Initialize(_keyLen)
            IF Tag != NULL
                _bTrail := (BYTE) (IIF(Tag:KeyType == __UsualType.String, 32, 0) )
            ELSE
                _bTrail := 32
            ENDIF

        PROTECTED VIRTUAL METHOD _setTag(newTag AS CdxTag) AS VOID
            _tag := newTag
            IF newTag != NULL
                _bTrail := (BYTE) (IIF(_tag:KeyType == __UsualType.String, 32, 0) )
            ENDIF

        INTERNAL VIRTUAL METHOD Initialize(keyLength AS WORD) AS VOID
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
            SELF:_lenShift      := (keyLength << 8 ) | (8 - SELF:DuplicateBits)
            RETURN

        PRIVATE METHOD SetProperties() as VOID
            SELF:ClearRecordsAndKeys()
            _keyLen             := keyLength
            VAR bits            := CdxHelpers.GetBits(keyLength)
            // base dupCountMask, trailCountMNask, numbitsRecno and other info are based on keylength 
            SELF:DataBytes      := IIF (bits > 12, 5, IIF( bits > 8, 4, 3))
            SELF:RecordBits     := (SELF:DataBytes << 3) - (bits << 1)
            SELF:DuplicateBits  := SELF:TrailingBits  := bits
            SELF:TrailingMask   := SELF:DuplicateMask := (BYTE) (( 1 << bits  ) - 1)
            SELF:RecnoMask      := (1 << SELF:RecordBits) -1
            RETURN

        INTERNAL METHOD ClearRecordsAndKeys() AS VOID
            SELF:Freespace  := CDXLEAF_BYTESFREE
            SELF:NumKeys    := 0
            SELF:_leaves    := NULL
            memset(SELF:Buffer,CDXLEAF_HEADERLEN,CDXLEAF_BYTESFREE,0)
            RETURN

        PROTECTED INTERNAL VIRTUAL METHOD Read() AS LOGIC
			VAR Ok := SUPER:Read()
            Debug.Assert (SELF:PageType:HasFlag(CdxPageType.Leaf))
            IF Ok
                SELF:_getValues()
            ENDIF
            RETURN Ok

        INTERNAL PROPERTY ValidLeaves AS LOGIC GET _leaves != NULL .AND. _leaves:Count == SELF:NumKeys
#region ICdxKeyValue
        PUBLIC METHOD GetRecno(nPos AS Int32) AS Int32
            IF SELF:ValidLeaves
                RETURN _leaves[nPos]:Recno
            ENDIF
            LOCAL nOffSet   AS Int32
            LOCAL nRecno    AS Int32
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nOffSet     := CDXLEAF_HEADERLEN + nPos * SELF:DataBytes
            nRecno      := _GetLong(nOffSet) 
            nRecno      := _AND( nRecno , SELF:RecnoMask)
            RETURN nRecno

        PUBLIC METHOD GetChildPage(nPos AS Int32) AS Int32
            RETURN 0

        PUBLIC METHOD GetKey(nPos AS Int32) AS BYTE[]
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            SELF:_ExpandLeaves(FALSE)
            RETURN _leaves[nPos]:Key
#endregion

         // For Debugging we calculate all Recnos and KeyBytes
         // Later we will remove this
         PRIVATE METHOD _ExpandLeaves(lForce := FALSE AS LOGIC)  AS LOGIC
            IF ! lForce .AND. ValidLeaves
                RETURN TRUE
            ENDIF
            LOCAL nOffSet   AS Int32
            LOCAL aBytes := BYTE[]{_KeyLen} AS BYTE[]
            LOCAL nRecno    AS Int32
            LOCAL nDup, nTrail AS Byte
            LOCAL nKey      AS Int32
            LOCAL nStart    AS Int32
            LOCAL nStep     AS Int32
            LOCAL nLast     AS Int32
            LOCAL trailchar  AS BYTE

            
            // First key starts at end of page
            nStart := CDXPAGE_SIZE
            _leaves := List<CdxLeaf>{}
            IF SELF:Tag != NULL
                trailChar :=  (BYTE) IIF (Tag:KeyType == __UsualType.String, 32, 0)
            ELSE
                trailChar := 32
            ENDIF
            nOffSet := CDXLEAF_HEADERLEN
            nStep := SELF:DataBytes
            IF SELF:NumKeys > 0
                nLast := SELF:NumKeys-1
                FOR VAR nI := 0 TO nLast
                    LOCAL iTemp AS WORD
                    nRecno  := _GetLong(nOffSet)
                    nRecno  := _AND( nRecno , SELF:RecnoMask)
                    iTemp   := _GetWord(nOffSet + SELF:DataBytes - 2)
                    SELF:_getDupTrail(iTemp, out nDup, out nTrail)
                    nKey    := _KeyLen - nTrail - nDup
                    IF nTrail > 0
                        IF nTrail > _KeyLen
                            ? "nTrail > _KeyLen"
                        ENDIF
                        TRY
                            MemSet(aBytes, _KeyLen - nTrail, nTrail, trailChar)
                        CATCH e AS Exception
                            NOP
                        END TRY
                    ENDIF
                    // Copy to aBytes from pos nDup
                    IF nKey > 0
                        IF nKey > nStart
                            ? "nKey > nStart"
                            Altd()
                        ENDIF
                        TRY
                            System.Array.Copy(Buffer, nStart - nKey, aBytes, nDup, nKey)
                        CATCH e AS Exception
                            NOP
                        END TRY
                        nStart := nStart - nKey
                    ENDIF
                    _leaves.Add( CdxLeaf{ nRecno, aBytes,nDup, nTrail})
                    nOffSet      += nStep
                NEXT
            ENDIF
            RETURN TRUE

        PROTECTED INTERNAL OVERRIDE METHOD Write() AS LOGIC
            SELF:Compress()
            RETURN SUPER:Write()
            

        PRIVATE METHOD _getValues AS VOID
            _numKeys        := _GetWord(CDXLEAF_OFFSET_NUMKEYS)
            _leftPtr        := _GetLong(CDXLEAF_OFFSET_LEFTPTR)
            _rightPtr       := _GetLong(CDXLEAF_OFFSET_RIGHTPTR)
            _freeSpace      := _GetWord(CDXLEAF_OFFSET_FREESPACE)
            _recnoMask      := _GetLong(CDXLEAF_OFFSET_RECNOMASK)
            _duplicateMask  := Buffer[CDXLEAF_OFFSET_DUPMASK]
            _trailingMask   := Buffer[CDXLEAF_OFFSET_TRAILMASK]
            _recordBits     := Buffer[CDXLEAF_OFFSET_RECNUMBITS]
            _dupBits        := Buffer[CDXLEAF_OFFSET_DUPCOUNTBITS]
            _trailBits      := Buffer[CDXLEAF_OFFSET_TRAILINGBITS]
            _dataBytes      := Buffer[CDXLEAF_OFFSET_DATABYTES]
            _lenShift       := (keyLength << 8 ) | (8 - SELF:DuplicateBits)


#region Properties
        // We read the values from our cache but write back to the cache and the buffer at the same time
        // The _Set.. methods set the IsHot flag of the page automatically
		PUBLIC PROPERTY NumKeys  AS WORD	GET _numKeys;
			SET _SetWord(CDXLEAF_OFFSET_NUMKEYS, VALUE), _numKeys := VALUE

		INTERNAL PROPERTY LeftPtr AS Int32  GET _leftPtr;
			SET _SetLong(CDXLEAF_OFFSET_LEFTPTR, VALUE), _leftPtr:= VALUE

		INTERNAL PROPERTY RightPtr AS Int32	GET _rightPtr;
			SET _SetLong(CDXLEAF_OFFSET_RIGHTPTR, VALUE), _rightPtr := VALUE
			
		INTERNAL PROPERTY Freespace AS WORD GET _freeSpace ;
			SET _SetWord(CDXLEAF_OFFSET_FREESPACE, VALUE),  _freeSpace := VALUE
			
		INTERNAL PROPERTY RecnoMask AS LONG GET _recnoMask;
			SET _SetLong(CDXLEAF_OFFSET_RECNOMASK, VALUE),  _recnoMask := VALUE
			
		INTERNAL PROPERTY DuplicateMask	AS BYTE	GET _duplicateMask;
			SET _buffer[CDXLEAF_OFFSET_DUPMASK]      := _duplicateMask := VALUE, _hot := TRUE

		INTERNAL PROPERTY TrailingMask AS BYTE	GET _trailingMask ;
			SET _buffer[CDXLEAF_OFFSET_TRAILMASK]    := _trailingMask := VALUE, _hot := TRUE
			
		INTERNAL PROPERTY RecordBits AS BYTE GET _recordBits;
			SET _buffer[CDXLEAF_OFFSET_RECNUMBITS]   := _recordBits := VALUE, _hot := TRUE
			
		INTERNAL PROPERTY DuplicateBits AS BYTE	GET _dupBits;
			SET _buffer[CDXLEAF_OFFSET_DUPCOUNTBITS] := _dupBits := VALUE, _hot := TRUE
			
		INTERNAL PROPERTY TrailingBits  AS BYTE	GET _trailBits;
			SET _buffer[CDXLEAF_OFFSET_TRAILINGBITS] := _trailBits := VALUE, _hot := TRUE
			
		INTERNAL PROPERTY DataBytes	AS BYTE	GET _dataBytes;
			SET _buffer[CDXLEAF_OFFSET_DATABYTES] := _dataBytes := VALUE, _hot := TRUE

        INTERNAL PROPERTY LenShift as INT GET _lenShift
#endregion			

#region Other properties
        INTERNAL PROPERTY KeyLength AS WORD GET _keyLen Set _keyLen := Value
            
        // Retrieve an index node in the current Page, at the specified position
        // return CdxLeagPageNode which always returns 0 for the ChildPage
        INTERNAL OVERRIDE PROPERTY SELF[ index AS WORD ] AS CdxPageNode
            GET
                RETURN CdxLeafPageNode{ SELF:KeyLength, SELF, index }
            END GET
        END PROPERTY

        INTERNAL PROPERTY LastNode AS CdxPageNode GET IIF(SELF:NumKeys == 0, NULL, SELF[SELF:NumKeys-1])
        INTERNAL PROPERTY Leaves    AS IList<CdxLeaf>
            GET
                // Does not expand the leaves
                RETURN SELF:_leaves
            END GET
        END PROPERTY
#endregion

        INTERNAL METHOD GetLeaves() AS IList<CdxLeaf>
            SELF:_ExpandLeaves(TRUE)
            RETURN SELF:_leaves

        INTERNAL PROPERTY DebugLeaves AS IList<CdxLeaf>
            GET
                RETURN SELF:GetLeaves()
            END GET
        END PROPERTY

   
        // This method assumes keys are added in the right order.
        INTERNAL METHOD Add(recno AS LONG, key AS BYTE[]) AS CdxAction
            LOCAL nTrailCount AS BYTE
            LOCAL nDupCount   AS BYTE
            //System.Diagnostics.Trace.WriteLine(i"CdxLeafPage:Add({recno})")
            IF _AND( recno, SELF:RecnoMask) != recno
                RETURN CdxAction.ExpandRecnos(SELF, recno, key, -1)
            ENDIF
            self:_ExpandLeaves(FALSE)
            nTrailCount := _getTrailCount(key)
            IF SELF:NumKeys == 0
                nDupCount := 0
            ELSE
                var prevkey := _Leaves[SELF:NumKeys-1]:Key
                nDupCount := _getDupCount(prevkey, key, nTrailCount)
            ENDIF
            var nBytesNeeded := SELF:_keyLen - nDupCount - nTrailCount  + self:DataBytes
            if SELF:Freespace < nBytesNeeded
                return CdxAction.SplitLeaf(SELF, recno, key, _leaves:Count)
            endif
            _leaves:Add( CdxLeaf{recno, key,nDupCount, nTrailCount})
            SELF:NumKeys += 1
            SELF:Freespace -= nBytesNeeded
            VAR compResult := SELF:Compress()
            IF CompResult:Type != CdxActionType.Ok
                RETURN CompResult
            ENDIF
            RETURN CdxAction.Ok


        INTERNAL METHOD SetLeaves(leaves as IList<CdxLeaf>, nStart as LONG, nCount as LONG) AS CdxAction
            LOCAL action as CdxAction
            SELF:ClearRecordsAndKeys()
            FOR VAR nKey := nStart TO nStart+nCount-1
                local key := leaves[nKey] as CdxLeaf
                action := SELF:Add(key:Recno, key:Key)
            NEXT
            action := SELF:Compress()
            return action

        INTERNAL METHOD Insert(nPos AS LONG, recno AS LONG, key AS BYTE[]) AS CdxAction

            // A quick calculation if we have enough room, ignoring the duplicate count
            LOCAL nTrailCount AS BYTE
            local nBytesNeeded as WORD
            IF _AND( recno, SELF:RecnoMask) != recno
                RETURN CdxAction.ExpandRecnos(SELF,recno, key, nPos)
            ENDIF
            nTrailCount := _getTrailCount(key)
            // we calculate if a key fits without looking at the Duplicate bytes to make things easier
            // it might fit when the # of duplicate bytes is large, but we simply don't allow it
            nBytesNeeded := SELF:DataBytes + SELF:_keyLen - nTrailCount
            IF SELF:Freespace < nBytesNeeded
                RETURN CdxAction.SplitLeaf(SELF, recno,key, nPos)
            ENDIF
            // Todo: optimize. We are now expanding the leaves which could be overkill.
            _ExpandLeaves(FALSE)
            // compare position in page
            #ifdef TESTCDX
            IF nPos > 0
                VAR existingkey := SELF:_leaves[nPos-1]:Key
                VAR nComp := SELF:Tag:__Compare(existingkey, key, SELF:_keyLen)
                Debug.Assert(nComp <=0) // New key larger than existig key
            ENDIF
            IF nPos < SELF:_Leaves:Count
                VAR existingkey := SELF:_leaves[nPos]:Key
                VAR nComp := SELF:Tag:__Compare(existingkey, key, SELF:_keyLen)
                Debug.Assert(nComp >=0) // New key smaller than existig key
            ELSEIF SELF:_leaves:Count > 0
                VAR existingkey := SELF:_leaves[SELF:_leaves:Count-1]:Key
                VAR nComp := SELF:Tag:__Compare(existingkey, key, SELF:_keyLen)
                Debug.Assert(nComp <=0) // New key larger than existing key
            ENDIF
            #endif
            LOCAL nDupCount as BYTE
            IF _Leaves:Count == 0
                _Leaves:Add(CdxLeaf{recno, Key, 0, nTrailCount})
            ELSEIF nPos < 0 .OR. nPos >= _Leaves:Count
                var prevkey := _Leaves[_Leaves:Count-1]:Key
                nDupCount := SELF:_getDupCount(prevkey, key,  nTrailCount)
                _Leaves:Add(CdxLeaf{recno, Key, nDupCount, nTrailCount})
            ELSEIF nPos == 0
                nDupCount := 0
                _Leaves:Insert(nPos, CdxLeaf{Recno, Key,nDupCount,nTrailCount})
                var nextElement := _Leaves[1]
                var nextKey := nextElement:Key
                nDupCount := SELF:_getDupCount(key, nextkey,  nTrailCount)
                nextElement:Dup := nDupCount
            ELSE // nPos > 0 .AND. nPos < _Leaves:Count
                var prevkey := _Leaves[nPos-1]:Key
                nDupCount := SELF:_getDupCount(prevkey, key,  nTrailCount)
                _Leaves:Insert(nPos, CdxLeaf{Recno, Key,nDupCount,nTrailCount})
            ENDIF

            VAR result := CdxAction.Ok
            IF nPos == SELF:NumKeys .AND. ! SELF:PageType:HasFlag(CdxPageType.Root)
                result := CdxAction.ChangeParent(SELF)
            ENDIF
            SELF:NumKeys := (WORD) _Leaves:Count
            VAR compResult := SELF:Compress()
            IF CompResult:Type != CdxActionType.Ok
                RETURN CompResult
            ENDIF
            RETURN result


        INTERNAL METHOD Split(oPageR AS CdxLeafPage, action AS CdxAction) AS CdxAction
            VAR leaves      := SELF:GetLeaves()
            //? "CdxLeafPage:Split( "+SELF:PageNo:ToString("X") +", "+oPageR:PageNo:ToString("X")+" ) "
            var nTrailCount := SELF:_getTrailCount(action:Key)
            local nDupCount as BYTE
            if action:Pos == 0
                nDupCount := 0
            ELSE
                var prevKey := leaves[action:Pos]:Key
                nDupCount := SELF:_getDupCount(prevkey, action:Key, nTrailCount)
            ENDIF
            leaves:Insert(action:Pos, CdxLeaf{action:Recno, action:Key, nDupCount, nTrailCount})
            VAR half := leaves:Count /2
            //? "Writing to page 1", oPageR:PageNo:ToString("X")
            SELF:ClearRecordsAndKeys()
            FOR VAR i := 0 TO half
                VAR leaf := leaves[i]
                action := CdxAction.AddKey(leaf:Recno, leaf:Key)
                action := SELF:Tag:DoAction(action)
                Debug.Assert(action:IsOk())
            NEXT
            oPageR:ClearRecordsAndKeys()
            SELF:Write()
            //? "Writing to page 2", oPageR:PageNo:ToString("X")
            SELF:Tag:AdjustStack(SELF, oPageR, oPageR:NumKeys)
            FOR VAR i := half+1 TO leaves:Count-1
                VAR leaf := leaves[i]
                action := CdxAction.AddKey(leaf:Recno, leaf:Key)
                action := SELF:Tag:DoAction(action)
                Debug.Assert(action:IsOk())
            NEXT
            oPageR:Write()
            RETURN CdxAction.Ok


        INTERNAL METHOD Delete(nPos AS LONG) AS CdxAction
             // Todo: optimize. We are now expanding the leaves which could be overkill.
            IF nPos >= 0 .AND. nPos < SELF:NumKeys
                _ExpandLeaves(FALSE)
                VAR result := CdxAction.Ok
                IF nPos == SELF:NumKeys -1
                    //SELF:Tag:SetChildToProcess(SELF:PageNo)
                    result := CdxAction.ChangeParent(SELF)
                ENDIF
                _Leaves:RemoveAt(nPos)
                SELF:NumKeys -= 1
                IF SELF:NumKeys = 0
                    RETURN CdxAction.Delete(SELF)
                ENDIF
                LOCAL result2 :=  SELF:Compress() AS CdxAction
                IF result2.Type == CdxActionType.Ok
                    RETURN result
                ELSE
                    RETURN result2
                ENDIF
            ENDIF
            RETURN CdxAction.OutofBounds(SELF)

        INTERNAL METHOD Replace(nPos AS LONG, node AS CdxNode) AS CdxAction
            // Todo: optimize. We are now expanding the leaves which could be overkill.
            IF nPos >= 0 .AND. nPos < SELF:NumKeys
                _ExpandLeaves(FALSE)
                var nTrailCount := self:_getTrailCount(node:KeyBytes)
                local nDupCount as BYTE
                if nPos == 0
                    nDupCount := 0
                ELSE
                    nDupCount := SELF:_getDupCount(self:_leaves[nPos-1]:Key, node:KeyBytes,nTrailCount)
                ENDIF
                _Leaves[nPos] := CdxLeaf{node:Recno, node:KeyBytes, nDupCount, nTrailCount}
                RETURN SELF:Compress()
            ENDIF
            RETURN CdxAction.OutofBounds(SELF)

        INTERNAL METHOD Compress() AS CdxAction
            // Todo: optimize. We are now expanding and compressing the leaves which could be overkill.
            // Also the buffer is saved (because we may discover a 'page full' and we need to restore then
            // maybe we can restore from disk ?
            // finally: do we have to call Write? Or is it enough to set the page as dirty
            IF SELF:NumKeys == 0 .or. ! SELF:ValidLeaves
                RETURN CdxAction.Ok
            ENDIF
            var leaves  := SELF:_Leaves
            var copy    := (BYTE[]) self:Buffer:Clone()
            SELF:InitBlank(SELF:Tag)
            LOCAL nKey   := 0 as INT
            LOCAL nStart := CDXLEAF_HEADERLEN + SELF:Freespace AS WORD
            FOREACH Leaf as CdxLeaf IN leaves
                var nBytesToCopy := SELF:_keyLen - Leaf:Dup - Leaf:Trail
                if self:Freespace < SELF:DataBytes + nBytesToCopy 
                    SELF:_Buffer := copy
                    RETURN CdxAction.Ok
                endif
                SELF:_placeRecno(nKey, Leaf:Recno, SELF:_makeDupTrail(Leaf:Dup, Leaf:Trail))
                IF nBytesToCopy != 0
                    System.Array.Copy(leaf:Key, Leaf:Dup, buffer, nStart-nBytesToCopy,  nBytesToCopy)
                    SELF:Freespace := SELF:Freespace -  nBytesToCopy - SELF:DataBytes
                ELSE
                    SELF:Freespace := SELF:Freespace -  SELF:DataBytes
                ENDIF
                nStart  -= nBytesToCopy  
                nKey    += 1
            NEXT
            SELF:NumKeys := (WORD) leaves:Count
            SELF:_leaves := leaves
            RETURN CdxAction.Ok

 
       PRIVATE METHOD _getTrailCount(data AS BYTE[]) AS BYTE
           LOCAL iLastTrail AS LONG
           LOCAL bTrail := _bTrail AS BYTE
           iLastTrail  := 0
           FOR VAR i := data:Length -1 DOWNTO 0 
                IF data[i] != bTrail
                    iLastTrail := i
                    EXIT
                ENDIF
           NEXT
           RETURN (BYTE)  (data:Length - iLastTrail -1)

        PRIVATE METHOD _getDupCount(prevdata as byte[], data AS BYTE[], trailCount AS LONG) AS BYTE
           LOCAL last := data:Length - trailCount  AS LONG
           LOCAL dup AS LONG
           FOR dup := 0 UPTO last -1 
              IF data[dup] != prevdata[dup]
                 EXIT
              ENDIF
           NEXT
           RETURN (BYTE) dup

       PRIVATE METHOD _makeDupTrail(dupCount AS BYTE, trailCount AS BYTE) AS WORD
            LOCAL w     := wordStruct{} AS wordStruct
            LOCAL shift AS INT
            shift := SELF:LenShift & 0xFF
            w:b1 := (BYTE) dupCount << shift
            w:b2 := trailCount
            w:wordValue := w:wordValue << shift
            return w:WordValue


       PRIVATE METHOD _getDupTrail(wData as WORD, dupCount out byte, trailCount out byte) AS VOID
            LOCAL w     := wordStruct{} AS wordStruct
            LOCAL shift AS INT
            shift := SELF:LenShift & 0xFF
            w:wordValue := wData >> shift
            trailCount := w:b2
            dupCount   := w:b1 >> shift
            return 
            

       PRIVATE METHOD _placeRecno(nIndex AS INT, recno AS LONG, dupLen AS WORD) AS VOID
            LOCAL nOffset AS LONG
            LOCAL nValue := LongStruct{} AS LongStruct
            nOffSet           := CDXLEAF_HEADERLEN + nIndex * SELF:DataBytes
            nValue:LongValue  := recno
	        buffer[nOffSet]   :=  nValue:b1
            buffer[nOffSet+1] :=  nValue:b2  
            buffer[nOffSet+2] :=  nValue:b3
            if self:DataBytes > 3
                buffer[nOffSet+3] :=  nValue:b4
            endif
            nOffSet := nOffSet + SELF:DataBytes - 2
            LOCAL wValue := _GetWord(nOffSet) AS WORD
            wValue |= dupLen
            _SetWord(nOffSet , wValue)
            RETURN


       INTERNAL override METHOD FindKey(key as byte[], recno as long) as long
            SELF:_ExpandLeaves(FALSE)
            for var nI := 0 to self:Numkeys -1
                var pageKey := _leaves[nI]:Key
                var nDiff := SELF:Tag:__Compare(pageKey, key, key:Length)
                if nDiff == 0 .and. _leaves[nI]:Recno >= recno
                    return nI
                elseIF nDiff > 0
                    // insert before this key
                    return nI
                endif
           next
           return -1

       INTERNAL METHOD Dump AS STRING
            LOCAL Sb AS stringBuilder
            LOCAL i AS WORD
            sb := stringBuilder{}
            sb:AppendLine("--------------------------")
            sb:AppendLine(String.Format("{0} Page {1:X6}, # of keys: {2}, Free Bytes {3}", SELF:PageType, SELF:PageNo, SELF:NumKeys, SELF:Freespace))
            sb:AppendLine(String.Format("Left page reference {0:X6}", SELF:LeftPtr))
            IF SELF:NumKeys > 0
               self:_ExpandLeaves(false)
               var nPos := 0
               foreach var leaf in _leaves
                    sb:AppendLine(String.Format("Item {0,2}, Record {1,5}, Dup {2,3}, Trail {3,3} : {4} ", nPos,  leaf:Recno, Leaf:Dup, leaf:Trail, leaf:KeyText))
                    nPos++
                NEXT
            ENDIF
            sb:AppendLine(String.Format("Right page reference {0:X6}", SELF:RightPtr))
            RETURN sb:ToString()

    END CLASS

END NAMESPACE 


