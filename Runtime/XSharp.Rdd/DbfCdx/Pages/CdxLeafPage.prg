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

USING System.Runtime.CompilerServices
USING System.Runtime.InteropServices
USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Diagnostics

BEGIN NAMESPACE XSharp.RDD.CDX
    [DebuggerDisplay("{DebuggerDisplay,nq}")];
    INTERNAL SEALED CLASS CdxLeaf
        INTERNAL Recno  AS LONG
        INTERNAL Key    AS BYTE[]
        INTERNAL Trail  AS BYTE
        INTERNAL Dup    AS BYTE
        INTERNAL PROPERTY KeyText AS STRING GET SELF:Key:ToAscii()
        INTERNAL PROPERTY DebuggerDisplay AS STRING GET String.Format("{0,6} {1} ", Recno, KeyText)
        CONSTRUCTOR (nRecno AS LONG, bKey AS BYTE[], nDup AS BYTE, nTrail AS BYTE)
            SELF:Recno := nRecno
            SELF:Key   := (BYTE[]) bKey:Clone()
            SELF:Dup   := nDup
            SELF:Trail := nTrail
            RETURN
    END CLASS
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
        PRIVATE _leaves    AS List<CdxLeaf>


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
            KeyLength   := nKeyLen
            _leaves     := NULL
            TrailByte   := 0
            SELF:_getValues()

            RETURN
        INTERNAL METHOD InitBlank(oTag AS CdxTag) AS VOID
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

        PROTECTED VIRTUAL METHOD _setTag(newTag AS CdxTag) AS VOID
            _tag := newTag
            IF newTag != NULL
                IF _Tag:Collation != NULL
                    TrailByte := 0
                ELSE
                    TrailByte := (BYTE) (IIF(_Tag:KeyType == __UsualType.String, 32, 0) )
                ENDIF
            ENDIF

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
            RETURN

        PRIVATE METHOD SetProperties() AS VOID
            SELF:ClearRecordsAndKeys()
            VAR bits            := CdxHelpers.GetBits(SELF:KeyLength)
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

        INTERNAL VIRTUAL METHOD Read() AS LOGIC
			VAR Ok := SUPER:Read()
            System.Diagnostics.Debug.Assert (SELF:PageType:HasFlag(CdxPageType.Leaf))
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
            System.Diagnostics.Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nOffSet     := CDXLEAF_HEADERLEN + nPos * SELF:DataBytes
            nRecno      := _GetLong(nOffSet) 
            nRecno      := _AND( nRecno , SELF:RecnoMask)
            RETURN nRecno

        PUBLIC METHOD GetChildPage(nPos AS Int32) AS Int32
            RETURN 0

        PUBLIC METHOD GetChildren as IList<LONG>
            RETURN List<LONG>{}

        PUBLIC METHOD GetKey(nPos AS Int32) AS BYTE[]
            System.Diagnostics.Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
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
            LOCAL aBytes := BYTE[]{KeyLength} AS BYTE[]
            LOCAL nRecno    AS Int32
            LOCAL nDup, nTrail AS BYTE
            LOCAL nCopy     AS Int32
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
                    SELF:_getDupTrail(iTemp, OUT nDup, OUT nTrail)
                    nCopy    := SELF:KeyLength - nTrail - nDup
                    IF nTrail > 0
                        IF nTrail > SELF:KeyLength
                            //? "nTrail > _KeyLen"
                            NOP
                        ELSE
                            TRY
                                MemSet(aBytes, SELF:KeyLength - nTrail, nTrail, trailChar)
                            CATCH AS Exception
                                NOP
                            END TRY
                        ENDIF
                    ENDIF
                    // Copy to aBytes from pos nDup
                    IF nCopy > 0
                        IF nCopy > nStart
                            //? "nKey > nStart"
                            NOP // Altd()
                        ELSE
                            TRY
                                System.Array.Copy(Buffer, nStart - nCopy, aBytes, nDup, nCopy)
                            CATCH AS Exception
                                NOP
                            END TRY
                            nStart := nStart - nCopy
                        ENDIF
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
            LenShift       := (SELF:KeyLength << 8 ) | (8 - SELF:DuplicateBits)


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

        INTERNAL PROPERTY LastNode AS CdxPageNode GET IIF(SELF:NumKeys == 0, NULL, SELF[SELF:NumKeys-1])
        INTERNAL PROPERTY Leaves    AS IList<CdxLeaf>
            GET
                _ExpandLeaves(FALSE)
                RETURN SELF:_leaves
            END GET
        END PROPERTY
#endregion
        INTERNAL METHOD SetRecordBits(numRecs as LONG) AS VOID
            VAR bits            := CdxHelpers.GetBits(SELF:KeyLength)
            DO CASE
            CASE numRecs < 2^12
                SELF:RecordBits     := 12
            CASE numRecs < 2^16
                SELF:RecordBits     := 16
            CASE numRecs < 2^24
                SELF:RecordBits     := 24
            OTHERWISE
                SELF:RecordBits     := 32
            ENDCASE
            var totalBits       := SELF:RecordBits + bits + bits
            DO CASE
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
            SELF:RecnoMask      := (1 << SELF:RecordBits) -1

        INTERNAL METHOD GetLeaves() AS IList<CdxLeaf>
            SELF:_ExpandLeaves(TRUE)
            RETURN SELF:_leaves

 
   
        // This method assumes keys are added in the right order.
        INTERNAL METHOD Add(recno AS LONG, key AS BYTE[]) AS CdxAction
            LOCAL nTrailCount AS BYTE
            LOCAL nDupCount   AS BYTE
            //System.Diagnostics.Trace.WriteLine(i"CdxLeafPage:Add({recno})")
            IF _AND( recno, SELF:RecnoMask) != recno
                //Debug( "triggers ExpandRecnos", "Rec", recno)
                RETURN CdxAction.ExpandRecnos(SELF, recno, key, -1)
            ENDIF
            //Debug("rec",recno, "keys", self:NumKeys, "free before", self:Freespace)
            SELF:_ExpandLeaves(FALSE)
            nTrailCount := _getTrailCount(key)
            IF SELF:NumKeys == 0
                nDupCount := 0
            ELSE
                VAR prevkey := _Leaves[SELF:NumKeys-1]:Key
                nDupCount   := _getDupCount(prevkey, key, nTrailCount)
            ENDIF
            VAR nBytesNeeded := SELF:KeyLength - nDupCount - nTrailCount  + SELF:DataBytes 
            IF SELF:Freespace < nBytesNeeded + nDupCount
                //Debug( "triggers SplitLeaf", "Rec", recno)
                RETURN CdxAction.SplitLeaf(SELF, recno, key, _leaves:Count)
            ENDIF
            VAR leaf := CdxLeaf{recno, key,nDupCount, nTrailCount}
            _leaves:Add( leaf)
#ifdef TESTCDX
            IF _leaves:Count > 1
                ValidateLeaves(_leaves[_leaves:Count-2], leaf)
            ENDIF
#endif
            SELF:NumKeys += 1
            SELF:Freespace -= nBytesNeeded
            //Debug("rec",recno, "keys", self:NumKeys, "free after", self:Freespace)
            SELF:Write()
            RETURN CdxAction.Ok


        INTERNAL METHOD SetLeaves(leaves AS IList<CdxLeaf>, nStart AS LONG, nCount AS LONG) AS CdxAction
            LOCAL action AS CdxAction
            //Debug(nStart, nCount, "keys", self:NumKeys, "free", self:Freespace)
            SELF:ClearRecordsAndKeys()
            FOR VAR nKey := nStart TO nStart+nCount-1
                LOCAL key := leaves[nKey] AS CdxLeaf
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
                //Debug(  "triggers ExpandRecnos", SELF:PageNo:ToString("X8"), "Rec", recno)
                RETURN CdxAction.ExpandRecnos(SELF,recno, key, nPos)
            ENDIF
            //Debug("Pos",nPos, "Rec", recno, "keys", self:NumKeys, "free before", self:Freespace)
            nTrailCount := _getTrailCount(key)
            // we calculate if a key fits without looking at the Duplicate bytes to make things easier
            // it might fit when the # of duplicate bytes is large, but we simply don't allow it
            nBytesNeeded := SELF:DataBytes + SELF:KeyLength - nTrailCount 
            IF SELF:Freespace < nBytesNeeded 
                //Debug( "triggers SplitLeaf",  "Rec", recno)
                RETURN CdxAction.SplitLeaf(SELF, recno,key, nPos)
            ENDIF
            // Todo: optimize. We are now expanding the leaves which could be overkill.
            SELF:_ExpandLeaves(FALSE)
            // compare position in page
            LOCAL nDupCount AS BYTE
            IF _Leaves:Count == 0
                leaf := CdxLeaf{recno, Key, 0, nTrailCount}
                _Leaves:Add(leaf)
                SELF:Freespace -= nBytesNeeded  
                last := TRUE
            ELSEIF nPos < 0 .OR. nPos >= _Leaves:Count
                VAR prevLeaf := _Leaves[_Leaves:Count-1]
                VAR prevkey := prevLeaf:Key
                nDupCount := SELF:_getDupCount(prevkey, key,  nTrailCount)
                leaf := CdxLeaf{recno, Key, nDupCount, nTrailCount}
                _Leaves:Add(leaf)
#ifdef TESTCDX
                ValidateLeaves(prevLeaf, leaf)
#endif
                SELF:Freespace -= (nBytesNeeded-nDupCount)
                last := TRUE
            ELSEIF nPos == 0
                nDupCount       := 0
                SELF:Freespace  -= (nBytesNeeded-nDupCount)
                leaf := CdxLeaf{Recno, Key,nDupCount,nTrailCount}
                _Leaves:Insert(nPos, leaf)
                adjustNext      := TRUE
            ELSE // nPos > 0 .AND. nPos < _Leaves:Count
                VAR prevLeaf    := _Leaves[nPos-1]
                nDupCount       := SELF:_getDupCount(prevLeaf:Key, key,  nTrailCount)
                SELF:Freespace  -= (nBytesNeeded-nDupCount)
                leaf := CdxLeaf{Recno, Key,nDupCount,nTrailCount}

                _Leaves:Insert(nPos, leaf)
#ifdef TESTCDX
                validateLeaves(prevLeaf, leaf)
#endif
                adjustNext      := TRUE
            ENDIF
            IF adjustNext
                VAR nextLeaf := _Leaves[nPos+1]
                nDupCount    := SELF:_getDupCount(key, nextLeaf:Key,  nextLeaf:Trail)
                IF nDupCount != nextLeaf:Dup
                    //Debug("Adjusted next leaf dup from ", nextLeaf:Dup, "to", nDupCount)
                    VAR diff        := nDupCount - nextLeaf:Dup
                    SELF:Freespace  += diff
                    nextLeaf:Dup    := nDupCount
                ENDIF
#ifdef TESTCDX
                validateLeaves(leaf, nextLeaf) 
#endif
            ENDIF
            SELF:NumKeys := (WORD) _Leaves:Count
            //Debug("Pos",nPos, "Rec", recno, "keys", self:NumKeys, "free after", self:Freespace)

            VAR result := CdxAction.Ok
            IF last .AND. ! SELF:PageType:HasFlag(CdxPageType.Root)
                //Debug( "triggers ChangeParent")
                result := CdxAction.ChangeParent(SELF)
            ENDIF
            VAR compResult := SELF:Compress()
            IF CompResult:Type != CdxActionType.Ok
                RETURN CompResult
            ENDIF
            SELF:Write()
            RETURN result

#ifdef TESTCDX
        INTERNAL METHOD ValidateChain() AS VOID
            LOCAL oPage AS CdxLeafPage
            LOCAL oLeft AS CdxLeaf
            LOCAL oRight AS CdxLeaf
            IF SELF:NumKeys == 0 
                RETURN
            ENDIF
            IF SELF:HasLeft
                oPage := SELF:Tag:GetPage(SELF:LeftPtr)
                IF oPage:NumKeys > 0
                    oLeft := oPage:Leaves[oPage:NumKeys-1]
                    oRight := SELF:Leaves[0]
                    ValidateLeaves(oLeft, oRight)
                ENDIF
            ENDIF
            IF SELF:HasRight
                oPage := SELF:Tag:GetPage(SELF:RightPtr)
                IF oPage:NumKeys > 0
                    oRight := oPage:Leaves[0]
                    oLeft := SELF:Leaves[SELF:NumKeys-1]
                    ValidateLeaves(oLeft, oRight)
                ENDIF
            ENDIF

        INTERNAL METHOD ValidateLeaves(oLeft AS CdxLeaf, oRight AS CdxLeaf) AS LOGIC
            LOCAL nDiff AS LONG
            IF SELF:Tag == NULL
                RETURN TRUE
            ENDIF
            nDiff := SELF:Tag:__Compare(oLeft:Key, oRight:Key, oLeft:Key:Length)
            IF nDiff == -1
                RETURN TRUE
            ELSEIF nDiff == 0 .AND. oLeft:Recno < oRight:Recno
                RETURN TRUE
            ENDIF
            //SELF:Debug("Incorrect order of Keys", oLeft:Key:ToAscii(),oRight:Key:ToAscii())
            RETURN FALSE

        METHOD Validate() AS VOID
            SELF:_ExpandLeaves(FALSE)
//            FOR var i := 0  to SELF:_leaves:Count-2
//                var leaf1 := self:_leaves[i]
//                var leaf2 := self:_leaves[i+1]
//                IF ! ValidateLeaves(leaf1, leaf2)
//                    SELF:Debug("Corruption detected: incorrect order of Keys", leaf1:Key:ToAscii(),leaf2:Key:ToAscii())
//                ENDIF
//            NEXT
            IF SELF:NumKeys > 0
                IF SELF:HasLeft
                    var oLeft := SELF:_tag:GetPage(SELF:LeftPtr) astype CdxLeafPage
                    if oLeft != NULL_OBJECT
                        IF oLeft:Leaves:Count > 0
                            var oLeftKey := oLeft:Leaves[oLeft:Leaves:Count-1]
                            var oOurKey  := SELF:Leaves[0]
                            IF ! ValidateLeaves(oLeftKey, oOurKey)
                                SELF:Debug("Corruption detected: Last key on left page",oLeft:PageNo:ToString("X")," is not < our first key", oLeftKey:Key:ToAscii():Trim(),oOurKey:Key:ToAscii():Trim())
                            ENDIF
                        ENDIF
                    ELSE
                        SELF:Debug("Corruption detected: Could not read Left Page")

                    ENDIF
                ENDIF
                IF SELF:HasRight
                    var oRight := SELF:_tag:GetPage(SELF:RightPtr) astype CdxLeafPage
                    if oRight != NULL_OBJECT
                        IF  oRight:Leaves:Count > 0
                            var oRightKey   := oRight:Leaves[0]
                            var oOurKey     := SELF:Leaves[SELF:Leaves:Count-1]
                            IF ! ValidateLeaves(oOurKey, oRightKey)
                                SELF:Debug("Corruption detected: Last key on our page is not < first key on right page", oRight:PageNo:ToString("X"), oOurKey:Key:ToAscii():Trim(),oRightKey:Key:ToAscii():Trim())
                            ENDIF
                        ENDIF
                    ELSE
                        SELF:Debug("Corruption detected: Could not read Right Page")
                    ENDIF
                ENDIF
            ENDIF

#endif
        INTERNAL METHOD Split(oPageR AS CdxLeafPage, action AS CdxAction) AS CdxAction
            //Debug("New", oPageR:PageNo:ToString("X"))
            VAR leaves      := _leaves
            IF action:Recno > 0
                leaves := (List<CdxLeaf>) SELF:GetLeaves()
                VAR nTrailCount := SELF:_getTrailCount(action:Key)
                LOCAL nDupCount AS BYTE
                VAR nPos := SELF:FindKey(action:Key,action:Recno,action:Key:Length)
                LOCAL leaf AS CdxLeaf
                IF nPos == 0
                    nDupCount := 0
                    leaf := CdxLeaf{action:Recno, action:Key, 0, nTrailCount}
                    leaves:Insert(0, leaf)
#ifdef TESTCDX 
                    IF leaves:Count > 1
                        ValidateLeaves(leaf, leaves[1])
                    ENDIF
#endif
                ELSEIF nPos > 0 .and. nPos < leaves:Count
                    VAR nextLeaf := leaves[nPos]
                    VAR nextKey := nextLeaf:Key
                    nDupCount := SELF:_getDupCount(nextKey, action:Key, nTrailCount)
                    leaf := CdxLeaf{action:Recno, action:Key, nDupCount, nTrailCount}
                    leaves:Insert(nPos, leaf)
#ifdef TESTCDX
                    ValidateLeaves(leaf, nextLeaf)
                    IF nPos > 0
                        ValidateLeaves( leaves[nPos-1], leaf)
                    ENDIF
#endif
                ELSE
                    // append at the end
                    VAR prevLeaf := leaves[leaves:Count-1]
                    VAR prevKey := prevLeaf:Key
                    nDupCount := SELF:_getDupCount(prevkey, action:Key, nTrailCount)
                    leaf := CdxLeaf{action:Recno, action:Key, nDupCount, nTrailCount}
                    leaves:Add(leaf)
#ifdef TESTCDX
                    ValidateLeaves(prevLeaf, leaf)
#endif
                    
                ENDIF
                
            ENDIF
            VAR half := leaves:Count /2
            //? "Writing to page 1", oPageR:PageNo:ToString("X")
            SELF:SetLeaves(leaves, 0, half)
            oPageR:SetLeaves(leaves, half, leaves:Count - half)
            //? "Writing to page 2", oPageR:PageNo:ToString("X")
            SELF:Write()
            oPageR:Write()
            SELF:Tag:AdjustStack(SELF, oPageR, oPageR:NumKeys)
#ifdef TESTCDX
            SELF:Write()
            SELF:ValidateChain()
#endif
            RETURN CdxAction.Ok


        INTERNAL METHOD Delete(nPos AS LONG) AS CdxAction
             // Todo: optimize. We are now expanding the leaves which could be overkill.
            IF nPos >= 0 .AND. nPos < SELF:NumKeys
                _ExpandLeaves(FALSE)
                VAR result := CdxAction.Ok
                IF nPos == SELF:NumKeys -1
                    //SELF:Tag:SetChildToProcess(SELF:PageNo)
                    //Debug(  "updates last key, triggers ChangeParent")
                    result := CdxAction.ChangeParent(SELF)
                ENDIF
                _Leaves:RemoveAt(nPos)
                SELF:NumKeys -= 1
                IF SELF:NumKeys = 0
                    RETURN CdxAction.DeletePage(SELF)
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
                VAR nTrailCount := SELF:_getTrailCount(node:KeyBytes)
                LOCAL nDupCount AS BYTE
                IF nPos == 0
                    nDupCount := 0
                ELSE
                    nDupCount := SELF:_getDupCount(SELF:_leaves[nPos-1]:Key, node:KeyBytes,nTrailCount)
                ENDIF
                _Leaves[nPos] := CdxLeaf{node:Recno, node:KeyBytes, nDupCount, nTrailCount}
                RETURN SELF:Compress()
            ENDIF
            RETURN CdxAction.OutofBounds(SELF)

        INTERNAL METHOD Compress() AS CdxAction
            // Todo: optimize. We are now expanding and compressing the leaves which could be overkill.
            // Also the buffer is saved (because we may discover a 'page full' and we need to restore then
            // maybe we can restore from disk ?
            //Debug("keys", self:NumKeys, "free", self:Freespace)

            IF SELF:NumKeys == 0 .OR. ! SELF:ValidLeaves
                RETURN CdxAction.Ok
            ENDIF
            VAR leaves  := SELF:_Leaves
            //SELF:InitBlank(SELF:Tag)
            SELF:ClearRecordsAndKeys()
            //LOCAL lastKey  := NULL AS BYTE[]

            LOCAL nKey   := 0 AS INT
            LOCAL nStart := CDXLEAF_HEADERLEN + SELF:Freespace AS WORD
            FOREACH Leaf AS CdxLeaf IN leaves
                VAR nDup   := leaf:Dup
                VAR nTrail := leaf:Trail
                IF nKey == 0
                    nDup := 0
		ELSE
		    NOP
		    // The dup and trail values are already stored in the leaves
                ENDIF
                //lastKey  := Leaf:Key

                VAR nBytesToCopy := SELF:KeyLength - nDup - nTrail
                IF SELF:Freespace < (SELF:DataBytes + nBytesToCopy)
                    VAR action := CdxAction.SplitLeaf(SELF, -1, NULL, 0)
                    SELF:_leaves := leaves
                    RETURN SELF:Tag:DoAction(action)
                ENDIF
                SELF:_placeRecno(nKey, Leaf:Recno, SELF:_makeDupTrail(nDup, nTrail))
                IF nBytesToCopy != 0
                    System.Array.Copy(leaf:Key, nDup, buffer, nStart-nBytesToCopy,  nBytesToCopy)
                ENDIF
                SELF:Freespace := SELF:Freespace -  (nBytesToCopy + SELF:DataBytes)
                nStart  -= nBytesToCopy  
                nKey    += 1
            NEXT
            SELF:NumKeys := (WORD) leaves:Count
            SELF:_leaves := leaves
#ifdef TESTCDX
            SELF:ValidateChain()
#endif
            RETURN CdxAction.Ok

 
       PRIVATE METHOD _getTrailCount(data AS BYTE[]) AS BYTE
           LOCAL iLastTrail AS LONG
           iLastTrail  := 0
           FOR VAR i := data:Length -1 DOWNTO 0 
                IF data[i] != TrailByte
                    iLastTrail := i
                    EXIT
                ENDIF
           NEXT
           RETURN (BYTE)  (data:Length - iLastTrail -1)

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

       PRIVATE METHOD _makeDupTrail(dupCount AS BYTE, trailCount AS BYTE) AS WORD
            BEGIN UNCHECKED
                LOCAL shift AS INT
                shift     := SELF:LenShift & 0xFF
                wValue:b1 := (BYTE) dupCount << shift
                wValue:b2 := trailCount
                wValue:wordValue := wValue:wordValue << shift
                RETURN wValue:WordValue
            END UNCHECKED


       PRIVATE METHOD _getDupTrail(wData AS WORD, dupCount OUT BYTE, trailCount OUT BYTE) AS VOID
            LOCAL shift AS INT
            shift := SELF:LenShift & 0xFF
            wValue:wordValue := wData >> shift
            trailCount := wValue:b2
            dupCount   := wValue:b1 >> shift
            RETURN 
            

       PRIVATE METHOD _placeRecno(nIndex AS INT, recno AS LONG, dupLen AS WORD) AS VOID
            BEGIN UNCHECKED
                LOCAL nOffset AS LONG
                nOffSet           := CDXLEAF_HEADERLEN + nIndex * SELF:DataBytes
                liValue:LongValue  := recno
	            buffer[nOffSet]   :=  liValue:b1
                buffer[nOffSet+1] :=  liValue:b2  
                buffer[nOffSet+2] :=  liValue:b3
                IF SELF:DataBytes > 3
                    buffer[nOffSet+3] :=  liValue:b4
                ENDIF
                nOffSet := nOffSet + SELF:DataBytes - 2
                LOCAL wValue := _GetWord(nOffSet) AS WORD
                wValue |= dupLen
                _SetWord(nOffSet , wValue)
            END UNCHECKED
            RETURN


       INTERNAL OVERRIDE METHOD FindKey(key AS BYTE[], recno AS LONG, nSearchLen AS LONG) AS WORD
            SELF:_ExpandLeaves(FALSE)
            IF SELF:NumKeys > 0
                LOCAL nI AS WORD
                FOR nI := 0 TO SELF:Numkeys -1
                    VAR leaf    := _leaves[nI]
                    VAR pageKey := leaf:Key
                    VAR nDiff   := SELF:Tag:__Compare(pageKey, key, nSearchLen)
                    IF nDiff == 0
                        IF leaf:Recno >= recno
                            RETURN nI
                        ENDIF
                    ELSEIF nDiff > 0
                        // insert before this key
                        RETURN nI
                    ENDIF
               NEXT
           ENDIF
           RETURN SELF:NumKeys+1

       INTERNAL METHOD Dump AS STRING
            LOCAL Sb AS stringBuilder
            VAR iLen := SELF:Tag:KeyLength
            sb := stringBuilder{}
            sb:AppendLine("--------------------------")
            sb:AppendLine(String.Format("{0} Page {1:X6}, # of keys: {2}, Free Bytes {3}", SELF:PageType, SELF:PageNo, SELF:NumKeys, SELF:Freespace))
            sb:AppendLine(String.Format("Left page reference {0:X6}", SELF:LeftPtr))
            IF SELF:NumKeys > 0
               SELF:_ExpandLeaves(FALSE)
               VAR nPos := 0
               FOREACH VAR leaf IN _leaves
                    sb:AppendLine(String.Format("Item {0,2}, Record {1,5}, Data {2,3}, Dup {3,3}, Trail {4,3} : {5} ", nPos,  leaf:Recno, iLen-Leaf:Dup-leaf:Trail, Leaf:Dup, leaf:Trail, leaf:KeyText))
                    nPos++
                NEXT
            ENDIF
            sb:AppendLine(String.Format("Right page reference {0:X6}", SELF:RightPtr))
            RETURN sb:ToString()

    END CLASS

END NAMESPACE 


