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
        INTERNAL Recno AS LONG
        INTERNAL Key   AS BYTE[]
        INTERNAL PROPERTY KeyText AS STRING GET SELF:Key:ToAscii()
        CONSTRUCTOR (nRecno AS LONG, bKey AS BYTE[])
            SELF:Recno := nRecno
            SELF:Key   := (BYTE[]) bKey:Clone()
            RETURN
    END STRUCTURE
	/// <summary>
	/// CdxLeaf page. this class maps the Leaf page from the file in memory
    /// Manipulating the page is implemented in the CdxTag class
	/// </summary>
	INTERNAL CLASS CdxLeafPage INHERIT CdxTreePage

#region constants				
		PRIVATE CONST CDXLEAF_NUMKEYS		:= 2	AS WORD // 2 WORD
		PRIVATE CONST CDXLEAF_LEFTPTR		:= 4	AS WORD // 4 LONGINT
		PRIVATE CONST CDXLEAF_RIGHTPTR 	    := 8	AS WORD // 4 LONGINT
		PRIVATE CONST CDXLEAF_FREESPACE	    := 12	AS WORD // 2 WORD		: Free space in this key
		PRIVATE CONST CDXLEAF_RECNOMASK	    := 14	AS WORD // 4 LONGINT	: Bit mask for record number
		PRIVATE CONST CDXLEAF_DUPMASK		:= 18	AS WORD // 1 Bit mask for duplicate byte count
		PRIVATE CONST CDXLEAF_TRAILMASK     := 19	AS WORD // 1 Bit mask for trailing byte count
		PRIVATE CONST CDXLEAF_RECNUMBITS	:= 20	AS WORD // 1 Number of bits used for record number
		PRIVATE CONST CDXLEAF_DUPCOUNTBITS  := 21	AS WORD // 1 Number of bits used for duplicate count
		PRIVATE CONST CDXLEAF_TRAILINGBITS  := 22	AS WORD // 1 Number of bits used for trailing count
		PRIVATE CONST CDXLEAF_DATABYTES	    := 23	AS WORD // 1 Bytes needed for recno+dups+trailing (sum of 20,21 & 22)
        PRIVATE CONST CDXLEAF_MASKLEN       := 10   AS WORD // length of the info that gets copied to new brother pages (recnomask .. DataBytes)
        PRIVATE CONST CDXLEAF_HEADERLEN     := 24   AS WORD // length of the page header
        PRIVATE CONST CDXLEAF_BYTESFREE     := 488  AS WORD // 512 - 24
#endregion

		PROTECTED _keyLen    AS WORD
        PROTECTED _lenShift  AS INT
        PROTECTED _leaves    AS List<CdxLeaf>
        PROTECTED _prevData  AS BYTE[]
        PROTECTED _bTrail    AS BYTE
        INTERNAL CONSTRUCTOR( bag AS CdxOrderBag, page AS CdxPage)
            SELF(bag, page:PageNo, page:Buffer, (WORD) IIF(page:Tag != NULL, page:Tag:KeyLength,0))

	    INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , nPage AS Int32 , buffer AS BYTE[], nKeyLen AS WORD)
            SUPER(bag, nPage, buffer)
            _keyLen     := nKeyLen
            _leaves     := NULL
            _prevData   := BYTE[]{nKeyLen}
            _bTrail     := 0

            RETURN
        INTERNAL METHOD InitBlank(oTag AS CdxTag) AS VOID
            SELF:Tag := oTag
            SELF:Initialize(_keyLen)
            IF Tag != NULL
                _bTrail := (BYTE) (IIF(Tag:KeyType == __UsualType.String, 32, 0) )
            ELSE
                _bTrail := 32
            ENDIF

        INTERNAL VIRTUAL METHOD Initialize(keyLength AS WORD) AS VOID
            SELF:PageType   := CdxPageType.Leaf
            SELF:_ClearRecordsAndKeys()
            _keyLen             := keyLength
            VAR bits            := CdxHelpers.GetBits(keyLength)
            // base dupCountMask, trailCountMNask, numbitsRecno and other info are based on keylength 
            SELF:DataBytes      := IIF (bits > 12, 5, IIF( bits > 8, 4, 3))
            SELF:RecordBits     := (SELF:DataBytes << 3) - (bits << 1)
            SELF:DuplicateBits  := SELF:TrailingBits  := bits
            SELF:TrailingMask   := SELF:DuplicateMask := (BYTE) (( 1 << bits  ) - 1)
            SELF:RecnoMask      := (DWORD) (1 << SELF:RecordBits) -1
            SELF:_lenShift      := (keyLength << 8 ) | (8 - SELF:DuplicateBits)
            IF SELF:Tag != NULL
                VAR numRecs     := SELF:Tag:RDD:RecCount
                DO WHILE _AND(numRecs , SELF:RecnoMask) != numRecs
                    SELF:DataBytes  += 1
                    SELF:RecordBits += 8
                    SELF:RecnoMask  := (DWORD) (1 << SELF:RecordBits) -1
                ENDDO
            ENDIF

            RETURN


        PRIVATE METHOD _ClearRecordsAndKeys() AS VOID
            SELF:Freespace  := CDXLEAF_BYTESFREE
            SELF:NumKeys    := 0
            SELF:_leaves    := NULL
            //MemSet(SELF:Buffer, CDXLEAF_HEADERLEN, CDXLEAF_BYTESFREE, 0)
            RETURN

        PROTECTED INTERNAL VIRTUAL METHOD Read() AS LOGIC
			VAR Ok := SUPER:Read()
            Debug.Assert (SELF:PageType:HasFlag(CdxPageType.Leaf))
            RETURN ok

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
            LOCAL nDup, nTrail AS BYTE
            LOCAL nKey      AS Int32
            LOCAL nStart    AS Int32
            LOCAL nStep     AS Int32
            LOCAL nLast     AS Int32
            LOCAL trailchar  AS BYTE
            VAR dupMask    := SELF:DuplicateMask
            VAR dupBits    := SELF:DuplicateBits
            VAR trailMask  := SELF:TrailingMask
            VAR recnoBits  := SELF:RecordBits

            
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
                    LOCAL iTemp AS Int32
                    nRecno  := _GetLong(nOffSet)
                    IF SELF:DataBytes <= 4
                        iTemp := nRecno >> recnoBits
                    ELSE
                        iTemp   := _GetWord(nOffSet + recnoBits/8)
                    ENDIF
                    nDup    := IIF(nI ==0, 0,  _AND(iTemp , dupMask))
                    nTrail  := (BYTE) _AND((iTemp >> dupBits) , trailMask)
                    nKey    := _KeyLen - nTrail - nDup
                    IF nTrail > 0
                        MemSet(aBytes, _KeyLen - nTrail, nTrail, trailChar)
                    ENDIF
                    // Copy to aBytes from pos nDup
                    System.Array.Copy(Buffer, nStart - nKey, aBytes, nDup, nKey)
                    nStart := nStart - nKey
                    nRecno      := _AND( nRecno , SELF:RecnoMask)
                    _leaves.Add( CdxLeaf{ nRecno, aBytes})
                    nOffSet      += nStep
                NEXT
            ENDIF
            RETURN TRUE

 

#region Properties that map to the buffer 

		PUBLIC PROPERTY NumKeys  AS WORD	;
			GET _GetWord(CDXLEAF_NUMKEYS);
			SET _SetWord(CDXLEAF_NUMKEYS, VALUE), isHot := TRUE

		INTERNAL PROPERTY LeftPtr		AS Int32			;
			GET _GetLong(CDXLEAF_LEFTPTR);
			SET _SetLong(CDXLEAF_LEFTPTR, VALUE), isHot := TRUE

		INTERNAL PROPERTY RightPtr		AS Int32			;
			GET _GetLong(CDXLEAF_RIGHTPTR);
			SET _SetLong(CDXLEAF_RIGHTPTR, VALUE), isHot := TRUE
			
		PROTECTED INTERNAL PROPERTY Freespace		AS WORD			;
			GET _GetWord(CDXLEAF_FREESPACE);
			SET _SetWord(CDXLEAF_FREESPACE, VALUE), isHot := TRUE
			
		PROTECTED INTERNAL PROPERTY RecnoMask 	AS DWORD			;
			GET _GetDWord(CDXLEAF_RECNOMASK);
			SET _SetDWord(CDXLEAF_RECNOMASK, VALUE), isHot := TRUE
			
		PROTECTED INTERNAL PROPERTY DuplicateMask	AS BYTE			;
			GET Buffer[CDXLEAF_DUPMASK];
			SET Buffer[CDXLEAF_DUPMASK] := VALUE, isHot := TRUE

		PROTECTED INTERNAL PROPERTY TrailingMask	AS BYTE			;
			GET Buffer[CDXLEAF_TRAILMASK];
			SET Buffer[CDXLEAF_TRAILMASK] := VALUE, isHot := TRUE
			
		PROTECTED INTERNAL PROPERTY RecordBits	AS BYTE			;
			GET Buffer[CDXLEAF_RECNUMBITS];
			SET Buffer[CDXLEAF_RECNUMBITS] := VALUE, isHot := TRUE
			
		PROTECTED INTERNAL PROPERTY DuplicateBits	AS BYTE			;
			GET Buffer[CDXLEAF_DUPCOUNTBITS];
			SET Buffer[CDXLEAF_DUPCOUNTBITS] := VALUE, isHot := TRUE
			
		PROTECTED INTERNAL PROPERTY TrailingBits	AS BYTE			;
			GET Buffer[CDXLEAF_TRAILINGBITS];
			SET Buffer[CDXLEAF_TRAILINGBITS] := VALUE, isHot := TRUE
			
		PROTECTED INTERNAL PROPERTY DataBytes	AS BYTE			;
			GET Buffer[CDXLEAF_DATABYTES];
			SET Buffer[CDXLEAF_DATABYTES] := VALUE, isHot := TRUE
#endregion			

#region Other properties
        INTERNAL PROPERTY KeyLength AS WORD GET _keyLen
            
        // Retrieve an index node in the current Page, at the specified position
        // return CdxLeagPageNode which always returns 0 for the ChildPage
        INTERNAL OVERRIDE PROPERTY SELF[ index AS WORD ] AS CdxPageNode
            GET
                RETURN CdxLeafPageNode{ SELF:KeyLength, SELF, index }
            END GET
        END PROPERTY

        INTERNAL PROPERTY LastNode AS CdxPageNode GET SELF[SELF:NumKeys-1]
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

        INTERNAL METHOD ExpandRecnos() AS CdxAction
            LOCAL aRecNos AS INT[]
            LOCAL nCount := NumKeys AS INT
            LOCAL aKeys   AS BYTE[][]
            IF SELF:Freespace < nCount
                // we need at least nCount extra bytes because we are adding 1 byte for every recno
                // Not enough space. We have to add another leaf page
                // This will not work for the TagList but should never happen with the Tag List
                Debug.Assert(! SELF IS CdxTagList)
                RETURN CdxAction.AddLeaf(SELF,0,NULL)
            ENDIF
            aRecNos := INT[]{nCount}
            aKeys    := BYTE[][]{nCount}            
            FOR VAR i := 0 TO nCount-1
                aRecNos[i] := SELF:GetRecno(i)
                aKeys[i]   := SELF:GetKey(i)
            NEXT
            SELF:DataBytes  += 1
            SELF:RecordBits += 8
            SELF:RecnoMask  := (DWORD) (1 << SELF:RecordBits) -1
            SELF:_ClearRecordsAndKeys()
            FOR VAR i := 0 TO nCount-1
                SELF:Add(aRecNos[i], aKeys[i])
            NEXT
            RETURN CdxAction.OK


        // This method assumes keys are added in the right order.
        // It assymes that the _prevData array contains the key from the previous record
        INTERNAL METHOD Add(recno AS LONG, data AS BYTE[]) AS CdxAction
            LOCAL nTrailCount AS BYTE
            LOCAL nDupCount   AS BYTE
            IF _AND( recno, SELF:RecnoMask) != recno
                RETURN CdxAction.ExpandRecnos(SELF)
            ENDIF
            nTrailCount := _getTrailCount(data)
            IF SELF:NumKeys == 0
                nDupCount := 0
            ELSE
                nDupCount := _getDupCount(data, nTrailCount)
            ENDIF
            LOCAL nBytesToCopy := SELF:_keyLen - nDupCount - nTrailCount AS WORD
            IF SELF:Freespace < (nBytesToCopy+SELF:DataBytes)
                RETURN CdxAction.AddLeaf(SELF, recno, data)
            ENDIF
            System.Array.Copy(data, _prevData, _KeyLen)
            LOCAL nHeaderLen := CDXLEAF_HEADERLEN + SELF:NumKeys * SELF:DataBytes AS INT
            LOCAL nStart := nHeaderLen + SELF:Freespace - nBytesToCopy  AS INT
            SELF:_placeRecno(SELF:NumKeys, recno, SELF:_makeDupTrail(nDupCount, nTrailCount))
            System.Array.Copy(data, nDupCount, buffer, nStart,  nBytesToCopy)
            SELF:Freespace := SELF:Freespace -  nBytesToCopy - SELF:DataBytes
            IF SELF:ValidLeaves
                _leaves:Add( CdxLeaf{recno, data})
            ENDIF
            SELF:NumKeys += 1
            RETURN CdxAction.Ok


        INTERNAL METHOD Insert(nPos AS LONG, recno as LONG, key as BYTE[]) AS CdxAction

            // A quick calculation if we have enough room, ignoring the duplicate count
            LOCAL nTrailCount AS LONG
            nTrailCount := _getTrailCount(key)
            VAR nBytesNeeded := SELF:DataBytes + SELF:_keyLen - nTrailCount
            IF SELF:Freespace < nBytesNeeded
                RETURN CdxAction.AddLeaf(SELF, recno,key)
            ENDIF
            // Todo: optimize. We are now expanding the leaves which could be overkill.
            _ExpandLeaves(FALSE)
            IF nPos < 0 .OR. nPos > _Leaves:Count
                RETURN CdxAction.OutofBounds(SELF)
            ENDIF
            IF nPos >= _Leaves:Count
                _Leaves:Add(CdxLeaf{recno, Key})
            ELSE // IF nPos >= 0 .AND. nPos < _Leaves:Count
                _Leaves:Insert(nPos, CdxLeaf{Recno, Key})
            ENDIF
            VAR result := CdxAction.Ok
            IF nPos == SELF:NumKeys
                result := CdxAction.ChangeParent(SELF)
            ENDIF
            SELF:NumKeys += 1
            var compResult := SELF:Compress()
            if CompResult:Type != CdxActionType.Ok
                RETURN CompResult
            ENDIF
            RETURN result
            

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
                result :=  SELF:Compress()
                RETURN result
            ENDIF
            RETURN CdxAction.OutofBounds(SELF)

        INTERNAL METHOD Replace(nPos AS LONG, node AS CdxNode) AS CdxAction
            // Todo: optimize. We are now expanding the leaves which could be overkill.
            IF nPos >= 0 .AND. nPos < SELF:NumKeys
                _ExpandLeaves(FALSE)
                _Leaves[nPos] := CdxLeaf{node:Recno, node:KeyBytes}
                RETURN SELF:Compress()
            ENDIF
            RETURN CdxAction.OutofBounds(SELF)

        INTERNAL METHOD Compress() AS CdxAction
            // Todo: optimize. We are now expanding and compressing the leaves which could be overkill.
            // Also the buffer is saved (because we may discover a 'page full' and we need to restore then
            // maybe we can restore from disk ?
            // finally: do we have to call Write? Or is it enough to set the page as dirty
            IF !SELF:ValidLeaves
                RETURN CdxAction.Ok
            ENDIF
            VAR copy := (BYTE[]) SELF:Buffer:Clone()
            VAR leaves := SELF:_Leaves
            SELF:InitBlank(SELF:Tag)
            SELF:_leaves := NULL
            FOREACH VAR Leaf IN leaves
                VAR result := SELF:Add(leaf:Recno, leaf:Key)
                IF result.Type != CdxActionType.Ok
                    Array.Copy(copy, SELF:Buffer, SELF:Buffer:Length)
                    RETURN result
                ENDIF
            NEXT
            SELF:Write()
            RETURN CdxAction.Ok

 
       PRIVATE METHOD _getTrailCount(data AS BYTE[]) AS BYTE
           LOCAL iLastTrail AS LONG
           LOCAL bTrail := _bTrail as BYTE
           iLastTrail  := 0
           FOR VAR i := data:Length -1 DOWNTO 0 
                IF data[i] != bTrail
                    iLastTrail := i
                    EXIT
                ENDIF
           NEXT
           RETURN (BYTE)  (data:Length - iLastTrail -1)

        PRIVATE METHOD _getDupCount(data AS BYTE[], trailCount AS LONG) AS BYTE
           LOCAL last := data:Length - trailCount  AS LONG
           LOCAL dup AS LONG
           FOR dup := 0 UPTO last -1 
              IF data[dup] != _prevData[dup]
                 EXIT
              ENDIF
           NEXT
           RETURN (BYTE) dup

       PRIVATE METHOD _makeDupTrail(dupCount AS BYTE, trailCount AS BYTE) AS WORD
            LOCAL w     := wordStruct{} AS wordStruct
            LOCAL shift AS INT
            shift := SELF:_lenShift & 0xFF
            w:b1 := (BYTE) dupCount << shift
            w:b2 := trailCount
            RETURN w:wordValue << shift

       PRIVATE METHOD _placeRecno(nIndex AS INT, recno AS LONG, dupLen AS WORD) AS VOID
            LOCAL nOffset AS LONG
            LOCAL nValue := LongStruct{} AS LongStruct
            nOffSet     := CDXLEAF_HEADERLEN + nIndex * SELF:DataBytes
            nValue:LongValue := recno
	        buffer[nOffSet]   :=  nValue:b1
            buffer[nOffSet+1] :=  nValue:b2  
            buffer[nOffSet+2] :=  nValue:b3
            IF SELF:DataBytes > 3
               buffer[nOffSet+3] :=  nValue:b4
            ENDIF
            nOffSet := nOffSet + SELF:DataBytes - 2
            LOCAL wValue := _GetWord(nOffSet) AS WORD
            wValue |= dupLen
            _SetWord(nOffSet , wValue)
            RETURN

       INTERNAL METHOD Dump AS STRING
            LOCAL Sb AS stringBuilder
            LOCAL i as WORD
            sb := stringBuilder{}
            VAR item := SELF[0]
            sb:AppendLine("--------------------------")
            sb:AppendLine(String.Format("{0} Page {1:X6}, # of keys: {2}", SELF:PageType, SELF:PageNo, SELF:NumKeys))
            sb:AppendLine(String.Format("Left page reference {0:X6}", SELF:LeftPtr))
            IF SELF:NumKeys > 0
                FOR  i := 0 TO SELF:NumKeys-1
                    item:Pos := i
                    sb:AppendLine(String.Format("Item {0,2}, Record {1,5} : {2} ", i,  item:Recno, item:KeyText))
                NEXT
            ENDIF
            sb:AppendLine(String.Format("Right page reference {0:X6}", SELF:RightPtr))
            RETURN sb:ToString()

    END CLASS

END NAMESPACE 


