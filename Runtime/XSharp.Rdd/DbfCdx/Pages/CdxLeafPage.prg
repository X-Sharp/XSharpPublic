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
	/// <summary>
	/// CdxLeaf page. this class maps the Leaf page from the file in memory
    /// Manipulating the page is implemented in the CdxTag class
	/// </summary>
	INTERNAL CLASS CdxLeafPage INHERIT CdxTreePage 
		PROTECTED _keyLen    AS WORD
        PROTECTED _keys      AS BYTE[]

        INTERNAL PROPERTY KeyLength AS WORD GET _keyLen


        INTERNAL CONSTRUCTOR( bag AS CdxOrderBag, page AS CdxPage)
            SELF(bag, page:PageNo, page:Buffer, page:KeyLength)

	    INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , nPage AS Int32 , buffer AS BYTE[], nKeyLen AS WORD)
            SUPER(bag, nPage, buffer)
            _keyLen     := nKeyLen
            _keys       := NULL
            //Debug.Assert (SELF:PageType:HasFlag(CdxPageType.Leaf))

            //SELF:_ExpandKeys()
            /*

            LOCAL keyBits := 0 AS INT
            DO WHILE nKeyLen > 0
                keyBits += 1
                nKeyLen >>= 1
            ENDDO
            _bytesNeeded := IIF(keyBits > 12, 5, IIF(keyBits > 8, 4, 3))
            */
            RETURN

        PROTECTED INTERNAL VIRTUAL METHOD Read() AS LOGIC
			VAR Ok := SUPER:Read()
            Debug.Assert (SELF:PageType:HasFlag(CdxPageType.Leaf))
            RETURN ok

#region ICdxKeyValue
        PUBLIC METHOD GetRecno(nPos AS Int32) AS Int32
            LOCAL nOffSet   AS Int32
            LOCAL nRecno    AS Int32
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nOffSet     := CDXLEAF_STARTOFDATA + nPos * SELF:DataBytes
            nRecno      := _GetLong(nOffSet) 
            nRecno      := _AND( nRecno , SELF:RecnoMask)
            RETURN nRecno

        PUBLIC METHOD GetChildPage(nPos AS Int32) AS Int32
            RETURN 0

        PUBLIC METHOD GetKey(nPos AS Int32) AS BYTE[]
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            IF _keys == NULL
                SELF:_ExpandKeys()
            ENDIF
            LOCAL nStart := nPos * _KeyLen AS INT
            RETURN _GetBytes(_keys, nStart, _KeyLen)
#endregion

         // For Debugging we calculate all Recnos and KeyBytes
         // Later we will remove this
         PRIVATE METHOD _ExpandKeys()  AS LOGIC
            LOCAL nOffSet   AS Int32
            LOCAL aBytes := BYTE[]{_KeyLen+1} AS BYTE[]
            LOCAL nRecno    AS Int32
            LOCAL nDup, nTrail AS BYTE
            LOCAL nKey      AS Int32
            LOCAL nStart    AS Int32
            LOCAL nStep     AS Int32
            LOCAL nLast     AS Int32
            VAR dupMask    := SELF:DuplicateMask
            VAR dupBits    := SELF:DuplicateBits
            VAR trailMask  := SELF:TrailingMask
            VAR recnoBits  := SELF:RecordBits
            
            // First key starts at end of page
            nStart := CDXPAGE_SIZE
            _keys    := BYTE[]{SELF:NumKeys * _KeyLen}
            IF SELF:Tag != NULL .AND. SELF:Tag:KeyType != __UsualType.String
                MemSet(_keys, 0, _keys:Length, 0)
            ELSE
                MemSet(_keys, 0, _keys:Length, 32)
            ENDIF
            nOffSet := CDXLEAF_STARTOFDATA
            nStep := SELF:DataBytes
            nLast := SELF:NumKeys-1
            FOR VAR nI := 0 TO nLast
                LOCAL iTemp AS Int32
                nRecno  := _GetLong(nOffSet) 
                iTemp   := nRecno >> recnoBits 
                nDup    := IIF(nI ==0, 0,  _AND(iTemp , dupMask))
                nTrail  := (BYTE) _AND((iTemp >> dupBits) , trailMask)
                nKey    := _KeyLen - nTrail - nDup
                // Copy to aBytes from pos nDup
                MemCopy(Buffer, nStart - nKey, aBytes, nDup, nKey)
                nStart := nStart - nKey
                MemCopy(aBytes, 0, _keys, nI * _KeyLen, nKey+nDup)
                nOffSet      += nStep
            NEXT
            //? "Leaf Page", SELF:PageNo:ToString("X"), SELF:NumKeys,"Startswith", _bag:_oRDD:_Encoding:GetString(GetKey(0),0,_keyLen)
            //? "Leaf Page", SELF:PageNo:ToString("X"), SELF:NumKeys,"Endswith", _bag:_oRDD:_Encoding:GetString(GetKey(SELF:NumKeys-1),0,_keyLen)
            RETURN TRUE
           
        

#region Properties
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
		PRIVATE CONST CDXLEAF_STARTOFDATA	:= 24	AS WORD // 1 area where the keys and values are stored
        PRIVATE CONST CDXLEAF_HEADERLEN     := 24   AS WORD // length of the page header
        PRIVATE CONST CDXLEAF_MASKLEN       := 10   AS WORD // length of the info that gets copied to new brother pages (recnomask .. DataBytes)
        PRIVATE CONST CDXLEAF_BYTESFREE     := 488  AS WORD // 512 - 24

        METHOD Dump AS STRING
            LOCAL Sb AS stringBuilder
            sb := stringBuilder{}
            VAR item := SELF[0]
            sb:AppendLine("--------------------------")
            sb:AppendLine(String.Format("{0} Page {1:X6}, # of keys: {2}", SELF:PageType, SELF:PageNo, SELF:NumKeys))
            sb:AppendLine(String.Format("Left page reference {0:X6}", SELF:LeftPtr))
            FOR VAR i := 0 TO SELF:NumKeys-1
                item:Pos := i
                sb:AppendLine(String.Format("Item {0,2}, Record {1,5} : {2} ", i,  item:Recno, item:KeyText))
            NEXT
            sb:AppendLine(String.Format("Right page reference {0:X6}", SELF:RightPtr))
            RETURN sb:ToString()
           
        // Retrieve an index node in the current Page, at the specified position
        // return CdxLeagPageNode which always returns 0 for the ChildPage
        INTERNAL OVERRIDE PROPERTY SELF[ index AS LONG ] AS CdxPageNode
            GET
                RETURN CdxLeafPageNode{ SELF:KeyLength, SELF, index }
            END GET
        END PROPERTY


        INTERNAL VIRTUAL METHOD Initialize(keyLength AS INT, numRecs AS INT) AS VOID
            SELF:PageType   := CdxPageType.TagList
            SELF:NumKeys    := 0
            SELF:LeftPtr    := SELF:RightPtr   := -1
            SELF:Freespace  := CDXLEAF_BYTESFREE
            // base dupCountMask, trailCountMNask, numbitsRecno and other info are based on keylength and NumRecs
            RETURN
            

    END CLASS

END NAMESPACE 


