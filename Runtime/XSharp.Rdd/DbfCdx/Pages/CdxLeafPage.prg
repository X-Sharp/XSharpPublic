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
	INTERNAL CLASS CdxLeafPage INHERIT CdxTreePage IMPLEMENTS ICdxKeyValue
		PROTECTED _keyLen    AS Int32
        PROTECTED _keys      AS BYTE[]
        PROTECTED _recnos    AS Int32[]
        PROTECTED _dupMask   AS BYTE
        PROTECTED _dupBits   AS BYTE
        PROTECTED _trailMask AS BYTE
        PROTECTED _trailBits AS BYTE
        PROTECTED _recnoMask AS Int32
        PROTECTED _recnoBits AS Int32
        //PROTECTED _bytesNeeded AS BYTE
		
	    INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , nPage AS Int32 , buffer AS BYTE[], nKeyLen AS Int32)
            SUPER(bag, nPage, buffer)
            _keyLen     := nKeyLen
            _keys       := NULL
            _dupMask    := SELF:DuplicateMask
            _dupBits    := SELF:DuplicateBits
            _trailMask  := SELF:TrailingMask
            _trailBits  := SELF:TrailingBits
            _recnoMask  := SELF:RecnoMask
            _recnoBits  := SELF:RecordBits
            /*

            LOCAL keyBits := 0 AS INT
            DO WHILE nKeyLen > 0
                keyBits += 1
                nKeyLen >>= 1
            ENDDO
            _bytesNeeded := IIF(keyBits > 12, 5, IIF(keyBits > 8, 4, 3))
            */
            RETURN


#region ICdxKeyValue
        PUBLIC METHOD GetRecno(nPos AS Int32) AS Int32
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            IF _recnos == NULL
                SELF:_ExpandKeys()
            ENDIF
            RETURN _recnos[nPos ]

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
            
            // First key starts at end of page
            nStart := CDXPAGE_SIZE
            _keys    := BYTE[]{SELF:NumKeys * _KeyLen}
            MemSet(_keys, 0, _keys:Length, 32)
            _recnos  := Int32[]{SELF:NumKeys}
            nOffSet := CDXBLKOFFSET_STARTOFDATA
            nStep := SELF:DataBytes
            nLast := SELF:NumKeys-1
            FOR VAR nI := 0 TO nLast
                LOCAL iTemp AS Int32
                nRecno      := _GetLong(nOffSet) 
                _recnos[nI] := (INT) _AND( nRecno , SELF:_recnoMask)
                iTemp   := nRecno >> SELF:_recnoBits 
                nDup    := IIF(nI ==0, 0,  _AND(iTemp , SELF:_dupMask))
                nTrail  := _AND((iTemp >> SELF:_dupBits) , SELF:_trailMask)
                nKey    := _KeyLen - nTrail - nDup
                // Copy to aBytes from pos nDup
                MemCopy(Buffer, nStart - nKey, aBytes, nDup, nKey)
                nStart := nStart - nKey
                MemCopy(aBytes, 0, _keys, nI * _KeyLen, nKey+nDup)
                nOffSet      += nStep
            NEXT
            RETURN TRUE
           
        

#region Properties
		PUBLIC PROPERTY NumKeys  AS WORD	;
			GET _GetWord(CDXBLKOFFSET_NUMKEYS);
			SET _SetWord(CDXBLKOFFSET_NUMKEYS, VALUE), isHot := TRUE

		PROTECTED INTERNAL PROPERTY LeftPtr		AS DWORD			;
			GET _GetDWord(CDXBLKOFFSET_LEFTPTR);
			SET _SetDWord(CDXBLKOFFSET_LEFTPTR, VALUE), isHot := TRUE

		PROTECTED INTERNAL PROPERTY RightPtr		AS DWORD			;
			GET _GetDWord(CDXBLKOFFSET_RIGHTPTR);
			SET _SetDWord(CDXBLKOFFSET_RIGHTPTR, VALUE), isHot := TRUE
			
		PROTECTED INTERNAL PROPERTY Freespace		AS WORD			;
			GET _GetWord(CDXBLKOFFSET_FREESPACE);
			SET _SetWord(CDXBLKOFFSET_FREESPACE, VALUE), isHot := TRUE
			
		PROTECTED INTERNAL PROPERTY RecnoMask 	AS DWORD			;
			GET _GetDWord(CDXBLKOFFSET_RECNOMASK);
			SET _SetDWord(CDXBLKOFFSET_RECNOMASK, VALUE), isHot := TRUE
			
		PROTECTED INTERNAL PROPERTY DuplicateMask	AS BYTE			;
			GET Buffer[CDXBLKOFFSET_DUPMASK];
			SET Buffer[CDXBLKOFFSET_DUPMASK] := VALUE, isHot := TRUE

		PROTECTED INTERNAL PROPERTY TrailingMask	AS BYTE			;
			GET Buffer[CDXBLKOFFSET_TRAILMASK];
			SET Buffer[CDXBLKOFFSET_TRAILMASK] := VALUE, isHot := TRUE
			
		PROTECTED INTERNAL PROPERTY RecordBits	AS BYTE			;
			GET Buffer[CDXBLKOFFSET_RECNUMBITS];
			SET Buffer[CDXBLKOFFSET_RECNUMBITS] := VALUE, isHot := TRUE
			
		PROTECTED INTERNAL PROPERTY DuplicateBits	AS BYTE			;
			GET Buffer[CDXBLKOFFSET_DUPCOUNTBITS];
			SET Buffer[CDXBLKOFFSET_DUPCOUNTBITS] := VALUE, isHot := TRUE
			
		PROTECTED INTERNAL PROPERTY TrailingBits	AS BYTE			;
			GET Buffer[CDXBLKOFFSET_TRAILINGBITS];
			SET Buffer[CDXBLKOFFSET_TRAILINGBITS] := VALUE, isHot := TRUE
			
		PROTECTED INTERNAL PROPERTY DataBytes	AS BYTE			;
			GET Buffer[CDXBLKOFFSET_DATABYTES];
			SET Buffer[CDXBLKOFFSET_DATABYTES] := VALUE, isHot := TRUE
#endregion			
				
		PRIVATE CONST CDXBLKOFFSET_NUMKEYS		:= 2	AS WORD // WORD
		PRIVATE CONST CDXBLKOFFSET_LEFTPTR		:= 4	AS WORD // LONGINT
		PRIVATE CONST CDXBLKOFFSET_RIGHTPTR 	:= 8	AS WORD // LONGINT
		PRIVATE CONST CDXBLKOFFSET_FREESPACE	:= 12	AS WORD // WORD		: Free space in this key
		PRIVATE CONST CDXBLKOFFSET_RECNOMASK	:= 14	AS WORD // LONGINT	: Bit mask for record number
		PRIVATE CONST CDXBLKOFFSET_DUPMASK		:= 18	AS WORD // Bit mask for duplicate byte count
		PRIVATE CONST CDXBLKOFFSET_TRAILMASK    := 19	AS WORD // Bit mask for trailing byte count
		PRIVATE CONST CDXBLKOFFSET_RECNUMBITS	:= 20	AS WORD // Number of bits used for record number
		PRIVATE CONST CDXBLKOFFSET_DUPCOUNTBITS := 21	AS WORD // Number of bits used for duplicate count
		PRIVATE CONST CDXBLKOFFSET_TRAILINGBITS := 22	AS WORD // Number of bits used for trailing count
		PRIVATE CONST CDXBLKOFFSET_DATABYTES	:= 23	AS WORD // Bytes needed for recno+dups+trailing (sum of 20,21 & 22)
		PRIVATE CONST CDXBLKOFFSET_STARTOFDATA	:= 24	AS WORD 	
			

    END CLASS
    INTERNAL ENUM CdxNodeAttribute AS WORD
        MEMBER Branch  := 0
        MEMBER Root    := 1
        MEMBER Leaf    := 2
        MEMBER TagList := 3
        MEMBER Unused  := 0xFF
    END ENUM



END NAMESPACE 


