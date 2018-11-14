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
using System.Diagnostics

BEGIN NAMESPACE XSharp.RDD.CDX
	/// <summary>
	/// CdxLeaf page. this class maps the Leaf page from the file in memory
    /// Manipulating the page is implemented in the CdxTag class
	/// </summary>
	INTERNAL CLASS CdxLeafPage INHERIT CdxTreePage IMPLEMENTS ICdxKeyValue
		PROTECTED _keyLen as Int32
        PROTECTED _keys     as byte[]
        PROTECTED _recnos   as Int32[]
		
	    PROTECTED INTERNAL CONSTRUCTOR( fileHandle AS IntPtr , nPage as Int32 , nKeyLen as Int32)
            SUPER(fileHandle, nPage)
            _keyLen     := nKeyLen
            _keys := null

#region ICdxKeyValue
        METHOD GetRecno(nPos as Int32) as Int32
            Debug.Assert(nPos >= 0 .and. nPos < Self:NumKeys)
            if _recnos == null
                self:_ExpandKeys()
            endif
            return _recnos[nPos ]
                

        Method GetKey(nPos as Int32) as BYTE[]
            Debug.Assert(nPos >= 0 .and. nPos < Self:NumKeys)
            if _keys == null
                Self:_ExpandKeys()
            endif
            local nStart := nPos * _KeyLen as int
            return _GetBytes(_keys, nStart, _KeyLen)
#endregion

         PRIVATE METHOD _ExpandKeys()  as LOGIC
            local nOffSet   as Int32
            local aBytes := BYTE[]{_KeyLen+1} AS BYTE[]
            local nDup, nTrail as byte
            local nRecno    as Int32
            local nKey      as Int32
            local nStart    as Int32
            // First key starts at end of page
            nStart := CDXPAGE_SIZE
            _keys    := Byte[]{self:NumKeys * _KeyLen}
            MemSet(_keys, 0, _keys:Length, 32)
            _recnos  := Int32[]{self:NumKeys}
            FOR VAR nI := 0 to SELF:NumKeys-1
                nOffSet     := CDXBLKOFFSET_STARTOFDATA + nI * SELF:DataBytes
                nRecno      := _GetLong(nOffSet)
                _recnos[nI] := _AND( nRecno , RecnoMask) 
                nTrail  := nDup := (byte) (nRecno >> self:RecordBits)
                nTrail  := nTrail >> self:DuplicateBits
                nTrail  := nTrail & Self:TrailingMask
                // Shift Left and Right to remove the trailbits and keep the duplicates
                nDup    := nDup << self:TrailingBits
                nDup    := nDup >> self:TrailingBits
                nKey    := _KeyLen - nTrail - nDup
                // Copy to aBytes from pos nDup
                MemCopy(Buffer, nStart - nKey, aBytes, nDup, nKey)
                nStart := nStart - nKey
                MemCopy(aBytes, 0, _keys, nI * _KeyLen, nKey+nDup)
            NEXT
            return true
           
        

#region Properties
		PROTECTED INTERNAL PROPERTY NumKeys  AS WORD	;
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
			

        PRIVATE STATIC METHOD SwapEndian(value as Int32) as Int32
            VAR b1 := (value >> 0)  & 0xFF
            VAR b2 := (value >> 8)  & 0xFF
            VAR b3 := (value >> 16)  & 0xFF
            VAR b4 := (value >> 24)  & 0xFF
            return b1 << 24 | b2 << 16 | b3 << 8 | b4 << 0

    END CLASS
    INTERNAL ENUM CdxNodeAttribute AS WORD
        MEMBER Branch  := 0
        MEMBER Root    := 1
        MEMBER Leaf    := 2
        MEMBER TagList := 3
        MEMBER Unused  := 0xFF
    END ENUM



END NAMESPACE 

