// CdxBlock.prg
// Created by    : fabri
// Creation Date : 10/25/2018 10:43:18 PM
// Created for   : 
// WorkStation   : FABPORTABLE

USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Runtime.CompilerServices

BEGIN NAMESPACE XSharp.RDD.CDX

	/// <summary>
	/// The CdxBlock class.
	/// </summary>
	INTERNAL CLASS CdxBlock
		PRIVATE _hFile AS IntPtr

		PRIVATE Buffer   AS BYTE[]
		// Hot ?  => Header has changed ?
		INTERNAL isHot	AS LOGIC
		
		
		
		INTERNAL METHOD Read() AS LOGIC
			LOCAL isOk AS LOGIC
			// Move to top
			FSeek3( SELF:_hFile, 0, SeekOrigin.Begin )
			// Read Buffer
			isOk := ( FRead3(SELF:_hFile, SELF:Buffer, CDXBLOCK_SIZE) == CDXBLOCK_SIZE )
			//
			RETURN isOk
			
		INTERNAL METHOD Write() AS LOGIC
			LOCAL isOk AS LOGIC
			// Move to top
			FSeek3( SELF:_hFile, 0, SeekOrigin.Begin )
			// Write Buffer
			isOk := ( FWrite3(SELF:_hFile, SELF:Buffer, CDXBLOCK_SIZE) == CDXBLOCK_SIZE )
			//
			RETURN isOk
			
			
			
		INTERNAL CONSTRUCTOR( fileHandle AS IntPtr )
			//
			SELF:_hFile := fileHandle
			SELF:Buffer := BYTE[]{CDXBLOCK_SIZE}
			SELF:isHot  := FALSE
			
			RETURN
			
			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PRIVATE METHOD _GetWord(nOffSet AS INT) AS WORD
				RETURN BitConverter.ToUInt16(Buffer, nOffset)
				
			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PRIVATE METHOD _SetWord(nOffSet AS INT, wValue AS WORD) AS VOID
				Array.Copy(BitConverter.GetBytes(wValue),0, Buffer, nOffSet, SIZEOF(WORD))
				isHot := TRUE
				
			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PRIVATE METHOD _GetDWord(nOffSet AS INT) AS DWORD
				RETURN BitConverter.ToUInt32(Buffer, nOffset)
				
			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PRIVATE METHOD _SetDWord(nOffSet AS INT, dwValue AS DWORD) AS VOID
				Array.Copy(BitConverter.GetBytes(dwValue),0, Buffer, nOffSet, SIZEOF(DWORD))
				isHot := TRUE
				
			INTERNAL PROPERTY NodeAttribute  AS WORD	;
			GET _GetWord(CDXBLKOFFSET_NODEATTR);
			SET _SetWord(CDXBLKOFFSET_NODEATTR, VALUE), isHot := TRUE

			INTERNAL PROPERTY Entry  AS WORD	;
			GET _GetWord(CDXBLKOFFSET_ENTRY);
			SET _SetWord(CDXBLKOFFSET_ENTRY, VALUE), isHot := TRUE

			INTERNAL PROPERTY LeftPtr		AS DWORD			;
			GET _GetDWord(CDXBLKOFFSET_LEFTPTR);
			SET _SetDWord(CDXBLKOFFSET_LEFTPTR, VALUE), isHot := TRUE

			INTERNAL PROPERTY RightPtr		AS DWORD			;
			GET _GetDWord(CDXBLKOFFSET_RIGHTPTR);
			SET _SetDWord(CDXBLKOFFSET_RIGHTPTR, VALUE), isHot := TRUE
			
			INTERNAL PROPERTY Freespace		AS WORD			;
			GET _GetWord(CDXBLKOFFSET_FREESPACE);
			SET _SetWord(CDXBLKOFFSET_FREESPACE, VALUE), isHot := TRUE
			
			INTERNAL PROPERTY Recnum		AS DWORD			;
			GET _GetDWord(CDXBLKOFFSET_RECNUM);
			SET _SetDWord(CDXBLKOFFSET_RECNUM, VALUE), isHot := TRUE
			
			INTERNAL PROPERTY DuplicateCount	AS BYTE			;
			GET Buffer[CDXBLKOFFSET_DUPCOUNT];
			SET Buffer[CDXBLKOFFSET_DUPCOUNT] := VALUE, isHot := TRUE

			INTERNAL PROPERTY TrailingCount	AS BYTE			;
			GET Buffer[CDXBLKOFFSET_TRAILINGCOUNT];
			SET Buffer[CDXBLKOFFSET_TRAILINGCOUNT] := VALUE, isHot := TRUE
			
			INTERNAL PROPERTY RecordBits	AS BYTE			;
			GET Buffer[CDXBLKOFFSET_RECNUMBITS];
			SET Buffer[CDXBLKOFFSET_RECNUMBITS] := VALUE, isHot := TRUE
			
			INTERNAL PROPERTY DuplicateBits	AS BYTE			;
			GET Buffer[CDXBLKOFFSET_DUPCOUNTBITS];
			SET Buffer[CDXBLKOFFSET_DUPCOUNTBITS] := VALUE, isHot := TRUE
			
			INTERNAL PROPERTY TrailingBits	AS BYTE			;
			GET Buffer[CDXBLKOFFSET_TRAILINGBITS];
			SET Buffer[CDXBLKOFFSET_TRAILINGBITS] := VALUE, isHot := TRUE
			
			INTERNAL PROPERTY ShortBytes	AS BYTE			;
			GET Buffer[CDXBLKOFFSET_SHORTBYTES];
			SET Buffer[CDXBLKOFFSET_SHORTBYTES] := VALUE, isHot := TRUE
			
				
		PRIVATE CONST CDXBLKOFFSET_NODEATTR		:= 0	AS WORD // WORD
		PRIVATE CONST CDXBLKOFFSET_ENTRY		:= 2	AS WORD // WORD
		PRIVATE CONST CDXBLKOFFSET_LEFTPTR		:= 4	AS WORD // LONGINT
		PRIVATE CONST CDXBLKOFFSET_RIGHTPTR 	:= 8	AS WORD // LONGINT
		PRIVATE CONST CDXBLKOFFSET_FREESPACE	:= 12	AS WORD // WORD		: Free space in this key
		PRIVATE CONST CDXBLKOFFSET_RECNUM		:= 14	AS WORD // LONGINT	: Bit mask for record number
		PRIVATE CONST CDXBLKOFFSET_DUPCOUNT		:= 18	AS WORD // Bit mask for duplicate byte count
		PRIVATE CONST CDXBLKOFFSET_TRAILINGCOUNT:= 19	AS WORD // Bit mask for trailing byte count
		PRIVATE CONST CDXBLKOFFSET_RECNUMBITS	:= 20	AS WORD // Num bits used for record number
		PRIVATE CONST CDXBLKOFFSET_DUPCOUNTBITS := 21	AS WORD // Number of bits used for duplicate count
		PRIVATE CONST CDXBLKOFFSET_TRAILINGBITS := 22	AS WORD // Number of bits used for trailing count
		PRIVATE CONST CDXBLKOFFSET_SHORTBYTES	:= 23	AS WORD // Bytes needed for recno+dups+trailing
			
		PRIVATE CONST CDXBLOCK_SIZE         := 512 AS WORD
			
		
	END CLASS
END NAMESPACE 
