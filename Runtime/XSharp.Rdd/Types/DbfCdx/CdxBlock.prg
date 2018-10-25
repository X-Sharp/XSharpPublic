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

BEGIN NAMESPACE XSharp.RDD

	/// <summary>
	/// The CdxBlock class.
	/// </summary>
	CLASS CdxBlock
		PROTECTED _hFile AS IntPtr
		PUBLIC Bytes AS DbfCdxBlock
		
		
		METHOD Read() AS LOGIC
			LOCAL isOk AS LOGIC
			// Move to top
			FSeek3( SELF:_hFile, 0, SeekOrigin.Begin )
			// Read Buffer
			isOk := ( FRead3(SELF:_hFile, SELF:Bytes:Buffer, CDXBLKOFFSETS.SIZE) == CDXBLKOFFSETS.SIZE )
			//
			RETURN isOk
			
		METHOD Write() AS LOGIC
			LOCAL isOk AS LOGIC
			// Move to top
			FSeek3( SELF:_hFile, 0, SeekOrigin.Begin )
			// Write Buffer
			isOk := ( FWrite3(SELF:_hFile, SELF:Bytes:Buffer, CDXBLKOFFSETS.SIZE) == CDXBLKOFFSETS.SIZE )
			//
			RETURN isOk
			
			
			
		CONSTRUCTOR( fileHandle AS IntPtr )
			//
			SELF:_hFile := fileHandle
			SELF:Bytes := DbfCdxBlock{}
			SELF:Bytes:initialize();
			
			RETURN
			
			
			/// <summary>CDX Block.</summary>        
		STRUCTURE DbfCdxBlock                     

			PUBLIC Buffer   AS BYTE[]
			// Hot ?  => Header has changed ?
			PUBLIC isHot	AS LOGIC
			
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
				
			PROPERTY NodeAttribute  AS WORD	;
			GET _GetWord(CDXBLKOFFSETS.NODEATTR);
			SET _SetWord(CDXBLKOFFSETS.NODEATTR, VALUE), isHot := TRUE

			PROPERTY Entry  AS WORD	;
			GET _GetWord(CDXBLKOFFSETS.ENTRY);
			SET _SetWord(CDXBLKOFFSETS.ENTRY, VALUE), isHot := TRUE

			PROPERTY LeftPtr		AS DWORD			;
			GET _GetDWord(CDXBLKOFFSETS.LEFTPTR);
			SET _SetDWord(CDXBLKOFFSETS.LEFTPTR, VALUE), isHot := TRUE

			PROPERTY RightPtr		AS DWORD			;
			GET _GetDWord(CDXBLKOFFSETS.RIGHTPTR);
			SET _SetDWord(CDXBLKOFFSETS.RIGHTPTR, VALUE), isHot := TRUE
			
			PROPERTY Freespace		AS WORD			;
			GET _GetWord(CDXBLKOFFSETS.FREESPACE);
			SET _SetWord(CDXBLKOFFSETS.FREESPACE, VALUE), isHot := TRUE
			
			PROPERTY Recnum		AS DWORD			;
			GET _GetDWord(CDXBLKOFFSETS.RECNUM);
			SET _SetDWord(CDXBLKOFFSETS.RECNUM, VALUE), isHot := TRUE
			
			PROPERTY DuplicateCount	AS BYTE			;
			GET Buffer[CDXBLKOFFSETS.DUPCOUNT];
			SET Buffer[CDXBLKOFFSETS.DUPCOUNT] := VALUE, isHot := TRUE

			PROPERTY TrailingCount	AS BYTE			;
			GET Buffer[CDXBLKOFFSETS.TRAILINGCOUNT];
			SET Buffer[CDXBLKOFFSETS.TRAILINGCOUNT] := VALUE, isHot := TRUE
			
			PROPERTY RecordBits	AS BYTE			;
			GET Buffer[CDXBLKOFFSETS.RECNUMBITS];
			SET Buffer[CDXBLKOFFSETS.RECNUMBITS] := VALUE, isHot := TRUE
			
			PROPERTY DuplicateBits	AS BYTE			;
			GET Buffer[CDXBLKOFFSETS.DUPCOUNTBITS];
			SET Buffer[CDXBLKOFFSETS.DUPCOUNTBITS] := VALUE, isHot := TRUE
			
			PROPERTY TrailingBits	AS BYTE			;
			GET Buffer[CDXBLKOFFSETS.TRAILINGBITS];
			SET Buffer[CDXBLKOFFSETS.TRAILINGBITS] := VALUE, isHot := TRUE
			
			PROPERTY ShortBytes	AS BYTE			;
			GET Buffer[CDXBLKOFFSETS.SHORTBYTES];
			SET Buffer[CDXBLKOFFSETS.SHORTBYTES] := VALUE, isHot := TRUE
			
			METHOD initialize() AS VOID STRICT
				Buffer := BYTE[]{CDXBLKOFFSETS.SIZE}
				isHot  := FALSE
				RETURN
				
				END STRUCTURE
				
		PUBLIC ENUM CDXBLKOFFSETS
			MEMBER NODEATTR		:= 0	// WORD
			MEMBER ENTRY		:= 2	// WORD
			MEMBER LEFTPTR		:= 4	// LONGINT
			MEMBER RIGHTPTR 	:= 8	// LONGINT

			MEMBER FREESPACE	:= 12	// WORD		: Free space in this key
			MEMBER RECNUM		:= 14	// LONGINT	: Bit mask for record number
			MEMBER DUPCOUNT		:= 18	// Bit mask for duplicate byte count
			MEMBER TRAILINGCOUNT:= 19	// Bit mask for trailing byte count
			MEMBER RECNUMBITS	:= 20	// Num bits used for record number
			MEMBER DUPCOUNTBITS := 21	// Number of bits used for duplicate count
			MEMBER TRAILINGBITS := 22	// Number of bits used for trailing count
			MEMBER SHORTBYTES	:= 23	// Bytes needed for recno+dups+trailing
			
			MEMBER SIZE         := 512
			
		END ENUM
		
	END CLASS
END NAMESPACE 