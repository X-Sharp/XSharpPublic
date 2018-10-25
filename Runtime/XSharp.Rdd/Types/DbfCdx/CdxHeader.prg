// CdxHeader.prg
// Created by    : fabri
// Creation Date : 10/14/2018 10:44:22 PM
// Created for   : 
// WorkStation   : FABPORTABLE

USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Runtime.CompilerServices

BEGIN NAMESPACE XSharp.RDD

	/// <summary>
	/// The CdxHeader class.
	/// </summary>
	CLASS CdxHeader
		PROTECTED _hFile AS IntPtr
		
		PUBLIC Bytes AS DbfCdxHeader
		
		
		METHOD Read() AS LOGIC
			LOCAL isOk AS LOGIC
			// Move to top
			FSeek3( SELF:_hFile, 0, SeekOrigin.Begin )
			// Read Buffer
			isOk := ( FRead3(SELF:_hFile, SELF:Bytes:Buffer, CDXOFFSETS.SIZE) == CDXOFFSETS.SIZE )
			//
			RETURN isOk
			
		METHOD Write() AS LOGIC
			LOCAL isOk AS LOGIC
			// Move to top
			FSeek3( SELF:_hFile, 0, SeekOrigin.Begin )
			// Write Buffer
			isOk := ( FWrite3(SELF:_hFile, SELF:Bytes:Buffer, CDXOFFSETS.SIZE) == CDXOFFSETS.SIZE )
			//
			RETURN isOk
			
			
			
		CONSTRUCTOR( fileHandle AS IntPtr )
			//
			SELF:_hFile := fileHandle
			SELF:Bytes := DbfCdxHeader{}
			SELF:Bytes:initialize();
			
			RETURN
			
			
			/// <summary>CDX Header.</summary>        
		STRUCTURE DbfCdxHeader                     
			// Fixed Buffer of 512 bytes
			// https://www.clicketyclick.dk/databases/xbase/format/cdx.html#CDX_STRUCT
			// Read/Write to/from the Stream with the Buffer 
			// and access individual values using the other fields
			PUBLIC Buffer   AS BYTE[]
			// Hot ?  => Header has changed ?
			PUBLIC isHot	AS LOGIC
			
			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PRIVATE METHOD _GetString(nOffSet AS INT, nSize AS INT) AS STRING
				LOCAL tmp := BYTE[]{nSize} AS BYTE[]
				Array.Copy( Buffer, nOffSet, tmp, 0, nSize )
				LOCAL count := Array.FindIndex<BYTE>( tmp, 0, { sz => sz == 0 } ) AS INT
				IF count == -1
					count := nSize
				ENDIF
				LOCAL str := System.Text.Encoding.ASCII:GetString( tmp,0, count ) AS STRING
				IF ( str == NULL )
					str := String.Empty
				ENDIF
				str := str:Trim()
				RETURN str
				
			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PRIVATE METHOD _SetString(nOffSet AS INT, nSize AS INT, sValue AS STRING) AS VOID
				// Be sure to fill the Buffer with 0
				Array.Clear( Buffer, nOffSet, nSize )
				System.Text.Encoding.ASCII:GetBytes( sValue, 0, Math.Min(nSize,sValue:Length), Buffer, nOffSet)
				isHot := TRUE
				
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
				
			PROPERTY Signature  AS BYTE	;
			GET Buffer[CDXOFFSETS.SIG] ;
			SET Buffer[CDXOFFSETS.SIG] := VALUE, isHot := TRUE
			
			PROPERTY IndexingVersion		AS DWORD			;
			GET _GetDWord(CDXOFFSETS.VERSION);
			SET _SetDWord(CDXOFFSETS.VERSION, VALUE), isHot := TRUE
			
			PROPERTY KeySize		AS WORD			;
			GET _GetWord(CDXOFFSETS.KEYLENGTH);
			SET _SetWord(CDXOFFSETS.KEYLENGTH, VALUE), isHot := TRUE
			
			PROPERTY Options	AS CDXOPTIONS			;
			GET (CDXOPTIONS)Buffer[CDXOFFSETS.OPTIONS];
			SET Buffer[CDXOFFSETS.OPTIONS] := VALUE, isHot := TRUE
			
			PROPERTY Descending	AS LOGIC  ;
			GET _GetWord( CDXOFFSETS.DESCENDING ) != 0 ;
			SET _SetWord( CDXOFFSETS.DESCENDING, IIF(VALUE,1,0) ), isHot := TRUE
			
			
			METHOD initialize() AS VOID STRICT
				Buffer := BYTE[]{CDXOFFSETS.SIZE}
				isHot  := FALSE
				RETURN
				
				END STRUCTURE
				
		PUBLIC ENUM CDXOFFSETS
			MEMBER ROOT			:= 0			// Byte offset to Root
			MEMBER FREELIST		:= 4			// Byte offset to next free block
			MEMBER VERSION		:= 8			// to increment on modification
			MEMBER KEYLENGTH	:= 12			// Length of key
			MEMBER OPTIONS		:= 14			// CDXOPTIONS : bit field
			MEMBER Sig			:= 15
			MEMBER DESCENDING	:= 502			// 0 = Ascending, 1 = Descending
			MEMBER FOREXPRPOS   := 504			// Offset of Filter expression
			MEMBER FOREXPRLEN   := 506			// Length of filter expression
			MEMBER KEYEXPRPOS   := 508			// Offset of Key expression
			MEMBER KEYEXPRLEN   := 510			// Length of key expression
			MEMBER KEYFOREXPR   := 512			// Key first with null terminator, then FOR expression.
			
			MEMBER KEYFORSIZE   := SIZE - 512
			MEMBER SIZE         := 1024
			
		END ENUM

		// Index options represented as the sum of the following values:
		PUBLIC ENUM CDXOPTIONS AS BYTE
			MEMBER IsUnique			:= 0x01		// Unique
			MEMBER IsWhile   		:= 0x02		// WHILE, ...
			MEMBER IsCustom			:= 0x04		// is a custom built Index
			MEMBER HasFor			:= 0x08		// FOR Clause
			MEMBER BitVector		:= 0x10		// Bit vector (SoftC)
			MEMBER IsCompact		:= 0x20		// Compact index format
			MEMBER IsTag			:= 0x40		// Tag inside CDX (Compounding index header)
			MEMBER IsHeader			:= 0x80		// CDX Header
			
		END ENUM


		
	END CLASS
END NAMESPACE 