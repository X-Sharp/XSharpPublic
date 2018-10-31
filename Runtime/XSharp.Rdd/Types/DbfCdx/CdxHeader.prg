//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Runtime.CompilerServices

BEGIN NAMESPACE XSharp.RDD.CDX

	/// <summary>
	/// The CdxHeader class.
	/// </summary>
	INTERNAL CLASS CdxHeader
		// Fixed Buffer of 512 bytes
		// https://www.clicketyclick.dk/databases/xbase/format/cdx.html#CDX_STRUCT
		// Read/Write to/from the Stream with the Buffer 
		// and access individual values using the other fields
		PRIVATE _hFile AS IntPtr
		PRIVATE Buffer   AS BYTE[]
		// Hot ?  => Header has changed ?
		INTERNAL isHot	AS LOGIC
		
		
		
		INTERNAL METHOD Read() AS LOGIC
			LOCAL isOk AS LOGIC
			// Move to top
			FSeek3( SELF:_hFile, 0, SeekOrigin.Begin )
			// Read Buffer
			isOk := ( FRead3(SELF:_hFile, SELF:Buffer, CDX_HEADERSIZE) == CDX_HEADERSIZE )
			//
			RETURN isOk
			
		INTERNAL METHOD Write() AS LOGIC
			LOCAL isOk AS LOGIC
			// Move to top
			FSeek3( SELF:_hFile, 0, SeekOrigin.Begin )
			// Write Buffer
			isOk := ( FWrite3(SELF:_hFile, SELF:Buffer, CDX_HEADERSIZE) == CDX_HEADERSIZE )
			//
			RETURN isOk
			
			
			
		INTERNAL CONSTRUCTOR( fileHandle AS IntPtr )
			//
			SELF:_hFile := fileHandle
			SELF:Buffer := BYTE[]{CDX_HEADERSIZE}
			SELF:isHot  := FALSE
			RETURN
			
			
			
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
				
			INTERNAL PROPERTY Signature  AS BYTE	;
			GET Buffer[CDXOFFSET_SIG] ;
			SET Buffer[CDXOFFSET_SIG] := VALUE, isHot := TRUE
			
			INTERNAL PROPERTY IndexingVersion		AS DWORD			;
			GET _GetDWord(CDXOFFSET_VERSION);
			SET _SetDWord(CDXOFFSET_VERSION, VALUE), isHot := TRUE
			
			INTERNAL PROPERTY KeySize		AS WORD			;
			GET _GetWord(CDXOFFSET_KEYLENGTH);
			SET _SetWord(CDXOFFSET_KEYLENGTH, VALUE), isHot := TRUE
			
			INTERNAL PROPERTY Options	AS CdxOptions			;
			GET (CdxOptions)Buffer[CDXOFFSET_OPTIONS];
			SET Buffer[CDXOFFSET_OPTIONS] := VALUE, isHot := TRUE
			
			INTERNAL PROPERTY Descending	AS LOGIC  ;
			GET _GetWord( CDXOFFSET_DESCENDING ) != 0 ;
			SET _SetWord( CDXOFFSET_DESCENDING, IIF(VALUE,1,0) ), isHot := TRUE
			
			
				
		
		PRIVATE CONST CDXOFFSET_ROOT		   := 0	AS WORD		// Byte offset to Root
		PRIVATE CONST CDXOFFSET_FREELIST	   := 4	AS WORD		// Byte offset to next free block
		PRIVATE CONST CDXOFFSET_VERSION		   := 8	AS WORD		// to increment on modification
		PRIVATE CONST CDXOFFSET_KEYLENGTH	   := 12	AS WORD		// Length of key
		PRIVATE CONST CDXOFFSET_OPTIONS		   := 14	AS WORD		// CdxOptions : bit field
		PRIVATE CONST CDXOFFSET_Sig			   := 15   AS WORD
        // bytes 16 - 501 are all reserved
		PRIVATE CONST CDXOFFSET_DESCENDING	   := 502	AS WORD		// 0 = Ascending, 1 = Descending
		PRIVATE CONST CDXOFFSET_FOREXPRPOS     := 504	AS WORD		// Offset of Filter expression
		PRIVATE CONST CDXOFFSET_FOREXPRLEN     := 506	AS WORD		// Length of filter expression
		PRIVATE CONST CDXOFFSET_KEYEXPRPOS     := 508	AS WORD		// Offset of Key expression
		PRIVATE CONST CDXOFFSET_KEYEXPRLEN     := 510	AS WORD		// Length of key expression
		PRIVATE CONST CDXOFFSET_KEYFOREXPR     := 512	AS WORD		// Key first with null terminator, then FOR expression.
			
		PRIVATE CONST CDX_KEYFORSIZE           := CDX_HEADERSIZE - 512 AS WORD
		PRIVATE CONST CDX_HEADERSIZE           := 1024 AS WORD

		// Index options represented as the sum of the following values:
        [Flags];
		INTERNAL ENUM CdxOptions AS BYTE
			MEMBER IsUnique			:= 0x01		// Unique
			MEMBER IsWhile   		:= 0x02		// WHILE, ...
			MEMBER IsCustom			:= 0x04		// is a custom built Index
			MEMBER HasFor			:= 0x08		// FOR Clause
			MEMBER BitVector		:= 0x10		// Bit vector (SoftC)
			MEMBER IsCompact		:= 0x20		// Compact index format
			MEMBER IsTag			:= 0x40		// Tag inside CDX (Compounding index header)
			MEMBER IsHeader			:= 0x80		// CDX Header (contains the names of the tags)
			
		END ENUM
		
	END CLASS
END NAMESPACE 
