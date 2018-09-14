// NtxHeader.prg
// Created by    : fabri
// Creation Date : 6/20/2018 5:04:18 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
using System.Runtime.CompilerServices

BEGIN NAMESPACE XSharp.RDD

	/// <summary>
	/// The NtxHeader class.
	/// </summary>
	CLASS NtxHeader
		PROTECTED _hFile AS IntPtr
		
		PROPERTY Bytes AS DbfNtxHeader AUTO GET 
		
		
		METHOD Read() AS LOGIC
			LOCAL isOk AS LOGIC
			// Move to top
			FSeek3( SELF:_hFile, 0, SeekOrigin.Begin )
			// Read Buffer
			isOk := ( FRead3(SELF:_hFile, SELF:Bytes:Buffer, NTXOFFSETS.SIZE) == NTXOFFSETS.SIZE )
			//
			RETURN isOk
			
		METHOD Write() AS LOGIC
			LOCAL isOk AS LOGIC
			// Move to top
			FSeek3( SELF:_hFile, 0, SeekOrigin.Begin )
			// Write Buffer
			isOk := ( FWrite3(SELF:_hFile, SELF:Bytes:Buffer, NTXOFFSETS.SIZE) == NTXOFFSETS.SIZE )
			//
			RETURN isOk
			
			
			
		CONSTRUCTOR( fileHandle AS IntPtr )
			//
			SELF:_hFile := fileHandle
			
			RETURN
			
			
			/// <summary>NTX Header.</summary>        
		STRUCTURE DbfNtxHeader                     
			// Fixed Buffer of 1024 bytes
			// https://www.clicketyclick.dk/databases/xbase/format/ntx.html#NTX_STRUCT  
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
				
			PROPERTY Signature  AS WORD	;
			GET _GetWord(NTXOFFSETS.SIG) ;
			SET _SetWord(NTXOFFSETS.SIG, VALUE)
			
			PROPERTY IndexingVersion		AS WORD			;
			GET _GetWord(NTXOFFSETS.INDEXING_VER);
			SET _SetWord(NTXOFFSETS.INDEXING_VER, VALUE)
			
			PROPERTY FirstPageOffset		AS DWORD			;
			GET _GetDWord(NTXOFFSETS.FPAGE_OFFSET);
			SET _SetDWord(NTXOFFSETS.FPAGE_OFFSET, VALUE)
			
			PROPERTY NextUnusedPageOffset		AS DWORD			;
			GET _GetDWord(NTXOFFSETS.NUPAGE_OFFSET)	;
			SET _SetDWord(NTXOFFSETS.NUPAGE_OFFSET, VALUE)
			
			// keysize + 2 longs. ie.e Left pointer + record no.
			PROPERTY EntrySize		AS WORD			;
			GET _GetWord(NTXOFFSETS.ENTRYSIZE);
			SET _SetWord(NTXOFFSETS.ENTRYSIZE, VALUE)
			
			PROPERTY KeySize		AS WORD			;
			GET _GetWord(NTXOFFSETS.KEYSIZE);
			SET _SetWord(NTXOFFSETS.KEYSIZE, VALUE)
			
			PROPERTY KeyDecimals	AS WORD			;
			GET _GetWord(NTXOFFSETS.KEYDECIMALS);
			SET _SetWord(NTXOFFSETS.KEYDECIMALS, VALUE)
			
			PROPERTY MaxItem	AS WORD			;
			GET _GetWord(NTXOFFSETS.MAXITEM);
			SET _SetWord(NTXOFFSETS.MAXITEM, VALUE)
			
			PROPERTY HalfPage	AS WORD			;
			GET _GetWord(NTXOFFSETS.HALFPAGE);
			SET _SetWord(NTXOFFSETS.HALFPAGE, VALUE)
			
			PROPERTY KeyExpression	 AS STRING ;
			GET _GetString(NTXOFFSETS.KEYEXPRESSION, NTXOFFSETS.EXPRESSION_SIZE ) ;
			SET _SetString(NTXOFFSETS.KEYEXPRESSION, NTXOFFSETS.EXPRESSION_SIZE, VALUE)
			
			PROPERTY Unique	AS LOGIC  ;
			GET Buffer[ NTXOFFSETS.UNIQUE] != 0 ;
			SET Buffer[ NTXOFFSETS.UNIQUE ] := IIF(VALUE,1,0), isHot := TRUE
			
			PROPERTY ForExpression	 AS STRING ;
			GET _GetString(NTXOFFSETS.FOREXPRESSION, NTXOFFSETS.EXPRESSION_SIZE ) ;
			SET _SetString(NTXOFFSETS.FOREXPRESSION, NTXOFFSETS.EXPRESSION_SIZE, VALUE)
			
			PROPERTY OrdName	 AS STRING ;
			GET _GetString(NTXOFFSETS.ORDNAME, NTXOFFSETS.EXPRESSION_SIZE );
			SET _SetString(NTXOFFSETS.ORDNAME, NTXOFFSETS.EXPRESSION_SIZE, VALUE)
			
			
			
			METHOD initialize() AS VOID STRICT
				Buffer := BYTE[]{NTXOFFSETS.SIZE}
				isHot  := FALSE
				RETURN
				
				END STRUCTURE
				
		PUBLIC ENUM NTXOFFSETS
			MEMBER SIG			:= 0
			MEMBER INDEXING_VER := 2
			MEMBER FPAGE_OFFSET := 4
			MEMBER NUPAGE_OFFSET := 8
			MEMBER ENTRYSIZE := 12
			MEMBER KEYSIZE := 14
			MEMBER KEYDECIMALS := 16
			MEMBER MAXITEM := 18
			MEMBER HALFPAGE := 20
			MEMBER KEYEXPRESSION := 22
			MEMBER EXPRESSION_SIZE := 256
			MEMBER UNIQUE := 258
			MEMBER FOREXPRESSION := 282
			MEMBER ORDNAME := 538
			
			MEMBER SIZE         := 1024
			
		END ENUM
		
	END CLASS
END NAMESPACE // global::XSharp.RDD.Types.DbfNtx