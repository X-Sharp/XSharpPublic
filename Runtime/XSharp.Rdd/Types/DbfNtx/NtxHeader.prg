// NtxHeader.prg
// Created by    : fabri
// Creation Date : 6/20/2018 5:04:18 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Runtime.CompilerServices

BEGIN NAMESPACE XSharp.RDD

	/// <summary>
	/// The NtxHeader class.
	/// </summary>
	INTERNAL SEALED CLASS NtxHeader
		// Fixed Buffer of 1024 bytes
		// https://www.clicketyclick.dk/databases/xbase/format/ntx.html#NTX_STRUCT  
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
			isOk := ( FRead3(SELF:_hFile, SELF:Buffer, NTXHEADER_SIZE) == NTXHEADER_SIZE )
			//
			RETURN isOk
			
		INTERNAL METHOD Write() AS LOGIC
			LOCAL isOk AS LOGIC
			// Move to top
			FSeek3( SELF:_hFile, 0, SeekOrigin.Begin )
			// Write Buffer
			isOk := ( FWrite3(SELF:_hFile, SELF:Buffer, NTXHEADER_SIZE) == NTXHEADER_SIZE )
			//
			RETURN isOk
			
			
			
		INTERNAL CONSTRUCTOR( fileHandle AS IntPtr )
			//
			SELF:_hFile := fileHandle
			Buffer := BYTE[]{NTXHEADER_SIZE}
			isHot  := FALSE
			
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
				
		INTERNAL PROPERTY Signature  AS NtxHeaderFlags	;
		GET (NtxHeaderFlags) _GetWord(NTXOFFSET_SIG) ;
		SET _SetWord(NTXOFFSET_SIG, VALUE)
			
		INTERNAL PROPERTY IndexingVersion		AS WORD			;
		GET _GetWord(NTXOFFSET_INDEXING_VER);
		SET _SetWord(NTXOFFSET_INDEXING_VER, VALUE)
			
		INTERNAL PROPERTY FirstPageOffset		AS DWORD			;
		GET _GetDWord(NTXOFFSET_FPAGE_OFFSET);
		SET _SetDWord(NTXOFFSET_FPAGE_OFFSET, VALUE)
			
		INTERNAL PROPERTY NextUnusedPageOffset		AS DWORD			;
		GET _GetDWord(NTXOFFSET_NUPAGE_OFFSET)	;
		SET _SetDWord(NTXOFFSET_NUPAGE_OFFSET, VALUE)
			
		// keysize + 2 longs. ie.e Left pointer + record no.
		INTERNAL PROPERTY EntrySize		AS WORD			;
		GET _GetWord(NTXOFFSET_ENTRYSIZE);
		SET _SetWord(NTXOFFSET_ENTRYSIZE, VALUE)
			
		INTERNAL PROPERTY KeySize		AS WORD			;
		GET _GetWord(NTXOFFSET_KEYSIZE);
		SET _SetWord(NTXOFFSET_KEYSIZE, VALUE)
			
		INTERNAL PROPERTY KeyDecimals	AS WORD			;
		GET _GetWord(NTXOFFSET_KEYDECIMALS);
		SET _SetWord(NTXOFFSET_KEYDECIMALS, VALUE)
			
		INTERNAL PROPERTY MaxItem	AS WORD			;
		GET _GetWord(NTXOFFSET_MAXITEM);
		SET _SetWord(NTXOFFSET_MAXITEM, VALUE)
			
		INTERNAL PROPERTY HalfPage	AS WORD			;
		GET _GetWord(NTXOFFSET_HALFPAGE);
		SET _SetWord(NTXOFFSET_HALFPAGE, VALUE)
			
		INTERNAL PROPERTY KeyExpression	 AS STRING ;
		GET _GetString(NTXOFFSET_KEYEXPRESSION, NTXOFFSET_EXPRESSION_SIZE ) ;
		SET _SetString(NTXOFFSET_KEYEXPRESSION, NTXOFFSET_EXPRESSION_SIZE, VALUE)
			
		INTERNAL PROPERTY Unique	AS LOGIC  ;
		GET Buffer[ NTXOFFSET_UNIQUE] != 0 ;
		SET Buffer[ NTXOFFSET_UNIQUE ] := IIF(VALUE,1,0), isHot := TRUE

		INTERNAL PROPERTY Descending	AS LOGIC  ;
		GET Buffer[ NTXOFFSET_DESCENDING] != 0 ;
		SET Buffer[ NTXOFFSET_DESCENDING ] := IIF(VALUE,1,0), isHot := TRUE
			
		INTERNAL PROPERTY ForExpression	 AS STRING ;
		GET _GetString(NTXOFFSET_FOREXPRESSION, NTXOFFSET_EXPRESSION_SIZE ) ;
		SET _SetString(NTXOFFSET_FOREXPRESSION, NTXOFFSET_EXPRESSION_SIZE, VALUE)
			
		INTERNAL PROPERTY OrdName	 AS STRING ;
		GET _GetString(NTXOFFSET_ORDNAME, NTXOFFSET_EXPRESSION_SIZE );
		SET _SetString(NTXOFFSET_ORDNAME, NTXOFFSET_EXPRESSION_SIZE, VALUE)
			
		PRIVATE CONST NTXOFFSET_SIG			    := 0   AS WORD
		PRIVATE CONST NTXOFFSET_INDEXING_VER    := 2   AS WORD
		PRIVATE CONST NTXOFFSET_FPAGE_OFFSET    := 4   AS WORD
		PRIVATE CONST NTXOFFSET_NUPAGE_OFFSET   := 8   AS WORD
		PRIVATE CONST NTXOFFSET_ENTRYSIZE       := 12  AS WORD
		PRIVATE CONST NTXOFFSET_KEYSIZE         := 14  AS WORD
		PRIVATE CONST NTXOFFSET_KEYDECIMALS     := 16  AS WORD
		PRIVATE CONST NTXOFFSET_MAXITEM         := 18  AS WORD
		PRIVATE CONST NTXOFFSET_HALFPAGE        := 20  AS WORD
		PRIVATE CONST NTXOFFSET_KEYEXPRESSION   := 22  AS WORD
		PRIVATE CONST NTXOFFSET_EXPRESSION_SIZE := 256 AS WORD
		PRIVATE CONST NTXOFFSET_UNIQUE          := 258 AS WORD
		PRIVATE CONST NTXOFFSET_DESCENDING      := 280 AS WORD
		PRIVATE CONST NTXOFFSET_FOREXPRESSION   := 282 AS WORD
		PRIVATE CONST NTXOFFSET_ORDNAME         := 538 AS WORD
		PRIVATE CONST NTXHEADER_SIZE            := 1024 AS WORD
        
			
		
    END CLASS
    [Flags];        
    INTERNAL ENUM NtxHeaderFlags AS WORD
        MEMBER Default      := 0x0006
        MEMBER Conditional  := 0x0001
        MEMBER Partial      := 0x0008
        MEMBER NewLock      := 0x0010
        MEMBER HpLock       := 0x0020
        // from Harbour, not used (yet)
        MEMBER ChangeOnly   := 0x0040
        MEMBER Template     := 0x0080
        MEMBER SortRecno    := 0x0100
        MEMBER LargeFile    := 0x0200
        MEMBER MultiKey     := 0x0400
        MEMBER Compound     := 0x8000
        MEMBER Flag_Mask    := 0x87FF
     END ENUM
END NAMESPACE // global::XSharp.RDD.Types.DbfNtx

