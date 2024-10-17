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

BEGIN NAMESPACE XSharp.RDD.NTX

	/// <summary>
	/// The NtxHeader class.
	/// </summary>
	INTERNAL SEALED CLASS NtxHeader
		// Fixed Buffer of 1024 bytes
		// https://www.clicketyclick.dk/databases/xbase/format/ntx.html#NTX_STRUCT
		// Read/Write to/from the Stream with the Buffer
		// and access individual values using the other fields
		PRIVATE _oStream AS FileStream
		PRIVATE Buffer   AS BYTE[]
		// Hot ?  => Header has changed ?
		INTERNAL isHot	AS LOGIC
        PRIVATE _Order as NtxOrder
        PROPERTY RDD AS DBFNTX Get _Order:RDD

		INTERNAL METHOD Read() AS LOGIC
			RETURN _oStream:SafeReadAt(0, SELF:Buffer)

		INTERNAL METHOD Write() AS LOGIC
			RETURN _oStream:SafeWriteAt(0, SELF:Buffer)

		INTERNAL CONSTRUCTOR( oOrder AS NtxOrder, stream AS FileStream )
			SELF:_oStream := stream
            SELF:_Order := oOrder
			Buffer := BYTE[]{NTXHEADER_SIZE}
			isHot  := FALSE

			RETURN

 		PRIVATE METHOD _GetString(nOffSet AS INT, nSize AS INT) AS STRING
			LOCAL count := System.Array.IndexOf<BYTE>( Buffer, 0, nOffSet, nSize) AS INT
			IF count == -1
                count := nSize
            ELSE
                count := count - nOffSet
			ENDIF
			LOCAL str := SELF:RDD:_GetString( Buffer,nOffSet, count ) AS STRING
			IF  str == NULL
				str := String.Empty
			ENDIF
			str := str:Trim()
			RETURN str

 		PRIVATE METHOD _GetBytes(nOffSet AS INT, nSize AS INT, sValue AS STRING) AS VOID
			// Be sure to fill the Buffer with 0
            var nLen := sValue:Length
            if nLen > nSize
                nLen := nSize
            elseif nLen < nSize
                Array.Clear( Buffer, nOffSet+nLen, nSize-nLen)
            endif
			SELF:RDD:_GetBytes( sValue, Buffer, nOffSet, nLen)
			isHot := TRUE

        [MethodImpl(MethodImplOptions.AggressiveInlining)];
		PRIVATE METHOD _GetWord(nOffSet AS INT) AS WORD
			RETURN BitConverter.ToUInt16(Buffer, nOffSet)

        [MethodImpl(MethodImplOptions.AggressiveInlining)];
		PRIVATE METHOD _SetWord(nOffSet AS INT, wValue AS WORD) AS VOID
			Array.Copy(BitConverter.GetBytes(wValue),0, Buffer, nOffSet, SIZEOF(WORD))
			isHot := TRUE

        [MethodImpl(MethodImplOptions.AggressiveInlining)];
		PRIVATE METHOD _GetLong(nOffSet AS INT) AS LONG
            RETURN BitConverter.ToInt32(Buffer, nOffSet)

        [MethodImpl(MethodImplOptions.AggressiveInlining)];
		PRIVATE METHOD _SetLong(nOffSet AS INT, nValue AS LONG) AS VOID
            Array.Copy(BitConverter.GetBytes(nValue), 0, Buffer, nOffSet, sizeof(LONG))
            isHot := TRUE

 		INTERNAL PROPERTY Signature  AS NtxHeaderFlags	;
		    GET (NtxHeaderFlags) SELF:_GetWord(NTXOFFSET_SIG) ;
		    SET SELF:_SetWord(NTXOFFSET_SIG, VALUE), isHot := TRUE

		INTERNAL PROPERTY IndexingVersion		AS WORD			;
		    GET SELF:_GetWord(NTXOFFSET_INDEXING_VER);
		    SET SELF:_SetWord(NTXOFFSET_INDEXING_VER, VALUE), isHot := TRUE

		INTERNAL PROPERTY FirstPageOffset		AS LONG			;
		    GET SELF:_GetLong(NTXOFFSET_FPAGE_OFFSET);
		    SET SELF:_SetLong(NTXOFFSET_FPAGE_OFFSET, VALUE), isHot := TRUE

		INTERNAL PROPERTY NextUnusedPageOffset		AS LONG			;
		    GET SELF:_GetLong(NTXOFFSET_NUPAGE_OFFSET)	;
		    SET SELF:_SetLong(NTXOFFSET_NUPAGE_OFFSET, VALUE), isHot := TRUE

		// keysize + 2 longs. ie.e Left pointer + record no.
		INTERNAL PROPERTY EntrySize		AS WORD			;
		    GET SELF:_GetWord(NTXOFFSET_ENTRYSIZE);
		    SET SELF:_SetWord(NTXOFFSET_ENTRYSIZE, VALUE), isHot := TRUE

		INTERNAL PROPERTY KeySize		AS WORD			;
		    GET SELF:_GetWord(NTXOFFSET_KEYSIZE);
		    SET SELF:_SetWord(NTXOFFSET_KEYSIZE, VALUE), isHot := TRUE

		INTERNAL PROPERTY KeyDecimals	AS WORD			;
		    GET SELF:_GetWord(NTXOFFSET_KEYDECIMALS);
		    SET SELF:_SetWord(NTXOFFSET_KEYDECIMALS, VALUE), isHot := TRUE

		INTERNAL PROPERTY MaxItem	AS WORD			;
		    GET SELF:_GetWord(NTXOFFSET_MAXITEM);
		    SET SELF:_SetWord(NTXOFFSET_MAXITEM, VALUE), isHot := TRUE

		INTERNAL PROPERTY HalfPage	AS WORD			;
		    GET SELF:_GetWord(NTXOFFSET_HALFPAGE);
		    SET SELF:_SetWord(NTXOFFSET_HALFPAGE, VALUE), isHot := TRUE

		INTERNAL PROPERTY KeyExpression	 AS STRING ;
		    GET SELF:_GetString(NTXOFFSET_KEYEXPRESSION, NTXOFFSET_EXPRESSION_SIZE ) ;
		    SET SELF:_GetBytes(NTXOFFSET_KEYEXPRESSION, NTXOFFSET_EXPRESSION_SIZE, VALUE), isHot := TRUE

		INTERNAL PROPERTY Unique	AS LOGIC  ;
		    GET SELF:_GetWord( NTXOFFSET_UNIQUE) != 0 ;
		    SET SELF:_SetWord( NTXOFFSET_UNIQUE , IIF(VALUE,1,0)), isHot := TRUE

		INTERNAL PROPERTY Descending	AS LOGIC  ;
		    GET SELF:_GetWord( NTXOFFSET_DESCENDING) != 0 ;
		    SET SELF:_SetWord( NTXOFFSET_DESCENDING, IIF(VALUE,1,0)), isHot := TRUE

		INTERNAL PROPERTY ForExpression	 AS STRING ;
		    GET SELF:_GetString(NTXOFFSET_FOREXPRESSION, NTXOFFSET_EXPRESSION_SIZE ) ;
		    SET SELF:_GetBytes(NTXOFFSET_FOREXPRESSION, NTXOFFSET_EXPRESSION_SIZE, VALUE), isHot := TRUE

		INTERNAL PROPERTY OrdName	 AS STRING ;
		    GET SELF:_GetString(NTXOFFSET_ORDNAME, 11 );
		    SET SELF:_GetBytes(NTXOFFSET_ORDNAME, 11, VALUE), isHot := TRUE

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
		PRIVATE CONST NTXOFFSET_UNIQUE          := 278 AS WORD
		PRIVATE CONST NTXOFFSET_DESCENDING      := 280 AS WORD
		PRIVATE CONST NTXOFFSET_FOREXPRESSION   := 282 AS WORD
		PRIVATE CONST NTXOFFSET_ORDNAME         := 538 AS WORD
		PRIVATE CONST NTXHEADER_SIZE            := 1024 AS WORD

	INTERNAL METHOD Dump(cText AS STRING) AS STRING
            VAR sb := System.Text.StringBuilder{}
            sb:AppendLine( String.Format("NTX Header {0}", cText))
            sb:AppendLine( "----------------------------------------------")
            sb:AppendLine( String.Format("Signature {0}, Version {1}, First page {2:X6}, Unused Page {3:X6}", SELF:Signature, SELF:IndexingVersion, SELF:FirstPageOffset, SELF:NextUnusedPageOffset))
            sb:AppendLine( String.Format("Item size {0}, Key Size {1}, Decimals {2}, Max Items {3}, HalfPage {4}", SELF:EntrySize, SELF:KeySize, SELF:KeyDecimals, SELF:MaxItem, SELF:HalfPage))
            sb:AppendLine( String.Format("Key Expression: {0}, Unique {1}, Descending {2}", SELF:KeyExpression, SELF:Unique, SELF:Descending))
            sb:AppendLine( String.Format("For Expression: {0}", SELF:ForExpression))
            sb:AppendLine( String.Format("Order name    : {0}", SELF:OrdName))
            sb:AppendLine( "----------------------------------------------")
            RETURN sb:ToString()


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

