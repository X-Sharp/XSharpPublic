//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
/*
- The TAG Header contains some flags and a pointer to the top page of the tag. it is followed by a page with the key
  expression and for expression. The positions of these strings in this second page are in the first page
        type    description         start       length
        =====================================================================
		long	root;		        0x00	    4       offset of the root node
		long	freePage;			0x04        4       offset of list of free pages, or -1
		long	version;			0x08	    4       counter that increments when index is updated
		int2	keyLen;				0x0c  = 12 	2       2 bytes, but max = 240
		char	tagType;			0x0e  = 14 	1       see CdxOptions Enum
		char	signature;			0x0f	    1       
        int2    headerLen;          0x10        2       .
        int2    pageLen;            0x12        2       .
        long    collation           0x14        4       .
        char    reserved            0x18        68      . This area is often not filled
        char    lang                0x44        26      . ADI files for Advantage use this !
        char    collatver           0x76        4       .
        char    reserved            0x7a        372     .
        char    vfpcodepage         0x1ee       5       .
        char    ignorecase          0x1f3       1       .
        char    expression length   0x1f4       2       .
		int2	descend;			0x1f6 = 502 2       . 
		int2	foxExprPos          0x1f8 = 504 2       .
		int2	forExprLen;			0x1fa = 506 2       . Key Expression Length + For Expression Length Max 512
		int2    keyExprPos          0x1fc = 508 2       . bytes
		int2	keyExprLen;			0x1fe = 510 2       .
        // the next page after the tag header has the key expression and for expression. 
        // The key expression is delimited with a zero byte. The for expression starts at the position
        // indicated in the tag header.

*/

USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Runtime.CompilerServices
USING System.Diagnostics
BEGIN NAMESPACE XSharp.RDD.CDX

	/// <summary>
	/// The CdxTagHeader class. = Class that maps the Tag Header to memory
    /// The logic around this is in the CdxTag class
	/// </summary>
	INTERNAL CLASS CdxTagHeader INHERIT CdxPage
        // TagHeader consists of 2 pages. One with flags and one with Expressions
        // so therefore allocate extra 2nd page
    	INTERNAL ExprBuffer   AS CdxPage
        INTERNAL TagName      AS STRING
			
	    INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , nPage AS Int32 , buffer AS BYTE[], cTagName AS STRING)
            SUPER(bag, nPage, buffer)
            SELF:ExprBuffer := bag:GetPage(nPage +CDXPAGE_SIZE)
            SELF:TagName    := cTagName
#region Read/Write            
        PROTECTED INTERNAL OVERRIDE METHOD Read() AS LOGIC
            IF SUPER:Read()
                RETURN SELF:ExprBuffer:Read()
            ENDIF
            RETURN FALSE
            
        PROTECTED INTERNAL OVERRIDE METHOD Write() AS LOGIC
            IF SUPER:Write()
                RETURN SELF:ExprBuffer:Write()
            ENDIF
            RETURN FALSE
#endregion
#region properties

        PROTECTED INTERNAL PROPERTY RootPage AS LONG ;
			GET _GetLong(CDXOFFSET_ROOT);
			SET _SetLong(CDXOFFSET_ROOT, VALUE), isHot := TRUE
            
        PROTECTED INTERNAL PROPERTY FreeList AS LONG ;
			GET _GetLong(CDXOFFSET_FREELIST);
			SET _SetLong(CDXOFFSET_FREELIST, VALUE), isHot := TRUE

		PROTECTED INTERNAL PROPERTY Version		AS DWORD			;
			GET _GetDWord(CDXOFFSET_VERSION);
			SET _SetDWord(CDXOFFSET_VERSION, VALUE), isHot := TRUE
			
		PROTECTED INTERNAL PROPERTY KeySize		AS WORD			;
			GET _GetWord(CDXOFFSET_KEYLENGTH);
			SET _SetWord(CDXOFFSET_KEYLENGTH, VALUE), isHot := TRUE

        PROTECTED INTERNAL PROPERTY Options	AS CdxOptions			;
			GET (CdxOptions)Buffer[CDXOFFSET_OPTIONS];
			SET Buffer[CDXOFFSET_OPTIONS] := VALUE, isHot := TRUE

		PROTECTED INTERNAL PROPERTY Signature  AS BYTE	;
			GET _GetByte(CDXOFFSET_SIG) ;
            SET _SetByte(CDXOFFSET_SIG, VALUE)

	    PROTECTED INTERNAL PROPERTY KeyExprPos		AS WORD			;
			GET _GetWord(CDXOFFSET_KEYEXPRPOS);
			SET _SetWord(CDXOFFSET_KEYEXPRPOS, VALUE), isHot := TRUE

	    PROTECTED INTERNAL PROPERTY KeyExprLen	AS WORD			;
			GET _GetWord(CDXOFFSET_KEYEXPRLEN);
			SET _SetWord(CDXOFFSET_KEYEXPRLEN, VALUE), isHot := TRUE

        PROTECTED INTERNAL PROPERTY ForExprPos		AS WORD			;
			GET _GetWord(CDXOFFSET_FOREXPRPOS);
			SET _SetWord(CDXOFFSET_FOREXPRPOS, VALUE), isHot := TRUE

	    PROTECTED INTERNAL PROPERTY ForExprLen		AS WORD			;
			GET _GetWord(CDXOFFSET_FOREXPRLEN);
			SET _SetWord(CDXOFFSET_FOREXPRLEN, VALUE), isHot := TRUE

  			
		PROTECTED INTERNAL PROPERTY Descending	AS LOGIC  ;
			GET _GetWord( CDXOFFSET_DESCENDING ) != 0 ;
			SET _SetWord( CDXOFFSET_DESCENDING, (WORD) IIF(VALUE,1,0) ), isHot := TRUE

        PROTECTED INTERNAL PROPERTY KeyExpression AS STRING ;
            GET _GetString(ExprBuffer:buffer, KeyExprPos, KeyExprLen-1) ;
            SET _SetString(ExprBuffer:Buffer, KeyExprPos, KeyExprLen, VALUE) , isHot := TRUE

        PROTECTED INTERNAL PROPERTY ForExpression AS STRING ;
            GET _GetString(ExprBuffer:buffer, ForExprPos, ForExprLen-1) ;
            SET _SetString(ExprBuffer:buffer, ForExprPos, ForExprLen, VALUE) , isHot := TRUE

#endregion
#region constants
		PRIVATE CONST CDXOFFSET_ROOT		   := 0	AS WORD		// Byte offset to Root
		PRIVATE CONST CDXOFFSET_FREELIST	   := 4	AS WORD		// Byte offset to next free block
		PRIVATE CONST CDXOFFSET_VERSION		   := 8	AS WORD		// to increment on modification
		PRIVATE CONST CDXOFFSET_KEYLENGTH	   := 12	AS WORD		// Length of key 
		PRIVATE CONST CDXOFFSET_OPTIONS		   := 14	AS WORD		// CdxOptions : bit field
		PRIVATE CONST CDXOFFSET_Sig			   := 15   AS WORD
        // bytes 16 - 501 are all reserved
		PRIVATE CONST CDXOFFSET_DESCENDING	   := 502	AS WORD		// 0 = Ascending, 1 = Descending
		PRIVATE CONST CDXOFFSET_FOREXPRPOS     := 504	AS WORD		// Offset of Filter expression
		PRIVATE CONST CDXOFFSET_FOREXPRLEN     := 506	AS WORD		// Length of filter expression incl zero terminator
		PRIVATE CONST CDXOFFSET_KEYEXPRPOS     := 508	AS WORD		// Offset of Key expression
		PRIVATE CONST CDXOFFSET_KEYEXPRLEN     := 510	AS WORD		// Length of key expression incl zero terminator
#endregion			

		
	END CLASS
END NAMESPACE 
