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
		int2	forExprPos          0x1f8 = 504 2       .
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
        INTERNAL PROPERTY TagName      AS STRING AUTO

	    INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , nPage AS Int32 , cTagName AS STRING, oTag as CdxTag)
            SUPER(bag)
            SELF:_nPage := nPage
            SELF:SetBuffer(bag:AllocBuffer(2))
            SELF:TagName    := cTagName
            SELF:_getValues()
            SELF:_tag := oTag
#region Read/Write            

         INTERNAL METHOD Dump(sIntro AS STRING) AS STRING
            LOCAL oSb AS stringBuilder
            osb := stringBuilder{}
            oSb:AppendLine(sIntro)
            oSb:AppendLine("Tag       : "+SELF:TagName)
            oSb:AppendLine("Page      : 0x"+SELF:PageNo:ToString("X"))
            oSb:AppendLine("Key       : "+SELF:KeyExpression)
            oSb:AppendLine("For       : "+SELF:ForExpression)
            oSb:AppendLine("Root      : 0x"+SELF:RootPage:ToString("X"))
            oSb:AppendLine("Free      : "+SELF:FreeList:ToString())
            oSb:AppendLine("Version   : "+SELF:Version:ToString())
            oSb:AppendLine("KeyLen    : "+SELF:KeySize:ToString())
            oSb:AppendLine("Options   : "+SELF:Options:ToString())
            oSb:AppendLine("Sig       : "+SELF:Signature:ToString())
            oSb:AppendLine("Descending: "+SELF:Descending:ToString())
            oSb:AppendLine("Collation : "+SELF:VFPCollation)
            RETURN oSb:ToString()

#endregion

        PRIVATE METHOD _getValues as VOID
            _rootPage   := _GetLong(CDXTAGHEADER_ROOT)
            _freeList   := _GetLong(CDXTAGHEADER_FREELIST)
            _version    := _GetDWord(CDXTAGHEADER_VERSION)
            _keyLength  := _GetWord(CDXTAGHEADER_KEYLENGTH)
            _options    := (CdxOptions)Buffer[CDXTAGHEADER_OPTIONS]
            _keyExprPos := _GetWord(CDXTAGHEADER_KEYEXPRPOS)
            _keyExprLen := _GetWord(CDXTAGHEADER_KEYEXPRLEN)
            _forExprPos := _GetWord(CDXTAGHEADER_FOREXPRPOS)
            _forExprLen := _GetWord(CDXTAGHEADER_FOREXPRLEN)
            _keyExpression := _GetString(_keyExprPos+CDXPAGE_SIZE, _keyExprLen)
            _forExpression := _GetString(_forExprPos+CDXPAGE_SIZE, _forExprLen)
            _descending  := _GetWord( CDXTAGHEADER_DESCENDING ) != 0
            _vfpCollation  := _GetString(CDXTAGHEADER_VFPCOLLATION, 8)

#region Fields
        PRIVATE _rootPage as LONG
        PRIVATE _freeList AS LONG
        PRIVATE _version as DWORD
        PRIVATE _keyLength as WORD
        PRIVATE _options as CdxOptions
        PRIVATE _keyExprPos as WORD
        PRIVATE _keyExprLen as WORD
        PRIVATE _forExprPos as WORD
        PRIVATE _forExprLen as WORD
        PRIVATE _keyExpression as STRING
        PRIVATE _forExpression AS STRING
        PRIVATE _descending as LOGIC
        PRIVATE _vfpCollation    as STRING    // GENERAL, ARABIC, DUTCH, GREEK, GERMAN etc
#endregion
#region properties

        PROTECTED INTERNAL PROPERTY RootPage AS LONG GET _rootPage;
            SET _SetLong(CDXTAGHEADER_ROOT, VALUE), _rootPage := Value
            
        PROTECTED INTERNAL PROPERTY FreeList AS LONG GET _freeList ;
			SET _SetLong(CDXTAGHEADER_FREELIST, VALUE), _freeList  := Value

		PROTECTED INTERNAL PROPERTY Version		AS DWORD GET _version;
			SET _SetDWord(CDXTAGHEADER_VERSION, VALUE), _version := Value
			
		PROTECTED INTERNAL PROPERTY KeySize		AS WORD	GET _keyLength;
			SET _SetWord(CDXTAGHEADER_KEYLENGTH, VALUE), _keyLength := Value

        PROTECTED INTERNAL PROPERTY Options	AS CdxOptions GET _options;
			SET Buffer[CDXTAGHEADER_OPTIONS] := VALUE, _options := value

		PROTECTED INTERNAL PROPERTY Signature  AS BYTE GET _GetByte(CDXTAGHEADER_SIG) ;
            SET _SetByte(CDXTAGHEADER_SIG, VALUE)

	    PROTECTED INTERNAL PROPERTY KeyExprPos AS WORD GET _keyExprPos;
			SET _SetWord(CDXTAGHEADER_KEYEXPRPOS, VALUE), _keyExprPos := Value

	    PROTECTED INTERNAL PROPERTY KeyExprLen	AS WORD GET _keyExprLen;
			SET _SetWord(CDXTAGHEADER_KEYEXPRLEN, VALUE), _keyExprLen := value

        PROTECTED INTERNAL PROPERTY ForExprPos	AS WORD	GET _forExprPos ;
			SET _SetWord(CDXTAGHEADER_FOREXPRPOS, VALUE), _forExprPos  := Value

	    PROTECTED INTERNAL PROPERTY ForExprLen	AS WORD	GET _forExprLen ;
			SET _SetWord(CDXTAGHEADER_FOREXPRLEN, VALUE), _forExprLen := value

		PROTECTED INTERNAL PROPERTY Descending	AS LOGIC  GET _descending ;
			SET _SetWord( CDXTAGHEADER_DESCENDING, (WORD) IIF(VALUE,1,0) ), _descending := value

        PROTECTED INTERNAL PROPERTY KeyExpression AS STRING GET _keyExpression ;
            SET _SetString(KeyExprPos+CDXPAGE_SIZE, KeyExprLen, VALUE) , _keyExpression := value

        PROTECTED INTERNAL PROPERTY ForExpression AS STRING GET _forExpression ;
            SET _SetString(ForExprPos+CDXPAGE_SIZE, ForExprLen, VALUE) , _forExpression := Value

        PROTECTED INTERNAL PROPERTY VFPCollation AS STRING GET _vfpCollation ;
            SET _SetString(CDXTAGHEADER_VFPCOLLATION, 8, VALUE) , _vfpCollation := Value


#endregion
#region constants
		PRIVATE CONST CDXTAGHEADER_ROOT		        := 0x00	AS WORD		// Byte offset to Root
		PRIVATE CONST CDXTAGHEADER_FREELIST	        := 0x04	AS WORD		// Byte offset to next free block
		PRIVATE CONST CDXTAGHEADER_VERSION		   := 0x08	AS WORD		// to increment on modification
		PRIVATE CONST CDXTAGHEADER_KEYLENGTH	   := 0x0c	AS WORD		// Length of key
		PRIVATE CONST CDXTAGHEADER_OPTIONS		   := 0x0e	AS WORD		// CdxOptions : bit field
		PRIVATE CONST CDXTAGHEADER_Sig			   := 0x0f   AS WORD
        // Harbour documents these values
        PRIVATE CONST CDXTAGHEADER_HEADERLEN      := 0x10 AS WORD  // 2
        PRIVATE CONST CDXTAGHEADER_PAGELEN        := 0x12 AS WORD  // 2
        PRIVATE CONST CDXTAGHEADER_COLLATION      := 0x14 AS WORD  // 4
        PRIVATE CONST CDXTAGHEADER_RESERVED       := 0x18 AS WORD // 68 bytes
        PRIVATE CONST CDXTAGHEADER_LANG           := 0x44 AS WORD // 26 bytes
        PRIVATE CONST CDXTAGHEADER_COLLATVER      := 0x76 AS WORD // 4 bytes
        PRIVATE CONST CDXTAGHEADER_RESERVED2      := 0x7a AS WORD // 372 bytes
        PRIVATE CONST CDXTAGHEADER_VFPCOLLATION    := 0x1ee AS WORD // 5 bytes
        PRIVATE CONST CDXTAGHEADER_IGNORECASE     := 0x1f3 AS WORD // 1 byte
        PRIVATE CONST CDXTAGHEADER_EXPR_LEN       := 0x1f4 AS WORD // 2 byte2
        // end of Harbour defines
		PRIVATE CONST CDXTAGHEADER_DESCENDING	  := 0x1f6	AS WORD		// 0 = Ascending, 1 = Descending
		PRIVATE CONST CDXTAGHEADER_FOREXPRPOS     := 0x1f8	AS WORD		// Offset of Filter expression
		PRIVATE CONST CDXTAGHEADER_FOREXPRLEN     := 0x1fa	AS WORD		// Length of filter expression incl zero terminator
		PRIVATE CONST CDXTAGHEADER_KEYEXPRPOS     := 0x1fc	AS WORD		// Offset of Key expression
		PRIVATE CONST CDXTAGHEADER_KEYEXPRLEN     := 0x1fe	AS WORD		// Length of key expression incl zero terminator
#endregion			

        INTERNAL VIRTUAL METHOD Read() AS LOGIC
            LOCAL lOk as LOGIC
            lOk := SUPER:Read()
            IF lOk
                SELF:_getValues()
            ENDIF
            RETURN lOk
		
	END CLASS
END NAMESPACE 
