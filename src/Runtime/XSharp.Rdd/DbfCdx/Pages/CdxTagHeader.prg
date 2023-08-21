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
        ** this block is undocumented **
        char    vfpcollation        0x1ee       8       .

		int2	descend;			0x1f6 = 502 2       .
		int2	forExprPos          0x1f8 = 504 2       .
		int2	forExprLen;			0x1fa = 506 2       . Key Expression Length + For Expression Length Max 512
		int2    keyExprPos          0x1fc = 508 2       . bytes
		int2	keyExprLen;			0x1fe = 510 2       .
        expressions                 0x200 = 512 .. 1023
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
            LOCAL oSb AS StringBuilder
            oSb := StringBuilder{}
            oSb:AppendLine(sIntro)
            oSb:AppendLine("Tag       : "+SELF:TagName)
            oSb:AppendLine("Page      : 0x"+SELF:PageNoX)
            oSb:AppendLine("Key       : "+SELF:KeyExpression)
            oSb:AppendLine("For       : "+SELF:ForExpression)
            oSb:AppendLine("Root      : 0x"+SELF:RootPage:ToString("X"))
            oSb:AppendLine("KeyLen    : "+SELF:KeySize:ToString())
            oSb:AppendLine("Options   : "+SELF:Options:ToString())
            oSb:AppendLine("Sig       : "+SELF:Signature:ToString())
            oSb:AppendLine("Descending: "+SELF:Descending:ToString())
            oSb:AppendLine("Collation : "+SELF:VFPCollation)
            RETURN oSb:ToString()

#endregion

        PRIVATE METHOD _getValues as VOID
            _rootPage   := SELF:_GetLong(CDXTAGHEADER_ROOT)
            _keyLength  := SELF:_GetWord(CDXTAGHEADER_KEYLENGTH)
            _options    := (CdxOptions)Buffer[CDXTAGHEADER_OPTIONS]
            _keyExprPos := SELF:_GetWord(CDXTAGHEADER_KEYEXPRPOS)
            _keyExprLen := SELF:_GetWord(CDXTAGHEADER_KEYEXPRLEN)
            _forExprPos := SELF:_GetWord(CDXTAGHEADER_FOREXPRPOS)
            _forExprLen := SELF:_GetWord(CDXTAGHEADER_FOREXPRLEN)
            _keyExpression := SELF:_GetString(_keyExprPos+CDXPAGE_SIZE, _keyExprLen)
            _forExpression := SELF:_GetString(_forExprPos+CDXPAGE_SIZE, _forExprLen)
            _descending  := SELF:_GetWord( CDXTAGHEADER_DESCENDING ) != 0
            _vfpCollation  := SELF:_GetString(CDXTAGHEADER_VFPCOLLATION, 8)

#region Fields
        PRIVATE _rootPage       as LONG
        PRIVATE _keyLength      as WORD
        PRIVATE _options        as CdxOptions
        PRIVATE _keyExprPos     as WORD
        PRIVATE _keyExprLen     as WORD
        PRIVATE _forExprPos     as WORD
        PRIVATE _forExprLen     as WORD
        PRIVATE _keyExpression  as STRING
        PRIVATE _forExpression  AS STRING
        PRIVATE _descending     as LOGIC
        PRIVATE _vfpCollation   as STRING    // GENERAL, ARABIC, DUTCH, GREEK, GERMAN etc
#endregion
#region properties

        INTERNAL PROPERTY RootPage AS LONG GET _rootPage;
            SET SELF:_SetLong(CDXTAGHEADER_ROOT, value), _rootPage := value

		INTERNAL PROPERTY KeySize		AS WORD	GET _keyLength;
			SET SELF:_SetWord(CDXTAGHEADER_KEYLENGTH, value), _keyLength := value

        INTERNAL PROPERTY Options	AS CdxOptions GET _options;
			SET Buffer[CDXTAGHEADER_OPTIONS] := value, _options := value

		INTERNAL PROPERTY Signature  AS BYTE GET SELF:_GetByte(CDXTAGHEADER_SIG) ;
            SET SELF:_SetByte(CDXTAGHEADER_SIG, value)

	    INTERNAL PROPERTY KeyExprPos AS WORD GET _keyExprPos;
			SET SELF:_SetWord(CDXTAGHEADER_KEYEXPRPOS, value), _keyExprPos := value

	    INTERNAL PROPERTY KeyExprLen	AS WORD GET _keyExprLen;
			SET SELF:_SetWord(CDXTAGHEADER_KEYEXPRLEN, value), _keyExprLen := value

        INTERNAL PROPERTY ForExprPos	AS WORD	GET _forExprPos ;
			SET SELF:_SetWord(CDXTAGHEADER_FOREXPRPOS, value), _forExprPos  := value

	    INTERNAL PROPERTY ForExprLen	AS WORD	GET _forExprLen ;
			SET SELF:_SetWord(CDXTAGHEADER_FOREXPRLEN, value), _forExprLen := value

		INTERNAL PROPERTY Descending	AS LOGIC  GET _descending ;
			SET SELF:_SetWord( CDXTAGHEADER_DESCENDING, (WORD) IIF(value,1,0) ), _descending := value

        INTERNAL PROPERTY KeyExpression AS STRING GET _keyExpression ;
            SET SELF:_SetString(KeyExprPos+CDXPAGE_SIZE, KeyExprLen, value) , _keyExpression := value

        INTERNAL PROPERTY ForExpression AS STRING GET _forExpression ;
            SET SELF:_SetString(ForExprPos+CDXPAGE_SIZE, ForExprLen, value) , _forExpression := value

        INTERNAL PROPERTY VFPCollation AS STRING GET _vfpCollation ;
            SET SELF:_SetString(CDXTAGHEADER_VFPCOLLATION, 8, value) , _vfpCollation := value


#endregion
#region constants
		PRIVATE CONST CDXTAGHEADER_ROOT		       := 0x00	AS WORD		// Byte offset to Root
		PRIVATE CONST CDXTAGHEADER_FREELIST	       := 0x04	AS WORD		// Byte offset to next free block
		PRIVATE CONST CDXTAGHEADER_VERSION		   := 0x08	AS WORD		// to increment on modification
		PRIVATE CONST CDXTAGHEADER_KEYLENGTH	   := 0x0c	AS WORD		// Length of key
		PRIVATE CONST CDXTAGHEADER_OPTIONS		   := 0x0e	AS WORD		// CdxOptions : bit field
		PRIVATE CONST CDXTAGHEADER_SIG			   := 0x0f   AS WORD
        PRIVATE CONST CDXTAGHEADER_VFPCOLLATION    := 0x1ee AS WORD     // 8 bytes GENERAL, MACHINE, DUTCH, GERMAN , NORDIC etc

        // Harbour documents these values
        PRIVATE CONST CDXTAGHEADER_HEADERLEN      := 0x10 AS WORD  // 2
        PRIVATE CONST CDXTAGHEADER_PAGELEN        := 0x12 AS WORD  // 2
        PRIVATE CONST CDXTAGHEADER_COLLATION      := 0x14 AS WORD  // 4
        PRIVATE CONST CDXTAGHEADER_RESERVED       := 0x18 AS WORD // 68 bytes
        PRIVATE CONST CDXTAGHEADER_LANG           := 0x44 AS WORD // 26 bytes
        PRIVATE CONST CDXTAGHEADER_COLLATVER      := 0x76 AS WORD // 4 bytes
        PRIVATE CONST CDXTAGHEADER_RESERVED2      := 0x7a AS WORD // 372 bytes
        //PRIVATE CONST CDXTAGHEADER_IGNORECASE     := 0x1f3 AS WORD // 1 byte
        //PRIVATE CONST CDXTAGHEADER_EXPR_LEN       := 0x1f4 AS WORD // 2 byte2
        // end of Harbour defines
		PRIVATE CONST CDXTAGHEADER_DESCENDING	  := 0x1f6	AS WORD		// 0 = Ascending, 1 = Descending
		PRIVATE CONST CDXTAGHEADER_FOREXPRPOS     := 0x1f8	AS WORD		// Offset of Filter expression
		PRIVATE CONST CDXTAGHEADER_FOREXPRLEN     := 0x1fa	AS WORD		// Length of filter expression incl zero terminator
		PRIVATE CONST CDXTAGHEADER_KEYEXPRPOS     := 0x1fc	AS WORD		// Offset of Key expression
		PRIVATE CONST CDXTAGHEADER_KEYEXPRLEN     := 0x1fe	AS WORD		// Length of key expression incl zero terminator
#endregion

        INTERNAL OVERRIDE METHOD Read() AS LOGIC
            LOCAL lOk as LOGIC
            lOk := SUPER:Read()
            IF lOk
                SELF:_getValues()
            ENDIF
            RETURN lOk

        INTERNAL METHOD UpdateWhenNeeded() AS VOID
            IF SELF:_bag:Root:RootVersion != SELF:Generation
                SELF:Read()
            ENDIF
            RETURN
	END CLASS
END NAMESPACE
