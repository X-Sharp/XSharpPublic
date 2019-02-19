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
	/// The CdxHeader class. = Class that maps the File Header to memory
    /// The logic around this is in the OrderBag class
	/// </summary>
	/// <remarks>
    /// The first page in the file is the FILE Header (Bag Header). |
  	/// The keylength is fixed to 10 (which is the tag length)
  	/// The root points to the list of tags (which is a LEAF page)
  	/// The freepage points to the list of free pages at BAG level.
    /// </remarks>
	INTERNAL CLASS CdxFileHeader INHERIT CdxPage

    INTERNAL CONSTRUCTOR( oBag AS CdxOrderBag, page AS CdxPage)
        SUPER(oBag, page:PageNo, page:Buffer)
        
#region Properties
	INTERNAL PROPERTY TagList		AS Int32;
		GET _GetLONG(CDXROOT_TAGLIST);
		SET _SetLONG(CDXROOT_TAGLIST, VALUE), isHot := TRUE

    INTERNAL PROPERTY FreeList		AS DWORD;
		GET _GetDWORD(CDXROOT_FREELIST);
		SET _SetDWORD(CDXROOT_FREELIST, VALUE), isHot := TRUE

    INTERNAL PROPERTY Version		AS DWORD;
		GET _GetDWORD(CDXROOT_VERSION);
		SET _SetDWORD(CDXROOT_VERSION, VALUE), isHot := TRUE

    INTERNAL OVERRIDE PROPERTY KeyLength		AS WORD;
		GET _GetWORD(CDXROOT_KEYLENGTH);
		SET _SetWord(CDXROOT_KEYLENGTH, VALUE), isHot := TRUE

    INTERNAL PROPERTY Options		AS CdxOptions;
		GET (CdxOptions) _GetByte(CDXROOT_OPTIONS);
		SET _SetByte(CDXROOT_OPTIONS, VALUE), isHot := TRUE

    INTERNAL PROPERTY Signature		AS BYTE;
		GET _GetByte(CDXROOT_Sig);
		SET _SetByte(CDXROOT_Sig, VALUE), isHot := TRUE
#endregion
#region constants
        PRIVATE CONST CDXROOT_TAGLIST	:= 0	AS INT 
		PRIVATE CONST CDXROOT_FREELIST  := 4    AS INT
		PRIVATE CONST CDXROOT_VERSION	:= 8	AS INT		// to increment on modification
		PRIVATE CONST CDXROOT_KEYLENGTH	:= 12	AS INT		// Length of key
		PRIVATE CONST CDXROOT_OPTIONS	:= 14	AS INT		// CdxOptions : bit field
		PRIVATE CONST CDXROOT_Sig		:= 15   AS INT
#endregion
	END CLASS
END NAMESPACE 
