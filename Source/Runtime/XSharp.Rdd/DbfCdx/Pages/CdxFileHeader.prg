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
  	/// The root page of the file header points to the list of tags (which is a LEAF page)
  	/// The freepage points to the list of free pages at BAG level.
    /// </remarks>

	INTERNAL SEALED CLASS CdxFileHeader INHERIT CdxTagHeader
    PRIVATE CONST CDXFILEHEADER_VERSION     := 8 AS LONG
	PRIVATE CONST CDXFILEHEADER_FREELIST	:= 0x04	AS WORD		// Byte offset to next free block

    PRIVATE _freeList       AS LONG

    INTERNAL PROPERTY FreeList AS LONG ;
        GET IIF(_freeList >= 0, _freeList, 0) ;
        SET SELF:_SetLong(CDXFILEHEADER_FREELIST, VALUE), _freeList  := IIF(VALUE >= 0, VALUE, 0)


    PRIVATE METHOD _getValues AS VOID
        _freeList   := SELF:_GetLong(CDXFILEHEADER_FREELIST)
        IF _freeList < 0
            _freeList := 0
            _hot := TRUE
        ENDIF


    INTERNAL OVERRIDE METHOD Read() AS LOGIC
        LOCAL lOk AS LOGIC
        lOk := SUPER:Read()
        IF lOk
            SELF:_getValues()
        ENDIF
        RETURN lOk

    INTERNAL CONSTRUCTOR( bag AS CdxOrderBag )
        SUPER(bag, 0, "__ROOT__",NULL)


    INTERNAL PROPERTY RootVersion AS DWORD GET SELF:_GetDWordLE(CDXFILEHEADER_VERSION) SET SELF:_SetDWordLE(CDXFILEHEADER_VERSION, value)

        METHOD Initialize() AS VOID
            SELF:FreeList   := 0
            SELF:KeySize    := 10
            SELF:RootPage   := CDXPAGE_SIZE *2
            SELF:Options    := CdxOptions.Compact | CdxOptions.Header| CdxOptions.Tag
            SELF:Signature  := 1
            SELF:KeyExprLen := 1
            SELF:ForExprPos := 1
            SELF:ForExprLen := 1
            SELF:Generation := SELF:RootVersion

        INTERNAL OVERRIDE METHOD Dump() AS STRING
            LOCAL oSb AS StringBuilder
            oSb := StringBuilder{}
            oSb:AppendLine("CDX Header for "+_bag:FullPath)
            oSb:AppendLine("Options   : "+SELF:Options:ToString())
            oSb:AppendLine("KeySize   : "+SELF:KeySize:ToString()+" (size of tag names)")
            oSb:AppendLine("TagList at: 0x"+SELF:RootPage:ToString("X8"))
            oSb:AppendLine("Freelist  : 0x"+SELF:FreeList:ToString("X8"))
            oSb:AppendLine("Version   : "+SELF:RootVersion:ToString())
            RETURN oSb:ToString()
	END CLASS
END NAMESPACE
