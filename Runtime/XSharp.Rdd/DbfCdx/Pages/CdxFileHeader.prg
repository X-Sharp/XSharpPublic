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
	INTERNAL CLASS CdxFileHeader INHERIT CdxTagHeader

    INTERNAL CONSTRUCTOR( bag AS CdxOrderBag )
        SUPER(bag, 0, "__ROOT__",NULL)
        

        METHOD Initialize() AS VOID
            SELF:FreeList   := 0
            SELF:KeySize    := 10
            SELF:RootPage   := CDXPAGE_SIZE *2
            SELF:Options    := CdxOptions.Compact | CdxOptions.Header| CdxOptions.Tag
            SELF:Signature  := 1
            SELF:KeyExprLen := 1
            SELF:ForExprPos := 1
            SELF:ForExprLen := 1
	END CLASS
END NAMESPACE 
