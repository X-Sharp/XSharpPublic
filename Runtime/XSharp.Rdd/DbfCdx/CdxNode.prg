//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING System.Diagnostics

BEGIN NAMESPACE XSharp.RDD.CDX

    /// <summary>
    /// The CdxNode class. 
	/// The size of each item is: 8 + Key Size ( the last item doesn't contain a key value, only a pointer to a sub-tree.)
    /// The structure of an Item is 
    /// long page_offset - file offset (within ntx file) where sub-tree is. if there's no subtree, a 0 is stored.
    /// long recno       - record number for this key value (in the dbf file)
    /// char key_value[key_size]
    /// </summary>
    [DebuggerDisplay("Rec {Recno} Page {ChildPageNo} Key {KeyText}")];
    INTERNAL CLASS CdxNode
        PROTECTED _keyLength      AS LONG
        PROTECTED _bytesKey       AS BYTE[]
        PROTECTED _childPage      AS LONG
        PROTECTED _recNo          AS LONG
        PROTECTED _pos            AS WORD				// Index of the Item in the page array of Offsets
		// Retrieve the Key as String
        INTERNAL PROPERTY Pos       AS WORD     GET _pos SET _pos := VALUE
        
		// Get/Set the Key info as Bytes, copied from/to the Page it belongs to
        INTERNAL VIRTUAL PROPERTY KeyBytes AS BYTE[]
            GET
                 RETURN _bytesKey
            END GET
            SET
                Array.Copy(VALUE, _bytesKey, _keyLength)
            END SET
        END PROPERTY

        INTERNAL PROPERTY KeyText AS STRING GET KeyBytes:ToAscii(FALSE)

		// Retrieve/set the PageNo/PageOffset of the Item
		// It is on top of the Item's bytes
		// it is the Record offset from start of page 
        INTERNAL VIRTUAL PROPERTY ChildPageNo AS LONG GET _childPage SET _childPage := VALUE

        INTERNAL VIRTUAL PROPERTY Recno AS LONG GET _recNo SET _recNo := VALUE

        INTERNAL CONSTRUCTOR( keylen AS LONG , pos AS WORD )
            SELF:_keyLength := keylen
            SELF:_pos       := pos
            SELF:Clear()

        INTERNAL VIRTUAL METHOD Clear() AS VOID
            SELF:_recNo  := 0
            SELF:_childPage := 0
            SELF:_bytesKey  := BYTE[]{ _keyLength }

    END CLASS

    INTERNAL CLASS CdxPageNode INHERIT CdxNode
        PRIVATE   _Page           AS CdxTreePage			// Page that olds the Item

        PRIVATE  PROPERTY HasPage   AS LOGIC    GET _Page != NULL

        INTERNAL CONSTRUCTOR( keylen AS LONG , page AS CdxTreePage, pos AS WORD )
            SUPER( keylen , pos)
            _Page := page
            _Pos  := pos

        INTERNAL PROPERTY Page AS CdxTreePage GET _Page // SET _Page := VALUE

        INTERNAL OVERRIDE METHOD Clear() AS VOID
            SUPER:Clear()
            SELF:_Page   := NULL

       INTERNAL OVERRIDE PROPERTY KeyBytes AS BYTE[]
            GET
                RETURN SELF:_Page:GetKey(_pos)
            END GET
        END PROPERTY


        INTERNAL OVERRIDE PROPERTY ChildPageNo AS LONG
            GET
                RETURN SELF:_Page:GetChildPage(_pos)                
            END GET
        END PROPERTY

	    // Retrieve/set the Recno of the Item
		// It is after the page_offset, which is a long, so 4 bytes after
        INTERNAL OVERRIDE PROPERTY Recno AS LONG
            GET
                RETURN SELF:_Page:GetRecno(_pos)
            END GET
        END PROPERTY

    END CLASS

    INTERNAL CLASS CdxLeafPageNode INHERIT CdxPageNode
        INTERNAL CONSTRUCTOR( keylen AS LONG , page AS CdxTreePage, pos AS WORD)
            SUPER( keylen, page  , pos)
        INTERNAL OVERRIDE PROPERTY ChildPageNo AS LONG GET 0 SET 

    END CLASS

END NAMESPACE 
