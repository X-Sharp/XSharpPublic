//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD.CDX

    /// <summary>
    /// The CdxNode class. 
	/// The size of each item is: 8 + Key Size ( the last item doesn't contain a key value, only a pointer to a sub-tree.)
    /// The structure of an Item is 
    /// long page_offset - file offset (within ntx file) where sub-tree is. if there's no subtree, a 0 is stored.
    /// long recno       - record number for this key value (in the dbf file)
    /// char key_value[key_size]
    /// </summary>
    INTERNAL CLASS CdxNode
        PROTECTED _keyLength      AS LONG
        PROTECTED _bytesKey       AS BYTE[]
        PROTECTED _PageNo         AS LONG
        PROTECTED _RecNo          AS LONG
        PROTECTED _Pos            AS LONG				// Index of the Item in the page array of Offsets
		// Retrieve the Key as String
        INTERNAL PROPERTY Pos       AS LONG     GET _Pos
        
		// Get/Set the Key info as Bytes, copied from/to the Page it belongs to
        INTERNAL VIRTUAL PROPERTY KeyBytes AS BYTE[]
            GET
                 RETURN _bytesKey
            END GET
            SET
                Array.Copy(VALUE, _bytesKey, _keyLength)
            END SET
        END PROPERTY

        INTERNAL PROPERTY KeyText AS STRING
            GET
            VAR sb := System.Text.StringBuilder{}
            FOREACH VAR b IN SELF:KeyBytes
                IF b > 0
                    sb:Append( String.Format("{0:X2}",b))
                ENDIF
            NEXT
            sb:Append(" ")
            FOREACH VAR b IN SELF:KeyBytes
                IF b > 31 .AND. b < 128
                    sb:Append( (CHAR) b)
                ENDIF
            NEXT
            RETURN sb:ToString()
            END GET
        END PROPERTY
		// Retrieve/set the PageNo/PageOffset of the Item
		// It is on top of the Item's bytes
		// it is the Record offset from start of page 
        INTERNAL VIRTUAL PROPERTY PageNo AS LONG GET _PageNo SET _PageNo := VALUE

        INTERNAL VIRTUAL PROPERTY Recno AS LONG GET _Recno SET _Recno := VALUE
        
        INTERNAL CONSTRUCTOR( keylen AS LONG  )
            SELF:_keyLength := keylen
            SELF:Clear()

        INTERNAL VIRTUAL METHOD Clear() AS VOID
            SELF:_Recno  := 0
            SELF:_PageNo := 0
            SELF:_bytesKey  := BYTE[]{ _keyLength+1 }
            

 
    END CLASS

    INTERNAL CLASS CdxPageNode INHERIT CdxNode
        PRIVATE   _Page           AS CdxTreePage			// Page that olds the Item
        PROTECTED _Offset         AS LONG				// Item offset from start of Page

        PRIVATE  PROPERTY HasPage   AS LOGIC    GET _Page != NULL

        INTERNAL CONSTRUCTOR( keylen AS LONG  )
            SUPER( keylen )

        // Set the informations for the current Item
		INTERNAL METHOD Fill( pos AS LONG , page AS CdxTreePage ) AS VOID
            SELF:_Page   := page
            SELF:_Pos    := pos
            SELF:_Offset := pos
        INTERNAL OVERRIDE METHOD Clear() AS VOID
            SUPER:Clear()
            SELF:_Page   := NULL

       INTERNAL OVERRIDE PROPERTY KeyBytes AS BYTE[]
            GET
                return self:_Page:GetKey(_Pos)
            END GET
            SET
                
            END SET
        END PROPERTY


        INTERNAL OVERRIDE PROPERTY PageNo AS LONG
            GET
                return self:_Page:GetChildPage(_Pos)                
            END GET
            SET
            END SET
        END PROPERTY

	    // Retrieve/set the Recno of the Item
		// It is after the page_offset, which is a long, so 4 bytes after
        INTERNAL OVERRIDE PROPERTY Recno AS LONG
            GET
                return SELF:_Page:GetRecno(_Pos)
            END GET
            SET
            END SET
        END PROPERTY

    END CLASS


END NAMESPACE 
