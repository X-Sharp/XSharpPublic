//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD.NTX

    /// <summary>
    /// The NtxNode class. (Should it be called NtxNode ?)
	/// The size of each item is: 8 + Key Size ( the last item doesn't contain a key value, only a pointer to a sub-tree.)
    /// The structure of an Item is 
    /// long page_offset - file offset (within ntx file) where sub-tree is. if there's no subtree, a 0 is stored.
    /// long recno       - record number for this key value (in the dbf file)
    /// char key_value[key_size]
    /// </summary>
    INTERNAL CLASS NtxNode
        PROTECTED _keyLength      AS LONG
        PROTECTED _bytesKey       AS BYTE[]
        PROTECTED _PageNo         AS LONG
        PROTECTED _RecNo          AS DWORD
        PROTECTED _Offset         AS LONG				// Item offset from start of Page
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

        INTERNAL VIRTUAL PROPERTY Recno AS DWORD GET _Recno SET _Recno := VALUE
        
        INTERNAL CONSTRUCTOR( keylen AS LONG  )
            SELF:_keyLength := keylen
            SELF:Clear()

        INTERNAL VIRTUAL METHOD Clear() AS VOID
            SELF:_Recno  := 0
            SELF:_PageNo := 0
            SELF:_bytesKey  := BYTE[]{ _keyLength+1 }
            

 
    END CLASS

    INTERNAL CLASS NtxPageNode INHERIT NtxNode
        PRIVATE _Page           AS NtxPage			// Page that olds the Item
        PRIVATE CONST PAGE_OFFSET  := 0 AS  LONG
        PRIVATE CONST REC_OFFSET   := 4 AS  LONG
        PRIVATE CONST DATA_OFFSET  := 8 AS LONG

        PRIVATE  PROPERTY HasPage   AS LOGIC    GET _Page != NULL

        INTERNAL CONSTRUCTOR( keylen AS LONG  )
            SUPER( keylen )

        // Set the informations for the current Item
		INTERNAL METHOD Fill( pos AS LONG , page AS NtxPage ) AS VOID
            SELF:_Page  := page
            SELF:_Offset:= page:GetRef( pos )
            SELF:_Pos   := pos

        INTERNAL OVERRIDE METHOD Clear() AS VOID
            SUPER:Clear()
            SELF:_Offset := -1
            SELF:_Page   := NULL

       INTERNAL OVERRIDE PROPERTY KeyBytes AS BYTE[]
            GET
                IF SELF:HasPage
                    Array.Copy(_Page:Bytes, _Offset + DATA_OFFSET, _bytesKey, 0, _keyLength)
                ENDIF
               RETURN _bytesKey 
                
            END GET
            SET
                Array.Copy(VALUE, _bytesKey, _keyLength)
                IF SELF:HasPage
                    Array.Copy(_bytesKey, 0, _Page:Bytes, _Offset + DATA_OFFSET, _keyLength)
                    _Page:Hot := TRUE
                ENDIF
                
            END SET
        END PROPERTY


        INTERNAL OVERRIDE PROPERTY PageNo AS LONG
            GET
                IF SELF:HasPage
                    _PageNo := BitConverter.ToInt32(_Page:Bytes, _Offset+PAGE_OFFSET)
                ENDIF
                RETURN _PageNo
                
            END GET
            SET
                _PageNo := VALUE
                IF SELF:HasPage
                    Array.Copy(BitConverter.GetBytes(VALUE), 0, _Page:Bytes, _OffSet+PAGE_OFFSET, 4)
                    _Page:Hot := TRUE
                ENDIF
            END SET
        END PROPERTY

	    // Retrieve/set the Recno of the Item
		// It is after the page_offset, which is a long, so 4 bytes after
        INTERNAL OVERRIDE PROPERTY Recno AS DWORD
            GET
                IF SELF:HasPage
                    _Recno :=BitConverter.ToUInt32(_Page:Bytes,  _Offset + REC_OFFSET)
                ENDIF
                RETURN _Recno
            END GET
            SET
                _Recno := VALUE
                IF SELF:HasPage
                    Array.Copy(BitConverter.GetBytes(VALUE), 0, _Page:Bytes, _Offset+REC_OFFSET, 4)
                    _Page:Hot := TRUE
                ENDIF
            END SET
        END PROPERTY

    END CLASS


END NAMESPACE 
