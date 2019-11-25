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
    /// The NtxNode class. 
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
        PROTECTED _RecNo          AS LONG
        PROTECTED _Pos            AS LONG				// Index of the Item in the page array of Offsets

        INTERNAL VIRTUAL PROPERTY Pos AS LONG GET _Pos SET _Pos := VALUE        
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
            // this is only use for debuggings
            GET
            VAR sb := System.Text.StringBuilder{}
            FOREACH VAR b IN SELF:KeyBytes
                IF b > 31 .AND. b < 128
                    sb:Append( (CHAR) b)
                ELSEIF b > 0
                    sb:Append( '.')
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

    INTERNAL CLASS NtxPageNode INHERIT NtxNode
        PRIVATE _Page           AS NtxPage			// Page that olds the Item
        PROTECTED _Offset       AS LONG				// Item offset from start of Page
        PRIVATE CONST PAGE_OFFSET  := 0 AS  LONG
        PRIVATE CONST REC_OFFSET   := 4 AS  LONG
        PRIVATE CONST DATA_OFFSET  := 8 AS LONG

       INTERNAL CONSTRUCTOR( keylen AS LONG, page AS NtxPage  )
            SUPER( keylen )
            _Page := page

        INTERNAL OVERRIDE PROPERTY Pos       AS LONG 
            GET
                RETURN _Pos
            END GET
            SET
                _Pos := VALUE
                SELF:_Offset:= _Page:GetRef( _Pos )
            END SET
        END PROPERTY


       INTERNAL OVERRIDE PROPERTY KeyBytes AS BYTE[]
            GET
                Array.Copy(_Page:Bytes, _OffSet + DATA_OFFSET, _bytesKey, 0, _keyLength)
                RETURN _bytesKey 
                
            END GET
            SET
                Array.Copy(VALUE, _bytesKey, _keyLength)
                Array.Copy(_bytesKey, 0, _Page:Bytes, _OffSet + DATA_OFFSET, _keyLength)
                _Page:Hot := TRUE
                
            END SET
        END PROPERTY


        INTERNAL OVERRIDE PROPERTY PageNo AS LONG
            GET
                 _PageNo := BitConverter.ToInt32(_Page:Bytes, _OffSet+PAGE_OFFSET)
                RETURN _PageNo
                
            END GET
            SET
                _PageNo := VALUE
                Array.Copy(BitConverter.GetBytes(VALUE), 0, _Page:Bytes, _OffSet+PAGE_OFFSET, 4)
                _Page:Hot := TRUE
            END SET
        END PROPERTY

	    // Retrieve/set the Recno of the Item
		// It is after the page_offset, which is a long, so 4 bytes after
        INTERNAL OVERRIDE PROPERTY Recno AS LONG
            GET

                _Recno :=BitConverter.ToInt32(_Page:Bytes,  _OffSet + REC_OFFSET)
                RETURN _Recno
            END GET
            SET
                _Recno := VALUE
                Array.Copy(BitConverter.GetBytes(VALUE), 0, _Page:Bytes, _OffSet+REC_OFFSET, 4)
                _Page:Hot := TRUE
            END SET
        END PROPERTY

    END CLASS


END NAMESPACE 
