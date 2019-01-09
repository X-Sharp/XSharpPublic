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
    /// The NtxItem class. (Should it be called NtxNode ?)
	/// The size of each item is: 8 + Key Size ( the last item doesn't contain a key value, only a pointer to a sub-tree.)
    /// The structure of an Item is 
    /// long page_offset - file offset (within ntx file) where sub-tree is. if there's no subtree, a 0 is stored.
    /// long recno       - record number for this key value (in the dbf file)
    /// char key_value[key_size]
    /// </summary>
    INTERNAL SEALED CLASS NtxItem
        PRIVATE _lenKey   AS LONG
        PRIVATE _bytesKey      AS BYTE[]
        PRIVATE _Page     AS NtxPage			// Page that olds the Item

        PRIVATE _Offset     AS LONG				// Item offset from start of Page
        PRIVATE _Pos        AS LONG				// Index of the Item in the page array of Offsets
        PRIVATE CONST PAGE_OFFSET  := 0 AS  LONG
        PRIVATE CONST REC_OFFSET   := 4 AS  LONG
        PRIVATE CONST DATA_OFFSET  := 8 AS LONG
		// Retrieve the Key as String
        INTERNAL PROPERTY Pos AS LONG GET _Pos
        PRIVATE PROPERTY HasPage AS LOGIC GET _Page != NULL
        
		// Get/Set the Key info as Bytes, copied from/to the Page it belongs to
        INTERNAL PROPERTY KeyBytes AS BYTE[]
            GET
                IF (SELF:HasPage)
                    Array.Copy(_Page:Bytes, _Offset + DATA_OFFSET, _bytesKey, 0, _lenKey)
                ENDIF
                RETURN _bytesKey
                
            END GET
            SET
                Array.Copy(VALUE, _bytesKey, _lenKey)
                IF (SELF:HasPage)
                    Array.Copy(_bytesKey, 0, _Page:Bytes, _Offset + DATA_OFFSET, _lenKey)
                    _Page:Hot := TRUE
                ENDIF
                
            END SET
        END PROPERTY
        
		// Retrieve/set the PageNo/PageOffset of the Item
		// It is on top of the Item's bytes
		// it is the Record offset from start of page 
        INTERNAL PROPERTY PageNo AS LONG
            GET
                LOCAL pageNum AS LONG
                IF (SELF:HasPage)
                    pageNum := BitConverter.ToInt32(_Page:Bytes, _Offset+PAGE_OFFSET)
                ELSE
                    pageNum := 0
                ENDIF
                RETURN pageNum
                
            END GET
            SET
                IF (SELF:HasPage)
                    Array.Copy(BitConverter.GetBytes(VALUE), 0, _Page:Bytes, _OffSet+PAGE_OFFSET, 4)
                    _Page:Hot := TRUE
                ENDIF
            END SET

        END PROPERTY
        
		// Retrieve/set the Recno of the Item
		// It is after the page_offset, which is a long, so 4 bytes after
        INTERNAL PROPERTY Recno AS DWORD
            GET
                TRY
                    LOCAL nRecno AS DWORD
                    IF (SELF:HasPage)
                        nRecno := BitConverter.ToUInt32(_Page:Bytes,  _Offset + REC_OFFSET)
                    ELSE
                        nRecno := 0
                    ENDIF
                    RETURN nRecno
                    
                CATCH //Exception
                    RETURN 0
                END TRY
                
            END GET
            SET
                IF (SELF:HasPage)
                    Array.Copy(BitConverter.GetBytes(VALUE), 0, _Page:Bytes, _Offset+REC_OFFSET, 4)
                    _Page:Hot := TRUE
                ENDIF
                
            END SET
        END PROPERTY
        
        INTERNAL CONSTRUCTOR( keylen AS LONG  )
            SELF:_lenKey := keylen
            SELF:_bytesKey := BYTE[]{ _lenKey+1 }
            SELF:Clear() 

        // Set the informations for the current Item
		INTERNAL METHOD Fill( pos AS LONG , page AS NtxPage ) AS VOID
            SELF:_Page := page
            SELF:_Offset := SELF:_Page:GetRef( pos )
            SELF:_Pos := pos
            
        INTERNAL METHOD Clear() AS VOID
            SELF:_Offset := -1
            SELF:_Page := NULL
 
            
    END CLASS


END NAMESPACE // global::XSharp.RDD.Types.DbfNtx
