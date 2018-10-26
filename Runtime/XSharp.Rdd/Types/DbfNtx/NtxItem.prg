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
        PRIVATE _oRDD       AS DBFNTX
        PRIVATE _lenKey   AS LONG
        PRIVATE _pageNum     AS LONG
        PRIVATE _recno      AS DWORD
        PRIVATE _bytesKey      AS BYTE[]
        PROTECTED _Page     AS NtxPage			// Page that olds the Item

        PRIVATE _hasPage    AS LOGIC
        PRIVATE _Offset     AS LONG				// Item offset from start of Page
        PRIVATE _Pos        AS LONG				// Index of the Item in the page array of Offsets
        
		// Retrieve the Key as String
        INTERNAL PROPERTY Key AS STRING GET _oRDD:_Encoding:GetString(KeyBytes, 0, _lenKey)

        INTERNAL PROPERTY Pos AS LONG GET _Pos
        
		// Get/Set the Key info as Bytes, copied from/to the Page it belongs to
        INTERNAL PROPERTY KeyBytes AS BYTE[]
            GET
                LOCAL sourceIndex AS LONG
                //
                IF (_hasPage)
                    sourceIndex := _Offset + 8
                    Array.Copy(_Page:Bytes, sourceIndex, _bytesKey, 0, _lenKey)
                ENDIF
                RETURN _bytesKey
                
            END GET
            SET
                LOCAL destinationIndex AS LONG
                //
                Array.Copy(VALUE, _bytesKey, _lenKey)
                IF (_hasPage)
                    destinationIndex := _Offset + 8
                    Array.Copy(_bytesKey, 0, _Page:Bytes, destinationIndex, _lenKey)
                    _Page:Hot := TRUE
                ENDIF
                
            END SET
        END PROPERTY
        
		// Retrieve/set the PageNo/PageOffset of the Item
		// It is on top of the Item's bytes
		// it is the Record offset from start of page 
        INTERNAL PROPERTY PageNo AS LONG
            GET
                LOCAL pageOffSet AS LONG
                //
                IF (_pageNum > 0)
                    RETURN _pageNum
                ENDIF
                IF (_Page != NULL)
                    pageOffSet := _Offset
                    _pageNum := BitConverter.ToInt32(_Page:Bytes, pageOffSet)
                ENDIF
                RETURN _pageNum
                
            END GET
            SET
                LOCAL pageOffSet AS LONG
                //
                _pageNum := VALUE
                IF (_Page != NULL)
                    pageOffSet := _Offset
                    Array.Copy(BitConverter.GetBytes(_pageNum), 0, _Page:Bytes, pageOffSet, 4)
                    _Page:Hot := TRUE
                ENDIF
                
            END SET
        END PROPERTY
        
		// Retrieve/set the Recno of the Item
		// It is after the page_offset, which is a long, so 4 bytes after
        INTERNAL PROPERTY Recno AS DWORD
            GET
                TRY
                    IF (_recno == 0)
                        IF (_hasPage)
                            VAR startIndex := _Offset + 4
                            _recno := BitConverter.ToUInt32(_Page:Bytes, startIndex)
                        ENDIF
                        RETURN _recno
                    ENDIF
                    RETURN _recno
                    
                CATCH //Exception
                    RETURN _recno
                END TRY
                
            END GET
            SET
                LOCAL destinationIndex AS LONG
                //
                _recno := VALUE
                IF (_hasPage)
                    destinationIndex := _Offset + 4
                    Array.Copy(BitConverter.GetBytes(_recno), 0, _Page:Bytes, destinationIndex, 4)
                    _Page:Hot := TRUE
                ENDIF
                
            END SET
        END PROPERTY
        
        INTERNAL CONSTRUCTOR( keylen AS LONG , area AS DBFNTX )
            SELF:Clear()
            SELF:_lenKey := keylen
            SELF:_oRDD := area
            
            
        // Set the informations for the current Item
		INTERNAL METHOD Fill( pos AS LONG , page AS NtxPage ) AS VOID
            SELF:Clear()
            SELF:_Page := page
            SELF:_hasPage := (SELF:_Page != NULL)
            SELF:_Offset := SELF:_Page:GetRef( pos )
            SELF:_Pos := pos
            
        INTERNAL METHOD Clear() AS VOID
            SELF:_recno := 0
            SELF:_pageNum := 0
            SELF:_bytesKey := BYTE[]{ 257 }
            SELF:_Offset := -1
            SELF:_Page := NULL
            SELF:_hasPage := FALSE
            
            
    END CLASS


END NAMESPACE // global::XSharp.RDD.Types.DbfNtx
