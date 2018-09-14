// NtxItem.prg
// Created by    : fabri
// Creation Date : 6/24/2018 8:54:48 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD

    /// <summary>
    /// The NtxItem class. (Should it be called NtxNode ?)
    /// </summary>
    INTERNAL CLASS NtxItem
        PRIVATE _oRDD       AS DBFNTX
        PRIVATE _lenKey   AS LONG
        PRIVATE _pageNum     AS LONG
        PRIVATE _recno      AS DWORD
        PRIVATE _bytesKey      AS BYTE[]
        PROTECTED _Page     AS NtxPage

        PRIVATE _hasPage    AS LOGIC
        PRIVATE _Offset     AS LONG
        PRIVATE _Pos        AS LONG
        
        PUBLIC PROPERTY Key AS STRING GET _oRDD:_Encoding:GetString(KeyBytes, 0, _lenKey)

        PUBLIC PROPERTY Pos AS LONG GET _Pos
        
        PUBLIC PROPERTY KeyBytes AS BYTE[]
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
        
        PUBLIC PROPERTY PageNo AS LONG
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
        
        PUBLIC PROPERTY Recno AS DWORD
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
        
        PUBLIC CONSTRUCTOR( keylen AS LONG , area AS DBFNTX )
            SELF:Clear()
            SELF:_lenKey := keylen
            SELF:_oRDD := area
            
            
        PUBLIC METHOD Fill( pos AS LONG , page AS NtxPage ) AS VOID
            SELF:Clear()
            SELF:_Page := page
            SELF:_hasPage := (SELF:_Page != NULL)
            SELF:_Offset := SELF:_Page:GetRef( pos )
            SELF:_Pos := pos
            
        PUBLIC METHOD Clear() AS VOID
            SELF:_recno := 0u
            SELF:_pageNum := 0L
            SELF:_bytesKey := BYTE[]{ 257 }
            SELF:_Offset := -1
            SELF:_Page := NULL
            SELF:_hasPage := FALSE
            
            
    END CLASS


END NAMESPACE // global::XSharp.RDD.Types.DbfNtx