// NtxSort.prg
// Created by    : fabri
// Creation Date : 9/4/2018 6:42:42 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections
USING System.Text

BEGIN NAMESPACE XSharp.RDD

    INTERNAL CLASS NTXSortRecord
        PRIVATE _data AS BYTE[]
        PRIVATE _Recno AS DWORD
        
        INTERNAL PROPERTY Data AS BYTE[] GET _data
        
        INTERNAL PROPERTY Recno AS DWORD GET _Recno
        
        INTERNAL CONSTRUCTOR(data AS BYTE[] , uiRecno AS DWORD )
            SELF:_data := (BYTE[])data:Clone()
            SELF:_Recno := uiRecno
            
            END CLASS
            
            
            
    INTERNAL CLASS NTXSortCompare IMPLEMENTS IComparer

        PRIVATE _sortInfo AS DBSORTINFO
        PRIVATE _oRdd AS DBFNTX
        
        INTERNAL CONSTRUCTOR( rdd AS DBFNTX , info AS DBSORTINFO )
            SELF:_oRdd := rdd
            SELF:_sortInfo := info
            
            
        PUBLIC METHOD Compare(x AS OBJECT , y AS OBJECT ) AS LONG
            LOCAL nTXSortRecord AS NTXSortRecord
            LOCAL nTXSortRecord2 AS NTXSortRecord
            LOCAL dataBuffer AS BYTE[]
            LOCAL dataBuffer2 AS BYTE[]
            LOCAL diff AS LONG
            LOCAL i AS LONG
            LOCAL iLen AS LONG
            LOCAL indxFlags AS LONG
            //
            nTXSortRecord := (NTXSortRecord)x
            nTXSortRecord2 := (NTXSortRecord)y
            IF (nTXSortRecord:Recno == nTXSortRecord2:Recno)
                RETURN 0
            ENDIF
            //
            dataBuffer := nTXSortRecord:data
            dataBuffer2 := nTXSortRecord2:data
            diff := 0
            iLen := SELF:_sortInfo:Items[0]:Length
            indxFlags := SELF:_sortInfo:Items[0]:Flags
            IF ((indxFlags & DBSORTITEM.SF_ASCII) != 0)
                FOR i := 0 TO ( iLen - 1 )
                    diff := dataBuffer[i] - dataBuffer2[i]
                    IF (diff != 0)
                        EXIT
                    ENDIF
                NEXT
            ELSE
                diff := SELF:_oRdd:StringCompare(dataBuffer, dataBuffer2, iLen)
            ENDIF
            IF ((indxFlags & DBSORTITEM.SF_Descending) != 0)
                diff *= -1
            ENDIF
            IF (diff == 0)
                diff := (LONG)(nTXSortRecord:Recno - nTXSortRecord2:Recno)
            ENDIF
            RETURN diff
            
            
    END CLASS
    
    
END NAMESPACE