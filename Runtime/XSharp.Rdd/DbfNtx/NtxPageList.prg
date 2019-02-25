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
    /// The NtxPageList class.
    /// </summary>
    INTERNAL SEALED CLASS NtxPageList
        PRIVATE _Pages AS Dictionary<LONG, NtxPage>
        PRIVATE _Order AS NtxOrder
        PRIVATE _hDump AS IntPtr
        
        PRIVATE METHOD _FindPage( offset AS LONG ) AS NtxPage
            LOCAL page AS NtxPage
            _pages:TryGetValue(offset, OUT page)
            RETURN page

        INTERNAL PROPERTY DumpHandle AS IntPtr GET _hDump SET _hDump := VALUE

        PRIVATE METHOD _DumpPage(page AS NtxPage) AS VOID
            IF _hDump != IntPtr.Zero .AND. ! page:Dumped
                FWrite(_hDump, page:Dump(SELF:_Order:_KeySize))
                page:Dumped := TRUE
            ENDIF
            RETURN
            
        INTERNAL CONSTRUCTOR( order AS NtxOrder )
            SELF:_Pages := Dictionary<LONG, NtxPage>{}
            SELF:_Order := order
            
            
        INTERNAL METHOD Update( pageNo AS LONG ) AS NtxPage
            LOCAL page AS NtxPage
            page := SELF:Read(pageNo)
            IF page != NULL 
                page:Hot := TRUE
            ENDIF
            RETURN page
            
            
        INTERNAL METHOD Append( pageNo AS LONG ) AS NtxPage
            LOCAL page AS NtxPage
            page := SELF:_FindPage(pageNo)
            IF page == NULL
                page := NtxPage{SELF:_Order, 0L}
                page:PageOffset := pageNo
                SELF:_Pages:Add(pageNo, page)
            ENDIF
            SELF:_dumpPage(page)
            page:Hot := TRUE
            RETURN page
            
            
        INTERNAL METHOD Read(pageNo AS LONG ) AS NtxPage
            LOCAL page AS NtxPage

            page := SELF:_FindPage(pageNo)
            IF page == NULL
                page := NtxPage{SELF:_Order, pageNo}
                SELF:_Pages:Add(pageNo, page)
            ENDIF
            SELF:_dumpPage(page)
            RETURN page
            
            
        INTERNAL METHOD Flush( keepData AS LOGIC ) AS LOGIC
            LOCAL isOk AS LOGIC

            isOk := TRUE
            TRY
                FOREACH VAR pair IN SELF:_Pages 
                    isOk := pair:Value:Write()
                    IF (!isOk)
                        EXIT
                    ENDIF
                NEXT
            CATCH AS Exception
                isOk := FALSE
            END TRY
            IF isOk .AND. !keepData
                SELF:_Pages:Clear()
            ENDIF
            RETURN isOk
            
            
        INTERNAL METHOD Write(pageNo AS LONG ) AS LOGIC
            LOCAL page AS NtxPage

            page := SELF:_FindPage(pageNo)
            IF page != NULL
                page:Hot := TRUE
                RETURN page:Write()
            ENDIF
            RETURN FALSE
            
            
    END CLASS
    
END NAMESPACE // global::XSharp.RDD.Types.DbfNtx
