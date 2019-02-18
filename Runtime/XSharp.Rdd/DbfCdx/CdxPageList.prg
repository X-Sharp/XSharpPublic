//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD.Cdx

    /// <summary>
    /// The pageList class.
    /// </summary>
    INTERNAL SEALED CLASS CdxPageList
        PRIVATE _Pages AS List<CdxPage>
        PRIVATE _Bag   AS CdxOrderBag
        PRIVATE _hDump AS IntPtr
        INTERNAL CONST CDXPAGE_SIZE        := 512 AS WORD
        
        PRIVATE METHOD _FindPage( offset AS LONG ) AS CdxPage
            LOCAL page AS Cdxpage
            page := SELF:_Pages:Find( { p => p:PageNo == offset } )
            RETURN page

       INTERNAL  METHOD GetPage(nPage AS Int32, nKeyLen := 0 AS Int32) AS CdxPage
         	LOCAL isOk AS LOGIC
            LOCAL buffer AS BYTE[]
            buffer  := SELF:_Bag:AllocBuffer()
            isOk    := self:_Bag:Read(nPage, buffer)
			//
            // Inspect first 2 byte and determine the page
            LOCAL nType AS SHORT
            nType := BitConverter.ToInt16(buffer, 0)
            SWITCH nType
            CASE 0  // Branche
            CASE 1  // Root
                 RETURN CdxBranchePage{SELF:_Bag, nPage, buffer,nKeyLen}
            CASE 2  // Leaf
                RETURN CdxLeafPage{SELF:_Bag, nPage, buffer, nKeyLen}
            CASE 3  // List of Tags
                RETURN CdxLeafPage{SELF:_Bag, nPage, buffer, nKeyLen}
            OTHERWISE 
               RETURN CdxGeneralPage{SELF:_Bag, nPage, buffer} 
            END SWITCH
            

        INTERNAL PROPERTY DumpHandle AS IntPtr GET _hDump SET _hDump := VALUE

        PRIVATE METHOD _DumpPage(page AS CdxPage) AS VOID
            IF _hDump != IntPtr.Zero .AND. ! page:Dumped
                FWrite(_hDump, page:Dump())
                page:Dumped := TRUE
            ENDIF
            RETURN
            
        INTERNAL CONSTRUCTOR( bag AS CdxOrderBag )
            SELF:_Pages := List<CdxPage>{}
            SELF:_Bag   := bag
            
            
        INTERNAL METHOD Update( pageNo AS LONG ) AS CdxPage
            LOCAL page AS CdxPage
            page := SELF:Read(pageNo)
            IF ( page != NULL )
                page:IsHot := TRUE
            ENDIF
            RETURN page
            
            
        INTERNAL METHOD Append( pageNo AS LONG ) AS CdxPage
            LOCAL page AS CdxTreePage
            page := SELF:_FindPage(pageNo)
            IF page == NULL
                page := SELF:GetPage(pageNo, 0)
                SELF:_Pages:Add(page)
            ENDIF
            SELF:_dumpPage(page)
            page:IsHot := TRUE
            RETURN page
            
            
        INTERNAL METHOD Read(pageNo AS LONG ) AS CdxTreePage
            LOCAL page AS CdxTreePage

            page := SELF:_FindPage(pageNo)
            IF page == NULL
                page := SELF:GetPage(pageNo, 0)
                SELF:_Pages:Add(page)
            ENDIF
            SELF:_dumpPage(page)
            RETURN page
            
            
        INTERNAL METHOD Flush( keepData AS LOGIC ) AS LOGIC
            LOCAL isOk AS LOGIC

            isOk := TRUE
//            TRY
//                FOREACH page AS CdxPage IN SELF:_Pages 
//                    isOk := page:Write()
//                    IF (!isOk)
//                        EXIT
//                    ENDIF
//                NEXT
//                IF isOk
//					FFlush( SELF:_Order:_hFile )
//                ENDIF
//            CATCH AS Exception
//                isOk := FALSE
//            END TRY
//            IF isOk .AND. !keepData
//                SELF:_Pages:Clear()
//            ENDIF
            RETURN isOk
            
            
        INTERNAL METHOD Write(pageNo AS LONG ) AS LOGIC
//            LOCAL page AS CdxPage
//
//            page := SELF:_FindPage(pageNo)
//            IF page != NULL
//                page:Hot := TRUE
//                RETURN page:Write()
//            ENDIF
            RETURN FALSE
            
            
    END CLASS
    
END NAMESPACE // global::XSharp.RDD.Types.DbfNtx
