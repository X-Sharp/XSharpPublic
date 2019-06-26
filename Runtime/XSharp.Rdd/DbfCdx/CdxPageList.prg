//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING System.Linq
USING System.Diagnostics

BEGIN NAMESPACE XSharp.RDD.Cdx

    /// <summary>
    /// The pageList class.
    /// </summary>
    [DebuggerDisplay("Pages: {Count}")];
    INTERNAL SEALED CLASS CdxPageList
        PRIVATE _pages AS Dictionary<LONG, CdxPage>
        PRIVATE _bag   AS CdxOrderBag
        PRIVATE _hDump AS IntPtr
        INTERNAL CONST CDXPAGE_SIZE        := 512 AS WORD
        INTERNAL CONST CDXPAGE_MAXCOUNT    := 64 AS WORD
        
        INTERNAL METHOD _FindPage( offset AS LONG ) AS CdxPage
            LOCAL page AS Cdxpage
            SELF:_pages:TryGetValue(offSet, OUT page)
            RETURN page

  
  
       INTERNAL METHOD GetPage(nPage AS Int32, nKeyLen AS WORD, tag AS CdxTag) AS CdxPage
         	LOCAL isOk AS LOGIC
            LOCAL buffer AS BYTE[]
            LOCAL oResult AS CdxPage
            oResult := _FindPage(nPage)
            IF oResult != NULL_OBJECT
                oResult:Tag := tag
                RETURN oResult
            ENDIF
            buffer  := SELF:_bag:AllocBuffer()
            isOk    := SELF:_bag:Read(nPage, buffer)

            IF ! isOk
                RETURN NULL
            ENDIF

			//
            // Inspect first 2 byte and determine the page
            LOCAL nType AS SHORT
            nType := BitConverter.ToInt16(buffer, 0)
            LOCAL nPT := (CdxPageType)  nType AS CdxPageType
            IF nPT:HasFlag(CdxPageType.Leaf)
                oResult := CdxLeafPage{SELF:_bag, nPage, buffer, nKeyLen}
                oResult:Tag := tag
            ELSE
                SWITCH nType
                CASE 0  // Branche
                CASE 1  // Root
                CASE 4  // VFP Branch
                CASE 5  // VFP Root
                     oResult :=  CdxBranchPage{SELF:_bag, nPage, buffer,nKeyLen}
                     oResult:Tag := tag
                OTHERWISE 
                   oResult := CdxGeneralPage{SELF:_bag, nPage, buffer}
                   oResult:Tag := tag
                END SWITCH
            ENDIF
            SELF:SetPage(nPage, oResult)
            SELF:_DumpPage(oResult)
            RETURN oResult
            
        INTERNAL PROPERTY Count AS INT GET _pages:Count
        INTERNAL PROPERTY DumpHandle AS IntPtr GET _hDump SET _hDump := VALUE

        INTERNAL METHOD SetPage(nPage AS LONG, page AS CdxPage) AS VOID
            SELF:_pages[nPage] :=  page

        INTERNAL CONSTRUCTOR( bag AS CdxOrderBag )
            SELF:_pages := Dictionary<LONG, CdxPage>{}
            SELF:_bag   := bag
            
         DESTRUCTOR()
            SELF:Flush(TRUE)

        INTERNAL METHOD Update( pageNo AS LONG ) AS CdxPage
            LOCAL page AS CdxPage
            page := SELF:Read(pageNo)
            IF page != NULL 
                page:IsHot := TRUE
            ENDIF
            RETURN page
            
         INTERNAL METHOD Add( page AS CdxPage ) AS VOID
            SELF:SetPage(page:PageNo, page)
            RETURN

        INTERNAL METHOD Delete(pageNo AS LONG) AS LOGIC
           IF _pages:ContainsKey(pageNo)
                _pages:Remove(pageNo)
                RETURN TRUE
           ENDIF
           RETURN FALSE
            
        INTERNAL METHOD Append( pageNo AS LONG ) AS CdxPage
            LOCAL page AS CdxTreePage
            page := (CdxTreePage) SELF:_FindPage(pageNo)
            IF page == NULL
                page := (CdxTreePage) SELF:GetPage(pageNo, 0,NULL)
                SELF:SetPage(pageNo, page)
            ENDIF
            SELF:_dumpPage(page)
            page:IsHot := TRUE
            RETURN page
            
            
        INTERNAL METHOD Read(pageNo AS LONG ) AS CdxTreePage
            LOCAL page AS CdxTreePage

            page := (CdxTreePage) SELF:_FindPage(pageNo)
            IF page == NULL
                page := (CdxTreePage) SELF:GetPage(pageNo, 0,NULL)
                SELF:_pages:Add(pageNo, page)
            ENDIF
            SELF:_dumpPage(page)
            RETURN page
            
            
        INTERNAL METHOD Flush( keepData AS LOGIC ) AS LOGIC
            LOCAL lOk := TRUE AS LOGIC
            FOREACH VAR pair IN _pages:ToArray()
                IF pair:Value:IsHot
                    IF ! pair:Value:Write()
                        lOk := FALSE
                    ENDIF
                ENDIF
            NEXT
            IF ! keepData
                SELF:_pages:Clear()
            ENDIF
            RETURN lOk

        INTERNAL METHOD Write(pageNo AS LONG ) AS LOGIC
            // Write a single page from the buffer
            LOCAL lOk := FALSE AS LOGIC
            VAR page := _FindPage(pageNo)
            IF page != NULL
                IF page:IsHot
                    lOk := page:Write()
                ELSE
                    lOk := TRUE
                ENDIF
            ENDIF
            RETURN lOk

        PRIVATE METHOD _DumpPage(page AS CdxPage) AS VOID
            IF _hDump != IntPtr.Zero .AND. ! page:Dumped
                FWrite(_hDump, page:Dump())
                page:Dumped := TRUE
            ENDIF
            RETURN
            
            
            
    END CLASS
    
END NAMESPACE // global::XSharp.RDD.Types.DbfNtx
