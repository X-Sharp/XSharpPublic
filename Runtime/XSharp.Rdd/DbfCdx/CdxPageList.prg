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

BEGIN NAMESPACE XSharp.RDD.CDX

    /// <summary>
    /// The pageList class.
    /// </summary>
    [DebuggerDisplay("Pages: {Count}")];
    INTERNAL SEALED CLASS CdxPageList
        PRIVATE _pages AS Dictionary<LONG, LinkedListNode<CdxPage>>
        PRIVATE _lruPages AS LinkedList<CdxPage>
        PRIVATE _bag   AS CdxOrderBag
        PRIVATE _hDump AS IntPtr
        INTERNAL CONST CDXPAGE_SIZE        := 512 AS WORD
        INTERNAL CONST CDXPAGE_MAXCOUNT    := 10000 AS WORD

        INTERNAL METHOD _FindPage( offset AS LONG ) AS CdxPage
            LOCAL node AS LinkedListNode<CdxPage>
            IF SELF:_pages:TryGetValue(offset, OUT node)
                _lruPages:Remove(node)
                _lruPages:AddLast(node)
                RETURN node:Value
            ENDIF
            RETURN NULL



       INTERNAL METHOD GetPage(nPage AS Int32, nKeyLen AS WORD, tag AS CdxTag) AS CdxPage
         	LOCAL isOk AS LOGIC
            LOCAL buffer AS BYTE[]
            LOCAL oResult AS CdxPage
            oResult := SELF:_FindPage(nPage)
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
        INTERNAL PROPERTY DumpHandle AS IntPtr GET _hDump SET _hDump := value

        INTERNAL METHOD SetPage(nPage AS LONG, page AS CdxPage) AS VOID
            LOCAL oldNode AS LinkedListNode<CdxPage>
            IF SELF:_pages:TryGetValue(nPage, OUT oldNode)
                SELF:_lruPages:Remove(oldNode)
            ENDIF
            VAR newNode := LinkedListNode<CdxPage>{page}
            SELF:_lruPages:AddLast(newNode)
            SELF:_pages[nPage] := newNode

        INTERNAL CONSTRUCTOR( bag AS CdxOrderBag )
            SELF:_pages := Dictionary<LONG, LinkedListNode<CdxPage>>{}
            SELF:_lruPages := LinkedList<CdxPage>{}
            SELF:_bag   := bag

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

        INTERNAL METHOD Clear() AS VOID
            SELF:_pages:Clear()
            SELF:_lruPages:Clear()

        INTERNAL METHOD Delete(pageNo AS LONG) AS LOGIC
           Debug.Assert(_pages:ContainsKey(pageNo))
           LOCAL node AS LinkedListNode<CdxPage>
           IF SELF:_pages:TryGetValue(pageNo, OUT node)
                SELF:_pages:Remove(pageNo)
                SELF:_lruPages:Remove(node)
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
            SELF:_DumpPage(page)
            page:IsHot := TRUE
            RETURN page


        INTERNAL METHOD Read(pageNo AS LONG ) AS CdxTreePage
            LOCAL page AS CdxTreePage

            page := (CdxTreePage) SELF:_FindPage(pageNo)
            IF page == NULL
                page := (CdxTreePage) SELF:GetPage(pageNo, 0,NULL)
            ENDIF
            SELF:_DumpPage(page)
            RETURN page


        INTERNAL METHOD Flush( keepData AS LOGIC ) AS LOGIC
            LOCAL lOk := TRUE AS LOGIC
            FOREACH VAR pair IN _pages
                IF pair:Value:Value:IsHot
                    IF ! pair:Value:Value:Write()
                        lOk := FALSE
                    ENDIF
                ENDIF
            NEXT
            IF ! keepData
                WHILE _pages:Count > CDXPAGE_MAXCOUNT
                    VAR node := SELF:_lruPages:First
                    _lruPages:Remove(node)
                    _pages:Remove(node:Value:PageNo)
                END WHILE
            ENDIF
            RETURN lOk

        INTERNAL METHOD Write(pageNo AS LONG ) AS LOGIC
            // Write a single page from the buffer
            LOCAL lOk := FALSE AS LOGIC
            VAR page := SELF:_FindPage(pageNo)
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

        INTERNAL METHOD SetVersion(nVersion as DWORD) AS VOID
            FOREACH VAR page IN _pages
                page:Value:Value:Generation := nVersion
            NEXT

        INTERNAL METHOD CheckVersion(nVersion as DWORD) AS VOID
            FOREACH VAR page IN _pages
                Debug.Assert(page:Value:Value:Generation == nVersion)
            NEXT
    END CLASS

END NAMESPACE // global::XSharp.RDD.Types.DbfNtx

