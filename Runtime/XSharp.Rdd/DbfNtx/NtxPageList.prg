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
        PRIVATE _Pages AS Dictionary<LONG, LinkedListNode<NtxPage>>
        PRIVATE _lruPages AS LinkedList<NtxPage>
        PRIVATE _Order AS NtxOrder
        PRIVATE _hDump AS IntPtr
        INTERNAL CONST NTXPAGE_MAXCOUNT := 64 AS WORD

        PRIVATE METHOD _FindPage( offset AS LONG ) AS NtxPage
            LOCAL node AS LinkedListNode<NtxPage>
            IF _Pages:TryGetValue(offset, OUT node)
                _lruPages:Remove(node)
                _lruPages:AddLast(node)
                RETURN node:Value
            ENDIF
            RETURN NULL

        INTERNAL PROPERTY DumpHandle AS IntPtr GET _hDump SET _hDump := value

        PRIVATE METHOD _DumpPage(page AS NtxPage) AS VOID
            IF _hDump != IntPtr.Zero .AND. ! page:Dumped
                FWrite(_hDump, page:Dump(SELF:_Order:KeyLength))
                page:Dumped := TRUE
            ENDIF
            RETURN

        INTERNAL CONSTRUCTOR( order AS NtxOrder )
            SELF:_Pages := Dictionary<LONG, LinkedListNode<NtxPage>>{}
            SELF:_lruPages := LinkedList<NtxPage>{}
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
                VAR node := LinkedListNode<NtxPage>{page}
                SELF:_Pages:Add(pageNo, node)
                SELF:_lruPages:AddLast(node)
            ENDIF
            SELF:_DumpPage(page)
            page:Hot := TRUE
            RETURN page


        INTERNAL METHOD Read(pageNo AS LONG ) AS NtxPage
            LOCAL page AS NtxPage

            page := SELF:_FindPage(pageNo)
            IF page == NULL
                page := NtxPage{SELF:_Order, pageNo}
                VAR node := LinkedListNode<NtxPage>{page}
                SELF:_Pages:Add(pageNo, node)
                SELF:_lruPages:AddLast(node)
            ENDIF
            SELF:_DumpPage(page)
            RETURN page


        INTERNAL METHOD Flush( keepData AS LOGIC ) AS LOGIC
            LOCAL isOk AS LOGIC

            isOk := TRUE
            TRY
                FOREACH VAR pair IN SELF:_Pages
                    isOk := pair:Value:Value:Write()
                    IF (!isOk)
                        EXIT
                    ENDIF
                NEXT
            CATCH AS Exception
                isOk := FALSE
                THROW //
            END TRY
            IF isOk .AND. !keepData
                WHILE _Pages:Count > NTXPAGE_MAXCOUNT
                    VAR node := SELF:_lruPages:First
                    _lruPages:Remove(node)
                    _Pages:Remove(node:Value:PageOffset)
                END WHILE
            ENDIF
            RETURN isOk


        INTERNAL METHOD Clear() AS VOID
            _Pages:Clear()
            _lruPages:Clear()
            RETURN


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

