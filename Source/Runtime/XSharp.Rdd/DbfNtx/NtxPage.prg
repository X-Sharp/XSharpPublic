//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Diagnostics

BEGIN NAMESPACE XSharp.RDD.NTX

    // the ntx file consists of pages of 1k. A Page contains Items (also called Nodes in some docs)
    // The first page is the Header => See NtxHeader Class
    // Based on the Key Size, we will have to calculate how many items a page can hold, and then determine the Max Number of Items
    // Each Item contains a key value, whose size depends on the Key Size.

    // The size of each item is: 8 + Key Size ( the last item doesn't contain a key value, only a pointer to a sub-tree.)
    // The structure of an Item is
    // long page_offset - file offset (within ntx file) where sub-tree is. if there's no subtree, a 0 is stored.
    // long recno       - record number for this key value (in the dbf file)
    // char key_value[key_size]

    // The number of Items per page depends on the Key Size

    // A Page is organized as follows :
    // 0x00 - (WORD) Item Count - how many items this particular page holds.
    // 0x02 - (WORD) Location of Item
    // 0x02 - (LONG) Items Offset - This is an array of <Item Count> elements, that contains the offset within the page of each Item
    // 0x?? - The area where Item values are stored

    /// <summary>
    /// The NtxPage class.
	/// This is a collection of Items
    /// </summary>

    [DebuggerDisplay("Page: {PageOffset}")];
    INTERNAL CLASS NtxPage
        INTERNAL CONST NTXPAGE_SIZE             := 1024 AS WORD
        PROTECTED _Order    AS NtxOrder
        PROTECTED _Offset   AS LONG
        PROTECTED _Hot      AS LOGIC

        PROTECTED _Bytes AS BYTE[]

        // Current Page Number = Page Offset
		INTERNAL PROPERTY PageOffset AS LONG GET SELF:_Offset SET SELF:_Offset := value
        // The locationof the next page in case this is part of the unused pages list
        INTERNAL PROPERTY NextPage   AS LONG GET SELF[ 0]:PageNo SET SELF[ 0]:PageNo := value

		// Bytes of the Page (1024)
        INTERNAL PROPERTY Bytes AS BYTE[] GET SELF:_Bytes

        INTERNAL PROPERTY Hot AS LOGIC GET SELF:_Hot SET SELF:_Hot := value

        INTERNAL METHOD Clear() AS VOID
            SELF:_Bytes := BYTE[]{NTXPAGE_SIZE}
            SELF:_Hot   := TRUE

        INTERNAL PROPERTY Dumped AS LOGIC AUTO

		// Item Count - how many items this particular page holds : a WORD stored at Offset 0x00
        INTERNAL PROPERTY NodeCount AS WORD
            GET
                RETURN BitConverter.ToUInt16( SELF:_Bytes, 0)
            END GET

            SET
                Array.Copy(BitConverter.GetBytes( value), 0, SELF:_Bytes, 0, 2)

            END SET
        END PROPERTY

        INTERNAL PROPERTY FirstKeyOffSet AS WORD
             GET
                RETURN BitConverter.ToUInt16( SELF:_Bytes, sizeof(WORD))
            END GET

            SET
                Array.Copy(BitConverter.GetBytes( value), 0, SELF:_Bytes, 2, sizeof(WORD))

            END SET
        END PROPERTY

		// Retrieve a NtxNode in the current Page, at the specified position
        INTERNAL PROPERTY SELF[ index AS LONG ] AS NtxPageNode
            GET
                LOCAL node := NULL AS NtxPageNode
                node := NtxPageNode{ SELF:_Order:KeyLength ,SELF }
                node:Pos := index
                RETURN node
            END GET
        END PROPERTY

		// Initialize the NtxPage; The pageNumber is in fact the offset of the page in the File
        INTERNAL CONSTRUCTOR( order AS NtxOrder, pageNumber AS LONG )
            //
            SELF:_Order := order
            SELF:_Offset := pageNumber
            SELF:_Bytes := BYTE[]{NTXPAGE_SIZE}
            SELF:_Hot := FALSE
            IF SELF:_Offset != 0
                SELF:Read()
            ENDIF
            RETURN

		// Read/Fill a Page
		// Move the Offset, then read 1024 bytes
        INTERNAL METHOD Read() AS LOGIC
            LOCAL isOk AS LOGIC
            //
            isOk := TRUE
            IF SELF:_Hot
                isOk := SELF:Write()
            ENDIF
            IF isOk
                isOk := SELF:_Order:ReadPage(SELF)
                SELF:Dumped := FALSE
            ENDIF
            RETURN isOk

		// Write a Page
		// Move the Offset, then write 1024 bytes
        INTERNAL METHOD Write() AS LOGIC
            LOCAL isOk AS LOGIC
            //
            isOk := TRUE
            IF !SELF:_Hot
                RETURN isOk
            ENDIF
            // Should it be <= ?
            IF SELF:_Offset < 0
                RETURN FALSE
            ENDIF
            isOk := SELF:_Order:WritePage(SELF)
            SELF:_Hot := FALSE
            RETURN isOk

		// Retrieve the Record/Item offset from start of Page
        INTERNAL METHOD GetRef( pos AS LONG ) AS WORD
             RETURN BitConverter.ToUInt16(SELF:_Bytes, (pos+1) * 2)

        // Set the Record/Item offset from start of Page
        INTERNAL  METHOD SetRef(pos AS LONG , newValue AS WORD ) AS VOID
            Array.Copy(BitConverter.GetBytes( newValue), 0, SELF:_Bytes, (pos+1) * 2, 2)
            SELF:Hot := TRUE

       INTERNAL METHOD InitRefs(uiMaxEntry AS WORD , uiEntrySize AS WORD ) AS VOID
            LOCAL offSet AS WORD
            SELF:Write( )
            offSet := (WORD) ((uiMaxEntry + 2) * 2)
            FOR VAR i := 0 TO uiMaxEntry
                SELF:SetRef(i, offSet)
                offSet += uiEntrySize
            NEXT
            NodeCount := 0
            FirstKeyOffSet := (WORD) (uiMaxEntry * 2 + 4)

        INTERNAL METHOD Dump(keyLen AS WORD) AS STRING
            VAR sb := System.Text.StringBuilder{}
            VAR item := NtxPageNode{keyLen,SELF}

            sb:AppendLine(String.Format("Page {0:X6}, # of keys: {1}", SELF:PageOffset, SELF:NodeCount))
            FOR VAR i := 0 TO SELF:NodeCount-1
                item:Pos := i
                sb:AppendLine(String.Format("Item {0,2}, Page {1:X6} Record {2,4} : {3} ", i, item:PageNo, item:Recno, item:KeyText))
            NEXT
            item:Pos := SELF:NodeCount
            sb:AppendLine(String.Format("Right page reference {0:X6}", item:PageNo))
            sb:AppendLine("")
            RETURN sb:ToString()

    END CLASS

END NAMESPACE
