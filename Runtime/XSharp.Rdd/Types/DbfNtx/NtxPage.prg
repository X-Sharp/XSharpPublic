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
    // 0x02 - (LONG) Items Offset - This is an array of <Item Count> elements, that contains the offset within the page of each Item
    // 0x?? - The area where Item values are stored
    
    /// <summary>
    /// The NtxPage class.
	/// This is a collection of Items
    /// </summary>

    [DebuggerDisplay("Page: {PageOffset}")];
    INTERNAL CLASS NtxPage
        PRIVATE CONST NTXPAGE_SIZE             := 1024 AS WORD
        PROTECTED _Order    AS NtxOrder
        PROTECTED _Offset   AS LONG
        PROTECTED _Hot      AS LOGIC
        
        PROTECTED _Bytes AS BYTE[]
        
        // Current Page Number = Page Offset
		INTERNAL PROPERTY PageOffset AS LONG GET SELF:_Offset SET SELF:_Offset := VALUE
        // The locationof the next page in case this is part of the unused pages list
        INTERNAL PROPERTY NextPage   AS LONG GET SELF[ 0]:PageNo SET SELF[ 0]:PageNo := VALUE

		// Bytes of the Page (1024)
        INTERNAL PROPERTY Bytes AS BYTE[] GET SELF:_Bytes
        
        INTERNAL PROPERTY Hot AS LOGIC GET SELF:_Hot SET SELF:_Hot := VALUE

        INTERNAL PROPERTY Dumped AS LOGIC AUTO

		// Item Count - how many items this particular page holds : a WORD stored at Offset 0x00
        INTERNAL PROPERTY NodeCount AS WORD
            GET
                LOCAL nCount := 0 AS WORD
                TRY
                    nCount := BitConverter.ToUInt16( SELF:_Bytes, 0)
                CATCH e AS Exception
                    Debug.WriteLine( "Ntx Error : " + e:Message )
                END TRY
                RETURN nCount
            END GET
            
            SET
                TRY
                    Array.Copy(BitConverter.GetBytes( VALUE), 0, SELF:_bytes, 0, 2)
                CATCH e AS Exception
                    Debug.WriteLine( "Ntx Error : " + e:Message )
                END TRY
                
            END SET
        END PROPERTY
        
		// Retrieve a NtxNode in the current Page, at the specified position
        INTERNAL PROPERTY SELF[ index AS LONG ] AS NtxPageNode
            GET
                LOCAL item := NULL AS NtxPageNode
                TRY
                    item := NtxPageNode{ SELF:_Order:_keySize  }
                    item:Fill( index, SELF )
                CATCH e AS Exception
                    Debug.WriteLine( "Ntx Error : " + e:Message )
                END TRY
                RETURN item
            END GET
        END PROPERTY
        
		// Initialize the NtxPage; The pageNumber is in fact the offset of the page in the File
        INTERNAL CONSTRUCTOR( order AS NtxOrder, pageNumber AS LONG )
            //
            SELF:_Order := order
            SELF:_Offset := pageNumber
            SELF:_Bytes := BYTE[]{NTXPAGE_SIZE}
            SELF:_Hot := FALSE
            IF ( SELF:_Offset != 0 )
                SELF:Read()
            ENDIF
            RETURN
            
		// Read/Fill a Page
		// Move the Offset, then read 1024 bytes
        INTERNAL METHOD Read() AS LOGIC
            LOCAL isOk AS LOGIC
            //
            isOk := TRUE
            IF ( SELF:_Hot )
                BEGIN LOCK SELF
                    isOk := SELF:Write()
                END LOCK
            ENDIF
            IF isOk
                TRY
                    // Move to top of Page
                    FSeek3( SELF:_Order:_hFile, (LONG) SELF:_Offset /*  * NtxHeader.NTXOFFSETS.SIZE    */, SeekOrigin.Begin )
                    // Read Buffer
                    isOk := FRead3(SELF:_Order:_hFile, SELF:_Bytes, NTXPAGE_SIZE) == NTXPAGE_SIZE
                    SELF:Dumped := FALSE
                CATCH e AS Exception
                    isOk := FALSE
                    Debug.WriteLine( "Ntx Error : " + e:Message )
                END TRY
            ENDIF
            RETURN isOk

		// Write a Page
		// Move the Offset, then write 1024 bytes            
        INTERNAL METHOD Write() AS LOGIC
            LOCAL isOk AS LOGIC
            //
            isOk := TRUE
            IF ( !SELF:_Hot )
                RETURN isOk
            ENDIF
            // Should it be <= ?
            IF ( SELF:_Offset < 0 )
                RETURN FALSE
            ENDIF
            TRY
                // Move to top of Page
                FSeek3( SELF:_Order:_hFile, (LONG) SELF:_Offset  /* * NtxHeader.NTXOFFSETS.SIZE    */ , SeekOrigin.Begin )
                // Write Buffer
                isOk := FWrite3(SELF:_Order:_hFile, SELF:_Bytes, NTXPAGE_SIZE) == NTXPAGE_SIZE 
                SELF:_Hot := FALSE
            CATCH e AS Exception
                isOk := FALSE
                Debug.WriteLine( "Ntx Error : " + e:Message )
            END TRY
            RETURN isOk
            
		// Retrieve the Record/Item offset from start of Page
        INTERNAL METHOD GetRef( pos AS LONG ) AS WORD
            TRY
                RETURN BitConverter.ToUInt16(SELF:_bytes, (pos+1) * 2)
            CATCH //Exception
                RETURN 0
            END TRY
            
        // Set the Record/Item offset from start of Page
        INTERNAL  METHOD SetRef(pos AS LONG , newValue AS WORD ) AS VOID
            Array.Copy(BitConverter.GetBytes( newValue), 0, SELF:_bytes, (pos+1) * 2, 2)
            SELF:Hot := TRUE

       INTERNAL METHOD InitRefs(uiMaxEntry AS WORD , uiEntrySize AS WORD ) AS VOID
            LOCAL offSet AS WORD
            SELF:Write( )
            offSet := (WORD) ((uiMaxEntry + 2) * 2)
            FOR VAR i := 0 TO uiMaxEntry
                SELF:SetRef(i, offset)
                offset += uiEntrySize
            NEXT
            NodeCount := 0


        INTERNAL METHOD Dump(keyLen AS WORD) AS STRING
            VAR sb := System.Text.StringBuilder{}
            VAR item := NtxPageNode{keyLen}
            sb:AppendLine(String.Format("Page {0}, # of keys: {1}", SELF:PageOffSet, SELF:NodeCount))
            FOR VAR i := 0 TO SELF:NodeCount-1
                item:Fill(i, SELF)
                sb:AppendLine(String.Format("Item {0,2}, Page {1,6} Record {2,4} : {3} ", i, item:PageNo, item:Recno, item:KeyText))
            NEXT
            item:Fill(SELF:NodeCount, SELF)
            sb:AppendLine(String.Format("Right page reference {0}", item:PageNo))
            RETURN sb:ToString()
            
    END CLASS
    
END NAMESPACE 
