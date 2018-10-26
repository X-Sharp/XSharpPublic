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
        PROTECT _Pages AS List<NtxPage>
        PROTECT _Order AS NtxOrder
        
        PRIVATE METHOD _FindPage( offset AS LONG ) AS NtxPage
            LOCAL ntxPage AS NtxPage
            //
            ntxPage := SELF:_Pages:Find( { p => p:PageOffset == offset } )
            RETURN ntxPage
            
            
        INTERNAL CONSTRUCTOR( order AS NtxOrder )
            SELF:_Pages := List<NtxPage>{}
            SELF:_Order := order
            
            
        INTERNAL METHOD Update( pageNo AS LONG ) AS NtxPage
            LOCAL ntxPage AS NtxPage
            //
            ntxPage := SELF:Read(pageNo)
            IF ( ntxPage != NULL )
                ntxPage:Hot := TRUE
            ENDIF
            RETURN ntxPage
            
            
        INTERNAL METHOD Append( offset AS LONG ) AS NtxPage
            LOCAL ntxPage AS NtxPage
            //
            ntxPage := SELF:_FindPage(offset)
            IF (ntxPage == NULL)
                ntxPage := NtxPage{SELF:_Order, 0L}
                ntxPage:PageOffset := offset
                SELF:_Pages:Add(ntxPage)
            ENDIF
            ntxPage:Hot := TRUE
            RETURN ntxPage
            
            
        INTERNAL METHOD Read(pageNo AS LONG ) AS NtxPage
            LOCAL ntxPage AS NtxPage
            //
            ntxPage := SELF:_FindPage(pageNo)
            IF (ntxPage == NULL)
                ntxPage := NtxPage{SELF:_Order, pageNo}
                SELF:_Pages:Add(ntxPage)
            ENDIF
            RETURN ntxPage
            
            
        INTERNAL METHOD Flush( keepData AS LOGIC ) AS LOGIC
            LOCAL isOk AS LOGIC
            //
            isOk := TRUE
            TRY
                FOREACH page AS NtxPage IN SELF:_Pages 
                    isOk := page:Write()
                    IF (!isOk)
                        EXIT
                    ENDIF
                NEXT
                IF (isOk)
					FFlush( SELF:_Order:_hFile )
                ENDIF
            CATCH AS Exception
                isOk := FALSE
            END TRY
            IF ((isOk) .AND. (!keepData))
                SELF:_Pages:Clear()
            ENDIF
            RETURN isOk
            
            
        INTERNAL METHOD Write(pageNo AS LONG ) AS LOGIC
            LOCAL ntxPage AS NtxPage
            //
            ntxPage := SELF:_FindPage(pageNo)
            IF (ntxPage != NULL)
                ntxPage:Hot := TRUE
                RETURN ntxPage:Write()
            ENDIF
            RETURN FALSE
            
            
    END CLASS
    
END NAMESPACE // global::XSharp.RDD.Types.DbfNtx
