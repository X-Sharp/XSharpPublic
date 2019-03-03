//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Collections
USING System.Collections.Generic
USING System.Diagnostics
USING System.Globalization
USING System.IO
USING System.Reflection
USING System.Text
USING System.Threading
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING System.Runtime.CompilerServices

BEGIN NAMESPACE XSharp.RDD.NTX

    
    INTERNAL PARTIAL SEALED CLASS NtxOrder 

        INTERNAL METHOD WritePage(page AS NtxPage) AS LOGIC
            FSeek3( SELF:_hFile, page:PageOffset, SeekOrigin.Begin )
            RETURN FWrite3(SELF:_hFile, page:Bytes, NtxPage.NTXPAGE_SIZE) == NtxPage.NTXPAGE_SIZE 


        INTERNAL METHOD ReadPage(page AS NtxPage) AS LOGIC
            FSeek3( SELF:_hFile, page:PageOffset , SeekOrigin.Begin )
            // Read Buffer
            RETURN FRead3(SELF:_hFile, page:Bytes, NtxPage.NTXPAGE_SIZE) == NtxPage.NTXPAGE_SIZE

    END CLASS
    
END NAMESPACE


