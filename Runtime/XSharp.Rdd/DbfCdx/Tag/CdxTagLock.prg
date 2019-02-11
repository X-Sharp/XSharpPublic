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

BEGIN NAMESPACE XSharp.RDD.CDX

    INTERNAL PARTIAL CLASS CdxTag
        // Methods for NTX Locking
    
        METHOD Slock AS LOGIC
            RETURN SELF:_Bag:SLock()

        METHOD Xlock AS LOGIC
            RETURN SELF:_Bag:XLock()

        METHOD UnLock AS LOGIC
            SELF:_Bag:UnLock()
            RETURN TRUE
            
    END CLASS
END NAMESPACE
