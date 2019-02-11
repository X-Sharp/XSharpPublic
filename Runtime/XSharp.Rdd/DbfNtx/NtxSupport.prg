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

BEGIN NAMESPACE XSharp.RDD.NTX

  // Ntx Stack item
    // Keep informations
    INTERNAL SEALED CLASS NtxStack
        INTERNAL Page   AS LONG
        INTERNAL Pos    AS WORD
        INTERNAL Count  AS WORD
        
        INTERNAL METHOD Clear() AS VOID
            SELF:Page := 0
            SELF:Count := 0
            SELF:Pos := 0
            
    END CLASS
            

    

    
END NAMESPACE
