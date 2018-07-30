//
// Copyright (c) B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp.RDD
    /// <summary>DBFNTX RDD. For DBF/DBT/NTX.</summary>
    CLASS DBFNTX INHERIT DBFDBT


        CONSTRUCTOR()
            SUPER()
            SELF:_oIndex := NtxIndex{SELF}
            RETURN
            
        PROPERTY SysName AS STRING GET TYPEOF(DbfNtx):ToString()	
        
    END CLASS
    
END NAMESPACE
