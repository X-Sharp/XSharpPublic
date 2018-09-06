//
// Copyright (c) B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp.RDD
    /// <summary>DBFNTX RDD. For DBF/DBT/NTX.</summary>
    CLASS DBFNTX INHERIT DBFDBT

        Internal _ntxList AS NtxOrderList

        CONSTRUCTOR()
            SUPER()
            SELF:_ntxList := NtxOrderList{SELF}
            SELF:_oIndex := NtxOrder{SELF}
            RETURN
            
        PROPERTY SysName AS STRING GET TYPEOF(DbfNtx):ToString()	

        INTERNAL METHOD StringCompare( leftStr AS BYTE[], rightStr AS BYTE[], len AS LONG ) AS LONG
            // Should be based on curretn RuntimeState
            VAR lStr := SELF:_Encoding:GetString( leftStr )
            VAR rStr := SELF:_Encoding:GetString( rightStr )
            
            RETURN String.Compare(lstr, 0, rStr, 0, len, StringComparison.InvariantCulture)

        // Order Support 

            
    END CLASS
    
END NAMESPACE
