//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Collections
USING System.Collections.Generic
USING System.Text
USING XSharp.Rdd.Support

BEGIN NAMESPACE XSharp.RDD

    INTERNAL INTERFACE IRddSortWriter
        METHOD WriteSorted( sortInfo AS DBSORTINFO , obj AS SortRecord ) AS LOGIC
    
	END INTERFACE
            
            
    INTERNAL CLASS RddSortHelper
        INTERNAL _sortInfo   AS DbSortInfo
        INTERNAL _dataBuffer AS List<SortRecord>
        INTERNAL _oRdd       AS DBF

        INTERNAL PROPERTY SortInfo AS DbSortInfo GET _sortInfo
        
        INTERNAL CONSTRUCTOR( rdd AS DBF, sortInfo AS DbSortInfo , len AS LONG )
            SELF:_oRDD      := rdd
            SELF:_sortInfo := sortInfo
            SELF:_dataBuffer := List<SortRecord>{len}
            
            
        INTERNAL METHOD Add(o AS SortRecord ) AS LOGIC
            SELF:_dataBuffer:Add(o)
            RETURN TRUE
            
            
        INTERNAL METHOD Sort(ic AS IComparer<SortRecord> ) AS LOGIC
            TRY
                SELF:_dataBuffer:Sort(ic)
                RETURN TRUE
                
            CATCH ex AS Exception
                SELF:_oRdd:_dbfError(ex, SubCodes.ERDD_SORT_SORT,GenCode.EG_CORRUPTION,  "RddSortHelper.Sort") 
                
                RETURN FALSE
            END TRY
            
            
        INTERNAL METHOD Write(isw AS IRddSortWriter ) AS LOGIC
            TRY
                FOREACH o AS SortRecord IN SELF:_dataBuffer 
                    isw:WriteSorted(SELF:_sortInfo, o)
                NEXT
                RETURN TRUE
                
            CATCH ex AS Exception
                SELF:_oRdd:_dbfError(ex, SubCodes.ERDD_SORT_END,GenCode.EG_WRITE,  "RddSortHelper.Write") 
                RETURN FALSE
            END TRY
            
            
        INTERNAL METHOD Clear() AS VOID
            SELF:_dataBuffer := NULL
            SELF:_sortInfo := NULL
            
            
    END CLASS
    
    
END NAMESPACE
