//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING XSharp.RDD
USING System.Data
USING System.Reflection

/// <summary>Create a DbDataTable object with the data from the current workarea</summary>
/// <returns>A DbDataTable object, or NULL when the current workarea is not in use.</returns>

Function DbDataTable() As DbDataTable
LOCAL oResult := NULL AS OBJECT
IF CoreDb.Info(DBI_RDD_OBJECT,REF oResult)
    VAR oRDD := (IRdd) oResult
    var oProp := oRDD:GetType():GetProperty("DataTable", BindingFlags.Instance+BindingFlags.IgnoreCase+BindingFlags.Public)
    IF oProp != NULL
        Return (DbDataTable) oProp:GetValue(oRDD)
    ENDIF
    RETURN DbDataTable{oRDD}
ENDIF
RETURN NULL


/// <summary>Create a DbDataSource object attached to the current workarea</summary>
/// <returns>A DbDataSource object, or NULL when the current workarea is not in use.</returns>

FUNCTION DbDataSource() AS DbDataSource
LOCAL oResult := NULL AS OBJECT
IF CoreDb.Info(DBI_RDD_OBJECT,REF oResult)
    VAR oRDD := (IRdd) oResult
    RETURN DbDataSource{oRDD}
ENDIF
RETURN NULL


FUNCTION DbTableSave(oTable AS DbDataTable) AS LOGIC
    LOCAL oResult := NULL AS OBJECT    
    IF CoreDb.Info(DBI_RDD_OBJECT,REF oResult)
        VAR oRDD := (IRdd) oResult
        RETURN oTable:Save(oRDD)
    ENDIF
    RddError.PostNoTableError(__FUNCTION__)
    RETURN FALSE


