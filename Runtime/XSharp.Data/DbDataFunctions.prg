//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING XSharp.RDD


/// <summary>Create a DbDataTable object with the data from the current workarea</summary>
/// <returns>A DbDataTable object, or NULL when the current workarea is not in use.</returns>

FUNCTION DbDataTable() AS DbDataTable
LOCAL oResult := NULL AS OBJECT
IF CoreDb.Info(DBI_RDD_OBJECT,REF oResult)
    VAR oRDD := (IRdd) oResult
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

