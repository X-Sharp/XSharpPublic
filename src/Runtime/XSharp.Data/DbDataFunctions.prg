//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING XSharp.RDD
USING System.Data
USING System.Reflection


/// <include file="XSharp.Data.Docs.xml" path="doc/DbDataTable/*" />
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



/// <include file="XSharp.Data.Docs.xml" path="doc/DbDataSource/*" />
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


