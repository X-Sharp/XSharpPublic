//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING XSharp.RDD


FUNCTION DbDataTable() AS DbDataTable
IF ! Used()
    RETURN NULL
ENDIF
LOCAL oRDD AS IRdd
oRDD := (IRdd) DbInfo(DBI_RDD_OBJECT)
RETURN DbDataTable{oRDD}


FUNCTION DbDataSource() AS DbDataSource
IF ! Used()
    RETURN NULL
ENDIF
LOCAL oRDD AS IRdd
oRDD := (IRdd) DbInfo(DBI_RDD_OBJECT)
RETURN DbDataSource{oRDD}

INTERNAL DELEGATE XSharp.DbGetData() AS OBJECT[]
