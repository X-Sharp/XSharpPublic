//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System
using System.Collections.Generic
using System.Text
using System.Data
using System.Data.Common

begin namespace XSharp.RDD.SqlRDD
enum TableMode
    member Table
    member Query
end enum


enum SqlRDDEventReason
    member ConnectionString
    member CommandText
    member RealName
    member MaxRecords
    member ColumnList
    member ServerFilter
    member OrderByClause
    member WhereClause
    member RecnoColumn
    member DeletedColumn
    member LongFieldNames
    member UpdateAllColumns
    member TrimTrailingSpaces
    member IndexTags
    member IndexInfo
    member AllowUpdates
    member CompareMemo
    member UpdatableColumns
    member KeyColumns
    member Indexes
    member Tags
    member Expression
    member Condition
    member Unique
end enum


end namespace
