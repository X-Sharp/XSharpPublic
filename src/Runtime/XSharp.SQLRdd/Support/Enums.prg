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
    /// <summary>
    /// This enum is used to specify the mode of the RDD
    /// </summary>
enum TableMode
    /// <summary>The RDD operates in Table mode and uses Metadata to control the columns and rows selected etc</summary>
    member Table
    /// <summary>The RDD operates in Query mode and uses returns all rows and columns from the sql stetemtn</summary>
    member Query
end enum


/// <summary>
/// This enum is used to specify the reason for the RDD event
/// </summary>
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
