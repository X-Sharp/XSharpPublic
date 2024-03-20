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
internal enum TableMode
/// <summary>The RDD operates in Table mode and uses Metadata to control the columns and rows selected etc</summary>
member Table
/// <summary>The RDD operates in Query mode and uses returns all rows and columns from the sql stetemtn</summary>
member Query
end enum


/// <summary>
/// This enum is used to specify the reason for the RDD event
/// </summary>
enum SqlRDDEventReason
/// <summary>Specifies the connection string for the database</summary>
    member ConnectionString
/// <summary>Specifies the command text to be executed</summary>
    member CommandText
/// <summary>Specifies the real name of a table</summary>
    member RealName
/// <summary>Specifies the maximum number of records to be fetched</summary>
    member MaxRecords
/// <summary>Specifies the list of columns for the table</summary>
    member ColumnList
/// <summary>Specifies the server filter for the table</summary>
    member ServerFilter
/// <summary>Specifies the order by clause for the table</summary>
    member OrderByClause
/// <summary>Specifies the where clause for the table</summary>
    member WhereClause
/// <summary>Specifies the record number column in the table</summary>
    member RecnoColumn
/// <summary>Specifies the deleted column in the table</summary>
    member DeletedColumn
/// <summary>Specifies if long field names should be used</summary>
    member LongFieldNames
/// <summary>Specifies if all columns can be updated</summary>
    member UpdateAllColumns
/// <summary>Specifies if trailing spaces should be trimmed</summary>
    member TrimTrailingSpaces
/// <summary>Specifies if updates are allowed</summary>
    member AllowUpdates
/// <summary>Specifies if memo fields should be used for where clauses (when no keycolumns are defined)</summary>
    member CompareMemo
/// <summary>Specifies the updatable columns in the table</summary>
    member UpdatableColumns
/// <summary>Specifies the key columns that should be used in where clauses</summary>
    member KeyColumns
/// <summary>Specifies the indexes for a table</summary>
    member Indexes
/// <summary>Specifies the tags for an index</summary>
    member Tags
/// <summary>Specifies the expression for an index tag</summary>
    member Expression
/// <summary>Specifies the condition for an index tag</summary>
    member Condition
/// <summary>Specifies if the index tag is unique</summary>
    member Unique
    /// <summary>Should the reccount be calculated from the highest value in the RecnoColumn</summary>
    member MaxRecnoAsRecCount
end enum


end namespace
