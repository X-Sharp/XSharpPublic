//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="Sql.xml" path="doc/SQLListColumnPrivileges/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListColumnPrivileges INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListColumnPrivileges.ctor/*" />
    CONSTRUCTOR( cQualifier, cOwner, cTableName, cColName, oSQLConnection ) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("ColumnPrivileges", oSQLConnection, {cQualifier, cOwner, cTableName, cColName})
        RETURN


END CLASS
/// <include file="Sql.xml" path="doc/SQLListColumns/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListColumns INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListColumns.ctor/*" />
    CONSTRUCTOR( cQualifier, cOwner, cTableName, cColName, oSQLConnection) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("Columns", oSQLConnection, {cQualifier, cOwner, cTableName, cColName})
        RETURN
END CLASS




/// <include file="Sql.xml" path="doc/SQLListDatabases/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListDatabases INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListDatabases.ctor/*" />
    CONSTRUCTOR( cQualifier, cDbName, oSQLConnection) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("Databases", oSQLConnection, {cDbName})
        RETURN
END CLASS


/// <include file="Sql.xml" path="doc/SQLListForeignKeys/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListForeignKeys INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListForeignKeys.ctor/*" />
    CONSTRUCTOR( cPQualifier, cpOwner, cPTableName, cFQualifier, cFOwner, cFTableName, oSQLConnection ) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("ForeignKeys", oSQLConnection, {cPQualifier, cpOwner, cPTableName, cFQualifier, cFOwner, cFTableName})
        RETURN
END CLASS


/// <include file="Sql.xml" path="doc/SQLListIndexes/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListIndexes INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListIndexes.ctor/*" />
    CONSTRUCTOR(  cQualifier, cOwner, cTableName, oSQLConnection ) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("Indexes", oSQLConnection, { cQualifier, cOwner, cTableName})
        RETURN
END CLASS




/// <include file="Sql.xml" path="doc/SQLListIndexColumns/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListIndexColumns INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListIndexColumns.ctor/*" />
    CONSTRUCTOR(  cQualifier, cOwner, cTableName, cConstraint, cColumn, oSQLConnection ) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("IndexColumns", oSQLConnection, { cQualifier, cOwner, cTableName, cConstraint, cColumn})
        RETURN
END CLASS






/// <include file="Sql.xml" path="doc/SQLListPrimaryKeys/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListPrimaryKeys INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListPrimaryKeys.ctor/*" />
    CONSTRUCTOR( cQualifier, cOwner, cTableName, oSQLConnection ) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("PrimaryKeys", oSQLConnection, {cQualifier, cOwner, cTableName})
        RETURN


END CLASS




/// <include file="Sql.xml" path="doc/SQLListProcedures/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListProcedures INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListProcedures.ctor/*" />
    CONSTRUCTOR( cQualifier, cOwner, cProcName, cType, oSQLConnection ) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("Procedures", oSQLConnection, {cQualifier, cOwner, cProcName, cType})
        RETURN


END CLASS


/// <include file="Sql.xml" path="doc/SQLListProcedureColumns/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListProcedureColumns INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListProcedureColumns.ctor/*" />
    CONSTRUCTOR( cQualifier, cOwner, cProcName, cColName, oSQLConnection ) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("ProcedureColumns", oSQLConnection, {cQualifier, cOwner, cProcName, cColName})
        RETURN


END CLASS




/// <include file="Sql.xml" path="doc/SQLListProcedureParameters/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListProcedureParameters INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListProcedureParameters.ctor/*" />
    CONSTRUCTOR( cQualifier, cOwner, cProcName, cParamName, oSQLConnection ) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("ProcedureParameters", oSQLConnection, {cQualifier, cOwner, cProcName, cParamName})
        RETURN


END CLASS


/// <include file="Sql.xml" path="doc/SQLListReservedWords/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListReservedWords INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListReservedWords.ctor/*" />
    CONSTRUCTOR( oSQLConnection ) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("ReservedWords", oSQLConnection, {})
        RETURN


END CLASS


/// <include file="Sql.xml" path="doc/SQLListRestrictions/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListRestrictions INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListRestrictions.ctor/*" />
    CONSTRUCTOR( oSQLConnection ) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("Restrictions", oSQLConnection, {})
        RETURN


END CLASS




/// <include file="Sql.xml" path="doc/SQLListSpecialColumns/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListSpecialColumns INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListSpecialColumns.ctor/*" />
    CONSTRUCTOR( nColType, cQualifier, cOwner, cTableName, nScope, nNullable, oSQLConnection ) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("SpecialColumns", oSQLConnection, {nColType, cQualifier, cOwner, cTableName, nScope, nNullable})
        RETURN
END CLASS


/// <include file="Sql.xml" path="doc/SQLListStatistics/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListStatistics INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListStatistics.ctor/*" />
    CONSTRUCTOR( cQualifier, cOwner, cTableName, nUnique, nAccuracy, oSQLConnection ) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("Statistics", oSQLConnection, {cQualifier, cOwner, cTableName, nUnique, nAccuracy})
        RETURN
END CLASS


/// <include file="Sql.xml" path="doc/SQLListTables/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListTables INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListTables.ctor/*" />
    CONSTRUCTOR( cQualifier, cOwner, cTableName, cTableType, oSQLConnection ) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("Tables", oSQLConnection, {cQualifier, cOwner, cTableName, cTableType})
        RETURN


END CLASS


/// <include file="Sql.xml" path="doc/SQLListTablePrivileges/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListTablePrivileges INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListTablePrivileges.ctor/*" />
    CONSTRUCTOR( cQualifier, cOwner, cTableName, oSQLConnection ) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("TablePrivileges", oSQLConnection, {cQualifier, cOwner, cTableName})
        RETURN
END CLASS


/// <include file="Sql.xml" path="doc/SQLListTriggers/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListTriggers INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListTriggers.ctor/*" />
    CONSTRUCTOR( cQualifier, cOwner, cTableName, oSQLConnection) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("Triggers", oSQLConnection, {cQualifier, cOwner, cTableName})
        RETURN
END CLASS




/// <include file="Sql.xml" path="doc/SQLListTypeInfo/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListTypeInfo INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListTypeInfo.ctor/*" />
    CONSTRUCTOR( nSqlType, oSQLConnection ) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("DataTypes", oSQLConnection, {nSqlType})
        RETURN


END CLASS


/// <include file="Sql.xml" path="doc/SQLListUsers/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListUsers INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListUsers.ctor/*" />
    CONSTRUCTOR( cUserName, oSQLConnection) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("Users", oSQLConnection, {cUserName})
        RETURN
END CLASS


/// <include file="Sql.xml" path="doc/SQLListViews/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListViews INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListViews.ctor/*" />
    CONSTRUCTOR( cQualifier, cOwner, cTableName, oSQLConnection) CLIPPER
        SUPER(oSQLConnection)
        SELF:_OpenTable("Views", oSQLConnection, {cQualifier, cOwner, cTableName})
        RETURN
END CLASS




/// <include file="Sql.xml" path="doc/SQLListViewColumns/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLListViewColumns INHERIT SQLCatalogQuery
/// <include file="Sql.xml" path="doc/SQLListViewColumns.ctor/*" />
    CONSTRUCTOR( cQualifier, cOwner, cTableName, cColumnName, oSQLConnection) CLIPPER
        SELF:_OpenTable("ViewColumns", oSQLConnection, {cQualifier, cOwner, cTableName, cColumnName})
        SUPER(oSQLConnection)
        RETURN
END CLASS


