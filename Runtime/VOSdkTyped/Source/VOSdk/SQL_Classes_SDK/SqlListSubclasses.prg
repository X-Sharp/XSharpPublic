//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

PARTIAL CLASS SQLListColumnPrivileges INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cTableName, cColName, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("ColumnPrivileges", oSQLConnection, {cQualifier, cOwner, cTableName, cColName})
        RETURN
        
END CLASS

PARTIAL CLASS SQLListColumns INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cTableName, cColName, oSQLConnection) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Columns", oSQLConnection, {cQualifier, cOwner, cTableName, cColName})
        RETURN
END CLASS



PARTIAL CLASS SQLListDatabases INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cDbName, oSQLConnection) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Databases", oSQLConnection, {cDbName})
        RETURN
END CLASS



PARTIAL CLASS SQLListForeignKeys INHERIT SQLCatalogQuery
    CONSTRUCTOR( cPQualifier, cpOwner, cPTableName, cFQualifier, cFOwner, cFTableName, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("ForeignKeys", oSQLConnection, {cPQualifier, cpOwner, cPTableName, cFQualifier, cFOwner, cFTableName})
        RETURN
END CLASS

PARTIAL CLASS SQLListIndexes INHERIT SQLCatalogQuery
    CONSTRUCTOR(  cQualifier, cOwner, cTableName, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Indexes", oSQLConnection, { cQualifier, cOwner, cTableName})
        RETURN
END CLASS


PARTIAL CLASS SQLListIndexColumns INHERIT SQLCatalogQuery
    CONSTRUCTOR(  cQualifier, cOwner, cTableName, cConstraint, cColumn, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("IndexColumns", oSQLConnection, { cQualifier, cOwner, cTableName, cConstraint, cColumn})
        RETURN
END CLASS





PARTIAL CLASS SQLListPrimaryKeys INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cTableName, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("PrimaryKeys", oSQLConnection, {cQualifier, cOwner, cTableName})
        RETURN
        
END CLASS


PARTIAL CLASS SQLListProcedures INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cProcName, cType, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Procedures", oSQLConnection, {cQualifier, cOwner, cProcName, cType})
        RETURN
        
END CLASS

PARTIAL CLASS SQLListProcedureColumns INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cProcName, cColName, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("ProcedureColumns", oSQLConnection, {cQualifier, cOwner, cProcName, cColName})
        RETURN
        
END CLASS


PARTIAL CLASS SQLListProcedureParameters INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cProcName, cParamName, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("ProcedureParameters", oSQLConnection, {cQualifier, cOwner, cProcName, cParamName})
        RETURN
        
END CLASS

PARTIAL CLASS SQLListReservedWords INHERIT SQLCatalogQuery
    CONSTRUCTOR( oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("ReservedWords", oSQLConnection, {})
        RETURN
        
END CLASS

PARTIAL CLASS SQLListRestrictions INHERIT SQLCatalogQuery
    CONSTRUCTOR( oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Restrictions", oSQLConnection, {})
        RETURN
        
END CLASS


PARTIAL CLASS SQLListSpecialColumns INHERIT SQLCatalogQuery
    CONSTRUCTOR( nColType, cQualifier, cOwner, cTableName, nScope, nNullable, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("SpecialColumns", oSQLConnection, {nColType, cQualifier, cOwner, cTableName, nScope, nNullable})
        RETURN
END CLASS

PARTIAL CLASS SQLListStatistics INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cTableName, nUnique, nAccuracy, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Statistics", oSQLConnection, {cQualifier, cOwner, cTableName, nUnique, nAccuracy})
        RETURN
END CLASS



PARTIAL CLASS SQLListTables INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cTableName, cTableType, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Tables", oSQLConnection, {cQualifier, cOwner, cTableName, cTableType})
        RETURN
        
END CLASS

PARTIAL CLASS SQLListTablePrivileges INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cTableName, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("TablePrivileges", oSQLConnection, {cQualifier, cOwner, cTableName})
        RETURN
END CLASS

PARTIAL CLASS SQLListTriggers INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cTableName, oSQLConnection) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Triggers", oSQLConnection, {cQualifier, cOwner, cTableName})
        RETURN
END CLASS


PARTIAL CLASS SQLListTypeInfo INHERIT SQLCatalogQuery
    CONSTRUCTOR( nSqlType, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("DataTypes", oSQLConnection, {nSqlType})
        RETURN
        
END CLASS

PARTIAL CLASS SQLListUsers INHERIT SQLCatalogQuery
    CONSTRUCTOR( cUserName, oSQLConnection) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Users", oSQLConnection, {cUserName})
        RETURN
END CLASS



PARTIAL CLASS SQLListViews INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cTableName, oSQLConnection) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Views", oSQLConnection, {cQualifier, cOwner, cTableName})
        RETURN
END CLASS


PARTIAL CLASS SQLListViewColumns INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cTableName, cColumnName, oSQLConnection) 
        SELF:_OpenTable("ViewColumns", oSQLConnection, {cQualifier, cOwner, cTableName, cColumnName})
        SUPER(oSQLConnection)
        RETURN
END CLASS

