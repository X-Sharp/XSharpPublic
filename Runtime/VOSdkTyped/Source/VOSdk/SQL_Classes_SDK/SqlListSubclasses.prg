//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
[XSharp.Internal.TypesChanged];
CLASS SQLListColumnPrivileges INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cTableName, cColName, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("ColumnPrivileges", oSQLConnection, {cQualifier, cOwner, cTableName, cColName})
        RETURN
        
END CLASS
[XSharp.Internal.TypesChanged];
CLASS SQLListColumns INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cTableName, cColName, oSQLConnection) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Columns", oSQLConnection, {cQualifier, cOwner, cTableName, cColName})
        RETURN
END CLASS


[XSharp.Internal.TypesChanged];
CLASS SQLListDatabases INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cDbName, oSQLConnection) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Databases", oSQLConnection, {cDbName})
        RETURN
END CLASS

[XSharp.Internal.TypesChanged];
CLASS SQLListForeignKeys INHERIT SQLCatalogQuery
    CONSTRUCTOR( cPQualifier, cpOwner, cPTableName, cFQualifier, cFOwner, cFTableName, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("ForeignKeys", oSQLConnection, {cPQualifier, cpOwner, cPTableName, cFQualifier, cFOwner, cFTableName})
        RETURN
END CLASS

[XSharp.Internal.TypesChanged];
CLASS SQLListIndexes INHERIT SQLCatalogQuery
    CONSTRUCTOR(  cQualifier, cOwner, cTableName, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Indexes", oSQLConnection, { cQualifier, cOwner, cTableName})
        RETURN
END CLASS


[XSharp.Internal.TypesChanged];
CLASS SQLListIndexColumns INHERIT SQLCatalogQuery
    CONSTRUCTOR(  cQualifier, cOwner, cTableName, cConstraint, cColumn, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("IndexColumns", oSQLConnection, { cQualifier, cOwner, cTableName, cConstraint, cColumn})
        RETURN
END CLASS



[XSharp.Internal.TypesChanged];
CLASS SQLListPrimaryKeys INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cTableName, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("PrimaryKeys", oSQLConnection, {cQualifier, cOwner, cTableName})
        RETURN
        
END CLASS


[XSharp.Internal.TypesChanged];
CLASS SQLListProcedures INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cProcName, cType, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Procedures", oSQLConnection, {cQualifier, cOwner, cProcName, cType})
        RETURN
        
END CLASS

[XSharp.Internal.TypesChanged];
CLASS SQLListProcedureColumns INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cProcName, cColName, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("ProcedureColumns", oSQLConnection, {cQualifier, cOwner, cProcName, cColName})
        RETURN
        
END CLASS


[XSharp.Internal.TypesChanged];
CLASS SQLListProcedureParameters INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cProcName, cParamName, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("ProcedureParameters", oSQLConnection, {cQualifier, cOwner, cProcName, cParamName})
        RETURN
        
END CLASS

[XSharp.Internal.TypesChanged];
CLASS SQLListReservedWords INHERIT SQLCatalogQuery
    CONSTRUCTOR( oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("ReservedWords", oSQLConnection, {})
        RETURN
        
END CLASS

[XSharp.Internal.TypesChanged];
CLASS SQLListRestrictions INHERIT SQLCatalogQuery
    CONSTRUCTOR( oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Restrictions", oSQLConnection, {})
        RETURN
        
END CLASS


[XSharp.Internal.TypesChanged];
CLASS SQLListSpecialColumns INHERIT SQLCatalogQuery
    CONSTRUCTOR( nColType, cQualifier, cOwner, cTableName, nScope, nNullable, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("SpecialColumns", oSQLConnection, {nColType, cQualifier, cOwner, cTableName, nScope, nNullable})
        RETURN
END CLASS

[XSharp.Internal.TypesChanged];
CLASS SQLListStatistics INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cTableName, nUnique, nAccuracy, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Statistics", oSQLConnection, {cQualifier, cOwner, cTableName, nUnique, nAccuracy})
        RETURN
END CLASS

[XSharp.Internal.TypesChanged];
CLASS SQLListTables INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cTableName, cTableType, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Tables", oSQLConnection, {cQualifier, cOwner, cTableName, cTableType})
        RETURN
        
END CLASS

[XSharp.Internal.TypesChanged];
CLASS SQLListTablePrivileges INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cTableName, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("TablePrivileges", oSQLConnection, {cQualifier, cOwner, cTableName})
        RETURN
END CLASS

[XSharp.Internal.TypesChanged];
CLASS SQLListTriggers INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cTableName, oSQLConnection) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Triggers", oSQLConnection, {cQualifier, cOwner, cTableName})
        RETURN
END CLASS


[XSharp.Internal.TypesChanged];
CLASS SQLListTypeInfo INHERIT SQLCatalogQuery
    CONSTRUCTOR( nSqlType, oSQLConnection ) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("DataTypes", oSQLConnection, {nSqlType})
        RETURN
        
END CLASS

[XSharp.Internal.TypesChanged];
CLASS SQLListUsers INHERIT SQLCatalogQuery
    CONSTRUCTOR( cUserName, oSQLConnection) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Users", oSQLConnection, {cUserName})
        RETURN
END CLASS

[XSharp.Internal.TypesChanged];
CLASS SQLListViews INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cTableName, oSQLConnection) 
        SUPER(oSQLConnection)
        SELF:_OpenTable("Views", oSQLConnection, {cQualifier, cOwner, cTableName})
        RETURN
END CLASS


[XSharp.Internal.TypesChanged];
CLASS SQLListViewColumns INHERIT SQLCatalogQuery
    CONSTRUCTOR( cQualifier, cOwner, cTableName, cColumnName, oSQLConnection) 
        SELF:_OpenTable("ViewColumns", oSQLConnection, {cQualifier, cOwner, cTableName, cColumnName})
        SUPER(oSQLConnection)
        RETURN
END CLASS

