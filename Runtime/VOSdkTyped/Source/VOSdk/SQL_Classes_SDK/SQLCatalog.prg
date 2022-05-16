//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System.Data
USING System.Diagnostics
/// <include file="Sql.xml" path="doc/SQLCatalogQuery/*" />
[XSharp.Internal.TypesChanged];
[DebuggerDisplay( "SQLCatalog {Collection,nq}" )] ;
CLASS SQLCatalogQuery INHERIT SQLSelect
    PROTECT oCatalog    AS DataTable
    PROTECT cCollection AS STRING
/// <include file="Sql.xml" path="doc/SQLCatalogQuery.ctor/*" />
    CONSTRUCTOR( oSQLConnection ) CLIPPER
        SUPER( NIL, oSQLConnection )
        RETURN


/// <include file="Sql.xml" path="doc/SQLCatalogQuery.Collection/*" />
    PROPERTY Collection AS STRING GET cCollection


/// <include file="Sql.xml" path="doc/SQLCatalogQuery._Open/*" />
    METHOD _Open(oTable AS DataTable) AS VOID
        IF oCatalog == NULL
            oCatalog := oTable
            oTable:AcceptChanges()
            IF String.IsNullOrEmpty(SELF:cTableName)
                cTableName := oTable:TableName
            ENDIF
        ENDIF




 /// <exclude />
    METHOD __GoCold(lUpdateBatch AS LOGIC) AS LOGIC STRICT
        RETURN TRUE


/// <include file="Sql.xml" path="doc/SQLCatalogQuery.Append/*" />
    METHOD Append() AS LOGIC STRICT
        RETURN FALSE


/// <include file="Sql.xml" path="doc/SQLCatalogQuery.Execute/*" />
    METHOD Execute()  AS LOGIC CLIPPER
        lBof := FALSE
        lEof := FALSE
        RETURN SELF:__Open(oCatalog, NULL)


/// <include file="Sql.xml" path="doc/SQLCatalogQuery.FieldPut/*" />
    METHOD FieldPut( uFieldID AS USUAL, uValue AS USUAL) AS USUAL
        RETURN uValue


/// <include file="Sql.xml" path="doc/SQLCatalogQuery.Prepare/*" />
    METHOD Prepare()  AS LOGIC
        RETURN TRUE


    PROTECTED METHOD _OpenTable(cCollectionName AS STRING, oSQLConnection AS SQLConnection, aFilters AS ARRAY) AS VOID
        LOCAL aFilterArray AS STRING[]
        LOCAL lHasFilter := FALSE AS LOGIC
        LOCAL nElement  AS DWORD
        LOCAL oTable AS DataTable
        cCollection := cCollectionName
        aFilterArray := STRING[]{(INT) ALen(aFilters)}
        FOR nElement := 1 TO ALen(aFilters)
            IF ! Empty(aFilters[nElement])
                aFilterArray[nElement] := (STRING) aFilters[nElement]
                lHasFilter := TRUE
            ENDIF
        NEXT
        IF lHasFilter
            oTable := oSQLConnection:GetSchemaTable(cCollectionName, aFilterArray)
        ELSE
            oTable := oSQLConnection:GetSchemaTable(cCollectionName, NULL)
        ENDIF
        IF oTable == NULL_OBJECT
            THROW NotSupportedException{"Collection "+cCollectionName+" is not supported by SQL Connection"}
        ENDIF
        SELF:cTableName := cCollectionName
        oTable:TableName := cCollectionName
        SELF:_Open(oTable)
END CLASS




