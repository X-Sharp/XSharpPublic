//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Data
using System.Diagnostics
[DebuggerDisplay( "SQLCatalog {Collection,nq}" )] ;
PARTIAL CLASS SQLCatalogQuery INHERIT SQLSelect
    PROTECT oCatalog    AS DataTable    
    PROTECT cCollection AS STRING
    CONSTRUCTOR( oSQLConnection ) 
        SUPER( NIL, oSQLConnection )
        RETURN 

    PROPERTY Collection AS STRING GET cCollection 

    METHOD _Open(oTable AS DataTable) AS VOID
        IF oCatalog == NULL 
            oCatalog := oTable
            oTable:AcceptChanges()
            IF STRING.IsNullOrEmpty(SELF:cTableName)
                cTableName := oTable:TableName
            ENDIF
        ENDIF
    

    METHOD __GoCold AS LOGIC STRICT
        RETURN TRUE
    
    METHOD Append() 
        RETURN FALSE
    
    METHOD Execute()  CLIPPER
        lBof := FALSE
        lEof := FALSE
        RETURN SELF:__Open(oCatalog, NULL)
    
    METHOD FIELDPUT( uFieldID , uValue ) AS USUAL CLIPPER
        RETURN uValue
    
    METHOD Prepare() 
        RETURN TRUE
    
    PROTECTED METHOD _OpenTable(cCollectionName AS STRING, oSQLConnection AS SqlConnection, aFilters AS ARRAY) AS VOID
        LOCAL aFilterArray AS STRING[]
        LOCAL lHasFilter AS LOGIC
        LOCAL nElement  AS DWORD
        LOCAL oTable AS DataTable
        cCollection := cCollectionName
        aFilterArray := STRING[]{(INT) Alen(aFilters)}
        FOR nElement := 1 TO alen(aFilters)
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


