//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING XSharp.RDD.Support
using System.Reflection
BEGIN NAMESPACE XSharp.RDD

    /// <summary>This classes manages open database. Both databases that are opened explicitely and also databases that are opened
    /// for "free" tables that have a backlink to a database</summary>
    /// <remarks>The Databases are opened like 'normal' tables but in a separate DataSession. <br/>
    /// The DoForDataBase() method selects this special datasession, executes the action that is passed as parameter
    /// and restores the original datasession. This way you can seek, evaluate macros etc without disturbing the normal opened cursors.</remarks>
    CLASS DbcManager

        STATIC PRIVATE _databases               AS List<DbcDatabase>
        STATIC PRIVATE PROPERTY DbcDataSession  AS DataSession AUTO
        STATIC INTERNAL PROPERTY ActiveDatabase  AS DbcDatabase AUTO

        STATIC PROPERTY Databases AS IList<DbcDatabase>  GET _databases

        STATIC CONSTRUCTOR
            _databases     := List<DbcDatabase>{}
            DbcDataSession := DataSession{0, "Datasession for Database Containers"}

        /// <summary>Enhance the DBC name. Adds the extension and path when needed.</summary>
        /// <param name="cFileName">Name of the file. The extension and path are optional.</param>
        /// <returns>Name with DBC extension when needed and with full path information when found.</returns>
        STATIC METHOD ExtendDbName(cFileName as STRING) AS STRING
            LOCAL cExt := System.IO.Path.GetExtension(cFileName) AS STRING
            IF String.IsNullOrEmpty(cExt)
                cFileName := System.IO.Path.ChangeExtension(cFileName, ".DBC")
            ENDIF
            IF File(cFileName)
                cFileName := FPathName()
            ENDIF
            RETURN cFileName

        /// <summary>Open a DBC file. This does NOT make the database the active database</summary>
        /// <param name="cFileName">Filename of the database to open.</param>
        /// <param name="lShared">Should the database be opened in Shared mode</param>
        /// <param name="lReadOnly">Should the database be opened Readonly</param>
        /// <param name="lValidate">Should the structure of the database be validated.</param>
        /// <returns>TRUE when opened succesfully. When the database was already open then FALSE is returned.</returns>
        STATIC METHOD Open(cFileName as STRING, lShared as LOGIC, lReadOnly as LOGIC,lValidate AS LOGIC) AS LOGIC
            IF DbcManager.FindDatabase(cFileName) != NULL_OBJECT
                RETURN FALSE
            ENDIF
            RETURN DbcManager.DoForDatabase({ =>
                local lOk := FALSE as LOGIC
                lOk := CoreDb.UseArea(TRUE, "DBFVFP",cFileName, "", lShared,lReadOnly)
                local oValue := NULL as OBJECT
                IF lOk .and. CoreDb.Info(DBI_RDD_OBJECT,REF oValue)
                    var oRDD := (IRdd) oValue
                    IF oRDD IS IRawData VAR oRaw
                        oRaw:ReturnRawData := TRUE
                    ENDIF
                ENDIF
                IF lOk .and. lValidate
                    lOk := ValidateStructure()
                    IF ! lOk
                        var err := Error{"Database structure for file "+cFileName+" is not correct"}
                        THROW err
                    ENDIF
                ENDIF
                IF lOk
                    local name as STRING
                    local area as DWORD

                    name := RuntimeState.Workareas:CurrentWorkarea:Alias
                    area := RuntimeState.Workareas:CurrentWorkarea:Area
                    local oDb as DbcDatabase
                    oDb := DbcDatabase{name, cFileName, area}
                    oDb:ObjectID := 1
                    oDb:GetData()
                    Databases:Add(oDb)
                ENDIF
                RETURN lOk
                })

        /// <summary>Validate the DBC structure</summary>
        /// <returns>TRUE when the structure is OK, otherwise FALSE.</returns>
        STATIC METHOD ValidateStructure() AS LOGIC
            LOCAL lOk := TRUE as LOGIC
            local oRDD as IRdd
            LOCAL oValue := NULL AS OBJECT
            var dbstruct := DbcStructure
            IF CoreDb.Info(DBI_RDD_OBJECT,REF oValue)
                oRDD := (IRdd) oValue

                IF oRDD != NULL .and. oRDD:FieldCount == dbstruct:Length
                    FOR VAR nFld := 1 to dbstruct:Length
                        var fld := dbstruct[nFld-1]
                        if oRDD:FieldName(nFld) != fld:Name
                            lOk := FALSE
                            EXIT
                        ENDIF
                        local oColumnInfo as DbColumnInfo
                        oColumnInfo := (DbColumnInfo) oRDD:FieldInfo(nFld,DBS_COLUMNINFO, NULL)
                        if ! oColumnInfo:SameType(fld)
                            lOk := FALSE
                            EXIT
                        ENDIF
                    NEXT
                ENDIF

            ELSE
                lOk := FALSE
            ENDIF
            RETURN lOk

        /// <summary>Close a Database</summary>
        /// <param name="cName">Alias name of the database to close.</param>
        /// <returns>TRUE when the Database was open and was closed succesfully, otherwise FALSE.</returns>
        STATIC METHOD Close(cName as STRING) AS LOGIC
            VAR oDb := FindDatabaseByName(cName)
            LOCAL lOk := FALSE AS LOGIC
            IF oDb != NULL
                lOk := TRUE
                BEGIN LOCK Databases
                    Databases:Remove(oDb)
                    lOk := DbcManager.DoForDatabase( {=>
                        CoreDb.Select(oDb:Area, OUT VAR nOld)
                        lOk := CoreDb.CloseArea()
                        CoreDb.Select(nOld, OUT NULL)
                        RETURN lOk
                        })
                END LOCK
            ENDIF
            RETURN lOk

        /// <summary>Search a Database by file name</summary>
        /// <param name="cFileName">The fully qualified filename to look for, so MUST have an extension and path.</param>
        /// <returns>The database object (when found) otherwise NULL_OBJECT.</returns>
        STATIC METHOD FindDatabase(cFileName as STRING) AS DbcDatabase
            FOREACH var oDb in Databases
                IF String.Compare(oDb:FileName, cFileName, TRUE) == 0
                    RETURN oDb
                ENDIF
            NEXT
            RETURN NULL_OBJECT

        /// <summary>Search a Database by name</summary>
        /// <param name="cName">Alias name of the database to close.</param>
        /// <returns>The database object (when found) otherwise NULL_OBJECT.</returns>
        STATIC METHOD FindDatabaseByName(cName as STRING) AS DbcDatabase
            FOREACH var oDb in Databases
                IF String.Compare(oDb:Name, cName, TRUE) == 0
                    RETURN oDb
                ENDIF
            NEXT
            RETURN NULL_OBJECT

        [DebuggerStepThroughAttribute];
        INTERNAL STATIC METHOD DoForDatabase<T>(action AS @@Func<T>) AS T
            LOCAL old as DataSession
            old := RuntimeState.Workareas
            if old != DbcDataSession
                RuntimeState.SetDataSession(DbcDataSession)
            ENDIF
            TRY
                RETURN action()
            CATCH e AS Error
                Fail(e)
            FINALLY
                IF old != DbcDataSession
                    RuntimeState.SetDataSession(old)
                ENDIF
            END TRY
            RETURN DEFAULT(T)

        INTERNAL STATIC METHOD Fail(e AS Exception) AS VOID
            RuntimeState.LastRddError := e

        STATIC METHOD Activate(oDbc as DbcDatabase) AS DbcDatabase
            LOCAL old as DbcDatabase
            old := ActiveDatabase
            IF old != NULL_OBJECT
                old:Active := FALSE
            ENDIF
            ActiveDatabase := oDbc
            IF oDbc != NULL_OBJECT
                oDbc:Active := TRUE
            ENDIF
            RETURN old

        /// <summary>Create a new DBC file</summary>
        /// <param name="cFileName">The fully qualified filename to create.</param>
        /// <returns>TRUE when the database file was created successfully.</returns>
        STATIC METHOD CreateDatabase(cFileName as STRING) AS LOGIC
            IF FindDatabase(cFileName) != NULL_OBJECT
                RETURN FALSE
            ENDIF
            RETURN DbcManager.DoForDatabase({ =>
                LOCAL lOk as LOGIC
                LOCAL lOpen AS LOGIC
                LOCAL cIndex as STRING
                cIndex := System.IO.Path.ChangeExtension(cFileName, ".DCX")
                IF System.IO.File.Exists(cIndex)
                    System.IO.File.Delete(cIndex)
                ENDIF
                var old := RuntimeState.MemoBlockSize
                RuntimeState.MemoBlockSize := 64
                lOk := CoreDb.Create(cFileName, DbcStructure, "DBFVFP",TRUE,"XXDBCXX","",FALSE, FALSE)
                RuntimeState.MemoBlockSize := old
                IF lOk
                    lOk := CoreDb.UseArea(TRUE, "DBFVFP",cFileName, "", FALSE,FALSE)
                ENDIF
                lOpen := lOk
                cIndex := System.IO.Path.ChangeExtension(cFileName, ".DCX")
                local oInfo as DbOrderCondInfo
                oInfo := DbOrderCondInfo{}
                oInfo:ForExpression := ".NOT. DELETED()"
                IF lOk
                    CoreDb.OrdCondSet(oInfo)
                    lOk := CoreDb.OrdCreate(cIndex, DbcObject.OBJECTNAME_ORDER,DbcObject.OBJECTNAME_EXPR, NULL, FALSE,oInfo)
                ENDIF
                IF lOk
                    CoreDb.OrdCondSet(oInfo)
                    lOk := CoreDb.OrdCreate(cIndex, DbcObject.OBJECTTYPE_ORDER,DbcObject.OBJECTTYPE_EXPR, NULL, FALSE, oInfo)
                ENDIF
                IF lOk
                    _writeRecord(<OBJECT>{1,1,"Database","Database"})
                    var props := DatabasePropertyCollection{}
                    props:Add(DatabasePropertyType.Version, 10)
                    props:Add(DatabasePropertyType.Comment,"This database was created with X#")
                    CoreDb.FieldPutBytes(5, props:SaveToMemo())
                    _writeRecord(<OBJECT>{2,1,"Database","TransactionLog"})
                    _writeRecord(<OBJECT>{3,1,"Database","StoredProceduresSource"})
                    _writeRecord(<OBJECT>{4,1,"Database","StoredProceduresObject"})
                    _writeRecord(<OBJECT>{5,1,"Database","StoredProceduresDependencies"})
                    CoreDb.Commit()
                ENDIF
                IF lOpen
                    CoreDb.CloseArea()
                ENDIF
                RETURN lOk
                })

        PRIVATE STATIC METHOD _writeRecord(data as OBJECT[]) as void
            CoreDb.Append(TRUE)
            FOR var nField := 0 to data:Length-1
                CoreDb.FieldPut((DWORD) nField+1, data[nField])
            NEXT
            RETURN
        PRIVATE STATIC _dbcFields as RddFieldInfo[]

        PRIVATE STATIC PROPERTY DbcStructure AS RddFieldInfo[]
            GET
            IF _dbcFields == NULL
                LOCAL aFields as List<RddFieldInfo>
                aFields := List<RddFieldInfo>{}
                aFields:Add(RddFieldInfo{"OBJECTID","I", 4, 0})
                aFields:Add(RddFieldInfo{"PARENTID","I", 4, 0})
                aFields:Add(RddFieldInfo{"OBJECTTYPE","C", 10, 0})
                aFields:Add(RddFieldInfo{"OBJECTNAME","C", 128, 0})
                aFields:Add(RddFieldInfo{"PROPERTY","M:B", 4, 0})
                aFields:Add(RddFieldInfo{"CODE","M:B", 4, 0})
                aFields:Add(RddFieldInfo{"RIINFO","C", 6, 0})
                aFields:Add(RddFieldInfo{"USER","M", 4, 0})
                _dbcFields := aFields:ToArray()
            ENDIF
            RETURN _dbcFields
            END GET
        END PROPERTY

    END CLASS

    /// <summary>Class that stores information about a database.</summary>
    /// <remarks>When opened the list of tables, views and connections is populated. Fields, relations etc are not read.</remarks>
    [DebuggerDisplay("{ObjectType,nq} {Name}")];
    CLASS DbcDatabase INHERIT DbcObject
        /// <summary>Is the database active ?</summary>
        PROPERTY Active         AS LOGIC AUTO
        /// <summary>Area number for the cursor in the Databases Datasession</summary>
        PROPERTY Area           AS DWORD AUTO
        /// <summary>Alias name for the database</summary>
        PROPERTY Name           AS STRING AUTO
        /// <summary>Filename for the database</summary>
        PROPERTY FileName       AS STRING AUTO

        /// <summary>List of tables</summary>
        PROPERTY Tables as ICollection<DbcTable> GET _tables:Values
        PRIVATE _tables         AS Dictionary<String, DbcTable>

        /// <summary>List of connections</summary>
        PROPERTY Connections as ICollection<DbcConnection>  GET _connections:Values
        PRIVATE _connections    AS Dictionary<String, DbcConnection>

        /// <summary>List of views</summary>
        PROPERTY Views          AS ICollection<DbcView> GET _views:Values
        PRIVATE _views          AS Dictionary<String, DbcView>

        PRIVATE _other          AS Dictionary<String, DbcObject>

        INTERNAL CONSTRUCTOR(cName as STRING, cFileName as STRING, nArea as DWORD)
            SELF:_tables         := Dictionary<String, DbcTable>{StringComparer.OrdinalIgnoreCase}
            SELF:_views          := Dictionary<String, DbcView>{StringComparer.OrdinalIgnoreCase}
            SELF:_connections    := Dictionary<String, DbcConnection>{StringComparer.OrdinalIgnoreCase}
            SELF:_other          := Dictionary<String, DbcObject>{StringComparer.OrdinalIgnoreCase}
            SELF:Name           := cName
            SELF:FileName       := cFileName
            SELF:Area           := nArea
            SELF:Active         := FALSE
            RETURN


        PRIVATE METHOD FindObject<T>(oColl as Dictionary<String, T>, cName as STRING) AS T WHERE T IS DbcObject
            IF oColl:ContainsKey(cName)
                var oObject := oColl[cName]
                oObject:GetData()
                RETURN oObject
            ENDIF
            RETURN NULL

        PRIVATE METHOD FindObject<T>(oColl as List<T>, cName as STRING) AS T WHERE T IS DbcObject
            FOREACH var oObject in oColl
                IF String.Compare(oObject:ObjectName, cName,TRUE) == 0
                    oObject:GetData()
                    RETURN oObject
                ENDIF
            NEXT
            RETURN NULL
        /// <summary>Find a connection in the Database container.</summary>
        /// <param name="cConnection">The connection name to look for.</param>
        PUBLIC METHOD FindConnection(cConnection as STRING) AS DbcConnection
            RETURN SELF:FindObject(SELF:_connections, cConnection)

        /// <summary>Find a table in the Database container.</summary>
        /// <param name="cTable">The table name to look for.</param>
        PUBLIC METHOD FindTable(cTable as STRING) AS DbcTable
            RETURN SELF:FindObject(SELF:_tables, cTable)

        /// <summary>Find a view in the Database container.</summary>
        /// <param name="cView">The view name to look for.</param>
        PUBLIC METHOD FindView(cView as STRING) AS DbcView
            RETURN SELF:FindObject(SELF:_views, cView)

        /// <summary>Loads the tables, views and connections from the DBC file</summary>
        OVERRIDE METHOD GetData() AS VOID
            var aChildren := SELF:LoadChildren(SELF:Area)
            FOREACH var oChild in aChildren
                IF oChild is DbcTable VAR oTable
                    _tables:Add(oChild:ObjectName, oTable)
                ELSEIF oChild is DbcView VAR oView
                    _views:Add(oChild:ObjectName, oView)
                ELSEIF oChild is DbcConnection VAR oConn
                    _connections:Add(oChild:ObjectName, oConn)
                ELSEIF oChild is DbcObject
                    IF oChild:ObjectName == NAME_DATABASE
                        SELF:Properties := oChild:Properties
                        SELF:ObjectName := oChild:ObjectName
                        SELF:ObjectType := oChild:ObjectType
                    ELSE
                        _other:Add(oChild:ObjectName, oChild)
                    ENDIF
                ENDIF
            NEXT


        /// <summary>Worker method for DbGetProp() </summary>
        PUBLIC METHOD GetProp(cName as STRING, cType as STRING, cProp as STRING) AS OBJECT
            SWITCH cType:ToLower()
            CASE "connection"
                var conn := SELF:FindConnection(cName)
                IF conn != NULL
                    RETURN conn:Properties:GetValue(cProp)
                ELSE
                    THROW Error.ArgumentError(__FUNCTION__, nameof(cName), "Could not find Connection '"+cName+"'")
                ENDIF
            CASE "database"
                RETURN NIL
            CASE "field"
                var names := cName:Split( '.')
                if (names:Length == 1)
                    THROW Error.ArgumentError(__FUNCTION__, nameof(cName), "Field Name must be specified as <Table>.FieldName or <View>.FieldName")
                ENDIF
                LOCAL fields := NULL as List<DbcField>
                var table := SELF:FindTable(names[0]:Trim())
                if table != NULL
                    fields := table:Fields
                else
                    var view := SELF:FindView(names[0]:Trim())
                    if view != NULL
                        fields := view:Fields
                    endif
                endif
                if fields == null
                    THROW Error.ArgumentError(__FUNCTION__, nameof(cName), "Could not find table or view '"+names[0]+"'")
                endif
                var oField := SELF:FindObject(fields, names[1]:Trim())
                if oField != NULL
                    RETURN oField:Properties:GetValue(cProp)
                ELSE
                    THROW Error.ArgumentError(__FUNCTION__, nameof(cName), "Could not find field '"+cName+"'")
                endif

            CASE "table"
                var table := SELF:FindTable(cName)
                IF table != NULL
                    RETURN table:Properties:GetValue(cProp)
                ELSE
                    THROW Error.ArgumentError(__FUNCTION__, nameof(cName), "Could not find Table '"+cName+"'")
                ENDIF
            CASE "view"
                var view := SELF:FindView(cName)
                IF view != NULL
                    RETURN view:Properties:GetValue(cProp)
                ELSE
                    THROW Error.ArgumentError(__FUNCTION__, nameof(cName), "Could not find View '"+cName+"'")
                ENDIF
            END SWITCH
            RETURN NULL

        /// <summary>-- todo --</summary>
        /// <summary>Worker method for DbSetProp() </summary>
        PUBLIC METHOD SetProp(cName as STRING, cType as STRING, cProp as STRING, ePropertyValue as OBJECT) AS LOGIC
            // check for readonly
            // check for shared. When shared make sure that DBC has not been changed in the meantime
            // finally set/add the property and persist the property to the DBC file.
            RETURN FALSE


    END CLASS

    /// <summary>Class that stores information about a table in a DBC</summary>
    /// <remarks>When opened the Fields, Indexes and Relations are not read. They need to be explicitely loaded with a call to GetData()</remarks>
    CLASS DbcTable INHERIT DbcObject
        /// <summary>The collection of fields</summary>
        PROPERTY Fields     AS List<DbcField> AUTO
        /// <summary>The collection of indexes</summary>
        PROPERTY Indexes    AS List<DbcIndex> AUTO
        /// <summary>The collection of relations</summary>
        PROPERTY Relations  AS List<DbcRelation> AUTO
        PRIVATE _loaded     AS LOGIC
        /// <summary>The database in which this table is defined.</summary>
        PROPERTY Database   As DbcDatabase GET (DbcDatabase) SELF:Parent
        /// <summary>The Table path as stored in the DBC file.</summary>
        PROPERTY Path       as STRING GET Properties:GetValue<STRING>(DatabasePropertyType.Path)
        CONSTRUCTOR()
            SUPER()
            SELF:Fields     := List<DbcField>{}
            SELF:Indexes    := List<DbcIndex>{}
            SELF:Relations  := List<DbcRelation>{}
            SELF:_loaded    := FALSE

        OVERRIDE METHOD GetData() AS VOID
            IF ! self:_loaded
                var aChildren := SELF:LoadChildren(Database:Area)
                FOREACH var oChild in aChildren
                    IF oChild is DbcField VAR oField
                        Fields:Add(oField)
                    ELSEIF oChild is DbcIndex VAR oIndex
                        Indexes:Add(oIndex)
                    ELSEIF oChild is DbcRelation VAR oRelation
                        Relations:Add(oRelation)
                    ELSE
                        NOP
                    ENDIF
                NEXT
                self:_loaded := TRUE
            ENDIF
            RETURN


    END CLASS

    /// <summary>Class that stores information about a view in a DBC</summary>
    /// <remarks>When opened the Fields etc are not read. They need to be explicitely loaded with a call to GetData()</remarks>
    CLASS DbcView INHERIT DbcObject
        /// <summary>The collection of fields</summary>
        PROPERTY Fields     AS List<DbcField> AUTO
        /// <summary>The database in which this table is defined.</summary>
        PROPERTY Database   As DbcDatabase GET (DbcDatabase) SELF:Parent

        PRIVATE _loaded     AS LOGIC
        CONSTRUCTOR
            SUPER()
            SELF:Fields     := List<DbcField>{}
            SELF:_loaded    := FALSE

        OVERRIDE METHOD GetData() AS VOID
            IF ! self:_loaded
                var aChildren := SELF:LoadChildren(Database:Area)
                FOREACH var oChild in aChildren
                    IF oChild is DbcField VAR oField
                        Fields:Add(oField)
                    ELSE
                        NOP
                    ENDIF
                NEXT
            ENDIF
            RETURN
    END CLASS

    /// <summary>Class that stores information about a field in a DBC</summary>
    CLASS DbcField INHERIT DbcObject
        CONSTRUCTOR
            SUPER()
    END CLASS

    /// <summary>Class that stores information about an index in a DBC</summary>
    CLASS DbcIndex INHERIT DbcObject
        CONSTRUCTOR
            SUPER()
    END CLASS

    /// <summary>Class that stores information about an ODBC connection in a DBC</summary>
    CLASS DbcConnection INHERIT DbcObject
        CONSTRUCTOR
            SUPER()
    END CLASS

    /// <summary>Class that stores information about a relation in a DBC</summary>
    CLASS DbcRelation INHERIT DbcObject
       /// <summary>The relational rules for Insert, Update, Delete.</summary>
        PROPERTY RIInfo AS STRING AUTO
        CONSTRUCTOR
            SUPER()
        OVERRIDE METHOD Read() AS LOGIC
            IF SUPER:Read()
                SELF:RIInfo := ((STRING) SELF:ReadField(POS_RIINFO)):Trim()
                RETURN TRUE
            ENDIF
            RETURN FALSE


    END CLASS
    /// <summary>Class that stores other info about Databases, such as the source and object code for Database </summary>
    CLASS DbcOther INHERIT DbcObject
        PROPERTY Code   AS Byte[] AUTO
        PROPERTY Source AS STRING AUTO

        OVERRIDE METHOD Read() AS LOGIC
          IF SUPER:Read()
                SELF:Code   := (BYTE[]) SELF:ReadField(POS_CODE)
                IF SELF:ObjectName == REC_SPSOURCE
                    IF SELF:Code != NULL .and. SELF:Code:Length > 8
                        SELF:Source := RuntimeState.WinEncoding:GetString(SELF:Code, 8, SELF:Code:Length-8)
                    ELSE
                        SELF:Source := ""
                    ENDIF

                ENDIF
                RETURN TRUE
          ENDIF
          RETURN FALSE

    END CLASS


    /// <summary>Base class for objects read from the Database Container</summary>
    [DebuggerDisplay("{ObjectType,nq} {ObjectName}")];
    ABSTRACT CLASS DbcObject
        INTERNAL CONST POS_OBJECTID        := 1 AS DWORD
        INTERNAL CONST POS_PARENTID        := 2 AS DWORD
        INTERNAL CONST POS_OBJECTTYPE      := 3 AS DWORD
        INTERNAL CONST POS_OBJECTNAME      := 4 AS DWORD
        INTERNAL CONST POS_PROPERTY        := 5 AS DWORD
        INTERNAL CONST POS_CODE            := 6 AS DWORD
        INTERNAL CONST POS_RIINFO          := 7 AS DWORD
        INTERNAL CONST POS_USER            := 8 AS DWORD
        INTERNAL CONST OBJECTNAME_ORDER    := "OBJECTNAME" AS STRING
        INTERNAL CONST OBJECTNAME_EXPR     := "STR(parentid)+objecttype+LOWER(objectname)" AS STRING
        INTERNAL CONST OBJECTTYPE_ORDER    := "OBJECTTYPE" AS STRING
        INTERNAL CONST OBJECTTYPE_EXPR     := "STR(parentid)+objecttype" AS STRING
        INTERNAL CONST NAME_TABLE          := "Table" AS STRING
        INTERNAL CONST NAME_DATABASE       := "Database" AS STRING
        INTERNAL CONST NAME_VIEW           := "View" AS STRING
        INTERNAL CONST NAME_CONNECTION     := "Connection" AS STRING
        INTERNAL CONST NAME_FIELD          := "Field" AS STRING
        INTERNAL CONST NAME_INDEX          := "Index" AS STRING
        INTERNAL CONST NAME_RELATION       := "Relation" AS STRING
        INTERNAL CONST REC_SPSOURCE        := "StoredProceduresSource" as STRING
        INTERNAL CONST REC_SPCode          := "StoredProceduresObject" as STRING


        PROPERTY ObjectID AS LONG AUTO
        PROPERTY ParentID AS LONG AUTO
        PROPERTY ObjectType AS STRING AUTO
        PROPERTY ObjectName AS STRING AUTO
        PROPERTY Parent     AS OBJECT AUTO
        PROPERTY ObjectKey  AS STRING GET ObjectID:ToString():PadLeft(10,' ')
        PROPERTY Properties AS DatabasePropertyCollection
            GET
                if _lazyProperties == NULL
                    _lazyProperties := DatabasePropertyCollection{}
                endif
                return _lazyProperties
            END GET
            SET
                _lazyProperties := value
            END SET
        END PROPERTY
        PRIVATE _lazyProperties  := NULL as DatabasePropertyCollection
        PROPERTY HasProperties as LOGIC GET _lazyProperties != NULL

        CONSTRUCTOR
            Properties := DatabasePropertyCollection{}
        VIRTUAL METHOD Read() AS LOGIC
            // Assume current area is the DBC
            TRY
                SELF:ObjectID   := (INT) SELF:ReadField(POS_OBJECTID)
                SELF:ParentID   := (INT) SELF:ReadField(POS_PARENTID)
                SELF:ObjectType := ((STRING) SELF:ReadField(POS_OBJECTTYPE)):Trim()
                SELF:ObjectName := ((STRING) SELF:ReadField(POS_OBJECTNAME)):Trim()
                SELF:DecodeProperties()
            CATCH  as Exception
                RETURN FALSE
            END TRY
            RETURN TRUE

        METHOD ReadChild() AS DbcObject
            VAR cType := (STRING) SELF:ReadField(POS_OBJECTTYPE)
            cType := cType:Trim()
            SWITCH cType
            CASE NAME_TABLE
                RETURN DbcTable{}
            CASE NAME_VIEW
                RETURN DbcView{}
            CASE NAME_CONNECTION
                RETURN DbcConnection{}
            CASE NAME_FIELD
                RETURN DbcField{}
            CASE NAME_INDEX
                RETURN DbcIndex{}
            CASE NAME_RELATION
                RETURN DbcRelation{}
            CASE NAME_DATABASE
            OTHERWISE
                RETURN DbcOther{}
            END SWITCH


        METHOD ReadField (nField as DWORD) AS OBJECT
            LOCAL oValue := NULL AS OBJECT
            IF CoreDb.FieldGet(nField, REF oValue)
                RETURN oValue
            ENDIF
            THROW RuntimeState.LastRddError

        METHOD DecodeProperties() AS VOID
            var oValue := SELF:ReadField(POS_PROPERTY)
            if (oValue is byte[] VAR bProps)
                bProps := (byte[]) oValue
                SELF:Properties:FillFromMemo(bProps)
            ELSEIF oValue is string VAR strValue
                 NOP
            ELSE
                 NOP
            ENDIF

            RETURN


        METHOD LoadChildren (nArea as DWORD) AS IList<DbcObject>
            RETURN DbcManager.DoForDatabase( { =>
                LOCAL nOld as DWORD
                LOCAL lOk as LOGIC
                VAR aChildren := List<DbcObject>{}
                lOk := CoreDb.Select(nArea, OUT nOld)
                lOk := CoreDb.OrdSetFocus(NULL, OBJECTTYPE_ORDER)
                lOk := CoreDb.Seek(SELF:ObjectKey,FALSE,FALSE)
                IF lOk
                    DO WHILE ! CoreDb.Eof()
                        VAR iValue := (INT) SELF:ReadField(POS_PARENTID)
                        IF iValue != SELF:ObjectID
                            EXIT
                        ENDIF
                        var oChild := SELF:ReadChild()
                        IF oChild != NULL_OBJECT
                            oChild:Read()
                            oChild:Parent := SELF
                            aChildren:Add(oChild)
                        ENDIF
                        CoreDb.Skip(1)
                    ENDDO
                ENDIF
                CoreDb.Select(nOld, OUT NULL)
                RETURN aChildren
                })


        VIRTUAL METHOD GetData() AS VOID
            RETURN


    END CLASS
END NAMESPACE


/// <summary>Helper class to open, close and select a database</summary>
STATIC CLASS XSharp.RDD.Dbc

    /// <summary>Open a database.</summary>
    STATIC METHOD Open(cFileName AS STRING, lShared AS LOGIC, lReadOnly as LOGIC, lValidate as LOGIC) AS LOGIC
        LOCAL lOpen := FALSE as LOGIC
        cFileName := DbcManager.ExtendDbName(cFileName)
        IF File(cFileName)
            lOpen := DbcManager.Open(cFileName, lShared, lReadOnly,lValidate)
        ENDIF
        RETURN lOpen

    /// <summary>Find a database by name.</summary>
    STATIC METHOD FindDatabase(cFileName as STRING) AS DbcDatabase
        LOCAL oDb as DbcDatabase
        oDb := DbcManager.FindDatabase(cFileName)
        RETURN oDb


    /// <summary>Create a database.</summary>
    STATIC METHOD Create(cFileName as STRING) AS LOGIC
        cFileName := DbcManager.ExtendDbName(cFileName)
        RETURN DbcManager.CreateDatabase(cFileName)


    /// <summary>Select a database.</summary>
    STATIC METHOD Select(cDatabaseName as STRING) AS DbcDatabase
        LOCAL oDb as DbcDatabase
        oDb := DbcManager.FindDatabaseByName(cDatabaseName)
        DbcManager.Activate(oDb)
        RETURN oDb

    /// <summary>DeSelect a database.</summary>
    STATIC METHOD Select() AS VOID
        DbcManager.Activate(NULL)
        RETURN

    /// <summary>Check if a database is used/opened.</summary>
    STATIC METHOD IsUsed(cDatabaseName as STRING) AS LOGIC
        VAR oDb := DbcManager.FindDatabaseByName(cDatabaseName)
        RETURN oDb != NULL


    /// <summary>Get the current active database.</summary>
    STATIC METHOD GetCurrent() AS DbcDatabase
        RETURN DbcManager.ActiveDatabase


    /// <summary>Validate a property name.</summary>
    STATIC METHOD IsValidPropertyName(cProp as STRING) AS LOGIC
        RETURN DatabasePropertyCollection.IsValidProperty(cProp)


    /// <summary>Validate a property type.</summary>
    STATIC METHOD IsValidObjectType(cType as STRING) AS LOGIC
    SWITCH cType:ToLower()
        CASE "connection"
        CASE "database"
        CASE "field"
        CASE "table"
        CASE "view"
            return TRUE
    END SWITCH
    RETURN FALSE

    STATIC METHOD Close(lAll AS LOGIC) AS LOGIC
        // todo: close all DBC files.
        RETURN TRUE

    /// <summary>Close a database.</summary>
    STATIC METHOD Close(cName as STRING) AS LOGIC
        RETURN DbcManager.Close(cName)

    /// <summary>Dump a database to the terminal window.</summary>
    STATIC METHOD Dump(cName as STRING) AS LOGIC
    VAR oDb := DbcManager.FindDatabaseByName(cName)
    IF oDb != NULL
        ? "Database info"
        ? "Name    : ", oDb:Name
        ? "FileName: ", oDb:FileName
        ? "Properties:"
        FOREACH VAR oProp in oDb:Properties
            ? "  ",oProp:Key, oProp:Value
        NEXT
        ? "List of Tables"
        FOREACH var oTable in oDb:Tables
            ? "  ", oTable:ObjectName
             DumpProperties(4, oTable:Properties)
            oTable:GetData()
            ? "  ","Fields"
            FOREACH VAR oField in oTable:Fields
                ? "    ",oField:ObjectName
                DumpProperties(6, oField:Properties)
            NEXT
            if oTable:Indexes:Count > 0
                ? "  ","Indexes"
                FOREACH VAR oIndex in oTable:Indexes
                    ? "    ",oIndex:ObjectName
                    DumpProperties(6, oIndex:Properties)
                NEXT
            endif
            if oTable:Relations:Count > 0
            ? "  ","Relations"
            FOREACH VAR oRel in oTable:Relations
                ? "    ",oRel:ObjectName, oRel:RIInfo
                DumpProperties(6, oRel:Properties)
            NEXT
            endif
        NEXT
        ? "List of Views"
        FOREACH var oView in oDb:Views
            ? "  ",oView:ObjectName
            DumpProperties(4, oView:Properties)
            oView:GetData()
            ? "  ","Fields"
            FOREACH VAR oField in oView:Fields
                ? "    ",oField:ObjectName
                DumpProperties(6, oField:Properties)
            NEXT
        NEXT
        ? "List of Connections"
        FOREACH var oConn in oDb:Connections
            ? "  ",oConn:ObjectName
           DumpProperties(4, oConn:Properties)
        NEXT
    ELSE
        ? "Database not found"
    ENDIF
    RETURN oDb != NULL

    /// <summary>Dump the conents of a properties collection.</summary>
    STATIC METHOD DumpProperties(nSpace as LONG, Properties AS DatabasePropertyCollection) AS VOID
        var ws := String{' ', nSpace}
        FOREACH VAR oProp in Properties
            ? ws,oProp:Key, oProp:Value
        NEXT
        RETURN
END CLASS
