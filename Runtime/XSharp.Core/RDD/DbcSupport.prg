//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING XSharp.RDD.Support

BEGIN NAMESPACE XSharp.RDD

    CLASS DbcDatabases
        STATIC PROPERTY Databases as List<DbcDatabase> AUTO
        STATIC PROPERTY DbcDataSession AS DataSession AUTO
        STATIC CONSTRUCTOR
            Databases   := List<DbcDatabase>{}
            DbcDataSession := DataSession{"DBC"}
        
        STATIC METHOD Open(cFileName as STRING, lShared as LOGIC, lReadOnly as LOGIC) AS LOGIC
            IF FindDatabase(cFileName) != NULL_OBJECT
                RETURN FALSE
            ENDIF
            RETURN DbcDatabases.Do({ =>
                local lOk := FALSE as LOGIC
                lOk := CoreDb.UseArea(TRUE, "DBFVFP",cFileName, "", lShared,lReadOnly)
                IF lOk
                    local name as STRING
                    local area as DWORD
                    name := RuntimeState.Workareas:CurrentWorkarea:Alias
                    area := RuntimeState.Workareas:CurrentWorkarea:Area
                    local oDb as DbcDatabase
                    oDb := DbcDatabase{name, cFileName, area}
                    Databases:Add(oDb)
                    oDb:ReadChildren()
                ENDIF
                RETURN lOk
                })
        STATIC METHOD Close(cName as STRING) AS LOGIC
            VAR oDb := FindDatabaseByName(cName)
            LOCAL lOk := FALSE AS LOGIC
            IF oDb != NULL
                lOk := TRUE
                BEGIN LOCK Databases
                    Databases:Remove(oDb)
                    lOk := DbcDatabases.Do( {=>
                        CoreDb.Select(oDb:Area, OUT VAR nOld)
                        lOk := CoreDb.CloseArea()
                        CoreDb.Select(nOld, OUT NULL)
                        RETURN lOk
                        })
                END LOCK
            ENDIF
            RETURN lOk
            
        STATIC METHOD FindDatabase(cFileName as STRING) AS DbcDatabase
            FOREACH var oDb in Databases
                IF String.Compare(oDb:FileName, cFileName, TRUE) == 0
                    RETURN oDb
                ENDIF
            NEXT
            RETURN NULL_OBJECT
            
        STATIC METHOD FindDatabaseByName(cName as STRING) AS DbcDatabase
            FOREACH var oDb in Databases
                IF String.Compare(oDb:Name, cName, TRUE) == 0
                    RETURN oDb
                ENDIF
            NEXT
            RETURN NULL_OBJECT

        INTERNAL STATIC METHOD Do<T>(action AS @@Func<T>) AS T
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
            
        STATIC METHOD CreateDatabase(cFileName as STRING) AS LOGIC
            IF FindDatabase(cFileName) != NULL_OBJECT
                RETURN FALSE
            ENDIF
            RETURN DbcDatabases.Do({ =>
                LOCAL lOk as LOGIC
                LOCAL lOpen AS LOGIC
                lOk := CoreDb.Create(cFileName, DbcStructure, "DBFVFP",TRUE,"XXDBCXX","",FALSE, FALSE)
                IF lOk
                    lOk := CoreDb.UseArea(TRUE, "DBFVFP",cFileName, "", FALSE,FALSE)
                ENDIF
                lOpen := lOk
                LOCAL cIndex as STRING
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
                IF lOpen
                    CoreDb.CloseArea()
                ENDIF
                RETURN lOk
                })

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
    
    
    CLASS DbcDatabase INHERIT DbcObject
        PROPERTY Area           AS DWORD AUTO
        PROPERTY Name           AS STRING AUTO
        PROPERTY FileName       AS STRING AUTO
        PROPERTY Tables         AS List<DbcTable> AUTO
        PROPERTY Connections    AS List<DbcConnection> AUTO
        PROPERTY Views          AS List<DbcView> AUTO
        PROPERTY Other          AS List<DbcObject> AUTO
        CONSTRUCTOR(cName as STRING, cFileName as STRING, nArea as DWORD)
            SELF:Tables         := List<DbcTable>{}
            SELF:Views          := List<DbcView>{}
            SELF:Connections    := List<DbcConnection>{}
            SELF:Other          := List<DbcObject>{}
            SELF:Name           := cName
            SELF:FileName       := cFileName
            SELF:Area           := nArea
            RETURN

        METHOD ParentKey(nKey as LONG) AS STRING
            RETURN nKey:ToString():PadLeft(10,' ')

        // Read the tables, views etc.
        METHOD ReadChildren() AS LOGIC
            RETURN DbcDatabases.Do( { => 
                LOCAL nOld as DWORD
                LOCAL lOk := FALSE as LOGIC
                lOk := CoreDb.Select(SELF:Area, OUT nOld)
                lOk := CoreDb.OrdSetFocus(NULL, OBJECTTYPE_ORDER)
                lOk := CoreDb.Seek(ParentKey(1),FALSE,FALSE)
                DO WHILE ! CoreDb.Eof() 
                    LOCAL oValue := NULL AS OBJECT
                    lOk := CoreDb.FieldGet(PARENTID_NUM, REF oValue)
                    IF Convert.ToInt32(oValue) != 1
                        EXIT
                    ENDIF
                    var oChild := SELF:ReadChild()
                    IF oChild != NULL_OBJECT
                        oChild:Read()
                        oChild:Parent := SELF
                        if oChild is DbcTable VAR oTable
                            SELF:Tables:Add(oTable)
                        ELSEIF oChild IS DbcView VAR oView
                            SELF:Views:Add(oView)
                        ELSEIF oChild IS DbcConnection VAR oConn
                            SELF:Connections:Add(oConn)
                        ELSEIF oChild:ObjectName == NAME_DATABASE
                            SELF:Read()
                        ELSE
                            SELF:Other:Add(oChild)
                        ENDIF
                    ENDIF
                    CoreDb.Skip(1)
                ENDDO
                CoreDb.Select(nOld, OUT NULL)
                RETURN lOk
                })
                
        METHOD ReadChild() AS DbcObject
            LOCAL oType := NULL as OBJECT
            CoreDb.FieldGet(OBJECTTYPE_NUM, REF oType)
            IF oType IS STRING VAR cType
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
            RETURN DbcObject{}
            END SWITCH
            ENDIF
            RETURN NULL_OBJECT  
              
    END CLASS
    
    CLASS DbcTable INHERIT DbcObject
        PROPERTY Fields     AS List<DbcField> AUTO
        PROPERTY Indexes    AS List<DbcIndex> AUTO
        PROPERTY Relations  AS List<DbcRelation> AUTO
        CONSTRUCTOR()
            SUPER()
            SELF:Fields     := List<DbcField>{}
            SELF:Indexes    := List<DbcIndex>{}
            SELF:Relations  := List<DbcRelation>{}
    END CLASS
    
    CLASS DbcView INHERIT DbcObject
        PROPERTY Fields     AS List<DbcObject> AUTO
        CONSTRUCTOR
            SUPER()
    END CLASS

    CLASS DbcField INHERIT DbcObject
        CONSTRUCTOR
            SUPER()
    END CLASS
    
    CLASS DbcIndex INHERIT DbcObject
        CONSTRUCTOR
            SUPER()
    END CLASS

    CLASS DbcConnection INHERIT DbcObject
        CONSTRUCTOR
            SUPER()
    END CLASS

    CLASS DbcRelation INHERIT DbcObject
        PROPERTY RIInfo AS STRING AUTO
        CONSTRUCTOR
            SUPER()
    END CLASS

    [DebuggerDisplay("{ObjectType,nq} {ObjectName}")];
    CLASS DbcObject
        INTERNAL CONST OBJECTID_NUM        := 1 AS DWORD
        INTERNAL CONST PARENTID_NUM        := 2 AS DWORD
        INTERNAL CONST OBJECTTYPE_NUM      := 3 AS DWORD
        INTERNAL CONST OBJECTNAME_NUM      := 4 AS DWORD
        INTERNAL CONST PROPERTY_NUM        := 5 AS DWORD
        INTERNAL CONST CODE_NUM            := 6 AS DWORD
        INTERNAL CONST RIINFO_NUM          := 7 AS DWORD
        INTERNAL CONST USER_NUM            := 8 AS DWORD
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

    
        PROPERTY ObjectID AS LONG AUTO
        PROPERTY ParentID AS LONG AUTO
        PROPERTY ObjectType AS STRING AUTO
        PROPERTY ObjectName AS STRING AUTO
        PROPERTY Properties AS PropertyCollection AUTO
        PROPERTY Parent     AS OBJECT AUTO
        
        CONSTRUCTOR
            Properties := PropertyCollection{}
        METHOD Read() AS LOGIC
            // Assume current area is the DBC
            TRY
                SELF:ObjectID   := (INT) SELF:Reader(OBJECTID_NUM)
                SELF:ParentID   := (INT) SELF:Reader(PARENTID_NUM)
                SELF:ObjectType := ((STRING) SELF:Reader(OBJECTTYPE_NUM)):Trim()
                SELF:ObjectName := ((STRING) SELF:Reader(OBJECTNAME_NUM)):Trim()
                SELF:DecodeProperties()
            CATCH  as Exception
                RETURN FALSE
            END TRY
            RETURN TRUE

        METHOD Reader (nField as DWORD) AS OBJECT
            LOCAL oValue := NULL AS OBJECT
            IF CoreDb.FieldGet(nField, REF oValue)
                RETURN oValue
            ENDIF
            THROW RuntimeState:LastRddError

        METHOD DecodeProperties() AS VOID
            LOCAL bProps as BYTE[]
            TRY
                bProps := NULL
                local oValue as Object
                CoreDb.FieldGet(PROPERTY_NUM, REF oValue)
                bProps := (byte[]) oValue
            
                VAR pos := 8
                DO WHILE pos < bProps:Length-10 // 4 for length, 4 for type, 2 for prop#
                    VAR len  := BitConverter.ToInt32(bProps, pos)
                    //VAR type := (Int) bProps[4]
                    VAR prop := FoxToShort(bProps, pos+5)
                    var dbProp := (DatabasePropertyType) prop
                    var expectedType := DatabasePropertyValType(prop)
                    SWITCH expectedType
                    CASE "L"
                        var lValue   := bProps[pos+7] != 0
                        SELF:Properties:Add(dbProp, lValue)
                    CASE "N"
                        local nValue as LONG
                        if len == 11
                            nValue   := FoxToLong(bProps, pos+7)
                        elseif len == 8
                            nValue := bProps[pos+7]
                        elseif len == 9
                            nValue   := FoxToShort(bProps, pos+7)
                        else
                            nValue := 0
                        endif
                        SELF:Properties:Add(dbProp, nValue)
                    CASE "C"
                        VAR strValue := RuntimeState:WinEncoding:GetString(bProps, pos+7, len-8)
                        SELF:Properties:Add(dbProp, strValue)
                    OTHERWISE
                        VAR strValue := RuntimeState:WinEncoding:GetString(bProps, pos+7, len-8)
                        SELF:Properties:Add(dbProp, strValue)
                    END SWITCH                                
                    pos += len
                ENDDO
            CATCH
                THROW
            END TRY
        RETURN
        
        INTERNAL METHOD FoxToShort( buffer AS BYTE[], nOffSet AS LONG) AS LONG
            LOCAL bytes as byte[]
            bytes := <byte>{0,0}
            bytes[1] := buffer[nOffSet+0]
            bytes[0] := buffer[nOffSet+1]
            RETURN BitConverter.ToInt16(bytes,0)
            
        INTERNAL METHOD FoxToLong( buffer AS BYTE[], nOffSet AS LONG) AS LONG
            LOCAL bytes as byte[]
            bytes := <byte>{0,0,0,0}
            bytes[3] := buffer[nOffSet+0]
            bytes[2] := buffer[nOffSet+1]
            bytes[1] := buffer[nOffSet+2]
            bytes[0] := buffer[nOffSet+3]
            RETURN BitConverter.ToInt32(bytes,0)

    END CLASS
END NAMESPACE

FUNCTION DbcOpen(cFileName AS STRING, lShared AS LOGIC, lReadOnly as LOGIC) AS LOGIC
    LOCAL lOpen := FALSE as LOGIC
    LOCAL cExt := System.IO.Path.GetExtension(cFileName) AS STRING
    IF String.IsNullOrEmpty(cExt)
        cFileName := System.IO.Path.ChangeExtension(cFileName, ".DBC")
    ENDIF
    IF File(cFileName)
        cFileName := FPathName()
        lOpen := DbcDatabases.Open(cFileName, lShared, lReadOnly)
    ENDIF
    RETURN lOpen
    
FUNCTION DbcCreate(cFileName as STRING) AS LOGIC
    LOCAL cExt := System.IO.Path.GetExtension(cFileName) AS STRING
    IF String.IsNullOrEmpty(cExt)
        cFileName := System.IO.Path.ChangeExtension(cFileName, ".DBC")
    ENDIF
    RETURN DbcDatabases.CreateDatabase(cFileName)



FUNCTION DbcClose(lAll AS LOGIC) AS LOGIC
    RETURN TRUE

FUNCTION DbcClose(cName as STRING) AS LOGIC
    RETURN DbcDatabases.Close(cName)
    
FUNCTION DbcSelect(cName AS STRING) AS LOGIC
    RETURN TRUE

FUNCTION DbcDump(cName as STRING) AS LOGIC
    VAR oDb := DbcDatabases.FindDatabaseByName(cName)
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
            FOREACH VAR oProp in oTable:Properties
                ? "    ",oProp:Key, oProp:Value
            NEXT
        NEXT
        ? "List of Views"
        FOREACH var oView in oDb:Views
            ? "  ",oView:ObjectName
           FOREACH VAR oProp in oView:Properties
               ? "    ",oProp:Key, oProp:Value
           NEXT
        NEXT
        ? "List of Connections"
        FOREACH var oConn in oDb:Connections
            ? "  ",oConn:ObjectName
           FOREACH VAR oProp in oConn:Properties
               ? "    ",oProp:Key, oProp:Value
           NEXT
        NEXT        
    ELSE
        ? "Database not found"
    ENDIF
    RETURN oDb != NULL    
