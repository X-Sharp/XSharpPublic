USING XSharp.RDD



/// <include file="VFPDocs.xml" path="Runtimefunctions/dbgetprop/*" />
/// <seealso cref="O:XSharp.VFP.Functions.DbSetProp" />
/// <seealso cref="T:XSharp.RDD.DbcDatabase" />
/// <seealso cref="T:XSharp.RDD.DbcTable" />
/// <seealso cref="T:XSharp.RDD.DbcView" />
/// <seealso cref="T:XSharp.RDD.DbcConnection" />
/// <seealso cref="T:XSharp.RDD.DbcField" />
FUNCTION DbGetProp( cName AS STRING, cType AS STRING, cProperty AS STRING)  AS USUAL
    IF ! Dbc.IsValidObjectType(cType)
        THROW Error.ArgumentError(__FUNCTION__, nameof(cType), "Database object type '"+cType+"' is invalid")
    ENDIF
    IF ! Dbc.IsValidPropertyName(cProperty)
        
        THROW Error.ArgumentError(__FUNCTION__, nameof(cProperty), "Database property name '"+cProperty+"' is invalid")
    ENDIF
    VAR oDb := Dbc.GetCurrent()
    IF oDb == NULL_OBJECT
        THROW Error.VoDbError(EG_DB, EDB_NODB,__FUNCTION__)
    ENDIF
    
    RETURN oDb:GetProp(cName, cType, cProperty)
    

/// <include file="VFPDocs.xml" path="Runtimefunctions/dbsetprop/*" />
/// <seealso cref="O:XSharp.VFP.Functions.DbGetProp" />
/// <seealso cref="T:XSharp.RDD.DbcDatabase" />
/// <seealso cref="T:XSharp.RDD.DbcTable" />
/// <seealso cref="T:XSharp.RDD.DbcView" />
/// <seealso cref="T:XSharp.RDD.DbcConnection" />
/// <seealso cref="T:XSharp.RDD.DbcField" />
FUNCTION DbSetProp(cName AS STRING, cType AS STRING, cProperty AS STRING, ePropertyValue AS USUAL)
    IF ! Dbc.IsValidObjectType(cType)
        THROW Error.ArgumentError(__FUNCTION__, nameof(cType), "Database object type '"+cType+"' is invalid")
    ENDIF
    IF ! Dbc.IsValidPropertyName(cProperty)
        THROW Error.ArgumentError(__FUNCTION__, nameof(cProperty), "Database property name '"+cProperty+"' is invalid")
    ENDIF
    VAR oDb := Dbc.GetCurrent()
    IF oDb == NULL_OBJECT
        THROW Error.VoDbError(EG_NOTABLE, EDB_NOTABLE,__FUNCTION__)
    ENDIF
    RETURN oDb:SetProp(cName, cType, cProperty, ePropertyValue)

    
/// <include file="VFPDocs.xml" path="Runtimefunctions/dbc/*" />
/// <seealso cref="O:XSharp.VFP.Functions.DbAlias" />
/// <seealso cref="O:XSharp.VFP.Functions.DbUsed" />

FUNCTION Dbc() AS STRING
    LOCAL oDb := Dbc.GetCurrent() as DbcDatabase
    IF oDb != NULL
        RETURN oDb:FileName
    ENDIF
    RETURN String.Empty
    
/// <include file="VFPDocs.xml" path="Runtimefunctions/dbused/*" />
/// <seealso cref="O:XSharp.VFP.Functions.DbAlias" />
/// <seealso cref="O:XSharp.VFP.Functions.Dbc" />
FUNCTION DbUsed( cDatabaseName AS STRING) AS LOGIC
    RETURN Dbc.IsUsed(cDatabaseName)

/// <include file="VFPDocs.xml" path="Runtimefunctions/dbalias/*" />
/// <seealso cref="O:XSharp.VFP.Functions.DbUsed" />
/// <seealso cref="O:XSharp.VFP.Functions.Dbc" />
FUNCTION DBAlias () AS STRING
    LOCAL oDb := Dbc.GetCurrent() as DbcDatabase
    IF oDb != NULL
        RETURN oDb:Name
    ENDIF
    RETURN String.Empty



/// <include file="VFPDocs.xml" path="Runtimefunctions/adatabases/*" />
FUNCTION ADatabases(ArrayName REF __FoxArray) 
    local result := DbcManager.Databases:Count AS LONG
    IF result > 0
        if ! IsArray(ArrayName) .or. ArrayName == NULL_OBJECT
            ArrayName := __FoxArray{}
        ENDIF
        ArrayName:ReDim(result,2)
        FOR VAR nDb := 1 to result
            var db := DbcManager.Databases[nDb]
            ArrayName[nDb,1]   := db:Name
            ArrayName[nDb,2]   := db:FileName
        NEXT
    ENDIF
    RETURN result
