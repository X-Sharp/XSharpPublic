USING XSharp.RDD



/// <include file="VFPDocs.xml" path="Runtimefunctions/dbgetprop/*" />
/// <seealso cref="DbSetProp" />
/// <seealso cref="DbcDatabase" />
/// <seealso cref="DbcTable" />
/// <seealso cref="DbcView" />
/// <seealso cref="DbcConnection" />
/// <seealso cref="DbcField" />
FUNCTION DbGetProp( cName AS STRING, cType AS STRING, cProperty AS STRING)  AS USUAL
    IF ! Dbc.IsValidObjectType(cType)
        THROW Error.ArgumentError(__FUNCTION__, nameof(cType), __VfpStr(VFPErrors.INVALID_DB_OBJECT, cType))
    ENDIF
    IF ! Dbc.IsValidPropertyName(cProperty)

        THROW Error.ArgumentError(__FUNCTION__, nameof(cProperty), __VfpStr(VFPErrors.INVALID_DB_PROPERTY_NAME, cProperty))
    ENDIF
    VAR oDb := Dbc.GetCurrent()
    IF oDb == NULL_OBJECT
        THROW Error.VoDbError(EG_DB, EDB_NODB,__FUNCTION__)
    ENDIF

    RETURN oDb:GetProp(cName, cType, cProperty)


/// <include file="VFPDocs.xml" path="Runtimefunctions/dbsetprop/*" />
/// <seealso cref="DbGetProp" />
/// <seealso cref="DbcDatabase" />
/// <seealso cref="DbcTable" />
/// <seealso cref="DbcView" />
/// <seealso cref="DbcConnection" />
/// <seealso cref="DbcField" />
FUNCTION DbSetProp(cName AS STRING, cType AS STRING, cProperty AS STRING, ePropertyValue AS USUAL)
    IF ! Dbc.IsValidObjectType(cType)
        THROW Error.ArgumentError(__FUNCTION__, nameof(cType), __VfpStr(VFPErrors.INVALID_DB_OBJECT, cType))
    ENDIF
    IF ! Dbc.IsValidPropertyName(cProperty)
        THROW Error.ArgumentError(__FUNCTION__, nameof(cProperty),  __VfpStr(VFPErrors.INVALID_DB_PROPERTY_NAME, cProperty))
    ENDIF
    VAR oDb := Dbc.GetCurrent()
    IF oDb == NULL_OBJECT
        THROW Error.VoDbError(EG_NOTABLE, EDB_NOTABLE,__FUNCTION__)
    ENDIF
    RETURN oDb:SetProp(cName, cType, cProperty, ePropertyValue)


/// <include file="VFPDocs.xml" path="Runtimefunctions/dbc/*" />
/// <seealso cref="DbAlias" />
/// <seealso cref="DbUsed" />

FUNCTION Dbc() AS STRING
    LOCAL oDb := Dbc.GetCurrent() as DbcDatabase
    IF oDb != NULL
        RETURN oDb:FileName
    ENDIF
    RETURN String.Empty

/// <include file="VFPDocs.xml" path="Runtimefunctions/dbused/*" />
/// <seealso cref="DbAlias" />
/// <seealso cref="Dbc" />
FUNCTION DbUsed( cDatabaseName AS STRING) AS LOGIC
    RETURN Dbc.IsUsed(cDatabaseName)

/// <include file="VFPDocs.xml" path="Runtimefunctions/dbalias/*" />
/// <seealso cref="DbUsed" />
/// <seealso cref="Dbc" />
FUNCTION DbAlias () AS STRING
    LOCAL oDb := Dbc.GetCurrent() as DbcDatabase
    IF oDb != NULL
        RETURN oDb:Name
    ENDIF
    RETURN String.Empty


#ifdef NOTDEFINED
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
#endif

/// <include file="VFPDocs.xml" path="Runtimefunctions/lock/*" />
FUNCTION Lock( cRecordNumberList, uArea) AS LOGIC
    RETURN RLock(cRecordNumberList, uArea)
