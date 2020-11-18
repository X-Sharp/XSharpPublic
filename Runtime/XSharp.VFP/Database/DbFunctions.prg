USING XSharp.RDD



/// <include file="VFPDocs.xml" path="Runtimefunctions/dbgetprop/*" />
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
FUNCTION Dbc() AS STRING
    LOCAL oDb := Dbc.GetCurrent() as DbcDatabase
    IF oDb != NULL
        RETURN oDb:FileName
    ENDIF
    RETURN String.Empty
    
/// <include file="VFPDocs.xml" path="Runtimefunctions/dbused/*" />
FUNCTION DbUsed( cDatabaseName AS STRING) AS LOGIC
    RETURN Dbc.IsUsed(cDatabaseName)

/// <include file="VFPDocs.xml" path="Runtimefunctions/dbalias/*" />
FUNCTION DBAlias () AS STRING
    LOCAL oDb := Dbc.GetCurrent() as DbcDatabase
    IF oDb != NULL
        RETURN oDb:Name
    ENDIF
    RETURN String.Empty


