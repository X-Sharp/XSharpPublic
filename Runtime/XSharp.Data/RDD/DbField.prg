//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp.RDD
USING System.ComponentModel



/// <summary>This class is used by the DbDataSource class to describe the fields in the workarea</summary>
CLASS XSharp.DbField
    INTERNAL Info AS DbColumnInfo
    /// <summary>Initializes a new instance of the DbField class</summary>
    /// <param name="info">Column info on which this field is based.</param>
    CONSTRUCTOR( info AS DbColumnInfo)
    SELF:Info       := info
    
    #region properties
    /// <summary>Dotnet datatype for the field</summary>
    PROPERTY DataType   AS System.Type GET SELF:Info:DotNetType
    /// <summary>Name of the field</summary>
    PROPERTY Name       AS STRING GET SELF:Info:Name
    /// <summary>Caption for the field</summary>
    PROPERTY Caption    AS STRING GET SELF:Info:ColumnName
    /// <summary>Ordinal position of the field</summary>
    PROPERTY Column     AS INT GET SELF:Info:Ordinal
    /// <summary>Is the field Readonly ?</summary>
    PROPERTY ReadOnly   AS LOGIC GET SELF:Info:IsAutoIncrement
    /// <summary>Can the field be sorted ?</summary>
    PROPERTY CanSort    AS LOGIC GET SELF:Info:CanSort
    #endregion
END CLASS


INTERNAL CLASS XSharp.DbFieldDescriptor INHERIT PropertyDescriptor
    PROTECTED _dbField AS DbField

    #region constructors
    CONSTRUCTOR( dbField AS DbField )
    SUPER( dbField:Caption , Attribute[]{0} )
    _dbField := dbField   
    RETURN
    #endregion   
    #region methods
    
    VIRTUAL METHOD CanResetValue(component AS OBJECT) AS LOGIC
        RETURN TRUE
        
    VIRTUAL METHOD GetValue(component AS OBJECT) AS OBJECT
        LOCAL record AS DbRecord
        LOCAL returnValue AS OBJECT
        /// cast to strongy typed variable first
        record := (DbRecord) component 
        
        IF record:Item[SELF:_dbField:Column] != NULL
            returnValue := record:Item[SELF:_dbField:Column]
        ELSE
            IF SELF:_dbField:DataType:IsValueType
                returnValue := System.Activator.CreateInstance(SELF:_dbField:DataType)
            ELSE
                returnValue := NULL
            ENDIF
        ENDIF
        RETURN returnValue
        
    VIRTUAL METHOD ResetValue(component AS OBJECT) AS VOID
        SELF:SetValue(component, DBNull.Value)
        
    VIRTUAL METHOD SetValue(component AS OBJECT, value AS OBJECT) AS VOID
        LOCAL record AS DbRecord
        record := (DbRecord)component 
        record:Item[SELF:_dbField:Column] := value
        RETURN
        
    /// Must be overwritten as the the base class declares this method as abstract.
    VIRTUAL METHOD ShouldSerializeValue(component AS OBJECT) AS LOGIC
        RETURN FALSE
    #endregion
    
    #region properties
    PROPERTY ComponentType  AS System.Type  GET typeof(DbRecord)
    PROPERTY DbField        AS DbField      GET SELF:_dbField SET SELF:_dbField := value
    PROPERTY IsReadOnly     AS LOGIC        GET SELF:_dbField:ReadOnly
    PROPERTY PropertyType   AS System.Type  GET SELF:_dbField:DataType
    #endregion
    
END CLASS
