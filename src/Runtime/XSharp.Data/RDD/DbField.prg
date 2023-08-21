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
    PROPERTY Name       AS STRING GET SELF:Info:ColumnName
    /// <summary>Caption for the field</summary>
    PROPERTY Caption    AS STRING GET SELF:Info:Caption
    /// <summary>Ordinal position of the field</summary>
    PROPERTY Ordinal     AS INT GET SELF:Info:Ordinal
    /// <summary>Is the field Readonly ?</summary>
    PROPERTY ReadOnly   AS LOGIC GET SELF:Info:IsAutoIncrement
    /// <summary>Can the field be sorted ?</summary>
    PROPERTY CanSort    AS LOGIC GET SELF:Info:CanSort
    /// <summary>Can the field be sorted ?</summary>
    PROPERTY Description   AS STRING GET SELF:Info:Description

    #endregion
END CLASS


INTERNAL CLASS XSharp.DbFieldDescriptor INHERIT PropertyDescriptor
    PROTECTED _dbField AS DbField

    #region constructors
    CONSTRUCTOR( dbField AS DbField )

    SUPER( dbField:Name , Attribute[]{0} )
    _dbField := dbField
    RETURN
    #endregion
    #region methods

    OVERRIDE METHOD CanResetValue(component AS OBJECT) AS LOGIC
        RETURN TRUE

    OVERRIDE METHOD GetValue(component AS OBJECT) AS OBJECT
        LOCAL record AS DbRecord
        LOCAL returnValue AS OBJECT
        /// cast to strongy typed variable first
        record := (DbRecord) component

        IF record:Item[SELF:_dbField:Ordinal] != NULL
            returnValue := record:Item[SELF:_dbField:Ordinal]
        ELSE
            IF SELF:_dbField:DataType:IsValueType
                returnValue := System.Activator.CreateInstance(SELF:_dbField:DataType)
            ELSE
                returnValue := NULL
            ENDIF
        ENDIF
        RETURN returnValue

    OVERRIDE METHOD ResetValue(component AS OBJECT) AS VOID
        SELF:SetValue(component, DBNull.Value)

    OVERRIDE METHOD SetValue(component AS OBJECT, value AS OBJECT) AS VOID
        LOCAL record AS DbRecord
        record := (DbRecord)component
        record:Item[SELF:_dbField:Ordinal] := value
        RETURN

    /// Must be overwritten as the the base class declares this method as abstract.
    OVERRIDE METHOD ShouldSerializeValue(component AS OBJECT) AS LOGIC
        RETURN FALSE
    #endregion

    #region properties
    OVERRIDE PROPERTY ComponentType  AS System.Type  GET typeof(DbRecord)
    VIRTUAL  PROPERTY DbField        AS DbField      GET SELF:_dbField SET SELF:_dbField := value
    OVERRIDE PROPERTY IsReadOnly     AS LOGIC        GET SELF:_dbField:ReadOnly
    OVERRIDE PROPERTY PropertyType   AS System.Type  GET SELF:_dbField:DataType
    #endregion

END CLASS
