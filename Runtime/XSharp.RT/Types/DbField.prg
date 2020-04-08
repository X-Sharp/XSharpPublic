//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Collections
USING System.Collections.Generic
USING XSharp.RDD
USING System.ComponentModel



/// <summary></summary>
CLASS XSharp.DbField
    INTERNAL Info AS DbColumnInfo
    CONSTRUCTOR( info AS DbColumnInfo)
    SELF:Info       := info
    
    #region properties
    PROPERTY DataType   AS System.Type GET SELF:Info:DotNetType
    PROPERTY Name       AS STRING GET SELF:Info:Name
    PROPERTY Caption    AS STRING GET SELF:Info:ColumnName
    PROPERTY Column     AS INT GET SELF:Info:Ordinal
    PROPERTY ReadOnly   AS LOGIC GET SELF:Info:IsAutoIncrement
    PROPERTY CanSort    AS LOGIC GET SELF:Info:CanSort
    #endregion
END CLASS


/// <summary></summary>
INTERNAL CLASS XSharp.DBFieldDescriptor INHERIT PropertyDescriptor
    
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
