//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System.Collections
USING System.Collections.Generic
USING System.Linq
USING XSharp.RDD
USING System.ComponentModel


/// <summary></summary>
[TypeDescriptionProvider(typeof(DbRecordDescriptorProvider))];
CLASS XSharp.DbRecord IMPLEMENTS INotifyPropertyChanged
    #region instance variables
    
    INTERNAL PROPERTY Datasource  AS DbDataSource AUTO
    EVENT PropertyChanged AS PropertyChangedEventHandler 
    
    #endregion
    
    #region construtors
    /// TODO: Check back if this is needed 
    CONSTRUCTOR( position AS INT)
    SELF:RecNo := position
    RETURN
    /// Standard constructor, the record object know the server it belongs to and
        /// the current record position, which is needed when we want to talk back to the dbf.
        /// TODO: If the dbf uses indexes, we need a different mechanism to address the positioning 
    ///       of the record position.
    CONSTRUCTOR( position AS INT, source AS DbDataSource)
    SELF(position)
    SELF:Datasource := source
    /// The server implements an event which will be triggered if a column value is changed.
    /// This is the modern way to implement the Notify mechanism of the dataserver class. 
    SELF:Datasource:FieldChanged += DbNotifyFieldChange{ SELF , @FieldChange() }
    RETURN
    #endregion

    PRIVATE _previous AS LONG
    METHOD SetPos() AS LOGIC
        LOCAL changed  :=FALSE AS LOGIC
        _previous := SELF:Datasource:RecNo
        changed := (_previous != SELF:RecNo)
        IF changed
            SELF:Datasource:GoTo( SELF:RecNo )
        ENDIF
        RETURN changed
 

    METHOD RestorePos() AS VOID
        SELF:Datasource:GoTo(SELF:_previous)
        RETURN

    #region properties
    
    PROPERTY  RecNo AS INT AUTO
    PROPERTY  Deleted      AS LOGIC
        GET
            LOCAL lRes AS LOGIC
            LOCAL changed:=FALSE AS LOGIC
            changed := SELF:SetPos()
            lRes := SELF:Datasource:Deleted
            IF changed
                SELF:RestorePos()
            ENDIF
            RETURN lRes
        END GET
    END PROPERTY

    PROPERTY SELF[fieldName AS STRING] AS OBJECT
        GET
            VAR fieldPos := SELF:Datasource:FieldIndex(fieldName)
            RETURN SELF[fieldPos]
        END GET
        SET
            VAR fieldPos := SELF:Datasource:FieldIndex(fieldName)
            SELF[fieldPos] := Value
        END SET
    END PROPERTY

    PROPERTY SELF[fieldPos AS INT] AS OBJECT
        GET
            LOCAL value AS OBJECT
            LOCAL changed:=FALSE AS LOGIC
            changed := SELF:SetPos()
            /// Now we can retrieve the column value 
            value := Datasource:GetValue(fieldPos)
            /// Repsotion the dbf to the record we have been on before moving to the expected one.
            IF changed
                SELF:RestorePos()
            ENDIF
            RETURN value
        END GET
        
        SET
            LOCAL changed:=FALSE AS LOGIC
            changed := SELF:SetPos()
            SELF:Datasource:FieldChanged -= DbNotifyFieldChange{ SELF , @FieldChange() }
            /// Now we can set the column value 
            Datasource:PutValue(fieldPos,value)
            // we want to be informed again
            SELF:OnPropertyChanged(fieldPos)
            SELF:Datasource:FieldChanged += DbNotifyFieldChange{ SELF , @FieldChange() }
            /// Repsotion the dbf to the record we have been on before moving to the expected one.
            IF changed
                SELF:RestorePos()
            ENDIF
        END SET
    END PROPERTY
    #endregion
    
    #region methods
    /// Always a good idea to implement this.
    PUBLIC METHOD ToString() AS STRING STRICT
        RETURN SELF:RecNo:ToString()
        
        /// This method will be called if somebody changes a column value inside the dbf server.
            /// If the record number is the same as the one we're representing, the we raise the 
    /// property changed event as one of our properties has changed.
    METHOD FieldChange( recno AS INT , colno AS INT ) AS VOID
    IF recno == SELF:RecNo
        SELF:OnPropertyChanged(colno)
    ENDIF
    RETURN
    
    /// We are using this to signal the consuming components that a property has changed. 
        /// As the PropertyChangedEventArgs has to provide the name of the property changed, 
    /// we are usign the column name of inside the dbf server.
    METHOD OnPropertyChanged(colno AS INT) AS VOID
    IF SELF:PropertyChanged != NULL
        LOCAL args:=PropertyChangedEventArgs{SELF:Datasource:FieldName(colno)} AS PropertyChangedEventArgs 
        SELF:PropertyChanged(SELF,args)
    ENDIF
    
    METHOD OnPropertyChanged(colname AS STRING ) AS VOID
    IF SELF:PropertyChanged != NULL
        LOCAL args:=PropertyChangedEventArgs{colname} AS PropertyChangedEventArgs 
        SELF:PropertyChanged(SELF,args)
    ENDIF
    RETURN 
    #endregion    
END CLASS



INTERNAL CLASS XSharp.DbRecordDescriptorProvider INHERIT TypeDescriptionProvider
    #region class variables
    STATIC PROTECTED defaultTypeProvider := TypeDescriptor.GetProvider(typeof(DbRecord)) AS TypeDescriptionProvider
    #endregion
    
    #region constructors
    CONSTRUCTOR() STRICT
    SUPER(defaultTypeProvider)
    RETURN
    #endregion
    #region methods
    /// This method is being called if someone wants to reflect the type we're operating on. 
    PUBLIC VIRTUAL METHOD GetTypeDescriptor(objectType AS Type, instanceObject AS OBJECT) AS ICustomTypeDescriptor
        LOCAL returnValue          AS ICustomTypeDescriptor
        LOCAL defaultDescriptor    AS ICustomTypeDescriptor
        
        defaultDescriptor := SUPER:GetTypeDescriptor(objectType,instanceObject)
        
        IF instanceObject == NULL
            returnValue := defaultDescriptor
        ELSE
            /// of our special type descriptor.
            returnValue := (ICustomTypeDescriptor) DbRecordDescriptor{defaultDescriptor, (DbRecord) instanceObject}
        ENDIF
        RETURN returnValue         
    #endregion
END CLASS




INTERNAL CLASS XSharp.DbRecordDescriptor  INHERIT CustomTypeDescriptor
    #region instance variables
    PRIVATE fields := List<DBFieldDescriptor>{} AS List<DBFieldDescriptor>
    #endregion
    #region constructors
    CONSTRUCTOR( parent AS ICustomTypeDescriptor, record AS DbRecord )
        SUPER(parent)
        FOREACH element AS DbField IN record:Datasource:Fields
            fields:Add( DBFieldDescriptor{element} )
        NEXT
        RETURN
    
    #endregion
    #region methods

    PRIVATE METHOD mergeProps(props AS PropertyDescriptorCollection) AS PropertyDescriptorCollection
        VAR list   := List<PropertyDescriptor>{}
        FOREACH VAR prop IN props
            list:Add((PropertyDescriptor)prop)
        NEXT
        list:AddRange(SELF:fields)
        RETURN PropertyDescriptorCollection{list:ToArray()}

    PUBLIC VIRTUAL METHOD GetProperties() AS PropertyDescriptorCollection STRICT
        VAR props := SUPER:GetProperties()
        RETURN SELF:mergeProps(props)
        
    /// Same as the method before except that attributes are provided for filtering.
    PUBLIC VIRTUAL METHOD GetProperties(attributes AS Attribute[]) AS PropertyDescriptorCollection
        VAR props := SUPER:GetProperties(attributes)
        RETURN SELF:mergeProps(props)
    #endregion
END CLASS


