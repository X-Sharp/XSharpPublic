//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System.Collections.Generic
USING System.ComponentModel
USING XSharp.RDD

/// <summary>This class is used by the DbDataSource class to represent the records in a workarea.</summary>
/// <remarks>The record class exposes the fields in the workarea as 'pseudo' properties of these fields.
/// The DbRecord class also implements INotifyPropertychanged, so when the record is bound to a data aware control
/// then this control will 'see' changes that were made to the record.
/// </remarks>
[TypeDescriptionProvider(typeof(DbRecordDescriptorProvider))];
CLASS XSharp.DbRecord IMPLEMENTS INotifyPropertyChanged, IDbRow
    #region instance variables

    INTERNAL PROPERTY Datasource  AS DbDataSource AUTO
    /// <inheritdoc/>
    EVENT PropertyChanged AS PropertyChangedEventHandler

    #endregion

    #region construtors
    /// the current record position, which is needed when we want to talk back to the dbf.
    /// <summary>Initializes a new instance of the DbRecord class</summary>
    /// <param name="position">Ordinal position in the datasource</param>
    /// <param name="source">Data source to which this object belongs</param>
    CONSTRUCTOR( position AS INT, source AS DbDataSource)
    SELF:RecNo := position
    SELF:Datasource := source
    /// The server implements an event which will be triggered if a column value is changed.
    /// This is the modern way to implement the Notify mechanism of the dataserver class.
    SELF:Datasource:FieldChanged += DbNotifyFieldChange{ SELF , @FieldChange() }
    RETURN
    #endregion

    PRIVATE _previous AS LONG
    // Make sure the workarea is on the right row before updating
    PRIVATE METHOD SetPos() AS LOGIC
        LOCAL changed  :=FALSE AS LOGIC
        _previous := SELF:Datasource:RecNo
        changed := (_previous != SELF:RecNo)
        IF changed
            SELF:Datasource:GoTo( SELF:RecNo )
        ENDIF
        RETURN changed

    // restore position in workarea
    PRIVATE METHOD RestorePos() AS VOID
        SELF:Datasource:GoTo(SELF:_previous)
        RETURN

    #region properties

    /// <summary>Record number in the workarea. Does not have to match the logical position in the list.</summary>
    [ReadOnly(TRUE)];
    PROPERTY  RecNo AS INT AUTO
    /// <summary>Is the current record deleted ?</summary>
    [ReadOnly(TRUE)];
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

    /// <summary>Read/Write fields in the workarea by name.</summary>
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


    /// <summary>Read/Write fields in the workarea by position.</summary>
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
            /// Reposition the dbf to the record we have been on before moving to the expected one.
            IF changed
                SELF:RestorePos()
            ENDIF
        END SET
    END PROPERTY
    #endregion

    /// <summary>Get the fieldname for a certain field position</summary>
    /// <param name="fieldPos">0 based Field number</param>
    /// <returns>Field Name</returns>
    PROPERTY FieldName[fieldPos AS INT] AS STRING
        GET
            return SELF:Datasource:FieldName(fieldPos)
        END GET
    END PROPERTY

    /// <summary>Get the value for a certain field position</summary>
    /// <param name="fieldPos">0 based Field number</param>
    /// <returns>Field Value</returns>
    PROPERTY FieldValue[fieldPos AS INT] AS OBJECT
        GET
            return SELF:Datasource:GetValue(fieldPos)
        END GET
    END PROPERTY


    #region methods
    /// Always a good idea to implement this.
    PUBLIC OVERRIDE METHOD ToString() AS STRING STRICT
        RETURN SELF:Datasource:Name+" "+SELF:RecNo:ToString()

    /// This method will be called if somebody changes a column value inside the dbf server.
    /// If the record number is the same as the one we're representing, the we raise the
    /// property changed event as one of our properties has changed.
    PRIVATE METHOD FieldChange( recno AS INT , colno AS INT ) AS VOID
        IF recno == SELF:RecNo
            SELF:OnPropertyChanged(colno)
        ENDIF
    RETURN

    /// We are using this to signal the consuming components that a property has changed.
        /// As the PropertyChangedEventArgs has to provide the name of the property changed,
    /// we are usign the column name of inside the dbf server.
    PRIVATE METHOD OnPropertyChanged(colno AS INT) AS VOID
    IF SELF:PropertyChanged != NULL
        LOCAL args:=PropertyChangedEventArgs{SELF:Datasource:FieldName(colno)} AS PropertyChangedEventArgs
        SELF:PropertyChanged(SELF,args)
    ENDIF

    PRIVATE METHOD OnPropertyChanged(colname AS STRING ) AS VOID
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
    PUBLIC OVERRIDE METHOD GetTypeDescriptor(objectType AS Type, instanceObject AS OBJECT) AS ICustomTypeDescriptor
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
    #region constructors
    PROTECTED oDatasource as DbDataSource
    CONSTRUCTOR( parent AS ICustomTypeDescriptor, record AS DbRecord )
        SUPER(parent)
        oDatasource := record:Datasource
        RETURN

    #endregion
    #region methods



    PUBLIC OVERRIDE METHOD GetProperties() AS PropertyDescriptorCollection STRICT
        RETURN SELF:oDatasource:PropertyDescriptors

    /// Same as the method before except that attributes are provided for filtering.
    PUBLIC OVERRIDE METHOD GetProperties(attributes AS Attribute[]) AS PropertyDescriptorCollection
        RETURN SELF:oDatasource:PropertyDescriptors
#endregion
END CLASS


