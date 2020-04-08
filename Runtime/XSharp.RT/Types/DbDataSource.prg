//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Collections
USING System.Collections.Generic
USING XSharp.RDD
USING System.ComponentModel

DELEGATE XSharp.DbNotifyFieldChange( recordNumer AS INT , column AS INT) AS VOID


CLASS XSharp.DbDataSource IMPLEMENTS IBindingList
    PROTECTED _fieldList     AS List<DbField>
    PROTECTED _oRDD          AS IRdd
    PROTECTED _readOnly      AS LOGIC 
    EVENT FieldChanged  AS DbNotifyFieldChange
    
    CONSTRUCTOR(oRDD AS IRdd)
    _oRDD       := oRDD
    _readOnly   := (LOGIC) oRDD:Info(DBI_READONLY,NULL)
    RETURN

    #region IBindingList implementation
        PROPERTY AllowEdit      AS LOGIC GET !_readOnly
        PROPERTY AllowNew       AS LOGIC GET !_readOnly
        PROPERTY AllowRemove    AS LOGIC GET !_readOnly
        PROPERTY IsSorted       AS LOGIC GET FALSE
        PROPERTY SortDirection  AS ListSortDirection  GET ListSortDirection.Ascending
        PROPERTY SortProperty   AS PropertyDescriptor GET NULL
        PROPERTY SupportsChangeNotification AS LOGIC GET TRUE
        PROPERTY SupportsSorting AS LOGIC GET FALSE
        PROPERTY SupportsSearching AS LOGIC GET FALSE


        PROPERTY IsReadOnly AS LOGIC GET _readOnly

        METHOD AddIndex(property AS PropertyDescriptor) AS VOID
            THROW NotImplementedException{}            

        METHOD AddNew() AS OBJECT STRICT
            IF SELF:_oRDD:Append(TRUE)
                FOREACH VAR FLD IN SELF:Fields
                    
                NEXT
                RETURN SELF:Current
            ENDIF
            RETURN NULL

        METHOD ApplySort(property AS PropertyDescriptor, direction AS ListSortDirection) AS VOID
            THROW NotImplementedException{}            

        METHOD Find(property AS PropertyDescriptor, key AS OBJECT) AS LONG
            THROW NotImplementedException{}

        METHOD RemoveIndex(property AS PropertyDescriptor) AS VOID
            THROW NotImplementedException{}            

        METHOD RemoveSort () AS VOID STRICT
            THROW NotImplementedException{}            

        EVENT ListChanged AS ListChangedEventHandler

   #endregion



    PROPERTY RecNo AS LONG GET _oRDD:RecNo
    PROPERTY EoF   AS LOGIC GET _oRDD:EoF
    
    PROPERTY Name       AS STRING GET _oRDD:Alias
    PROPERTY FullName   AS STRING GET (STRING) _oRDD:Info(DBI_FULLPATH,NULL)
    
    METHOD GoTop() AS LOGIC
        RETURN SELF:_oRDD:GoTop()
    
    METHOD Skip() AS LOGIC
        RETURN SELF:_oRDD:Skip(1)
    
    INTERNAL PROPERTY Current AS DbRecord GET DbRecord{SELF:RecNo,SELF} 
    
    #region IList methods
    VIRTUAL METHOD Add( value AS OBJECT ) AS INT
        THROW NotImplementedException{}
        
    VIRTUAL METHOD Clear() AS VOID STRICT
        THROW NotImplementedException{}
        //RETURN 
        
    VIRTUAL METHOD Contains( value AS OBJECT ) AS LOGIC
        THROW NotImplementedException{}
        //RETURN FALSE
        
    VIRTUAL METHOD IndexOf( value AS OBJECT ) AS INT
        RETURN  ((DbRecord) value):RecNo
        
    VIRTUAL METHOD Insert(index AS INT,value AS OBJECT) AS VOID
        THROW NotImplementedException{}
        //RETURN 
        
    VIRTUAL PROPERTY IsFixedSize AS LOGIC GET FALSE
    
    VIRTUAL METHOD Remove(item AS OBJECT) AS VOID
        IF item IS DbRecord  VAR record
            SELF:RemoveAt(record:RecNo)
        ENDIF
        
    VIRTUAL METHOD RemoveAt(index AS INT) AS VOID
        SELF:GoTo(index+1)
        IF SELF:_oRDD:Deleted
            SELF:_oRDD:Recall()
        ELSE
            SELF:_oRDD:Delete()
        ENDIF
        //SELF:Current:OnPropertyChanged("Deleted")
        SELF:ListChanged(SELF, ListChangedEventArgs{ListChangedType.ItemChanged,index})
        
    #endregion
    
    
    
    
    VIRTUAL PROPERTY  SELF[index AS INT] AS OBJECT
        GET
            LOCAL record AS DbRecord
            LOCAL recno := _oRDD:RecNo AS INT
            LOCAL repos := FALSE AS LOGIC
            repos := (recno != index+1 )
            IF repos 
                _oRDD:GoTo( index+1 ) 
            ENDIF
            record := SELF:Current
            IF repos 
                _oRDD:GoTo( recno )
            ENDIF
            RETURN record
        END GET
        SET
            THROW NotImplementedException{}         
        END SET
    END PROPERTY
    
    VIRTUAL METHOD CopyTo( liste AS System.Array , IndexExt AS INT ) AS VOID
        THROW NotImplementedException{}
        //RETURN
        
    VIRTUAL PROPERTY Count          AS INT GET _oRDD:RecCount
    VIRTUAL PROPERTY IsSynchronized AS LOGIC GET TRUE
    VIRTUAL PROPERTY SyncRoot       AS OBJECT GET SELF:_oRDD
    
    VIRTUAL METHOD GetEnumerator() AS IEnumerator STRICT
        RETURN DbEnumerator{SELF}
        
    #region methods

    PROPERTY Deleted AS LOGIC GET SELF:_oRDD:Deleted
    
    METHOD FieldIndex(fieldName AS STRING) AS LONG
        RETURN SELF:_oRDD:FieldIndex(fieldName)

    METHOD FieldName(fieldNo AS LONG) AS STRING
        RETURN SELF:_oRDD:FieldName(fieldNo)
    
    METHOD GoTo(nRec AS LONG) AS LOGIC
        RETURN SELF:_oRDD:GoTo(nRec)
    
    INTERNAL METHOD OnFieldChange(recno AS INT, fieldNo AS INT) AS VOID
        IF SELF:FieldChanged != NULL
            SELF:FieldChanged( recno,fieldNo )
        ENDIF
        RETURN

    INTERNAL METHOD OnFieldChange(recno AS INT, fieldName AS STRING) AS VOID
        IF SELF:FieldChanged != NULL
            VAR fieldNo := SELF:FieldIndex(fieldName)
            SELF:FieldChanged( recno,fieldNo )
        ENDIF
        RETURN    

        
        
    METHOD GetValue( uField AS OBJECT)  AS OBJECT
    LOCAL fieldPos AS INT
    LOCAL retVal := NULL AS OBJECT
    IF uField IS STRING VAR strField
        fieldPos := SELF:_oRDD:FieldIndex(strField)
    ELSEIF uField IS LONG VAR liField
        fieldPos := (LONG) liField
    ELSE
        fieldPos := -1        
    ENDIF
    IF fieldPos > 0    
        retVal     := SELF:_oRDD:GetValue(fieldPos)
    ENDIF
    RETURN retVal
    
    
    /// We write through into the dbf table and signal the rest of the world, that we have changed something-
    METHOD PutValue( uField AS OBJECT, oValue  AS OBJECT)  AS LOGIC
    LOCAL fieldPos AS INT
    LOCAL retVal := FALSE AS LOGIC
    IF uField IS STRING VAR strField
        fieldPos := SELF:_oRDD:FieldIndex(strField)
    ELSEIF uField IS LONG VAR liField
        fieldPos := (LONG) liField
    ELSE
        fieldPos := -1        
    ENDIF
    IF fieldPos > 0    
        retVal     := SELF:_oRDD:PutValue(fieldPos, oValue)
    ENDIF
    SELF:OnFieldChange(SELF:RecNo,fieldPos)   
    RETURN retVal
    
    
    /// As the meta data are the same for all records, we generate them just once.
    PROPERTY Fields AS IEnumerable<DbField>
        GET
            IF _fieldList == NULL
                SELF:GenerateFields()
            ENDIF
            RETURN _fieldList
        END GET
    END PROPERTY



    /// Generate the meta data which will be used as source for generating the dynamic properties.
    INTERNAL METHOD GenerateFields() AS VOID
        _fieldList  := List<DbField>{}
        LOCAL f AS INT
        LOCAL fieldCount := SELF:_oRDD:FieldCount
        FOR f:=1 UPTO fieldCount
            LOCAL fieldName:=_oRDD:FieldName(f) AS STRING
            LOCAL oInfo    AS DbColumnInfo
            oInfo  := (DbColumnInfo) SELF:_oRDD:FieldInfo(f, DBS_COLUMNINFO, NULL)
            IF oInfo == NULL
                LOCAL cType    AS STRING
                LOCAL nDec     AS LONG
                LOCAL nLen     AS LONG
                
                cType := (STRING) SELF:_oRDD:FieldInfo(f, DBS_TYPE, NULL)
                nLen  := (LONG)   SELF:_oRDD:FieldInfo(f, DBS_DEC, NULL)
                nDec  := (LONG)   SELF:_oRDD:FieldInfo(f, DBS_DEC, NULL)
                oInfo :=  DbColumnInfo{fieldName, cType, nLen, nDec}
                oInfo:Ordinal := f
            ENDIF
            
            _fieldList:Add(DbField{oInfo})
            
        NEXT
        RETURN
    #endregion        
END CLASS



CLASS XSharp.DbEnumerator IMPLEMENTS IEnumerator

    PROTECT datasource AS DbDataSource
    
    CONSTRUCTOR( source AS DbDataSource )
        datasource := source
        RETURN
    
    PUBLIC VIRTUAL PROPERTY Current AS OBJECT
        GET
            RETURN datasource:Current
        END GET
    END PROPERTY
    
    PUBLIC VIRTUAL METHOD MoveNext() AS LOGIC STRICT
        datasource:Skip()
        RETURN ! datasource:EoF
        
    PUBLIC VIRTUAL METHOD Reset() AS VOID STRICT
        datasource:GoTop()
        RETURN 
    
END CLASS

