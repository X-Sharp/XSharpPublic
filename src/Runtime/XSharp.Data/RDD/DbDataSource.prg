//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Collections
USING System.Collections.Generic
USING XSharp.RDD
USING XSharp.RDD.Support
USING System.ComponentModel

INTERNAL DELEGATE XSharp.DbNotifyFieldChange( recordNumer AS INT , column AS INT) AS VOID

/// <summary>This class implements an IBindingList on a workarea</summary>
/// <remarks>
/// The class can be used TO directly bind a workarea to a data aware .Net control, such as a DataGridView
/// Each row in the List represents a record in the workarea. Records are represented by DbRecord objects
/// Each record
/// </remarks>
CLASS XSharp.DbDataSource IMPLEMENTS IBindingList
    PROTECTED _fieldList     AS List<DbField>
    PROTECTED _oRDD          AS IRdd
    PROTECTED _readOnly      AS LOGIC
    PROTECTED _shared        AS LOGIC
    PROTECTED _records       AS Dictionary<LONG, DbRecord>
    PROTECTED _index         AS LONG
    PROTECTED _sorted        AS LOGIC
    PROTECTED _indexFile     AS STRING

    INTERNAL EVENT FieldChanged       AS DbNotifyFieldChange

CONSTRUCTOR(oRDD AS IRdd)
    SELF:_oRDD       := oRDD
    SELF:_readOnly   := (LOGIC) oRDD:Info(DBI_READONLY,NULL)
    SELF:_shared     := (LOGIC) oRDD:Info(DBI_SHARED,NULL)
    SELF:_records    := Dictionary<LONG, DbRecord>{}
    SELF:_index      := -1
    SupportsSorting  := TRUE
    SELF:_sorted     := FALSE
    RETURN

DESTRUCTOR
    IF ! String.IsNullOrEmpty(_indexFile)
        System.IO.File.Delete(_indexFile)
    ENDIF

/// <summary>Should the Deleted Flag be included as "virtual column"</summary>
PROPERTY ShowDeleted AS LOGIC AUTO := TRUE
/// <summary>Should the Record number be included as "virtual column"</summary>
PROPERTY ShowRecno   AS LOGIC AUTO := TRUE
#region IBindingList implementation
/// <summary>TRUE when the workarea is not readonly</summary>
PROPERTY AllowEdit      AS LOGIC GET !SELF:_readOnly
/// <summary>TRUE when the workarea is not readonly</summary>
PROPERTY AllowNew       AS LOGIC GET !SELF:_readOnly
/// <summary>TRUE when the workarea is not readonly</summary>
PROPERTY AllowRemove    AS LOGIC GET !SELF:_readOnly
/// <inheritdoc/>
PROPERTY IsSorted       AS LOGIC GET SELF:_sorted
/// <inheritdoc/>
PROPERTY SortDirection  AS ListSortDirection  AUTO
/// <inheritdoc/>
PROPERTY SortProperty   AS PropertyDescriptor AUTO
/// <inheritdoc/>
PROPERTY SupportsChangeNotification AS LOGIC GET TRUE
/// <inheritdoc/>
PROPERTY SupportsSorting    AS LOGIC AUTO
/// <inheritdoc/>
PROPERTY SupportsSearching  AS LOGIC GET FALSE
/// <summary>TRUE when the workarea is readonly</summary>
PROPERTY IsReadOnly AS LOGIC GET _readOnly

/// <inheritdoc/>
METHOD AddIndex(property AS PropertyDescriptor) AS VOID
    THROW NotImplementedException{}

/// <inheritdoc/>
METHOD AddNew() AS OBJECT STRICT
    IF SELF:_oRDD:Append(TRUE)
        RETURN SELF:Current
    ENDIF
RETURN NULL

PRIVATE METHOD findFieldName(property AS PropertyDescriptor) AS STRING
    FOREACH VAR oFld IN SELF:_fieldList
        IF String.Compare(oFld:Caption, property:Name,TRUE) == 0
            IF oFld:CanSort
                RETURN oFld:Name
            ELSE
                RETURN ""
            ENDIF
        ENDIF
    NEXT
RETURN "Recno()"

/// <inheritdoc/>
/// <remarks>This method will create an index on the underlying workarea to sort the rows</remarks>
METHOD ApplySort(property AS PropertyDescriptor, direction AS ListSortDirection) AS VOID
    LOCAL fldName AS STRING
    LOCAL lDescending AS LOGIC
    lDescending  := direction == ListSortDirection.Descending
    fldName := SELF:findFieldName(property)
    IF String.IsNullOrEmpty(fldName)
        RETURN
    ENDIF
IF property != SELF:SortProperty
    // check to see if the index is already there
    VAR nOrder := SELF:getOrder(fldName)
    IF nOrder > 0
        SELF:setOrder(fldName, lDescending)
    ELSE
        SELF:createOrder(fldName, lDescending)
    ENDIF
ELSE
    SELF:setOrder(fldName, lDescending)
ENDIF
SELF:_records:Clear()
SELF:GoTop()
SELF:SortDirection := direction
SELF:SortProperty  := property
SELF:_sorted  := TRUE

PRIVATE METHOD createOrder(fldName AS STRING, lDesc AS LOGIC) AS LOGIC
    VAR cond := DbOrderCondInfo{}
    IF String.IsNullOrEmpty(SELF:_indexFile )
        SELF:_indexFile := System.IO.Path.GetTempFileName()
        FErase(_indexFile)
        SELF:_indexFile := System.IO.Path.GetFileNameWithoutExtension(SELF:_indexFile)
    ENDIF
    VAR info := DbOrderCreateInfo{}
    info:BagName 		:= SELF:_indexFile
    info:Order			:= fldName:Replace("()","")
    info:Expression     := fldName
    cond:Descending     := lDesc
    info:OrdCondInfo 	:= cond
    var result := SELF:_oRDD:OrderCreate(info)
    _indexFile := (String) SELF:_oRDD:OrderInfo(DBOI_BAGNAME,DbOrderInfo{})
    RETURN result

PRIVATE METHOD setOrder(cName AS STRING, lDesc AS LOGIC) AS LOGIC
    VAR info := DbOrderInfo{}
    info:Order   := cName:Replace("()","")
    info:Result  := NULL
    SELF:_oRDD:OrderListFocus(info)
    info:Result  := lDesc
    SELF:_oRDD:OrderInfo(DBOI_ISDESC, info)
RETURN TRUE

PRIVATE METHOD getOrder(cName AS STRING) AS LONG
    VAR info := DbOrderInfo{}
    info:Order   := cName:Replace("()","")
    info:Result  := NULL
RETURN (LONG) SELF:_oRDD:OrderInfo(DBOI_NUMBER, info)


/// <summary>This required method has not (yet) been implemented</summary>
METHOD Find(property AS PropertyDescriptor, key AS OBJECT) AS LONG
    THROW NotImplementedException{}

/// <summary>This required method has not (yet) been implemented</summary>
METHOD RemoveIndex(property AS PropertyDescriptor) AS VOID
    THROW NotImplementedException{}

/// <inheritdoc/>
METHOD RemoveSort () AS VOID STRICT
    VAR info := DbOrderInfo{}
    info:Order   := 0
    info:Result  := NULL
    SELF:_oRDD:OrderListFocus(info)
    SELF:_sorted := FALSE
    SELF:SortProperty := NULL


/// <inheritdoc/>
    EVENT ListChanged AS ListChangedEventHandler

#endregion



/// <summary>Recordnumber in underlying workarea</summary>
PROPERTY RecNo AS LONG GET _oRDD:RecNo
/// <summary>Is underlying workarea at EOF</summary>
PROPERTY EoF   AS LOGIC GET _oRDD:EoF
/// <summary>Alias of underlying workarea</summary>
PROPERTY Name       AS STRING GET _oRDD:Alias
/// <summary>File name of underlying workarea</summary>
PROPERTY FullName   AS STRING GET (STRING) SELF:_oRDD:Info(DBI_FULLPATH,NULL)

INTERNAL METHOD GoTop() AS LOGIC
    SELF:_index := 0
RETURN SELF:_oRDD:GoTop()

INTERNAL METHOD Skip() AS LOGIC
    SELF:_index += 1
RETURN SELF:_oRDD:Skip(1)

INTERNAL PROPERTY Current AS DbRecord GET DbRecord{SELF:RecNo,SELF}

#region IList methods
/// <summary>This required method has not been implemented.</summary>
VIRTUAL METHOD Add( value AS OBJECT ) AS INT
    THROW NotImplementedException{}

/// <inheritdoc />
/// <remarks>This method will call Zap() on the workarea, so it requires exclusive use.</remarks>
VIRTUAL METHOD Clear() AS VOID STRICT
    IF ! SELF:_shared
        SELF:_oRDD:Zap()
    ENDIF

/// <inheritdoc />
VIRTUAL METHOD Contains( oValue AS OBJECT ) AS LOGIC
    IF oValue  IS DbRecord
        VAR oRec := (DbRecord) oValue
        RETURN _records:ContainsValue(oRec)
    ENDIF
RETURN FALSE

/// <inheritdoc />
VIRTUAL METHOD IndexOf( oValue AS OBJECT ) AS INT
RETURN  ((DbRecord) oValue):RecNo

/// <summary>This required method has not been implemented.</summary>
VIRTUAL METHOD Insert(index AS INT,value AS OBJECT) AS VOID
    THROW NotImplementedException{}

/// <inheritdoc />
VIRTUAL PROPERTY IsFixedSize AS LOGIC GET FALSE

/// <inheritdoc />
VIRTUAL METHOD Remove(item AS OBJECT) AS VOID
    TRY
        XSharp.RuntimeState.PushCurrentWorkarea(SELF:_oRDD:Area)
        IF item  IS DbRecord
            VAR oRec := (DbRecord) item

            SELF:GoTo(oRec:RecNo)
            IF SELF:LockRecord()
                IF SELF:_oRDD:Deleted
                    SELF:_oRDD:Recall()
                ELSE
                    SELF:_oRDD:Delete()
                ENDIF
                SELF:UnLockRecord()
            ELSE
                var error := Error{"Cannot lock record"}
                error:Gencode := Gencode.EG_LOCK
                error:SubCode := Subcodes.ERDD_WRITE_LOCK
                THROW error
            ENDIF
            SELF:ListChanged(SELF, ListChangedEventArgs{ListChangedType.ItemChanged,0})
        ENDIF
    FINALLY
        XSharp.RuntimeState.PopCurrentWorkarea()
    END TRY
RETURN

/// <inheritdoc />
VIRTUAL METHOD RemoveAt(index AS INT) AS VOID
    TRY
        XSharp.RuntimeState.PushCurrentWorkarea(SELF:_oRDD:Area)
        LOCAL oRec := ( DbRecord) SELF[index] AS DbRecord
        SELF:GoTo(oRec:RecNo)
        IF SELF:LockRecord()
            IF SELF:_oRDD:Deleted
                SELF:_oRDD:Recall()
            ELSE
                SELF:_oRDD:Delete()
            ENDIF
            SELF:UnLockRecord()
        ELSE
            var error := Error{"Cannot lock record"}
            error:Gencode := Gencode.EG_LOCK
            error:SubCode := Subcodes.ERDD_WRITE_LOCK
            THROW error
        ENDIF

    //SELF:Current:OnPropertyChanged("Deleted")
    FINALLY
        XSharp.RuntimeState.PopCurrentWorkarea()
    END TRY
    SELF:ListChanged(SELF, ListChangedEventArgs{ListChangedType.ItemChanged,index})

#endregion



/// <summary>Retrieve the DbRecordObject for the record at the given position</summary>
/// <remarks>This property is READ ONLY. Assigning to the property will throw an exception</remarks>
/// <seealso cref='DbRecord'>DbRecord class</seealso>
VIRTUAL PROPERTY  SELF[index AS INT] AS OBJECT
    GET
        LOCAL record AS DbRecord
        IF SELF:_records:ContainsKey(index)
            record := SELF:_records[index]
            SELF:_oRDD:GoTo(record:RecNo)
            SELF:_index := index
        ELSE
            IF SELF:_index == -1
                SELF:_oRDD:GoTop()
                SELF:_oRDD:Skip(index)
            ELSE
                SELF:_oRDD:Skip(index - SELF:_index)
            ENDIF
            SELF:_index := index
            record := SELF:Current
            SELF:_records:Add(index, record)
        ENDIF
        RETURN record
    END GET
    SET
        THROW NotImplementedException{}
    END SET
END PROPERTY

/// <summary>This required method has not (yet) been implemented</summary>

VIRTUAL METHOD CopyTo( list AS System.Array , index AS INT ) AS VOID
    SELF:_oRDD:GoTop()
    DO WHILE ! _oRDD:EoF
        list:SetValue(SELF:Current, index)
        index += 1
        SELF:_oRDD:Skip(1)
    ENDDO
    SELF:_oRDD:GoTop()


/// <summary>Returns the # of records in the RDD</summary>
VIRTUAL PROPERTY Count          AS INT GET _oRDD:RecCount
/// <exclude/>
VIRTUAL PROPERTY IsSynchronized AS LOGIC GET TRUE
/// <exclude/>
VIRTUAL PROPERTY SyncRoot       AS OBJECT GET SELF:_oRDD

/// <summary>Returns an enumerator with which you can walk the RDD.</summary>
/// <remarks>This enumerator returns DbRecord objects for each record in the Data Source</remarks>
/// <seealso cref='DbRecord'>DbRecord class</seealso>
VIRTUAL METHOD GetEnumerator() AS IEnumerator STRICT
RETURN DbEnumerator{SELF}

#region methods

INTERNAL PROPERTY Deleted AS LOGIC GET SELF:_oRDD:Deleted

INTERNAL METHOD FieldIndex(fieldName AS STRING) AS LONG
RETURN SELF:_oRDD:FieldIndex(fieldName)

INTERNAL METHOD FieldName(fieldNo AS LONG) AS STRING
RETURN SELF:_oRDD:FieldName(fieldNo)

INTERNAL METHOD GoTo(nRec AS LONG) AS LOGIC
    SELF:_index := -1
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



INTERNAL METHOD GetValue( uField AS OBJECT)  AS OBJECT
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
        IF retVal IS STRING VAR strValue
            retVal := strValue:TrimEnd()
        ENDIF
    ENDIF
RETURN retVal

INTERNAL METHOD LockRecord() AS LOGIC
    LOCAL retVal := FALSE AS LOGIC
    LOCAL lockInfo AS DbLockInfo
    lockInfo := DbLockInfo{}
    lockInfo:Method  := DbLockInfo.LockMethod.Exclusive
    lockInfo:RecId    := SELF:_oRDD:RecNo
    retVal  := SELF:_oRDD:Lock(REF lockInfo)
    RETURN retVal

INTERNAL METHOD UnLockRecord() AS LOGIC
    RETURN SELF:_oRDD:UnLock(SELF:_oRDD:RecNo)



/// We write through into the dbf table and signal the rest of the world, that we have changed something-
INTERNAL METHOD PutValue( uField AS OBJECT, oValue  AS OBJECT)  AS LOGIC
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
        IF SELF:LockRecord()
            retVal     := SELF:_oRDD:PutValue(fieldPos, oValue)
            self:UnLockRecord()
        ELSE
            var error := Error{"Cannot lock record"}
            error:Gencode := Gencode.EG_LOCK
            error:SubCode := Subcodes.ERDD_WRITE_LOCK
            THROW error
        ENDIF
    ENDIF
    SELF:OnFieldChange(SELF:RecNo,fieldPos)
RETURN retVal


/// As the meta data are the same for all records, we generate them just once.
INTERNAL PROPERTY Fields AS IEnumerable<DbField>
    GET
        IF SELF:_fieldList == NULL
            SELF:generateFields()
        ENDIF
        RETURN SELF:_fieldList
    END GET
END PROPERTY

private _oColl := NULL as PropertyDescriptorCollection
INTERNAL PROPERTY PropertyDescriptors AS PropertyDescriptorCollection
    GET
        IF _oColl == NULL
            var props  := TypeDescriptor.GetProperties(Typeof(DbRecord))
            VAR list   := List<PropertyDescriptor>{}
            FOREACH prop AS PropertyDescriptor IN props
                LOCAL lAdd := TRUE AS LOGIC
                IF ! SELF:ShowDeleted .and. String.Compare(prop:Name,"Deleted", true) == 0
                    lAdd := FALSE
                ELSEIF ! SELF:ShowRecno .and. String.Compare(prop:Name,"Recno", true) == 0
                    lAdd := FALSE
                ENDIF
                IF lAdd
                    list:Add((PropertyDescriptor)prop)
                ENDIF
            NEXT
            FOREACH element AS DbField IN SELF:Fields
                list:Add( DbFieldDescriptor{element} )
            NEXT
            _oColl := PropertyDescriptorCollection{list:ToArray()}
        ENDIF
        RETURN _oColl
    END GET
END PROPERTY

/// Generate the meta data which will be used as source for generating the dynamic properties.
PRIVATE METHOD generateFields() AS VOID
    SELF:_fieldList  := List<DbField>{}
    LOCAL f AS INT
    LOCAL fieldCount := SELF:_oRDD:FieldCount AS LONG
    FOR f:=1 UPTO fieldCount
        LOCAL oInfo    AS DbColumnInfo
        oInfo  := (DbColumnInfo) SELF:_oRDD:FieldInfo(f, DBS_COLUMNINFO, NULL)
        SELF:_fieldList:Add(DbField{oInfo})

    NEXT
RETURN
#endregion

// Helper class to enumerate the DbDataSource
PRIVATE CLASS DbEnumerator IMPLEMENTS IEnumerator

    PROTECT datasource AS DbDataSource

CONSTRUCTOR( source AS DbDataSource )
    SELF:datasource := source
RETURN

PUBLIC VIRTUAL PROPERTY Current AS OBJECT
    GET
        RETURN SELF:datasource:Current
    END GET
END PROPERTY

PUBLIC VIRTUAL METHOD MoveNext() AS LOGIC STRICT
    SELF:datasource:Skip()
RETURN ! datasource:EoF

PUBLIC VIRTUAL METHOD Reset() AS VOID STRICT
    SELF:datasource:GoTop()
RETURN

END CLASS


END CLASS



