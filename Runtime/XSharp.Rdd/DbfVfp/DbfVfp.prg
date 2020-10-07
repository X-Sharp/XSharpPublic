//
// Copyright (c) B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING System.IO
USING System.Collections.Generic
USING System.Diagnostics

BEGIN NAMESPACE XSharp.RDD
/// <summary>DBFVFP RDD. DBFCDX with support for the FoxPro field types.</summary>
[DebuggerDisplay("DBFVFP ({Alias,nq})")];
CLASS DBFVFP INHERIT DBFCDX
    PRIVATE CONST VFP_BACKLINKSIZE := 262 AS LONG
	CONSTRUCTOR()
		SUPER()
		SELF:_AllowedFieldTypes := "BCDFGILMNPQTVWY0"
		RETURN
		
	PUBLIC   PROPERTY Driver                  AS STRING GET "DBFVFP"
    INTERNAL PROPERTY DbcName        AS STRING AUTO
    INTERNAL PROPERTY DbcPosition    AS INT GET DbfHeader.SIZE + SELF:_Fields:Length  * DbfField.SIZE +1

    PUBLIC OVERRIDE METHOD Create( openInfo AS DbOpenInfo ) AS LOGIC
	LOCAL isOk AS LOGIC
    isOk := SUPER:Create(openInfo)
    IF isOk
        SELF:_SetFoxHeader()
    ENDIF
    SELF:_ReadDbcInfo()
    RETURN isOk

    PROTECTED VIRTUAL METHOD _checkField( dbffld REF DbfField) AS LOGIC
        IF dbffld:Type:IsVfp()
            IF dbffld:Flags:HasFlag(DBFFieldFlags.AutoIncrement)
                IF dbffld:Counter == 0
                    dbffld:Counter := 1
                ENDIF
                IF dbffld:IncStep == 0
                    dbffld:IncStep := 1
                ENDIF
            ENDIF
            RETURN TRUE
        ENDIF
        RETURN FALSE

     PROTECTED METHOD _SetFoxHeader() AS VOID
        LOCAL lVar AS LOGIC
        LOCAL lAutoIncr AS LOGIC
        // check for foxpro field types and adjust the header
        lVar        := FALSE
        lAutoIncr   := FALSE
        FOREACH VAR fld IN _Fields
            SWITCH fld:FieldType
            CASE DbFieldType.VarChar
            CASE DbFieldType.VarBinary
            CASE DbFieldType.Blob
                lVar := TRUE
            END SWITCH
            IF fld:Flags:HasFlag(DBFFieldFlags.AutoIncrement)
                lAutoIncr := TRUE
            ENDIF
        NEXT
        IF lAutoIncr
            IF lVar
                SELF:_Header:Version := DBFVersion.VisualFoxProVarChar
            ELSE
                SELF:_Header:Version := DBFVersion.VisualFoxProAutoIncrement
            ENDIF
        ELSEIF lVar
            SELF:_Header:Version := DBFVersion.VisualFoxProVarChar
        ELSE
            SELF:_Header:Version := DBFVersion.VisualFoxPro
        ENDIF
        // Set Flags
        SELF:_Header:TableFlags := DBFTableFlags.None
        IF SELF:_HasMemo
            SELF:_Header:TableFlags |= DBFTableFlags.HasMemoField
        ENDIF
        IF SELF:_indexList != NULL .AND. SELF:_indexList:Count > 0
            VAR orderBag := SELF:_indexList:First
            IF orderBag != NULL .AND. orderBag:Structural
                SELF:_Header:TableFlags |= DBFTableFlags.HasStructuralCDX
            ENDIF
        ENDIF
        
        SELF:_Header:HeaderLen += VFP_BACKLINKSIZE
        SELF:_HeaderLength   += VFP_BACKLINKSIZE
        SELF:_writeHeader()
        // Adjust the file size to accomodate the backlink data
        _oStream:SafeSetLength(SELF:_HeaderLength)
        RETURN

    /// <inheritdoc />
    METHOD CreateFields(aFields AS RddFieldInfo[]) AS LOGIC
        LOCAL NullCount := 0 AS LONG
        LOCAL nullFld  := NULL AS RddFieldInfo
        LOCAL lOk := FALSE as LOGIC
        FOREACH VAR fld IN aFields
            IF fld:IsNullable
                NullCount += 1
            ENDIF
            IF fld:IsVarLength
                NullCount += 1
            ENDIF
            IF String.Compare(fld:Name, _NULLFLAGS, TRUE) == 0
                nullFld := fld
                fld:Name := _NULLFLAGS
            ENDIF
        NEXT
        IF nullFld != NULL
            LOCAL nLen AS LONG
            nLen := NullCount / 8
            IF NullCount %8 != 0
                nLen += 1
            ENDIF
            nullFld:Length := (BYTE) nLen
        ENDIF
        lOk := SUPER:CreateFields(aFields)
        RETURN lOk

        PROPERTY FieldCount AS LONG
        GET
            // Exclude the _NullColumn
            LOCAL ret := 0 AS LONG
            IF SELF:_Fields != NULL 
                ret := SELF:_Fields:Length
                IF SELF:_NullColumn != NULL
                    ret -= 1
                ENDIF
            ENDIF
            RETURN ret
        END GET
        END PROPERTY

    METHOD Open ( info AS DbOpenInfo) AS LOGIC
        LOCAL lOk AS LOGIC
        LOCAL lOld AS LOGIC
        // Delay auto open until after we have read the DBC name and we have read the long fieldnames
        lOld := XSharp.RuntimeState.AutoOpen
        XSharp.RuntimeState.AutoOpen := FALSE
        lOk := SUPER:Open(info)
        XSharp.RuntimeState.AutoOpen := lOld
        IF lOk
            IF SELF:_Header:Version:IsVfp()
                SELF:_ReadDbcInfo()
            ENDIF
            IF XSharp.RuntimeState.AutoOpen
                SELF:OpenProductionIndex(info)
            ENDIF
            SELF:GoTop()
        ENDIF
        
        RETURN lOk

    METHOD FieldInfo(nFldPos AS LONG, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT

        IF nOrdinal == DbFieldInfo.DBS_PROPERTIES
           RETURN DbFieldInfo.DBS_FLAGS
        ELSEIF nOrdinal == DbFieldInfo.DBS_CAPTION
            VAR oCol := SELF:_GetColumn(nFldPos)
            IF oCol IS IColumnInfo VAR info
                RETURN info:Caption
            ENDIF
            nOrdinal := DbFieldInfo.DBS_ALIAS
        ELSEIF nOrdinal == DbFieldInfo.DBS_DESCRIPTION
            VAR oCol := SELF:_GetColumn(nFldPos)
            IF oCol IS IColumnInfo VAR info
                RETURN info:Description
            ENDIF
            RETURN ""
        ENDIF
        RETURN SUPER:FieldInfo(nFldPos, nOrdinal, oNewValue)

    PROTECTED METHOD _ReadDbcInfo() AS VOID
        LOCAL nPos := SELF:DbcPosition AS LONG
        LOCAL buffer AS BYTE[]
        buffer := BYTE[]{VFP_BACKLINKSIZE}
        SELF:DbcName := ""
        IF _oStream:SafeSetPos(nPos) .AND. _oStream:SafeRead(buffer)
            VAR cName := System.Text.Encoding.Default:GetString(buffer):Replace(e"\0","")
            IF ! String.IsNullOrEmpty(cName)
                SELF:DbcName := Path.Combine(Path.GetDirectoryName(SELF:_FileName), cName)
                SELF:_ReadDbcFieldNames()
            ENDIF
        ENDIF
        RETURN

    PROTECTED METHOD _ReadDbcFieldNames() AS VOID
        LOCAL oDbc AS DBFVFP
        LOCAL oi   AS DbOpenInfo
        LOCAL nTable := 0 AS LONG
        LOCAL nType AS LONG
        LOCAL nName AS LONG
        LOCAL nParent AS LONG
        LOCAL nProp AS LONG
        LOCAL fields AS List<STRING>
        LOCAL props  AS List<FoxDbProperties>
        LOCAL cFile AS STRING
        LOCAL lOk AS LOGIC
        LOCAL lOld AS LOGIC
        oDbc := DBFVFP{}
        oi := DbOpenInfo{}
        oi:FileName  := SELF:DbcName
        oi:Extension := System.IO.Path.GetExtension(SELF:DbcName)
        oi:Shared   := TRUE
        oi:ReadOnly := TRUE
        cFile := System.IO.Path.GetFileNameWithoutExtension(SELF:_FileName)
        lOld := XSharp.RuntimeState.AutoOpen
        XSharp.RuntimeState.AutoOpen := FALSE
        TRY
            lOk := oDbc:Open(oi)
        CATCH as Exception
            lOk := FALSE
        END TRY
        XSharp.RuntimeState.AutoOpen := lOld
        IF lOk
            nType   := oDbc:FieldIndex("OBJECTTYPE")
            nName   := oDbc:FieldIndex("OBJECTNAME")
            nParent := oDbc:FieldIndex("PARENTID")
            nProp   := oDbc:FieldIndex("PROPERTY")
            oDbc:GoTop()
            DO WHILE ! oDbc:EoF
                IF ((STRING)oDbc:GetValue(nType)):StartsWith("Table") 
                    LOCAL cName := (STRING) oDbc:GetValue(nName)  AS STRING
                    cName := cName:Trim()
                    IF String.Compare(cName, cFile, TRUE) == 0
                        nTable := (INT) oDbc:GetValue(oDbc:FieldIndex("OBJECTID"))
                        EXIT
                    ENDIF
                ENDIF
                oDbc:Skip(1)
            ENDDO
            fields := List<STRING>{}
            props  := List<FoxDbProperties>{}
            IF nTable != 0
                oDbc:GoTop()
                DO WHILE ! oDbc:EoF
                    VAR n1 := (LONG) oDbc:GetValue(nParent)
                    VAR c1 := (STRING) oDbc:GetValue(nType)
                    IF n1== nTable .AND.  c1:StartsWith("Field")
                        VAR cAlias := (STRING) oDbc:GetValue(nName) 
                        VAR b1 := (BYTE[]) oDbc:Memo:GetValue(nProp)
                        fields:Add(cAlias:Trim())
                        props:Add(SplitDbProperties(b1, _Encoding))
                    ENDIF
                    oDbc:Skip(1)
                ENDDO
            ENDIF
            oDbc:Close()
            IF SELF:FieldCount == fields:Count
                // assign aliases
                LOCAL nPos AS LONG
                FOR nPos := 1 TO SELF:FieldCount
                    LOCAL oColumn AS DbfColumn
                    oColumn := SELF:_GetColumn(nPos)
                    oColumn := DbfVfpColumn{oColumn, SELF}
                    oColumn:Properties := props[nPos-1]
                    SELF:_Fields[nPos-1] := oColumn
                    oColumn:Alias := fields[nPos-1]
                    IF String.Compare(oColumn:Name, oColumn:Alias, TRUE) != 0
                        SELF:_fieldNames:Remove(oColumn:Name)
                        SELF:_fieldNames:Add(oColumn:Alias, nPos-1)
                    ENDIF
                NEXT
            ENDIF
        ENDIF

    /// <inheritdoc />
    METHOD AddField(info AS RddFieldInfo) AS LOGIC
        LOCAL isOk AS LOGIC
        isOk := SUPER:AddField( info )
        IF String.Compare(info:Name, _NULLFLAGS,TRUE) == 0 .AND. info IS DbfNullColumn VAR dbfnc
            SELF:_NullColumn := dbfnc
        ENDIF
        IF info IS DbfColumn VAR column
            IF column:IsVarLength
                column:LengthBit := SELF:_NullCount++
            ENDIF
            IF column:IsNullable
                column:NullBit := SELF:_NullCount++
            ENDIF
        ENDIF
        RETURN isOk

    OVERRIDE PROTECTED METHOD _readRecord() AS LOGIC 
        LOCAL lOk AS LOGIC
        LOCAL lMustReadNull AS LOGIC
        lMustReadNull := ! SELF:_BufferValid
        lOk := SUPER:_readRecord()
        IF lOk .AND. SELF:_NullColumn != NULL .AND. lMustReadNull
            SELF:_NullColumn:GetValue(SELF:_RecordBuffer)
        ENDIF
        RETURN lOk

    OVERRIDE PROTECTED METHOD _writeRecord() AS LOGIC
        // Write VFP Null flags, if any
        IF SELF:_NullColumn != NULL .AND. ! SELF:_ReadOnly 
            SELF:_NullColumn:PutValue(0, SELF:_RecordBuffer)
        ENDIF
        RETURN SUPER:_writeRecord() 
END CLASS

ENUM FoxPropertyType 
    MEMBER Path             := 1
    MEMBER Class            := 2
    MEMBER Comment          := 7
    MEMBER RuleExpr         := 9 
    MEMBER RuleText         := 10
    MEMBER DefaultValue     := 11
    MEMBER ParameterList    := 12
    MEMBER ChildTag         := 13
    MEMBER InsTrigger       := 14
    MEMBER UpdateTrigger    := 15
    MEMBER DelTrigger       := 16
    MEMBER IsUnique         := 17
    MEMBER RelTable         := 18
    MEMBER RelTag           := 19
    MEMBER PrimaryKey       := 20
    MEMBER Version          := 24
    MEMBER BatchUpdateCount := 28
    MEMBER DataSource       := 29
    MEMBER ConnectName      := 32
    MEMBER UpdateName       := 35
    MEMBER FetchMemo        := 36
    MEMBER FetchSize        := 37
    MEMBER KeyField         := 38
    MEMBER MaxRecords       := 39
    MEMBER ShareConnection  := 40
    MEMBER SourceType       := 41
    MEMBER SQL              := 42
    MEMBER Tables           := 43
    MEMBER SendUpdates      := 44
    MEMBER UpdatableField   := 45
    MEMBER UpdateType       := 46
    MEMBER UseMemoSize      := 47
    MEMBER WhereType        := 48
    MEMBER Mask             := 54
    MEMBER Format           := 55
    MEMBER Caption          := 56
    MEMBER Asynchronous     := 64
    MEMBER BatchMode        := 65
    MEMBER ConnectString    := 66
    MEMBER ConnectTimeout   := 67
    MEMBER DispLogin        := 68
    MEMBER DispWarnings     := 69
    MEMBER IdleTimeout      := 70
    MEMBER QueryTimeOut     := 71
    MEMBER Password         := 72
    MEMBER Transactions     := 73
    MEMBER UserID           := 74
    MEMBER WaitTime         := 75
    MEMBER TimeStamp        := 76
    MEMBER DataType         := 77
END ENUM

[DebuggerDisplay("{Type} : {Value,nq}")]; 
CLASS FoxDbProperty
    PUBLIC PROPERTY Type AS FoxPropertyType AUTO
    PUBLIC PROPERTY @@Value AS STRING AUTO
END CLASS

CLASS FoxDbProperties
    PROTECTED props AS Dictionary<FoxPropertyType, FoxDbProperty>
    CONSTRUCTOR()
        props := Dictionary<FoxPropertyType, FoxDbProperty>{}
        RETURN
    METHOD Add(oProp AS FoxDbProperty) AS VOID
        props:Add(oProp:Type, oProp)
        RETURN 
    METHOD GetProperty(nType AS FoxPropertyType) AS STRING
        IF SELF:props:ContainsKey(nType)
            RETURN SELF:props[nType]:Value
        ENDIF
        RETURN  ""
END CLASS


FUNCTION SplitDbProperties (bProps AS BYTE[], encoding AS System.Text.Encoding) AS FoxDbProperties
    VAR token  := FtpMemoToken{bProps}
    VAR length := token:Length
    Debug.Assert(token.DataType == FlexFieldType.String )
    Debug.Assert(length == bProps.Length-8)
    VAR pos := 8
    VAR result := FoxDbProperties{}
    DO WHILE pos < bProps:Length
        VAR len  := (LONG) bProps[pos]
        VAR type := FoxToLong(bProps, pos+1)
        Debug.Assert(type == FlexFieldType.String )
        VAR prop := (FoxPropertyType) FoxToShort(bProps, pos+5)
        VAR strvalue := encoding:GetString(bProps, pos+7, len-8)
        pos += len
        result:Add( FoxDbProperty{} { Type :=  prop, @@Value := strvalue} )
    ENDDO
    RETURN result


CLASS DbfVfpColumn INHERIT DbfColumn IMPLEMENTS IColumnInfo
    CONSTRUCTOR (oColumn AS DbfColumn, oRDD AS XSharp.RDD.DBF)
        SUPER(oColumn, oRDD)
        SELF:NullBit        := oColumn:NullBit
        SELF:LengthBit      := oColumn:LengthBit
        SELF:OffSetInHeader := oColumn:OffSetInHeader
        

    PROPERTY DbProperties AS FoxDbProperties
        GET
            IF ! (SELF:Properties IS FoxDbProperties) 
                SELF:Properties := FoxDbProperties{}
            ENDIF
            RETURN (FoxDbProperties) SELF:Properties 
        END GET
    END PROPERTY
    PROPERTY Caption      AS STRING GET DbProperties:GetProperty(FoxPropertyType.Caption)
    PROPERTY ColumnName   AS STRING GET SELF:Alias
    PROPERTY Description  AS STRING GET DbProperties:GetProperty(FoxPropertyType.Comment)
    PROPERTY Mask         AS STRING GET DbProperties:GetProperty(FoxPropertyType.Mask)
    PROPERTY Format       AS STRING GET DbProperties:GetProperty(FoxPropertyType.Format)
        
END CLASS

END NAMESPACE


 
