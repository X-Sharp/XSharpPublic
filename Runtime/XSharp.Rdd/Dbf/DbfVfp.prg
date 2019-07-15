//
// Copyright (c) B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp.RDD.Enums
USING XSharp.RDD.Support

BEGIN NAMESPACE XSharp.RDD
/// <summary>DBFVFP RDD. DBFCDX with support for the FoxPro field types.</summary>
CLASS DBFVFP INHERIT DBFCDX
	CONSTRUCTOR()
		SUPER()
		RETURN
		
	PROPERTY Driver AS STRING GET "DBFVFP"

    PUBLIC OVERRIDE METHOD Create( openInfo AS DbOpenInfo ) AS LOGIC
	LOCAL isOk AS LOGIC
    isOk := SUPER:Create(openInfo)
    IF isOk
        SELF:_SetFoxHeader()
    ENDIF
    RETURN isOk

    PROTECTED VIRTUAL METHOD _checkField( dbffld REF DbfField) AS LOGIC
        IF dbffld:Type:IsVfp()
            if dbffld:Flags:HasFlag(DBFFieldFlags.Autoincrement)
                if dbffld:Counter == 0
                    dbffld:Counter := 1
                endif
                if dbffld:IncStep == 0
                    dbffld:IncStep := 1
                endif
            ENDIF
            return true
        ENDIF
        RETURN FALSE

     METHOD _SetFoxHeader() AS VOID
        LOCAL lVar AS LOGIC
        LOCAL lAutoIncr AS LOGIC
        // check for foxpro field types and adjust the header
        lVar        := FALSE
        lAutoIncr   := FALSE
        FOREACH VAR fld IN _Fields
            SWITCH fld:FieldType
            CASE DBFieldType.VarChar
            CASE DBFieldType.VarBinary
            CASE DBFieldType.Blob
                lVar := TRUE
            END SWITCH
            IF fld:Flags:HasFlag(DBFFieldFlags.AutoIncrement)
                lAutoIncr := TRUE
            ENDIF
        NEXT
        IF lAutoIncr
            IF lVar
                SELF:_Header:Version := DbfVersion.VisualFoxProVarChar
            ELSE
                SELF:_Header:Version := DbfVersion.VisualFoxProAutoIncrement
            ENDIF
        ELSEIF lVar
            SELF:_Header:Version := DbfVersion.VisualFoxProVarChar
        ELSE
            SELF:_Header:Version := DbfVersion.VisualFoxPro
        ENDIF
        SELF:_Header:HeaderLen += 263
        SELF:_HeaderLength   += 263
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
            IF Fld:IsVarLength
                NullCount += 1
            ENDIF
            IF string.Compare(fld:Name, _NULLFLAGS, TRUE) == 0
                nullFld := fld
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


    /// <inheritdoc />
    METHOD AddField(info AS RddFieldInfo) AS LOGIC
        LOCAL isOk AS LOGIC
        isok := SUPER:AddField( info )
        IF String.Compare(info:Name, _NULLFLAGS,TRUE) == 0 .AND. info IS DbfNullColumn VAR dbfnc
            SELF:_NullColumn := dbfnc
        ENDIF
        IF info IS DbfColumn VAR column
            IF column:isVarLength
                column:LengthBit := SELF:_NullCount++
            ENDIF
            IF column:IsNullable
                column:NullBit := SELF:_NullCount++
            ENDIF
        ENDIF
        RETURN isOk

    OVERRIDE PROTECTED METHOD _readRecord() AS LOGIC
        LOCAL lOk AS LOGIC
        lOk := SUPER:_readRecord()
        IF lOk .AND. SELF:_NullColumn != NULL
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

END NAMESPACE
