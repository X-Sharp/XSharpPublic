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
USING STATIC XSharp.Conversions

BEGIN NAMESPACE XSharp.RDD
/// <summary>DBFVFP RDD. DBFCDX with support for the FoxPro field types.</summary>
[DebuggerDisplay("DBFVFP ({Alias,nq})")];
CLASS DBFVFP INHERIT DBFCDX
    PRIVATE CONST VFP_BACKLINKSIZE := 262 AS LONG
    PRIVATE oDbcTable as DbcTable
	CONSTRUCTOR()
		SUPER()
		SELF:_AllowedFieldTypes := "BCDFGILMNPQTVWY0"
		RETURN

	OVERRIDE PROPERTY Driver         AS STRING GET nameof(DBFVFP)
    INTERNAL PROPERTY DbcName        AS STRING AUTO
    INTERNAL PROPERTY DbcPosition    AS INT GET DbfHeader.SIZE + SELF:_Fields:Length  * DbfField.SIZE +1
    INTERNAL PROPERTY DeleteOnClose  AS LOGIC AUTO

    OVERRIDE METHOD Close() AS LOGIC
        LOCAL lOk AS LOGIC
        // This method deletes the temporary file after the file is closed
        LOCAL cFileName := SELF:_FileName AS STRING
        LOCAL cMemoName := "" AS STRING

        IF SELF:_Memo IS AbstractMemo VAR memo
            cMemoName := memo:FileName
        ENDIF
        lOk := SUPER:Close()
        IF lOk .and. SELF:DeleteOnClose
            IF File(cFileName)
                FErase(FPathName())
            ENDIF
            IF ! String.IsNullOrEmpty(cMemoName) .AND. File(cMemoName)
                FErase(FPathName())
            ENDIF
            // delete production index
            cFileName := System.IO.Path.ChangeExtension(cFileName, "cdx")
            IF File(cFileName)
                FErase(FPathName())
            ENDIF

        ENDIF
        RETURN lOk

    OVERRIDE METHOD Create( openInfo AS DbOpenInfo ) AS LOGIC
        LOCAL isOk AS LOGIC
        isOk := SUPER:Create(openInfo)
        IF isOk
            SELF:_SetFoxHeader()
        ENDIF
        // read fields again so the field flags are correct, since these depend on the FoxPro version in the header
        // that was just written
        SELF:_readFieldsHeader()

        SELF:_ReadDbcInfo()
        RETURN isOk

    PROTECTED OVERRIDE METHOD _checkField( dbffld REF DbfField) AS LOGIC
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
        LOCAL cExt := System.IO.Path.GetExtension(SELF:_FileName) AS STRING
        IF cExt:ToLower() == ".dbc"
            SELF:_Header:TableFlags  |= DBFTableFlags.IsDBC
        ENDIF
        SELF:_Header:HeaderLen += VFP_BACKLINKSIZE
        SELF:_HeaderLength   += VFP_BACKLINKSIZE
        SELF:_writeHeader()
        // Adjust the file size to accomodate the backlink data
        SELF:_putEndOfFileMarker()
        _oStream:SafeSetLength(SELF:_HeaderLength)
        RETURN

    /// <inheritdoc />
    OVERRIDE METHOD CreateFields(aFields AS RddFieldInfo[]) AS LOGIC
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
        lOk := super:CreateFields(aFields)
        RETURN lOk

        OVERRIDE PROPERTY FieldCount AS LONG
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

    OVERRIDE METHOD Open ( info AS DbOpenInfo) AS LOGIC
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
                IF RuntimeState.LastRddError != NULL
                    lOk := FALSE
                ENDIF
            ENDIF
            SELF:GoTop()
        ENDIF

        RETURN lOk

    OVERRIDE METHOD FieldInfo(nFldPos AS LONG, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
        IF nOrdinal == DbFieldInfo.DBS_PROPERTIES
           RETURN DbFieldInfo.DBS_FLAGS
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
        local cDbcFile as STRING
        cDbcFile := SELF:DbcName
        IF !System.IO.Path.IsPathRooted(cDbcFile)
            VAR cPath := System.IO.Path.GetDirectoryName(SELF:FileName)
            cDbcFile := System.IO.Path.Combine(cPath, cDbcFile)
        ENDIF
        Dbc.Open(cDbcFile, TRUE, TRUE, FALSE)
        VAR oDb := Dbc.FindDatabase(cDbcFile)
        IF oDb != NULL
            var base := System.IO.Path.GetFileNameWithoutExtension(SELF:FileName)
            var oTable := oDb:FindTable(System.IO.Path.GetFileName(base))
            IF oTable != NULL
                SELF:oDbcTable := oTable
                // assign aliases
                IF SELF:FieldCount == oTable:Fields:Count
                    LOCAL nPos AS LONG
                    FOR nPos := 1 TO SELF:FieldCount
                        LOCAL oColumn AS DbfColumn
                        VAR oField := oTable:Fields[nPos-1]
                        oColumn := SELF:_GetColumn(nPos) ASTYPE DbfColumn
                        IF oField:HasProperties
                            oColumn:Properties  := oField:Properties
                        ENDIF
                        oColumn:Alias       := oField:ObjectName
                        oColumn:ColumnName  := oField:ObjectName
                        IF String.Compare(oColumn:Name, oColumn:Alias, TRUE) != 0
                            // We add the alias as alternative to the fieldname.
                            // As a result we can call FieldIndex with the alias AND the fieldName
                            SELF:_fieldNames:Add(oColumn:Alias:ToUpper(), nPos-1)
                        ENDIF
                    NEXT
                ENDIF
            ENDIF
        ENDIF


    /// <inheritdoc />
    OVERRIDE METHOD AddField(info AS RddFieldInfo) AS LOGIC
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

    OVERRIDE METHOD Info(nOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
    LOCAL oResult AS OBJECT
        oResult := NULL
        BEGIN SWITCH nOrdinal
        CASE DbInfo.DBI_ISTEMPORARY
            oResult := SELF:DeleteOnClose
            IF oNewValue IS LOGIC VAR lValue
                SELF:DeleteOnClose := lValue
            ENDIF
        OTHERWISE
            oResult := SUPER:Info(nOrdinal, oNewValue)
        END SWITCH
        RETURN oResult

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

END NAMESPACE

