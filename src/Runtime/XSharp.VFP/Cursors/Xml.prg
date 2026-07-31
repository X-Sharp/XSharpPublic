//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System
using System.IO
using System.Data
using System.Text
using System.Xml
using XSharp.RDD
using XSharp.RDD.Support
using System.Globalization

// nFlags values for CursorToXml(). Only the ones implemented are listed here:
DEFINE C2X_FLAG_CONTINUOUS := 1 // Produce unformatted XML as one continuous string
DEFINE C2X_FLAG_TOFILE := 512 // Send the output to the file name in cOutput


/// <include file="VFPDocs.xml" path="Runtimefunctions/cursortoxml/*" />
[FoxProFunction("CURSORTOXML", FoxFunctionCategory.General, FoxEngine.RuntimeCore, FoxFunctionStatus.Partial, FoxCriticality.Medium)];
FUNCTION CursorToXML (uArea, cOutput, nOutputFormat, nFlags, nRecords, cSchemaName, cSchemaLocation, cNameSpace ) AS USUAL CLIPPER
    local nArea as dword
    local cTarget as string
    local nFlagsInt as long
    local nRecs as long
    local cSchema as string
    local cNs as string
    local cXml as string

    if !IsString(cOutput)
        throw Error.VoDbError(EG_ARG, EDB_PARAM, __FUNCTION__, nameof(cOutput), 2, <object>{cOutput})
    endif

    cTarget := (string) cOutput
    nFlagsInt := (long) iif(IsNumeric(nFlags), nFlags, 0)
    nRecs := (long) iif(IsNumeric(nRecords), nRecords, 0)
    cSchema := (string) iif(IsString(cSchemaName), cSchemaName, "")
    cNs := (string) iif(IsString(cNameSpace), cNameSpace, "")

    // nOutputformat 1 = ELEMENTS (default), 2 = ATTRIBUTES, 3 = RAW.
    // Only ELEMENTS is produces; the parameter is accepted but ignored

    // uArea: NIL or 0 means the current work area
    if IsNil(uArea) .OR. (IsNumeric(uArea) .AND. (long) uArea == 0)
        nArea := RuntimeState.CurrentWorkarea
    else
        nArea := _AreaFromParam(uArea)
    endif

    if nArea == 0
        throw Error.VoDbError(EG_ARG, EDB_BADALIAS, __FUNCTION__, nameof(uArea), 1, <object>{uArea})
    endif

    var nOldArea := RuntimeState.CurrentWorkarea
    RuntimeState.CurrentWorkarea := nArea
    try
        cXml := __FoxCursorToXmlString(nRecs, cSchema, cNs, nFlagsInt)
    finally
        RuntimeState.CurrentWorkarea := nOldArea
    end try

    if _AND(nFlagsInt, C2X_FLAG_TOFILE) != 0
        File.WriteAllText(cTarget, cXml, UTF8Encoding{FALSE})
        return (long) FileInfo{cTarget}:Length
    endif

    // no file flag: cOutput is the name of a memory variable, create when missing
    XSharp.MemVar.Put(cTarget, cXml)
    RETURN Encoding.UTF8:GetByteCount(cXml)

/// <summary>Builds the XML for the cursor in the current work area.</summary>
INTERNAL FUNCTION __FoxCursorToXmlString(nRecs as long, cSchema as string, cNs as string, nFlags as long) as string
    local nMode as XmlWriteMode

    var oData := __FoxCursorDataTable(nRecs)

    // VFP always names the root element VFPData, whatever the output format is
    var oSet := DataSet{"VFPData"}
    if !String.IsNullOrEmpty(cNs)
        oSet:Namespace := cNs
    endif
    oSet:Tables:Add(oData)

    // cSchemaName: "" = no schema, "1" = inline schema, anything else = external .xsd
    nMode := XmlWriteMode.IgnoreSchema
    if cSchema == "1"
        nMode := XmlWriteMode.WriteSchema
    elseif !String.IsNullOrEmpty(cSchema)
        var cSchemaFile := cSchema
        if String.IsNullOrEmpty(Path.GetExtension(cSchemaFile))
            cSchemaFile += ".xsd"
        endif
        oSet:WriteXmlSchema(cSchemaFile)
    endif

    var oSettings := XmlWriterSettings{}
    oSettings:Indent := _AND(nFlags, C2X_FLAG_CONTINUOUS) == 0
    oSettings:Encoding := UTF8Encoding{FALSE}

    var oStream := MemoryStream{}
    var oWriter := XmlWriter.Create(oStream, oSettings)
    oSet:WriteXml(oWriter, nMode)
    oWriter:Flush()
    oWriter:Close()

    return Encoding.UTF8:GetString(oStream:ToArray())


/// <summary>Reads the cursor in the current work area into a table of preformatted values.</summary>
INTERNAL FUNCTION __FoxCursorDataTable(nRecs as long) as DataTable
    local oResult := NULL as object

    if !CoreDb.Info(DBI_RDD_OBJECT, REF oResult)
        throw Error.VoDbError(EG_NOTABLE, EDB_NOTABLE, __FUNCTION__, "uArea", 1, <object>{})
    endif
    var oRDD    := (IRdd) oResult
    var nFields := oRDD:FieldCount
    var oTable  := DataTable{oRDD:Alias:ToLower()}

    // Every column is written as text: that is the only way to reproduce the
    // field width and decimal scale that VFP puts in the XML.
    local aTypes as string[]
    local aDecs  as long[]
    aTypes := string[]{nFields}
    aDecs  := long[]{nFields}
    for var nI := 1 to nFields
        var cName  := ((string) oRDD:FieldInfo(nI, DBS_ALIAS, NULL)):ToLower()
        aTypes[nI] := ((string) oRDD:FieldInfo(nI, DBS_TYPE, NULL)):ToUpper()
        aDecs[nI]  := (long) oRDD:FieldInfo(nI, DBS_DEC, NULL)
        oTable:Columns:Add(cName, typeof(string))
    next

    // The record order and any active filter are honoured because we simply
    // walk the work area from top to bottom.
    var nWritten := 0
    oRDD:GoTop()
    do while !oRDD:EoF
        local oRow as object[]
        oRow := object[]{nFields}
        for var nI := 1 to nFields
            oRow[nI] := __FoxXmlValue(oRDD:GetValue(nI), aTypes[nI], aDecs[nI])
        next
        oTable:Rows:Add(oRow)
        nWritten++
        // VFP does not restore the record pointer: it ends up at EOF when every
        // record was written, or on the last record written when nRecords limits it
        if nRecs > 0 .AND. nWritten >= nRecs
            exit
        endif
        oRDD:Skip(1)
    enddo
    oTable:AcceptChanges()
    return oTable


/// <summary>Formats one field value the way the VFP CursorToXml() output does.</summary>
INTERNAL FUNCTION __FoxXmlValue(oValue as object, cType as string, nDec as long) as object
    local r8 as real8

    // VFP leaves the element out altogether for a NULL field
    if oValue == NULL .OR. oValue == DBNull.Value
        return DBNull.Value
    endif

    switch cType
    case "L"
        return iif((logic) oValue, "true", "false")

    case "D"
        if oValue IS XSharp.IDate VAR dVal
            // an empty date becomes an empty element, not a missing one
            return iif(dVal:Month == 0, "", dVal:Value:ToString("yyyy-MM-dd", CultureInfo.InvariantCulture))
        endif
        return oValue:ToString()

    case "T"
        local dtVal as DateTime
        if oValue IS XSharp.IDate VAR dtSrc
            dtVal := dtSrc:Value
        else
            dtVal := Convert.ToDateTime(oValue, CultureInfo.InvariantCulture)
        endif
        // "s" gives the ISO layout VFP uses: 2024-01-15T10:30:45
        return iif(dtVal:Year <= 1, "", dtVal:ToString("s", CultureInfo.InvariantCulture))

    case "Y"
        // VFP always writes currency with 4 decimals
        r8 := __FoxXmlNumber(oValue)
        return r8:ToString("F4", CultureInfo.InvariantCulture)

    case "I"
        r8 := __FoxXmlNumber(oValue)
        return r8:ToString("F0", CultureInfo.InvariantCulture)

    case "N"
    case "F"
        r8 := __FoxXmlNumber(oValue)
        return r8:ToString("F" + nDec:ToString(), CultureInfo.InvariantCulture)

    case "B"
        // The RDD does not report the scale of Double fields, so I cannot
        // reproduce the fixed decimals VFP writes; emit the full value instead
        // of truncating it.
        r8 := __FoxXmlNumber(oValue)
        if nDec > 0
            return r8:ToString("F" + nDec:ToString(), CultureInfo.InvariantCulture)
        endif
        return r8:ToString("R", CultureInfo.InvariantCulture)
    end switch

    // C, M, V and anything else: VFP strips the padding blanks
    return oValue:ToString():TrimEnd()


/// <summary>Unwraps the numeric types the RDD layer hands out.</summary>
INTERNAL FUNCTION __FoxXmlNumber(oValue as object) as real8
    if oValue IS XSharp.IFloat VAR fVal
        return fVal:Value
    endif
    return Convert.ToDouble(oValue, CultureInfo.InvariantCulture)
