//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System
using System.IO
using System.Data
using System.Text
using System.Collections.Generic
using System.Xml
using XSharp.RDD
using XSharp.RDD.Enums
using XSharp.RDD.Support
using System.Globalization

// nFlags values for CursorToXml(). Only the ones implemented are listed here:
DEFINE C2X_FLAG_CONTINUOUS := 1 // Produce unformatted XML as one continuous string
DEFINE C2X_FLAG_TOFILE := 512 // Send the output to the file name in cOutput

// nFlags values for XmlToCursor(). Only the ones implemented are listed here:
DEFINE X2C_FLAG_FROMFILE  := 512    // The first parameter is a file name, not XML text
DEFINE X2C_FLAG_NOCPTRANS := 1024   // Character and Memo fields get the NOCPTRANS flag
DEFINE X2C_FLAG_CURRENCY  := 2048   // decimal(19,4) becomes Currency instead of Numeric
DEFINE X2C_FLAG_APPEND    := 8192   // cCursorName names an existing cursor to append to
DEFINE X2C_FLAG_VARCHAR   := 65536  // Character fields become VarChar

// VFP names the cursor XMLRESULT when the caller does not name one
DEFINE X2C_DEFAULTCURSOR := "XMLRESULT"
// Anything wider than this becomes a Memo instead of a Character field
DEFINE X2C_MAXCHARLEN := 254
DEFINE X2C_XSDNAMESPACE := "http://www.w3.org/2001/XMLSchema"

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

/// <include file="VFPDocs.xml" path="Runtimefunctions/xmltocursor/*" />
[FoxProFunction("XMLTOCURSOR", FoxFunctionCategory.General, FoxEngine.RuntimeCore, FoxFunctionStatus.Partial, FoxCriticality.High)];
FUNCTION XmlToCursor( eExpression, cCursorName, nFlags ) AS USUAL CLIPPER
    local cAlias    as string
    local nFlagsInt as long

    if !IsString(eExpression)
        throw Error.VoDbError(EG_ARG, EDB_PARAM, __FUNCTION__, nameof(eExpression), 1, <object>{eExpression})
    endif

    nFlagsInt := (long) iif(IsNumeric(nFlags), nFlags, 0)
    cAlias    := (string) iif(IsString(cCursorName), cCursorName, "")

    var oDoc := __FoxXmlLoad((string) eExpression, _AND(nFlagsInt, X2C_FLAG_FROMFILE) != 0)
    var oSchema := __FoxXmlSchemaTypes(oDoc)
    var oTable  := __FoxXmlDataTable(oDoc)

    if oTable != NULL
        if _AND(nFlagsInt, X2C_FLAG_APPEND) != 0
            __FoxXmlAppendRows(oTable, cAlias)
        else
            __FoxXmlBuildCursor(oTable, oSchema, ;
                iif(String.IsNullOrEmpty(cAlias), X2C_DEFAULTCURSOR, cAlias), nFlagsInt)
        endif
    endif

    return __FoxXmlCurrentRecCount()

/// <summary>Parses the XML once, from a string or from a file.</summary>
INTERNAL FUNCTION __FoxXmlLoad(cSource as string, lFromFile as logic) as XmlDocument
    var oDoc := XmlDocument{}
    oDoc:XmlResolver := NULL
    try
        if lFromFile
            if !File.Exists(cSource)
                // VFP reports a file it cannot open as a parse error, not a file error
                throw FileNotFoundException{cSource}
            endif
            oDoc:Load(cSource)
        else
            oDoc:LoadXml(cSource)
        endif
    catch e as Exception
        throw Exception{ __VfpStr(VFPErrors.VFP_XML_PARSE_ERROR, e:Message) }
    end try
    return oDoc

/// <summary>Reads the rows as a table of strings, whatever the document declares.</summary>
INTERNAL FUNCTION __FoxXmlDataTable(oDoc as XmlDocument) as DataTable
    var oSet := DataSet{}
    oSet:Locale := CultureInfo.InvariantCulture
    try
        using var oReader := XmlNodeReader{oDoc}
        oSet:ReadXml(oReader, XmlReadMode.InferSchema)
    catch e as Exception
        throw Exception{ __VfpStr(VFPErrors.VFP_XML_PARSE_ERROR, e:Message) }
    end try

    if oSet:Tables:Count == 0
        return NULL
    endif

    if oSet:DataSetName == "NewDataSet"
        throw Exception{ __VfpStr(VFPErrors.VFP_XML_NOSCHEMA) }
    endif

    return oSet:Tables[0]

/// <summary>The columns that carry data, in document order.</summary>
INTERNAL FUNCTION __FoxXmlDataColumns(oTable as DataTable) as List<DataColumn>
    local lHasElements := FALSE as logic
    var oResult := List<DataColumn>{}

    foreach oColumn as DataColumn in oTable:Columns
        if oColumn:ColumnMapping == MappingType.Element
            lHasElements := TRUE
            exit
        endif
    next

    foreach oColumn as DataColumn in oTable:Columns
        switch oColumn:ColumnMapping
        case MappingType.Element
            oResult:Add(oColumn)
        case MappingType.Attribute
            if !lHasElements
                oResult:Add(oColumn)
            endif
        end switch
    next
    return oResult

/// <summary>
/// Maps element name to the field its inline xsd type calls for. The schema is
/// read out of the document text rather than through XmlSchemaSet:Compile(),
/// because the object model only exposes the facets we need through the
/// post-schema-compilation infoset.
/// </summary>
INTERNAL FUNCTION __FoxXmlSchemaTypes(oDoc as XmlDocument) as Dictionary<string, _FoxXmlColumn>
    var oResult := Dictionary<string, _FoxXmlColumn>{StringComparer.OrdinalIgnoreCase}

    var oNs := XmlNamespaceManager{oDoc:NameTable}
    oNs:AddNamespace("xsd", X2C_XSDNAMESPACE)
    var oNodes := oDoc:SelectNodes("//xsd:schema//xsd:element[@name]", oNs)
    if oNodes == NULL
        return oResult
    endif
    foreach oNode as XmlNode in oNodes
        if oNode is XmlElement var oElement
            // the dataset and the row element carry a complexType: not fields
            if oElement:SelectSingleNode("xsd:complexType", oNs) == NULL
                __FoxXmlSchemaField(oElement, oNs, oResult)
            endif
        endif
    next
    return oResult

/// <summary>Reads one field declaration out of the inline schema.</summary>
INTERNAL FUNCTION __FoxXmlSchemaField(oElement as XmlElement, oNs as XmlNamespaceManager, ;
        oResult as Dictionary<string, _FoxXmlColumn>) as void
    local cBase     as string
    local nMaxLen   := 0 as long
    local nTotal    := 0 as long
    local nFraction := 0 as long

    var cName := oElement:GetAttribute("name")
    if String.IsNullOrEmpty(cName) .or. oResult:ContainsKey(cName)
        return
    endif

    cBase := oElement:GetAttribute("type")
    if oElement:SelectSingleNode("xsd:simpleType/xsd:restriction", oNs) is XmlElement var oRestriction
        cBase := oRestriction:GetAttribute("base")
        foreach oFacet as XmlNode in oRestriction:ChildNodes
            if oFacet is XmlElement var oFacetElement
                var nValue := __FoxXmlFacetValue(oFacetElement:GetAttribute("value"))
                switch oFacetElement:LocalName
                case "maxLength"
                    nMaxLen := nValue
                case "totalDigits"
                    nTotal := nValue
                case "fractionDigits"
                    nFraction := nValue
                end switch
            endif
        next
    endif

    oResult:Add(cName, __FoxXmlColumnFromType(cName, __FoxXmlLocalName(cBase), nMaxLen, nTotal, nFraction))
    return

/// <summary>Drops the namespace prefix from an xsd type name: "xsd:decimal" gives "decimal".</summary>
INTERNAL FUNCTION __FoxXmlLocalName(cType as string) as string
    if String.IsNullOrEmpty(cType)
        return ""
    endif
    var nPos := cType:IndexOf(c':')
    if nPos < 0
        return cType
    endif
    return cType:Substring(nPos + 1)

/// <summary>Turns one xsd type into the field VFP 9 builds from it.</summary>
INTERNAL FUNCTION __FoxXmlColumnFromType(cName as string, cBase as string, ;
        nMaxLen as long, nTotal as long, nFraction as long) as _FoxXmlColumn

    switch cBase
    case "boolean"
        return _FoxXmlColumn{cName, "L", 1, 0}
    case "date"
        return _FoxXmlColumn{cName, "D", 8, 0}
    case "dateTime"
        return _FoxXmlColumn{cName, "T", 8, 0}
    case "int"
    case "integer"
    case "short"
    case "long"
    case "unsignedInt"
    case "unsignedShort"
        return _FoxXmlColumn{cName, "I", 4, 0}
    case "double"
    case "float"
        return _FoxXmlColumn{cName, "B", 8, 8}
    case "decimal"
        var oDecimal := _FoxXmlColumn{cName, "N", nTotal + iif(nFraction > 0, 1, 0), nFraction}
        oDecimal:IsCurrency := (nTotal == 19 .and. nFraction == 4)
        return oDecimal
    end switch

    if nMaxLen <= 0 .or. nMaxLen > X2C_MAXCHARLEN
        return _FoxXmlColumn{cName, "M", 4, 0}
    endif
    return _FoxXmlColumn{cName, "C", nMaxLen, 0}

/// <summary>Reads one xsd facet, which the schema object model hands over as a string.</summary>
INTERNAL FUNCTION __FoxXmlFacetValue(cValue as string) as long
    local nResult as long
    if Int32.TryParse(cValue, NumberStyles.Integer, CultureInfo.InvariantCulture, out nResult)
        return nResult
    endif
    return 0

/// <summary>Infers the field for one column from its values, the way VFP 9 does.</summary>
INTERNAL FUNCTION __FoxXmlInferColumn(cName as string, oTable as DataTable, oColumn as DataColumn) as _FoxXmlColumn
    local lHasEmpty := FALSE as logic
    local lAnyValue := FALSE as logic
    local lAllBool  := TRUE  as logic
    local lAllNum   := TRUE  as logic
    local lAllDate  := TRUE  as logic
    local lAllTime  := TRUE  as logic
    local nMaxLen   := 0     as long
    local nMaxDec   := 0     as long

    foreach oRow as DataRow in oTable:Rows
        var cValue := __FoxXmlText(oRow, oColumn)
        if cValue:Length == 0
            lHasEmpty := TRUE
            loop
        endif
        lAnyValue := TRUE
        nMaxLen   := Math.Max(nMaxLen, cValue:Length)
        if !__FoxXmlIsBool(cValue)
            lAllBool := FALSE
        endif
        var nDec := __FoxXmlDecimals(cValue)
        if nDec < 0
            lAllNum := FALSE
        else
            nMaxDec := Math.Max(nMaxDec, nDec)
        endif
        if !__FoxXmlIsDate(cValue)
            lAllDate := FALSE
        endif
        if !__FoxXmlIsDateTime(cValue)
            lAllTime := FALSE
        endif
    next

    do case
    case !lAnyValue
        return _FoxXmlColumn{cName, "C", 1, 0}
    case lAllBool .and. !lHasEmpty
        return _FoxXmlColumn{cName, "L", 1, 0}
    case lAllNum
        return _FoxXmlColumn{cName, "N", nMaxLen, nMaxDec}
    case lAllDate .and. !lHasEmpty
        return _FoxXmlColumn{cName, "D", 8, 0}
    case lAllTime .and. !lHasEmpty
        return _FoxXmlColumn{cName, "T", 8, 0}
    case nMaxLen > X2C_MAXCHARLEN
        return _FoxXmlColumn{cName, "M", 4, 0}
    endcase
    return _FoxXmlColumn{cName, "C", nMaxLen, 0}

/// <summary>The values VFP reads back as Logical.</summary>
INTERNAL FUNCTION __FoxXmlIsBool(cValue as string) as logic
    switch cValue:ToLowerInvariant()
    case "0"
    case "1"
    case "true"
    case "false"
        return TRUE
    end switch
    return FALSE

/// <summary>
/// Number of decimals when the value is one VFP infers as Numeric, -1 otherwise.
/// A sign is deliberately rejected: VFP infers Character for "-5" and for "+5",
/// and Character for "1e3" as well. That is almost certainly a bug in the parser
/// VFP infers with, but migrated code sees the resulting structure, so it is
/// reproduced here. A leading or trailing dot is accepted, matching VFP: ".5"
/// gives N(2,1) and "5." gives N(2,0).
/// </summary>
INTERNAL FUNCTION __FoxXmlDecimals(cValue as string) as long
    local nDot      := -1    as long
    local lAnyDigit := FALSE as logic

    for var nI := 0 upto cValue:Length - 1
        var cChar := cValue[nI]
        if cChar == c'.'
            if nDot >= 0
                return -1
            endif
            nDot := nI
        elseif Char.IsDigit(cChar)
            lAnyDigit := TRUE
        else
            return -1
        endif
    next
    if !lAnyDigit
        return -1
    endif
    if nDot < 0
        return 0
    endif
    return cValue:Length - nDot - 1

/// <summary>True when the value is an xsd date, the only date layout VFP infers.</summary>
INTERNAL FUNCTION __FoxXmlIsDate(cValue as string) as logic
    local dValue as DateTime
    return DateTime.TryParseExact(cValue, "yyyy-MM-dd", CultureInfo.InvariantCulture, ;
        DateTimeStyles.None, out dValue)

/// <summary>True when the value is an xsd dateTime.</summary>
INTERNAL FUNCTION __FoxXmlIsDateTime(cValue as string) as logic
    local dValue   as DateTime
    local aFormats as string[]
    aFormats := <string>{"yyyy-MM-ddTHH:mm:ss", "yyyy-MM-ddTHH:mm:ss.FFFFFFF"}
    return DateTime.TryParseExact(cValue, aFormats, CultureInfo.InvariantCulture, ;
        DateTimeStyles.None, out dValue)

/// <summary>The text of one cell, trimmed the way VFP trims it.</summary>
INTERNAL FUNCTION __FoxXmlText(oRow as DataRow, oColumn as DataColumn) as string
    if oRow:IsNull(oColumn)
        return ""
    endif
    // VFP strips the surrounding blanks unless nFlags 4 asks it not to; that flag
    // is not implemented, so the values are always trimmed
    var cValue := oRow[oColumn]:ToString()
    if cValue == NULL
        return ""
    endif
    return cValue:Trim()

/// <summary>Applies the flags that change the type of a whole column.</summary>
INTERNAL FUNCTION __FoxXmlApplyFlags(oColumn as _FoxXmlColumn, nFlags as long) as void
    if oColumn:IsCurrency .and. _AND(nFlags, X2C_FLAG_CURRENCY) != 0
        oColumn:FldType := "Y"
        oColumn:Len     := 8
        oColumn:Dec     := 4
    endif
    if _AND(nFlags, X2C_FLAG_NOCPTRANS) != 0
        if oColumn:FldType == "C" .or. oColumn:FldType == "M"
            oColumn:Flags := _OR(oColumn:Flags, (long) DBFFieldFlags.Binary)
            // VFP doubles the width of a Character field read with NOCPTRANS
            if oColumn:FldType == "C"
                oColumn:Len := Math.Min(oColumn:Len * 2, X2C_MAXCHARLEN)
            endif
        endif
    endif
    if _AND(nFlags, X2C_FLAG_VARCHAR) != 0 .and. oColumn:FldType == "C"
        oColumn:FldType := "V"
    endif
    return

/// <summary>Creates the cursor, selects it and fills it.</summary>
INTERNAL FUNCTION __FoxXmlBuildCursor(oTable as DataTable, oSchema as Dictionary<string, _FoxXmlColumn>, ;
        cAlias as string, nFlags as long) as void
    local oColumn as _FoxXmlColumn
    var aColumns := List<_FoxXmlColumn>{}

    var aSource := __FoxXmlDataColumns(oTable)
    foreach oSrc as DataColumn in aSource
        if oSchema:ContainsKey(oSrc:ColumnName)
            oColumn := oSchema[oSrc:ColumnName]:Clone()
        else
            oColumn := __FoxXmlInferColumn(oSrc:ColumnName, oTable, oSrc)
        endif
        __FoxXmlApplyFlags(oColumn, nFlags)
        aColumns:Add(oColumn)
    next

    if aColumns:Count == 0
        return
    endif

    var aStruct := {}
    foreach oCol as _FoxXmlColumn in aColumns
        AAdd(aStruct, {oCol:Name, oCol:FldType, oCol:Len, oCol:Dec, oCol:Name, oCol:Flags})
    next

    if RuntimeState.Workareas:FindAlias(cAlias) != 0
        DbCloseArea(cAlias)
    endif
    DbCreate(Path.GetTempFileName(), aStruct, "DBFVFP", TRUE, cAlias)
    DbSelectArea(cAlias)

    __FoxXmlFillCursor(oTable, aSource, aColumns)
    return

/// <summary>Writes the rows into the cursor selected in the current work area.</summary>
INTERNAL FUNCTION __FoxXmlFillCursor(oTable as DataTable, aSource as List<DataColumn>, ;
        aColumns as List<_FoxXmlColumn>) as void
    local oResult := NULL as object

    if !CoreDb.Info(DBI_RDD_OBJECT, REF oResult)
        throw Error.VoDbError(EG_NOTABLE, EDB_NOTABLE, __FUNCTION__, "cCursorName", 2, <object>{})
    endif
    var oRDD := (IRdd) oResult

    foreach oRow as DataRow in oTable:Rows
        oRDD:Append(TRUE)
        for var nI := 0 upto aColumns:Count - 1
            __FoxXmlPut(oRDD, nI + 1, __FoxXmlToField(__FoxXmlText(oRow, aSource[nI]), aColumns[nI]:FldType))
        next
    next
    oRDD:GoTop() // VFP leaves the pointer on the first row
    return

/// <summary>Appends the rows to a cursor that already exists, matching fields by name.</summary>
INTERNAL FUNCTION __FoxXmlAppendRows(oTable as DataTable, cAlias as string) as void
    local oResult := NULL as object
    var nArea := iif(String.IsNullOrEmpty(cAlias), RuntimeState.CurrentWorkarea, ;
        RuntimeState.Workareas:FindAlias(cAlias))
    if nArea == 0
        throw Error.VoDbError(EG_ARG, EDB_BADALIAS, __FUNCTION__, "cCursorName", 2, <object>{cAlias})
    endif

    var nOldArea := RuntimeState.CurrentWorkarea
    RuntimeState.CurrentWorkarea := nArea
    try
        if !CoreDb.Info(DBI_RDD_OBJECT, REF oResult)
            throw Error.VoDbError(EG_NOTABLE, EDB_NOTABLE, __FUNCTION__, "cCursorName", 2, <object>{cAlias})
        endif
        var oRDD := (IRdd) oResult

        var aSource := __FoxXmlDataColumns(oTable)
        var aTarget := List<long>{}
        var aTypes  := List<string>{}
        foreach oColumn as DataColumn in aSource
            var nField := oRDD:FieldIndex(oColumn:ColumnName)
            aTarget:Add(nField)
            aTypes:Add(iif(nField > 0, ((string) oRDD:FieldInfo(nField, DBS_TYPE, NULL)):ToUpper(), ""))
        next

        foreach oRow as DataRow in oTable:Rows
            oRDD:Append(TRUE)
            for var nI := 0 upto aTarget:Count - 1
                if aTarget[nI] > 0
                    __FoxXmlPut(oRDD, aTarget[nI], __FoxXmlToField(__FoxXmlText(oRow, aSource[nI]), aTypes[nI]))
                endif
            next
        next
        oRDD:GoTop()
    finally
        RuntimeState.CurrentWorkarea := nOldArea
    end try
    return

/// <summary>
/// Writes one value, swallowing the error when it does not fit its field. The
/// inferred width is the longest value seen and VFP never reconciles it with the
/// decimals it infers alongside, so a column holding 99999 and 0.1 becomes N(5,1)
/// and the 99999 no longer fits. VFP parks its numeric overflow marker there; our
/// RDD refuses the value outright. The structure is still the one VFP builds, so
/// the field is left blank rather than failing the whole import.
/// </summary>
INTERNAL FUNCTION __FoxXmlPut(oRDD as IRdd, nField as long, oValue as object) as void
    try
        oRDD:PutValue(nField, oValue)
    catch
        // the field keeps its blank value
    end try
    return

/// <summary>Converts one XML text value to the type of the field it goes into.</summary>
INTERNAL FUNCTION __FoxXmlToField(cValue as string, cType as string) as object
    local dValue  as DateTime
    local r8Value as real8
    local nValue  as long

    switch cType
    case "L"
        switch cValue:ToLowerInvariant()
        case "1"
        case "true"
            return TRUE
        end switch
        return FALSE
    case "D"
    case "T"
        if DateTime.TryParse(cValue, CultureInfo.InvariantCulture, DateTimeStyles.None, out dValue)
            return dValue
        endif
        return DateTime.MinValue
    case "N"
    case "F"
    case "Y"
    case "B"
        if System.Double.TryParse(cValue, NumberStyles.Float, CultureInfo.InvariantCulture, out r8Value)
            return r8Value
        endif
        return 0.0
    case "I"
        if Int32.TryParse(cValue, NumberStyles.Integer, CultureInfo.InvariantCulture, out nValue)
            return nValue
        endif
        return 0
    end switch
    return cValue

/// <summary>RecCount() of the work area currently selected, 0 when there is none.</summary>
INTERNAL FUNCTION __FoxXmlCurrentRecCount() as long
    local oResult := NULL as object
    if !CoreDb.Info(DBI_RDD_OBJECT, REF oResult)
        return 0
    endif
    return (long) ((IRdd) oResult):RecCount

/// <summary>One field of the cursor XmlToCursor() is about to build.</summary>
INTERNAL CLASS _FoxXmlColumn
    INTERNAL Name       AS STRING
    INTERNAL FldType    AS STRING
    INTERNAL Len        AS LONG
    INTERNAL Dec        AS LONG
    INTERNAL Flags      AS LONG
    /// <summary>Set for a decimal(19,4), which nFlags 2048 turns into Currency.</summary>
    INTERNAL IsCurrency AS LOGIC

    INTERNAL CONSTRUCTOR(cName as string, cType as string, nLen as long, nDec as long)
        SELF:Name       := cName
        SELF:FldType    := cType
        SELF:Len        := nLen
        SELF:Dec        := nDec
        SELF:Flags      := 0
        SELF:IsCurrency := FALSE
    END CONSTRUCTOR

    /// <summary>The schema dictionary is reused, so the flags are applied to a copy.</summary>
    INTERNAL METHOD Clone() as _FoxXmlColumn
        var oResult := _FoxXmlColumn{SELF:Name, SELF:FldType, SELF:Len, SELF:Dec}
        oResult:Flags      := SELF:Flags
        oResult:IsCurrency := SELF:IsCurrency
        return oResult
    END METHOD
END CLASS
