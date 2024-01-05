//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using XSharp.Parsers
using XSharp.Internal
using XSharp.RDD
using XSharp.RDD.Support
using System.IO
using System.Linq
[NeedsAccessToLocals(FALSE)];
FUNCTION __SqlInsertMemVar(sTable as STRING) AS LOGIC
    // FoxPro opens the table when needed and keeps it open
    // FoxPro only writes memvars that exist and also uses locals
    // Note that FoxPro does "optimistic" type conversions
    IF ! FoxEmbeddedSQL.OpenArea(sTable)
        return FALSE
    ENDIF
    DbAppend()
    local nMax    := FCount() as DWORD
    local nFld as DWORD
    FOR nFld := 1 to nMax
        var fName := FieldName(nFld)
        var uValue :=  XSharp.MemVar.GetSafe(fName)
        if uValue != nil
            FieldPut(nFld, FoxEmbeddedSQL.Convert(FieldGet(nFld), uValue))
        ENDIF
    NEXT
    DbUnLock()
    RETURN Used()


FUNCTION __SqlInsertFromObject(sTable as STRING, oValues as Object) AS Logic
    // FoxPro opens the table when needed and keeps it open
    // FoxPro enumerates the properties from the object and writes the properties that
    // have a name in the current area
    // Note that FoxPro does "optimistic" type conversions
    IF ! FoxEmbeddedSQL.OpenArea(sTable)
        return FALSE
    ENDIF
    DbAppend()
    if oValues IS XSharp.VFP.Empty VAR eValue
        var props := eValue:Properties
        foreach var prop in props
            var pos := FieldPos(prop:Name)
            IF pos > 0
                FieldPut(pos, FoxEmbeddedSQL.Convert(FieldGet(pos), prop:Value))
            endif
        next
    endif
    DbUnLock()
    RETURN Used()

FUNCTION __SqlInsertFromArray(sTable as STRING, aValues as ARRAY) AS LOGIC
    // FoxPro opens the table when needed and keeps it open
    // FoxPro walks the array and assigns values
    // Note that FoxPro does "optimistic" type conversions
    IF ! FoxEmbeddedSQL.OpenArea(sTable)
        return FALSE
    ENDIF
    DbAppend()
    local nMax := Min(FCount(), ALen(aValues)) as DWORD
    local nFld as DWORD
    FOR nFld := 1 to nMax
        FieldPut(nFld, FoxEmbeddedSQL.Convert(FieldGet(nFld), aValues[nFld]))
    NEXT
    DbUnLock()
    RETURN Used()

FUNCTION __SqlInsertValues(sTable as STRING, aFields as ARRAY, aValues as ARRAY) AS LOGIC
    // FoxPro opens the table when needed and keeps it open
    // what does foxpro do when aValues > # of fields
    // what does foxpro do when aValues < # of fields
    local i as DWORD
    local nMax as DWORD
    IF ! FoxEmbeddedSQL.OpenArea(sTable)
        return FALSE
    ENDIF
    IF ALen(aFields) == 0 .and. ALen(aValues) > 0
        nMax := ALen(aValues)
        aFields := ArrayNew(ALen(aValues))
        FOR i := 1 to nMax
            aFields[i] := FieldName(i)
        NEXT
    ELSE
        nMax := Min(ALen(aFields), ALen(aValues))
    ENDIF
    DbAppend()
    FOR i := 1 to nMax
        __FieldSet(aFields[i], aValues[i])
    NEXT
    DbUnLock()
    RETURN TRUE

FUNCTION __SqlAlterTable(sCommand as STRING) AS LOGIC
    var oContext := FoxEmbeddedSQL.ParseSqlAlter(sCommand)
    if (oContext != NULL)
        RETURN FoxEmbeddedSQL.SqlAlterTable(oContext)
    endif
    RETURN FALSE

FUNCTION __SqlCreateCursor(sCommand as STRING) AS LOGIC
    var oContext := FoxEmbeddedSQL.ParseSqlCreate(sCommand, TRUE)
    if (oContext != NULL)
        RETURN FoxEmbeddedSQL.CreateTableCursor(oContext)
    endif
    RETURN FALSE


FUNCTION __SqlCreateTable(sCommand as STRING) AS LOGIC
    // FoxPro creates the table and keeps it open
    var oContext := FoxEmbeddedSQL.ParseSqlCreate(sCommand, FALSE)
    if (oContext != NULL)
        RETURN FoxEmbeddedSQL.CreateTableCursor(oContext)
    endif
    THROW Error{"Syntax error in command: "+sCommand}

STATIC CLASS FoxEmbeddedSQL

    STATIC METHOD CreateTableCursor(oTable as FoxCreateTableContext) AS LOGIC
        if oTable != NULL
            local aStruct as ARRAY
            aStruct := {}
            foreach col as FoxColumnContext in oTable:Columns
                var aField := {col:Name, ((Char)col:FieldType):ToString(), col:Length, col:Decimals, col:Name, col:Flags}
                AAdd(aStruct, aField)
            next
            VAR cTable := oTable:Name
            if oTable:IsCursor
                cTable := System.IO.Path.GetTempFileName()
            ENDIF
            if RuntimeState.Workareas.FindAlias(oTable:Name) != 0
                DbCloseArea(oTable:Name)
            endif
            DbCreate(cTable, aStruct, "DBFVFP", TRUE, oTable:Name)
            DbCloseArea(oTable:Name)
            DbUseArea(TRUE, "DBFVFP", cTable, oTable:Name, FALSE, FALSE)
            FOR var nI := 1 to oTable:Columns:Count
                var oCol := oTable:Columns[nI-1]
                DbFieldInfo(DBS_CAPTION, nI, oCol:Caption)
            NEXT
            if oTable:IsCursor
                DbInfo(DBI_ISTEMPORARY, TRUE)
            endif
            RETURN TRUE
        endif
        RETURN FALSE
    STATIC METHOD ParseSqlCreate(sCommand as STRING, lCursor as LOGIC) AS FoxCreateTableContext
        VAR lexer := XSqlLexer{sCommand}
        VAR tokens := lexer:AllTokens()
        var parser := SQLParser{XTokenList{tokens}}
        local table as FoxCreateTableContext
        IF lCursor
            IF ! parser:ParseCreateCursor(out table)
                THROW Error{"Syntax error in command: "+parser.Error+CRLF+sCommand}
            ENDIF
        ELSE
            IF ! parser:ParseCreateTable(out table)
                THROW Error{"Syntax error in command: "+parser.Error+CRLF+sCommand}
            ENDIF
        endif
        return table

    STATIC METHOD ParseSqlAlter(sCommand as STRING) AS FoxAlterTableContext
        VAR lexer := XSqlLexer{sCommand}
        VAR tokens := lexer:AllTokens()
        var parser := SQLParser{XTokenList{tokens}}
        var table := parser:ParseAlterTable()
        if table == NULL
            Throw Error{parser:Error+" in command: "+sCommand}
        endif
        return table

    STATIC METHOD OpenArea(sTable as STRING) AS LOGIC
        IF ! DbSelectArea(sTable)
            DbUseArea(TRUE, "DBFVFP", sTable, sTable, TRUE, FALSE)
        ENDIF
        RETURN Used()

    STATIC METHOD Convert(currentVal as USUAL, newVal as USUAL) AS USUAL
        if UsualType(currentVal) == UsualType(newVal)
            return newVal
        endif
        local oValue as OBJECT
        oValue := __castclass(object, newVal)
        var sValue := oValue:ToString()
        SWITCH UsualType(currentVal)
        CASE __UsualType.Long
            if Int32.TryParse(sValue, OUT Var result)
                return result
            endif
            return 0
        CASE __UsualType.Float
        CASE __UsualType.Currency
            if Double.TryParse(sValue, OUT Var result)
                return result
            endif
            return 0.0
        CASE __UsualType.Date
            RETURN CToD(sValue)
        CASE __UsualType.String
        CASE __UsualType.Memo
            return sValue
        CASE __UsualType.Binary
            return (Binary) sValue
        END SWITCH
        RETURN NIL
STATIC METHOD SqlAlterTable(table as FoxAlterTableContext) AS LOGIC
    IF ! FoxEmbeddedSQL.OpenArea(table:Name)
        RETURN FALSE
    ENDIF
    var area := RuntimeState.Workareas.FindAlias(table:Name)
    if area == 0
        Throw Error{"Table "+table:Name+" not found"}
    ENDIF
    var oRdd := RuntimeState.Workareas.GetRDD(area)
    local fields := NULL as RddFieldInfo[]
    if oRdd is XSharp.RDD.Workarea var oWA
        fields := oWA:_Fields
    endif
    var column := table:ColumnInfo
    var index  := oRdd:FieldIndex(column:Name)
    local aStruct := DbStruct() as ARRAY
    switch table:Mode
    case FoxAlterMode.AddColumn
        if index > 0
            Throw Error{"Column "+column:Name+" already exists"}
        endif
        AAdd(aStruct, {column:Name, column:FieldTypeStr, column:Length, column:Decimals, column:Alias, column:Flags})

    case FoxAlterMode.DropColumn
        if index == 0
            Throw Error{"Column "+column:Name+" does not exist"}
        endif
        ADel(aStruct, (DWORD) index)
        ASize(aStruct, ALen(aStruct)-1)
    case FoxAlterMode.AlterColumn
        if index == 0
            Throw Error{"Column "+column:Name+" does not exist"}
        endif
        aStruct[index] := { column:Name, column:FieldTypeStr, column:Length, column:Decimals, column:Alias, column:Flags}
    end switch

    var oldFile  := (String) oRdd:Info(DBI_FULLPATH,null)
    var memoExt  := (String) oRdd:Info(DBI_MEMOEXT, null)
    var dbfExt   := Path.GetExtension(oldFile)
    var oldMemo  := Path.ChangeExtension(oldFile, memoExt)
    var rand     := Path.GetRandomFileName()
    var newFile  := Path.Combine(Path.GetDirectoryName(oldFile), Path.GetFileNameWithoutExtension(rand))
    newFile      := Path.ChangeExtension(newFile, dbfExt)
    var newMemo  := Path.ChangeExtension(newFile, memoExt)
    DbCloseArea()
    IF !DbCreate(newFile, aStruct)
        RETURN FALSE
    ENDIF
    IF ! DbUseArea(TRUE, "DBFVFP", newFile, table:Name, FALSE, FALSE)
        RETURN FALSE
    ENDIF
    IF ! DbApp(oldFile)
        RETURN FALSE
    ENDIF
    DbCloseArea()
    var cBak := Path.ChangeExtension(oldFile, "bak")
    FErase(cBak)
    FRename(oldFile, cBak)
    IF File(oldMemo)
        cBak := Path.ChangeExtension(oldFile, "tbk")
        FErase(cBak)
        FRename(oldMemo, cBak)
    ENDIF
    FRename(newFile, oldFile)
    IF File(newMemo)
        FRename(newMemo, oldMemo)
    ENDIF
    DbUseArea(TRUE, "DBFVFP", oldFile, table:Name, FALSE, FALSE)
    area := RuntimeState.Workareas.FindAlias(table:Name)
    oRdd := RuntimeState.Workareas.GetRDD(area)
    if fields != NULL .and. oRdd is XSharp.RDD.Workarea var oWANew
        foreach var fld in oWANew:_Fields
            var oldFld := fields:FirstOrDefault({f => f:Name == fld:Name})
            if oldFld != NULL
                fld:Caption := oldFld:Caption
            else // Must be the new column
                fld:Caption := column:Caption
            endif
        next
    endif
    RETURN TRUE
END CLASS
