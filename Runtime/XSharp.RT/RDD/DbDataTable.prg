// DbDataTable.prg
// Created by    : robert
// Creation Date : 4/7/2020 5:07:04 PM
// Created for   : 
// WorkStation   : ARTEMIS


USING System
USING System.Collections.Generic
USING System.Data
USING System.Reflection
USING XSharp.RDD
USING XSharp.RDD.Support


FUNCTION DbDataTable() AS DataTable
IF ! Used()
    RETURN NULL
ENDIF
LOCAL aStruct AS ARRAY
LOCAL nI, nFldCount AS LONG
LOCAL oRDD AS IRdd
oRDD := (IRdd) DbInfo(DBI_RDD_OBJECT)
LOCAL aColumns AS DbColumnInfo[]
aColumns := DbColumnInfo[]{oRDD:FieldCount}
aStruct  := DbStruct()
IF oRDD:Driver == "DBFVFPSQL"
    FOR nI := 1 TO aColumns:Length
        LOCAL oResult AS OBJECT
        oResult := oRDD:FieldInfo(nI, DBS_COLUMNINFO,NULL)
        IF oResult IS DbColumnInfo VAR info
            aColumns[nI] := info
        ELSE
            LOCAL aField  := aStruct[nI] AS ARRAY
            VAR oColumn := DbColumnInfo{aField[DBS_NAME], aField[DBS_TYPE]+":0", aField[DBS_LEN], aField[DBS_DEC]}
            aColumns[nI] := oColumn
        ENDIF
    NEXT
ELSE
    FOR nI := 1 TO aColumns:Length
        LOCAL aField  := aStruct[nI] AS ARRAY
        VAR oColumn := DbColumnInfo{aField[DBS_NAME], aField[DBS_TYPE]+":0", aField[DBS_LEN], aField[DBS_DEC]}
        aColumns[nI] := oColumn
    NEXT
ENDIF
LOCAL oTable AS DbDataTable
oTable := DbDataTable{}
FOREACH oDbCol AS DbColumnInfo IN aColumns
    oTable:Columns:Add(DbDataColumn{oDbCol})
NEXT
LOCAL oMIGet := NULL AS MethodInfo 
// DBFVFPSQL has a method GetData to get the array that stores all the field values
oMIGet := oRDD:GetType():GetMethod("GetData", BindingFlags.Instance+BindingFlags.IgnoreCase+BindingFlags.Public)
VAR nOld := oRDD:RecNo
oRDD:GoTop()
IF oMIGet != NULL
    VAR GetData := (SqlGetData) oMIGet:CreateDelegate(typeof(SqlGetData), oRDD) 
    DO WHILE ! oRDD:EoF
        VAR data := GetData()
        oTable:AddRow(data,oRDD:RecNo)
        oRDD:Skip(1)
    ENDDO
ELSE
    nFldCount := aColumns:Length
    DO WHILE ! oRDD:EoF
        LOCAL oData AS OBJECT[]
        oData := OBJECT[]{nFldCount}
        FOR nI := 1 TO nFldCount
            oData[nI] := oRDD:GetValue(nI)
        NEXT
        oTable:AddRow(oData,oRDD:RecNo)
        oRDD:Skip(1)
    ENDDO
ENDIF
oRDD:GoTo(nOld)
oTable:AcceptChanges()
RETURN oTable    

INTERNAL DELEGATE SqlGetData() AS OBJECT[]
