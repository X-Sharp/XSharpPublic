//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Data
USING System.Data.SqlClient
USING System.Data.Common
USING System.Reflection
USING System.Text
USING System.Runtime.CompilerServices
USING System.Runtime.InteropServices

/// <summary>This is the class that implements a Factory to access data through the Ado.Net Microsoft SQL Server classes.</summary>


CLASS XSharp.Data.OleDbFactory INHERIT XSharp.Data.AbstractSqlFactory

    /// <inheritdoc />

    PRIVATE quotes AS STRING

    OVERRIDE PROPERTY QuoteChar AS STRING GET quotes
    /// <inheritdoc />
    OVERRIDE PROPERTY Name      AS STRING GET "OleDbFactory"
    
    CONSTRUCTOR
        SUPER()
        oInstance := System.Data.OleDb.OleDbFactory.Instance
        VAR oCmdBuilder := oInstance:CreateCommandBuilder()
        quotes := oCmdBuilder:QuotePrefix+";"+oCmdBuilder:QuoteSuffix

    

    /// <inheritdoc />
    OVERRIDE METHOD GetName(oConn AS DbConnection) AS STRING
        RETURN "OleDb"

    /// <inheritdoc />
    OVERRIDE METHOD DriverConnect(hWindow AS IntPtr, uCompletion AS OBJECT, cConnectionString AS OBJECT) AS STRING
        LOCAL cTemp AS STRING
        cTemp := System.IO.Path.GetTempFileName()
        FErase(cTemp)
        cTemp := System.IO.Path.ChangeExtension(cTemp,".UDL")
        FClose(FCreate2(cTemp,FC_NORMAL))
        VAR startInfo := System.Diagnostics.ProcessStartInfo{}
        startInfo:FileName := cTemp
        VAR process := System.Diagnostics.Process.Start(startInfo)
        LOCAL hDialog := Win32.FindWindow("#32770",NULL) AS IntPtr
        IF hDialog != IntPtr.Zero
            IF hWindow == IntPtr.Zero
                hWindow := Win32.GetParentWindow()
            ENDIF
            Win32.SetParent(hDialog, hWindow)
        ENDIF
        process:WaitForExit()
        VAR lines := System.IO.File.ReadAllLines(cTemp)
        FErase(cTemp)
        FOREACH VAR line IN lines
            IF line:ToLower():StartsWith("provider=")
                RETURN line
            ENDIF
        NEXT
        RETURN ""
        
        

        
    /// <inheritdoc />
    OVERRIDE METHOD GetMetaDataColumnValues(oRow AS DataRow) AS OBJECT[]
        VAR result := OBJECT[]{19}                         // 
        result[01] := oRow["TABLE_CATALOG"]                // aStruct[1] := {"TABLE_CAT","C:0",128,0}
        result[02] := oRow["TABLE_SCHEMA"]                 // aStruct[2] := {"TABLE_SCHE","C:0",128,0}
        result[03] := oRow["TABLE_NAME"]                   // aStruct[3] := {"TABLE_NAME","C",128,0}
        result[04] := oRow["COLUMN_NAME"]                  // aStruct[4] := {"COLUMN_NAM","C",128,0}
        result[05] := oRow["DATA_TYPE"]                    // aStruct[5] := {"DATA_TYPE","I",4,0}
        result[06] := ""                                   // aStruct[6] := {"TYPE_NAME","C",128,0}
        result[07] := oRow["CHARACTER_MAXIMUM_LENGTH"]     // aStruct[7] := {"COLUMN_SIZ","I:0",4,0}
        result[08] := NULL                                 // aStruct[8] := {"BUFFER_LEN","I:0",4,0}
        result[09] := oRow["NUMERIC_SCALE"]                // aStruct[9] := {"DECIMAL_DI","I:0",4,0}
        result[10] := 0                                    // aStruct[10] := {"NUM_PREC_R","I:0",4,0}
        result[11] := oRow["IS_NULLABLE"]                  // aStruct[11] := {"NULLABLE","I",4,0}
        result[12] := oRow["DESCRIPTION"]                  // aStruct[12] := {"REMARKS","C:0",254,0}
        result[13] := oRow["COLUMN_DEFAULT"]               // aStruct[13] := {"COLUMN_DEF","M:0",4,0}
        result[14] := 0                                    // aStruct[14] := {"SQL_DATA_T","I",4,0}
        result[15] := 0                                    // aStruct[15] := {"SQL_DATETI","I:0",4,0}
        result[16] := oRow["CHARACTER_OCTET_LENGTH"]       // aStruct[16] := {"CHAR_OCTET","I:0",4,0}
        result[17] := oRow["ORDINAL_POSITION"]             // aStruct[17] := {"ORDINAL_PO","I",4,0}
        result[18] := oRow["IS_NULLABLE"]                  // aStruct[18] := {"IS_NULLABL","C:0",254,0}
        result[19] := 0                                    // aStruct[19] := {"SS_DATA_TY","I:0",4,0}
        IF result[7] == DBNull.Value  .OR. (result[7] IS LONG VAR lValue .AND. lValue == 0) .OR. result[7] == NULL
            result[7] := oRow["NUMERIC_PRECISION"]
        ENDIF
        IF result[11] IS STRING VAR sValue
            result[11] := IIF(sValue:ToUpper() == "YES", 1, 0)
        ELSEIF result[11] IS LOGIC
            result[11] := IIF((LOGIC) result[11], 1, 0)
        ENDIF
        IF result[18] IS LOGIC VAR logValue
            result[18] := IIF(logValue ,"YES", "NO")
        ENDIF  
        RETURN result       

    /// <inheritdoc />
    OVERRIDE METHOD GetMetaDataTableValues(oRow AS DataRow) AS OBJECT[]
        VAR result := OBJECT[]{5}
        result[1] := oRow["TABLE_CATALOG"]
        result[2] := oRow["TABLE_SCHEMA"]
        result[3] := oRow["TABLE_NAME"]
        result[4] := IIF(oRow:Table:Columns:IndexOf("TABLE_TYPE") > 0, oRow["TABLE_TYPE"],"VIEW")
        result[5] := oRow["DESCRIPTION"]
        RETURN result

END CLASS


