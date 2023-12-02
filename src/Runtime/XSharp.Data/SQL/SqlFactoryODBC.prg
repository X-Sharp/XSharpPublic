//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Data
USING System.Data.Odbc
USING System.Data.Common
USING System.Reflection
USING System.Text
USING System.Runtime.InteropServices


   /// <summary>This is the class that implements a Factory to access data through the Ado.Net ODBC Classes.</summary>


CLASS XSharp.Data.OdbcFactory INHERIT XSharp.Data.AbstractSqlFactory

    /// <inheritdoc />
    PRIVATE quotes AS STRING
    OVERRIDE PROPERTY QuoteChar AS STRING GET quotes
    /// <inheritdoc />
    OVERRIDE PROPERTY Name      AS STRING GET "OdbcFactory"

    CONSTRUCTOR
        SUPER()
        oInstance := System.Data.Odbc.OdbcFactory.Instance
        VAR oCmdBuilder := oInstance:CreateCommandBuilder()
        quotes := oCmdBuilder:QuotePrefix+";"+oCmdBuilder:QuoteSuffix

    /// <inheritdoc />
    OVERRIDE METHOD GetName(oConn AS DbConnection) AS STRING
        RETURN "ODBC"


    /// <inheritdoc />
    OVERRIDE METHOD EnhanceException(oEx AS System.Exception)  AS System.Exception
        RETURN oEx

    /// <inheritdoc />
    OVERRIDE METHOD HandleSpecialValue(oValue AS OBJECT, oFS AS OBJECT, lDateTimeAsDate AS LOGIC) AS OBJECT
        RETURN oValue

    /// <inheritdoc />
    OVERRIDE METHOD TranslateStatement(cStatement AS STRING) AS STRING
        RETURN cStatement


    /// <inheritdoc />
    OVERRIDE METHOD AfterOpen(oDataReader AS DbDataReader) AS DbDataReader
        RETURN oDataReader

    /// <inheritdoc />
    OVERRIDE METHOD DriverConnect(hWindow AS IntPtr, uCompletion AS OBJECT, cConnectionString AS OBJECT) AS STRING
        LOCAL nRetCode      AS INT
        LOCAL cConnect      AS STRING
        LOCAL sbResult      AS StringBuilder
        LOCAL cResult       AS STRING
        LOCAL nCompletion   AS WORD
        LOCAL nSize		  	AS SHORTINT
        LOCAL hEnv          AS IntPtr
        LOCAL hDBc          AS IntPtr
        LOCAL nBytes        AS SHORT
        TRY
            IF uCompletion != NULL
                nCompletion := Convert.ToUInt16(uCompletion)
            ELSE
                nCompletion := XWin32.SQL_DRIVER_PROMPT
            ENDIF
        CATCH
            nCompletion := XWin32.SQL_DRIVER_PROMPT
        end try
        sbResult := StringBuilder{XWin32.SQL_MAX_MESSAGE_LENGTH}
        IF hWindow == IntPtr.Zero
            hWindow := XWin32.GetParentWindow()
        ENDIF

        IF cConnectionString IS STRING
            cConnect  := (STRING) cConnectionString
            nSize     := (short) (XSharp.Core.Functions.SLen( cConnect ) + 1 )
        ELSE
            cConnect  := ""
            nSize     := 0
        ENDIF

        nRetCode := XWin32.SQLAllocEnv( out hEnv )
        nRetCode := XWin32.SQLAllocConnect( hEnv, OUT hDBc )
        nRetCode := XWin32.SQLDriverConnect( 	hDBc,                    ;
                                        hWindow,                    ;
                                        cConnect,               ;
                                        nSize    , ;
                                        sbResult,              ;
                                        XWin32.SQL_MAX_MESSAGE_LENGTH,  ;
                                        OUT nBytes,                 ;
                                        nCompletion  )
        IF nRetCode == XWin32.SQL_SUCCESS .OR. nRetCode == XWin32.SQL_SUCCESS_WITH_INFO .AND. nBytes > 0
            cResult  := sbResult:ToString()
        ELSE
            cResult := ""
        ENDIF

        nRetCode := XWin32.SQLDisconnect( hDBc )
        nRetCode := XWin32.SQLFreeConnect(hDBc)
        nRetCode := XWin32.SQLFreeEnv( hEnv )
        RETURN cResult

      /// <inheritdoc />
    OVERRIDE METHOD GetMetaDataColumnValues(oRow AS DataRow) AS OBJECT[]

        var result := object[]{19}                      //
        result[01] := oRow["TABLE_CAT"]                 // aStruct[1] := {"TABLE_CAT","C:0",128,0}
        result[02] := oRow["TABLE_SCHEM"]               // aStruct[2] := {"TABLE_SCHE","C:0",128,0}
        result[03] := oRow["TABLE_NAME"]                // aStruct[3] := {"TABLE_NAME","C",128,0}
        result[04] := oRow["COLUMN_NAME"]               // aStruct[4] := {"COLUMN_NAM","C",128,0}
        result[05] := oRow["DATA_TYPE"]                 // aStruct[5] := {"DATA_TYPE","I",4,0}
        result[06] := oRow["TYPE_NAME"]                 // aStruct[6] := {"TYPE_NAME","C",128,0}
        result[07] := oRow["COLUMN_SIZE"]               // aStruct[7] := {"COLUMN_SIZ","I:0",4,0}
        result[08] := oRow["BUFFER_LENGTH"]             // aStruct[8] := {"BUFFER_LEN","I:0",4,0}
        result[09] := oRow["DECIMAL_DIGITS"]            // aStruct[9] := {"DECIMAL_DI","I:0",4,0}
        result[10] := oRow["NUM_PREC_RADIX"]            // aStruct[10] := {"NUM_PREC_R","I:0",4,0}
        result[11] := oRow["NULLABLE"]                  // aStruct[11] := {"NULLABLE","I",4,0}
        result[12] := oRow["REMARKS"]                   // aStruct[12] := {"REMARKS","C:0",254,0}
        result[13] := oRow["COLUMN_DEF"]                // aStruct[13] := {"COLUMN_DEF","M:0",4,0}
        result[14] := oRow["SQL_DATA_TYPE"]             // aStruct[14] := {"SQL_DATA_T","I",4,0}
        result[15] := oRow["SQL_DATETIME_SUB"]          // aStruct[15] := {"SQL_DATETI","I:0",4,0}
        result[16] := oRow["CHAR_OCTET_LENGTH"]         // aStruct[16] := {"CHAR_OCTET","I:0",4,0}
        result[17] := oRow["ORDINAL_POSITION"]          // aStruct[17] := {"ORDINAL_PO","I",4,0}
        result[18] := oRow["IS_NULLABLE"]               // aStruct[18] := {"IS_NULLABL","C:0",254,0}
        result[19] := oRow["SS_DATA_TYPE"]              // aStruct[19] := {"SS_DATA_TY","I:0",4,0}
        RETURN result

    /// <inheritdoc />
    OVERRIDE METHOD GetMetaDataTableValues(oRow AS DataRow) AS OBJECT[]
        VAR result := OBJECT[]{5}
        result[1] := oRow["TABLE_CAT"]
        result[2] := oRow["TABLE_SCHEM"]
        result[3] := oRow["TABLE_NAME"]
        result[4] := result[4] := IIF(oRow:Table:Columns:IndexOf("TABLE_TYPE") > 0, oRow["TABLE_TYPE"],"VIEW")
        result[5] := oRow["REMARKS"]
        RETURN result


END CLASS

