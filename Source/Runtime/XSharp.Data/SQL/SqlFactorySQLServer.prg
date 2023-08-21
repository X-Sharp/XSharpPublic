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
USING System.Runtime.InteropServices

/// <summary>This is the class that implements a Factory to access data through the Ado.Net Microsoft SQL Server classes.</summary>


CLASS XSharp.Data.SqlServerFactory INHERIT XSharp.Data.AbstractSqlFactory

    PRIVATE quotes AS STRING
    /// <inheritdoc />
    OVERRIDE PROPERTY QuoteChar AS STRING GET quotes
    /// <inheritdoc />
    OVERRIDE PROPERTY Name      AS STRING GET "SqlServerFactory"

    /// <inheritdoc />
    OVERRIDE PROPERTY ParameterPrefix AS CHAR GET '@'
    /// <inheritdoc />
    OVERRIDE PROPERTY ParameterNameInQuery AS LOGIC GET TRUE


    CONSTRUCTOR
        SUPER()
        oInstance := System.Data.SqlClient.SqlClientFactory.Instance
        VAR oCmdBuilder := oInstance:CreateCommandBuilder()
        quotes := oCmdBuilder:QuotePrefix+";"+oCmdBuilder:QuoteSuffix


    /// <inheritdoc />
    OVERRIDE METHOD GetMetaDataColumnValues(oRow AS DataRow) AS OBJECT[]

        VAR result := OBJECT[]{19}                        // 
        result[01] := oRow["TABLE_CATALOG"]               // aStruct[1] := {"TABLE_CAT","C:0",128,0}
        result[02] := oRow["TABLE_SCHEMA"]                // aStruct[2] := {"TABLE_SCHE","C:0",128,0}
        result[03] := oRow["TABLE_NAME"]                  // aStruct[3] := {"TABLE_NAME","C",128,0}
        result[04] := oRow["COLUMN_NAME"]                 // aStruct[4] := {"COLUMN_NAM","C",128,0}
        result[05] := NULL                                // aStruct[5] := {"DATA_TYPE","I",4,0}
        result[06] := oRow["DATA_TYPE"]                   // aStruct[6] := {"TYPE_NAME","C",128,0}
        result[07] := oRow["CHARACTER_MAXIMUM_LENGTH"]    // aStruct[7] := {"COLUMN_SIZ","I:0",4,0}
        result[08] := NULL                                // aStruct[8] := {"BUFFER_LEN","I:0",4,0}
        result[09] := oRow["NUMERIC_SCALE"]               // aStruct[9] := {"DECIMAL_DI","I:0",4,0}
        result[10] := oRow["NUMERIC_PRECISION_RADIX"]     // aStruct[10] := {"NUM_PREC_R","I:0",4,0}
        result[11] := oRow["IS_NULLABLE"]                 // aStruct[11] := {"NULLABLE","I",4,0}
        result[12] := NULL                                // aStruct[12] := {"REMARKS","C:0",254,0}
        result[13] := oRow["COLUMN_DEFAULT"]              // aStruct[13] := {"COLUMN_DEF","M:0",4,0}
        result[14] := 0                                   // aStruct[14] := {"SQL_DATA_T","I",4,0}
        result[15] := 0                                   // aStruct[15] := {"SQL_DATETI","I:0",4,0}
        result[16] := oRow["CHARACTER_OCTET_LENGTH"]      // aStruct[16] := {"CHAR_OCTET","I:0",4,0}
        result[17] := oRow["ORDINAL_POSITION"]            // aStruct[17] := {"ORDINAL_PO","I",4,0}
        result[18] := oRow["IS_NULLABLE"]                 // aStruct[18] := {"IS_NULLABL","C:0",254,0}
        result[19] := 0                                   // aStruct[19] := {"SS_DATA_TY","I:0",4,0}
        IF result[7] == DBNull.Value  .OR. (result[7] IS LONG VAR lValue .AND. lValue == 0) .OR. result[7] == NULL
            result[7] := oRow["NUMERIC_PRECISION"]
        ENDIF
        IF result[11] IS STRING VAR sValue
            result[11] := IIF(sValue:ToUpper() == "YES", 1, 0)
        ENDIF
        RETURN result       


    /// <inheritdoc />
    OVERRIDE METHOD GetMetaDataTableValues(oRow AS DataRow) AS OBJECT[]
        VAR result := OBJECT[]{5}
        result[1] := oRow["TABLE_CATALOG"]
        result[2] := oRow["TABLE_SCHEMA"]
        result[3] := oRow["TABLE_NAME"]
        result[4] := result[4] := IIF(oRow:Table:Columns:IndexOf("TABLE_TYPE") > 0, oRow["TABLE_TYPE"],"VIEW")
        result[5] := ""
        IF (STRING) result[4]  == "BASE TABLE"
            result[4] := "TABLE"
        ENDIF
        RETURN result


/*
    From the Docs online
    TABLE_CATALOG 	String 	Catalog of the table.
    TABLE_SCHEMA 	String 	Schema that contains the table.
    TABLE_NAME 	String 	Table name.
    COLUMN_NAME 	String 	Column name.
    ORDINAL_POSITION 	Int32 	Column identification number.
    COLUMN_DEFAULT 	String 	Default value of the column
    IS_NULLABLE 	String 	Nullability of the column. If this column allows NULL, this column returns YES. Otherwise, NO is returned.
    DATA_TYPE 	String 	System-supplied data type.
    CHARACTER_MAXIMUM_LENGTH 	Int32 	Maximum length, in characters, for binary data, character data, or text and image data. Otherwise, NULL is returned.
    CHARACTER_OCTET_LENGTH 	Int32 	Maximum length, in bytes, for binary data, character data, or text and image data. Otherwise, NULL is returned.
    NUMERIC_PRECISION 	Unsigned Byte 	Precision of approximate numeric data, exact numeric data, integer data, or monetary data. Otherwise, NULL is returned.
    NUMERIC_PRECISION_RADIX 	Int16 	Precision radix of approximate numeric data, exact numeric data, integer data, or monetary data. Otherwise, NULL is returned.
    NUMERIC_SCALE 	Int32 	Scale of approximate numeric data, exact numeric data, integer data, or monetary data. Otherwise, NULL is returned.
    DATETIME_PRECISION 	Int16 	Subtype code for datetime and SQL-92 interval data types. For other data types, NULL is returned.
    CHARACTER_SET_CATALOG 	String 	Returns master, indicating the database in which the character set is located, if the column is character data or text data type. Otherwise, NULL is returned.
    CHARACTER_SET_SCHEMA 	String 	Always returns NULL.
    CHARACTER_SET_NAME 	String 	Returns the unique name for the character set if this column is character data or text data type. Otherwise, NULL is returned.
    COLLATION_CATALOG 	String 	Returns master, indicating the database in which the collation is defined, if the column is character data or text data type. Otherwise, this column is NULL.
    IS_FILESTREAM 	String 	YES if the column has FILESTREAM attribute.
                    NO if the column does not have FILESTREAM attribute.
    IS_SPARSE 	String 	YES if the column is a sparse column.
                    NO if the column is not a sparse column.
    IS_COLUMN_SET 	String 	YES if the column is a column set column.
                    NO if the column is not a column set column.
*/

    /// <inheritdoc />
    OVERRIDE METHOD GetName(oConn AS DbConnection) AS STRING
        RETURN "SQL"

    /// <inheritdoc />
    OVERRIDE METHOD DriverConnect(hWindow AS IntPtr, uCompletion AS OBJECT, cConnectionString AS OBJECT) AS STRING
        LOCAL oODBC AS OdbcFactory
        LOCAL cResult AS STRING
        oODBC := OdbcFactory{}
        cConnectionString := "Driver=SQL Server"
        cResult := oODBC:DriverConnect(hWindow, Win32.SQL_DRIVER_PROMPT, cConnectionString)
        IF String.IsNullOrEmpty(cResult)
            RETURN cResult
        ENDIF
        VAR oBuilder := oODBC:CreateConnectionStringBuilder()
        oBuilder:ConnectionString := cResult
        oBuilder:Remove("Driver")
        RETURN oBuilder:ToString()


END CLASS

