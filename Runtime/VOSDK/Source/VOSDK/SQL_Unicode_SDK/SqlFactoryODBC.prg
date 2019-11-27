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

PROCEDURE InitDefaultProvider INIT3
    SetSqlFactory(OdbcFactory{})



CLASS OdbcFactory INHERIT AbstractSqlFactory

    PROPERTY QuoteChar AS STRING GET ""

    CONSTRUCTOR
        SUPER()
        oInstance := System.Data.Odbc.OdbcFactory.Instance

    METHOD GetName(oConn AS DbConnection) AS STRING
        RETURN "ODBC"


    METHOD EnhanceException(oEx AS SYstem.Exception)  AS SYstem.Exception
        RETURN oEx

    METHOD HandleSpecialValue(oValue AS OBJECT, oFS AS FieldSpec, lDateTimeAsDate AS LOGIC) AS USUAL
        RETURN oValue

    METHOD TranslateStatement(cStatement AS STRING) AS STRING
        RETURN cStatement


    METHOD AfterOpen(oDataReader AS DbDataReader) AS DbDataReader
        RETURN oDataReader

    METHOD DriverConnect(hWindow AS USUAL, uCompletion AS USUAL, cConnectionString AS USUAL) AS STRING
        LOCAL nRetCode      AS INT
        LOCAL cConnect      AS STRING
        LOCAL sbResult      AS StringBuilder
        LOCAL cResult       AS STRING
        LOCAL nCompletion   AS WORD
        LOCAL nSize		  	AS SHORTINT
        LOCAL hWnd          AS IntPtr
        LOCAL hEnv          AS IntPtr
        LOCAL hDBc          AS IntPtr
        LOCAL nBytes        AS SHORT
        IF IsNumeric( uCompletion )
            nCompletion := uCompletion
        ELSE
            nCompletion := SQL_DRIVER_PROMPT
        ENDIF
        sbResult := StringBuilder{SQL_MAX_MESSAGE_LENGTH}
        IF hWnd ==  IntPtr.Zero
            hWnd := GetActiveWindow()
        ENDIF
        IF hWnd = NULL_PTR
            hWnd := GetDeskTopWindow()
        ENDIF
        
        IF IsString( cConnectionString )
            cConnect  := cConnectionString
            nSize     := (SHORT) SLen( cConnect ) + 1 
        ELSE
            cConnect  := ""
            nSize     := 0
        ENDIF
        
        nRetCode := OdbcFactory.SQLAllocEnv( OUT hEnv ) 
        nRetCode := OdbcFactory.SQLAllocConnect( hEnv, OUT hDBc )
        nRetCode := OdbcFactory.SQLDriverConnect( 	hDBc,                    ;
                                        hWnd,                    ;
                                        cConnect,               ;
                                        nSize    , ;
                                        sbResult,              ;
                                        SQL_MAX_MESSAGE_LENGTH,  ;
                                        OUT nBytes,                 ;
                                        nCompletion  )
        IF nRetCode == SQL_SUCCESS .OR. nRetCode == SQL_SUCCESS_WITH_INFO .AND. nBytes > 0
            cResult  := sbResult:ToString()
        ELSE
            cResult := ""
        ENDIF
        
        nRetCode := OdbcFactory.SQLDisconnect( hDBc )
        nRetCode := OdbcFactory.SQLFreeConnect(hDBc)
        nRetCode := OdbcFactory.SQLFreeEnv( hEnv )
        RETURN cResult
 
    [DllImport("USER32.dll", CharSet := CharSet.Ansi)];
    STATIC METHOD GetActiveWindow() AS IntPtr PASCAL
        
    [DllImport("USER32.dll", CharSet := CharSet.Ansi)];
    STATIC METHOD GetDesktopWindow() AS PTR PASCAL

    [DllImport("ODBC32.dll", EntryPoint := "SQLAllocEnv")];
    STATIC METHOD SQLAllocEnv(phenv OUT IntPtr) AS SHORTINT PASCAL

    [DllImport("ODBC32.dll")];
    STATIC METHOD SQLAllocConnect(henv AS IntPtr, phdbc OUT IntPtr) AS SHORTINT PASCAL

    [DllImport("ODBC32.dll", CharSet := CharSet.Unicode)];
    STATIC METHOD SQLDriverConnect(hdbc AS IntPtr, hwnd AS IntPtr,szConnStrIn AS STRING,cbConnStrIn AS SHORT,;
	    szConnStrOut AS StringBuilder, cbConnStrOutMax AS SHORT,pcbConnStrOut OUT SHORT ,;
	    fDriverCompletion AS WORD) AS SHORTINT PASCAL
        
    [DllImport("ODBC32.dll")];
    STATIC METHOD SQLDisconnect(hdbc AS IntPtr) AS SHORTINT PASCAL

    [DllImport("ODBC32.dll")];
    STATIC METHOD SQLFreeConnect(hdbc AS IntPtr) AS SHORTINT PASCAL

    [DllImport("ODBC32.dll")];
    STATIC METHOD SQLFreeEnv(henv AS IntPtr) AS SHORTINT PASCAL


END CLASS

