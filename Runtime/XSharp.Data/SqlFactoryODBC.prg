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
    PROPERTY QuoteChar AS STRING GET ""

    CONSTRUCTOR
        SUPER()
        oInstance := System.Data.Odbc.OdbcFactory.Instance

    /// <inheritdoc />
    METHOD GetName(oConn AS DbConnection) AS STRING
        RETURN "ODBC"


    /// <inheritdoc />
    METHOD EnhanceException(oEx AS System.Exception)  AS System.Exception
        RETURN oEx

    /// <inheritdoc />
    METHOD HandleSpecialValue(oValue AS OBJECT, oFS AS OBJECT, lDateTimeAsDate AS LOGIC) AS OBJECT
        RETURN oValue

    /// <inheritdoc />
    METHOD TranslateStatement(cStatement AS STRING) AS STRING
        RETURN cStatement


    /// <inheritdoc />
    METHOD AfterOpen(oDataReader AS DbDataReader) AS DbDataReader
        RETURN oDataReader

    /// <inheritdoc />
    METHOD DriverConnect(hWindow AS object, uCompletion AS object, cConnectionString AS object) AS STRING
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
        TRY
            IF uCompletion != NULL
                nCompletion := Convert.ToUInt16(uCompletion)
            ELSE
                nCompletion := Win32.SQL_DRIVER_PROMPT
            ENDIF
        CATCH
            nCompletion := Win32.SQL_DRIVER_PROMPT
        END TRY
        sbResult := StringBuilder{Win32.SQL_MAX_MESSAGE_LENGTH}
        IF hWindow IS IntPtr
            hWnd := (IntPtr) hWindow
        ELSEIF hWindow IS System.Int32
            hWnd := IntPtr{(System.Int32) hWindow}
        ELSEIF hWindow IS System.Int64
            hWnd := IntPtr{(System.Int64) hWindow}
        ELSE
            hWnd := Win32.GetActiveWindow()
        ENDIF
        IF hWnd == IntPtr.Zero
            hWnd := Win32.GetDesktopWindow()
        ENDIF
        
        IF cConnectionString IS STRING
            cConnect  := (STRING) cConnectionString
            nSize     := (SHORT) (SLen( cConnect ) + 1 )
        ELSE
            cConnect  := ""
            nSize     := 0
        ENDIF
        
        nRetCode := Win32.SQLAllocEnv( OUT hEnv ) 
        nRetCode := Win32.SQLAllocConnect( hEnv, OUT hDBc )
        nRetCode := Win32.SQLDriverConnect( 	hDBc,                    ;
                                        hWnd,                    ;
                                        cConnect,               ;
                                        nSize    , ;
                                        sbResult,              ;
                                        Win32.SQL_MAX_MESSAGE_LENGTH,  ;
                                        OUT nBytes,                 ;
                                        nCompletion  )
        IF nRetCode == Win32.SQL_SUCCESS .OR. nRetCode == Win32.SQL_SUCCESS_WITH_INFO .AND. nBytes > 0
            cResult  := sbResult:ToString()
        ELSE
            cResult := ""
        ENDIF
        
        nRetCode := Win32.SQLDisconnect( hDBc )
        nRetCode := Win32.SQLFreeConnect(hDBc)
        nRetCode := Win32.SQLFreeEnv( hEnv )
        RETURN cResult

 
END CLASS

