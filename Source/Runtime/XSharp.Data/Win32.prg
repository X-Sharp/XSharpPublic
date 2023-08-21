// Win32.prg
// Created by    : robert
// Creation Date : 1/9/2020 11:01:41 AM
// Created for   : 
// WorkStation   : ARTEMIS


USING System
USING System.Collections.Generic
USING System.Text
USING System.Runtime.InteropServices

BEGIN NAMESPACE XSharp.Data

    INTERNAL STATIC CLASS Win32
    
        INTERNAL CONST SQL_DRIVER_NOPROMPT	:=	0 AS WORD
        INTERNAL CONST SQL_DRIVER_COMPLETE	:=	1 AS WORD
        INTERNAL CONST SQL_DRIVER_PROMPT	:=	2 AS WORD
        INTERNAL CONST SQL_DRIVER_COMPLETE_REQUIRED	:=	3 AS WORD
        INTERNAL CONST SQL_MAX_MESSAGE_LENGTH	:=	512 AS LONG
        INTERNAL CONST SQL_SUCCESS	:=	0 AS LONG
        INTERNAL CONST SQL_SUCCESS_WITH_INFO	:=	1 AS LONG

        [DllImport("USER32.dll", CharSet := CharSet.Ansi)];
        INTERNAL STATIC METHOD GetActiveWindow() AS IntPtr PASCAL
        
        [DllImport("USER32.dll", CharSet := CharSet.Ansi)];
        INTERNAL STATIC METHOD GetDesktopWindow() AS IntPtr PASCAL

        INTERNAL STATIC METHOD GetParentWindow() AS IntPtr
            LOCAL hResult AS IntPtr
            hResult := GetActiveWindow()
            IF hResult == IntPtr.Zero
                hResult := GetDesktopWindow()
            ENDIF
            RETURN hResult


        [DllImport("ODBC32.dll", EntryPoint := "SQLAllocEnv")];
        INTERNAL STATIC METHOD SQLAllocEnv(phenv OUT IntPtr) AS SHORTINT PASCAL

        [DllImport("ODBC32.dll")];
        INTERNAL STATIC METHOD SQLAllocConnect(henv AS IntPtr, phdbc OUT IntPtr) AS SHORTINT PASCAL

        [DllImport("ODBC32.dll", CharSet := CharSet.Unicode)];
        INTERNAL STATIC METHOD SQLDriverConnect(hdbc AS IntPtr, hwnd AS IntPtr, ;
            szConnStrIn AS STRING,cbConnStrIn AS SHORT, szConnStrOut AS StringBuilder, ;
            cbConnStrOutMax AS SHORT,pcbConnStrOut OUT SHORT ,;
            fDriverCompletion AS WORD) AS SHORTINT PASCAL
        
        [DllImport("ODBC32.dll")];
        INTERNAL STATIC METHOD SQLDisconnect(hdbc AS IntPtr) AS SHORTINT PASCAL

        [DllImport("ODBC32.dll")];
        INTERNAL STATIC METHOD SQLFreeConnect(hdbc AS IntPtr) AS SHORTINT PASCAL

        [DllImport("ODBC32.dll")];
        INTERNAL STATIC METHOD SQLFreeEnv(henv AS IntPtr) AS SHORTINT PASCAL


        [DllImport("user32.dll", CharSet := CharSet.Auto, SetLastError := TRUE)] ;
		INTERNAL STATIC METHOD FindWindow(lpClassName AS STRING , lpWindowName AS STRING ) AS IntPtr


        [DllImport("user32.dll", CharSet := CharSet.Auto, SetLastError := TRUE)] ;
        INTERNAL STATIC METHOD SetParent( hWndChild AS IntPtr, hWndNewParent AS IntPtr) AS IntPtr 



    END CLASS
END NAMESPACE // XSharp.Data
