//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections.Generic
using System.Runtime.InteropServices


#define PM_NOREMOVE   0x0000
#define MB_TOPMOST              0x00040000
#define MB_ICONEXCLAMATION      0x00000030

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/_accept/*" />
FUNCTION _accept() AS STRING STRICT
    RETURN _accept( "" )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/_accept/*" />
FUNCTION _accept( uValuePrompt AS STRING ) AS STRING
    LOCAL retval AS STRING

    Console.WriteLine()
    Console.Write( uValuePrompt )

    TRY
        retval := Console.ReadLine()
    CATCH AS System.InvalidOperationException
        retval := ""
    END TRY

    RETURN IIF( retval == NULL, "", retval )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/cls/*" />
FUNCTION cls() AS VOID STRICT
    Console.Clear()
    RETURN

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/col/*" />
FUNCTION Col() AS SHORT STRICT
    RETURN (SHORT) Console.CursorLeft


//INTERNAL FUNCTION PrintFileOpen(lAdditive as LOGIC) AS VOID
//    PrintFileClose()
//    var cPrintFile := RuntimeState.PrintFile
//    IF ! String.IsNullOrEmpty(cPrintFile)
//        IF File(cPrintFile)
//            cPrintFile := FPathName()
//        ENDIF
//        IF File(cPrintFile) .and. lAdditive
//            PrintFileHandle := FOpen2(cPrintFile,FO_WRITE)
//            FSeek(PrintFileHandle, FS_END, 0)
//        ELSE
//           PrintFileHandle := FCreate(cPrintFile)
//        ENDIF
//    ENDIF
//    RETURN
//
//INTERNAL FUNCTION PrintFileClose() AS VOID
//    IF PrintFileHandle != IntPtr.Zero
//        FClose(PrintFileHandle)
//        PrintFileHandle := IntPtr.Zero
//    ENDIF
//    RETURN


INTERNAL FUNCTION AltFileOpen(lAdditive as LOGIC) AS VOID
    AltFileClose()
    var cAltFile := RuntimeState.AltFile
    IF ! String.IsNullOrEmpty(cAltFile)
        IF File(cAltFile)
            cAltFile := FPathName()
        ENDIF
        IF File(cAltFile) .and. lAdditive
            ConsoleHelpers.AltFileHandle := FOpen2(cAltFile,FO_WRITE)
            FSeek(ConsoleHelpers.AltFileHandle, FS_END, 0)
        ELSE
            ConsoleHelpers.AltFileHandle := FCreate(cAltFile)
        ENDIF
    ENDIF
RETURN

INTERNAL FUNCTION AltFileClose() AS VOID
    IF ConsoleHelpers.AltFileHandle != IntPtr.Zero
        FClose(ConsoleHelpers.AltFileHandle)
        ConsoleHelpers.AltFileHandle := IntPtr.Zero
    ENDIF
RETURN

INTERNAL FUNCTION ConsoleWriteLine() AS VOID
    IF RuntimeState.Console
        Console.WriteLine()
    ENDIF
RETURN

INTERNAL FUNCTION ConsoleWrite(cText as STRING) AS VOID
    IF RuntimeState.Console
        Console.Write(cText)
    ENDIF
RETURN

//INTERNAL FUNCTION PrintWrite(cText as STRING) AS VOID
//    IF RuntimeState.Printer .and. PrintFileHandle != IntPtr.Zero
//        FWrite(PrintFileHandle, cText, cText:Length)
//    ENDIF
//    RETURN


//INTERNAL FUNCTION PrintWriteLine() AS VOID
//    IF RuntimeState.Printer .and. PrintFileHandle != IntPtr.Zero
//        FWriteLine(PrintFileHandle,"",0)
//    ENDIF
//    RETURN

INTERNAL FUNCTION AltWrite(cText as STRING) AS VOID
    IF RuntimeState.Alternate .and. ConsoleHelpers.AltFileHandle != IntPtr.Zero
        FWrite(ConsoleHelpers.AltFileHandle, cText, cText:Length)
    ENDIF
RETURN


INTERNAL FUNCTION AltWriteLine() AS VOID
    IF RuntimeState.Alternate .and. ConsoleHelpers.AltFileHandle != IntPtr.Zero
        FWriteLine(ConsoleHelpers.AltFileHandle,"",0)
    ENDIF
RETURN

INTERNAL FUNCTION QWriteLine() AS VOID
    ConsoleWriteLine()
AltWriteLine()
//PrintWriteLine()

INTERNAL FUNCTION QWrite(cText as STRING) AS VOID
    IF cText != NULL
        ConsoleWrite(cText)
        AltWrite(cText)
        //PrintWrite(cText)
    ENDIF
RETURN

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/qout/*" />
FUNCTION QOut() AS VOID STRICT
    QWriteLine()
    RETURN

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/qout/*" />
FUNCTION QOut( uValueList AS USUAL ) AS VOID
    QWriteLine()
    QQOut( uValueList )
    RETURN

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/qout/*" />
FUNCTION QOut( uValueList PARAMS USUAL[] ) AS VOID
    QWriteLine()
    QQOut( uValueList )
    RETURN

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/qqout/*" />
FUNCTION QQOut( uValueList AS USUAL ) AS VOID
    QWrite(AsString( uValueList ) )
    RETURN

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/qqout/*" />
FUNCTION QQOut( uValueList PARAMS  USUAL[] ) AS VOID
    LOCAL count := uValueList:Length AS INT
    LOCAL x                 AS INT
    LOCAL lAddSpace         AS LOGIC
    lAddSpace := RuntimeState.GetValue<LOGIC>(Set.Space)
    FOR x := 1 UPTO count
        QWrite( AsString(uValueList[x]) )
        IF x < count .and. lAddSpace
            QWrite( " " )
        ENDIF
    NEXT
    RETURN

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/row/*" />
FUNCTION Row() AS SHORT
    RETURN (SHORT) Console.CursorTop

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setpos/*" />
FUNCTION SetPos( iRow AS INT, iCol AS INT ) AS VOID
    Console.SetCursorPosition( iCol, iRow )
    RETURN


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/_wait/*" />
FUNCTION _wait() AS STRING STRICT
    RETURN _wait( __CavoStr(VOErrors.TMSG_PRESSANYKEY))

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/_wait/*" />
FUNCTION _wait( uValuePrompt AS STRING ) AS STRING
    LOCAL info AS ConsoleKeyInfo
    LOCAL retval AS STRING
    Console.WriteLine()
    Console.Write( uValuePrompt )
    TRY
        info   := Console.ReadKey()
        retval := info:KeyChar:ToString()
    CATCH AS System.InvalidOperationException
        UnSafeNativeMethods.MessageBox(IntPtr.Zero,  uValuePrompt + chr(10) + chr(10) + "Wait", "Wait", MB_ICONEXCLAMATION| MB_TOPMOST )
        retval := ""
    END TRY

    RETURN retval

/// <exclude/>

FUNCTION DoEvents() AS VOID
    UnSafeNativeMethods.DoEvents()


/// <summary>Dump the contents of an array to the terminal window</summary>
/// <param name="aTest">Array to dump</param>
/// <param name="cPrefix">Name to show before the array brackets. Defaults to 'a'</param>
/// <returns>Nothing</returns>
/// <remarks>This dumps the information to the terminal window.
/// This will only work if the main application is a console application.</remarks>
FUNCTION ShowArray  (aTest as array, cPrefix := "" as STRING) AS VOID
    LOCAL i         AS DWORD
    LOCAL n         AS DWORD
    LOCAL x         AS USUAL
    LOCAL cOut      AS STRING
    LOCAL cOutTemp := "" AS STRING
    IF aTest:GetType():FullName:ToLower() == "xsharp.__foxarray"
        _CallClipFunc(#ShowFoxArray, aTest, cPrefix)
        RETURN
    ENDIF
    IF cPrefix:Length == 0
        cPrefix := "a"
    ENDIF

    n := ALen(aTest)

    FOR i := 1 TO n
        cOut := cPrefix + "[" + NTrim(i) + "]"
        x    := aTest[i]

        IF x:IsArray
            cOutTemp := cOut
        ENDIF

        cOut += " = "
        cOut += AsString(x)
        cOut += " ("
        cOut += ValType(x)
        cOut += ")"
        QOut(cOut)

        IF x:IsArray
            ShowArray(x, cOutTemp)
        ENDIF

    NEXT
    RETURN



/// <summary>Dump the contents of an object to the terminal window</summary>
/// <param name="oObject">Object to dump</param>
/// <param name="cPrefix">Name to show before the field names. Defaults to 'o'</param>
/// <returns>Nothing</returns>
/// <remarks>This dumps the information to the terminal window.
/// This will only work if the main application is a console application.</remarks>

FUNCTION ShowObject(oObject as OBJECT,cPrefix := "" as STRING) AS VOID
    LOCAL aNames := IvarList(oObject) AS ARRAY
    IF cPrefix:Length == 0
        cPrefix := "o"
    ENDIF
    var nLen := 10
    FOREACH cName AS STRING in aNames
        nLen := Max(nLen, cName:Length)
    NEXT
    FOREACH cName AS STRING in aNames
        ? cPrefix+":"+PadR(cName,nLen), IVarGet(oObject, cName)
    NEXT
    ?
    RETURN

/// <summary>Dump the currently defined privates to the terminal window</summary>
/// <param name="lCurrentOnly">Only dump the privates from the current level on the evaluation stack.</param>
/// <returns>Nothing</returns>
/// <remarks>This dumps the information to the terminal window.
/// This will only work if the main application is a console application.</remarks>

FUNCTION ShowPrivates(lCurrentOnly := FALSE AS LOGIC) AS VOID
    VAR cName := _PrivateFirst(lCurrentOnly)
    var aNames := List<string>{}
    var nLen   := 10
    do while ! String.IsNullOrEmpty(cName)
        aNames:Add(cName)
        nLen  := Math.Max(nLen, cName:Length)
        cName := _PrivateNext()
    enddo
    FOREACH var cElement in aNames
        ? PadR(cElement,nLen), MemVarGet(cElement)
    NEXT
    RETURN

/// <summary>Dump the currently defined publics to the terminal window</summary>
/// <returns>Nothing</returns>
/// <remarks>This dumps the information to the terminal window.
/// This will only work if the main application is a console application.</remarks>

FUNCTION ShowPublics() AS VOID
    VAR cName := _PublicFirst()
    var aNames := List<string>{}
    var nLen   := 10
    do while ! String.IsNullOrEmpty(cName)
        aNames:Add(cName)
        nLen  := Math.Max(nLen, cName:Length)
        cName := _PublicNext()
    enddo
    FOREACH var cElement in aNames
        ? PadR(cElement,nLen), MemVarGet(cElement)
    NEXT
    RETURN


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/pause/*" />
FUNCTION Pause() AS DWORD
    RETURN (DWORD) UnSafeNativeMethods.MessageBox(IntPtr.Zero, "Pause","Waiting",0)


FUNCTION GetColor() AS STRING
    RETURN ConsoleHelpers.ColNum2String(Console.ForegroundColor)+"/"+ConsoleHelpers.ColNum2String(Console.BackgroundColor)


FUNCTION SetColor(cNewColor as STRING) AS STRING
    LOCAL nFore, nBack		 AS BYTE
    VAR cOldCol  := GetColor()
    IF IsString(cNewColor)
        var newColor := ConsoleHelpers.String2Color(cNewColor)
        nBack := _AND(newColor, 0xF0) >> 4
        nFore := _AND(newColor, 0x0F)
        Console.ForegroundColor := (ConsoleColor) nFore
        Console.BackgroundColor := (ConsoleColor) nBack
    ENDIF
    RETURN cOldCol

#pragma options ("az", on)
INTERNAL STATIC CLASS ConsoleHelpers
    INTERNAL STATIC AltFileHandle := IntPtr.Zero as IntPtr
    //INTERNAL STATIC PrintFileHandle := IntPtr.Zero as IntPtr

    INTERNAL STATIC aColors := <STRING> {"N", "B","G","R","BG","RG","W","N+", "B+","G+","R+","BG+","RG+","W+"}  AS STRING[]
    INTERNAL STATIC METHOD ColNum2String(nColor AS ConsoleColor) AS STRING
        var bColor := (Byte) nColor
        IF bColor < aColors:Length
            RETURN aColors[bColor]
        ENDIF
        RETURN "?"


    INTERNAL STATIC METHOD String2Color(cColor AS STRING) AS BYTE
        LOCAL nColor        AS BYTE
        LOCAL nFore, nBack  AS BYTE
        LOCAL lFore		    AS LOGIC
        LOCAL nHigh		    AS BYTE
        nFore  := nBack := nColor := nHigh := 0
        lFore  := TRUE
        FOREACH VAR cChar in cColor
            local done := FALSE AS LOGIC
            SWITCH cChar
                CASE c'N'
                CASE c'n'
                    nColor := 0 // never combined
                CASE c'W'
                CASE c'w'
                    nColor := 7 // never combined
                CASE c'B'
                CASE c'b'
                    nColor += 1 // can be combined
                CASE c'G'
                CASE c'g'
                    nColor += 2 // can be combined
                CASE c'R'
                CASE c'r'
                    nColor += 4 // can be combined
                CASE c'*'
                    nHigh  := 8 // flashing: not available, so intense
                CASE c'+'
                    nHigh  := 8 // intense
                CASE c'/'
                    IF lFore
                        nFore 	:= nColor + nHigh
                        nColor	:= nHigh := 0
                        lFore 	:= FALSE
                    ELSE
                        done := TRUE
                    ENDIF
                CASE c','
                    done := TRUE
                OTHERWISE
                    // Ignore other characters
                    NOP
            END SWITCH
            IF done
                EXIT
            ENDIF
        NEXT
        nBack	   := nColor + nHigh
        nColor := (nBack<< 4) + nFore
        RETURN nColor

END CLASS


INTERNAL CLASS UnSafeNativeMethods

    [DllImport("User32.dll", CharSet := CharSet.Ansi)];
    STATIC METHOD MessageBox(hwnd AS IntPtr, lpText AS STRING, lpCaption AS STRING, uType AS DWORD) AS INT

    STATIC METHOD DoEvents() AS VOID
        local msg    as xMessage
        local handle as HandleRef
        msg     := xMessage{}
        handle  := HandleRef{}
        DO WHILE UnSafeNativeMethods.PeekMessage( REF msg,handle, 0,0, PM_NOREMOVE)
            IF UnSafeNativeMethods.GetMessage( REF msg, handle,0,0 )
                UnSafeNativeMethods.TranslateMessage(REF msg)
                UnSafeNativeMethods.DispatchMessage(REF msg)
            ENDIF
        ENDDO
        RETURN
    PRIVATE STRUCT xMessage
        PRIVATE hWnd   AS IntPtr
        PRIVATE msg    AS LONG
        PRIVATE wParam as IntPtr
        PRIVATE lParam as IntPtr
        PRIVATE result as IntPtr
    END STRUCT

    // Private Helper methods
    [DllImport("User32.dll", CharSet := CharSet.Ansi)];
    PRIVATE STATIC METHOD PeekMessage(msg REF xMessage, hwnd as HandleRef, msgMin as INT, msgMax as INT, iremove as Int) AS LOGIC
    [DllImport("User32.dll", CharSet := CharSet.Unicode)];
    PRIVATE STATIC METHOD PostMessage(msg AS xMessage, hwnd as HandleRef, imsg as INT, wParam AS IntPtr, lParam as IntPtr) AS LOGIC
    [DllImport("User32.dll", CharSet := CharSet.Unicode)];
    PRIVATE STATIC METHOD GetMessage(msg REF xMessage, hwnd as HandleRef, msgMin as INT, msgMax as INT) AS LOGIC
    [DllImport("User32.dll", CharSet := CharSet.Unicode)];
    PRIVATE STATIC METHOD TranslateMessage(msg REF xMessage) AS LOGIC
    [DllImport("User32.dll", CharSet := CharSet.Unicode)];
    PRIVATE STATIC METHOD DispatchMessage(msg REF xMessage) AS LOGIC


END CLASS



