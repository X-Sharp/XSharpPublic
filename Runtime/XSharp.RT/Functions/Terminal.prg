//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

#using System.Windows.Forms

#define MB_TOPMOST 0x40000

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

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/qout/*" />
FUNCTION QOut() AS VOID STRICT
   Console.WriteLine()
   RETURN

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/qout/*" />
FUNCTION QOut( uValueList AS USUAL ) AS VOID
   Console.WriteLine()
   QQOut( uValueList )
   RETURN

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/qout/*" />
FUNCTION QOut( uValueList PARAMS USUAL[] ) AS VOID
   Console.WriteLine()
   QQOut( uValueList )
   RETURN

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/qqout/*" />
FUNCTION QQOut( uValueList AS USUAL ) AS VOID
   Console.Write( AsString( uValueList ) )
   RETURN

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/qqout/*" />
FUNCTION QQOut( uValueList PARAMS  USUAL[] ) AS VOID
   LOCAL count := uValueList:Length AS INT
   LOCAL x                 AS INT
   LOCAL lAddSpace         AS LOGIC
   lAddSpace := RuntimeState.GetValue<LOGIC>(Set.Space)
   FOR x := 1 UPTO count
      QQOut( uValueList[x] )
      IF x < count .and. lAddSpace
         Console.Write( " " )
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
   RETURN _wait( __CavoStr(VoErrors.TMSG_PRESSANYKEY))

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
      MessageBox.Show( uValuePrompt + chr(10) + chr(10) + "Wait", "Wait", MessageBoxButtons.OK, MessageBoxIcon.Exclamation, MessageBoxDefaultButton.Button1, (MessageBoxOptions)(INT) MB_TOPMOST )
      retval := ""
   END TRY

   RETURN retval
/// <exclude/>
FUNCTION DoEvents() AS VOID
    System.Windows.Forms.Application.DoEvents()


FUNCTION ShowArray  (aTest as array, cName := "" as STRING) AS VOID
    LOCAL i         AS DWORD
    LOCAL n         AS DWORD
    LOCAL x         AS USUAL
    LOCAL cOut      AS STRING
    LOCAL cOutTemp := "" AS STRING

    IF cName:Length == 0
        cName := "a"
    ENDIF

    n := ALen(aTest)

    FOR i := 1 TO n
        cOut := cName + "[" + NTrim(i) + "]"
        x    := aTest[i]

        IF x:isArray
            cOutTemp := cOut
        ENDIF

        cOut += " = "
        cOut += AsString(x)
        cOut += " ("
        cOut += ValType(x)
        cOut += ")"
        Qout(cOut)

        IF x:IsArray
            ShowArray(x, cOutTemp)
        ENDIF

    NEXT
    RETURN 
