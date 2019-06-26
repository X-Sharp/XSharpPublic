//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

#using System.Windows.Forms

#define MB_TOPMOST 0x40000

/// <summary>Take input from the keyboard and return it.</summary>
FUNCTION _accept() AS STRING STRICT
   RETURN _accept( "" )

/// <summary>Take input from the keyboard and return it.</summary>
FUNCTION _accept( prompt AS STRING ) AS STRING
   LOCAL retval AS STRING

   Console.WriteLine()
   Console.Write( prompt )

   TRY
      retval := Console.ReadLine()
   CATCH AS System.InvalidOperationException
	 retval := ""
   END TRY

   RETURN IIF( retval == NULL, "", retval )

/// <summary>Clear the terminal window and position the cursor at row and column 0.</summary>
FUNCTION cls() AS VOID STRICT
   Console.Clear()
   RETURN
    
/// <summary>Return the screen column position of the cursor in the terminal window.</summary>
FUNCTION Col() AS SHORT STRICT
   RETURN (SHORT) Console.CursorLeft

/// <summary>Display the results of one or more expressions in the terminal window to the console, preceded with a newline.</summary>
FUNCTION QOut() AS VOID STRICT
   Console.WriteLine()
   RETURN

/// <summary>Display the results of one or more expressions in the terminal window to the console, preceded with a newline.</summary>
FUNCTION QOut( o AS USUAL ) AS VOID
   Console.WriteLine()
   QQOut( o )
   RETURN
/// <summary>Display the results of one or more expressions in the terminal window to the console, preceded with a newline.</summary>
FUNCTION QOut( o PARAMS USUAL[] ) AS VOID
   Console.WriteLine()
   QQOut( o )
   RETURN

/// <summary>Display the results of one or more expressions in the terminal window to the console.</summary>
FUNCTION QQOut( o AS USUAL ) AS VOID
   Console.Write( AsString( o ) )
   RETURN

/// <summary>Display the results of one or more expressions in the terminal window to the console.</summary>
FUNCTION QQOut( o PARAMS  USUAL[] ) AS VOID
   LOCAL count := o:Length AS INT
   LOCAL x                 AS INT

   FOR x := 1 UPTO count
      QQOut( o[x] )
      IF x < count
         Console.Write( " " )
      ENDIF
   NEXT
   RETURN

/// <summary>Return the screen row position of the cursor in the terminal window.</summary>
FUNCTION Row() AS SHORT
   RETURN (SHORT) Console.CursorTop

/// <summary>Move the cursor to a new position on the terminal window.</summary>
FUNCTION SetPos( nRow AS INT, nCol AS INT ) AS VOID
   Console.SetCursorPosition( nCol, nRow )
   RETURN


/// <summary>Display a prompt after sending a carriage return/linefeed to the terminal window, then wait for a key to be pressed.</summary>
 FUNCTION _wait() AS STRING STRICT
   RETURN _wait( __CavoStr(VoErrors.TMSG_PRESSANYKEY))

/// <summary>Display a prompt after sending a carriage return/linefeed to the terminal window, then wait for a key to be pressed.</summary>
FUNCTION _wait( prompt AS STRING ) AS STRING
   LOCAL info AS ConsoleKeyInfo
   LOCAL retval AS STRING

   Console.WriteLine()
   Console.Write( prompt )

   TRY
      info   := Console.ReadKey()
      retval := info:KeyChar:ToString()
   CATCH AS System.InvalidOperationException
      MessageBox.Show( prompt + chr(10) + chr(10) + "Wait", "Wait", MessageBoxButtons.OK, MessageBoxIcon.Exclamation, MessageBoxDefaultButton.Button1, (MessageBoxOptions)(INT) MB_TOPMOST )
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
