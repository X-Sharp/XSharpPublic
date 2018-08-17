//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

#using System.Windows.Forms

#define MB_TOPMOST 0x40000


FUNCTION _accept() AS STRING STRICT
   RETURN _accept( "" )

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

FUNCTION cls() AS VOID STRICT
   Console.Clear()
   RETURN

FUNCTION Col() AS SHORT STRICT
   RETURN (SHORT) Console.CursorLeft

FUNCTION QOut() AS VOID STRICT
   Console.WriteLine()
   RETURN

FUNCTION QOut( o AS USUAL ) AS VOID
   Console.WriteLine()
   QQOut( o )
   RETURN

FUNCTION QOut( o PARAMS USUAL[] ) AS VOID
   Console.WriteLine()
   QQOut( o )
   RETURN

FUNCTION QQOut( o AS USUAL ) AS VOID
   //Console.Write( _ToVOString( o ) )
   Console.Write( AsString( o ) )
   RETURN

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

FUNCTION Row() AS SHORT
   RETURN (SHORT) Console.CursorTop

FUNCTION SetPos( nRow AS INT, nCol AS INT ) AS VOID
   Console.SetCursorPosition( nCol, nRow )
   RETURN

 FUNCTION _wait() AS STRING STRICT
   RETURN _wait( __CavoStr(VoErrors.TMSG_PRESSANYKEY))

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

FUNCTION DoEvents() AS VOID
    System.Windows.Forms.Application.DoEvents()
