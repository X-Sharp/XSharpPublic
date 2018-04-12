#using System.Windows.Forms

#define MB_TOPMOST 0x40000 


FUNCTION _accept() AS STRING
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
   
   RETURN iif( retval == NULL, "", retval )

FUNCTION cls() AS VOID
   Console.Clear()
   RETURN
   
FUNCTION Col() AS SHORT
   RETURN (SHORT) Console.CursorLeft  

FUNCTION QOut() AS VOID
   Console.WriteLine()
   RETURN

FUNCTION QOut( o AS USUAL ) AS VOID
   Console.WriteLine()
   QQOut( o )
   RETURN
      
FUNCTION QOut( o params USUAL[] ) AS VOID
   Console.WriteLine()
   QQOut( o )
   RETURN

FUNCTION QQOut( o AS USUAL ) AS VOID
   //Console.Write( _ToVOString( o ) )
   Console.Write( AsString( o ) )
   RETURN
   
FUNCTION QQOut( o params  USUAL[] ) AS VOID
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

 FUNCTION _wait() AS STRING
   RETURN _wait( "Press any key to continue")

FUNCTION _wait( prompt AS STRING ) AS STRING
   LOCAL info AS ConsoleKeyInfo
   LOCAL retval AS STRING
   
   Console.WriteLine()
   Console.Write( prompt )
   
   TRY
      info   := Console.ReadKey()
      retval := info:KeyChar:ToString()
   CATCH AS System.InvalidOperationException
      MessageBox.Show( prompt + chr(10) + chr(10) + "Wait", "Wait", MessageBoxButtons.OK, MessageBoxIcon.Exclamation, MessageBoxDefaultButton.Button1, (MessageBoxOptions)(Int) MB_TOPMOST )
      retval := ""
   END TRY
   
   RETURN retval 


