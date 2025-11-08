USING System
USING System.Collections.Generic
USING System.Windows.Forms

USING XSharp.WinFormsCore

[STAThread] ;
FUNCTION Start() AS VOID

Application.EnableVisualStyles()
Application.SetCompatibleTextRenderingDefault( FALSE )
Application.Run( Form1{} )

RETURN
