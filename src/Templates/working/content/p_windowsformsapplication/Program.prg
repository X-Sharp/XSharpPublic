USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.Windows.Forms

USING Company.Namespace1

[STAThread] ;
FUNCTION Start() AS VOID

Application.EnableVisualStyles()
Application.SetCompatibleTextRenderingDefault( FALSE )
Application.Run( Form1{} )

RETURN
