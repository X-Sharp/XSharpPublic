USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.Windows.Forms

USING XSharp.VFP.UI_Test

BEGIN NAMESPACE XSharp.VFP.UI_Test

[STAThread] ;
	FUNCTION Start() AS VOID
    TRY
       Application.EnableVisualStyles()
       Application.SetCompatibleTextRenderingDefault( FALSE )
       Application.Run( MainWindow{} )
    CATCH e as Exception
         MessageBox(e:ToString())
    END TRY
    RETURN

END NAMESPACE


