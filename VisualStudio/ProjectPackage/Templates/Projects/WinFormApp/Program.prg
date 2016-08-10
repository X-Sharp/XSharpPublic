USING System
USING System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)USING System.Linq
$endif$USING System.Text
USING System.Windows.Forms

USING $safeprojectname$

BEGIN NAMESPACE $safeprojectname$

[STAThread] ;
	FUNCTION Start() AS VOID
    
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault( FALSE )
    Application.Run( Form1{} )
   
    RETURN
	
END NAMESPACE


