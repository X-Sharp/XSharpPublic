USING System
USING System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)USING System.Linq
$endif$USING System.Text
USING System.Windows.Forms

USING $safeprojectname$

BEGIN NAMESPACE $safeprojectname$

[STAThread] ;
FUNCTION Start() AS INT

    LOCAL exitCode := 0 AS INT
    
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault( false )
    Application.Run( Form1{} )
   
    RETURN exitCode
	
END NAMESPACE


