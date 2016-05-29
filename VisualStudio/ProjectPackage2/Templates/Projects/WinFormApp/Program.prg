USING System
USING System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)USING System.Linq
$endif$USING System.Text
USING System.Windows.Forms

USING $safeprojectname$

BEGIN NAMESPACE $safeprojectname$

[STAThread] ;
	Function Start() as void
    
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault( false )
    Application.Run( WinForm1{} )
   
    RETURN
	
END NAMESPACE


