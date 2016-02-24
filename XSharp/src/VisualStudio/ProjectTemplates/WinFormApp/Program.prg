#using System
#using System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)#using System.Linq
$endif$#using System.Text
#using System.Windows.Forms

#using $safeprojectname$

Begin Namespace $safeprojectname$

[STAThread] ;
FUNCTION Start() AS INT

    LOCAL exitCode := 0 AS INT
    
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault( false )
    Application.Run( Form1{} )
   
    RETURN exitCode
	
End Namespace


