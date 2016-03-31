#using System
#using System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)#using System.Linq
$endif$#using System.Text
#using System.Drawing
#using System.Windows.Forms
#using $safeprojectname$

Begin Namespace $safeprojectname$

	Function Start() as void
       Application.EnableVisualStyles()
       Application.SetCompatibleTextRenderingDefault( false )
       Application.Run( WinForm1{} )
	
End Namespace
