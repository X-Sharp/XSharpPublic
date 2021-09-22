USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.Windows.Forms

USING UDCTesterApp
GLOBAL oSettings as UDCSettings
BEGIN NAMESPACE UDCTesterApp

[STAThread] ;
	FUNCTION Start() AS VOID
    oSettings := UdcSettings{}
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault( FALSE )
    Application.Run( UDCTester{} )
   
    RETURN
	
END NAMESPACE


