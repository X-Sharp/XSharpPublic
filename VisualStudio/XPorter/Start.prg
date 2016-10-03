#using System.Windows.Forms
#using System.Drawing

[STAThreadAttribute];
FUNCTION Start(  ) AS INT
	
	LOCAL oForm AS frmXporter
	
	Application.EnableVisualStyles()
	Application.DoEvents()
	
	oForm := frmXporter{}
	Application.Run(oForm)

RETURN 0

