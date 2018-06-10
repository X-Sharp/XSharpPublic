//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

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

