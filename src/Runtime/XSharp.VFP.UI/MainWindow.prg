// MainWindow.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.


USING System
USING System.Collections.Generic
USING System.Text
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFP compatible MainWindow class.
	/// Used to emulate the Visual FoxPro window object through _Screen
	/// </summary>
	CLASS MainWindow INHERIT Form
        PRIVATE _Forms AS List<Form>

        PROPERTY MainMenu AS System.Windows.Forms.MainMenu AUTO

		CONSTRUCTOR()
            Super()
            SELF:_Forms := List<Form>{}
            SELF:MainMenu := System.Windows.Forms.MainMenu{}
            SELF:Size := System.Drawing.Size{ 800, 600 }
			RETURN

		PUBLIC METHOD Forms( i AS INT ) AS Form
			RETURN SELF:_Forms[ i-1 ]

		PROPERTY FormCount AS LONG GET SELF:_Forms:Count

	END CLASS
END NAMESPACE // XSharp.VFP.UI
