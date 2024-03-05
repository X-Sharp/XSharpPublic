// VFPMainWindow.prg
// Created by    : fabri
// Creation Date : 9/10/2022 5:48:22 PM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFPMainWindow class.
	/// Used to emulate the Visual FoxPro window object through _Screen
	/// </summary>
	CLASS MainWindow INHERIT Form
        PRIVATE _Forms AS List<Form>

        PROPERTY MainMenu AS System.Windows.Forms.MainMenu AUTO

		CONSTRUCTOR()
			Super()
            SELF:_Forms := List<Form>{}
            SELF:MainMenu := System.Windows.Forms.MainMenu{}
			RETURN

		PUBLIC METHOD Forms( i AS INT ) AS Form
			RETURN SELF:_Forms[ i-1 ]

		PROPERTY FormCount AS LONG GET SELF:_Forms:Count

	END CLASS
END NAMESPACE // XSharp.VFP.UI