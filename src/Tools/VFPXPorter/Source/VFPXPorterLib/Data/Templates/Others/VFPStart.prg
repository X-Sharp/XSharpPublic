// main.prg
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.Windows.Forms

USING XSharp.VFP.UI


FUNCTION Start AS VOID
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault( FALSE )
	// Create the _Screen var
    _Screen := XSharp.VFP.UI.MainWindow{}

	// Create the Application Object
	_vfp := XSharp.VFP.UI.Application{}

	// The original Entry Point is here
    _Screen:StartFunction := "<@startcode@>"
    _Screen:StartForm := "<@startform@>"
    _Screen:StartMenu := "<@startmenu@>"

    Application.Run( _Screen )

    RETURN


GLOBAL _Screen AS XSharp.VFP.UI.MainWindow

GLOBAL _vfp AS XSharp.VFP.UI.Application

GLOBAL _MSYSMENU AS XSharp.VFP.UI.Menu
