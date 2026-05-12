// main.prg
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.Windows.Forms

USING XSharp.VFP.UI


FUNCTION Start AS VOID
	// Create the _Screen var
    _Screen := XSharp.VFP.UI.MainWindow{}
    _MSYSMENU := _Screen:MainMenu

	// Create the Application Object
	_vfp := XSharp.VFP.UI.Application{}

	// The original Entry Point is here
	<@startcode@>
    RETURN


GLOBAL _Screen AS XSharp.VFP.UI.MainWindow

GLOBAL _vfp AS XSharp.VFP.UI.Application

GLOBAL _MSYSMENU AS System.Windows.Forms.MainMenu