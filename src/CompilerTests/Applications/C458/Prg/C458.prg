// error XS1503: Argument 1: cannot convert from 'method' to 'int'
// name conflict between DEFINE and METHOD
#pragma warnings(219, off) // assigned but not used
FUNCTION Start() AS VOID
	TestClass{}
RETURN

DEFINE HELPINFO := 3

CLASS HelpRequestEvent
ACCESS HelpType AS DWORD STRICT
RETURN (DWORD) HELPINFO
ACCESS HelpInfo AS PTR STRICT
RETURN NULL_PTR
END CLASS


DEFINE SHOWCENTERED := 1

CLASS TestClass
	CONSTRUCTOR()
		SELF:Show( SHOWCENTERED )
	RETURN
	METHOD Show(nStyle AS INT) AS VOID
		? nStyle
	RETURN
	METHOD ShowCentered() AS VOID
		SELF:Show( SHOWCENTERED )
	RETURN
END CLASS


DEFINE CENTER := 1
CLASS MyWindow INHERIT Vulcan.VO.Window
METHOD Test() AS VOID
	LOCAL n AS INT
	n := CENTER
RETURN
END CLASS
