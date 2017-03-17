// error XS1503: Argument 1: cannot convert from 'method' to 'int'
// name conflict between DEFINE and METHOD
FUNCTION Start() AS VOID
	TestClass{}
RETURN

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

