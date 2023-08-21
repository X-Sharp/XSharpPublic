// error XS1955: Non-invocable member 'TextBox' cannot be used like a method.
USING System.Windows.Forms

FUNCTION Start() AS VOID
	NS.wShellWindow{}:Reorg()
RETURN

FUNCTION TextBox() AS INT
RETURN 0

BEGIN NAMESPACE NS
// compiler error only when this USING is here:
USING System.Windows.Forms

	CLASS wShellWindow
		METHOD Reorg() AS VOID
			? TextBox() // error XS1955
		RETURN
	END CLASS

END NAMESPACE

