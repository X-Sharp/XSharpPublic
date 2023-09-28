USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE VFP_ConsoleApplication8

	/// <summary>
	/// The Class1 class.
	/// </summary>
	CLASS Class1

		PUBLIC MyTab AS USUAL

		CONSTRUCTOR()

			// This does not work
			DIMENSION this.MyTab(3)
            MyTab = 42
			// Same inside a WITH clause
			WITH This
				DIMENSION .MyTab(4)
			END WITH
            ShowArray(This.MyTab) // shows 3 x 42 and 1 x NIL
			RETURN

	END CLASS
END NAMESPACE // VFP_ConsoleApplication8
FUNCTION Start() as void
    Class1{}
    return

