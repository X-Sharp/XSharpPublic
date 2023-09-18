// See https://github.com/X-Sharp/XSharpPublic/issues/1267

USING System
USING System.Collections.Generic
USING System.Text


FUNCTION Start() as void
    LOCAL o := Class1{} as usual
    xAssert(IsArray(o:MyTab))
    xAssert(ALen(o:MyTab) == 4)
    xAssert(o:MyTab(1) == 42)
    xAssert(o:MyTab[2] == 42)
    xAssert(o:MyTab[3] == 42)
    xAssert(o:MyTab[4] == NIL)
    return





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


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
RETURN
