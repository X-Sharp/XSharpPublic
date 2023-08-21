// R763 After a change in the lexer, keywords after a semi colon were not recognized if the semi colon was following a "dotted identifier"
FUNCTION Start( ) AS VOID
	System.Console.WriteLine("Hello x#!")
	? TestSemiColons()
	xAssert(TestSemiColons() == -42)
RETURN


FUNCTION TestSemiColons() AS LONG
    LOCAL oFoo AS Test
    oFoo := Test{}{Foo := 42}
    SWITCH oFoo:Foo
    CASE testEnum.One; RETURN oFoo:Foo
    CASE testEnum.FortyTwo; RETURN - oFoo:Foo
    OTHERWISE
        RETURN 0
    END SWITCH


CLASS Test
    PROPERTY Foo AS LONG AUTO
END CLASS


ENUM testEnum
    MEMBER One := 1
    MEMBER FortyTwo := 42
END ENUM

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

