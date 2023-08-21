// https://github.com/X-Sharp/XSharpPublic/issues/770

FUNCTION Start() AS VOID STRICT
    TestClass{}:Test()
    RETURN

PUBLIC CLASS TestClass

    PUBLIC METHOD Test() AS VOID STRICT

        VAR a := ClassA{ClassB{"X#rules"}}

        LOCAL b := a?:B AS ClassB // without the question mark (i.e. "local b := a.B as ClassB") no exception is thrown
        xAssert(b:S == "X#rules")
        b := a:B
        xAssert(b:S == "X#rules")
        RETURN

END CLASS

PUBLIC CLASS ClassA

    PUBLIC PROPERTY B AS USUAL AUTO

    PUBLIC CONSTRUCTOR(b AS USUAL) STRICT
        SELF:B := b
        RETURN

END CLASS

PUBLIC CLASS ClassB

    PUBLIC PROPERTY S AS STRING AUTO

    PUBLIC CONSTRUCTOR(s AS STRING) STRICT
        SELF:S := s
        RETURN

END CLASS


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

