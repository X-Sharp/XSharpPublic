// https://github.com/X-Sharp/XSharpPublic/issues/770

FUNCTION Start() AS VOID STRICT
    TestClass{}.Test()
    RETURN

PUBLIC CLASS TestClass

    PUBLIC METHOD Test() AS VOID STRICT

        VAR a := ClassA{ClassB{"asdf"}}

        LOCAL b := a?:B AS ClassB // without the question mark (i.e. "local b := a.B as ClassB") no exception is thrown

        Console.WriteLine("A: " + b:S) // b.S throws System.AccessViolationException
        Console.ReadLine()

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

