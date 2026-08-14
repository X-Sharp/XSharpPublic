#pragma options("fox3", enable)
#pragma warnings(165, disable)
using XSharp.VFP
using XSharp.Internal
FUNCTION Start as VOID
    private Foo
    try
    foo = CreateObject("Empty")
    AddProperty(foo, "bar", 456)
    DbCreate("foo", {{"Bar","N",10,0}})
    USE Foo
    Append blank
    FieldPut(1, 123)
    ? "->",Foo->Bar
    xAssert(Foo->Bar == 123)
    ? "m ",m->Foo.Bar
    xAssert(m->Foo.Bar == 456)
    ? "m ",m.Foo.Bar
    xAssert(m.Foo.Bar == 456)
    ? ". ",Foo.Bar
    xAssert(Foo.Bar == 123)
    ? ".:",Foo.:Bar
    xAssert(Foo.:Bar == 222)

    Foo.Bar = 333
    Foo.:Bar := 444
    m.Foo.Bar = 555
    ? "->",Foo->Bar
    xAssert(Foo->Bar == 333)
    ? ". ",Foo.Bar
    xAssert(Foo.Bar == 333)
    ? ".:",Foo.:Bar
    xAssert(Foo.:Bar == 444)
    ? "m ",m->Foo.Bar
    xAssert(m->Foo.Bar == 555)
    ? "m ",m.Foo.Bar
    xAssert(m.Foo.Bar == 555)

    DbCloseAll()
    xAssert(Foo.Bar == 555)     // Area closed, return local
    Foo.Bar = 666
    ? ". ",Foo.Bar
    xAssert(Foo.Bar == 666)
    xAssert(m.Foo.Bar == 666)
    xAssert(m->Foo.Bar == 666)


    catch oError
        ? oError:ToString()
    end try
    RETURN

CLASS Foo
    //STATIC PROPERTY Bar as INT GET 111
    STATIC Export Bar := 222 as LONG
END CLASS

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
