using System.Reflection
using System.Linq
function Start as void
    TRY
        local oFoo as Foo
        oFoo := Foo():New()
        ? oFoo:ClassObject()
    ? "SomeVar", Foo():SomeVar
    xAssert(Foo():SomeVar == "SomeVar")
    //? "SomeNotExistingVar", Foo():SomeNotExistingVar
    ? "AnotherVar", Foo():AnotherVar
    xAssert(Foo():AnotherVar == "AnotherVar")
    Foo():AnotherVar := "new Value"
    xAssert(Foo():AnotherVar == "new Value")
    //Foo():NoAnotherVar := "new Value"
    xAssert(Foo():MethodCall("a","b","c") == "abc")
    //? Foo():DoNotExist()
    CATCH oEx as Exception
        ? oEx:ToString()
    END TRY
CLASS Foo
EXPORTED:
    CLASS VAR SomeVar
    CLASS VAR AnotherVar
    CLASS METHOD MethodCall()
    CLASS METHOD InitClass

ENDCLASS

CLASS METHOD Foo:InitClass()
    ::SomeVar := "SomeVar"
    ::AnotherVar := "AnotherVar"

METHOD Foo:MethodCall(a,b,c)
      ? ProcName(1)  , PCount(), "parameters"
      var result := ""
      FOR VAR i := 1 to PCount()
          ? i,_GetMParam(i)
          result += _GetMParam(i)
      NEXT
      RETURN result


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
