using System.Reflection
// https://github.com/X-Sharp/XSharpPublic/issues/1108
#pragma warnings( 9073, off)      // Variable 'oTest' has not been declared. Assuming this is a FIELD or a MEMVAR.
FUNCTION Start( ) AS VOID
	private oTest as TestClass
	otest := TestClass{}
    tester()
RETURN



function Tester as void
    try
    //       memvar oTest
    xAssert(oTest:SquareRoot(4) == 2)
    xAssert(oTest:Add(2,3) == 5)
    //? oTest:DoSomething3(1) // error
    xAssert(oTest:DoSomething3("Robert") ==42)
    xAssert(oTest:SomeProperty(1) == 42)
    oTest:SomeProperty[1] := 101
    xAssert(oTest:SomeProperty(1) == 101)

    xAssert(oTest:SomeProperty2Dim(1,2) == 2)
    oTest:SomeProperty2Dim(1,2) = 424242
    xAssert(oTest:SomeProperty2Dim(1,2) == 424242)

    catch e as Exception
        ? e:ToString()
    end try
    return





#pragma options("memvars", off)
#pragma options("undeclared", off)
CLASS testClass
    PROPERTY SomeProperty AS ARRAY AUTO
    PROPERTY SomeProperty2Dim AS ARRAY AUTO
    CONSTRUCTOR
        SomeProperty := {42,43,44}
        SomeProperty2Dim := {{1,2,3},{4,5,6}}
    method SquareRoot(nInt as int) as float
        return SQrt(nInt)
    method Add(nInt1 as int, nInt2 as int) as int
        return nInt1 + nInt2

    method DoSomething3(cName as STRING) as int
        return 42

end class



PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN
