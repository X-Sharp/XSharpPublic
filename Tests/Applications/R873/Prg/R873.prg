using System.Reflection
// https://github.com/X-Sharp/XSharpPublic/issues/1108
//#pragma warnings( 9073, off)      // Variable 'oTest' has not been declared. Assuming this is a FIELD or a MEMVAR.
FUNCTION Start( ) AS VOID
	private oTest
	otest := TestClass{}
    tester()
RETURN



function Tester as void
    try
    //       memvar oTest
    /// the 2.13 compiler compiles this into
    // oTest:DoSomething[1]
    // iow
    // IVarGet(Varget("oTest"),"DoSomething")[1]

    // we need to add a new runtime function that figures out if "DoSomething" is a property/field or method
    ? oTest:DoSomething(1)
    ? oTest:DoSomething2(1,2)
    //? oTest:DoSomething3(1) // error
    ? oTest:DoSomething3("Robert")
    ? oTest:SomeProperty(1)
    oTest:SomeProperty[1] := 101
    ? oTest:SomeProperty(1)
    ? oTest:SomeProperty2Dim(1,2) // error


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
    method DoSomething(nInt as int) as int
        ? nInt
        return nInt
    method DoSomething2(nInt1 as int, nInt2 as int) as int
        ? nInt1, nInt2
        return nInt1 + nInt2

    method DoSomething3(cName as STRING) as int
        ? cName
        return 42

end class



