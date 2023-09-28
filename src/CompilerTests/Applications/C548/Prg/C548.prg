// 548. System.AccessViolationException: Attempted to read or write protected memory. This is often an indication that other memory is corrupt.
/*
Problem is the incorrect code that assignes a usual holding Foo to a var defined as Bar

In Vulcan, this problem is found in the assignment and an exception is thrown
and this exception can be caught of course.

In X#, the bad assignment is not throwing an exception, but instead this causes
an access vialoation to be causes later in execution of the problem and this exception
cannot be caught to exit gracefully

Difference between vulcan/x# is that vulcan adds a cast to Bar after the assignment,
which traps the problem earlier at this point

x code for "oBar := uFooInUsual":

call object [VulcanRTFuncs]Vulcan.__Usual::ToObject(valuetype [VulcanRTFuncs]Vulcan.__Usual)

vulcan code:

call object [VulcanRTFuncs]Vulcan.__Usual::ToObject(valuetype [VulcanRTFuncs]Vulcan.__Usual)
castclass Bar

*/
CLASS Foo
END CLASS

CLASS parent
	VIRTUAL METHOD Test() AS VOID
	RETURN
END CLASS
CLASS Bar INHERIT parent
END CLASS

FUNCTION Start() AS VOID
	TRY
		DoTest()
	CATCH e AS InvalidCastException
		? "InvalidCastException correcty trapped through an exception:"
		?
		? e:ToString()
	END TRY
RETURN

PROCEDURE DoTest()
	LOCAL oFoo AS Foo
	oFoo := Foo{}

	LOCAL uFooInUsual AS USUAL
	uFooInUsual := oFoo

	LOCAL oBar AS Bar

	// vulcan thrwos a (correct) runtime exception here, because it uses an extra cast to Bar
	oBar := uFooInUsual

	// this leads to an (uncatchable) acces violation in X#. Problem should be trapped in the above line
	oBar:Test()
RETURN
