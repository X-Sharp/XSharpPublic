// 770. Problems with passing values by reference in FoxPro dialect
/*
When using the = operator to assign values to params passed by reference, the new value is not passed back to the caller
Also when calling a method latebound, PRIVATEs passed by reference do not get updated values even with :=
*/
FUNCTION Start( ) AS VOID
	LOCAL oFoo AS Foo
	
	LOCAL x = 0
	PRIVATE p 
	
	oFoo := Foo{}
	
	oFoo:TestTyped_colonequals(@x,1) // OK
	? x
	xAssert(x==1)
	oFoo:TestTyped_equals(@x,2) // fails
	? x
	xAssert(x==2)
	
	oFoo:TestTyped_colonequals(p,3)
	? p
	xAssert(p==0)
	oFoo:TestTyped_equals(p,4)
	? p
	xAssert(p==0)

	oFoo:TestTyped_colonequals(@p,5) // this works correctly
	? p
	xAssert(p==5)
	oFoo:TestTyped_equals(@p,6)
	? p
	xAssert(p==6)
	
	? "-------"
	
	LOCAL lb

	lb := Foo{}
	x = 0;p = 0

	lb:TestTyped_colonequals(@x,1)
	? x
	xAssert(x==1)
	lb:TestTyped_equals(@x,2)
	? x
	xAssert(x==2)

	lb:TestTyped_colonequals(p,3)
	? p
	xAssert(p==0)
	lb:TestTyped_equals(p,4)
	? p
	xAssert(p==0)

	lb:TestTyped_colonequals(@p,5) // this fails
	? p
	xAssert(p==5)
	lb:TestTyped_equals(@p,6)
	? p
	xAssert(p==6)

RETURN

CLASS Foo
	METHOD TestTyped_colonequals(r,n)
		r := n
	RETURN
	METHOD TestTyped_equals(r,n)
		r = n
	RETURN
END CLASS

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

