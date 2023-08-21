// 417. error XS0266: Cannot implicitly convert type 'ITest' to 'Vulcan.__Usual'. An explicit conversion exists (are you missing a cast?)
// Vulcan does not support this either so leave it for now
INTERFACE ITest
	METHOD Test() AS VOID
END INTERFACE

CLASS TestClass IMPLEMENTS ITest
	VIRTUAL METHOD Test( ) AS VOID
	? "test"
END CLASS

FUNCTION Start() AS VOID
	LOCAL i AS ITest
	LOCAL o AS OBJECT
	LOCAL u AS USUAL
	i := TestClass{}
	o := i
	u := i
	? u
	o := u
	? o
	u := o
	? u

	LOCAL a := {} AS ARRAY
	AAdd(a,i)
	a := {i}
	? a[1]
	o := a[1]
	? o
	i := a[1]
	? i:ToString()
RETURN
