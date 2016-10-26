// 198. error XS1656: Cannot assign to 'n' because it is a 'foreach iteration variable'
// incompatibility with vulcan
// convert error to warning?
FUNCTION Start( ) AS VOID
	LOCAL a AS INT[]
	a := <INT>{1,2,3}
	FOREACH n AS INT IN a
		? n
		n := 1
		? n
	NEXT
RETURN

