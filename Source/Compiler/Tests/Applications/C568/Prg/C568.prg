// 568. error XS0122: 'TestClass' is inaccessible due to its protection level
// the auto created class in the helper dll is emitted as internal
FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	? o
RETURN
