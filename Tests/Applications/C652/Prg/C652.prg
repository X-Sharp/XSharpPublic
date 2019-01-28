// 652. error XS0266: Cannot implicitly convert type 'int' to 'byte'. 
FUNCTION Start() AS VOID
	LOCAL lLogic := TRUE AS LOGIC
	LOCAL b AS BYTE
	b := 1 // no error
	b := iif(TRUE, 1,2) // no error
	b := iif(lLogic, 1,2) // ERROR
	? b

	LOCAL w AS WORD
	w := 1024 // no error
	w := iif(FALSE, 1024,1025) // no error
	w := iif(lLogic, 1024,1025) // ERROR
	? w
	LOCAL d AS DWORD
	d := 1 // no error
	d := iif(TRUE, 1,2) // no error
	d := iif(lLogic, 1,2) // ERROR
RETURN
