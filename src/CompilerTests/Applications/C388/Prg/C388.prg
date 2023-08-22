// 388. Problem with _And(), WORD and DWORD
// error XS0034: Operator '&' is ambiguous on operands of type 'word' and 'dword'
FUNCTION Start( ) AS VOID
LOCAL w AS WORD
LOCAL dw AS DWORD
w := 1976
dw := 0xFFFFFFFF

? _And(w,dw) // OK
? DWORD(_And(w,dw)) // OK

? _And(w,0xFFFFFFFF) // OK
? DWORD(_And(w,0xFFFFFFFF)) // Error

IF _And(w,dw) != 1976 .or. DWORD(_And(w,dw)) != 1976 .or. _And(w,0xFFFFFFFF) != 1976 .or. DWORD(_And(w,0xFFFFFFFF)) != 1976
	THROW Exception{"Incorrect result"}
END IF


// note that the bogus error in the original code accidentally pointed to a problem in the code:
w := 1976
? DWORD(_And(w,0xFF000000))
// this does not make sense, because the _and() result will always be zero, so it would be nice
// if the compiler did report a warning on using _And(), _Or(), _XOr() with arguments with different byte length

