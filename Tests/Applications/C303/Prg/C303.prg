// 303. Runtime exception:
// System.FormatException
// Index (zero based) must be greater than or equal to zero and less than the size of the argument list.
// String>format resolves to string,object[] overload, instead of string,object
FUNCTION Start() AS VOID
LOCAL u := 0 AS USUAL
LOCAL c := "{0:g}" AS STRING
c := String.Format(c , u)
? c 
IF c != "0"
	THROW Exception{"Incorrect result from String.Format(c , u)"}
END IF
