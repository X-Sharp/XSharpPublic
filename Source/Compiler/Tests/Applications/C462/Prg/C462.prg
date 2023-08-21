// 462. error XS0150: A constant value is expected
// reported by Frank Maraite
#pragma warnings(165, off) // unassigned
FUNCTION Start() AS VOID
LOCAL  cTest AS Char
LOCAL lFOund AS LOGIC
cTest := 'a'
SWITCH cTest
CASE 'a'  // <-- XS0150 A constant Value is expected
	? "ok"
	lFound := TRUE
END SWITCH

IF .not. lFound
	THROW Exception{"Incorrect result"}
END IF
