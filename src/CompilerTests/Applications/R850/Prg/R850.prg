// https://github.com/X-Sharp/XSharpPublic/issues/885

function start as void


local aTest as array of Brw
local uBrw as usual
aTest := {}

uBrw := Brw{}
AAdd(aTest, uBrw)
? aTest[1]
xAssert(aTest[1] == uBrw)
return

class Brw
end class


PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN
