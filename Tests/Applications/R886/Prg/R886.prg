#translate $(<d>/<m>/<y>) => {^<y>-<m>-<d>}
FUNCTION Start() AS VOID STRICT
local d := $(23/03/2023)
xAssert( $(24/03/2023) > d )
xAssert( $(22/03/2023) < d )
xAssert(2023.03.23 == d)

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
