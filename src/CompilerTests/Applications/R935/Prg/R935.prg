// See https://github.com/X-Sharp/XSharpPublic/pull/1988

FUNCTION Start( ) AS VOID
    local loDog
    local loAnimal
	loDog = createobject("Dog", "Rex")
? loDog._name
xAssert(loDog._name == "Rex")
loAnimal := createobject("Animal", "Mickey")
? loAnimal._name
xAssert(loAnimal._name == "Mickey")
return

define class Animal as Custom
    _name = ""
    procedure Init(tcName)
        this._name = tcName
    endproc
enddefine

define class Dog as Animal
    && no Init defined
enddefine



PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
