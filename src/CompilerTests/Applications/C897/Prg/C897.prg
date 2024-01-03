// 897. UDCs do not allow all possible orders of optional clauses
// https://github.com/X-Sharp/XSharpPublic/issues/1410
FUNCTION Start( ) AS VOID
	LOCAL a AS INT
	LOCAL f := TRUE AS LOGIC
	LOCAL wh := TRUE AS LOGIC
	LOCAL cDbf := "C897" AS STRING
	DbCreate(cDbf , {{"FLD","N",5,0}})
	DbUseArea(TRUE,,cDbf)
	DbAppend()
	DbAppend()
	DbAppend()
	DbAppend()
	DbGoTop()

	COUNT TO a FOR f WHILE wh NEXT 1
	? a
	xAssert(a==1)
	COUNT TO a FOR f NEXT 2 WHILE wh
	? a
	xAssert(a==2)
	COUNT TO a WHILE FALSE FOR f NEXT 3
	? a
	xAssert(a==0)
	COUNT WHILE wh TO a FOR f NEXT 4
	? a
	xAssert(a==3)
	COUNT FOR f NEXT 5 WHILE wh TO a
	? a
	xAssert(a==0)

	DbCloseArea()
RETURN

PROCEDURE Test()
COPY STRUCTURE EXTENDED TO Test
COPY STRUCTURE TO a FIELDS b,c
COPY STRUCTURE FIELDS b,c TO a

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
