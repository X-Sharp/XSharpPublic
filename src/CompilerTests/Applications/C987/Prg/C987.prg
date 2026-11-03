// 987. Problem with /fox3 and ENUMs #2091
// https://github.com/X-Sharp/XSharpPublic/issues/2091

//#pragma options("fox2", enable)
#pragma options("fox3", enable)
#pragma options("allowdot", enable)
#pragma options("lb", enable)
#pragma options("memvar", enable)
#pragma options("undeclared", enable)

FUNCTION Start( ) AS VOID
LOCAL lCentury,lDeleted AS LOGIC
lCentury := SetCentury()
lDeleted := SetDeleted()

SET BELL ON 
SET CENTURY OFF
SET CONSOLE ON
SET DELETED ON

xAssert( SetDeleted() )
xAssert( .not. SetCentury() )

Set(Set.Bell, "ON" )
Set(Set.Century, "ON" )
Set(Set.Console, "ON" )
Set(Set.Deleted, "OFF" )

xAssert( .not. SetDeleted() )
xAssert( SetCentury() )

SetCentury(lCentury)
SetDeleted(lDeleted)

PROC xAssert(l AS LOGIC) AS VOID
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

