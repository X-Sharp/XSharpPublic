// 899. Various UDCs
// https://github.com/X-Sharp/XSharpPublic/issues/1407
FUNCTION Start( ) AS VOID

clear screen

SET MEMOBLOCKSIZE TO 123
SET MEMOBLOCK TO 123

set driver TO abc
? RddSetDefault()
xAssert(RddSetDefault() == "abc")

set OPTIMIZE off
set EXACT off
xAssert(SetExact() == FALSE)

SET DELETED ON
xAssert(SetDeleted() == TRUE)

set fixed off
? SetFixed()
xAssert(SetFixed() == FALSE)

set decimals TO 1
xAssert(SetDecimal() == 1)

set DIGITS TO 12
xAssert(SetDigit() == 12)
SET DIGITS TO               
xAssert(SetDigit() == 10)

set scope TO
set scope TO 1
set scope TO 1,2
set scopebottom TO 1
                  
set INTERNATIONAL TO WINDOWS
? SetInternational()
xAssert(SetInternational() == "WINDOWS")

set COLLATION TO WINDOWS
? SetCollation()
xAssert(SetCollation() == "WINDOWS")

RETURN

PROCEDURE Othertests()
copy file c:\test\aaa.txt TO "c:\test\abc.txt"
erase "c:\test\abc.txt"

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
RETURN
