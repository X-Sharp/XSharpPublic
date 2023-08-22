// R 858 Usual -> Enum that is not an Integer Enum
// https://github.com/X-Sharp/XSharpPublic/issues/1069
public enum MyTwoValues
   First := 1
   Second := 2
end enum

public enum MyTwoValuesDword as dword
   First := 1
   Second := 2
end enum
public enum MyTwoValuesInt64 as dword
   First := 1
   Second := 2
end enum

function Start() as void strict
   local UsualVal := 1 as usual
   var EnumVal1 := (MyTwoValues)UsualVal // This works
   var EnumVal2 := (MyTwoValuesDword)UsualVal
   xAssert(EnumVal1 == MyTwoValues.First)
   xAssert(EnumVal2 == MyTwoValuesDword.First)
   UsualVal := 2
   var EnumVal3 := (MyTwoValuesInt64)UsualVal
   xAssert(EnumVal3 == MyTwoValuesInt64.Second)
   return




PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
