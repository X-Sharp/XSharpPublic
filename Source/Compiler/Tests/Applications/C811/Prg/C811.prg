// 811. Problems with ACopy() function
// https://github.com/X-Sharp/XSharpPublic/issues/815
/*
Below are the results VO gives with certain arguments passed in the function
Most of them currently give different results in X#. 
*/

FUNCTION Start() AS VOID
LOCAL aSrc, aDest AS ARRAY

aSrc := {}
aDest := {}
ACopy(aSrc, aDest, 1, 0, 1)
xAssert(A2S(aDest) == "")

aSrc := { 1,2,3 }
aDest := { 0 }
ACopy(aSrc, aDest, 1, 1, 1)
xAssert(A2S(aDest) == "1")

aSrc := { 1,2,3 }
aDest := { 0 }
ACopy(aSrc, aDest, 1, 2, 1)
xAssert(A2S(aDest) == "1")

aSrc := { 1,2,3 }
aDest := { 0,0 }
ACopy(aSrc, aDest, 1, 1, 1)
xAssert(A2S(aDest) == "10")

aSrc := { 1,2,3 }
aDest := { 0,0 }
ACopy(aSrc, aDest, 1, 2, 1)
xAssert(A2S(aDest) == "12")

aSrc := { 1,2,3 }
aDest := { 0,0,0,0 }
ACopy(aSrc, aDest, 1, 5, 1)
xAssert(A2S(aDest) == "1230")

aSrc := { 1,2,3 }
aDest := { 0 }
ACopy(aSrc, aDest, 2, 5, 3)
xAssert(A2S(aDest) == "2")

aSrc := { 1 }
aDest := { 0,0,0 }
ACopy(aSrc, aDest, 2, 2, 2)
xAssert(A2S(aDest) == "010")
aSrc := { 1,2,3,4 }
aDest := { 0,0 }
ACopy(aSrc, aDest, 2, 3, 1)
? __LINE__
xAssert(A2S(aDest) == "23")
aSrc := { 1,2,3,4 }
aDest := { 0,0,0,0,0 }
ACopy(aSrc, aDest, 2, 3, 3)
? __LINE__ ,A2S(aDest)
xAssert(A2S(aDest) == "00234")
aSrc := { 1,2,3,4 }
aDest := { 0,0,0,0,0 }
ACopy(aSrc, aDest, 2, 3, 2)
? __LINE__ ,A2S(aDest)
xAssert(A2S(aDest) == "02340")
aSrc := { 1,2,3,4 }
aDest := { 0,0 }
ACopy(aSrc, aDest, 2, 3, 2)
? __LINE__
xAssert(A2S(aDest) == "02")

aSrc := { 1,2,3,4 }
aDest := { 0,0,0 }
ACopy(aSrc, aDest, 100, 200, 300)
xAssert(A2S(aDest) == "004")


FUNCTION A2S(a AS ARRAY) AS STRING
STATIC LOCAL sb := System.Text.StringBuilder{} AS System.Text.StringBuilder
sb:Length := 0
FOR LOCAL n := 1 AS INT UPTO ALen(a)
	sb:Append(AsString(a[n]))
NEXT
RETURN sb:ToString()

PROC xAssert(l AS LOGIC) 
IF .NOT. l
//	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
	? "FAILED"
ELSE
	? "Assertion passed"   
END IF
RETURN 		

