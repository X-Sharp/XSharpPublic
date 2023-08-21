// 427. Runtime problem accessing VOSTRUT LOGIC members with LOGIC PTR
// note that this is used in the SDK, see CLASS __FormattedString below
FUNCTION Start() AS VOID
LOCAL DIM a[5] AS LOGIC
LOCAL p AS LOGIC PTR
LOCAL s IS __struct
LOCAL d AS DWORD

p := (LOGIC PTR) @a
LOGIC(p + 1) := TRUE
? "DIM ARRAY:" , a[1] , a[2] , a[3] , a[4] , a[5]

// Update: cannot make LOGIC PTR handle both LOGIC DIM ARRAYs and LOGICs in VOSTRUCT the same
// way as VO does, so the following will fail also in x#, same as with vulcan:
/*
xAssert(a[2]) // warning, vulcan fails on that, it sets a[5] to TRUE instead. x# works corerctly
d := 3
LOGIC(p + d) := TRUE
xAssert(a[4]) // vulcan wrong here, too. x# is ok
?
*/

p := (LOGIC PTR) @s
LOGIC(p + 1) := TRUE
? "VOSTRUCT: " ,  s:m1 , s:m2 , s:m3 , s:m4 , s:m5

#error exception here
xAssert(s:m2)
d := 3
LOGIC(p + d) := TRUE
xAssert(s:m4)


// original report:
LOCAL o AS __FormattedString
LOCAL cPic AS STRING
cPic := "@! XXXXX" // uppercase
?
? "picture: " + cPic + " :"
o := __FormattedString{}
o:Picture := cPic 
? o:FuncFlags:lLeftJust , o:FuncFlags:lDispCR , o:FuncFlags:lSetDate  , o:FuncFlags:lBritDate , o:FuncFlags:lInsNonTemp 
? o:FuncFlags:lDispDB , o:FuncFlags:lZeroBlank  , o:FuncFlags:lNegInParen , o:FuncFlags:lConvUpper , o:FuncFlags:lAlphaOnly

xAssert(o:FuncFlags:lConvUpper)
o:Picture := "@D( XXXXX" // neg paren
xAssert(o:FuncFlags:lNegInParen)
o:Picture := "@B XXXXX" // left justified
xAssert(o:FuncFlags:lLeftJust)

RETURN

VOSTRUCT __struct
MEMBER m1 AS LOGIC
MEMBER m2 AS LOGIC
MEMBER m3 AS LOGIC
MEMBER m4 AS LOGIC
MEMBER m5 AS LOGIC


CLASS __FormattedString
	EXPORT FuncFlags IS strucPictureFuncFlags
	PROTECT sPicture AS STRING
ASSIGN Picture(cNewPicture AS STRING)  STRICT 
	LOCAL pLogic AS LOGIC PTR
	LOCAL iSpcPos, iFuncPos, i AS DWORD
	LOCAL sFuncChar AS STRING
	LOCAL iAsc, iAscD, iAscM, iAscY, iAsc9 AS DWORD

	sPicture := cNewPicture
	pLogic := (LOGIC PTR) @FuncFlags
	// handle functions
	IF (Left(sPicture, 1) == "@")
		iSpcPos := At2(" ", sPicture)
		IF (iSpcPos == 0)
			iSpcPos :=  SLen(sPicture) + 1
		ENDIF
		FOR i := 2 TO (iSpcPos-1)
			sFuncChar := CharPos(sPicture, i)
			iFuncPos := At2(sFuncChar, "BCDERXZ(!A")
			IF (iFuncPos > 0)
				? "index (zero based)" , iFuncPos - 1
				LOGIC(pLogic + iFuncPos - 1) := TRUE
			ENDIF
		NEXT
	ENDIF
END CLASS

VOSTRUCT strucPictureFuncFlags
	MEMBER lLeftJust AS LOGIC
	MEMBER lDispCR AS LOGIC
	MEMBER lSetDate AS LOGIC
	MEMBER lBritDate AS LOGIC
	MEMBER lInsNonTemp AS LOGIC
	MEMBER lDispDB AS LOGIC
	MEMBER lZeroBlank AS LOGIC
	MEMBER lNegInParen AS LOGIC
	MEMBER lConvUpper AS LOGIC
	MEMBER lAlphaOnly AS LOGIC



PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

