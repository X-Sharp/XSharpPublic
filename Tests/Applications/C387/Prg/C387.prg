// 387. Some runtime issues with numeric conversions
// also error XS0266: Cannot implicitly convert type 'int64' to 'dword'. without /vo4 (vulcan does not require it)
FUNCTION Start() AS VOID
LOCAL wParam AS DWORD
wParam := 4287102976
? wParam , AsHexString(wParam) // 0xFF880000
? HiWord(wParam) , AsHexString(HiWord(wParam))  // 0x0000FF88
? SHORTINT(_CAST, HiWord(wParam)) , AsHexString((SHORTINT(_CAST, HiWord(wParam)))) // -120, 0xFFFFFF88
? SHORTINT(_CAST, HiWord(wParam)) < 0 // TRUE

? SHORTINT(HiWord(wParam)) , AsHexString((SHORTINT(HiWord(wParam)))) // -120, 0xFFFFFF88
? SHORTINT(HiWord(wParam)) < 0 // TRUE

xAssert(wParam == 0xFF880000) // ok
xAssert(AsHexString(wParam) == "FF880000") // ok

xAssert(HiWord(wParam) == 0x0000FF88) // ok
xAssert(AsHexString(HiWord(wParam)) == "0000FF88") // ok

xAssert(SHORTINT(_CAST, HiWord(wParam)) == -120) // error
xAssert(AsHexString((SHORTINT(_CAST, HiWord(wParam)))) == "FFFFFF88") // ok
xAssert(SHORTINT(HiWord(wParam)) == -120) // error
xAssert(AsHexString((SHORTINT(HiWord(wParam)))) == "FFFFFF88") // ok

xAssert(SHORTINT(_CAST, HiWord(wParam)) < 0) // error
xAssert(SHORTINT(HiWord(wParam)) < 0) // error

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF

