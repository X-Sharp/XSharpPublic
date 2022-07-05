// 402. error XS0266: Cannot implicitly convert type 'Vulcan.__VOFloat' to 'int'. An explicit conversion exists (are you missing a cast?)
// in vulcan this compiles without any compiler options enabled
#pragma options("vo4", on)
FUNCTION Start() AS VOID
LOCAL nPaperLength AS INT
LOCAL nDevicePixelsPerInchY := 10 AS INT
LOCAL AbsHeight := 1234.5 AS USUAL
nPaperLength := AbsHeight/FLOAT(nDevicePixelsPerInchY)
? nPaperLength
IF nPaperLength != 123
	THROW Exception{"Incorrect result"}
END IF
