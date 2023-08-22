// error XS0221: Constant value '-1' cannot be converted to a 'dword' (use 'unchecked' syntax to override)
// This is allowed in vulcan, to cast to a unsigned value.
// With UNCHECKED() it does compile and run correctly, should we require that in VO/vlcan dialect?

DEFINE ADS_NTS  := ( ( DWORD ) -1 )
DEFINE ADS_NTSw  := ( ( WORD ) -1 )
DEFINE ADS_NTSb  := ( ( BYTE ) -1 )

GLOBAL gADS_NTS  := ( ( DWORD ) -1 ) AS DWORD
GLOBAL gADS_NTSw  := ( ( WORD ) -1 ) AS WORD
GLOBAL gADS_NTSb  := ( ( BYTE ) -1 ) AS BYTE

FUNCTION Start( ) AS VOID
	? ADS_NTS
	? gADS_NTS
	xAssert(ADS_NTS == UInt32.MaxValue)
	xAssert(ADS_NTSw == UInt16.MaxValue)
	xAssert(ADS_NTSb == Byte.MaxValue)

	xAssert(gADS_NTS == UInt32.MaxValue)
	xAssert(gADS_NTSw == UInt16.MaxValue)
	xAssert(gADS_NTSb == Byte.MaxValue)
RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

