// 815. Various issues with the @ operator
// https://github.com/X-Sharp/XSharpPublic/issues/810
// error XS9109: Argument xxx may not be passed with the '@' prefix

// Snippets taken as is from customers' code, I have intentionally not "fixed" them
#pragma warnings(165, off) // unasigned local
#pragma warnings(219, off) // assigned but not used
#pragma warnings(9071, off) // out expected
FUNCTION Start() AS VOID
    NoRunTest3()
    NoRunTest5()
    NoRunTest6()
    NoRunTest7()
    NoRunTest8()
    NoRunTest9()
    NoRunTest10()
    NoRunTest11()
RETURN

PROCEDURE NoRunTest1()
	LOCAL nHandle := NULL_PTR	AS IntPtr
	LOCAL sInstall	IS ADS_MGMT_INSTALL_INFO
	LOCAL nInstall	AS WORD
	AdsMgGetInstallInfo(nHandle,@sInstall,@nInstall)

PROCEDURE NoRunTest2()
	LOCAL DIM buf[ 123 ] 	AS BYTE
	IF ! ( LoadString( _GetInst(), 123, @buf, 456) == 0 )
		AppendMenu( NULL_PTR, 123, 456, @buf )
	ENDIF

PROCEDURE NoRunTest3()
	LOCAL DOCINFO IS _WINDOCINFO
	MemSet(@DOCINFO, 255, _SIZEOF(_WINDOCINFO))
	MemSet(@DOCINFO, 0, 2)

	LOCAL p AS BYTE PTR
	p := @DOCINFO
	? p[1]
	? p[2]
	? p[3]
	xAssert(p[1] == 0)
	xAssert(p[2] == 0)
	xAssert(p[3] == 255)
	xAssert(p[4] == 255)


PROCEDURE NoRunTest4()
	LOCAL bi      IS _winBITMAPINFOHEADER
	LOCAL lpbi    AS _winBITMAPINFOHEADER
	MemCopy(lpbi, @bi, _SIZEOF(_winBITMAPINFOHEADER))

PROCEDURE NoRunTest5()
	LOCAL strucCharFormat IS _winCHARFORMAT
	LOCAL cFaceName := "buffer" AS STRING
	MemCopy(@strucCharFormat:szFaceName[1], String2Psz(cFaceName), SLen(cFaceName))
	? Psz2String(strucCharFormat:szFaceName)
	xAssert(Psz2String(strucCharFormat:szFaceName) == "buffer")

PROCEDURE NoRunTest6()
	LOCAL DIM firstDay[2] AS BYTE
	GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IFIRSTDAYOFWEEK, @firstDay, 2)
	? firstDay[1]
	? firstDay[2]

PROCEDURE NoRunTest7()
	LOCAL DIM abTemp[100]   AS BYTE
	LOCAL cRet AS STRING
	LOCAL i AS INT
	abTemp[1] := 65
	abTemp[2] := 66
	abTemp[3] := 67
	? cRet := Psz2String(@abTemp[1])
	? cRet := Psz2String(@abTemp[2])
	? cRet := Psz2String(@abTemp[3])
	FOR i := 1 TO 3
    	? cRet := Psz2String(@abTemp[i])
	NEXT
	xAssert( Psz2String(@abTemp[1]) == "ABC")
	xAssert( Psz2String(@abTemp[2]) == "BC")
	xAssert( Psz2String(@abTemp[3]) == "C")

PROCEDURE NoRunTest8()
	LOCAL uValue := 1,uTemp := 2 AS USUAL
	bConvertLogicToUsual(uValue, 123, @uTemp)
	xAssert(uTemp == 456)

FUNCTION bConvertLogicToUsual(lValue AS LOGIC, uConvertRule AS USUAL, uValue REF USUAL) AS LOGIC STRICT
	uValue := 456
RETURN FALSE

PROCEDURE NoRunTest9()
	LOCAL nDword AS DWORD
	VoDbSelect( 123,  @nDword )
	VoDbSelect( 456,  @nDword )
	xAssert(nDword == 123)
	nDword := 456
	MyVoDbSelect(123, @nDword)
	? nDword
	xAssert(nDword == 2)

FUNCTION MyVoDbSelect(wNew AS DWORD,wOld OUT USUAL) AS LOGIC
	wOld := 1
RETURN TRUE
FUNCTION MyVoDbSelect(wNew AS DWORD,wOld OUT DWORD ) AS LOGIC
	wOld := 2
RETURN TRUE


// This error here happens only when XSharp.Core.dll is before XSharp.RT.dll in the list of references, doesn't happen if they are entered the other way around
// It's because those two libraries have 2 overloads of the FRead3() function, but should this have any effect?
PROCEDURE NoRunTest10()
	LOCAL pByte AS BYTE
	LOCAL pHandle AS IntPtr
	System.IO.File.WriteAllText("test.txt", "A")
	IF File("test.txt")
    	pHandle := FOpen(FPathName(),FO_READ)
	    FRead3(pHandle, @pByte, 1) // error XS9109: Argument 2 may not be passed with the '@' prefix
        FClose(pHandle)
        xAssert(pByte == Asc("A")  )
	ENDIF
    RETURN


#pragma options("lb", on)
PROCEDURE NoRunTest11()
LOCAL o AS OBJECT
o := TestClass{}
LOCAL rf := 123 AS INT
o:Test(1, @rf)
? rf
xAssert(rf == 456)
rf := 123
Send(o, "Test", 1, @rf)
? rf
xAssert(rf == 456)

CLASS TestClass
	METHOD Test(n AS INT, rf REF INT) AS VOID
		rf := 456
END CLASS


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN



