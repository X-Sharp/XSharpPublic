// 815. Various issues with the @ operator
// https://github.com/X-Sharp/XSharpPublic/issues/810
// error XS9109: Argument xxx may not be passed with the '@' prefix

// Snippets taken as is from customers' code, I have intentionally not "fixed" them
FUNCTION Start() AS VOID
    //NoRunTest6()
    NoRunTest1()
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
	MEMSET(@DOCINFO, 0, _SIZEOF(_WINDOCINFO))
	
	
PROCEDURE NoRunTest4()
	LOCAL bi      IS _winBITMAPINFOHEADER
	LOCAL lpbi    AS _winBITMAPINFOHEADER
	MemCopy(lpbi, @bi, _SIZEOF(_winBITMAPINFOHEADER))

PROCEDURE NoRunTest5()
	LOCAL strucCharFormat IS _winCHARFORMAT
	LOCAL cFaceName := "" AS STRING
	MemCopy(@strucCharFormat:szFaceName[1], PTR(_CAST, cFaceName), SLen(cFaceName)+1)	

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

PROCEDURE NoRunTest8()
	LOCAL uValue := 1,uTemp := 2 AS USUAL
	bConvertLogicToUsual(uValue, 123, @uTemp)
	
FUNCTION bConvertLogicToUsual(lValue AS LOGIC, uConvertRule AS USUAL, uValue REF USUAL) AS LOGIC STRICT
RETURN FALSE

