#pragma warnings(9068, off)
// test various implicit converters for usual type                                 //
// There should be no calls to Usual.ToObject() in the IL for this code
// With the exception of the assignment to the variable of type iFoo
FUNCTION Start( ) AS VOID
    try
	local uValue as USUAL
	local dValue as Date
	local dtValue as DateTime
	local wdValue AS __WINDATE
	local dtStart as DateTime
	dtStart := DateTime.Now
	uValue := dtStart
	dValue := uValue
	dtValue := uValue
	wdValue := uValue
	xAssert(dValue == ToDay())
	xAssert(dtValue == dtStart)
	xAssert(wdValue == Today())
	uValue := ToDay()
	dValue := uValue
	dtValue := uValue
	xAssert(dValue == ToDay())
	xAssert(dtValue == ToDay())

    local r8Value as real8
    local r4Value as real4
    local wValue as word
    local siValue as short
    local dwValue as DWORD
    local liValue as long
    local curValue as Currency
    local decValue as Decimal
    LOCAL enumValue as TestEnum
    uValue := 42
    r8value := uValue
    r4value := uValue
    wvalue := uValue
    sivalue := uValue
    dwValue := uValue
    liValue := uValue
    decValue := uValue
    curValue := uValue
    enumValue := uValue
    xAssert(r8Value == 42.0)
    xAssert(r4value == 42.0)
    xAssert(wValue == 42)
    xAssert(siValue == 42)
    xAssert(dwValue == 42)
    xAssert(liValue == 42)
    xAssert(decValue == 42m)
    xAssert(curValue == $42)
    xAssert(enumValue == TestEnum.FortyTwo)
    uValue := "A123"
    local binValue as Binary
    local strValue as string
    local symValue as Symbol
    local pszValue as Psz
    binValue := uValue
    strValue := uValue
    symValue := uValue
    pszValue := uValue
    xAssert(binValue == 0h41313233)
    xAssert(strValue == "A123")
    xAssert(symValue == #A123)
    xAssert(Psz2String(pszValue) == "A123")
    uValue := #abc
    strValue := uValue
    symValue := uValue
    xAssert(strValue == "ABC")
    xAssert(symValue == #ABC)

    uValue :=  String2psz("pszValue")
    strValue := uValue
    symValue := uValue
    pszValue := uValue
    xAssert(strValue == "pszValue")
    xAssert(symValue == #pszValue)
    xAssert(Psz2String(pszValue) == "pszValue")

    local ptrValue as Ptr
    local i64Value as Int64
    LOCAL pValue as Ptr
    pValue := MemAlloc(10)
    uValue := pValue
    ptrValue := uValue
    i64Value := uValue
    xAssert(ptrValue == pValue)
    xAssert(i64Value == (int64) pValue)

    uValue := CFoo{}
    local ifValue as IFoo
    ifValue := uValue
    xAssert(ifValue:ToString() == "CFOO:Robert")
    xAssert(ifValue:Name == "Robert")

	LOCAL wbValue as __WInBool
	LOCAL logValue as LOGIC
	uValue := TRUE
	wbValue := uValue
	logValue := uValue
	xAssert(wbValue)
	xAssert(logValue)
	uValue := FALSE
	wbValue := uValue
	logValue := uValue
	xAssert(!wbValue)
	xAssert(!logValue)

	local strucTest IS TestStruct
    strucTest:First := 42
    uValue := @strucTest
    ptrValue := uValue
    xAssert(ptrValue == @strucTest)
    local strucTest2 AS TestStruct
    strucTest2 := ptrValue
    xAssert(strucTest2 == @strucTest)
    xAssert(strucTest2:First == 42)
    strucTest2 := uValue
    xAssert(strucTest2 == @strucTest)
    xAssert(strucTest2:First == 42)


    catch e as Exception
        ? e:ToString()
    end try
RETURN


ENUM TestEnum
    MEMBER Ten := 10
    Member FortyTwo := 42
END ENUM

INTERFACE IFoo
    PROPERTY Name as STRING GET

END INTERFACE
CLASS CFoo implements IFoo
    PROPERTY Name as STRING AUTO GET PRIVATE SET
    CONSTRUCTOR()
        Name := "Robert"
    RETURN
    OVERRIDE METHOD ToString() AS STRING
        RETURN ClassName(SELF) + ":" + Name

END CLASS


VOSTRUCT TestStruct
    MEMBER First as LONG



PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN
