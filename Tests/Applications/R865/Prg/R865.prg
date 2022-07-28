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
	uValue := DateTime.Now
	dValue := uValue
	dtValue := uValue
	wdValue := uValue
	? dValue, dtValue, wdValue
	uValue := ToDay()
	dValue := uValue
	dtValue := uValue
	? dValue
	? dtValue
    local r8Value as real8
    local r4Value as real4
    local wValue as word
    local siValue as short
    local dwValue as DWORD
    local liValue as long
    local curValue as Currency
    local decValue as Decimal
    LOCAL enumValue as TestEnum
    uValue := 10
    r8value := uValue
    r4value := uValue
    wvalue := uValue
    sivalue := uValue
    dwValue := uValue
    liValue := uValue
    decValue := uValue
    curValue := uValue
    enumValue := uValue
    ? r8value, r4value, wvalue, sivalue, dwValue, liValue
    ? decValue, curValue, enumValue:ToString()
    uValue := "123"
    local binValue as Binary
    local strValue as string
    local symValue as Symbol
    local pszValue as Psz
    binValue := uValue
    strValue := uValue
    symValue := uValue
    pszValue := uValue
    ? binValue, strValue, symValue, pszValue
    uValue := #abc
    strValue := uValue
    symValue := uValue
    ? strValue, symValue, pszValue
    uValue :=  String2psz("pszValue")
    strValue := uValue
    symValue := uValue
    pszValue := uValue
    ? strValue, symValue, pszValue

    local ptrValue as Ptr
    local i64Value as Int64
    ptrValue := MemAlloc(10)
    uValue := ptrValue
    ptrValue := uValue
    i64Value := uValue
    ? ptrValue, i64Value:ToString("X")

    uValue := CFoo{}
    local ifValue as IFoo
    ifValue := uValue
    ? ifValue:ToString()
    ? ifValue:Name

	LOCAL wbValue as __WInBool
	LOCAL logValue as LOGIC
	uValue := TRUE
	wbValue := uValue
	logValue := uValue
	? wbValue, logValue
	uValue := FALSE
	wbValue := uValue
	logValue := uValue
	? wbValue, logValue

	local strucTest IS TestStruct
    strucTest:First := 42
    uValue := @strucTest
    ptrValue := uValue
    ? ptrValue
    local strucTest2 AS TestStruct
    strucTest2 := ptrValue
    ? strucTest2
    ? strucTest2:First
    strucTest2 := uValue
     ? strucTest2
    ? strucTest2:First


    catch e as Exception
        ? e:ToString()
    end try
RETURN


ENUM TestEnum
    MEMBER Ten := 10
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
