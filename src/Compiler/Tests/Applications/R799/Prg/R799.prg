// https://github.com/X-Sharp/XSharpPublic/issues/773
// The X# runtime now has a new type __WinDate
// The compiler compiles dates inside VOStruct and Union to this type
// the values inside this type represent the Julian Number, just like in VO.
// When compiling with the Vulcan runtime we replace the values in the structure
// with a UInt32. Vulcan does not have a __WinDate  and the __VoDate is 8 bytes                                                                              

#pragma warnings (170, off)     // warning XS0170: Use of possibly unassigned field 'xx'
FUNCTION Start( ) AS VOID
    LOCAL dTest IS mystruct        
    LOCAL uTest IS myunion
    LOCAL p AS INT PTR
    xAssert(sizeof(mystruct) == 16)
    xAssert(sizeof(myunion) == 4)
#ifdef __XSHARP_RT__   
    dTest.d1 := 1901.01.01 
    dTest.d2 := STOD("00010101")
    dTest.d3 := NULL_DATE
    dTest.d4 := ToDay()
#else
    dTest.d1 := (DWORD) 1901.01.01 
    dTest.d2 := (DWORD) STOD("00010101")
    dTest.d3 := (DWORD) NULL_DATE
    dTest.d4 := (DWORD) ToDay()
#endif    
    p := (INT PTR) @dTest
    XAssert( p[1] == 2415386)
    XAssert( p[2] == 2451911)
#ifdef __XSHARP_RT__   
    XAssert(  p[3] == 0)        // Vulcan returns an incorrect value for (DWORD) NULL_DATE
#endif    
    XAssert(  p[4] == (DWORD) ToDay())
#ifdef __XSHARP_RT__       
    uTest.d1 :=  1901.01.01 
#else
    uTest.d1 :=  (DWORD) 1901.01.01 
#endif    
    XAssert(uTest.l1 == 2415386)
RETURN


VOSTRUCT mystruct
    MEMBER d1 AS DATE
    MEMBER d2 AS DATE
    MEMBER d3 AS DATE
    MEMBER d4 AS DATE    


UNION myUnion
    MEMBER d1 AS DATE
    MEMBER l1 AS LONG
    
    
    
PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN    
