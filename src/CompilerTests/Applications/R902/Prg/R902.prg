// Single record replace
#COMMAND REPLACE <(F1)> WITH <V1> [, <(FN)> WITH <VN> ]                     ;
      => DbAutoLock(); __FIELDSET(<(F1)>,<V1>) [;__FIELDSET(<(FN)>,<VN>)]; DbAutoUnLock()

// Single record replace with IN clause
#command REPLACE <(f1)> WITH <v1> [, <(fN)> WITH <vN> ]                 ;
         <x:IN,ALIAS> <(a)>                                             ;
         => DbAutoLock(<(a)>), __FieldSetWa(<(a)>, <(f1)>,<v1>) [,__FieldSetWa(<(a)>,<(fN)>,<vN>)], DbAutoUnLock(<(a)>)
#pragma warnings(9043, off) // redefine of runtime functions
FUNCTION Start as VOID
    REPLACE a with "b"
    REPLACE c with "d", e with "f"
    REPLACE a with "b" in X
    REPLACE c with "d", e with "f" in X
    REPLACE g with "h", i with "j", k with "l" in X



FUNCTION DbAutoLock(area) AS LOGIC CLIPPER
    //?  __FUNCTION__, area
    RETURN TRUE
FUNCTION DbAutoUnLock(area) AS LOGIC CLIPPER
    //?  __FUNCTION__, area
    RETURN TRUE

FUNCTION __FieldSetWA(area , name , value ) AS USUAL
    //? __FUNCTION__, area, name, value
    xAssert(name != NIL)
    xAssert(value != NIL)
    RETURN value
FUNCTION __FieldSet(name , value ) AS USUAL
    //? __FUNCTION__, name, value
    xAssert(name != NIL)
    xAssert(value != NIL)
    RETURN value
PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

