// 383. Incorrect runtime PTR conversions
FUNCTION Start() AS VOID

LOCAL oResId AS ResourceID
oResId := ResourceID{}

? oResId:Address()
? PTR(oResId:Address())
? PTR(_CAST, oResId:Address())

IF AsString(oResId:Address()) != AsString(PTR(oResId:Address()))
	THROW Exception{"oResId:Address()   !=    PTR(oResId:Address())"}
END IF
IF AsString(PTR(oResId:Address())) != AsString(PTR(_CAST, oResId:Address()))
	THROW Exception{"PTR(oResId:Address())   !=    PTR(_CAST, oResId:Address())"}
END IF

PtrFunc(oResId:Address())
PtrFunc(PTR(oResId:Address()))
PtrFunc(PTR(_CAST, oResId:Address()))

LOCAL p,pPtr,pPtrCast AS PTR
p := oResId:Address()
pPtr := PTR(oResId:Address())
pPtrCast := PTR(_CAST , oResId:Address())

IF p != pPtr
	THROW Exception{"oResId:Address()  !=  PTR(oResId:Address())" + AsString(p) + " / "  + AsString(pPtr)}
END IF
IF pPtr != pPtrCast
	THROW Exception{"PTR(oResId:Address())  !=  PTR(_CAST , oResId:Address()) " + AsString(pPtr) + " / "  + AsString(pPtrCast)}
END IF

RETURN

FUNCTION PtrFunc(p AS PTR) AS VOID
STATIC pTest := NULL_PTR AS PTR
IF pTest == NULL_PTR
	pTest := p
END IF
IF p != pTest .or. pTest != p .or. !(p == pTest)
	THROW Exception{"Incorrect conversion usual to pointer"}
END IF
RETURN


CLASS ResourceID
PROTECT sID := "test" AS STRING
PROTECT _lpAddress AS PSZ
CONSTRUCTOR()
SELF:_lpAddress := StringAlloc(sID)
? SELF:_lpAddress
? sID

METHOD Address() CLIPPER
LOCAL lpAddress := SELF:_lpAddress AS PTR
RETURN lpAddress
END CLASS

