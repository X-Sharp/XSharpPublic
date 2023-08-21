// 384. Incorrect runtime casting from VOSTRUCT to LONG
// note that vulcan requires /vo7, whiile x# doesn't
USING System.Runtime.InteropServices

FUNCTION Start() AS VOID
LOCAL p AS SelfPtr
p := MemAlloc(SizeOf(SelfPtr))
? p , LONGINT(p) , LONGINT(_CAST , p)
IF LONGINT(p) != LONGINT(_CAST , p)
	THROW Exception{"LONGINT(p) != LONGINT(_CAST , p)"}
END IF

EventContext{}:DoTest()

VOSTRUCT SelfPtr
	MEMBER ptrSelf AS PTR

CLASS EventContext
	PROTECT ptrSelfPtr AS SelfPtr
	ACCESS SelfPtrStruc 
		RETURN ptrSelfPtr
	CONSTRUCTOR() 
	ptrSelfPtr :=__WCSelfPtrAlloc(SELF)
	METHOD DoTest() AS VOID
		? LONGINT(_CAST,ptrSelfPtr)
		? LONGINT(ptrSelfPtr)
		IF LONGINT(_CAST,ptrSelfPtr) != LONGINT(ptrSelfPtr)
			THROW Exception{"LONGINT(_CAST,ptrSelfPtr) != LONGINT(ptrSelfPtr)"}
		END IF
	RETURN
		
END CLASS

FUNCTION __WCSelfPtrAlloc(oObject AS OBJECT) AS  SelfPtr
	LOCAL strucSelfPtr AS SelfPtr
	strucSelfPtr := MemAlloc(_SizeOf(SelfPtr))
	strucSelfPtr:ptrSelf := GCHandle.ToIntPtr( GCHandle.Alloc( oObject ) )
RETURN strucSelfPtr

