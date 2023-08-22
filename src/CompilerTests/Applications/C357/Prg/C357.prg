// 357. error XS1503: Argument 1: cannot convert from 'void*' to 'string'
#pragma warnings(165, off) // uniassigned local
#pragma warnings(219, off) // assigned but not used
FUNCTION Start() AS VOID
LOCAL ptrBuffer AS PTR
LOCAL p AS PSZ
? PSZ( _CAST, ptrBuffer )
? PSZ(_CAST, -1L)
p := PSZ( _CAST, ptrBuffer )
p := PSZ(_CAST, -1L)

LOCAL pMsg AS PSZ PTR
pMsg := MemAlloc(123)
pMsg[1] := String2Psz("Robert")
? Psz2String( PSZ(_CAST , PTR(_CAST,pMsg) ) )

FUNCTION GetSystemMessage(dwId AS DWORD) AS STRING
	LOCAL pMsg AS PSZ PTR
	LOCAL cReturn AS STRING

	pMsg := MemAlloc(512)
	cReturn := Psz2String(PSZ(_CAST,PTR(_CAST,pMsg)))
	MemFree(pMsg)

	RETURN cReturn

