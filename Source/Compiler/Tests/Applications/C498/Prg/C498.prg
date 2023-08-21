// 498. error XS0106: The modifier 'static' is not valid for this item
// Should we manually change this to INTERNAL VOSTRUCT in the code, or should the compiler accept this?
STATIC VOSTRUCT MenuBar
   MEMBER hHook       AS PTR
   MEMBER hToolbar    AS PTR
   MEMBER iButtonID   AS INT
   MEMBER hMenu       AS PTR
   MEMBER dwMenuExit  AS DWORD
   MEMBER lLeftAvail  AS LOGIC
   MEMBER lRightAvail AS LOGIC
   MEMBER iBtnIndex   AS INT
   MEMBER hHotMenuBar AS PTR

FUNCTION Start( ) AS VOID
	LOCAL s IS MenuBar
	s:iButtonID := 1
RETURN
