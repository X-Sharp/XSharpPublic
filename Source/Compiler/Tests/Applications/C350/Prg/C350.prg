// 350. error XS1503: Argument 2: cannot convert from '_winPOINT' to '_winPOINT*'
#pragma warnings(165, off) // uniassigned local
#pragma warnings(162, off) // unreachable code
VOSTRUCT LOCAL_winPOINT
	MEMBER x AS LONGINT
	MEMBER y AS LONGINT

FUNCTION LOCAL_Polygon(hdc AS PTR, lpPoints AS LOCAL_winPOINT, nCOunt AS INT) AS LOGIC PASCAL
	? lpPoints:x , lpPoints:y
RETURN TRUE

FUNCTION Start() AS VOID
	LOCAL DIM aPtIS[2] IS LOCAL_WINPOINT
	LOCAL hDC AS PTR

	aPtIS[1]:x := 1
	aPtIS[1]:y := 2
	aPtIS[2]:x := 3
	aPtIS[2]:y := 4

	LOCAL_Polygon(hDC,@aPtIS,3)
	LOCAL_Polygon(hDC,@aPtIS[1],3)
	LOCAL_Polygon(hDC,@aPtIS[2],3)

	LOCAL DIM aPtAS[2] AS LOCAL_WINPOINT
	aPtAS[1] := MemAlloc(sizeof(LOCAL_WINPOINT))
	aPtAS[2] := MemAlloc(sizeof(LOCAL_WINPOINT))

	aPtAS[1]:x := 1
	aPtAS[1]:y := 2
	aPtAS[2]:x := 3
	aPtAS[2]:y := 4
	?
	LOCAL_Polygon(hDC,aPtAS[1],3)
	LOCAL_Polygon(hDC,aPtAS[2],3)


	IF FALSE
		LOCAL DIM sPtIS[2] IS _WINPOINT
		Polygon(hDC,@sPtIS,3)
		Polygon(hDC,@sPtIS[1],3)
		Polygon(hDC,@sPtIS[2],3)

		LOCAL DIM sPtAS[2] AS _WINPOINT
		Polygon(hDC,sPtAS[1],3)
		Polygon(hDC,sPtAS[2],3)
	ENDIF
RETURN
