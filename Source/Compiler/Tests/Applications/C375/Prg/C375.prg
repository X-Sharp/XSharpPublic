// 375. error XS0266: Cannot implicitly convert type 'Vulcan.__Usual' to 'TestVOStruct*'. An explicit conversion exists (are you missing a cast?)
VOSTRUCT TestVOStruct
MEMBER n AS INT
MEMBER m AS INT
FUNCTION Start() AS VOID
	LOCAL p AS TestVOStruct
	p := MemAlloc(SizeOf(TestVOStruct))
	p:n := 1
	p:m := 2
	? p:n , p:m
	
	LOCAL u AS USUAL
	u := p
	? u

	p := u
	? p
	? p:n , p:m

	// p := __WCMenuList[idx][1]
	LOCAL pPtrPtr AS PTR PTR
	pPtrPtr := u
	? pPtrPtr

	DoSomething(u)
	DoSomethingElse(u)
RETURN

FUNCTION DoSomething(u)
	LOCAL p AS TestVOStruct
	p := u
	? p
	? p:n , p:m
RETURN NIL

FUNCTION DoSomethingElse(p AS TestVOStruct) AS VOID
	? p
	? p:n , p:m

