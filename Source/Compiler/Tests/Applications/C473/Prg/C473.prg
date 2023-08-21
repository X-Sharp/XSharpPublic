// 473. error XS1503: Argument 2: cannot convert from '_winPOINT' to '_winPOINT*'
// taken from the VO SDK
VOSTRUCT _winPOINT
	MEMBER x AS LONG
	MEMBER y AS LONG
VOSTRUCT _winDRAGLISTINFO  ALIGN 1
	MEMBER uNotification AS DWORD
	MEMBER hWnd AS PTR
	MEMBER ptCursor IS _winPOINT

FUNCTION LBItemFromPt(hLB AS PTR, pt AS _winPOINT, bAutoScroll AS LOGIC) AS INT PASCAL
	? hLB
	? pt:x, pt:y
	? bAutoScroll
RETURN 0

FUNCTION Start( ) AS VOID
    LOCAL dli AS _winDRAGLISTINFO
    dli := MemAlloc(_sizeof(_winDRAGLISTINFO))
    
    dli.hWnd := NULL_PTR
    // This works in VO, Vulcan, X#     
    ? "With @ sign"
	LBItemFromPt(dli.hWnd, @dli.ptCursor, TRUE)
    // This only compiles in VO but crashes at runtime 
    // Inside the LbItemFromPt method because a NULL is passed
    ? "Without @ sign"
	LBItemFromPt(dli.hWnd, dli.ptCursor, TRUE)
    MemFree(dli)
	wait
RETURN 

