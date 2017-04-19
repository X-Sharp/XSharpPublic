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
RETURN 0

FUNCTION Start( ) AS VOID
    LOCAL dli AS _winDRAGLISTINFO
    dli.hWnd := NULL_PTR
	LBItemFromPt(dli.hWnd, dli.ptCursor, TRUE)
	LBItemFromPt(dli:hWnd, dli:ptCursor, TRUE)
RETURN

