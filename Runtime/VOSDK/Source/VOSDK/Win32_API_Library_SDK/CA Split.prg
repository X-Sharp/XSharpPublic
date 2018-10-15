_DLL FUNCTION SpltAssocGet(hWnd AS PTR) AS PTR PASCAL:cato3spl.SpltAssocGet

_DLL FUNCTION SpltColorSet(hWnd AS PTR, iColor AS DWORD, cr AS INT) AS INT PASCAL:cato3spl.SpltColorSet

_DLL FUNCTION SpltColorGet(hWnd AS PTR, iColor AS DWORD) AS INT PASCAL:cato3spl.SpltColorGet

_DLL FUNCTION SpltDeferPaint(hWnd AS PTR) AS VOID PASCAL:cato3spl.SpltDeferPaint

_DLL FUNCTION SpltEndDeferPaint(hWnd AS PTR, bUpdate AS LOGIC) AS VOID PASCAL:cato3spl.SpltEndDeferPaint

_DLL FUNCTION SpltAttribSet(hWnd AS PTR, dwAttrib AS DWORD) AS VOID PASCAL:cato3spl.SpltAttribSet

_DLL FUNCTION SpltAttribGet(hWnd AS PTR) AS DWORD PASCAL:cato3spl.SpltAttribGet

_DLL FUNCTION SpltAttribClear(hWnd AS PTR, dwAttrib AS DWORD) AS VOID PASCAL:cato3spl.SpltAttribClear

_DLL FUNCTION SpltStyleSet(hWnd AS PTR, dwStyle AS DWORD) AS VOID PASCAL:cato3spl.SpltStyleSet

_DLL FUNCTION SpltStyleGet(hWnd AS PTR) AS DWORD PASCAL:cato3spl.SpltStyleGet

_DLL FUNCTION SpltStyleClear(hWnd AS PTR, dwStyle AS DWORD) AS VOID PASCAL:cato3spl.SpltStyleClear

_DLL FUNCTION SpltPaneShow(hWnd AS PTR, iPane AS INT, uCode AS DWORD) AS LOGIC PASCAL:cato3spl.SpltPaneShow

_DLL FUNCTION SpltPaneAssocSet(hWnd AS PTR, hWndAssoc AS PTR, iPane AS INT) AS PTR PASCAL:cato3spl.SpltPaneAssocSet

_DLL FUNCTION SpltPaneAssocGet(hWnd AS PTR, iPane AS INT) AS PTR PASCAL:cato3spl.SpltPaneAssocGet

_DLL FUNCTION SpltPaneExtSet(hWnd AS PTR, iPane AS INT, lpExt AS _winSize) AS LOGIC PASCAL:cato3spl.SpltPaneExtSet

_DLL FUNCTION SpltPaneExtGet(hWnd AS PTR, iPane AS INT, lpExt AS _winSize) AS LOGIC PASCAL:cato3spl.SpltPaneExtGet

_DLL FUNCTION SpltPaneOrgGet(hWnd AS PTR, iPane AS INT, lpPos AS _winPoint) AS LOGIC PASCAL:cato3spl.SpltPaneOrgGet

_DLL FUNCTION SpltAssocSet(hWnd AS PTR, hWndAssoc AS PTR) AS PTR PASCAL:cato3spl.SpltAssocSet

_DLL FUNCTION SpltGetVersion() AS WORD PASCAL:cato3spl.SpltGetVersion

_DLL FUNCTION SpltLayout(hWnd AS PTR, nRows AS INT, nCols AS INT) AS LOGIC PASCAL:cato3spl.SpltLayout
	
#region defines
DEFINE CASPLIT_CLASS            := "CA_SplitWindow32"
DEFINE SWS_HALIGN               := 0x00000001
DEFINE SWS_VALIGN               := 0x00000002
DEFINE SWS_TEXTHASPARMS		:= 0x00000004
DEFINE SWS_NOHORZDRAG		:= 0x00000008
DEFINE SWS_NOVERTDRAG		:= 0x00000010
DEFINE SWN_ASSOCIATEGAIN        := 401
DEFINE SWN_ASSOCIATELOSS        := 402
DEFINE SPLTCOLOR_BAR            := 0
DEFINE SPLTCOLOR_BARFRAME       := 1
DEFINE SPLTCOLOR_WINDOW         := 2
DEFINE SPLTCOLOR_3DHIGH         := 3
DEFINE SPLTCOLOR_3DSHADOW       := 4
DEFINE SPS_SHOWPANE             := 1
DEFINE SPS_HIDEPANE             := 2
DEFINE SPS_SHOWROW              := 3
DEFINE SPS_HIDEROW              := 4
DEFINE SPS_SHOWCOLUMN		:= 5
DEFINE SPS_HIDECOLUMN		:= 6
DEFINE SPS_SHOWALLPANES		:= 7
DEFINE SPS_HIDEALLPANES		:= 8	
#endregion
