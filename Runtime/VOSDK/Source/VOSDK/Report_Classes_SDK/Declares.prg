_DLL FUNC RccInitialize(lprRccInit AS CARCCINIT) AS DWORD PASCAL:CAQR3RCC.RccInitialize
_DLL FUNC DbaInitialize(lprDbaInit AS CADBAINIT) AS DWORD PASCAL:CAQR3DBA.DbaInitialize
_DLL FUNC WpInitialize(lprWpInit AS CAWPINIT) AS DWORD PASCAL:CAQR3WUT.WpInitialize
_DLL FUNC CqmConnect(hCqmHandle AS DWORD, lpdb_init AS CACQMDBINIT) AS LONG PASCAL:CAQR3CQM.CqmConnect
_DLL FUNC CqmListDbDlg(hCqmHandle AS DWORD, lpdb_init AS CACQMDBINIT) AS LONG PASCAL:CAQR3CQM.CqmListDbDlg
_DLL FUNC CqmTerminate(hCqmHandle AS DWORD) AS LONG PASCAL:CAQR3CQM.CqmTerminate
_DLL FUNC WqmInitialize(hWpHandle AS DWORD, lprWqmInit AS CAWQMINIT) AS LONG PASCAL:CAQR3WQM.WqmInitialize
_DLL FUNC WbmInitialize(hWpHandle AS DWORD, lprWbmInit AS CAWBMINIT) AS LONG PASCAL:CAQR3WBM.WbmInitialize
_DLL FUNC WrmTerminate(hWpHandle AS DWORD) AS LONG PASCAL:CAQR3WRM.WrmTerminate
_DLL FUNC WbmTerminate(hWpHandle AS DWORD) AS LONG PASCAL:CAQR3WBM.WbmTerminate
_DLL FUNC WqmTerminate(hWpHandle AS DWORD) AS LONG PASCAL:CAQR3WQM.WqmTerminate
_DLL FUNC WpTerminate(hWpHandle AS DWORD) AS LONG PASCAL:CAQR3WUT.WpTerminate
_DLL FUNC RccTerminate(hRccHandle AS DWORD) AS LONG PASCAL:CAQR3RCC.RccTerminate
_DLL FUNC DbaTerminate(hDbaHandle AS DWORD) AS LONG PASCAL:CAQR3DBA.DbaTerminate
_DLL FUNC CqmSetDefaultDatabase(hCqmHandle AS DWORD, hDbConn AS PTR) AS LONG PASCAL:CAQR3CQM.CqmSetDefaultDatabase
_DLL FUNC CqmInitialize(lprCqmInit AS CACQMINIT) AS DWORD PASCAL:CAQR3CQM.CqmInitialize
_DLL FUNC WrmCreateDefinerWindow(hWpHandle AS DWORD, lprWrmDefInit AS CAWRMCREATEDEFINIT) AS LONG PASCAL:CAQR3WRM.WrmCreateDefinerWindow
_DLL FUNC WrmCreateViewerWindow(hWpHandle AS DWORD, lprWrmVwInit AS CAWRMCREATEVWINIT) AS LONG PASCAL:CAQR3WRM.WrmCreateViewerWindow
_DLL FUNC WrmInitialize(hWpHandle AS DWORD, lprWrmInit AS CAWRMINIT) AS LONG PASCAL:CAQR3WRM.WrmInitialize
_DLL FUNC WrmExecute(hWpHandle AS DWORD, lpstrTopic AS PSZ, lpstrCommand AS PSZ) AS LONG PASCAL:CAQR3WRM.WrmExecute
_DLL FUNC WrmSetAdvise(hCaWpHndl AS DWORD, lpstrTopic AS PSZ, lpstrItem AS PSZ,	hNtfWindow AS PTR, wNtfMessage AS WORD, dwUserData AS DWORD) AS DWORD PASCAL:CAQR3WRM.WrmSetAdvise
_DLL FUNC WrmSetParameter(hCaWpHndl AS DWORD, i AS LONG, pval AS PSZ) AS DWORD PASCAL:CAQR3WRM.WrmSetParameter
_DLL FUNC WrmMinimize(hWrmHandle AS DWORD) AS LONG PASCAL:CAQR3WRM.WrmMinimize
_DLL FUNC WrmMaximize(hWrmHandle AS DWORD) AS LONG PASCAL:CAQR3WRM.WrmMaximize
_DLL FUNC WrmSize(hWrmHandle AS DWORD, x AS LONG, y AS LONG) AS LONG PASCAL:CAQR3WRM.WrmSize
_DLL FUNC WrmRestore(hWrmHandle AS DWORD) AS LONG PASCAL:CAQR3WRM.WrmRestore
_DLL FUNC WrmMove(hWrmHandle AS DWORD, x AS LONG, y AS LONG) AS LONG PASCAL:CAQR3WRM.WrmMove
_DLL FUNC WrmShow(hWrmHandle AS DWORD, bShow AS LOGIC) AS LONG PASCAL:CAQR3WRM.WrmShow
_DLL FUNC CqmDisconnect(hCqmHandle AS DWORD, hDbConn AS PTR) AS LONG PASCAL:CAQR3CQM.CqmDisconnect
_DLL FUNC WpCheckMessage(hWpHandle AS DWORD, hWnd AS PTR, hCqmHandle AS DWORD, hAccel AS PTR, lpMsg AS PTR) AS LONG PASCAL:CAQR3WUT.WpCheckMessage
