#region defines
DEFINE WRM_NTFY_MSG_BASE 	:=	6500
DEFINE MAX_RCC_FONT := 2	// This must always be the number
DEFINE MAX_RCC_COLOR := 12	// This must always be the number of color ids we use
DEFINE CAWRM_NTFY_CLOSE := WM_USER+1
// Notification that the wrapper window has closed, where:
// wParam == ?; lParam == CAWRMHANDLE report wrapper handle
DEFINE CAWRM_NTFY_NEW_WRAP := WM_USER+2
// Notification that a new wrapper was created, where:
// wParam == ?; lParam == CAWRMHANDLE report wrapper handle
DEFINE CAWRM_NTFY_COMMAND := WM_USER+3
// Notification that an application defined menu command was selected, where:
// wParam == Menu Command; lParam == CAWRMHANDLE report wrapper handle
DEFINE CAWRM_NTFY_SET_MENU := WM_USER+4  // app set menu state
// Notification that menu command state has changed, where:
// wParam == hMenu; lParam == CAWRMHANDLE report wrapper handle
DEFINE CAWRM_NTFY_ADVISE := WM_USER+5	
// Notification that the value FOR an advise has changed, where:
// wParam == 0; lParam == LPCAWRMADVISENTFY
DEFINE CAWRM_NTFY_DEPENDENCY := WM_USER+6
// Notification OF a dependency where:
// wParam == 0; lParam == lpCAWRMDEPENDENCYNTFY
DEFINE CAWRM_NTFY_PREVIEW := WM_USER+7
// Notification OF a preview command where:
// wParam == 0; lParam == lpCAWRMPREVIEWNTFY
DEFINE CAWRM_NTFY_ADVISE_CLEAR := WM_USER+9
// Notification that the report an advise was on was closed so the
// advise IS being cleared automatically.  DO not CALL WrmClearAdvise
// after getting this message. where:
// wParam == 0; lParam == LPCAWRMADVISENTFY
DEFINE CAWRM_NTFY_ABOUT := WM_USER+10
// Notification that About was selected, where:
// wParam == NULL; lParam == WrmGetVersion information
DEFINE CAWRM_NTFY_EDIT_QUERY := WM_USER+8
// Notification OF an edit query command where:
// wParam == 0;	lParam == lpCAWRMEDITQUERYNTFY
DEFINE CAWRM_NTFY_HELP := WM_USER+11
// Notification that help was selected FOR an application defined
// menu command was selected, where:
// wParam == Menu Command; lParam == CAWRMHANDLE report wrapper handle
DEFINE CAWRM_NTFY_SAVE := WM_USER+12
// Notification that a report was saved where:
// wParam == 0; lParam == LPCAWRMSAVENTFY
DEFINE WRM_NTFY_ADVISE_CLEAR:=	(WRM_NTFY_MSG_BASE+CAWRM_NTFY_ADVISE_CLEAR)
DEFINE TP_MENU_BASE := 100 // TP menu base number
DEFINE NTFY_MSG_BASE := 2000
DEFINE DBA_MENU_BASE := (TP_MENU_BASE +1600)
DEFINE QQ_MENU_BASE := (TP_MENU_BASE +400)
DEFINE RET_REPORT_OPEN := 1
DEFINE RET_FILE_NAME := 1
DEFINE CAWRMWRAPINFO_WRMTYPE := 2 // LoWord( wrm type ) whether definer, viewer etc
DEFINE REPORTCLOSEEVENT  := 301
DEFINE REPORTCOMPLETEERROREVENT  := 305 // introduced in CA-RET 2.0
DEFINE REPORTCOMPLETEEVENT  := 302
DEFINE REPORTFILESAVEEVENT  := 306   // undocumented advice from CA-RET 1.0
DEFINE REPORTOPENEVENT  := 300
DEFINE REPORTSERVERCLOSEEVENT  := 303
DEFINE REPORTVIEWCLOSEEVENT  := 304
DEFINE RET_RPTEVT_REPORT_OPENED := 1 // is this useful? sent at end of Load
DEFINE RET_RPTEVT_PRINTER_CANCELED := 2
DEFINE RET_RPTEVT_REPORT_COMPLETE := 3
DEFINE RET_RPTEVT_REPORT_CLOSE := 4
DEFINE RET_RPTEVT_FILE_SAVE := 5
DEFINE RET_RPTEVT_VIEW_CLOSE := 6
DEFINE RET_REPORT_CREATE := 0
DEFINE RPTSTYLE_TABULAR := 2
DEFINE RET_QUERY_CQMSQLSTRING := 4
DEFINE RPTSTYLE_FREESTYLE := 1
DEFINE RPTSTYLE_FORM := 3
DEFINE RPTSTYLE_LABEL := 4
DEFINE RPTSTYLE_LETTER := 5
DEFINE RPTSTYLE_CROSSTAB := 6
DEFINE RPTSTYLE_NOTSPECIFIED := 0
DEFINE FILE_NEW := 1 // create new .RET
DEFINE FILE_OPEN := 2 // open the .ret file in a HIDDEN definer window
DEFINE WRM_NTFY_CLOSE		:=	(WRM_NTFY_MSG_BASE+CAWRM_NTFY_CLOSE)
DEFINE WRM_NTFY_NEW_WRAP	:=	(WRM_NTFY_MSG_BASE+CAWRM_NTFY_NEW_WRAP)
DEFINE WRM_NTFY_COMMAND		:=	(WRM_NTFY_MSG_BASE+CAWRM_NTFY_COMMAND)
DEFINE WRM_NTFY_SET_MENU	:=	(WRM_NTFY_MSG_BASE+CAWRM_NTFY_SET_MENU)
DEFINE WRM_NTFY_ADVISE		:=	(WRM_NTFY_MSG_BASE+CAWRM_NTFY_ADVISE)
DEFINE WRM_NTFY_DEPENDENCY	:=	(WRM_NTFY_MSG_BASE+CAWRM_NTFY_DEPENDENCY)
DEFINE WRM_NTFY_PREVIEW		:=	(WRM_NTFY_MSG_BASE+CAWRM_NTFY_PREVIEW)
DEFINE WRM_NTFY_EDIT_QUERY	:=	(WRM_NTFY_MSG_BASE+CAWRM_NTFY_EDIT_QUERY)
DEFINE WRM_NTFY_ABOUT		:=	(WRM_NTFY_MSG_BASE +CAWRM_NTFY_ABOUT)
DEFINE WRM_NTFY_HELP		:=	(WRM_NTFY_MSG_BASE +CAWRM_NTFY_HELP)
DEFINE WRM_NTFY_SAVE		:=	(WRM_NTFY_MSG_BASE +CAWRM_NTFY_SAVE)
#endregion
