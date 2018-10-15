VOSTRUCT bdCACQMINIT ALIGN 1
//	bdThreeD		bit 1
//	bdDebugSql
//	bdNoQQ
//	bdNoEDT
//	bdNoSQI

//	bdRamis
//	bdEZtrieve
//	bdHost			bit 8
	MEMBER bddwFlags AS DWORD

VOSTRUCT bdCACQMQRYPROPS ALIGN 1
//	bdNoStatusBar	bit 1
//	bdNoToolBar
//	bdOem			bit 3
	MEMBER bddwFlags AS DWORD

VOSTRUCT CAWBMINIT ALIGN 1
	// inputs
	MEMBER lpAppDefRslt AS CAWPAPPDEF // Result application defined info

	MEMBER DIM dwXFutureUse[10] AS DWORD
	MEMBER wNumExts AS WORD
	MEMBER dwExtension AS DWORD
	MEMBER BD IS bdCAWBMINIT
	
VOSTRUCT CADBAINIT ALIGN 1
	// inputs
	MEMBER hDbaInitAppWnd AS PTR

	MEMBER wMessageBase AS WORD

	MEMBER wCommandBase AS WORD
	MEMBER hCaCqmHndl AS DWORD

	MEMBER DIM dwXFutureUse[10] AS DWORD
	MEMBER wNumExts AS WORD
	MEMBER dwExtension AS DWORD
	MEMBER BD IS bdCADBAINIT

VOSTRUCT CACQMDBPROPS ALIGN 1
	// inputs
	MEMBER lpsDbName AS PSZ
	MEMBER lpsUserName AS PSZ
	MEMBER lpsQueryFile AS PSZ

	MEMBER DIM dwXFutureUse[10] AS DWORD
	MEMBER wNumExts AS WORD
	MEMBER dwExtension AS DWORD
  MEMBER sODBCPrompAction AS SHORTINT

	MEMBER BD IS bdCACQMDBPROPS

	// outputs
	MEMBER lpsServerName AS PSZ

VOSTRUCT CACQMDBINIT ALIGN 1
	// inputs
/////////////////////////////////
// workaround for compiler bug that DWORD aligns despite ALIGN 1
//	MEMBER DbProps IS CACQMDBPROPS
	// inputs
	MEMBER lpsDbName AS PSZ
	MEMBER lpsUserName AS PSZ
	MEMBER lpsQueryFile AS PSZ

	MEMBER DIM dwXFutureUse[10] AS DWORD
	MEMBER wNumExts2 AS WORD
	MEMBER dwExtension2 AS DWORD
	MEMBER sODBCPromptAction AS SHORTINT 
	MEMBER BD2 IS bdCACQMDBPROPS

	// outputs
	MEMBER lpsServerName AS PSZ
/////////////////////////////////
	MEMBER lpsPassword AS PSZ

	MEMBER hDbcOdbc AS PTR

	MEMBER wNumExts AS WORD
	MEMBER dwExtension AS DWORD
 	MEMBER BD IS bdCACQMDBINIT

 	MEMBER hDbHandle AS DWORD

VOSTRUCT bdCAWRMINIT ALIGN 1
//	bdRuntimeOnly		bit 1
	MEMBER bddwFlags AS DWORD

VOSTRUCT bdCACQMLSTDBINIT ALIGN 1
//	bdLdbReg			bit 1
//	bdProperties
//	bdNoRegProps		bit 3
	MEMBER bddwFlags AS DWORD

VOSTRUCT bdCAWQMINIT ALIGN 1
	MEMBER bddwFlags AS DWORD
VOSTRUCT bdCAWRMCREATEDEFINIT ALIGN 1
//	bdRptPrompt			bit 1
//	bdRptIsHidden

//	bdRptEmbedded
//	bdRptIsMdi

//	bdRptPreview
//	bdRptPrint
//	bdRptExport

//	bdNtfyClose
//	bdNtfyPreview
//	bdNtfyEditQuery
//	bdNtfyRunQuery
//	bdNtfyNewWrap
//	bdNtfySave			bit 13

	MEMBER bddwFlags AS DWORD

VOSTRUCT RETCALLBACK ALIGN 1
    // we should just do callbacks and let the wrappers worry about the
    // other mechanisms if they are desired?  Note we can't make NOTWINDOW
    // be the default since there is not neccessarily a window around
    // for the notification if we just loading a report.

//    enum {
//	RET_CLLBCK_NOTSPECIFIED,	// no callback mechanism requested
//	RET_CLLBCK_NOTWINDOW,	// send a message to the specified window
//	RET_CLLBCK_CALLBACK		// call the specified routine
	//RET_CLLBCK_ADVISESINK	// let the user do the advise sink on top of this
//    } eType;
	MEMBER eType AS INT // enum

	MEMBER uTypeData IS CallBackTypeData

    MEMBER dwCallbackContext AS DWORD	//  value put in the callback argument

VOSTRUCT FILENAME ALIGN 1 			// info about the file for FILENAME
	MEMBER lpsFileName AS PSZ
	MEMBER wFileBufferLen AS WORD
	MEMBER lpsDefFileType AS PSZ

VOSTRUCT RETFILE ALIGN 4
//    enum eFileType {
//	RET_FILE_UNSPECIFIED,	// for saves etc, force RET to prompt if no default file
//	RET_FILE_NAME,		// specifies a file name
//	RET_FILE_HANDLE,		// specifies a file handle
//	RET_FILE_STORAGEHANDLE	// specifies an OLE structured storage handle
			//   I'm not sure we will really need this, the
			//   OLE wrapper may be able to handle the storage
			//   handle and just pass in a file handle.  Leave
			//   it for now as a place holder.
			// will we want a memory psuedo file (for things like
			//   saving internal macros?)
//    } eFileType;
	MEMBER eFileType AS INT // enum
	MEMBER uTypeData IS  RETFILETYPEDATA

VOSTRUCT RETQUERY ALIGN 1
//   enum {
//	RET_QUERY_NOTSPECIFIED=0,
//	RET_QUERY_NOQUERY,
//	RET_QUERY_BUCKETTABLE,
//	RET_QUERY_CQMQUERYFILE,
//	RET_QUERY_CQMSQLSTRING,
//	RET_QUERY_CQMQUERY
//   } eQueryType;
	MEMBER eQueryType AS INT // enum
	MEMBER lpstrName AS PSZ
	MEMBER uTypeData IS RetQueryTypeData

VOSTRUCT NOTSPECIFIED ALIGN 1		// information for not specified
//	bdExcludeNoQuery		bit 1	// when prompting, do not include no query queries
//	bdExcludeBucketTable			// when prompting, do not include bucket tables
//	bdExcludeCqmQuery				// exclude the various classes of CQM queries
//	bdExcludeReferenced				// exclude the any queries which are referenced
//	bdReturnOnly			bit 5	// if there is only one query not excluded, return it
	MEMBER bddwFlags AS DWORD

VOSTRUCT RETRPTPROPERTIES ALIGN 1
	MEMBER rDisplayOptions AS DISPLAYOPTIONS

//    enum { 					// how to display fields
//        RETPROPFLD_NAMES=0,
//	RETPROPFLD_FORMAT
//    } eFieldDisplay;
	MEMBER eFieldDisplay AS INT // enum
	
//    enum { 					// units to use for measuring
//        RETPROP_INCHES=0,
//	RETPROP_CENTIMETERS
//    } eMeasurementUnits;
	MEMBER eMeasurementUnits AS INT // enum

	MEMBER uiSnapResolution AS DWORD			// snap resolution in logical twips
	MEMBER rEditOptions IS EDITOPTIONS
	MEMBER rExecutionOptions IS EXECUTIONOPTIONS
VOSTRUCT EDITOPTIONS ALIGN 1
//	bdNoCreateBackup		bit 1	// create a .bak file when a save is done
//	bdNoCenterColumns
//	bdSelectTextFirst		bit 3
	MEMBER bddwFlags AS DWORD
VOSTRUCT CQMQUERYFILE ALIGN 1		// info for CQMQUERYFILE
	MEMBER rFile AS RETFILE
	MEMBER hDbHandle AS DWORD
	MEMBER iQryFileType AS INT	// CQM query type field
VOSTRUCT CQMSQLSTRING ALIGN 1		// info for CQMSQLSTRUNG
	MEMBER lpstrSQL AS PSZ
	MEMBER hDbHandle AS DWORD

VOSTRUCT CAWRMSAVENTFY ALIGN 1
	MEMBER hWrmHandle AS DWORD	// handle for report that was saved
	MEMBER lpstrFileName AS PSZ	// pointer to file name

	MEMBER wNumExts AS WORD
	MEMBER dwExtension AS DWORD
	MEMBER BD IS bdCAQRMSAVENTFY
VOSTRUCT bdCAQRMSAVENTFY ALIGN 1
	MEMBER bddwFlags AS DWORD
VOSTRUCT bdCAWPAPPDEF ALIGN 1
	MEMBER bddwFlags AS DWORD
VOSTRUCT CAWQMINIT ALIGN 1
	// inputs
	MEMBER lpAppDefQq AS CAWPAPPDEF
	MEMBER lpAppDefSql AS CAWPAPPDEF

	MEMBER DIM dwXFutureUse[10] AS DWORD
	MEMBER wNumExts AS WORD
	MEMBER dwExtension AS DWORD
	MEMBER BD IS bdCAWQMINIT
VOSTRUCT CACQMINIT ALIGN 1
	// inputs
	MEMBER hCqmInitAppWnd AS PTR

	MEMBER lpstrDebugFlags AS PSZ
	MEMBER lpstrAppTitle AS PSZ
	MEMBER lpstrMbTitle AS PSZ
	MEMBER lpstrIniFilePath AS PSZ
	MEMBER lpsHelpFile AS PSZ

	MEMBER wMessageBase AS WORD
	MEMBER wCommandBase AS WORD
	MEMBER wDialogHelpBase AS WORD
	
	MEMBER hEnvOdbc AS PTR
	
	MEMBER lpQryProps AS CACQMQRYPROPS

	MEMBER hCableInst AS PTR

	MEMBER lpfErrMsgProc AS PTR

	MEMBER DIM dwXFutureUse[10] AS DWORD
	MEMBER wNumExts AS WORD
	MEMBER dwExtension AS DWORD

	MEMBER BD IS bdCACQMINIT

	MEMBER hDbHandle AS DWORD

VOSTRUCT bdCADBAINIT ALIGN 1
//	bdThreeD			bit 1
//	bdDebugSql			bit 2

	MEMBER bddwFlags AS DWORD

VOSTRUCT RETREPORT ALIGN 1
    // description of report to open or create

//    enum {
//	RET_REPORT_CREATE,
//	RET_REPORT_OPEN
//    } eReportType;
	MEMBER eReportType AS INT // enum

	MEMBER rRptCallback IS RETCALLBACK	// how to notifiy loader concerning this report
	MEMBER rRptContinue IS RETCONTINUE	// check whether to continue loading

    MEMBER hCqmHandle AS DWORD	    // cqm handle to use

    // Cqm database to use for the report.  This may be overridden by
    // a database included in the RETQUERY
    MEMBER hDbHandle AS DWORD

    // specification of the report file
	MEMBER rRptFile IS RETFILE

    // if Create, query & report style to use
	MEMBER rRptQuery IS RETQUERY
	MEMBER rRptRptStyle IS RETRPTSTYLE
	MEMBER lpsRptTitle AS PSZ	// string to use to identify the
								// report if there is no report file

	MEMBER lpRptProperties AS RETRPTPROPERTIES// optional properties for the report
	MEMBER lpsRptDlgHelpFile AS PSZ
	MEMBER wRptDlgHelpBase AS WORD
	MEMBER lpsRptRefHelpFile AS PSZ
	MEMBER wRptRefHelpBase AS WORD
	MEMBER hDlgParent AS PTR	// window to use as the parent
								// for dialogs if there is no
								// control associated with the report

VOSTRUCT EXECUTIONOPTIONS ALIGN 1
	MEMBER bdPreviewWhilePrinting AS DWORD	// preview while printing

VOSTRUCT bdCAWPINIT ALIGN 1
//	bdQueryWrapper			bit 1
//	bdResultWrapper
//	bdReportWrapper			bit 3

	MEMBER bddwFlags AS DWORD

VOSTRUCT NOTWINDOW ALIGN 1
	MEMBER hNotWnd AS PTR
	MEMBER wNotMessage AS WORD	// message to send to the window
				// structure will be the lparam

VOSTRUCT CACQMQRYPROPS ALIGN 1
	// inputs
/////////////////////////////////
// temp
//	MEMBER LogFont IS LOGFONT // align 1 struct defined in Report Class
	MEMBER lfHeight AS LONGINT
	MEMBER lfWidth AS LONGINT
	MEMBER lfEscapement AS LONGINT
	MEMBER lfOrientation AS LONGINT
	MEMBER lfWeight AS LONGINT
	MEMBER lfItalic AS BYTE
	MEMBER lfUnderline AS BYTE
	MEMBER lfStrikeOut AS BYTE
	MEMBER lfCharSet AS BYTE
	MEMBER lfOutPrecision AS BYTE
	MEMBER lfClipPrecision AS BYTE
	MEMBER lfQuality AS BYTE
	MEMBER lfPitchAndFamily AS BYTE
	MEMBER DIM lfFaceName[LF_FACESIZE] AS BYTE
/////////////////////////////////
	MEMBER DIM dwXFutureUse[10] AS DWORD
	MEMBER wNumExts AS WORD
	MEMBER dwExtension AS DWORD
	
	MEMBER BD IS bdCACQMQRYPROPS

VOSTRUCT CAWPAPPDEF ALIGN 1
	MEMBER hInst AS PTR			// application INSTANCE handle
	MEMBER lpsTitle AS PSZ		// app window component title
	MEMBER lpsMenu AS PSZ		// app menu name
	MEMBER lpsAccel AS PSZ		// app accelerator table name

	MEMBER wMessageBase AS WORD	// base TO use FOR control notification
								// messages (IF 0, use defaults)

	MEMBER wCommandBase AS WORD	// base TO use FOR menu command
								// messages (IF 0, use defaults)

	MEMBER iToolBar AS INT		// the calling apps toolbar
								// id TO use
	MEMBER iNumButtons AS INT	// number OF buttons IN the
								// button STRING (including spaces)
	MEMBER lpsButton AS PSZ		// toolbar button STRING

	MEMBER DIM dwXFutureUse[10] AS DWORD
	MEMBER wNumExts AS WORD
	MEMBER dwExtension AS DWORD
	MEMBER BD IS bdCAWPAPPDEF

VOSTRUCT bdCARCCINIT ALIGN 1
	MEMBER bddwFlags AS DWORD

VOSTRUCT CACQMLSTDBINIT ALIGN 1
	// inputs
	MEMBER hDbHandle AS DWORD

	MEMBER wNumExts AS WORD
	MEMBER dwExtension AS DWORD
	MEMBER BD IS bdCACQMLSTDBINIT

VOSTRUCT CAWRMINIT ALIGN 1

	// inputs
	MEMBER lpAppDefRet AS CAWPAPPDEF
	MEMBER lpAppDefVw AS CAWPAPPDEF

	MEMBER hRetHandle AS DWORD
	MEMBER hCqmHandle AS DWORD
	MEMBER hRccHandle AS DWORD
	
	MEMBER DIM dwXFutureUse[10] AS DWORD
	MEMBER wNumExts AS WORD
	MEMBER dwExtension AS DWORD
	MEMBER BD IS bdCAWRMINIT

VOSTRUCT QRTCALLBACK ALIGN 1
//	RETCALLBACKPROC callbackRoutine;	// callback routine to call
	MEMBER callbackRoutine AS PTR	// callback routine to call

VOSTRUCT DISPLAYOPTIONS ALIGN 1	
//	bdNoToolbar				bit 1	// no toolbar
//	bdNoStatusbar					// no statusbar
//	bdNoRulers						// no rulers
//	bdNoSectionNames				// no section names
//	bdNoMargins						// don't gray margins
//	bdNoSectionDividers				// don't display section dividers
//	bdNoColumnRowDividers			// don't display column & row dividers
//	bdParagraphMarks		bit 8	// display paragraph marks
	MEMBER bddwFlags AS DWORD

VOSTRUCT RETCONTINUE ALIGN 1
//    RETCONTINUEPROC continueCallback;
	MEMBER continueCallback AS PTR
    MEMBER dwContinueContext AS DWORD

VOSTRUCT bdCAWRMADVISENTFY ALIGN 1
	MEMBER bddwFlags AS DWORD

VOSTRUCT bdCACQMDBINIT ALIGN 1
//	bdNoIniDbProps			bit 1
	MEMBER bddwFlags AS DWORD

VOSTRUCT CAWPINIT ALIGN 1
	// inputs
	MEMBER hCqmHandle AS DWORD

	MEMBER lpsIniFile AS PSZ
	MEMBER lpsQrySection AS PSZ
	MEMBER lpsResSection AS PSZ
	MEMBER lpsRetSection AS PSZ
	
	MEMBER DIM dwXFutureUse[10] AS DWORD
	MEMBER wNumExts AS WORD
	MEMBER dwExtension AS DWORD
	MEMBER BD IS bdCAWPINIT

VOSTRUCT bdCAWBMINIT ALIGN 1
	MEMBER bddwFlags AS DWORD

VOSTRUCT bdCACQMDBPROPS ALIGN 1
//	bdAutoConnect			bit 1
//	bdNoAutoCommit
//	bdExactName
//	bdOem
//	bdRegistered			bit 5
	MEMBER bddwFlags AS DWORD

VOSTRUCT CARCCINIT ALIGN 1
	MEMBER hWndApp AS PTR
	MEMBER hWndClient AS PTR

	MEMBER hCqmHandle AS DWORD

	MEMBER lpCtrlProps AS CARCCCTRLPROPS
	MEMBER lpsIniFilePath AS PSZ
	
	MEMBER DIM dwXFutureUse[10] AS DWORD
	MEMBER wNumExts AS WORD
	MEMBER dwExtension AS DWORD
	MEMBER BD IS bdCARCCINIT

VOSTRUCT bdCARCCCTRLPROPS ALIGN 1
//	bdNoStatusBar			bit 1
//	bdNoToolBar				bit 2
	MEMBER bddwFlags AS DWORD

VOSTRUCT CARCCCTRLPROPS ALIGN 1

	// inputs
	MEMBER DIM hFont[MAX_RCC_FONT] AS PTR
/////////////////////////////////////
//	MEMBER DIM LogFont[MAX_RCC_FONT] IS LOGFONT // align 1 struct defined in Report Class
	MEMBER lfHeight AS LONGINT
	MEMBER lfWidth AS LONGINT
	MEMBER lfEscapement AS LONGINT
	MEMBER lfOrientation AS LONGINT
	MEMBER lfWeight AS LONGINT
	MEMBER lfItalic AS BYTE
	MEMBER lfUnderline AS BYTE
	MEMBER lfStrikeOut AS BYTE
	MEMBER lfCharSet AS BYTE
	MEMBER lfOutPrecision AS BYTE
	MEMBER lfClipPrecision AS BYTE
	MEMBER lfQuality AS BYTE
	MEMBER lfPitchAndFamily AS BYTE
	MEMBER DIM lfFaceName[LF_FACESIZE] AS BYTE

	MEMBER lfHeight2 AS LONGINT
	MEMBER lfWidth2 AS LONGINT
	MEMBER lfEscapement2 AS LONGINT
	MEMBER lfOrientation2 AS LONGINT
	MEMBER lfWeight2 AS LONGINT
	MEMBER lfItalic2 AS BYTE
	MEMBER lfUnderline2 AS BYTE
	MEMBER lfStrikeOut2 AS BYTE
	MEMBER lfCharSet2 AS BYTE
	MEMBER lfOutPrecision2 AS BYTE
	MEMBER lfClipPrecision2 AS BYTE
	MEMBER lfQuality2 AS BYTE
	MEMBER lfPitchAndFamily2 AS BYTE
	MEMBER DIM lfFaceName2[LF_FACESIZE] AS BYTE
/////////////////////////////////////
	MEMBER DIM ColorRef[MAX_RCC_COLOR] AS DWORD

	MEMBER uiStopTrigger AS DWORD

	MEMBER DIM dwXFutureUse[10] AS DWORD
	MEMBER wNumExts AS WORD
	MEMBER dwExtension AS DWORD
	MEMBER BD IS bdCARCCCTRLPROPS
VOSTRUCT CAWRMADVISENTFY ALIGN 1
	MEMBER dwUserData AS DWORD	// context passed into SetAdvise
	MEMBER dwAdviseId AS DWORD	// advise id
	MEMBER hWrmHandle AS DWORD	// handle the advise was on if not on system (currently always 0)
	MEMBER lpstrTopic AS PSZ	// topic string
	MEMBER lpstrItem AS PSZ		// item string
	MEMBER lpstrValue AS PSZ	// value string

	MEMBER wNumExts AS WORD
	MEMBER dwExtension AS DWORD
	MEMBER BD IS bdCAWRMADVISENTFY

VOSTRUCT bdCAWRMPREVIEWNTFY ALIGN 1
	MEMBER bddwFlags AS DWORD

VOSTRUCT CAWRMCREATEVWINIT ALIGN 1
	// inputs
	MEMBER hWndParent AS PTR
	MEMBER hWndNotify AS PTR

	MEMBER hWrmFrame AS DWORD
	MEMBER hWndMdiClient AS PTR

	MEMBER hCqmHandle AS DWORD

	MEMBER hRetVw AS DWORD
///////////////
// Never access it; just make it 36 bytes.
//	MEMBER rRetLoadVw IS RETLOADVW
	MEMBER dwFiller1 AS DWORD
	MEMBER dwFiller2 AS DWORD
	MEMBER dwFiller3 AS DWORD
	MEMBER dwFiller4 AS DWORD
	MEMBER dwFiller5 AS DWORD
	MEMBER dwFiller6 AS DWORD
	MEMBER dwFiller7 AS DWORD
	MEMBER dwFiller8 AS DWORD
	MEMBER dwFiller9 AS DWORD
//////////////////
	MEMBER hWrmReport AS DWORD
	
	MEMBER wMessageBase AS WORD

	MEMBER lpsTitle AS PSZ

	MEMBER lpAppDefVw AS CAWPAPPDEF

	MEMBER dwUserData AS DWORD

	MEMBER DIM dwXFutureUse[10] AS DWORD
	MEMBER wNumExts AS WORD
	MEMBER dwExtension AS DWORD

	MEMBER BD IS bdCAWRMCREATEVWINIT

	// outputs
	MEMBER hVwMenu AS PTR	// application defined menu handle
	MEMBER hWrmHandle AS DWORD	// Wrm handle

VOSTRUCT bdCAWRMCREATEVWINIT ALIGN 1
//	bdVwEmbedded			bit 1
//	bdVwIsMdi
//	bdVwSynch

//	bdNtfyClose				bit 4
	MEMBER bddwFlags AS DWORD

VOSTRUCT RetLoadVw ALIGN 1
	MEMBER hRpt AS DWORD
	MEMBER rCallback IS RETCALLBACK
	MEMBER rContinue IS RETCONTINUE
    MEMBER lpVwProperties AS RETVWPROPERTIES
	MEMBER lpsVwDlgHelpFile AS PSZ
	MEMBER wRptVwHelpBase AS WORD
VOSTRUCT RetVwProperties ALIGN 1
	MEMBER bddwFlags AS DWORD
//    STRUCT {	
//        BITDEF( bdNoToolbar );			// no toolbar
//	BITDEF( bdNoStatusbar );		// no statusbar
//    } rDisplayOptions;


VOSTRUCT CAWRMCREATEDEFINIT ALIGN 1
	// inputs
	MEMBER hWndParent AS PTR
	MEMBER hWndNotify AS PTR

	MEMBER hWrmFrame AS DWORD
	MEMBER hWndMdiClient AS PTR

	MEMBER hCqmHandle AS DWORD
	MEMBER hRccHandle AS DWORD

	MEMBER hReport AS DWORD
//11111111111111111111111111111111
//	MEMBER rRetReport IS RETREPORT

	MEMBER eReportType AS INT
//2222222222222222222222222222222
//	MEMBER rRptCallback IS RETCALLBACK	// how to notifiy loader concerning this report
	MEMBER eType AS INT
//333333333333333333333333333333
//	MEMBER uTypeData IS CallBackTypeData
//4444444444444444
//	MEMBER rNotWindow IS NOTWINDOW
	MEMBER hNotWnd AS PTR
	MEMBER wNotMessage AS WORD	// message to send to the window
				// structure will be the lparam
//4444444444444444
//333333333333333333333333333333
    MEMBER dwCallbackContext AS DWORD	//  value put in the callback argument
//2222222222222222222222222222222
//555555555555555555555555
//	MEMBER rRptContinue IS RETCONTINUE	// check whether to continue loading
	MEMBER continueCallback AS PTR
    MEMBER dwContinueContext AS DWORD
//555555555555555555555555

    MEMBER hCqmHandle2 AS DWORD	    // cqm handle to use

    // Cqm database to use for the report.  This may be overridden by
    // a database included in the RETQUERY
    MEMBER hDbHandle AS DWORD

    // specification of the report file
//66666666666666666666666666
//	MEMBER rRptFile IS RETFILE
	MEMBER eFileType AS INT
//77777777777777777
//	MEMBER uTypeData IS  RETFILETYPEDATA
//8888888888888888
//	MEMBER rFileName IS FILENAME
	MEMBER lpsFileName AS PSZ
	MEMBER wFileBufferLen AS WORD
	MEMBER lpsDefFileType AS PSZ
//8888888888888888
// below from UNION
//	MEMBER hFileHandle AS PTR // handle for FILEHANDLE case

//77777777777777777
//66666666666666666666666666
    // if Create, query & report style to use
//9999999999999999999   30 bytes till closing 9's
//	MEMBER rRptQuery IS RETQUERY
	MEMBER eQueryType AS INT
	MEMBER lpstrName AS PSZ
//101010101010101010
//	MEMBER uTypeData IS RetQueryTypeData
	MEMBER lpstrSQL AS PSZ
    MEMBER rNotSpecified IS NOTSPECIFIED
//12 12 12 12 12 12
//	MEMBER rCqmQueryFile IS CqmQueryFile
//	MEMBER rFile AS RETFILE
	MEMBER hDbHandle3 AS DWORD
	MEMBER iQryFileType AS INT	// CQM query type field
//12 12 12 12 12 12
//13 13 13 13 13 13 13
//	MEMBER rCqmSqlString IS CqmSqlString
////	MEMBER lpstrSQL AS PSZ
////	MEMBER hDbHandle AS DWORD
//13 13 13 13 13 13 13
////	MEMBER hQueryHandle AS DWORD // handle of a CQM query for CQMQUERY
	MEMBER wFiller AS WORD
	MEMBER Filler2 AS RETFILE

//101010101010101010
//9999999999999999999
//11 11 11 11 11 11 11 11 11 11
//	MEMBER rRptRptStyle IS RETRPTSTYLE
	MEMBER eDefReportStyle AS INT
	MEMBER lpsLabelStyle AS PSZ	// Label info
//11 11 11 11 11 11 11 11 11 11
	MEMBER lpsRptTitle AS PSZ	// string to use to identify the
								// report if there is no report file

	MEMBER lpRptProperties AS RETRPTPROPERTIES // optional properties for the report
	MEMBER lpsRptDlgHelpFile AS PSZ
	MEMBER wRptDlgHelpBase AS WORD
	MEMBER lpsRptRefHelpFile AS PSZ
	MEMBER wRptRefHelpBase AS WORD
	MEMBER hDlgParent AS PTR	// window to use as the parent
								// for dialogs if there is no
								// control associated with the report

//11111111111111111111111111111111
	MEMBER wMessageBase AS WORD

	MEMBER lpsTitle AS PSZ

	MEMBER lpAppDefRet AS CAWPAPPDEF
	
	MEMBER dwUserData AS DWORD

	MEMBER DIM dwXFutureUse[10] AS DWORD
	MEMBER wNumExts AS WORD
	MEMBER dwExtension AS DWORD

	MEMBER BD IS bdCAWRMCREATEDEFINIT
	// outputs
	MEMBER hRptMenu AS PTR
	MEMBER hWrmHandle AS DWORD

VOSTRUCT RETRPTSTYLE ALIGN 1
//    enum {
//	RET_RPTSTYLE_NOTSPECIFIED=0,
//	RET_RPTSTYLE_FREESTYLE,
// 	RET_RPTSTYLE_TABULAR,
//	RET_RPTSTYLE_FORM,
//	RET_RPTSTYLE_LABEL,
//	RET_RPTSTYLE_LETTER,
//	RET_RPTSTYLE_CROSSTAB
//    } eDefReportStyle;
	MEMBER eDefReportStyle AS INT // enum
	MEMBER lpsLabelStyle AS PSZ	// Label info

VOSTRUCT CAWRMPREVIEWNTFY ALIGN 1
	MEMBER hWrmHandle AS DWORD	// handle for the definer to preview
	MEMBER bWait AS LOGIC		// TRUE if you should do a preview wait

	MEMBER wNumExts AS WORD
	MEMBER dwExtension AS DWORD
	MEMBER BD IS bdCAWRMPREVIEWNTFY

UNION RETFILETYPEDATA
	MEMBER rFileName IS FILENAME
	MEMBER hFileHandle AS PTR // handle for FILEHANDLE case
					// storage handles not implemented yet
UNION RETQUERYTYPEDATA
    MEMBER rNotSpecified AS NOTSPECIFIED
	MEMBER rCqmQueryFile AS CqmQueryFile
	MEMBER rCqmSqlString AS CqmSqlString

	MEMBER hQueryHandle AS DWORD // handle of a CQM query for CQMQUERY

UNION CALLBACKTYPEDATA
	MEMBER rNotWindow IS NOTWINDOW
	MEMBER rCallback IS QRTCALLBACK
	
