// Harbour functions Library for use by several reportss

CLASS _GET_Object
	EXPORT reader AS USUAL
	EXPORT cargo AS USUAL
	
/*	PROPERTY ComboAlias AS USUAL AUTO
	PROPERTY ComboBrowse AS ARRAY AUTO
	PROPERTY ComboReturnField AS USUAL AUTO*/
	
	CONSTRUCTOR()
		// should not be neccessary
//		SELF:cargo := ArrayCreate(10)
	RETURN
/*	PROPERTY MessageSet AS USUAL
		SET
		END GET
	END PROPERTY*/
	
	METHOD Display() AS _GET_Object
	RETURN SELF
	METHOD colorDisp() AS _GET_Object
	RETURN SELF
END CLASS

GLOBAL _Row AS INT
GLOBAL _Col AS INT

FUNCTION SetPos(y AS INT , x AS INT) AS INT
	_Row := y
	_Col := x
RETURN _Row
FUNCTION Row() AS INT
RETURN _Row
FUNCTION Col() AS INT
RETURN _Col

FUNCTION DevPos(y AS INT,x AS INT) AS USUAL
	SetPos(y,x)
RETURN NIL

FUNCTION DevOut(y AS INT, x AS INT, cText AS STRING , uColor AS INT) AS USUAL
	DevPos(y,x)
RETURN DevOut(cText)
FUNCTION DevOut(y AS INT, x AS INT, cText AS STRING) AS USUAL
	DevPos(y,x)
RETURN DevOut(cText)

FUNCTION DevOut(cText AS STRING , uColor AS INT) AS USUAL
RETURN DevOut(cText)

FUNCTION DevOut(cText AS STRING) AS USUAL
RETURN NIL

FUNCTION _GET_(a,b,c,d,e) CLIPPER
	LOCAL oGet AS _GET_Object
	oGet := _GET_Object{}
	? a,b,c,d,e
RETURN oGet

FUNCTION zGEDBComboNew( cVar, cTheVar, bVar, bWhen, bValid, lDropOnEnter,;
                       cAlias, cTag, xTop, xBottom, bFor, bWhile,;
                       cBaseFilter, xRetFld, aBrowse_,;
                       xDispFld, cKeyTag, lStrict, aHotKeys_, cpicture,;
                       nWidth,nHeight, cColor, bPostEval, lEmptyAllowed)

	LOCAL oGet AS _GET_Object
	oGet := _GET_Object{}
	oGet:cargo := ArrayCreate(10)
RETURN oGet

FUNCTION zGECheckNew CLIPPER
	LOCAL oGet AS _GET_Object
	oGet := _GET_Object{}
	oGet:cargo := ArrayCreate(10)
RETURN oGet

FUNCTION zGEReader() CLIPPER
RETURN NIL

FUNCTION zGetMaxRow() AS INT
RETURN 0
FUNCTION zGetMaxCol() AS INT
RETURN 0
FUNCTION zGetHelpFile() CLIPPER 
RETURN ""

FUNCTION zColorSet() CLIPPER
RETURN NIL
FUNCTION zSaveEnv() CLIPPER
RETURN NIL
FUNCTION zRestEnv() CLIPPER
RETURN NIL
FUNCTION zMessage() CLIPPER
RETURN NIL

FUNCTION zdbUnLock() CLIPPER
RETURN NIL
FUNCTION zDBBRefresh() CLIPPER
RETURN NIL
FUNCTION zRecAdd() CLIPPER
RETURN NIL

FUNCTION zGeEscapeConfirm() CLIPPER
RETURN NIL
FUNCTION zGetValid() CLIPPER
RETURN 0
FUNCTION zIsPositive() CLIPPER
RETURN TRUE
FUNCTION zNotNegative() CLIPPER
RETURN TRUE

FUNCTION zdbCheckKey(cKey,cOrder,aInValidChr,cMessage,lDisplayWarning)
RETURN TRUE
FUNCTION zNotEmpty(xValue,cMessage)
RETURN .not. Empty(xValue)

FUNCTION zHaveAccess(cSection,cSysId,cEntity,cUserId,lNotifyUser)
RETURN TRUE

FUNCTION zOpenShared(aTables,aWorkAreas,lCreate)
RETURN zTable(aTables,TRUE,aWorkAreas,lCreate)
FUNCTION zTable(aTables,lShared,aWorkAreas,lCreate,lNoError)
RETURN TRUE

FUNCTION zdbDelete( xMsg, bDelete)
RETURN TRUE

FUNCTION zSkip(nSkip, bEval, oBrowse, bFor, bWhile)
RETURN TRUE

FUNCTION SetFgPkgsFp(cFgPkgType,lSet)

// SetFgPkgsFp
//RETURN FgPkgs->(zdbSeek(Pad(cFgPkgType,Len(FgPkgs->PkgType)),"FgPkgsPT",iif(lSet != NIL,lSet,.f.)))
RETURN TRUE

FUNCTION zdbRelDelete(aFiles,cSeekKey,lPreLock)
RETURN TRUE

FUNCTION FillExcl
RETURN TRUE

FUNCTION zTBPrint(o,aReport,lPortrait,nRptDst)
RETURN TRUE

FUNCTION zDBRestore(aWorkAreas,nSel, lClose)
RETURN TRUE

FUNCTION zdbSeek(vSeekValue,cOrderFocus,lSetFocus,lSoftSeek)
RETURN TRUE

FUNCTION zDlSwitch(aParentList,aChildList)
RETURN TRUE

FUNCTION zDlChildRefresh(aChildList)
RETURN TRUE


FUNCTION zHeader(cText,cAddOnText)
RETURN NIL

FUNCTION zTBrowse(oBrowse,aExitKeys,lRowHighLight,lRestoreScr)
RETURN TRUE

FUNCTION zDlChildCreate(cTitle,cAlias,cFocus,bTop,bWhile,aColumns,aOptions,nTop,nLeft,nBottom,nRight,aSeperators,nBoxType,aKeyExceptions,bFor,bLineCounter)
RETURN NIL

FUNCTION zDlParentCreate(cTitle,aChild,aColumns,aOptions,aPostKeys,nTop,nLeft,nBottom,nRight,aSeperators,aKeyExceptions,bLineCounter,nBoxType)
RETURN Dummy{}

CLASS Dummy
	EXPORT PlBrowse AS OBJECT
END CLASS

FUNCTION zGetSysId()
RETURN NIL

FUNCTION zWaitOn() CLIPPER
RETURN NIL
FUNCTION zWaitOff() CLIPPER
RETURN NIL
FUNCTION zSpinBar() CLIPPER
RETURN FALSE
FUNCTION SetBfLotFgFp() CLIPPER
RETURN NIL
FUNCTION zExactMatch() CLIPPER
RETURN FALSE
FUNCTION zDialog() CLIPPER
RETURN NIL


FUNCTION zGetSet CLIPPER
RETURN NIL
FUNCTION ValidActive CLIPPER
RETURN FALSE

FUNCTION zGetErr() CLIPPER
RETURN TRUE


