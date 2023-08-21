#ifndef __VULCAN__


/// <include file="Gui.xml" path="doc/OleControl/*" />
CLASS OleControl INHERIT OleObject
	PROTECT oAuto AS OleAutoObject


 /// <exclude />
ACCESS __oAuto 
	RETURN SELF:oAuto


/// <include file="Gui.xml" path="doc/OleControl.CreateEmbedding/*" />
METHOD CreateEmbedding(cProgID, cLicKey) 
	LOCAL pszLicKey AS PSZ


	IF !Empty(cLicKey)
		pszLicKey := String2Psz(cLicKey)
	ENDIF


	IF _VOOLECreateEmbObject(ObjHandle, String2Psz(cProgID), TRUE, pszLicKey)
		SELF:InitAutoObject()
		lCreated := TRUE
	ELSE
		SELF:DetachFromServer()
		lCreated := FALSE
	ENDIF


	RETURN lCreated


/// <include file="Gui.xml" path="doc/OleControl.CreateFromAppDocStorage/*" />
METHOD CreateFromAppDocStorage(oAppDocStg) 


	IF SUPER:CreateFromAppDocStorage(oAppDocStg)
		SELF:InitAutoObject()
		lCreated := TRUE
	ELSE
		SELF:DetachFromServer()
		lCreated := FALSE
	ENDIF


	RETURN lCreated


/// <include file="Gui.xml" path="doc/OleControl.Destroy/*" />
METHOD Destroy(oEvent) 


	IF (SELF:oAuto != NULL_OBJECT)
		SELF:oAuto:Destroy()
		IF !InCollect()
			SELF:oAuto := NULL_OBJECT
		ENDIF
	ENDIF


	RETURN SUPER:Destroy()


/// <include file="Gui.xml" path="doc/OleControl.Dispatch/*" />
METHOD Dispatch(oEvent) 
	LOCAL oEvt := oEvent AS @@Event
	LOCAL msg := oEvt:uMsg AS DWORD


	IF (msg == WM_PAINT) .AND. lCreated
		RETURN (SELF:EventReturnValue := 0L)
	ELSEIF (msg == WM_ERASEBKGND) .AND. lCreated
		RETURN (SELF:EventReturnValue := 1L)
	ENDIF


	RETURN SUPER:Dispatch(oEvent)


/// <include file="Gui.xml" path="doc/OleControl.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lDataAware) 
    
    
    SUPER(oOwner, xID, oPoint, oDimension, lDataAware)




RETURN 


/// <include file="Gui.xml" path="doc/OleControl.InitAutoObject/*" />
METHOD InitAutoObject() 
	LOCAL pDispatch 	AS PTR
	LOCAL e 				AS Error


	IF _VOOLEGetControlDispatch(ObjHandle, @pDispatch)
		SELF:oAuto := OLEAutoObject{pDispatch}


		IF (SELF:oAuto == NULL_OBJECT)
			e 					:= ErrorNew()
			e:SUBSYSTEM 	:= __CavoStr(OLEAUTO_ERROR_SUBSYSTEM)
			e:CANDEFAULT 	:= TRUE
			e:ARG 			:= NULL_STRING
			e:FUNCSYM 		:= SysAddAtom(PSZ(_CAST, __ENTITY))
			e:Description 	:= __CavoStr(OLEAUTO_ERROR_MSG_INIT_FAILED)
			Eval(ErrorBlock(), e)
			RETURN NIL
		ENDIF
	ENDIF
	RETURN SELF


/// <include file="Gui.xml" path="doc/OleControl.NoIVarGet/*" />
METHOD NoIVarGet(symIVar) 
	LOCAL dwStack := _BP+24 AS DWORD


	IF (SELF:oAuto == NULL_OBJECT)
		RETURN NIL
	ENDIF


	RETURN SELF:oAuto:__NoIVarGet(dwStack, PCount(), symIVar)


/// <include file="Gui.xml" path="doc/OleControl.NoIVarPut/*" />
METHOD NoIVarPut(symIVar, uValue) 
	LOCAL dwStack := _BP+24 AS DWORD


	IF (SELF:oAuto == NULL_OBJECT)
		RETURN NIL
	ENDIF


	RETURN SELF:oAuto:__NoIVarPut(dwStack, PCount(), symIVar, uValue)


/// <include file="Gui.xml" path="doc/OleControl.NoMethod/*" />
METHOD NoMethod() 
	LOCAL dwStack := _BP+16 AS DWORD


	IF (SELF:oAuto == NULL_OBJECT)
		RETURN NIL
	ENDIF


	RETURN SELF:oAuto:__NoMethod(dwStack, PCount())
END CLASS


#endif
