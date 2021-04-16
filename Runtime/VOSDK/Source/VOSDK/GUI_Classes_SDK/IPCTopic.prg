/// <include file="Gui.xml" path="doc/IpcTopic/*" />
CLASS IpcTopic INHERIT VObject
	EXPORT cTopicName AS STRING
	EXPORT aItemList AS ARRAY


/// <include file="Gui.xml" path="doc/IpcTopic.AddItem/*" />
METHOD AddItem (cItemString ) 
	LOCAL cItemName AS STRING


	
	


	IF !IsString(cItemString)
		WCError{#AddItem,#IpcTopic,__WCSTypeError,cItemString,1}:Throw()
	ENDIF
	cItemName := Upper(cItemString)
	AAdd(aItemList, cItemName)


	RETURN SELF


/// <include file="Gui.xml" path="doc/IpcTopic.DeleteItem/*" />
METHOD DeleteItem(cItemString) 
	//SE-060526
	LOCAL cItemName AS STRING
	LOCAL dwI, dwCount AS DWORD


	
	


	IF !IsString(cItemString)
		WCError{#AddItem,#IpcTopic,__WCSTypeError,cItemString,1}:Throw()
	ENDIF


	cItemName := Upper(cItemString)


	dwCount := ALen(aItemList)
	FOR dwI := 1 UPTO dwCount
	   IF aItemList[dwI] = cItemName
	   	ADel(aItemList, dwI)
		   ASize(aItemList, dwCount-1)
		   EXIT
	   ENDIF
	NEXT  // dwI


	RETURN SELF


/// <include file="Gui.xml" path="doc/IpcTopic.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER
	
	


	IF ! InCollect()
		cTopicName := NULL_STRING
		aItemList:={}
		UnregisterAxit(SELF)
	ENDIF


	SUPER:Destroy()


	RETURN SELF


/// <include file="Gui.xml" path="doc/IpcTopic.ctor/*" />
CONSTRUCTOR(cTopicString) 
	
	


	aItemList := {}
	SUPER()


	IF !IsString(cTopicString)
		WCError{#Init,#IpcTopic, __WCSTypeError,cTopicString,1}:Throw()
	ENDIF


	cTopicName := Upper(cTopicString)
	
	


	RETURN 


END CLASS


/// <include file="Gui.xml" path="doc/IpcTopicData/*" />
CLASS IpcTopicData INHERIT VObject
	EXPORT ptrData AS PTR
	EXPORT liLen AS LONGINT
	PROTECT lWasAlloc AS LOGIC


/// <include file="Gui.xml" path="doc/IpcTopicData.Data/*" />
ACCESS Data 
	
	


	RETURN ptrData


/// <include file="Gui.xml" path="doc/IpcTopicData.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER
	
	


	IF ptrData!= NULL_PTR .and. lWasAlloc
		MemFree(ptrData)
	ENDIF


	IF ! InCollect()
		UnregisterAxit(SELF)
		ptrData := Null_Ptr
	ENDIF


	SUPER:Destroy()


	RETURN SELF


/// <include file="Gui.xml" path="doc/IpcTopicData.ctor/*" />
CONSTRUCTOR(ptrString, nLength) 
	
	


	SUPER()


	IF !IsPtr(ptrString) .And. !IsString(ptrString)
		WCError{#Init,#IpcTopicData,__WCSTypeError,ptrString,1}:Throw()
	ENDIF


	IF IsPtr(ptrString)
		ptrData := ptrString
	ELSE
		ptrData := StringAlloc(ptrString)
		lWasAlloc := TRUE
		RegisterAxit(SELF) // TODO: Conditional call to RegisterAxit() should be replaced with call to GC.SuppressFinalize() for opposite condition 
	ENDIF


	IF IsLong(nLength)
		liLen := nLength
	ELSE
		liLen := INT(_CAST, PszLen(ptrData))
	ENDIF


	RETURN 


/// <include file="Gui.xml" path="doc/IpcTopicData.Length/*" />
ACCESS Length 
	
	


	RETURN liLen
END CLASS


