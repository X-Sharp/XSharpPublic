//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
//#ifdef DONOTwINCLUDE

CLASS IpcTopic INHERIT VObject
	EXPORT cTopicName AS STRING
	EXPORT aItemList AS ARRAY

	METHOD AddItem (cItemString ) 
		LOCAL cItemName AS STRING
		IF !IsString(cItemString)
			WCError{#AddItem,#IpcTopic,__WCSTypeError,cItemString,1}:Throw()
		ENDIF
		cItemName := Upper(cItemString)
		AAdd(aItemList, cItemName)

		RETURN SELF

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
				ATrueDel(aItemList,dwI)
				EXIT
			ENDIF
		NEXT  // dwI

		RETURN SELF

	METHOD Destroy() AS USUAL  CLIPPER
		

		cTopicName := NULL_STRING
		aItemList:={}

		SUPER:Destroy()

		RETURN SELF

	CONSTRUCTOR(cTopicString) 
		

		aItemList := {}
		SUPER()

		IF !IsString(cTopicString)
			WCError{#Init,#IpcTopic, __WCSTypeError,cTopicString,1}:Throw()
		ENDIF

		cTopicName := Upper(cTopicString)
		

		RETURN 

END CLASS

CLASS IpcTopicData INHERIT VObject
	EXPORT ptrData AS PTR
	EXPORT liLen AS LONGINT
	PROTECT lWasAlloc AS LOGIC

	ACCESS Data 
		

		RETURN ptrData

	METHOD Destroy() AS USUAL clipper
		

		IF ptrData!= NULL_PTR .and. lWasAlloc
			MemFree(ptrData)
		ENDIF

		ptrData := Null_Ptr

		SUPER:Destroy()

		RETURN SELF

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
		ENDIF

		IF IsLong(nLength)
			liLen := nLength
		ELSE
			liLen := INT(_CAST, PszLen(ptrData))
		ENDIF

		RETURN 

	ACCESS Length 
		

		RETURN liLen
END CLASS

//#endif
