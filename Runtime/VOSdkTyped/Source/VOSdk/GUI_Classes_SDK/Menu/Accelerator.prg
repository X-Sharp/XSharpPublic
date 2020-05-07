

#USING System.Collections.Generic

CLASS AcceleratorKey
	INTERNAL Flags	as WORD
	INTERNAL ASCII	as Word
	INTERNAL ID		as WORD
	CONSTRUCTOR(wFlags as WORD, wASCII as word, wID as WORD)
		Flags := wFlags
		ASCII := wASCII
		ID	  := wID
	ACCESS Shortcut as System.Windows.Forms.Shortcut
		LOCAL iKey as LONG
		if _AND(Flags, FALT) == FALT
			iKey += System.Windows.Forms.Keys.Alt
		ENDIF
		if _AND(Flags, FCONTROL) == FCONTROL
			iKey += System.Windows.Forms.Keys.Control
		ENDIF
		if _AND(Flags, FSHIFT) == FSHIFT
			iKey += System.Windows.Forms.Keys.Shift
		ENDIF
		IF _AND(Flags, FVIRTKEY) == FVIRTKEY
			iKey += ASCII
		ELSE
			iKey += ASCII
		ENDIF
		RETURN (System.Windows.Forms.Shortcut) iKey
		
		

END CLASS

CLASS Accelerator INHERIT VObject
	PROTECT hAccel  AS IntPtr
	PROTECT aKeys	as List<AcceleratorKey>
	
	
	METHOD __AddKeys(hTable as IntPtr) as LOGIC
		LOCAL dwI     AS DWORD
		LOCAL dwCount AS DWORD
		LOCAL pAccel  AS _winAccel
		LOCAL pMem    AS _winAccel
		IF hTable != NULL_PTR 
		
			dwCount := DWORD(Win32.CopyAcceleratorTable(hTable, NULL_PTR, 0l))
			IF dwCount > 1
				IF (pMem := MemAlloc(_SIZEOF(_winAccel)*dwCount)) != NULL_PTR
					Win32.CopyAcceleratorTable(hTable, pMem, INT(_CAST,dwCount))
					pAccel := pMem
					FOR dwI := 1 UPTO dwCount
						aKeys:Add(AcceleratorKey{pAccel:fVirt, pAccel:key, pAccel:cmd})
						pAccel += 1
					NEXT //dwI
					MemFree(pMem)
				ENDIF
				RETURN TRUE
			ENDIF
		ENDIF
		RETURN FALSE 
	
	ACCESS Keys as IList<AcceleratorKey>
		if aKeys == NULL_OBJECT
			aKeys :=  List<AcceleratorKey>{}
		ENDIF
		if aKeys:Count == 0
			SELF:__AddKeys(hAccel)
		ENDIF
		return aKeys
		
	METHOD AddAccelerator(oAccelerator as Accelerator) as LOGIC 
		IF hAccel == NULL_PTR
			SELF:__AddKeys(oAccelerator:Handle())
			RETURN TRUE
		ENDIF
		RETURN FALSE

	METHOD AddKey(nMenuItemId, xKeyId, lCtrl, lAlt, lShift) 
		LOCAL fVirt AS DWORD
		LOCAL wKey  AS DWORD
		LOCAL wCmd  AS DWORD


		IF hAccel == NULL_PTR

			fVirt := 0

			IF IsLogic(lAlt) .AND. lAlt
				fVirt := _OR(fVirt, FALT)
			ENDIF

			IF IsLogic(lCtrl) .AND. lCtrl
				fVirt := _OR(fVirt, FCONTROL)
			ENDIF

			IF IsLogic(lShift) .AND. lShift
				fVirt := _OR(fVirt, FSHIFT)
			ENDIF

			IF IsNumeric(xKeyId)
				fVirt := _OR(fVirt, FVIRTKEY)  
				wKey  := xKeyId
			ELSE
				IF fVirt > 0
					fVirt := _OR(fVirt, FVIRTKEY)  
					wKey  := WORD(Asc(Upper(xKeyId)))
				ELSE
					wKey  := WORD(Asc(xKeyId))
				ENDIF
			ENDIF

			wCmd := nMenuItemId

			aKeys:Add(AcceleratorKey{(WORD) fVirt, (WORD) wKey, (WORD) wCmd})
			RETURN TRUE
		ENDIF

		RETURN FALSE

	METHOD Create() 
		LOCAL dwCount AS LONG
		LOCAL pAccel  AS _winAccel
		LOCAL pMem    AS _winAccel


		IF hAccel == NULL_PTR
			IF (dwCount := aKeys:Count) > 0
				IF (pMem := MemAlloc(_SIZEOF(_winAccel)*dwCount)) != NULL_PTR
					pAccel := pMem
					FOREACH IMPLIED oKey in aKeys
						pAccel:fVirt := (BYTE) oKey:Flags
						pAccel:key   := oKey:ASCII
						pAccel:cmd   := oKey:ID
						pAccel += 1
					NEXT //dwI
					hAccel := Win32.CreateAcceleratorTable(pMem, INT(_CAST,dwCount))
					MemFree(pMem)
				ENDIF
				aKeys:Clear()
			ENDIF
		ENDIF

		RETURN NIL

	METHOD Destroy() AS USUAL CLIPPER
		IF hAccel != NULL_PTR
			Win32.DestroyAcceleratorTable(hAccel)
			hAccel := NULL_PTR
		ENDIF

		SUPER:Destroy()

		RETURN NIL


	METHOD Handle() AS IntPtr STRICT
		IF hAccel == NULL_PTR
			SELF:Create()
		ENDIF

		RETURN hAccel

	CONSTRUCTOR(xResourceID) 
		LOCAL hInst AS PTR
		LOCAL lpTableName AS PTR

		SUPER()

		IF IsNumeric(xResourceID) .OR. IsSymbol(xResourceID) .OR. IsString(xResourceID)
			xResourceID := ResourceID{xResourceID}
		ELSEIF IsInstanceOfUsual(xResourceID, #ResourceID)
			// Ok
		ELSEIF IsNil(xResourceID) .OR. IsInstanceOfUsual(xResourceID, #Accelerator) //SE-060525
		aKeys := List<AcceleratorKey>{}
			hAccel  := NULL_PTR
			IF ! IsNil(xResourceID)
				SELF:AddAccelerator(xResourceID)
			ENDIF
			RETURN
		ELSE
			WCError{#Init, #Accelerator, __WCSTypeError, xResourceID, 1}:@@Throw()
		ENDIF

		aKeys := NULL_OBJECT

		hInst := xResourceID:Handle()
		lpTableName := xResourceID:Address()

		hAccel := Win32.LoadAccelerators(hInst, lpTableName)

		RETURN 
		
END CLASS


