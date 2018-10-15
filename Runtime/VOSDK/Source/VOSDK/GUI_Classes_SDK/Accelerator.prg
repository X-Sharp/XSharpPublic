PARTIAL CLASS Accelerator INHERIT VObject
	PROTECT hAccel  AS PTR
	PROTECT aAccels AS ARRAY //SE-060525

METHOD AddAccelerator(oAccelerator) 
	//SE-060525
   LOCAL hAc     AS PTR
   LOCAL dwI     AS DWORD
   LOCAL dwCount AS DWORD
   LOCAL pAccel  AS _winAccel
   LOCAL pMem    AS _winAccel


   IF ! IsInstanceOfUsual(oAccelerator, #Accelerator)
		WCError{#AddAccelerator, #Accelerator, __WCSTypeError, oAccelerator, 1}:@@Throw()
	ENDIF

   IF hAccel == NULL_PTR

      hAc := oAccelerator:Handle()

      IF hAc != NULL_PTR

         dwCount := DWORD(CopyAcceleratorTable(hAc, NULL_PTR, 0l))
         IF dwCount > 1
            IF (pMem := MemAlloc(_SIZEOF(_winAccel)*dwCount)) != NULL_PTR
               CopyAcceleratorTable(hAc, pMem, INT(_CAST,dwCount))
               pAccel := pMem
               FOR dwI := 1 UPTO dwCount
                   AAdd(aAccels, {pAccel:fVirt, pAccel:key, pAccel:cmd})
                   pAccel += 1
               NEXT //dwI
               MemFree(pMem)
            ENDIF
         ENDIF
      ENDIF
      RETURN TRUE

   ENDIF

   RETURN FALSE

METHOD AddKey(nMenuItemId, xKeyId, lCtrl, lAlt, lShift) 
	//SE-060525
   LOCAL fVirt AS BYTE
   LOCAL wKey  AS LONGINT
   LOCAL wCmd  AS LONGINT


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
	      fVirt := _OR(fVirt, 0x01)  //FVIRTKEY
	      wKey  := xKeyId
	   ELSE
	      IF fVirt > 0
	         fVirt := _OR(fVirt, 0x01)  //FVIRTKEY
	         wKey  := LONGINT(Asc(Upper(xKeyId)))
	      ELSE
	         wKey  := LONGINT(Asc(xKeyId))
	      ENDIF
	   ENDIF

	   wCmd := nMenuItemId

	   AAdd(aAccels, {fVirt, wKey, wCmd})
	   RETURN TRUE
   ENDIF

   RETURN FALSE

METHOD Create() 
	//SE-060525
   LOCAL dwI     AS DWORD
   LOCAL dwCount AS DWORD
   LOCAL pAccel  AS _winAccel
   LOCAL pMem    AS _winAccel
   LOCAL aAccel  AS ARRAY


   IF hAccel == NULL_PTR
      IF (dwCount := ALen(aAccels)) > 0
         IF (pMem := MemAlloc(_SIZEOF(_winAccel)*dwCount)) != NULL_PTR
            pAccel := pMem
            FOR dwI := 1 UPTO dwCount
                aAccel := aAccels[dwI]
                pAccel:fVirt := aAccel[1]
                pAccel:key   := aAccel[2]
                pAccel:cmd   := aAccel[3]
                pAccel += 1
            NEXT //dwI
            hAccel := CreateAcceleratorTable(pMem, INT(_CAST,dwCount))
            MemFree(pMem)
#ifdef __VULCAN__
            IF hAccel == NULL_PTR
               GC.SuppressFinalize(SELF)
            ENDIF
#else            
            IF hAccel != NULL_PTR
               RegisterAxit(SELF) // TODO: Conditional call to RegisterAxit() should be replaced with call to GC.SuppressFinalize() for opposite condition 
            ENDIF
#endif            
         ENDIF
         aAccels := {}
      ENDIF
   ENDIF

   RETURN NIL

METHOD Destroy() 
	//SE-060525

   IF hAccel != NULL_PTR
      DestroyAcceleratorTable(hAccel)
      hAccel := NULL_PTR
   ENDIF

   SUPER:Destroy()

   RETURN NIL


METHOD Handle() AS PTR
	//SE-060525

   IF hAccel == NULL_PTR
      SELF:Create()
   ENDIF

   RETURN hAccel

CONSTRUCTOR(xResourceID) 
	//SE-060525
	LOCAL hInst AS PTR
	LOCAL lpTableName AS PTR

	SUPER()

	IF IsNumeric(xResourceID) .OR. IsSymbol(xResourceID) .OR. IsString(xResourceID)
		xResourceID := ResourceID{xResourceID}
	ELSEIF IsInstanceOfUsual(xResourceID, #ResourceID)
		// Ok
	ELSEIF IsNil(xResourceID) .OR. IsInstanceOfUsual(xResourceID, #Accelerator) //SE-060525
		hAccel  := NULL_PTR
		aAccels := {}
		IF ! IsNil(xResourceID)
         SELF:AddAccelerator(xResourceID)
      ENDIF
		RETURN
   ELSE
		WCError{#Init, #Accelerator, __WCSTypeError, xResourceID, 1}:@@Throw()
	ENDIF

   aAccels := NULL_ARRAY

	hInst := xResourceID:Handle()
	lpTableName := xResourceID:Address()

	hAccel := LoadAccelerators(hInst, lpTableName)

	RETURN 
END CLASS

