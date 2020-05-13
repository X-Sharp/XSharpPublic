



#using System.Drawing.Printing
#using System.Reflection
CLASS PrintingDevice INHERIT VObject
	PROTECT oSettings AS System.Drawing.Printing.PrinterSettings
	PROTECT cDriver AS STRING
	PROTECT cDevice AS STRING
	PROTECT cPort AS STRING

	METHOD __FillDevMode() AS LOGIC STRICT 
		RETURN TRUE

	ACCESS Copies AS SHORT
		IF oSettings != NULL_OBJECT
			RETURN oSettings:Copies
		ENDIF
		RETURN 0

	ASSIGN Copies(nCopies AS SHORT) 
		IF oSettings != NULL_OBJECT
			oSettings:Copies := nCopies
		ENDIF
		RETURN 

	METHOD Destroy() AS USUAL CLIPPER
		IF oSettings != NULL_OBJECT
			oSettings := NULL_OBJECT
		ENDIF		
		RETURN  SELF

	ACCESS Device  AS STRING
		RETURN cDevice

	METHOD DeviceCapabilities(wCapability) 
		LOCAL uReturn 			AS USUAL
		//Todo
		//LOCAL pWord				AS WORD PTR
		//LOCAL pDWord			AS DWORD PTR
		//LOCAL pLong				AS LONGINT PTR
		//LOCAL pByte				AS BYTE PTR  
		//LOCAL dwReturn			AS DWORD 
		//LOCAL dwTotal			AS DWORD
		//LOCAL aReturn := {}	AS ARRAY
		//LOCAL i, dwTrail, dwStart 	AS DWORD 
		//LOCAL nElementSize	AS DWORD 
		//LOCAL bSave				AS BYTE     
		//LOCAL pszData			AS PSZ

		//// For DC_COPIES, DC_DRIVER, DC_DUPLEX, DC_EXTRA, DC_FIELDS, DC_MAXSIZE, DC_MINSIZE
		//// DC_SIZE, DC_ORIENTATION, DC_TRUETYPE and DC_VERSION dwReturn is the value for the capabilty.
		//// For all others capabalities it is the size of the buffer needed to hold the value.
		//// A second call is then required.

		//dwReturn := PCALL(gpfnDeviceCapabilities, String2Psz(cDevice), String2Psz(cPort), WORD(wCapability),;
		//NULL_PSZ, pDevMode)

		//IF (dwReturn == 0xFFFFFFFF)					// unsupported capability
		//	uReturn := dwReturn


		//ELSEIF (wCapability == DC_MAXEXTENT)	.OR. (wCapability == DC_MINEXTENT)
		//	AAdd(aReturn, LoWord(dwReturn))
		//	AAdd(aReturn, HiWord(dwReturn))
		//	uReturn := aReturn
			
		//ELSEIF (wCapability == DC_BINS) .OR. (wCapability == DC_PAPERS)
		//	// array of words
		//	pWord := MemAlloc(dwReturn* _SIZEOF(WORD))
		//	PCALL(gpfnDeviceCapabilities, String2Psz(cDevice), String2Psz(cPort), WORD(wCapability),;
		//	pWord, pDevMode)

		//	FOR i := 1 TO dwReturn
		//		AAdd(aReturn, pWord[i])
		//	NEXT

		//	uReturn := aReturn
		//	MemFree(pWord)

		//	ELSEIF  wCapability == DC_BINNAMES .OR. wCapability == DC_FILEDEPENDENCIES ;
		//	.OR. wCapability == DC_PAPERNAMES .OR. wCapability == DC_MEDIATYPENAMES ;
		//	.OR. wCapability == DC_PERSONALITY   
		//		// Array of PSZs. 
		//		// Set element size
		//		nElementSize := 0
		//		IF wCapability == DC_BINNAMES
		//			nElementSize := CCHBINNAME 	// 24  
					
		//			ELSEIF  wCapability == DC_FILEDEPENDENCIES ;
		//			.OR. wCapability == DC_PAPERNAMES;
		//			.OR. wCapability == DC_MEDIATYPENAMES
		//			nElementSize := CCHPAPERNAME	// 64
					
		//		ELSEIF wCapability == DC_PERSONALITY
		//			nElementSize := 32
					
		//		ELSE
		//			// What did I miss ?
					
		//		ENDIF     
		//		IF nElementSize > 0
		//			// array of nElementSize byte "c" strings
		//			dwTotal := dwReturn * nElementSize 
		//			pByte := MemAlloc(dwTotal+1)
		//			PCALL(gpfnDeviceCapabilities,String2Psz(cDevice), String2Psz(cPort), WORD(wCapability),;
		//			pByte, pDevMode)
					
		//			FOR i := 1 TO dwTotal STEP nElementSize   
		//				// If the Element = nElementSize characters we have no 0 terminator !
		//				// So make sure we set one 
		//				dwStart := i
		//				dwTrail := dwStart + nElementSize
		//				bSave   := pByte[dwTrail]
		//				pByte[dwTrail] := 0      
		//				pszData := pByte + dwStart -1        
		//				AAdd(aReturn, Trim(Psz2String(pszData)))  
		//				pByte[dwTrail] := bSave
		//			NEXT
		//			MemFree(pByte)
		//		ENDIF
		//	uReturn := aReturn

		//ELSEIF wCapability == DC_ENUMRESOLUTIONS 
		//	// array of pairs of long ints  
		//	pLong := MemAlloc(dwReturn * 2 * _SIZEOF(LONGINT))
		//	PCALL(gpfnDeviceCapabilities,String2Psz(cDevice), String2Psz(cPort), WORD(wCapability),;
		//	pLong, pDevMode)

		//	FOR i := 1 TO dwReturn
		//		AAdd(aReturn, { pLong[i*2-1], pLong[i*2]})
		//	NEXT

		//	uReturn := aReturn
		//	MemFree(pLong)

		//ELSEIF wCapability == DC_MEDIATYPES .OR. wCapability == DC_NUP 
		//	// array of DWORDS
		//	pDWord := MemAlloc(dwReturn * _SIZEOF(DWORD))
		//	PCALL(gpfnDeviceCapabilities,String2Psz(cDevice), String2Psz(cPort), WORD(wCapability),;
		//	pDWord, pDevMode)

		//	FOR i := 1 TO dwReturn
		//		AAdd(aReturn, pDWord[i])
		//	NEXT

		//	uReturn := aReturn
		//	MemFree(pDWord)

		//ELSEIF (wCapability == DC_PAPERSIZE)
		//	// array of pairs of Longs
		//	pLong  := MemAlloc(dwReturn*2 * _SIZEOF(LONGINT))

		//	PCALL(gpfnDeviceCapabilities,String2Psz(cDevice), String2Psz(cPort), WORD(wCapability),;
		//	pLong, pDevMode)

		//	FOR i := 1 TO dwReturn
		//		AAdd(aReturn, { pLong[i*2-1], pLong[i*2]})
		//	NEXT

		//	uReturn := aReturn
		//	MemFree(pLong)
		//	ELSE	// dwReturn is desired value
		//	uReturn := dwReturn

		//ENDIF

		RETURN uReturn

	ACCESS Driver AS STRING
		RETURN cDriver

	METHOD GetDevMode() AS IntPtr
		IF oSettings != NULL_OBJECT
			RETURN oSettings:GetHdevmode()
		ENDIF
		RETURN IntPtr.Zero

	CONSTRUCTOR(uName) 
		SUPER()
		oSettings := PrinterSettings{}
		IF IsString(uName)
			LOCAL aElements AS STRING[]
			LOCAL cName AS STRING
			cName := uName
			aElements := cName:Split(",":ToCharArray())
			oSettings:PrinterName := aElements[1]
		ENDIF
		

		//IF IsNil(uName)
		//	ptrTemp := MemAlloc(256)
		//	IF GetProfileString(String2Psz("windows"), String2Psz("Device"), String2Psz(_CHR(0)), ptrTemp, 256) == 0
		//		RETURN
		//	ENDIF
		//	cTemp := Psz2String(ptrTemp)
		//	MemFree(ptrTemp)
		//ELSE
		//	IF !IsString(uName)
		//		WCError{#Init,#PrintingDevice,__WCSTypeError,uName,1}:@@Throw()
		//	ENDIF
		//	cTemp:=uName
		//ENDIF

		//wTemp1 := At2(",", cTemp) //position of first comma

		//IF (wTemp1 != 0)
		//	cDevice := SubStr3(cTemp, 1, wTemp1-1) //extract device
		//	wTemp2 := At3(",", cTemp, wTemp1+1) //Position of second comma

		//	IF (wTemp2 != 0)
		//		cDriver := SubStr3(cTemp, wTemp1+1, wTemp2-wTemp1-1) //extract driver
		//		wTemp1 := wTemp2
		//		wTemp2 := SLen(cTemp)

		//		IF (wTemp2 > wTemp1)
		//			cPort := SubStr3(cTemp, wTemp1+1, wTemp2-wTemp1) //rest of string is port
		//		ENDIF
		//	ENDIF
		//ENDIF

		//lValid := PCALL(gpfnOpenPrinter, String2Psz(cDevice), PTR(_CAST, @hPrinter), NULL_PTR)
		//isize := _SIZEOF(_winDEVMODE)

		//SELF:__FillDevMode()

		RETURN 

	METHOD IsValid() 
		IF oSettings != NULL_OBJECT
			RETURN oSettings:IsValid
		ENDIF
		RETURN FALSE

	ACCESS Orientation AS LONG
		IF oSettings != NULL_OBJECT
			IF oSettings:DefaultPageSettings:Landscape 
				RETURN 2
			ELSE
				RETURN 1
			ENDIF
		ENDIF

		RETURN 0

	ASSIGN Orientation(nOrientation AS LONG) 
		IF oSettings != NULL_OBJECT
			oSettings:DefaultPageSettings:Landscape := nOrientation == 2
		ENDIF
		SELF:UpdateDevMode()

		RETURN 


	ACCESS PaperHeight AS LONG
		IF oSettings != NULL_OBJECT
			RETURN oSettings:DefaultPageSettings:PaperSize:Height
		ENDIF
		RETURN 0


	ASSIGN PaperHeight(nHeight AS LONG) 
		IF oSettings != NULL_OBJECT
			oSettings:DefaultPageSettings:PaperSize:Height := nHeight
		ENDIF
		SELF:UpdateDevMode()

		RETURN 


	ACCESS PaperSize AS LONG
		IF oSettings != NULL_OBJECT
			RETURN (LONG) oSettings:DefaultPageSettings:PaperSize:Kind
		ENDIF
		RETURN 0

	ASSIGN PaperSize(nSize AS LONG) 
		
		IF oSettings != NULL_OBJECT
			FOREACH oSize AS PaperSize IN oSettings:PaperSizes
				IF oSize:Kind == (PaperKind) nSize
					oSettings:DefaultPageSettings:PaperSize := oSize
					EXIT
				ENDIF
			NEXT
		ENDIF
		SELF:UpdateDevMode()

		RETURN 

	ACCESS PaperWidth AS LONG
		IF oSettings != NULL_OBJECT
			RETURN (LONG) oSettings:DefaultPageSettings:PaperSize:Width
		ENDIF
		RETURN 0	

	ASSIGN PaperWidth(nWidth AS LONG) 
		IF oSettings != NULL_OBJECT
			oSettings:DefaultPageSettings:PaperSize:Width := nWidth
		ENDIF

		SELF:UpdateDevMode()

		RETURN 

	ACCESS Port AS STRING
		RETURN cPort

	METHOD SetUp() AS LOGIC
		LOCAL oSetup AS System.Windows.Forms.PrintDialog
		LOCAL lRetVal AS LOGIC
		LOCAL oPI AS PropertyInfo
		LOCAL oType AS System.Type
		oSetup := System.Windows.Forms.PrintDialog{}
		oSetup:PrinterSettings := oSettings
		IF oSetup:ShowDialog() == System.Windows.Forms.DialogResult.OK
			lRetVal := TRUE
			oSettings := oSetup:PrinterSettings
			cDevice   := oSettings:PrinterName
			// Driver and Port are internal. Use a trick to read them
			oType := oSettings:GetType()
			oPI := oType:GetProperty("DriverName", BindingFlags.NonPublic | BindingFlags.Instance| BindingFlags.IgnoreCase)
			IF oPI != NULL_OBJECT
				SELF:cDriver := oPI:GetValue(oSettings, NULL)
			ENDIF
			oPI := oType:GetProperty("OutputPort", BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.IgnoreCase)
			IF oPI != NULL_OBJECT
				SELF:cPort := oPI:GetValue(oSettings, NULL)
			ENDIF
			
		ENDIF
		RETURN lRetVal

	METHOD UpdateDevMode() 
		RETURN TRUE


END CLASS



