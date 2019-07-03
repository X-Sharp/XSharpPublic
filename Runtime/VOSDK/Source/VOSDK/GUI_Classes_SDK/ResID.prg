CLASS ResourceID INHERIT VObject
	PROTECT hInst AS PTR
	PROTECT nID AS INT
	PROTECT sID AS STRING
	PROTECT _lpAddress AS PSZ
	
DESTRUCTOR()
   MemFree( _lpAddress )
   RETURN	

METHOD Address() 
	LOCAL lpAddress AS PTR

	IF NULL_STRING != sID    
		//RvdH 070615 Make sure the string is STATIC. We can't control the lifetime of the return variable
		//lpAddress := PTR(_CAST, Cast2Psz(sID))
#ifdef __VULCAN__
         IF SELF:_lpAddress == NULL
             SELF:_lpAddress := lpAddress := StringAlloc(sID)
             GC.ReRegisterForFinalize( SELF )
         ELSE
             lpAddress := SELF:_lpAddress
         ENDIF
#else
		lpAddress := SysGetAtomName(SysAddAtom(String2Psz(sID)))
#endif		
	ELSE
		lpAddress := PTR(_CAST, nID)
	ENDIF

	RETURN lpAddress

METHOD Handle() AS PTR

	RETURN hInst

ACCESS ID 

	IF (NULL_STRING != sID)
		RETURN sID
	ENDIF
	RETURN nID

CONSTRUCTOR(xID, xResourceFile) 
	LOCAL argTypeError AS LOGIC
	
	SUPER()

	IF IsString(xID)
		sID := xID
	ELSEIF IsNumeric(xID)
		nID := xID
	ELSEIF IsSymbol(xID)
		sID := Symbol2String(xID)
	ELSE
		argTypeError := TRUE
	ENDIF

	IF IsPtr(xResourceFile)
		hInst := xResourceFile
	ELSEIF IsInstanceOfUsual(xResourceFile, #ResourceFile)
		hInst := xResourceFile:Handle()
	ELSEIF IsNil(xResourceFile)     
		IF IsNumeric(xID)		// String table
			hInst := GetNatDllHandle()
		ELSE
			hInst := _GetInst()
		ENDIF
	ELSE
		argTypeError := TRUE
	ENDIF

	IF argTypeError
		WCError{#Init, #ResourceID, __WCSTypeError}:Throw()
	ENDIF
	
#ifdef __VULCAN__		
      // we only need the destructor if lpAddress
      // has a pointer that needs to be released
      GC.SuppressFinalize( SELF )
#endif
      
	RETURN 
END CLASS

