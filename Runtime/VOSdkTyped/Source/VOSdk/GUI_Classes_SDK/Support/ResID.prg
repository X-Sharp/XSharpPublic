/// <include file="Gui.xml" path="doc/ResourceID/*" />
CLASS ResourceID INHERIT VObject
	PROTECT hInst AS IntPtr
	PROTECT nID AS INT
	PROTECT sID AS STRING
	PROTECT _lpAddress AS PSZ
	PROTECT _lMustFree as LOGIC

/// <include file="Gui.xml" path="doc/ResourceID.dtor/*" />
	DESTRUCTOR()
		IF _lpAddress != NULL_PSZ .and. _lMustFree
			MemFree( _lpAddress )
			_lpAddress := NULL_PSZ
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/ResourceID.Address/*" />
	METHOD Address() AS IntPtr
		LOCAL lpAddress AS IntPtr

		IF NULL_STRING != sID
			IF SELF:_lpAddress == NULL_PSZ
				SELF:_lpAddress := lpAddress := StringAlloc(sID)
				SELF:_lMustFree := TRUE
			ELSE
				lpAddress := SELF:_lpAddress
			ENDIF
		ELSE
			lpAddress := IntPtr{nID}
		ENDIF

		RETURN lpAddress

/// <include file="Gui.xml" path="doc/ResourceID.Handle/*" />
	METHOD Handle() AS IntPtr STRICT
	RETURN hInst

/// <include file="Gui.xml" path="doc/ResourceID.ID/*" />

	ACCESS ID	AS LONG
		RETURN nID

/// <include file="Gui.xml" path="doc/ResourceID.ctor/*" />
	CONSTRUCTOR(xID, xResourceFile)
		LOCAL argTypeError AS LOGIC

		SUPER()
		SELF:_lMustFree := FALSE
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
			WCError{#Init, #ResourceID, __WCSTypeError}:@@Throw()
		ENDIF

		RETURN
END CLASS

