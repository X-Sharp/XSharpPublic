//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
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
	METHOD Address() AS IntPtr STRICT
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


/// <include file="Gui.xml" path="doc/ResourceID.Name/*" />
	PROPERTY Name as STRING GET sID

/// <include file="Gui.xml" path="doc/ResourceID.ID/*" />
	PROPERTY ID	AS LONG GET nID

/// <include file="Gui.xml" path="doc/ResourceID.ctor/*" />
	CONSTRUCTOR(xID as usual, xResourceFile := NIL as usual)
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

		if xResourceFile is Intptr var ip
			hInst := ip
		elseif xResourceFile is ResourceFile var oResFile
			hInst := oResFile:Handle()
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

		RETURN
END CLASS

