CLASS SysLink INHERIT TextControl

	//PP-030828 Strong typing
	METHOD __StripTags(sHTML AS STRING) AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL iPosOpen, iPosClose AS DWORD

	iPosOpen := At2("<", sHTML)
	WHILE (iPosOpen > 0)
		iPosClose := At2(">", sHTML)
		IF (iPosClose > 0)
			sHTML := Stuff(sHTML, iPosOpen, (iPosClose-iPosOpen+1), "")
		ENDIF
		iPosOpen := At2("<", sHTML)
	END
   RETURN


CONSTRUCTOR(oOwner, xID, oPoint, oDimension, cText, lDataAware) 
	LOCAL cClass AS USUAL
	LOCAL lResID AS LOGIC
	LOCAL dwInfoSize, dw AS DWORD
	LOCAL pData AS PTR
	LOCAL pVI AS _winVS_FIXEDFILEINFO

	Default(@lDataAware, TRUE)
	lResID := IsInstanceOfUsual(xID,#ResourceID)
	IF !lResID
		dwInfoSize := GetFileVersionInfoSize(PSZ("COMCTL32.DLL"), @dw)
		pData := MemAlloc(dwInfoSize)
		GetFileVersionInfo(PSZ("COMCTL32.DLL"), 0, dwInfoSize, pData)
		VerQueryValue(pData, PSZ("\"), @pVI, @dw)
		IF (HiWord(pVI:dwFileVersionMS) >= 6)
			cClass := "SysLink"
		ELSE
			cClass := "Static"
		ENDIF
	ENDIF
	MemFree(pData)

	SUPER(oOwner, xID, oPoint, oDimension, cClass, SS_Left, lDataAware)

	IF !lResID
		IF !IsNil(cText)
			IF (cClass == "Static")
				SELF:__StripTags(cText)
			ENDIF
			cWindowName := cText
			SELF:Caption := cText
		ENDIF
	ENDIF

	RETURN 

END CLASS

