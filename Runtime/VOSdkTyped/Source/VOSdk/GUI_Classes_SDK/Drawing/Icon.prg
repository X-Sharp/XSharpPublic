


CLASS Icon INHERIT VObject
	PROTECT oIcon AS System.Drawing.Icon

	ACCESS __Icon AS System.Drawing.Icon
		RETURN oIcon


	METHOD Destroy() AS USUAL CLIPPER
		IF ! oIcon == NULL_OBJECT
			oIcon:Dispose()
			oIcon := NULL_OBJECT
		ENDIF

		SUPER:Destroy()

		RETURN NIL

	METHOD Handle() AS IntPtr STRICT
		RETURN oIcon:Handle

	CONSTRUCTOR(xResourceID, kLoadOption, iWidth, iHeight) 
		LOCAL hInst AS IntPtr
		LOCAL oResourceID as ResourceID

		SUPER()

		IF IsNumeric(xResourceID)
			// This is loading a standard icon
			oIcon := __WCConvertIcon(xResourceID)
		ELSEIF IsPtr(xResourceID)
			oIcon := System.Drawing.Icon.FromHandle((IntPtr) xResourceID)
		ELSE
			Default(@xResourceID, ICONSTANDARD)
			Default(@kLoadOption, LR_DEFAULTCOLOR)
			IF ! IsLong(iWidth)
				iWidth := 0
			ENDIF
			IF ! IsLong(iHeight)
				iHeight := 0
			ENDIF

			IF IsSymbol(xResourceID) .or. IsString(xResourceID)
				oResourceID := ResourceID{xResourceID}
			ELSEIF !IsInstanceOfUsual(xResourceID, #ResourceID)
				WCError{#Init, #Icon, __WCSTypeError, xResourceID, 1}:@@Throw()
			ELSE
				oResourceID := xResourceID
			ENDIF

			hInst := oResourceID:Handle()
			
			TRY
				LOCAL hIcon AS IntPtr
				hIcon := Win32.LoadImage(hInst, oResourceID:Address(), IMAGE_ICON, iWidth, iHeight, kLoadOption)
				oIcon := System.Drawing.Icon.FromHandle(hIcon)
			CATCH
				oIcon := NULL_OBJECT
			END TRY
			IF oIcon == NULL_OBJECT .and. !STRING.IsNullOrEmpty(oResourceID:Name)
				LOCAL oBmp AS System.Drawing.Bitmap
				TRY
					oBmp := System.Drawing.Bitmap.FromResource(hInst, oResourceID:Name) 
					oIcon := System.Drawing.Icon.FromHandle(oBmp:GetHicon())
				END TRY
			ENDIF
	
		ENDIF
		RETURN 

	ACCESS Size 
		RETURN (Dimension) oIcon:Size

	METHOD FromFile(cFile AS STRING	) AS LOGIC
		IF File(cFile)
			IF ! oIcon == NULL_OBJECT
				oIcon:Dispose()
			ENDIF
			oIcon := System.Drawing.Icon{cFile}
			RETURN oIcon != NULL_OBJECT
		ENDIF
		RETURN FALSE

END CLASS

FUNCTION __WCConvertIcon(iconType AS INT) AS System.Drawing.Icon
	LOCAL retVal AS System.Drawing.Icon

	DO CASE
	CASE iconType == IconAsterisk
		retVal := System.Drawing.SystemIcons.Asterisk
	CASE iconType == IconExclamation
		retVal := System.Drawing.SystemIcons.Exclamation
	CASE iconType == IconHand
		retVal := System.Drawing.SystemIcons.Hand
	CASE iconType == IconQuestionMark
		retVal := System.Drawing.SystemIcons.Question
	OTHERWISE
		retVal := System.Drawing.SystemIcons.Application
	ENDCASE

	RETURN retVal

