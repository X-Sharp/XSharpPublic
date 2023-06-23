

/// <include file="Gui.xml" path="doc/Icon/*" />

CLASS Icon INHERIT VObject
	PROTECT oIcon AS System.Drawing.Icon

 /// <exclude />
	ACCESS __Icon AS System.Drawing.Icon
		RETURN oIcon


/// <include file="Gui.xml" path="doc/Icon.Destroy/*" />
	METHOD Destroy() AS USUAL
		IF ! oIcon == NULL_OBJECT
			oIcon:Dispose()
			oIcon := NULL_OBJECT
		ENDIF

		SUPER:Destroy()

		RETURN NIL

/// <include file="Gui.xml" path="doc/Icon.Handle/*" />
	METHOD Handle() AS IntPtr STRICT
		RETURN oIcon:Handle

/// <include file="Gui.xml" path="doc/Icon.ctor/*" />
	CONSTRUCTOR(xResourceID, kLoadOption, iWidth, iHeight)
		LOCAL hInst AS IntPtr
		LOCAL oResourceID as ResourceID

		SUPER()
        IF xResourceID IS System.Drawing.Icon var oSDIcon
            SELF:oIcon := oSDIcon
        ELSEIF xResourceID IS System.Drawing.Bitmap var oBmp
            SELF:oIcon := System.Drawing.Icon.FromHandle(oBmp:GetHicon())
        ELSEIF IsNumeric(xResourceID)
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
				WCError{#Init, #Icon, __WCSTypeError, xResourceID, 1}:Throw()
			ELSE
				oResourceID := xResourceID
			ENDIF

			hInst := oResourceID:Handle()

			TRY
				LOCAL hIcon AS IntPtr
				hIcon := GuiWin32.LoadImage(hInst, oResourceID:Address(), IMAGE_ICON, iWidth, iHeight, kLoadOption)
				oIcon := System.Drawing.Icon.FromHandle(hIcon)
			CATCH
				oIcon := NULL_OBJECT
			END TRY
			IF oIcon == NULL_OBJECT .and. !STRING.IsNullOrEmpty(oResourceID:Name)
				LOCAL oBmp AS System.Drawing.Bitmap
				TRY
					oBmp := System.Drawing.Bitmap.FromResource(hInst, oResourceID:Name)
					oIcon := System.Drawing.Icon.FromHandle(oBmp:GetHicon())
                CATCH
                    NOP
				END TRY
			ENDIF

		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/Icon.Size/*" />
	ACCESS Size as Dimension
		RETURN (Dimension) oIcon:Size
    /// <include file="Gui.xml" path="doc/Icon.FromFile/*" />
	STATIC METHOD FromFile(cFile AS STRING	) AS Icon
		IF File(cFile)
			VAR oIcon := System.Drawing.Icon{FPathName()}
			RETURN Icon{oIcon}
		ENDIF
		RETURN NULL

END CLASS

 /// <exclude />
FUNCTION __WCConvertIcon(iconType AS INT) AS System.Drawing.Icon
	LOCAL retVal AS System.Drawing.Icon

	SWITCH iconType
	CASE IconAsterisk
		retVal := System.Drawing.SystemIcons.Asterisk
	CASE IconExclamation
		retVal := System.Drawing.SystemIcons.Exclamation
	CASE IconHand
		retVal := System.Drawing.SystemIcons.Hand
	CASE IconQuestionMark
		retVal := System.Drawing.SystemIcons.Question
	OTHERWISE
		retVal := System.Drawing.SystemIcons.Application
	END SWITCH

	RETURN retVal

