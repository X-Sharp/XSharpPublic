


CLASS Pointer INHERIT VObject
	PROTECT oCursor AS System.Windows.Forms.Cursor
	ACCESS __Cursor AS System.Windows.Forms.Cursor
		RETURN oCursor

	METHOD Confine(oRect as BoundingBox) AS VOID
		System.Windows.Forms.Cursor.Clip := oRect
		RETURN 

	METHOD Handle() AS IntPtr STRICT
		RETURN oCursor:Handle

	METHOD Hide() 
		System.Windows.Forms.Cursor.Hide()
		RETURN SELF

	CONSTRUCTOR(xResourceID) 
		LOCAL lOk AS LOGIC	
		SUPER()

		DEFAULT(@xResourceID, POINTERARROW)

		IF IsObject(xResourceID)  
			LOCAL oResID := xResourceID AS OBJECT
			IF oResID IS System.Windows.Forms.Cursor VAR oCurs
				oCursor := oCurs
				lOk := TRUE
			ELSEIF oResId IS ResourceID VAR oResourceID
				LOCAL hInst AS IntPtr
				LOCAL hCursor AS IntPtr
				hInst		:= oResourceID:Handle()
				IF String.IsNullOrEmpty(oResourceID:Name)
					hCursor     := Win32.LoadCursor(hInst, oResourceID:ID)
				ELSE
					hCursor     := Win32.LoadCursor(hInst, oResourceID:Name)
				ENDIF
				oCursor     := System.Windows.Forms.Cursor{hCursor}
				lOk := TRUE
			ENDIF
		ELSEIF IsNumeric(xResourceID) 
			oCursor := __WCConvertPointer(xResourceID)
			lOk := TRUE
		ELSEIF IsSymbol(xResourceID) .or. IsString(xResourceID)
			oCursor := System.Windows.Forms.Cursor{ (STRING) xResourceID}
			lOk := TRUE
		ENDIF
		IF ! lOk
			WCError{#Init, #Pointer, __WCSTypeError}:@@Throw()
		ENDIF

		RETURN 

	ACCESS Position AS Point
		RETURN (Point) System.Windows.Forms.Cursor.Position

	ASSIGN Position(oPoint AS Point) 
		System.Windows.Forms.Cursor.Position := oPoint
		RETURN 

	METHOD Show()
		System.Windows.Forms.Cursor.Show()
		RETURN  SELF

	STATIC METHOD __WCConvertPointer(pointerType AS INT) AS System.Windows.Forms.Cursor
		LOCAL retVal AS System.Windows.Forms.Cursor
		SWITCH pointerType 
		CASE PointerCrossHairs
			retVal := System.Windows.Forms.Cursors.Cross
		CASE PointerIBeam
			retVal := System.Windows.Forms.Cursors.IBeam
		CASE PointerIcon
			retVal := System.Windows.Forms.Cursors.Default
		CASE PointerFourArrow
			retVal := System.Windows.Forms.Cursors.SizeNESW
		CASE PointerUpArrow
			retVal := System.Windows.Forms.Cursors.UpArrow
		CASE PointerHourGlass
			retVal := System.Windows.Forms.Cursors.WaitCursor
		CASE PointerAppStarting
			retVal := System.Windows.Forms.Cursors.AppStarting
		OTHERWISE
			retVal := System.Windows.Forms.Cursors.Arrow
		END SWITCH

		RETURN retVal


	OPERATOR IMPLICIT ( c AS System.Windows.Forms.Cursor) AS Pointer
		RETURN Pointer{c}

	OPERATOR IMPLICIT ( p AS Pointer ) AS System.Windows.Forms.Cursor
		IF p != NULL_OBJECT
			RETURN p:__Cursor
		ENDIF	
		RETURN NULL_OBJECT
	

END CLASS

