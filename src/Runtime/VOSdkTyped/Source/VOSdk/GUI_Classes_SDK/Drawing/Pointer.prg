//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/// <include file="Gui.xml" path="doc/Pointer/*" />


CLASS Pointer INHERIT VObject
    PROTECT oCursor AS System.Windows.Forms.Cursor
    /// <exclude />
    

/// <include file="Gui.xml" path="doc/Pointer.Confine/*" />
	METHOD Confine(oRect as BoundingBox) AS VOID
		System.Windows.Forms.Cursor.Clip := oRect
		RETURN

/// <include file="Gui.xml" path="doc/Pointer.Handle/*" />
	METHOD Handle() AS IntPtr STRICT
		RETURN oCursor:Handle

/// <include file="Gui.xml" path="doc/Pointer.Hide/*" />
	METHOD Hide()
		System.Windows.Forms.Cursor.Hide()
		RETURN SELF

/// <include file="Gui.xml" path="doc/Pointer.ctor/*" />
	CONSTRUCTOR(xResourceID)
		LOCAL lOk AS LOGIC
		SUPER()

		DEFAULT(REF xResourceID, POINTERARROW)

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
					hCursor     := GuiWin32.LoadCursor(hInst, oResourceID:ID)
				ELSE
					hCursor     := GuiWin32.LoadCursor(hInst, oResourceID:Name)
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
			WCError{#Init, #Pointer, __WCSTypeError}:Throw()
		ENDIF

		RETURN

/// <include file="Gui.xml" path="doc/Pointer.Position/*" />
	ACCESS Position AS Point
		RETURN (Point) System.Windows.Forms.Cursor.Position

/// <include file="Gui.xml" path="doc/Pointer.Position/*" />
	ASSIGN Position(oPoint AS Point)
		System.Windows.Forms.Cursor.Position := oPoint
		RETURN

/// <include file="Gui.xml" path="doc/Pointer.Show/*" />
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
			RETURN p:oCursor
		ENDIF
		RETURN NULL_OBJECT


END CLASS

