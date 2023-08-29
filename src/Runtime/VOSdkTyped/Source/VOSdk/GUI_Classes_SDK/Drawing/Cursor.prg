//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// The cursor class handles the Windows (textBox) Caret
// This is not used in Vewa, so the class is empty

CLASS Cursor INHERIT VObject
END CLASS

#ifdef NOTIMPLEMENTED

CLASS Cursor INHERIT VObject
	PROTECT oWnd     AS window
	PROTECT oDim     AS Dimension
	PROTECT oBitmap  AS Bitmap
	PROTECT oPoint   AS Point
	PROTECT lDimmed  AS LOGIC
	PROTECT lCreated AS LOGIC
	PROTECT lVisible AS LOGIC

	METHOD __Update(lCreate AS LOGIC) AS VOID STRICT 
		
		IF lCreate
			IF !lCreated
				IF (oBitmap != NULL_OBJECT)
					CreateCaret(oWnd:Handle(), oBitmap:Handle(), 0, 0)
				ELSE
					CreateCaret(oWnd:Handle(), DWORD(_CAST,lDimmed), oDim:Width, oDim:Height)
				ENDIF
				lCreated := TRUE
				IF (oPoint != NULL_OBJECT)
					SELF:Position := oPoint
				ENDIF
			ENDIF
			IF lVisible
				ShowCaret(oWnd:Handle())
			ENDIF
		ELSE
			IF lCreated
				DestroyCaret()
				lCreated := FALSE
			ENDIF
		ENDIF
		RETURN


	METHOD Destroy()  AS USUAL 
		LOCAL oWndCursor AS Cursor

		IF lCreated
			DestroyCaret()
			lCreated := FALSE
		ENDIF

		IF !InCollect()
			UnRegisterAxit(SELF)
			oWndCursor := oWnd:__Cursor
			IF (oWndCursor == SELF)
				oWnd:__Cursor := NULL_OBJECT
			ENDIF
			oWnd := NULL_OBJECT
			oDim := NULL_OBJECT
			oBitmap := NULL_OBJECT
			oPoint := NULL_OBJECT
		ENDIF

		RETURN NIL

	METHOD Handle() as IntPtr STRICT
		RETURN oWnd:Handle()

	METHOD Hide() 
		

		IF lCreated .AND. lVisible
			HideCaret(oWnd:Handle())
		ENDIF

		lVisible := FALSE

		RETURN SELF

	CONSTRUCTOR(oOwner, oObject, lDimmed) 
		LOCAL oOldCursor AS Cursor

		SUPER()
		

		IF !IsInstanceOfUsual(oOwner,#Window)
			WCError{#Init,#Cursor,__WCSTypeError,oOwner,1}:Throw()
		ENDIF

		oWnd := oOwner
		IF IsInstanceOfUsual(oObject,#Dimension)
			oDim := oObject
			IF !IsNil(lDimmed)
				IF !IsLogic(lDimmed)
					WCError{#Init,#Cursor,__WCSTypeError,lDimmed,3}:Throw()
				ENDIF
				SELF:lDimmed := lDimmed
			ELSE
				SELF:lDimmed := TRUE
			ENDIF
			SELF:Position := Point{0,0}
		ELSE
			IF !IsInstanceOfUsual(oObject,#Bitmap)
				WCError{#Init,#Cursor,__WCSTypeError,oObject,2}:Throw()
			ENDIF
			IF !IsNil(lDimmed)
				WCError{#Init,#Cursor,__WCSTypeError,lDimmed,3}:Throw()
			ENDIF
			oBitmap := oObject
			oDim := oBitmap:Size
		ENDIF

		// If Window already has caret destroy it
		oOldCursor := oWnd:__Cursor
		IF (oOldCursor != NULL_OBJECT)
			oOldCursor:__Update(FALSE)
		ENDIF

		// If Window has focus create caret now
		oWnd:__Cursor := SELF
		IF GetFocus() == oWnd:Handle()
			SELF:__Update(TRUE)
		ENDIF

		RETURN 

	ACCESS Position AS Point
		LOCAL strucPoint IS _WinPoint

		IF !lCreated
			RETURN oPoint
		ENDIF

		GetCaretPos( @strucPoint )

		RETURN oPoint := __WCConvertPoint(oWnd,Point{strucPoint:X,strucPoint:Y})

	ASSIGN Position(oNewPoint AS Point) 
		LOCAL oTempPoint AS Point

		oPoint := oNewPoint
		IF lCreated
			oTempPoint := __WCConvertPoint(oWnd,oPoint)
			SetCaretPos ( oTempPoint:X, oTempPoint:Y)
		ENDIF

		RETURN 

	METHOD Show() 
		

		IF lCreated .AND. !lVisible
			ShowCaret (oWnd:Handle())
		ENDIF
		lVisible := TRUE

		RETURN SELF
END CLASS

#endif
