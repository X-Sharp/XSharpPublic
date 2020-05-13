// WindowExtensions.prg
// Extensions to the windows class from Vewa and bBrowser



PARTIAL CLASS Window
	METHOD GSG_CenterWindow( ) AS Window STRICT
		SELF:Center()
		RETURN SELF

	METHOD InVisibleChildAction( xChild ) 
		IF !IsNil( SELF:Owner ) .AND. IsObject( SELF:Owner ) .AND. ;
			IsMethod( SELF:Owner, #InvisibleChildAction )
			RETURN Send(SELF:Owner,#InvisibleChildAction, xChild ) 
		ELSE
			RETURN xChild
		ENDIF

	ACCESS IsResizable() AS LOGIC
		RETURN ( _AND( Win32.GetWindowLong( SELF:Handle(), GWL_STYLE ), WS_THICKFRAME ) = WS_THICKFRAME )

	ACCESS Min_Visibility AS INT
		//Für Subwindows ebenfalls interessant
		RETURN SELF:size:width

	PROTECT oOldCursor AS OBJECT
	PROTECT oBusyCursor AS OBJECT
	PROTECT lSetState   AS LOGIC
	
	METHOD SetBusyCursor( lSet, lNotify ) 

		DEFAULT( @lSet, FALSE )
		DEFAULT( @lNotify, FALSE )

		IF lNotify .AND. SELF:Owner != NULL_OBJECT .AND. IsMethod( SELF:Owner, #SetBusyCursor )
			RETURN Send(SELF:Owner,#SetBusyCursor, lSet, lNotify )
		ELSE
			IF oOldCursor == NULL_OBJECT
				oOldCursor := SELF:Pointer
			ENDIF
			IF oBusyCursor == NULL_OBJECT
				oBusyCursor := Pointer{ POINTERHOURGLASS }
			ENDIF

			IF lSet != lSetState
				IF ( lSetState := lSet )
					oOldCursor := SELF:Pointer
					SELF:Pointer := oBusyCursor
				ELSE
					SELF:Pointer := oOldCursor
				ENDIF
			ENDIF
			YieldMessageLoop( SELF )
		ENDIF

		RETURN lSet

	METHOD VisibleState( lState  ) AS LOGIC
		LOCAL lRetVal := TRUE AS LOGIC
		IF oParent != NULL_OBJECT .AND. IsMethod( oParent, #VisibleState )
			lRetVal := Send(oParent,#VisibleState, lState )
			IF !lRetVal .AND. IsNil( lState )
				Send(oParent,#InVisibleChildAction, SELF )
			ENDIF
		ENDIF
		RETURN lRetVal
END CLASS		




FUNCTION YieldMessageLoop( oWin AS OBJECT )
	LOCAL msg  IS _WINMSG
	LOCAL hWnd AS PTR

	IF oWin is IGuiObject
        LOCAL oGui := (IGuiObject) oWin
		hWnd := oGui:__Handle
		DO WHILE ( Win32.PeekMessage( @msg, hWnd, 0, 0, PM_REMOVE ) )
			Win32.TranslateMessage( @msg )
			Win32.DispatchMessage( @msg )
		ENDDO
	ENDIF
	RETURN NIL
