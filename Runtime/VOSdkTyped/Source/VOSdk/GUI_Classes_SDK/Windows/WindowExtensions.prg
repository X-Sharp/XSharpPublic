// WindowExtensions.prg

FUNCTION YieldMessageLoop( oWin AS OBJECT ) AS VOID
    LOCAL msg  IS _WINMSG
    LOCAL hWnd AS PTR

    IF oWin is IGuiObject
        LOCAL oGui := (IGuiObject) oWin
        hWnd := oGui:__Handle
        DO WHILE ( GuiWin32.PeekMessage( @msg, hWnd, 0, 0, PM_REMOVE ) )
            GuiWin32.TranslateMessage( @msg )
            GuiWin32.DispatchMessage( @msg )
        ENDDO
    ENDIF
    RETURN

