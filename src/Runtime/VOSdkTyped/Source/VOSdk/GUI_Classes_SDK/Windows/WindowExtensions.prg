//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// WindowExtensions.prg

FUNCTION YieldMessageLoop( oWin AS OBJECT ) AS VOID
    LOCAL msg  IS _WINMSG
    LOCAL hWnd AS PTR

    IF oWin is IGuiObject
        VAR oGui := (IGuiObject) oWin
        hWnd := oGui:__Handle
        DO WHILE ( GuiWin32.PeekMessage( @msg, hWnd, 0, 0, PM_REMOVE ) )
            GuiWin32.TranslateMessage( @msg )
            GuiWin32.DispatchMessage( @msg )
        ENDDO
    ENDIF
    RETURN

