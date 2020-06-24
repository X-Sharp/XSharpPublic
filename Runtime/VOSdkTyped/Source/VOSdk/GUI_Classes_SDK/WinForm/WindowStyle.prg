// WindowStyle.prg



#USING System.Text
INTERNAL CLASS WindowStyle

	STATIC METHOD SetStyle(hWnd AS IntPtr, dwSetStyle AS DWORD, lEnable AS LOGIC) AS VOID
		LOCAL dwStyle AS DWORD
		
		IF (hWnd != IntPtr.Zero)
			dwStyle := DWORD(_CAST, GuiWin32.GetWindowLong(hWnd, GWL_STYLE))
			
			IF lEnable
				dwStyle := (DWORD)_OR(dwStyle, dwSetStyle)
			ELSE
				dwStyle := (DWORD)_AND(dwStyle, _NOT(dwSetStyle))
			ENDIF
			
			GuiWin32.SetWindowLong(hWnd, GWL_STYLE, LONG(_CAST, dwStyle))
			GuiWin32.UpdateWindow(hWnd)
		ENDIF
		
		RETURN 
	
	STATIC METHOD SetExStyle(hWnd AS IntPtr, dwSetStyle AS DWORD, lEnable AS LOGIC) AS VOID
		LOCAL dwStyle AS DWORD
		
		IF (hWnd != IntPtr.Zero)
			dwStyle := DWORD(_CAST, GuiWin32.GetWindowLong(hWnd, GWL_EXSTYLE))
			
			IF lEnable
				dwStyle := (DWORD)_OR(dwStyle, dwSetStyle)
			ELSE
				dwStyle := (DWORD)_AND(dwStyle, _NOT(dwSetStyle))
			ENDIF
			
			GuiWin32.SetWindowLong(hWnd, GWL_EXSTYLE, LONG(_CAST, dwStyle))
		ENDIF
		
		RETURN
		
	STATIC METHOD ClassName(hWnd AS IntPtr) AS STRING
	   LOCAL pszName 	AS StringBuilder
		
	   pszName := StringBuilder{128}
	   GuiWin32.GetClassName(hWnd,pszName,pszName:Capacity-1)
	   RETURN pszName:ToString()

END CLASS
