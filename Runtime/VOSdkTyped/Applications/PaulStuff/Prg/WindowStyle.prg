// WindowStyle.prg

INTERNAL CLASS WindowStyle

    STATIC METHOD SetStyle(hWnd AS PTR, dwSetStyle AS DWORD, lEnable AS LOGIC) AS VOID
        LOCAL dwStyle AS DWORD
        
        IF (hWnd != NULL_PTR)
            dwStyle := DWORD(_CAST, GetWindowLong(hWnd, GWL_STYLE))
            
            IF lEnable
                dwStyle := (DWORD)_OR(dwStyle, dwSetStyle)
            ELSE
                dwStyle := (DWORD)_AND(dwStyle, _NOT(dwSetStyle))
            ENDIF
            
            SetWindowLong(hWnd, GWL_STYLE, LONG(_CAST, dwStyle))
            UpdateWindow(hWnd)
        ENDIF
        
        RETURN 
    
    STATIC METHOD SetExStyle(hWnd AS PTR, dwSetStyle AS DWORD, lEnable AS LOGIC) AS VOID
        LOCAL dwStyle AS DWORD
        
        IF (hWnd != NULL_PTR)
            dwStyle := DWORD(_CAST, GetWindowLong(hWnd, GWL_EXSTYLE))
            
            IF lEnable
                dwStyle := (DWORD)_OR(dwStyle, dwSetStyle)
            ELSE
                dwStyle := (DWORD)_AND(dwStyle, _NOT(dwSetStyle))
            ENDIF
            
            SetWindowLong(hWnd, GWL_EXSTYLE, LONG(_CAST, dwStyle))
        ENDIF
        
        RETURN
        
    STATIC METHOD ClassName(hWnd AS PTR) AS STRING
	    LOCAL pszName 	AS PSZ
	    LOCAL cName 	AS STRING
    	
	    pszName := PSZ(_CAST,MemAlloc(128))
	    GetClassName(hWnd,pszName,128)
	    cName := Psz2String(pszName)
	    MemFree(PTR(_CAST,pszName))
    	
	    RETURN cName

END CLASS
