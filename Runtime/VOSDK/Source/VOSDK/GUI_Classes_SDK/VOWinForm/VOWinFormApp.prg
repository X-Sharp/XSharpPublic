#using System.Collections

CLASS Vulcan.VOWinFormApp INHERIT App
    PRIVATE winForms := HashTable{} AS HashTable

    METHOD RegisterWinForm(form AS System.Windows.Forms.Form) AS VOID
        winForms:Add(form:Handle:ToString(),form)
        RETURN
        
    METHOD UnRegisterWinForm(form AS System.Windows.Forms.Form) AS VOID
        winForms:Remove(form:Handle:ToString())
        RETURN
        
	METHOD BeforeDispatch(hWnd, uMsg, wParam, lParam) 
       
        // If we attempt to process all messages the window will appear to hang...
        // And in testing only KeyDown messages seem to require attention
        IF uMsg == WM_KEYDOWN 

            // Look through the ownership chain to confirm this is message has occurred in a registered windows form
	        LOCAL form AS System.Windows.Forms.Form

            LOCAL tempHandle AS IntPtr
            tempHandle := (IntPtr)hWnd
            DO WHILE ! tempHandle == NULL
                form := (System.Windows.Forms.Form)SELF:winForms[tempHandle:ToString()]
                IF form == NULL
                    tempHandle := GetParent(tempHandle)
                ELSE
                    tempHandle := NULL
                ENDIF
            ENDDO
            
	        IF ! form == NULL
	        
                LOCAL netMsg AS System.Windows.Forms.Message
                netMsg := System.Windows.Forms.Message{}
                netMsg:HWnd := hWnd
                netMsg:Msg := uMsg
                netMsg:WParam := (IntPtr)((Int32)wParam)
                netMsg:LParam := (IntPtr)((Int32)lParam)
                
                LOCAL control AS System.Windows.Forms.Control
                // Find the .NET object corresponding to the handle
                // Could be a control, or could be the owning window
                // If there isn't one look up the ownership chain, 
                // will have to eventually hit an owning control or failing that, the owning window
                DO WHILE control == NULL
                    control := System.Windows.Forms.Control.FromHandle(hWnd)
                    hWnd := GetParent(hWnd)
                ENDDO
                
                // If PreProcessMessage didn't handle it, let it through to VO 
                IF ! control:PreProcessMessage(netMsg)
                    RETURN TRUE
                ELSE
                    // Ctrl-Tab needs to be passed through to VO for processing
                    IF wParam == VK_TAB
                        IF GetAsyncKeyState(VK_CONTROL) < 0
                            RETURN TRUE
                        ELSE
                            // Returning TRUE causes the "ding" as VO tries to reprocess the TAB
                            // TAB has already been handled by PreProcessMessage
                            RETURN FALSE
                        ENDIF
                    ELSE
                        // Returning TRUE here allows the keydown event of OLE controls to fire
                        RETURN TRUE
                    ENDIF
                ENDIF
                    
            ENDIF
            
        ENDIF
	    
	    RETURN TRUE
	    
	    
	    
END CLASS
