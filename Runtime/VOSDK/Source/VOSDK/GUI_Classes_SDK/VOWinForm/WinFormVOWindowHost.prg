using System.ComponentModel
/// <summary>This class is used to host a VO GUI Window in a Windows Forms UI Hierarchy</summary>
CLASS XSharp.WinFormVOWindowHost INHERIT Component
    PRIVATE components := NULL AS System.ComponentModel.IContainer
    PRIVATE window AS Window
    PRIVATE windowClassName AS STRING
    PRIVATE hostControl AS System.Windows.Forms.Control
    PRIVATE classNeedTranslateTabToArrow := <STRING>{CONTAINER_CLASS:ToUpper()} AS STRING[]

    /// <inheritdoc/>
    CONSTRUCTOR() STRICT
        SUPER()

        SELF:InitializeComponent()

        RETURN

    /// <inheritdoc/>
    CONSTRUCTOR(container AS IContainer) STRICT
        SUPER()

        container:Add(SELF)
        SELF:InitializeComponent()

        RETURN
    /// <summary>Create w WinFormVOHostwindow </summary>
    CONSTRUCTOR(VOWindow AS Window,host AS System.Windows.Forms.Control)
        SUPER()

        SELF:InitializeComponent()

        window := VOWindow
        windowClassName := window:ToString()
        hostControl := host
        
        SELF:UpdateHost()
        
        RETURN
        
    /// <inheritdoc/>
    PROTECTED METHOD Dispose( disposing AS LOGIC ) AS VOID
        IF disposing .AND. (components != NULL)
            SELF:Close()
            components:Dispose()
        ENDIF
        
        SUPER:Dispose(disposing)
        
        RETURN
        
    PRIVATE METHOD InitializeComponent() AS VOID STRICT
    
        components := System.ComponentModel.Container{}
        
        SELF:UpdateHost()
        
        RETURN

    PRIVATE METHOD UpdateHost() AS VOID STRICT
        IF ! SELF:DesignMode
            IF window == NULL .AND. ! String.IsNullOrEmpty( windowClassName )
                TRY
                    window := (Window)CreateInstance(windowClassName)
                CATCH
                    window := NULL
                END TRY
            ENDIF
            
            IF ! window == NULL .AND. ! hostControl == NULL
            
                IF SELF:IsHostingDataWindow()
                    ((DataWindow)window):EnableBorder(WINDOWNONSIZINGBORDER)
                    ((DataWindow)window):menu := NULL_OBJECT
                    ((DataWindow)window):toolbar := NULL_OBJECT
                ENDIF
                WindowStyle.SetStyle(window:handle(),WS_CHILD,TRUE)
                WindowStyle.SetStyle(window:handle(),WS_CAPTION,FALSE)
                WindowStyle.SetStyle(window:handle(),WS_SYSMENU,FALSE)
                WindowStyle.SetStyle(window:handle(),DS_MODALFRAME,FALSE)
                WindowStyle.SetStyle(window:handle(),WS_THICKFRAME,FALSE)
                WindowStyle.SetStyle(window:handle(),WS_MINIMIZEBOX,FALSE)
                WindowStyle.SetStyle(window:handle(),WS_MAXIMIZEBOX,FALSE)
                WindowStyle.SetStyle(window:handle(),WS_POPUP,FALSE)
                WindowStyle.SetStyle(window:handle(),DS_3DLOOK,FALSE)
                WindowStyle.SetExStyle(window:handle(),WS_EX_DLGMODALFRAME,FALSE)
                WindowStyle.SetExStyle(window:handle(),WS_EX_CONTROLPARENT,TRUE)

                SetParent(window:Handle(),hostControl:Handle)
                SELF:AdjustVOWindow()

                window:Show()

            ENDIF
        ENDIF

        RETURN
    /// <summary>Get/Set the class name of VO window that needs to be hosted</summary>
    /// <remarks>You should either set the classname or the object. Not both </remarks>
    PROPERTY VOWindowClassName AS STRING
        GET 
            RETURN windowClassName
        END GET
        SET 
            windowClassName := value
            SELF:UpdateHost()
        END SET
    END PROPERTY
    /// <summary>Get/Set the object of VO window that needs to be hosted</summary>
    /// <remarks>You should either set the classname or the object. Not both </remarks>
    PROPERTY VOWindow AS Window
        GET 
            RETURN window
        END GET
        SET 
            window := value
            SELF:UpdateHost()
        END SET
    END PROPERTY


    /// <summary>The control that is used to host the VO Window</summary>    
    PROPERTY HostingControl AS System.Windows.Forms.Control
        GET
            RETURN hostControl
        END GET
        SET 
            hostControl := value
            SELF:UpdateHost()
        END SET
    END PROPERTY

    /// <summary>Is the VO Window a DataWindow ?</summary>    
    METHOD IsHostingDataWindow AS LOGIC
        LOCAL result AS LOGIC
        
        IF ! window == NULL
            result := IsInstanceOf(window,#DataWindow)
        ENDIF
        
        RETURN result
        
    /// <summary>Resize VO window</summary>    
    METHOD AdjustVOWindow() AS VOID STRICT
        IF ! window == NULL
            LOCAL windowRect IS _winRect
            GetWindowRect(window:Handle(),@windowRect)

            IF SELF:IsHostingDataWindow()
                LOCAL clientRect IS _winRect
                GetWindowRect(((DataWindow)window):__GetFormSurface():Handle(),@clientRect)
                
                SetWindowPos(window:Handle(),HWND_TOP,-1*(clientRect:Left-windowRect:Left),-1*(clientRect:Top-windowRect:Top),;
                                hostControl:Width+(window:Size:width-window:CanvasArea:size:width)+1,hostControl:Height+(window:Size:height-window:CanvasArea:size:height)+1,SWP_NOACTIVATE)
            ELSE
                SetWindowPos(window:Handle(),HWND_TOP,0,0,hostControl:Width,hostControl:Height,SWP_NOACTIVATE)
            ENDIF
        ENDIF
        
        RETURN
        
    /// <summary>Close the window</summary>
    METHOD Close() AS VOID STRICT
        IF ! window == NULL
            SendMessage(window:Handle(),WM_CLOSE,0,0)
        ENDIF
        
        RETURN

    /// <inheritdoc/>
    VIRTUAL METHOD ProcessDialogKey( keyData AS System.Windows.Forms.Keys ) AS LOGIC
        LOCAL keyProcessed := FALSE AS LOGIC
        LOCAL control AS OBJECT
        LOCAL win AS PTR
        keyProcessed := FALSE
                
        IF ! window == NULL

            LOCAL p AS PTR
            DO CASE
            CASE keyData == System.Windows.Forms.Keys.Up
                keyProcessed := TRUE
                SendMessage(GetFocus(),WM_KEYDOWN,VK_UP,0)
                
            CASE keyData == ( System.Windows.Forms.Keys.Up | System.Windows.Forms.Keys.Shift )
                keyProcessed := TRUE
                SendMessage(GetFocus(),WM_KEYDOWN,VK_SHIFT,0)
                SendMessage(GetFocus(),WM_KEYDOWN,VK_UP,0)
                
            CASE keyData == System.Windows.Forms.Keys.Down
                keyProcessed := TRUE
                SendMessage(GetFocus(),WM_KEYDOWN,VK_DOWN,0)
                
            CASE keyData == ( System.Windows.Forms.Keys.Down | System.Windows.Forms.Keys.Shift )
                keyProcessed := TRUE
                SendMessage(GetFocus(),WM_KEYDOWN,VK_SHIFT,0)
                SendMessage(GetFocus(),WM_KEYDOWN,VK_DOWN,0)
                
            CASE keyData == System.Windows.Forms.Keys.Left
                keyProcessed := TRUE
                SendMessage(GetFocus(),WM_KEYDOWN,VK_LEFT,0)
                
            CASE keyData == ( System.Windows.Forms.Keys.Left | System.Windows.Forms.Keys.Shift )
                keyProcessed := TRUE
                SendMessage(GetFocus(),WM_KEYDOWN,VK_SHIFT,0)
                SendMessage(GetFocus(),WM_KEYDOWN,VK_UP,0)
                
            CASE keyData == System.Windows.Forms.Keys.Right
                keyProcessed := TRUE
                SendMessage(GetFocus(),WM_KEYDOWN,VK_RIGHT,0)
                
            CASE keyData == ( System.Windows.Forms.Keys.Right | System.Windows.Forms.Keys.Shift )
                keyProcessed := TRUE
                SendMessage(GetFocus(),WM_KEYDOWN,VK_SHIFT,0)
                SendMessage(GetFocus(),WM_KEYDOWN,VK_DOWN,0)
                
            CASE keyData == System.Windows.Forms.Keys.Tab
                keyProcessed := TRUE
                IF SELF:InClassList(WindowStyle.ClassName(GetParent(GetFocus())))
                    SendMessage(GetFocus(),WM_KEYDOWN,VK_RIGHT,0)
                ELSE
                    win := GetParent(GetFocus())
                    control := GetObjectByHandle(win)
                    DO WHILE !IsInstanceOf(control,#Window) .AND. (win != NULL)
                       win := GetParent(win)
                       control := GetObjectByHandle(win)
                    ENDDO
                    p := GetNextDlgTabItem(win,GetFocus(),FALSE)
                    SetFocus(p)
                ENDIF

            CASE keyData == (System.Windows.Forms.Keys.Tab | System.Windows.Forms.Keys.Shift)
                keyProcessed := TRUE
                IF SELF:InClassList(WindowStyle.ClassName(GetParent(GetFocus())))
                    SendMessage(GetFocus(),WM_KEYDOWN,VK_LEFT,0)
                ELSE
                    win := GetParent(GetFocus())
                    control := GetObjectByHandle(win)
                    DO WHILE !IsInstanceOf(control,#Window) .AND. (win != NULL)
                       win := GetParent(win)
                       control := GetObjectByHandle(win)
                    ENDDO
                    p := GetNextDlgTabItem(win,GetFocus(),TRUE)
                    SetFocus(p)
                ENDIF


            ENDCASE

        ENDIF
        
        RETURN keyProcessed
    /// <summary>Specify the class names that require special handling for TAB keys.</summary>
    PROPERTY ClassesNeedingTranslateTabToArrow AS STRING[]
    GET 
    
        RETURN SELF:classNeedTranslateTabToArrow
    END GET
    SET 
        LOCAL i AS INT
        FOR i := 1 UPTO value:Length
            value[i] := value[i]:ToUpper()
        NEXT
        SELF:classNeedTranslateTabToArrow := value
        RETURN
    END SET
    END PROPERTY

    PRIVATE METHOD InClassList(c AS STRING)
        LOCAL i AS INT
        LOCAL result := FALSE AS LOGIC

        c := c:ToUpper()
        FOR i := 1 UPTO classNeedTranslateTabToArrow:Length
            IF classNeedTranslateTabToArrow[i] == c
                result := TRUE
                EXIT
            ENDIF
        NEXT
        
        RETURN result

    /// <summary>Set focus to the form and the window that it hosts.</summary>
    METHOD Focus() AS VOID STRICT
        SELF:hostControl:Focus()
        SELF:window:SetFocus()
        RETURN        
        

END CLASS

