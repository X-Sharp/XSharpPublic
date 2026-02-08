#ifdef NET5_0_OR_GREATER
USING System.Runtime.InteropServices
ENUM MessageBoxButtons AS INT
    OK                := 0
    OKCancel          := 1
    AbortRetryIgnore  := 2
    YesNoCancel       := 3
    YesNo             := 4
    RetryCancel       := 5
END ENUM
ENUM MessageBoxIcon AS INT
    None        := 0
    Hand        := 16
    Question    := 32
    Exclamation := 48
    Asterisk    := 64
    Stop        := 16
    Error       := 16
    Warning     := 32
    Information := 64
END ENUM
ENUM MessageBoxDefaultButton AS INT
    Button1 := 0
    Button2 := 0x100
    Button3 := 0x200
END ENUM
ENUM MessageBoxOptions AS INT
    ApplicationModal  := 0x0
    SystemModal         := 0x1000
    TaskModal           := 0x2000
    DefaultDesktopOnly  := 0x20000
    RightAlign          := 0x80000
    RtlReading          := 0x100000
    ServiceNotification := 0x200000
END ENUM

ENUM MessageBoxResult AS INT
    OK        := 1
    Cancel    := 2
    Abort     := 3
    Retry     := 4
    Ignore    := 5
    Yes       := 6
    No        := 7
END ENUM

/// <exclude />
FUNCTION MessageBoxShow(lpText AS STRING, lpCaption AS STRING, uType AS DWORD) AS INT
    RETURN Win32.MessageBox(IntPtr.Zero, lpText, lpCaption, uType)

INTERNAL CLASS Win32
    [DllImport("User32.dll")];
    STATIC METHOD MessageBox(hwnd AS IntPtr, lpText AS STRING, lpCaption AS STRING, uType AS DWORD) AS INT
END CLASS






#endif
