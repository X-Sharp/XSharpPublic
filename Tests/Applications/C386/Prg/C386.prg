[STAThreadAttribute];
FUNCTION Start( ) AS VOID
DoTest()

PROCEDURE DoTest() STRICT
LOCAL oShell AS ShellWindow
LOCAL oDataWin AS DataWindow
LOCAL oApp AS App

oApp := App{}

oShell := ShellWindow{oApp}
oShell:Show()

oDataWin := DataWindow{ oShell , ResourceID{"Data_WinresID" , _GetInst()} } // ok
oDataWin:Show() 
oDataWin := DataWindow{ oShell , ResourceID{"abcdef" , _GetInst()} } // ok
oDataWin:Show()
oDataWin := DataWindow{ oShell , ResourceID{"abcdefg" , _GetInst()} } // ok
oDataWin:Show()
oDataWin := DataWindow{ oShell , ResourceID{"DataWinresID" , _GetInst()} } // error
oDataWin:Show()
oDataWin := DataWindow{ oShell , ResourceID{"abcdefgh" , _GetInst()} } // error
oDataWin:EndWindow()

oApp:Exec()

