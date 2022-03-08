#pragma options("vo4", on)
DEFINE FAPPCOMMAND_MASK := 0xF000
FUNCTION Start( ) AS VOID
	System.Console.WriteLine("Hello x#!")
RETURN


FUNCTION Get_AppCommand_lParam(lParam AS DWORD) AS DWORD
	RETURN _AND(HiWord(lParam),_NOT(FAPPCOMMAND_MASK))



FUNCTION SwapBytes(nShort AS SHORT) AS Short
    RETURN _OR( _AND(nShort, 0xFF) <<8, _AND(nShort >>8, 0xFF))
DEFINE UUE_START_NEWS            := "begin"    
    
function TestMe as string                                         
    local cTemp as string
    local nRet as dword                    
    local c := "aaabeginbbb" as string
    nRet := 1
    cTemp   := SubStr(c, nRet + SLen(UUE_START_NEWS), 5)    
    return cTemp
