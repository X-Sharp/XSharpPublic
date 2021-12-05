//  error XS0111: Type 'R816.Exe.Functions' already defines a member 
// called '_CallClipFunc' with the same parameter 

FUNCTION Start( ) AS VOID
	System.Console.WriteLine("Hello x#!")
RETURN

FUNCTION _CallClipFunc(symFunction AS STRING,aArgs AS ARRAY) AS USUAL
    RETURN 0
FUNCTION _CallClipFunc(symFunction AS STRING,aArgs PARAMS USUAL[]) AS USUAL
    RETURN 0            
    
FUNCTION _CallClipFunc(symFunction,aArgs) AS USUAL
    RETURN 0



