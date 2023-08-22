#command MD <(dir)>    => _cmdDirMake (<(dir)>)

GLOBAL cPath as string
    
// Make sure preprocessor matches all kinds of path expressions
FUNCTION Start( ) AS VOID
    cPath := "C:\test"
    MD C:\test	
    cPath := "C:\test\a1"
    MD C:\test\a1
    cPath := "C:\test\a2"
    MD C:\test\a2
    cPath := "C:\test\1"                      
    MD C:\test\1
    cPath := "C:\test\2020.01.01"                       
    MD C:\test\2020.01.01
    cPath := "C:\test\1.23"                       
    MD C:\test\1.23
    cPath := "C:\test\#Symbol"                       
    MD C:\test\#Symbol

    cPath := "C:\test\SomeName"                       
    MD &cPath
    MD (cPath)
RETURN



FUNCTION _cmdDirMake(cDir as STRING) AS VOID
    xAssert(cPath == cDir)
    RETURN
    
    
PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN       
