
FUNCTION Start( ) AS VOID
	LOCAL oTest AS Test
	oTest := Test{}
	? oTest:TestMe()
RETURN


CLASS Test          
    METHOD TestMe() AS STRING
        ? SubStr("Substr Function",1,20)
        RETURN Left("Left Function",13)
        
    METHOD Left(cString AS STRING, nLen AS DWORD) AS STRING
        RETURN "SELF:LEFT"     
    STATIC METHOD SubStr(cString AS STRING, nStart AS DWORD, nLen AS DWORD) AS STRING
        RETURN "Test.SubStr"
END CLASS    
