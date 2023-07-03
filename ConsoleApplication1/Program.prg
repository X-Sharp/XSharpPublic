USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XSharp.RDD.SqlRDD

FUNCTION Start() AS VOID STRICT
    LOCAL oIni AS IniFile
    oIni := IniFile{"C:\Test\test.ini"}
    oIni:Create()
    FOR VAR i := 1 TO 10
        oIni:WriteString("Section","Entry"+i:ToString(),"value"+i:ToString())
    NEXT
    FOR VAR i := 1 TO 10
        ? oIni:GetString("Section","Entry"+i:ToString(),"")
    NEXT
    VAR values := oIni:GetSection("Section")
    FOREACH VAR strValue IN Values
        ? strValue
    NEXT
    Console.ReadLine()
