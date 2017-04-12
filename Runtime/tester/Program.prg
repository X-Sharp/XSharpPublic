USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XSharp
BEGIN NAMESPACE tester

	FUNCTION Start() AS VOID
        local  d1 as __VoDate
        local  d2 as __VoDate
        local d as dword
        local l as long
        d2 := __VoDate{2017,4,12}
        d1 := __VoDate{1959,6,8}
        ? d2
        ? d1
        ? d2 - d1
        ? d2 > d1
        ? d2 >= d1
        ? d2 < d1
        ? d2 <= d1
        ? d2 == d1
        ? d2 != d1
        l := d1
        d1 := l
        d := (dword) d1
        d1 := (__VODate) d
        ? (DWORD) d1
        ? (DWORD) d2
        d2 += 1
        ? d2
        console.Read()
	
END NAMESPACE
