USING System
USING System.Collections.Generic
USING System.Linq
using System.Text

BEGIN NAMESPACE ConsoleApplication1

	FUNCTION Start() AS VOID
		local i as int
		local j as byte
		local p as IntPtr
		local r8 as real8
		p := MemAlloc(1000)
		r8 := Seconds()
		for i := 1 to 500000
			for j := 1 to 254
				memset(p, j, 1000)
			next
		next
		? "Done", Seconds() - r8
		MemFree(p)
		Console.ReadLine()
END NAMESPACE



