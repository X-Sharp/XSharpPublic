USING System
USING System.Collections.Generic
USING System.Linq
using System.Text

BEGIN NAMESPACE ConsoleApplication1

	FUNCTION Start() AS VOID
		SetEpoch(1900)
		? ConDate(18,3,15)
		SetEpoch(2000)
		? ConDate(18,3,15)
		test()
		Console.ReadLine()


	function test as void
		? Procname(-2)
		? Procname(-1)
		? Procname()
		? Procname(1)
		? Procname(2)
		? ProcLine()
		? ProcFile()

END NAMESPACE



