// 856. Problems with ASYNC/AWAIT
// https://github.com/X-Sharp/XSharpPublic/issues/1049
// vo3+
/*
System.AccessViolationException
Attempted to read or write protected memory. This is often an indication that other memory is corrupt.
*/

FUNCTION Start() AS VOID
	MultiThreadScan{}:OrderActioningScan(123)
	MultiThreadScan{}:OrderActioningScan(42)
	Console.ReadKey()
RETURN

CLASS MultiThreadScan
	ASYNC METHOD OrderActioningScan(loActionFileObject AS INT) AS VOID
	var result := AWAIT System.Threading.Tasks.Task.Run({=> SELF:CallCreateActionFile(loActionFileObject)})
	? result
	return

	METHOD CallCreateActionFile(loActionFileObject AS INT) AS INT
	? loActionFileObject
	return loActionFileObject * 2
END CLASS

