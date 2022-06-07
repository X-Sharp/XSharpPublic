// 856. Problems with ASYNC/AWAIT
// vo3+
/*
System.AccessViolationException
Attempted to read or write protected memory. This is often an indication that other memory is corrupt.
*/
FUNCTION Start() AS VOID
	MultiThreadScan{}:OrderActioningScan(123)
	Console.ReadKey()
RETURN

CLASS MultiThreadScan 
	ASYNC METHOD OrderActioningScan(loActionFileObject AS INT) AS VOID
	AWAIT System.Threading.Tasks.Task.Run({=> SELF:CallCreateActionFile(loActionFileObject)})
	
	METHOD CallCreateActionFile(loActionFileObject AS INT) AS VOID
	? loActionFileObject
END CLASS

