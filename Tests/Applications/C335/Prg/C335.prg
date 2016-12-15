// 335. error XS0120: An object reference is required for the non-static field, method, or property 'object.ToString()'

CLASS Window
END CLASS

CLASS WinFormVOWindowHost
	PROTECT window AS Window
	
	METHOD Test() AS VOID
		window := Window{}
		? window:ToString()
	RETURN
END CLASS

FUNCTION Start() AS VOID
WinFormVOWindowHost{}:Test()

