GLOBAL gTest := 0 AS USUAL

CLASS ParentDll
	CONSTRUCTOR(a,b,c)
		? a,b,c
		gTest := a
	RETURN
END CLASS

