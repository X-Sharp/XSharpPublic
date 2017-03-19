// 461. Compiler crashes with invalid syntax
FUNCTION Start() AS VOID
TestClass2{}:Test()

INTERFACE ITest1
	METHOD Test() AS VOID
END mybad // <- compiler crash


INTERFACE ITest2
	METHOD Test(a b) AS VOID
END // <- compiler crash


INTERFACE ITest3
METHOD Test(INT a,INT b) AS VOID // <- compiler crash
END INTERFACE


CLASS TestClass
METHOD Test(INT a,INT b) AS VOID // <- compiler crash
END CLASS


CLASS TestClass2
METHOD Test() AS VOID
SELF:DelegateTest({ |o,e| ;  // <- compiler crash due to the ";"
? o,e
RETURN
} )
METHOD DelegateTest(o AS EventHandler) AS VOID
o:Invoke(123,NULL)
END CLASS

