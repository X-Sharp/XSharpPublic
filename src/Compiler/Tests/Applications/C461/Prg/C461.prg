// 461. Compiler crashes with invalid syntax

// obviously most of the following code snippets should result to compiler errors
// but currently the compiler crashes with them


DEFINE testdefine := }{1,2,3} // <- compiler crash

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



CLASS TestClass3
	METHOD asd?aas() AS VOID // <- compiler crash
	RETURN
END CLASS


DEFINE aaa?bbb := 1 // <- compiler crash
CLASS TestClass4
CONSTRUCTOR()
? aaa?bbb // <- compiler crash
RETURN
END CLASS


CLASS TestClass5
	METHOD Test() AS VOID
		SELF:Test())  // <- compiler crash
	RETURN
END CLASS
