// 892. Multiple type constraints in a generic type
// https://github.com/X-Sharp/XSharpPublic/issues/1389

CLASS TestClass1<K, T> WHERE K IS STRUCT WHERE T IS NEW()
	CONSTRUCTOR()
		? typeof(K), typeof(t)
	RETURN
END CLASS

CLASS TestClass2<K, T> WHERE K IS NEW() WHERE T IS STRUCT
	CONSTRUCTOR()
		? typeof(K), typeof(t)
	RETURN
END CLASS

FUNCTION Start( ) AS VOID
	LOCAL o1 AS TestClass1<INT, Test>
	o1 := TestClass1<INT, Test>{}
	LOCAL o2 AS TestClass2<Test, System.Boolean>
	o2 := TestClass2<Test, System.Boolean>{}
RETURN


CLASS Test
	CONSTRUCTOR()
	RETURN
END CLASS
