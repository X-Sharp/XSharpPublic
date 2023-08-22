// 415. error XS0149: Method name expected
FUNCTION Start( ) AS VOID
	TestClass{}
	AnotherClass{}
RETURN

CLASS TestClass INHERIT System.Windows.Forms.Form
	CONSTRUCTOR()
		SUPER()
		? Left("abc" , 1)
	RETURN
END CLASS



CLASS AnotherClass
	PROPERTY Test AS INT AUTO
	CONSTRUCTOR()
		SUPER()
		? Test()
	RETURN
	
END CLASS

FUNCTION Test() AS INT
RETURN 123

