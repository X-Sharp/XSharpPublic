// 146. error XS1737: Optional parameters must appear after all required parameters
FUNCTION Start() AS VOID
LOCAL o AS TestClass
o := TestClass{}
o:DoTest(1 ,2 , 3)
o:DoTest(1 , , 3)

CLASS TestClass
	METHOD DoTest(n AS INT , m := 222 AS INT,k AS INT) AS VOID
		? n,m,k
END CLASS

