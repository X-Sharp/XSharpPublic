// 489. error XS1737: Optional parameters must appear after all required parameters

USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

FUNCTION Start() AS VOID
Test1()
Test1("test")
Test2("test",123)
Test2(,123)
Test3("test" ,456)
Test3(,456)
Test4(FALSE , 789)
Test4( , 789)
Test5(1 , 2)
RETURN

FUNCTION Test1(c := ("asd") AS STRING) AS VOID // ok
	? c
RETURN
FUNCTION Test2(c := "asd" AS STRING ,  i AS INT) AS VOID // ok
	? c
RETURN
FUNCTION Test3(c := ("asd") AS STRING , i AS INT) AS VOID // error
	? c , i
RETURN
FUNCTION Test4(l := (TRUE) AS LOGIC , i AS INT) AS VOID // error
	? l , i
RETURN
FUNCTION Test5(n := (1) AS INT , i AS INT) AS VOID // error
	? n , i
RETURN

// original report
BEGIN NAMESPACE ConsoleTestApp

	FUNCTION Start_Original() AS VOID
        Console.WriteLine("Hello World!")
        Console.WriteLine("Press any key to continue...")
        Console.ReadKey()
	
#DEFINE dwNumberDefineNoBracket 10
#DEFINE dwNumberDefineWithBracket 10

	FUNCTION FunctionWIthDefaultParam(;
	dwNumber := 10 AS DWORD;
	) AS VOID

	RETURN

FUNCTION FunctionWIthDefaultParamAsDefineWithNoBrackets(;
	cOther AS STRING;
	,dwNumber := dwNumberDefineNoBracket AS DWORD;
	,cOther2 := "" AS STRING;
	) AS VOID

	RETURN

FUNCTION FunctionWIthDefaultParamAsDefineWithBrackets(;
	cOther AS STRING;
	,dwNumber := dwNumberDefineWithBracket AS DWORD;
	,cOther2 := "" AS STRING;
	) AS VOID

	RETURN
  
END NAMESPACE

