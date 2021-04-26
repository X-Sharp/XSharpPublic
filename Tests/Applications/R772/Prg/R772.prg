// R772: Cannot use DATE in VOSTRUCT
FUNCTION Start( ) AS VOID
	System.Console.WriteLine("Hello x#!")
RETURN

VOSTRUCT MyStruct
MEMBER n AS INT
MEMBER s AS SYMBOL // ok
MEMBER d AS DATE // error
