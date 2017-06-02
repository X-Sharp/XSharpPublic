// 526. Compiler crash with local with the same name with type
USING System.Data

FUNCTION Start( ) AS VOID
LOCAL test AS Test
test := Test{}
? test:ToString()
	
// original code
LOCAL DataTable AS DataTable 
DataTable := DataTable{}
DataTable:Columns:Add("test")
DataTable:Rows:Add(<OBJECT>{1})
RETURN

CLASS Test
END CLASS
