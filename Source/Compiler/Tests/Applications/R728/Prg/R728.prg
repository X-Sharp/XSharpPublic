#pragma warnings(9100, off) // upper ascii _chr()
DEFINE MyDef1 := "MyDef" + _Chr(150)  // does not work because Chr() and _Chr() need codepage for this
DEFINE MyDef2 := "MyDef" + _Chr(0)
DEFINE MyDef3 := "MyDef" + _Chr(65)

FUNCTION Start as VOID
	? MyDef1
	? MyDef2
	? MyDef3
	RETURN
