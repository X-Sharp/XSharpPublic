FUNCTION Start() AS VOID
LOCAL DIM cBuffer[512]	IS BYTE // change to IS, compiler will report a correct error but will report it twice
LOCAL dwLen := 512		IS DWORD // warning XS0184
? cBuffer[10] 		// 0, OK   
cBuffer[10] := 17
? cBuffer[10]		// 17, OK
? dwLen				// FALSE ?
? dwLen:GetType():ToString() // System.Boolean
dwLen := 17
? dwLen				// 17, OK                             

RETURN

GLOBAL dw2 := 512 IS DWORD  

CLASS Foo
	PUBLIC x := 512 IS DWORD
END CLASS	1

