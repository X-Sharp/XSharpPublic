// R776 - Incorrect calculation of the size of the VOSTRUCT
// https://github.com/X-Sharp/XSharpPublic/issues/635
// please note that the compiler now checks the platform when compiling the _sizeof() operator.
// for platform x86 it assumes size of ptr types = 4
// for platform x64 it assumes size of ptr types = 8
// for platform anycp it does not generate a constant for _sizeof() but calculates the size at runtime
// for the special [VoStruct(..)] attribute the compiler now has the following logic
// for platform x86 it assumes size of ptr = 4  . So the attribute for the structure below becomes [VoStruct(16, 4)]
// for all other platforms it assumes size of ptr = 8. The attribute becomes [VoStruct(28, 8)]
// this example is compiled in x86 mode
STATIC DEFINE DARRAYLEN := _SIZEOF(DARRAY_ROW)

STATIC VOSTRUCT DARRAY_ROW
	MEMBER dwSize AS DWORD
	MEMBER pData AS BYTE PTR
	MEMBER pTop AS DARRAY_ROW PTR
	MEMBER pBottom AS DARRAY_ROW PTR           
	

FUNCTION Start() AS VOID STRICT
    xAssert(IntPtr.Size == 4)
	? DARRAYLEN     
	xAssert(DARRAYLEN      == 16)
	? _SIZEOF(DARRAY_ROW) 
	xAssert(_SIZEOF(DARRAY_ROW)  == 16)
	LOCAL dwSize AS DWORD
    dwSize := _SIZEOF(DARRAY_ROW)              
    ? dwSize
    xAssert(dwSize == 16)
 
	RETURN
	
	
PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF	
