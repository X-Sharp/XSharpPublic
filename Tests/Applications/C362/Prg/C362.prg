// 362. missing pinning of locals in unsafe generated code
FUNCTION Start() AS VOID
Test()
RETURN

FUNCTION Test() AS VOID
  LOCAL DIM pTemp[10] AS BYTE
  //#error pTemp is not pinned
  Split(PSZ(_CAST, @pTemp[1]))
  ? pTemp[1],pTemp[2],pTemp[3],pTemp[4]

FUNCTION Split(p AS PSZ) AS VOID
  MemCopy( p, String2Psz( "ABC" ), 4 )

/*
Vulcan generates this for the Test() function:

fixed (byte[] pTemp = new byte[10])
{
    Functions.Split(&pTemp[0]);
}

[0] uint8[] pinned pTemp
....
ldc.i4.0
ldelema [mscorlib]System.Byte
call valuetype [VulcanRTFuncs]Vulcan.__Psz [VulcanRTFuncs]Vulcan.__Psz::op_Implicit(uint8*)
call void vulcan.Exe.Functions::Split(valuetype [VulcanRTFuncs]Vulcan.__Psz)

x# generates:

byte[] pTemp = new byte[10];
Functions.Split(&pTemp[0]);

[0] uint8[] pTemp
....
ldc.i4.0
ldelema [mscorlib]System.Byte
conv.u
call valuetype [VulcanRTFuncs]Vulcan.__Psz [VulcanRTFuncs]Vulcan.__Psz::op_Implicit(uint8*)
call void C362.Exe.Functions::Split(valuetype [VulcanRTFuncs]Vulcan.__Psz)

*/

