// 756. error XS0104: ambiguous reference
/*
In the previous compiler, a warning was being reported:

warning XS9043: '_WINNMCUSTOMDRAW' is ambiguous. Could be  NamedType '_WINNMCUSTOMDRAW' in C756 or NamedType '_winNMCUSTOMDRAW' in C756_helper. Using the first one.

In the new one, those errors are reported:

error XS0104: '_WINNMCUSTOMDRAW' is an ambiguous reference between '_WINNMCUSTOMDRAW' and '_winNMCUSTOMDRAW'
error XS0118: '_WINNMCUSTOMDRAW' is a element but is used like a VOSTRUCT/UNION
error XS9027: VoStruct member type must be one of the following: bool, byte, short, int, long, char, sbyte, ushort, uint, ulong, float, double, ptr, psz or vostruct
*/
#pragma warnings(9043, off) //   ambiguos

FUNCTION Start() AS VOID

RETURN

VOSTRUCT _WINNMTTCUSTOMDRAW
	MEMBER nmcd IS _WINNMCUSTOMDRAW // error
	MEMBER uDrawFlags AS DWORD

VOSTRUCT _WINNMCUSTOMDRAW
	MEMBER lItemlParam AS LONG

