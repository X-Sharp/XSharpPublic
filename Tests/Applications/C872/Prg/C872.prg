// 872. #pragma warning disables more warnings than it should
// https://github.com/X-Sharp/XSharpPublic/issues/1133
/*
This code should report those warnings:

C872.prg(8,13): warning XS0169: The field 'ClassWithPragma.test' is never used
C872.prg(14,1): warning XS1591: Missing XML comment for publicly visible type or member 'C872.Exe.Functions.Start()'
C872.prg(6,1): warning XS1591: Missing XML comment for publicly visible type or member 'ClassWithPragma'
C872.prg(10,5): warning XS1591: Missing XML comment for publicly visible type or member 'ClassWithPragma.Execute()'

but none of them are reported. After uncommenting the 2nd pragma, then the 1591 warnins are reported, but the 169 still not
After removing all pragmas, then all warnings are being reported as expected
*/

#pragma warning disable 1591
PUBLIC CLASS ClassWithPragma
	PRIVATE test AS LOGIC
    PUBLIC METHOD Execute() AS VOID
        RETURN
END CLASS

FUNCTION Start() AS VOID

RETURN
