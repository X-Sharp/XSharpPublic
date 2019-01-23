// 655. Incorrect line/col error number for missing type in INHERIT and IMPLEMENTS clause
/*
The compiler reports the errors with this info:

C:\xSharp\Dev\Tests\Applications\C655\Prg\C655.prg(11,1): error XS0246: The type or namespace name 'Unknown' could not be found (are you missing a using directive or an assembly reference?)
C:\xSharp\Dev\Tests\Applications\C655\Prg\C655.prg(11,1): error XS0246: The type or namespace name 'IUnknown' could not be found (are you missing a using directive or an assembly reference?)

Note that in both messages, the line/col info point to the Start() function, not the correct lines (14 and 17)
*/

FUNCTION Start( ) AS VOID
RETURN

CLASS TestClass INHERIT Unknown
END CLASS

CLASS TestClass2 IMPLEMENTS IUnknown
END CLASS

