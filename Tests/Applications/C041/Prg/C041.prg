// 41. error XS1112: Do not use ExtensionAttribute. Use the 'this' keyword instead.
#pragma warnings(165, off)    // use of unassigned local variable
#pragma warnings(1112, off)    // do not use extension attribue
FUNCTION Start AS VOID
	LOCAL i AS INT
	? i:MyExtension()
	Console.ReadLine()
	RETURN

PUBLIC STATIC CLASS Extensions
[System.Runtime.CompilerServices.Extension] ;
PUBLIC STATIC METHOD MyExtension( n AS INT ) AS INT
RETURN 0
END CLASS

