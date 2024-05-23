// 906. Incorrect Compiler incorrectly resolves static member of another type to SELF property when /allowdot is enabled
// https://github.com/X-Sharp/XSharpPublic/issues/1460

// error XS1061: 'int' does not contain a definition for 'Val1' and no accessible extension method 'Val1' accepting a first argument of type 'int' could be found 

// /allowdot+
USING System.Reflection

FUNCTION Start() AS VOID

RETURN

CLASS Test1
   PROPERTY Test AS INT AUTO GET SET
   PROPERTY BindingFlags AS INT AUTO GET SET
   PROPERTY TestStatic AS INT AUTO GET SET

	METHOD TestMethod() AS VOID
		LOCAL e AS OBJECT
		? SELF:Test > Test.Val1 // error XS1558
		e := BindingFlags.CreateInstance // error XS1558
		? TestStatic.StaticExport // error XS1558
END CLASS


PUBLIC ENUM Test
	MEMBER Val1
	MEMBER Val2
	MEMBER Val3
END ENUM

STATIC CLASS TestStatic
	STATIC EXPORT StaticExport AS INT
END CLASS
