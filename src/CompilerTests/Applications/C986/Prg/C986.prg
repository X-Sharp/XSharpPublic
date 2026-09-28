// 986. Missing compiler error on overriding property (implementing interface) that is not virtual #2028
// https://github.com/X-Sharp/XSharpPublic/issues/2028

// a compiler error should be reported on using ovverride in a non-virtual property

/*
System.TypeLoadException
Declaration referenced in a method implementation cannot be a final method.  Type: 'ImplementClass'.  Assembly: 'C986, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null'.
*/

// any dialect, no compiler options used
INTERFACE ITest
	PROPERTY TestProperty1 AS INT AUTO
	PROPERTY TestProperty2 AS INT AUTO
END INTERFACE

CLASS Parent IMPLEMENTS ITest
	PROPERTY TestProperty1 AS INT AUTO
	PROPERTY TestProperty2 AS INT AUTO
END CLASS

CLASS ImplementClass INHERIT Parent // IMPLEMENTS ITest
	OVERRIDE PROPERTY TestProperty1 AS INT AUTO
	VIRTUAL OVERRIDE PROPERTY TestProperty2 AS INT AUTO
END CLASS

FUNCTION Start() AS VOID
	ImplementClass{}
