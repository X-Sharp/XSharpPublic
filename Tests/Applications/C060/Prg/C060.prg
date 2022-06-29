// 60. error XS0542: 'TestClass': member names cannot be the same as their enclosing type
#pragma warnings(542, off) // field name same as class name
CLASS TestClass
	CLASS TestClass
		PROTECT TestClass AS INT
	END CLASS
END CLASS

