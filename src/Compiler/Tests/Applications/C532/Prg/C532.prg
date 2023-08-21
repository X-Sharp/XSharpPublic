// error XS0246: The type or namespace name 'DataGridView' could not be found
#using System.Windows.Forms
#pragma warnings(168, off) // declared but not used
CLASS NestClass
	CLASS Nested
	END CLASS
END CLASS

FUNCTION Start( ) AS VOID
	LOCAL o1 AS NestClass.Nested
	LOCAL o2 AS DataGridView.HitTestInfo
RETURN

// originl problem:
CLASS TestClass
	EXPORT info AS DataGridView.HitTestInfo
END CLASS
