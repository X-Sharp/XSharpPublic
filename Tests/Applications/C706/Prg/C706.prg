// 706. Problem using indexed properties on usuals
CLASS TestClass
	ACCESS Test1(c AS STRING) AS STRING
	RETURN c
	ACCESS Test2(c1 AS STRING, c2 AS STRING) AS STRING
	RETURN c1 + c2
END CLASS

FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	? o:Test1["abc"] // ok
	? o:Test2["abc","def"] // ok
	
	LOCAL u AS USUAL
	u := o

	// Runtime: Unhandled Exception: System.InvalidCastException: value in USUAL does not support indexed operations. The value needs to be an X# Array or implement the interface 'XSharp.IIndexedProperties'.
	? u:Test1["abc"]

	// error XS9059: Cannot convert Array Index from 'string' to 'int'.
	? u:Test2["abc","def"] 
RETURN
