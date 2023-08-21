// 706. Problem using indexed properties on usuals
// To properly fix this we need to add something like
// IVarGetCollection(oObject as Object, cProperty as String, uPars params usual[])
// This function should retrieve the property and check if it is an array (in which case the uPars must be numeric)
// or if it is an indexed property

CLASS TestClass
	ACCESS Test1(c AS STRING) AS STRING
	RETURN c
	ACCESS Test2(c1 AS STRING, c2 AS STRING) AS STRING
	RETURN c1 + c2
	ACCESS Test3(c1 AS STRING, c2 AS STRING, c3 AS STRING) AS STRING
	RETURN c1 + c2 + c3
END CLASS

FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	? o:Test1["abc"] // ok
	? o:Test2["abc","def"] // ok
	? o:Test3["abc","def","ghi"] // ok

	LOCAL u AS USUAL
	u := o

	// Runtime: Unhandled Exception: System.InvalidCastException: value in USUAL does not support indexed operations. The value needs to be an X# Array or implement the interface 'XSharp.IIndexedProperties'.
	? u:Test1["abc"]


	// error XS9059: Cannot convert Array Index from 'string' to 'int'.
	? u:Test2["abc","def"]

	? u:Test1["abc","def","ghi"]


RETURN
