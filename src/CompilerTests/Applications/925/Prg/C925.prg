// 925. Incorrect compiler cast with abstract collection
// https://github.com/X-Sharp/XSharpPublic/issues/1593
// Following code used to run correctly in previous X# builds, but at least starting with 2.20 (or a build prior to that), it stopped, now throwing an ArgumentOutOfRangeException:


CLASS KeyedCollectionSubclass INHERIT System.Collections.ObjectModel.KeyedCollection<INT, STRING>
	PROTECTED METHOD GetKeyForItem(o AS STRING) AS INT
	RETURN 123
END CLASS

FUNCTION Start() AS VOID
	LOCAL col AS KeyedCollectionSubclass
	col := KeyedCollectionSubclass{}
	col:Add("abc")
	LOCAL o AS OBJECT

	// this throws an index out of range exception in 2.20
	o := col[123] // abc in 2.14
	? o 
	xAssert(col[123] == "abc")

/*	
	// this works in 2.20, but it is wrong
	o := col[0] // abc in 2.20
	? o 
*/

PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN
