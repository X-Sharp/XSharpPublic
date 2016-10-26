// 115. error XS0246: The type or namespace name 'ns1' could not be found
// compile with /ns:ns1.ns2
CLASS TestClass
END CLASS
FUNCTION Start() AS VOID
LOCAL o AS ns1.ns2.TestClass
o := NULL
? o

