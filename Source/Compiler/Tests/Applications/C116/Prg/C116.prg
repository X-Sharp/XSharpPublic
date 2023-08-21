// 116. error XS0246: The type or namespace name 'TestClass' could not be found
// compile with /ns:ns1.ns2
BEGIN NAMESPACE ns1.ns2
CLASS TestClass
END CLASS
END NAMESPACE

CLASS AnotherClass
	PROTECT o AS TestClass
END CLASS

