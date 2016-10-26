// 63. error XS0246: The type or namespace name 'TestClass' could not be found
// I never liked this behavior, but in vulcan this works
BEGIN NAMESPACE ns
CLASS TestClass
END CLASS
FUNCTION Test() AS VOID
LOCAL o AS TestClass
END NAMESPACE

