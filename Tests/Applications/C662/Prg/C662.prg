// 662. error XS0460: Constraints for override and explicit interface implementation methods are inherited from the base method, so they cannot be specified directly
FUNCTION Start( ) AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	o:GenericTest<TestClass>()
RETURN

CLASS TestClass
	 PUBLIC METHOD GenericTest<T>() AS VOID WHERE T IS TestClass // error here
	 	? typeof(T)
	 RETURN
END CLASS
