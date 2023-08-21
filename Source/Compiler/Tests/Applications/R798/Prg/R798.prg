// test for https://github.com/X-Sharp/XSharpPublic/issues/765
// error XS0738: 'Test' does not implement interface member 'ITest.WrongCasing()'. 'Test.WrOnGcAsInG()' cannot implement 'ITest.WrongCasing()' because it does not have the matching return type of 'object'.
INTERFACE ITest
	METHOD WrongCasing() AS OBJECT
END INTERFACE

CLASS Test IMPLEMENTS ITest
	METHOD WrOnGcAsInG() AS OBJECT 
	RETURN NULL
END CLASS

