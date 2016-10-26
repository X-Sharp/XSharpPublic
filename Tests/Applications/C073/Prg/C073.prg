// 73. : error XS0106: The modifier 'public','virtual','override' is not valid for this item
CLASS TestClass IMPLEMENTS ITest
   VIRTUAL PROPERTY ITest.MyProp AS Boolean GET FALSE
END CLASS

INTERFACE ITest
	PROPERTY MyProp AS Boolean GET
END INTERFACE

