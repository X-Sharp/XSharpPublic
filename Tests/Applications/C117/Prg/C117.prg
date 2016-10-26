// 117. error XS0501: 'TestClass.prop.set' must declare a body because it is not marked abstract, extern, or partial
INTERFACE ITest
	PROPERTY prop AS INT GET SET
END INTERFACE

CLASS TestClass IMPLEMENTS ITest
	VIRTUAL PROPERTY prop AS INT GET 0 SET 
END CLASS

