// 112. error XS0531: 'ITest.prop2.set': interface members cannot have a definition
// related to #50, but different order of SET/GET keywords
INTERFACE ITest
	PROPERTY prop1 AS INT GET SET // ok
	PROPERTY prop2 AS INT SET GET // error
END INTERFACE

