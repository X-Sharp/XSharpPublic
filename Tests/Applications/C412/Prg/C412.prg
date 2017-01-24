// 412. error XS0200: Property or indexer 'Parent.MyProp' cannot be assigned to -- it is read only
// error XS0154: The property or indexer 'Parent2.MyProp2' cannot be used in this context because it lacks the get accessor
// looks like the compiler resolves the property to the parent class?
// problem happens only if properties are declared as VIRTUAL
FUNCTION Start() AS VOID
TestClass{}:MyProp := "efg"

CLASS TestClass INHERIT Child

	CONSTRUCTOR()
		SUPER()
		SELF:MyProp := "abc"
		? SELF:MyProp
		IF SELF:MyProp != "Child"
			THROW Exception{"Incorrect access used"}
		END IF
		
END CLASS

/*
IN Library:

CLASS Child INHERIT Parent
VIRTUAL ASSIGN MyProp(c AS STRING) STRICT
RETURN 
VIRTUAL ACCESS MyProp AS STRING STRICT
RETURN "Child"
END CLASS


CLASS Parent
VIRTUAL ACCESS MyProp  AS STRING STRICT
RETURN "Parent"
END CLASS

*/

CLASS TestClass2 INHERIT Child2
	CONSTRUCTOR()
		SUPER()
		? SELF:MyProp2 // similar problem with only assign in the parent class
	RETURN
END CLASS

