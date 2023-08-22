// 193. error XS1527: Elements defined in a namespace cannot be explicitly declared as private, protected, or protected internal
// looks like vulcan translates private to internal here
PRIVATE STRUCTURE TestClass
EXPORT n AS INT       
CONSTRUCTOR(nValue AS INT)
	n := nValue
END STRUCTURE

