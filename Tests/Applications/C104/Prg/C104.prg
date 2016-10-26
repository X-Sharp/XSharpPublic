// 104. error XS0238: 'Child.Test()' cannot be sealed because it is not an override
// vulcan incompatibility, related to #91
CLASS Parent
	VIRTUAL METHOD Test() AS VOID
END CLASS

CLASS Child INHERIT Parent
	SEALED METHOD Test() AS VOID
END CLASS

