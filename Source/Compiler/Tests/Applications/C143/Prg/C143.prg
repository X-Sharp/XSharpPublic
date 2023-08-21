// 143. error XS0154: The property or indexer 'ChildClass.Prop' cannot be used in this context because it lacks the get accessor
// /vo3
// probably same with C142
FUNCTION Start() AS VOID
LOCAL o AS ChildClass
o := ChildClass{}
? o:Prop == 0

CLASS BaseClass
	VIRTUAL PROPERTY @@Prop AS INT GET 1 SET
END CLASS

CLASS ChildClass INHERIT BaseClass
	VIRTUAL ASSIGN Prop(n AS INT)
END CLASS

