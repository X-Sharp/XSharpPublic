// 319. error XS0103: The name 'Xs$PszList' does not exist in the current context
FUNCTION Start() AS VOID
LOCAL o AS TestClass
o := TestClass{}
o:MyPropAssign := 123
? o:MyProp

CLASS TestClass
	PROTECT n AS INT
	ACCESS MyProp AS INT
		? String2Psz("123")
	RETURN SELF:n
	ASSIGN MyPropAssign(nValue AS INT)
		LOCAL p AS PSZ
		p := String2Psz(nValue:ToString())
		? p
	SELF:n := nValue
END CLASS
