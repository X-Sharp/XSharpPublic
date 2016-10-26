// 58. compiler crash

CLASS TestClass
PROTECT lfield AS LOGIC
METHOD Test(l AS LOGIC) AS VOID
	LOCAL ll AS LOGIC
	ll := SELF:lfield .xor. l
RETURN
END CLASS

