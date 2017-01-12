// 406. error XS0029: Cannot implicitly convert type 'Vulcan.__Usual' to 'System.DateTime'
// error XS0030: Cannot convert type 'Vulcan.__Usual' to 'System.DateTime'
// strange, DateTime is the only value or reference type that I could see there's a problem with..
GLOBAL dDef := DateTime{2016,1,12} AS DateTime

FUNCTION Start() AS VOID
	LOCAL u AS USUAL
	LOCAL d AS DateTime
	u := dDef
	? u
	d := u
	? d
	d := (DateTime)u
	Test(u)
	Test((DateTime)u)
	? d , u
	? d == u, u == d // both return FALSE in Vulcan!
	IF !d:Equals((DateTime)u) .or. !dDef:Equals((DateTime)u)
		THROW Exception{"Incorrect datetime"}
	END IF
RETURN

FUNCTION Test(d AS DateTime) AS VOID
	? d
	IF .not. d:Equals(dDef)
		THROW Exception{"Incorrect datetime"}
	END IF
RETURN 

