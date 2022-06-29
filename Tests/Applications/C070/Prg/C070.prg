// 70. error XS0216: The operator 'Foo.operator ==(Foo, string)' requires a matching operator '!=' to also be defined
// vulcan compiles it without errors
#pragma warnings(660, off) // missing Equals
#pragma warnings(661, off) // missing Gethashcode
#pragma warnings(216, off) // missing != operator
CLASS Foo
	OPERATOR ==(a AS Foo , b AS STRING) AS LOGIC
	RETURN TRUE
END CLASS

