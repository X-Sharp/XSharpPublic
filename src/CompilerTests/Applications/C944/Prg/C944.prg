// 944. No compiler error when using properties as out/ref parameter
// https://github.com/X-Sharp/XSharpPublic/issues/1773

CLASS TestClassWithAutoProperty
	PROTECT n AS INT
	PUBLIC PROPERTY Val AS INT AUTO GET SET
	PUBLIC PROPERTY MyProp AS INT 
		GET
			RETURN SELF:n
		END GET
		SET
			SELF:n := value
		END SET
	END PROPERTY			
END CLASS

PUBLIC STATIC CLASS TestClassNew
	PUBLIC STATIC METHOD TestOutParam(outParam OUT INT) AS VOID
		outParam := 42
	PUBLIC STATIC METHOD TestRefParam(refParam REF INT) AS VOID
		refParam := 42
END CLASS

FUNCTION Start() AS VOID STRICT
	VAR testObj := TestClassWithAutoProperty{}

	TestClassNew.TestOutParam(OUT testObj:Val) // should be compiler error
	? testObj:Val // 0
	TestClassNew.TestOutParam(OUT testObj:MyProp) // should be compiler error
	? testObj:MyProp // 0

	TestClassNew.TestRefParam(REF testObj:Val) // should be compiler error
	? testObj:Val // 0
	TestClassNew.TestRefParam(REF testObj:MyProp) // should be compiler error
	? testObj:MyProp // 0

