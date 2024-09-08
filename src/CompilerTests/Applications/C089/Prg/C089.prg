// 89. error: no viable alternative at input 'PROPERTY SELF'mismatched input ']' expecting EOS
CLASS TestClass
	PROPERTY SELF[index AS DWORD] AS INT 
		GET 
			RETURN 0
		END GET
	END PROPERTY
END CLASS

