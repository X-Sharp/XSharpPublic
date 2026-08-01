// 969. Nested WITH statements produce a bogus compiler error #1991
// https://github.com/X-Sharp/XSharpPublic/issues/1991

CLASS TestClass
	EXPORT n := 123 AS int
END CLASS

FUNCTION Start( ) AS VOID
	LOCAL c := "abc" AS STRING
	WITH c
		? .Length
		xAssert( .Length == 3 )
		WITH .Length // error XS0841: Cannot use local variable 'Xs$WithVar$C969_93_8_2' before it is declared
			? .ToString()
			xAssert( .ToString( ) == "3" )
		END WITH
		
		LOCAL o := TestClass{} AS TestClass
		WITH o
			xAssert( .n == 123 )
			.n := 456
			WITH .n
				? .ToString()
				xAssert( .ToString() == "456" )
			END WITH
			
		END WITH
		
	END WITH
RETURN


PROC xAssert(l AS LOGIC) AS VOID
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

