// error XS9002: Parser: unexpected input 'FORLOCALn:=1ASINTUPTO100CRLFAAdd({},{1,AsType'

// updated to:
// error XS9002: Parser: unexpected input 'FORLOCALn:=1ASINTUPTO100CRLFAAdd({},{1,REF'

// problem in code is that the kwywords ASTYPE and REPEAT are used as identifiers
// the parser error messages need to be improved, because right now they do not help
// finding what exactly the offending code is. Real code having that problem was a 3000 lines source file..
FUNCTION Start() AS VOID

RETURN

CLASS TestClass
	METHOD Test() AS VOID
		IF TRUE .and. FALSE .or. TRUE
			? "test"
			FOR LOCAL n := 1 AS INT UPTO 100
//				AAdd({} , {1,AsType(123)})
				AAdd({} , {1,REF(123)})
			NEXT
		ELSE
			? "test"
			DO WHILE FALSE
				AAdd({} , {1,Repeat(123)})
			ENDDO
		ENDIF
	RETURN
END CLASS

FUNCTION AsType(n AS INT) AS INT
RETURN n
FUNCTION Repeat(n AS INT) AS INT
RETURN n
               
Function Ref(n as int) as INT
	return n
