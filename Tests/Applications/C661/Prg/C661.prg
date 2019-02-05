// 661. Compiler crash with DO CASE having an OTHERWISE but no CASE clauses
FUNCTION Start AS VOID
	LOCAL i AS INT
	i := 10
	DO CASE
   // CASE i == 10
   //  i += 1
	OTHERWISE        // this should now generate a warning about an empty case statement
		i := 20 
	ENDCASE
	? i
	IF i != 20
		THROW Exception{"Incorrect result"}
	END IF
RETURN
