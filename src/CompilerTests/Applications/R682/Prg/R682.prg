
FUNCTION Start( ) AS VOID
	LOCAL a,b,c
	a := TRUE
	b := TRUE
	c := TRUE
	? a AND b AND c
	b := FALSE
	c := FALSE
	? a OR b OR c
	
RETURN


Function Not(a,b,c)      
	// of course the variable naming is crazy
	// but this proves that the compiler can distinguish between the names 
	// and the logical operators         
	local and , or, not
	and := a and b and c
	or  := a or b or c 
	not := a or b and not c
	RETURN and and or and not not
