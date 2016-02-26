FUNCTION Start() AS VOID
	LOCAL d AS DWORD
	LOCAL n AS INT
	
	d := UInt32.MaxValue
	? "Initial value of d:", d

	BEGIN UNCHECKED
		// arithmetic operations inside an UNCHECKED block will not produce 
		// overflow exceptions on arithmetic conversions and operations,
		// no matter if overflow checking is enabled application-wide or not
		n := (INT)d
		? "Value of n after conversion:", n
		d ++
		? "Value of d after increasing it:", d
	END UNCHECKED

	d := UInt32.MaxValue
	BEGIN CHECKED
		// arithmetic operations inside a CHECKED block always do
		// overflow checking and throw exceptions if overflow is detected
		TRY
			n := (INT)d
			d ++
		CATCH e AS Exception
			? "Exception thrown in CHECKED operation:", e:Message
		END TRY
	END CHECKED
	Console.ReadLine()
RETURN
