// Symbol.prg
// Created by    : robert
// Creation Date : 4/29/2016 1:08:15 PM
// Created for   : 
// WorkStation   : ZEUS



BEGIN NAMESPACE Vulcan
	STRUCTURE __Symbol
		PRIVATE Value as Long
		CONSTRUCTOR(s as STRING)
			Value := 1
		OPERATOR IMPLICIT(s as STRING) AS SYMBOL
			RETURN __Symbol{s}
		PROPERTY __Value as LONG GET Value
	END STRUCTURE
END NAMESPACE