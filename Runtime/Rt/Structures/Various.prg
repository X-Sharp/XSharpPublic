// Error.prg
// Created by    : robert
// Creation Date : 4/29/2016 1:26:10 PM
// Created for   : 
// WorkStation   : ZEUS


BEGIN NAMESPACE Vulcan
	STRUCTURE _GCDUMP
		PRIVATE Value as LONG
		CONSTRUCTOR()
			Value := 1
		PROPERTY __Value as LONG GET Value
	END STRUCTURE

	STRUCTURE _WINRTL_CRITICAL_SECTION
		PRIVATE Value as LONG
		CONSTRUCTOR()
			Value := 1
		PROPERTY __Value as LONG GET Value
	END STRUCTURE

	STRUCTURE _JMP_BUF
		PRIVATE Value as LONG
		CONSTRUCTOR()
			Value := 1
		PROPERTY __Value as LONG GET Value
	END STRUCTURE
END NAMESPACE