// Date.prg
// Created by    : robert
// Creation Date : 4/29/2016 1:06:19 PM
// Created for   : 
// WorkStation   : ZEUS

BEGIN NAMESPACE Vulcan
	STRUCTURE __VODate
		PRIVATE Value as INT
		CONSTRUCTOR(i as INT)
			Value := i
		OPERATOR IMPLICIT(i as INT) AS __VODate
			RETURN __VoDate{}
		PROPERTY __Value as Int GET Value
	END STRUCTURE
END NAMESPACE