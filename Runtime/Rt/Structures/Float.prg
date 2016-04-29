// FLoat.prg
// Created by    : robert
// Creation Date : 4/29/2016 1:08:46 PM
// Created for   : 
// WorkStation   : ZEUS

BEGIN NAMESPACE Vulcan
	STRUCTURE __VOFloat
		PRIVATE Value as Real8
		CONSTRUCTOR (r8 as Real8)
			Value := r8
		OPERATOR IMPLICIT( i as INT) AS __VOFLoat
			RETURN __VOFLoat{i}
		PROPERTY __Value as Real8 GET Value
	END STRUCTURE
END NAMESPACE