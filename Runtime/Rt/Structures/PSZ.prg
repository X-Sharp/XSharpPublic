// Date.prg
// Created by    : robert
// Creation Date : 4/29/2016 1:06:19 PM
// Created for   : 
// WorkStation   : ZEUS

BEGIN NAMESPACE Vulcan
	UNSAFE STRUCTURE __Psz
		PRIVATE Value as Byte PTR
		OPERATOR IMPLICIT( s as STRING) AS __Psz
			RETURN __Psz{}
		CONSTRUCTOR(s as STRING)
			Value := NULL
			RETURN
		PROPERTY __Value as Byte Ptr GET Value
	END STRUCTURE
END NAMESPACE