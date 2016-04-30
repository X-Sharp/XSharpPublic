//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

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