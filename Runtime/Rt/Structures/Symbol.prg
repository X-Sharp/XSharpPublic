//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


STRUCTURE Vulcan.__Symbol
	PRIVATE Value as Long
	CONSTRUCTOR(s as STRING)
		Value := 1
	OPERATOR IMPLICIT(s as STRING) AS SYMBOL
		RETURN __Symbol{s}
	PROPERTY __Value as LONG GET Value
END STRUCTURE
