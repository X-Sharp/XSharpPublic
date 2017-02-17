//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

STRUCTURE _GCDUMP
	PRIVATE Value as LONG
	CONSTRUCTOR(l as long) STRICT
		Value := l
	PROPERTY __Value as LONG GET Value
END STRUCTURE

STRUCTURE _WINRTL_CRITICAL_SECTION
	PRIVATE Value as LONG
	CONSTRUCTOR(l as long) 
	Value := l
	PROPERTY __Value as LONG GET Value
END STRUCTURE

STRUCTURE _JMP_BUF
	PRIVATE Value as LONG
	CONSTRUCTOR(l as long) 
	Value := l
	PROPERTY __Value as LONG GET Value
END STRUCTURE
