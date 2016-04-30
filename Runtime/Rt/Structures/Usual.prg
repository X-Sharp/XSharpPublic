//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
STRUCTURE Vulcan.__Usual
	PRIVATE Value  as IntPtr 
	STATIC PUBLIC _NIL AS __Usual
	CONSTRUCTOR(p as IntPtr)
		Value := p
	STATIC CONSTRUCTOR
		_NIL := __Usual{}
	PROPERTY __Value as IntPtr GET Value
END STRUCTURE


