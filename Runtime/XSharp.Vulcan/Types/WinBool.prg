//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections
using System.Collections.Generic
using System.Linq
using XSharp
begin namespace XSharp	

[DebuggerDisplay("{ToString(),nq}", Type = "LOGIC")];
PUBLIC SEALED STRUCT __WinBool 
	PRIVATE _value as INT
	CONSTRUCTOR (lValue as LOGIC)
		_value := iif(lValue, 1, 0)
END CLASS
END NAMESPACE