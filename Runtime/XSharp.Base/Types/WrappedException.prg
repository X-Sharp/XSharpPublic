//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

CLASS XSharp.Internal.WrappedException INHERIT Exception
	PRIVATE _value as OBJECT

	PROPERTY Value as Object get _value

	CONSTRUCTOR(value as Object)
		_value := value

END CLASS