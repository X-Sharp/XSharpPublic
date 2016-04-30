//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

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