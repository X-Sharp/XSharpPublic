//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

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