//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Runtime.InteropServices

// Most of the class CompilerServices has been moved to XSharp.Vulcan
// because it accesses state and is used to manipulate State

STATIC CLASS XSharp.Internal.CompilerServices
	STATIC METHOD __StringSubtract (lhs AS STRING, rhs AS STRING) AS STRING
		IF lhs != NULL .and. rhs != NULL
			VAR len := lhs:Length + rhs:Length
			RETURN (lhs:TrimEnd() + rhs:TrimEnd()):PadRight(len)
		ELSEIF lhs != NULL
			RETURN lhs
		ELSEIF rhs != NULL
			RETURN rhs
		ENDIF
		RETURN String.Empty
END CLASS
