//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

[XSharp.Internal.TypesChanged];
CLASS ConsoleCoord
	EXPORT X AS INT
	EXPORT Y AS INT

CONSTRUCTOR( nX AS INT, nY AS INT) 
	X := nX
	Y := nY
	RETURN 

END CLASS
