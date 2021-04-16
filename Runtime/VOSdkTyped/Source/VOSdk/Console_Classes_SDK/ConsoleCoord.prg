//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
/// <include file="Console.xml" path="doc/ConsoleCoord/*" />
[XSharp.Internal.TypesChanged];
CLASS ConsoleCoord
    /// <include file="Console.xml" path="doc/ConsoleCoord.X/*" />
	EXPORT X AS INT
    /// <include file="Console.xml" path="doc/ConsoleCoord.Y/*" />
	EXPORT Y AS INT

/// <include file="Console.xml" path="doc/ConsoleCoord.ctor/*" />
CONSTRUCTOR( nX AS INT, nY AS INT) 
	X := nX
	Y := nY
	RETURN 

END CLASS
