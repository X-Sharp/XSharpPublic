/// <include file="Console.xml" path="doc/ConsoleCoord/*" />
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
