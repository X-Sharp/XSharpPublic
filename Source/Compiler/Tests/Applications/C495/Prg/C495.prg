// 495. error XS1503: Argument 1: cannot convert from 'Vulcan.__Usual[]' to 'Vulcan.Runtime.ErrorInfo'
// vo16+

CLASS Parent
	CONSTRUCTOR()
		? "default constructor"
	CONSTRUCTOR(n AS INT)
		? n
	RETURN
END CLASS

CLASS Child INHERIT Parent
// no constructor provided, vulcan calls the default parent constructor

// x# does the same thing without /vo16, but with /vo16+ the above error is reported
END CLASS

FUNCTION Start() AS VOID
	Child{}
	
	LOCAL o AS FabError
	o := FabError{}
	? o
RETURN

// original VO code:
CLASS FabError INHERIT Error
END CLASS

