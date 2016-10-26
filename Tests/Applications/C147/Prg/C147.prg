// 147. incorrect params passed to super constructor
// /dialect:vulcan

FUNCTION Start() AS VOID
Child{1,2}
RETURN

CLASS Parent
    CONSTRUCTOR(b)
    	IF b != 2
    		THROW Exception{"Incorrect param passed"}
    	END IF
END CLASS

CLASS Child INHERIT Parent
    CONSTRUCTOR(a,b)
        SUPER(b)
END CLASS 

