// 173. No compiler error and parent constructor not being invoked
// vulcan dialect only. Core dialect properly reports a compiler error
CLASS Parent
CONSTRUCTOR(n AS INT,m AS STRING)
? "parent initialized"
END CLASS

CLASS Child INHERIT Parent
CONSTRUCTOR(a AS STRING)
SUPER() // overrides parent constructor!
END CLASS

FUNCTION Start() AS VOID
Child{NULL}

