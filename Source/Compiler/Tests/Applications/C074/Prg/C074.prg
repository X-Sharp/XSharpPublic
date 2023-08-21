// 74. compiler crash
CLASS BaseClass
CONSTRUCTOR(c AS STRING)
END CLASS

CLASS ChildClass INHERIT BaseClass
CONSTRUCTOR(n AS STRING) AS VOID // allowed in vulcan
SUPER(n)
END CLASS


