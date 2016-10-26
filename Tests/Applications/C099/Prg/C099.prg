// 99. error XS0027: Keyword 'this' is not available in the current context
CLASS Parent
CONSTRUCTOR(o AS OBJECT)
END CLASS

CLASS Child INHERIT Parent
CONSTRUCTOR()
SUPER(SELF)
END CLASS

