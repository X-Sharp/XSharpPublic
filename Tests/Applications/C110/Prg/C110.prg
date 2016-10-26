// 110. error XS0263: Partial declarations of 'TestClass' must not specify different base classes
// just mentioning it as it is an incompatibility with vulcan
// not sure if it's a good idea to "fix" this particular one, though
// allthough it's pretty common scenario in vulcan code...
CLASS Parent
END CLASS

PARTIAL CLASS TestClass INHERIT Parent
END CLASS
PARTIAL CLASS TestClass
END CLASS

