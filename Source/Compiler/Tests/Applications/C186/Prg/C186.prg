// 186. error XS9002: Parser: missing EOS at 'CLASS'
// vulcan incompatibility and parser goes out of sync later 
// reporting many other errors, making it very difficult 
// to understand what the compiler is complaining about
CLASS TestClass
	CONSTRUCTOR() CLASS TestClass
END CLASS

