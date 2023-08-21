// 560. CreateInstance() cannot "see" classes with namespaces
// Especially the first case (no explicit namespace, but one implicitly supplied with the /ns option) 
// is very important, because it makes code directly ported from VO to stop working
// problem is the same in vulcan of course

// this gets emitted as XSharp.Namespace.TestClass, due to the /ns: option
CLASS TestClass
END CLASS

CLASS TestNS.AnotherClass
END CLASS

BEGIN NAMESPACE AnotherNS
	CLASS ThirdClass
	END CLASS
	
	BEGIN NAMESPACE NestedNS
		CLASS FourthClass
		END CLASS
	END NAMESPACE
END NAMESPACE

FUNCTION Start() AS VOID
? CreateInstance(#TestClass)
? CreateInstance(#AnotherClass)
? CreateInstance(#ThirdClass)
? CreateInstance(#FourthClass)
RETURN
