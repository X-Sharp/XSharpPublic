// 560. CreateInstance() cannot "see" classes with namespaces
// Especially the first case (no explicit namespace, but one implicitly supplied with the /ns option) 
// is very important, because it makes code directly ported from VO to stop working
// problem is the same in vulcan of course

// this gets emitted as XSharp.Namespace.TestClass, due to the /ns: option
CLASS TestClassC560
END CLASS

CLASS TestNS.AnotherClassC560
END CLASS

BEGIN NAMESPACE AnotherNS
	CLASS ThirdClassC560
	END CLASS
	
	BEGIN NAMESPACE NestedNS
		CLASS FourthClassC560
		END CLASS
	END NAMESPACE
END NAMESPACE

FUNCTION Start() AS VOID
? CreateInstance(#TestClassC560)
? CreateInstance(#AnotherClassC560)
? CreateInstance(#ThirdClassC560)
? CreateInstance(#FourthClassC560)
RETURN
