// Note: Inside Visual Studio you should open the Test - Windows - Test Explorer Window
// From there you can run the tests and see which tests succeeded and which tests failed
//
USING System
USING System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)USING System.Linq
$endif$USING System.Text
using Microsoft.VisualStudio.TestTools.UnitTesting

BEGIN NAMESPACE $safeprojectname$
    [TestClass]	;
	CLASS TestClass
	CONSTRUCTOR()  STRICT
		RETURN		
		
	[TestMethod];
	METHOD TestMethod1 AS VOID  STRICT
		Assert.AreEqual(1,1)
	RETURN
	[TestMethod];
	METHOD TestMethod2 AS VOID  STRICT
		Assert.AreEqual(True, False) // Note that this will fail of course
	END CLASS
END NAMESPACE
