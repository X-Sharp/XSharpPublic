USING System.Windows.Forms
USING System.Collections.Generic
USING System.Reflection
USING System.Text

[STAThread];
FUNCTION Start() AS VOID
	Application.EnableVisualStyles()

	DoTests()

	XideUnitTest.Initialize()
	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.Core.Tests.StringTests):Assembly)
	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.VO.Tests.DateTests):Assembly)
	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.VO.Tests.OOPTests):Assembly)
	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.RDD.Tests.TestDBF):Assembly)
	XideUnitTest.Run()
RETURN
PROCEDURE DoTests()
	
// placeholder for quick tests
RETURN	
