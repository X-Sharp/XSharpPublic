USING System.Windows.Forms
USING System.Reflection

[STAThread];
FUNCTION Start() AS VOID
	Application.EnableVisualStyles()
	
	DoTests()

	XideUnitTest.Initialize()
	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.Core.Tests.StringTests):Assembly)
	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.VO.Tests.DateTests):Assembly)
	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.VO.Tests.OOPTests):Assembly)
	XideUnitTest.Run()
//	?
//	? Assert.Groups , "test groups, Passed:", Assert.Passed , ", Failed:" , Assert.Failed
RETURN


PROCEDURE DoTests()
// placeholder for quick tests
//	wait

