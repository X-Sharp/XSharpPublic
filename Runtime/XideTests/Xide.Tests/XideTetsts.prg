USING System.Windows.Forms
USING System.Collections.Generic
USING System.Reflection
USING System.Text

[STAThread];
FUNCTION Start() AS VOID
	Application.EnableVisualStyles()

	DoTests()

	XSharp.RuntimeState.WinCodePage := 1252

	XideUnitTest.Initialize()
	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.Core.Tests.StringTests):Assembly)
	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.RT.Tests.ArrayBaseTests):Assembly)
	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.VO.Tests.WinDateTests):Assembly)
	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.RDD.Tests.TestDBF):Assembly)
	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.VFP.Tests.NumericTests):Assembly)
	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.XPP.Tests.MiscTests):Assembly)

//	XideUnitTest.AddTestsFromAssembly(TypeOf(StringTests):Assembly)

	XideUnitTest.Run()
//	?
//	? Assert.Groups , "test groups, Passed:", Assert.Passed , ", Failed:" , Assert.Failed

RETURN

PROCEDURE DoTests()
// placeholder for quick tests
RETURN

