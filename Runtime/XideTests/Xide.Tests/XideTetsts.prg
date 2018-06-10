USING System.Windows.Forms
USING System.Reflection

[STAThread];
FUNCTION Start( ) AS VOID
	Application.EnableVisualStyles()

	XideUnitTest.Initialize()
	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.Core.Tests.StringTests):Assembly)
	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.VO.Tests.SymbolTests):Assembly)
	XideUnitTest.Run()
//	?
//	? Assert.Groups , "test groups, Passed:", Assert.Passed , ", Failed:" , Assert.Failed
RETURN

