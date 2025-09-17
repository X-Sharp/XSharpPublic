using System.Windows.Forms
using System.Collections.Generic
using System.Reflection
using System.Text
using System.Runtime.Versioning
[assembly: TargetFramework(".NETFramework,Version=v4.6", FrameworkDisplayName := ".NET Framework 4.6")]
[STAThread];
function Start() as void
	Application.EnableVisualStyles()

	DoTests()

	XSharp.RuntimeState.WinCodePage := 1252

	XideUnitTest.Initialize()
//	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.Core.Tests.StringTests):Assembly)
//	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.RT.Tests.ArrayBaseTests):Assembly)
//	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.VO.Tests.WinDateTests):Assembly)
//	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.RDD.Tests.TestDBF):Assembly)
//	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.VFP.Tests.NumericTests):Assembly)
//	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.XPP.Tests.MiscTests):Assembly)
//	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.Harbour.Tests.GeneralTests):Assembly)
//	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.SQLRdd.Tests.SQLiteTests):Assembly)

//	XideUnitTest.AddTestsFromAssembly(TypeOf(StringTests):Assembly)

	XideUnitTest.AddTestsFromAssembly(TypeOf(XSharp.VFP.Tests.DateTests):Assembly)

	XideUnitTest.Run()
//	?
//	? Assert.Groups , "test groups, Passed:", Assert.Passed , ", Failed:" , Assert.Failed

return

procedure DoTests()
	var dt := DateTime{2025, 08, 13, 14, 30, 45}
	local lSeconds := RuntimeState.GetValue<logic>(Set.Seconds) as logic
	local nHours := RuntimeState.GetValue<long>(Set.Hours) as long

	? "Seconds: ", IIF(lSeconds, "ON", "OFF")
	? "Hours: ", nHours
	? "TToC(dt, 0):", XSharp.VFP.Functions.TToC(dt, 0)
	? "TToC(dt, 1):", XSharp.VFP.Functions.TToC(dt, 1)
	? "TToC(dt, 2):", XSharp.VFP.Functions.TToC(dt, 2)
	? "TToC(dt, 3):", "'" + XSharp.VFP.Functions.TToC(dt, 3) + "'"
return

