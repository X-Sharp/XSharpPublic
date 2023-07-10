// 43. error XS1620: Argument 2 must be passed with the 'out' keyword
// vulcan compiles both
// XS9069: Argument 2 is passed with REF keyword but should be passed with the 'out' keyword
#pragma warnings disable(9069)
FUNCTION Start() AS VOID
LOCAL r AS REAL8
System.Globalization.CultureInfo.CurrentCulture := System.Globalization.CultureInfo.GetCultureInfo("en-us")
System.Double.TryParse("1.2" , OUT r) // this should probably be allowed only in the VO/Vulcan dialect
? r
System.Double.TryParse("1.3" , REF r)
? r
