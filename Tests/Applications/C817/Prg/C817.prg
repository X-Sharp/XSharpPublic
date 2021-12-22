// 817. Problem with Math.Round(float/int) when /vo12 is enabled (ambiguous call)
// not that the call Math.Round(f) works with no issues, it's only Math.Round(f/n) that causes trouble

// /vo12+ enabled
FUNCTION Start() AS VOID
LOCAL f := 0.0 AS FLOAT
? Math.Round(f) 	// OK
? Math.Round(f/2) 	// error XS0121: The call is ambiguous between the following methods or properties: 'System.Math.Round(real8)' and 'System.Math.Round(decimal)'

