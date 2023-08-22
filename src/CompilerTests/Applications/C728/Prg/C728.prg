// 728. Problem in TEXT TO with substituting text with << >> inside double quotes
// https://github.com/X-Sharp/XSharpPublic/issues/469
#pragma warnings(9025, off) //   return statement
#pragma warnings(219, off) //   assigned but not used
/*
Unhandled Exception: System.FormatException: Input string was not in a correct format.
   at System.Text.StringBuilder.FormatError()
   at System.Text.StringBuilder.AppendFormatHelper(IFormatProvider provider, String format, ParamsArray args)
   at System.String.FormatHelper(IFormatProvider provider, String format, ParamsArray args)
   at System.String.Format(String format, Object arg0, Object arg1)
   at C728.Exe.Functions.Start() in C:\xSharp\Dev\Tests\Applications\C728\Prg\C728.prg:line 7
*/

// note that TextMerge requires /memvar in X# 2.12 and later
FUNCTION Start() AS VOID
LOCAL cSubstitute
LOCAL cText
cSubstitute = "substitute here"
TEXT TO cText TEXTMERGE NOSHOW
<<cSubstitute>>
'<<cSubstitute>>'
"<<cSubstitute>>"
ENDTEXT
LOCAL cTest := cText as string
xAssert( cTest[cTest:Length-4] == 101) // e
xAssert( cTest[cTest:Length-3] == 34)  // "
xAssert( cTest[cTest:Length-2] == 13) // \r
xAssert( cTest[cTest:Length-1] == 10) // \n
xAssert(cText == e"substitute here\r\n'substitute here'\r\n\"substitute here\"\r\n")


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

