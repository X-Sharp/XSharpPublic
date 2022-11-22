//https://github.com/X-Sharp/XSharpPublic/issues/1179
#using System.Reflection

[Assembly: AssemblyVersion("751.3.0405.1")]
[Assembly: AssemblyFileVersion("751.3.0405.1")]
[Assembly: AssemblyInformationalVersionAttribute("751.3.C0405.1")]

FUNCTION Start( ) AS VOID
	var asm := Assembly.GetExecutingAssembly()
	foreach implied attr in asm:GetCustomAttributes()
	    if attr is AssemblyFileVersionAttribute var afva
             xAssert(afva:Version ==  "751.3.0405.1")
	    elseif attr is AssemblyInformationalVersionAttribute var aiva
             xAssert(aiva:InformationalVersion ==  "751.3.C0405.1")
	    endif
	next
	xAssert(asm:GetName():Version:Major == 751)
	xAssert(asm:GetName():Version:Minor == 3)
	xAssert(asm:GetName():Version:Build == 405)
	xAssert(asm:GetName():Version:Revision == 1)
RETURN


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
