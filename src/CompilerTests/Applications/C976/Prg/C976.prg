// 976. Problems with the /fox3 option - thisform implementation
// https://github.com/X-Sharp/XSharpPublic/issues/2025

#pragma options("fox3", enable)
#pragma options("allowdot", enable)
#pragma options("lb", enable)
#pragma options("memvar", enable)
#pragma options("undeclared", enable)
CLASS MyForm INHERIT XSharp.VFP.Custom // problem happens even if not inheriting from any class
	PROPERTY Test AS INT AUTO
END CLASS

CLASS TestControl INHERIT XSharp.VFP.Custom // problem happens even if not inheriting from any class
	PROPERTY ThisForm AS MyForm AUTO

	// should compile also without this, but for now uncomment to run test:

/*	METHOD FindForm() AS MyForm // workaround because the compiler needs this method to exist
	RETURN SELF:ThisForm*/
		
	METHOD DoTest() AS VOID
		thisform := MyForm{}
		thisform.Test := 123
		? thisform.Test
END CLASS

FUNCTION Start() AS VOID
	TRY
		TestControl{}:DoTest()
	CATCH oException AS System.Reflection.ReflectionTypeLoadException
		FOREACH oLoaderException AS Exception IN oException:LoaderExceptions
			// System.TypeLoadException: Declaration referenced in a method implementation cannot be a final method.  Type: 'ImplementClass'.
			? oLoaderException:ToString( )
		NEXT
	END TRY		

