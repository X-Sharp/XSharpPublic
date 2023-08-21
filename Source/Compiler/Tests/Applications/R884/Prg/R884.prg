using System.Reflection
using System.Runtime.InteropServices
FUNCTION Start( ) AS VOID
    local type := Typeof(Functions)  as system.Type
    ? "Type", type
    foreach oMethod as methodInfo in type:GetMethods()
        ? "Method", oMethod:Name
        var atts := oMethod.GetCustomAttributes()
        if atts != null
            foreach attr as System.Attribute in atts
                if attr IS DllImportAttribute VAR dll
                    ? "DllImport: ", "DLL", dll:Value, "EntryPoint", dll:EntryPoint, "CC", dll:CallingConvention, "SetLastError", dll:SetLastError, "CharSet", dll:CharSet
                    xAssert(oMethod:Name == dll:Value+dll:EntryPoint)
                endif
            next
        endif

    next

RETURN
_DLL FUNCTION FooBar() as LONG PASCAL:Foo."Bar" UNICODE
_DLL FUNCTION BarBar() as LONG PASCAL:Bar.Bar ANSI



PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		? "Incorrect result!!!!!!"
	END IF
RETURN
