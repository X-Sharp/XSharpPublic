// 420. LoadLibraryW() returns NULL_PTR at runtime

/* problem is that the function is emitted in x# as:

[DllImport("KERNEL32", ExactSpelling = true, SetLastError = true)]
public unsafe static extern void* LoadLibraryW(string lpLibFileName);

while vulcan emits it as:

[SuppressUnmanagedCodeSecurity]
[DllImport("kernel32.dll", CharSet = CharSet.Auto, ExactSpelling = true, SetLastError = true)]
public unsafe static extern void* LoadLibraryW(string lpLibFileName);

Note that CharSet = CharSet.Auto is missing from the x# version

*/

_DLL FUNCTION LoadLibraryW(lpLibFileName AS STRING) AS PTR PASCAL:KERNEL32.LoadLibraryW
_DLL FUNCTION LoadLibraryTestWithDifferentName(lpLibFileName AS STRING) AS PTR PASCAL:KERNEL32.LoadLibraryW

FUNCTION Start() AS VOID
LOCAL hDll := LoadLibraryW( "user32.dll" ) AS PTR	
? hDll
? hdll == NULL_PTR
IF hdll == NULL_PTR
	THROW Exception{e"LoadLibraryW( \"user32.dll\" ) returned a NULL_PTR"}
END IF

hDll := LoadLibraryTestWithDifferentName( "kernel32.dll" )
? hDll
? hdll == NULL_PTR
IF hdll == NULL_PTR
	THROW Exception{e"LoadLibraryTestWithDifferentName( \"kernel32.dll\" ) returned a NULL_PTR"}
END IF


