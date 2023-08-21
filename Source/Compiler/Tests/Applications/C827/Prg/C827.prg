// 827. Test for /noinit compiler option
// https://github.com/X-Sharp/XSharpPublic/issues/854
FUNCTION Start( ) AS VOID
	LOCAL lFailed := FALSE AS LOGIC
	TRY
		CreateInstance("TestClass")
	CATCH
		lFailed := TRUE
	END TRY
	
	IF lFailed
		? "CreateInstance() successfuly failed :-)"
	ELSE
		THROW Exception{"CreateInstance() found the class, while it shouldn't, with /noinit+ enabled"}
	ENDIF
RETURN
