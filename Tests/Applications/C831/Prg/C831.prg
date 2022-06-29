// 831. Various preprocessor issues
// https://github.com/X-Sharp/XSharpPublic/issues/931
#pragma warnings(162, off) // unreachable code
FUNCTION Start() AS VOID
	IF FALSE
		LOCAL cPfad AS STRING
		RddSetDefault ( "DBFCDX" )
		cPfad := "c:\TEST\"
		? DbUseArea(,,"c:\dbf\customer")

		COPY STRUCTURE TO ( cPfad + "struccopy.dbf")
		COPY STRUCTURE TO ( cPfad + "struccopy.dbf") FIELDS "LAST"
	END IF

