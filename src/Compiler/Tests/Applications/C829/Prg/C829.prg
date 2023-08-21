// 829. Various UDC issues
// https://github.com/X-Sharp/XSharpPublic/issues/931


FUNCTION Start( ) AS VOID
	LOCAL cPfad AS STRING 	
	RddSetDefault ( "DBFCDX" ) 
	cPfad := "C:\TEST\" 
    DbCreate(cPFad+"Source", {{"FIRST","C",20,0},{"LAST","C",20,0}})
    DbUseArea(TRUE, "DBFCDX", cPFad+"Source")
	
	// ok:
	COPY STRUCTURE TO ( cPfad + "struccopy.dbf") // FIELDS "LAST"	
	// error XS9002: Parser: unexpected input 'STRUCTURE':
	COPY STRUCTURE TO ( cPfad + "struccopy.dbf") FIELDS LAST
RETURN
