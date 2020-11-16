// 584. error XS9003: Pre-processor: Optional block does not contain a match marker
/*
Shows several preprocessor errors "Optional block does not contain a match marker"
Typically reported for directives like the following one, found in the DBCmds.vh file:

#command APPEND        ;
   [FROM <(src)>]      ;
   [FIELDS <list,...>] ;
   [FOR <fo>]          ;
   [WHILE <wh>]        ;
   [NEXT <nx>]         ;
   [RECORD <rec>]      ;
   [<rs:REST>]         ;
   [VIA <rdd>]         ;
   [ALL]               ;
   => DBApp( <(src)>, { <(list)> }, <{fo}>, <{wh}>, <nx>,  <rec>, <.rs.>, <rdd> )

*/

#include "DBCmd.xh"
FUNCTION Start( ) AS VOID
	LOCAL cDbf AS STRING
	cDbf := System.Environment.CurrentDirectory + "\C584.dbf"
	IF .not. System.IO.File.Exists(cDbf)
		DBCreate(cDbf, {{"TEST", "C", 10, 0}}, "DBFCDX", TRUE)
		DBCloseArea()
	END IF
	
	USE C584 NEW EXCLUSIVE
	? DBCloseArea()
RETURN
