// 739. Problem with pre processor
/*
Following code did compile without errors in build 2.50
With 2.60, an error is reported: error XS9002: Parser: unexpected input 'zGet'
*/
#command @ <row>, <col> zGET <var> ;
                  		[<clauses,...>]  ;
                  		MESSAGE <msg> ;
                  		[<moreClauses,...>]  ;
     =>  @ <row>, <col> zGET <VAR> ;
	                  	[<clauses>] ;
                                [<moreClauses>] ;
                                ;AAdd(getlist,<msg> )


#xcommand @ <row>, <col> zGET <cString> MEMO TO <brow>, <rcol>     ;
               [WHEN <bWhen> ]                                     ;
               [VALID <bValid>]                                    ;
               [MENU  <bMemo> ]                                    ;
               =>                                                  ;
               AAdd(getlist, zGEMemoNew(                           ;
                  {|x| iif(x == NIL, <cString>, <cString> := x) }, ;
                  <cString>, <(cString)>,                          ;
                  <{bWhen}>, {<row>, <col>, <brow>, <rcol> },[<{bValid}>], <{bMemo}>))


FUNCTION Start() AS VOID
LOCAL getlist := {}
LOCAL u := 123 AS USUAL

@1,2 zGet u MEMO TO 1,2 MESSAGE "THEMESSAGE"
? getlist[1] // NIL
? getlist[2] // THEMESSAGE
xAssert(getlist[1] == NIL)
xAssert(getlist[2] == "THEMESSAGE")

FUNCTION zGEMemoNew() CLIPPER
RETURN NIL

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

