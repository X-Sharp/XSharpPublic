// C482. Antlr: SLL parsing failed with failure: Exception of type 'LanguageService.SyntaxTree.InputMismatchException' was thrown.. Trying again in LL mode.

#command @ <row>, <col> zSAY <xpr>                                       ;                                
                        [PICTURE <pic>]                                 ;
                        [COLOR <color>]                                 ;
                                                                        ;
      => Picture(<pic>  , <xpr>[, <color>] )


#command @ <row>, <col> zSAY <xpr>                                       ;
                        [COLOR <color>]                                 ;
                                                                        ;
      => DevOut( <xpr> [, <color>] )


*  @..GET
#command @ <row>, <col> zGET <avar>                                      ;
                        [PICTURE <pic>]                                 ;
                        [VALID <valid>]                                 ;
                        [WHEN <when>]                                   ;
                        [SEND <msg>]                                    ;
                                                                        ;
      => ; Picture(<pic>)                                              ;
      [; <msg>]



// @..GET COLOR
#command @ <row>, <col> zGET <avar>                                       ;
                        [<clauses,...>]                                 ;
                        COLOR <color>                                   ;
                        [<moreClauses,...>]                             ;
                                                                        ;
      => @ <row>, <col> zGET <avar>                                       ;
                        [<clauses>]                                     ;
                        SEND colorDisp(<color>)                         ;
                        [<moreClauses>]



// MESSAGE DISPLAY
#command @ <row>, <col> zGET <avar> ;
                  		[<clauses,...>]  ;
                  		MESSAGE <msg> ;
                  		[<moreClauses,...>]  ;
     =>  @ <row>, <col> zGET <avar> ;
	                  	[<clauses>] ;
                                [<moreClauses>] ;
                                ;Message(<msg>)


// GET & SAY
#command @ <row>, <col> zSAY <sayxpr>                                    ;
                        [<sayClauses,...>]                              ;
                        zGET <avar>                                       ;
                        [<getClauses,...>]                              ;
                                                                        ;
      => @ <row>, <col> zSAY <sayxpr> [<sayClauses>]                     ;
       ; @ Row(), Col()+1 zGET <avar> [<getClauses>]

FUNCTION colorDisp(color)
	? color
RETURN NIL
FUNCTION message(mes)
	? mes
RETURN NIL
FUNCTION picture(pic)
	? pic
RETURN NIL
FUNCTION devout(c)
	? c
RETURN NIL


FUNCTION Start( ) AS VOID

//ok:
@1,2 zSay "test" zGet a[1] MESSAGE "message" VALID TRUE

//ok:
@1,2 zSay "test" zGet a[1] PICTURE "pic" VALID TRUE

//ok
@1,2 zSay "test" zGet a[1] MESSAGE "message" COLOR 10

// ok
@1,2 zSay "test" zGet a[1] COLOR 10 VALID TRUE

// error if included all MESSAGE, COLOR, VALID:
@1,2 zSay "test" zGet a[1] MESSAGE "message" COLOR 10 VALID TRUE
/*
harbour produces:
DevOut("test" );  Picture() ; colorDisp(10);Message("message")
*/

@1,2 zSay "test" zGet a[1] PICTURE "!" MESSAGE "message" WHEN TRUE COLOR 10 VALID TRUE
/*
harbour produces:
DevOut("test" );  Picture("!") ; colorDisp(10);Message("message")
*/
RETURN
