// 452. error XS9002: Parser: mismatched input 
// with different order of clauses passed. I assume this is a bug, because harbour allows it

#command @ <one>, <two> THREE <three> [FOUR <four>] [FIVE <five>] => Test(<one>, <two> , <three> , <four> , <five>)

#command TEST1 LEFT RIGHT => Test()
#command TEST2 LEFT RIGHT => TEST1 RIGHT LEFT


FUNCTION Test(a,b,c,d,e) CLIPPER
	? pcount(),a,b,c,d,e
RETURN NIL

FUNCTION Start() AS VOID
// OK:
@ 1 , 2 three 3
@ 1 , 2 THREE 3 FOUR 4
@ 1 , 2 THREE 3 FOUR 4 FIVE 5

// error XS9002: Parser: mismatched input 'THREE'
@ 1 , 2 THREE 3 FIVE 5 FOUR 4 

TEST1 LEFT RIGHT // ok
TEST2 LEFT RIGHT // error

//errors:
TEST1 RIGHT LEFT
TEST2 RIGHT LEFT

OriginalCode()
RETURN






// original code :

#command @ <row>, <col> zSAY <xpr> [PICTURE <pic>] [COLOR <color>] ;
=> Test("one argument")

#warning By moving the VALID clause after WHEN, the code compiles without errors
#command @ <row>, <col> zGET <getvar>                                      ;
                        [PICTURE <pic>]                                 ;
                        [VALID <valid>]                                 ;
                        [WHEN <when>]                                   ;
                        [OTHER <other>]                                 ;
                                                                        ;
=> Test( <getvar>, <"getvar">, <pic>, <{valid}>, <{when}> )

// MESSAGE DISPLAY
#command @ <row>, <col> zGET <getvar> ;
                  		[<clauses,...>]  ;
                  		MESSAGE <msg> ;
                  		[<moreClauses,...>]  ;
     =>  @ <row>, <col> zGET <getvar> ;
	                  	[<clauses>] ;
                                [<moreClauses>]

// GET & SAY
#command @ <row>, <col> zSAY <sayxpr>                                    ;
                        [<sayClauses,...>]                              ;
                        zGET <getvar>                                       ;
                        [<getClauses,...>]                              ;
                                                                        ;
      => @ <row>, <col> zSAY <sayxpr> [<sayClauses>]                     ;
       ; @ <row>, <col>+1 zGET <getvar> [<getClauses>]


FUNCTION OriginalCode() AS VOID
LOCAL n := 1976 AS INT
LOCAL lWhen AS LOGIC

// next ones ok
@1,2 zSAY "Filler" zGET n
@1,2 zSAY "Filler" zGET n PICTURE "@!"
@1,2 zSAY "Filler" zGET n PICTURE "@!" MESSAGE "Enter the filler"
@1,2 zSAY "Filler" zGET n PICTURE "@!" MESSAGE "Enter the filler" WHEN lWhen

// error XS9002: Parser: mismatched input 'zGET'
@1,2 zSAY "Filler" zGET n PICTURE "@!" MESSAGE "Enter the filler" WHEN lWhen VALID lWhen

RETURN

