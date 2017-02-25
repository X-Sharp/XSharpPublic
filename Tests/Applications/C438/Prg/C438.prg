// 438. error XS9002: Parser: mismatched input '(' expecting {COMMA, EOS}

#command @ <row>, <col> DOSAY <xpr>                                       ;
                        [PICTURE <pic>]                                 ;
                        [COLOR <color>]                                 ;
                                                                        ;
      => DevPos( <row>, <col> )                                         ;
       ; DevOutPict( <xpr>, <pic> [, <color>] )


#command @ <row>, <col> DOSAY <xpr>                                       ;
                        [COLOR <color>]                                 ;
                                                                        ;
      => DevPos( <row>, <col> )                                         ;
       ; DevOut( <xpr> [, <color>] )


FUNCTION Start() AS VOID
@1,2 DOSAY 123 // this one is ok
@3,4 DOSAY 456 PICTURE 789

@1,2 DOSAY "abc"
@3,4 DOSAY "abc" PICTURE "999"

//Harbour's ppo:
/*
DevPos(1,2 ) ; DevOut(123 )
DevPos(3,4 ) ; DevOutPict(456,789 )

DevPos(1,2 ) ; DevOut("abc" )
DevPos(3,4 ) ; DevOutPict("abc","999" )
*/

PROCEDURE DevPos(y,x)
? y,x
PROCEDURE DevOut(c)
? "without pict",c
PROCEDURE DevOutPict(c,p)
? "with pict",c,p

