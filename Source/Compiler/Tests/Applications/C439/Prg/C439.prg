// 439. error XS9002: Parser: unexpected input ':' expecting {COMMA, EOS}
#pragma warnings (9043,off)
#command @ <row>, <col> DOSAY <xpr>                                       ;
                        [PICTURE <pic>]                                 ;
                        [COLOR <color>]                                 ;
                                                                        ;
      => DevPos( <row>, <col> )                                         ;
       ; DevOutPict( <xpr>, <pic> [, <color>] )


#command @ <row>, <col> zGET <varr>                                      ;
                        [PICTURE <pic>]                                 ;
                        [VALID <valid>]                                 ;
                        [WHEN <when>]                                   ;
                        [SEND <msg>]                                    ;
                                                                        ;
      => SetPos( <row>, <col> )                                         ;
       ; AAdd(                                                          ;
           GetList,                                                     ;
           _GET_( <varr>, <"varr">, <pic>, <{valid}>, <{when}> ):display();
             )                                                          ;
       ;ATail(Getlist):reader := {|x|x} ;
       ;ATail(Getlist):cargo := ArrayCreate(100) ;
      [; ATail(GetList):<msg>]

// I think the last part is a comment?

#command @ <row>, <col> zSAY <sayxpr>                                    ;
                        [<sayClauses,...>]                              ;
                        zGET <varr>                                       ;
                        [<getClauses,...>]                              ;
                                                                        ;
      => @ <row>, <col> DOSAY <sayxpr> [<sayClauses>]                     ;
       ; @ Row(), Col()+1 zGET <varr> [<getClauses>]


FUNCTION Start( ) AS VOID
LOCAL n := 1976 AS INT

@ 1,2 zGET n
@ 1,2 zSAY "ABC" zGET n

//Harbour's ppo:
/*
SetPos(1,2 ) ; AAdd(GetList,_GET_(n,"n",,,):display() ) ;ATail(Getlist):reader := {|x|x} ;ATail(Getlist):cargo := ArrayCreate(100)
DevPos(1,2 ) ; DevOutPict("ABC",);SetPos(Row(),Col()+1 ) ; AAdd(GetList,_GET_(n,"n",,,):display() ) ;ATail(Getlist):reader := {|x|x} ;ATail(Getlist):cargo := ArrayCreate(100)
*/

GLOBAL GetList := {} AS ARRAY
PROCEDURE SetPos(y,x)
? y,x
PROCEDURE DevPos(y,x)
? y,x
PROCEDURE DevOutPict(c,p)
? c,p
FUNCTION _GET_(a,b,c,d,e) CLIPPER
RETURN GetObject{a,b,c,d,e}

CLASS GetObject
CONSTRUCTOR(a,b,c,d,e)
? a,b,c,d,e
METHOD display() CLIPPER
? "display"
RETURN SELF
PROPERTY cargo AS ARRAY AUTO
PROPERTY reader AS CODEBLOCK AUTO
END CLASS

