#define CARGO_ELEMENTS 4
#translate ARRAY(<x>) => ArrayCreate(<x>)
#xtranslate :Messages    =>  :cargo\[2\]

#command @ <row>, <col> zSAY <xpr>                                       ;
                        [PICTURE <pic>]                                 ;
                        [COLOR <color>]                                 ;
                                                                        ;
      => DevPos( <row>, <col> )                                         ;
       ; DevOutPict(<xpr>, <pic> [, <color>] )


#command @ <row>, <col> zSAY <xpr>                                       ;
                        [COLOR <color>]                                 ;
                                                                        ;
      => DevPos( <row>, <col> )                                         ;
       ; DevOut( <xpr> [, <color>] )


/***
*  @..GET
*/

#command @ <row>, <col> zGET <var>                                      ;
                        [PICTURE <pic>]                                 ;
                        [VALID <valid>]                                 ;
                        [WHEN <when>]                                   ;
                        [SEND <msg>]                                    ;
                        [SEND <msg2>]                                   ;
                        [SEND <msg3>]                                   ;
                        [SEND <msg4>]                                   ;
                                                                        ;
      => SetPos( <row>, <col> )                                         ;
       ; AAdd(                                                          ;
           GetList,                                                     ;
           _GET_( <var>, <"var">, <pic>, <{valid}>, <{when}> ):display();
             )                                                          ;
       ;atail(Getlist):reader := {|x| zGEReader(x) } ;
       ;atail(Getlist):cargo := ARRAY(CARGO_ELEMENTS) ;
      [; ATail(GetList):<msg>]    ;
      [; ATail(GetList):<msg2>]   ;
      [; ATail(GetList):<msg3>]   ;
      [; ATail(GetList):<msg4>]

/***
*   @..SAY..GET
*/

#command @ <row>, <col> zSAY <sayxpr>                                    ;
                        [<sayClauses,...>]                              ;
                        zGET <var>                                       ;
                        [<getClauses,...>]                              ;
                                                                        ;
      => @ <row>, <col> SAY <sayxpr> [<sayClauses>]                     ;
       ; @ Row(), Col()+1 zGET <var> [<getClauses>]

// @..GET COLOR
#command @ <row>, <col> zGET <var>                                       ;
                        [<clauses,...>]                                 ;
                        COLOR <color>                                   ;
                        [<moreClauses,...>]                             ;
                                                                        ;
      => @ <row>, <col> zGET <var>                                       ;
                        [<clauses>]                                     ;
                        SEND colorDisp(<color>)                         ;
                        [<moreClauses>]



// MESSAGE DISPLAY
#command @ <row>, <col> zGET <var> ;
                  		[<clauses,...>]  ;
                  		MESSAGE <msg> ;
                  		[<moreClauses,...>]  ;
     =>  @ <row>, <col> zGET <var> ;
	                  	[<clauses>] ;
                                [<moreClauses>] ;
                                ;atail(getlist):Messages := <msg> 


// Step Get
#command @ <row>, <col> zGet <var>   ;
            [<clauses,...>]          ;
            STEP                     ;
            [<moreclauses,...>]      ;
            => @ <row>, <col> zGet <var> ;
            [<clauses>]                  ;
            SEND reader := {|oGet| zGeStepReader(oGet) } ;
            [<moreclauses>]



// GET & SAY
#command @ <row>, <col> zSAY <sayxpr>                                    ;
                        [<sayClauses,...>]                              ;
                        zGET <var>                                       ;
                        [<getClauses,...>]                              ;
                                                                        ;
      => @ <row>, <col> zSAY <sayxpr> [<sayClauses>]                     ;
       ; @ Row(), Col()+1 zGET <var> [<getClauses>]

