#xtranslate :Messages    =>  :cargo\[2\]
#xtranslate :GetType     =>  :cargo\[3\]
#xtranslate :GetPostEval =>  :cargo\[4\]
#define CARGO_ELEMENTS 4


/***
*  @..SAY
*/

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
                                                                        ;
      => SetPos( <row>, <col> )                                         ;
       ; AAdd(                                                          ;
           GetList,                                                     ;
           _GET_( <var>, <"var">, <pic>, <{valid}>, <{when}> ):display();
             )                                                          ;
       ;atail(Getlist):reader := {|x| zGEReader(x) } ;
       ;atail(Getlist):cargo := ARRAY(CARGO_ELEMENTS) ;
      [; ATail(GetList):<msg>]




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

/***
*   fancy GETs...
*/


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


// DATABASE COMBO BOX
#xcommand @ <row>, <col> zGET <var> COMBO                         ;
                        BROWSE < aBrowse_ >                       ;
                        [COLOR <color> ]                          ;
                        [ALIAS <cAlias>]                          ;
                        [RETURN_FIELD < xRetFld>]                 ;
                        [DISPLAY_TAG <cTag>]                      ; 
                        [VALID <valid>]                           ;
                        [WHEN <when>]                             ;
                        [<lDropOnEnter: ALWAYS>]                  ;
                        [XTOP <xTop> ]                            ;
                        [XBOTTOM <xBottom> ]                      ;
                        [FOR <for> ]                              ;
                        [WHILE <while> ]                          ;
                        [BASE_FILTER <cBaseFilter> ]              ;
                        [DISPLAY_FIELD <xDispFld> ]               ;
                        [RETURN_TAG <cKeyTag>  ]                  ;
                        [STRICT <lStrict> ]                       ;
                        [EMPTY_ALLOWED <lEmptyAllowed> ]          ;
                        [HOT_KEYS <aHotKeys_> ]                   ;
                        [PICTURE <cPicture>]                      ;
                        [WIDTH <nWidth>]                          ;
                        [HEIGHT <nHeight> ]                       ;
                        [POST_EVAL <bPostEval>]                   ;
      =>                                                          ;
         SetPos(<row>, <col>)                                     ;
         ; Aadd(GetList,                                          ;
              zGEDBComboNew( <(var)>, <var>,;
                            {|x| iif(x == nil, <var>, <var> := x) };
                            , <{when}>, <{valid}>,  <.lDropOnEnter.>,;
                            <cAlias>, <cTag>, <xTop>, <xBottom>, ;
                            <{for}>, <{while}>, <cBaseFilter>, ;
                            <xRetFld>, < aBrowse_>,<xDispFld>,;
                            <cKeyTag>, <lStrict>,;
                            <aHotKeys_>, <cPicture>, <nWidth>,;
                            <nHeight>, <color>,[<{bPostEval}>], <lEmptyAllowed> ) ) 


// GET & SAY
#command @ <row>, <col> zSAY <sayxpr>                                    ;
                        [<sayClauses,...>]                              ;
                        zGET <var>                                       ;
                        [<getClauses,...>]                              ;
                                                                        ;
      => @ <row>, <col> zSAY <sayxpr> [<sayClauses>]                     ;
       ; @ Row(), Col()+1 zGET <var> [<getClauses>]


// CHECK BOX
#command @ <row>, <col> zGET <var> CHECKBOX <cStr>                   ;
                        [<lRight: RIGHT>]                            ;
                        [ON <xOn>]                                   ;
                        [OFF <xOff>]                                 ;
                        [BOX <cBox>]                                 ;
                        [CHECK <cCheck>]                             ;
                        [WHEN  <bWhen> ]                             ;
                        [VALID <bValid>]                             ;
                        [POST_EVAL <bPostEval>]                      ;
                                                                     ;
      =>                                                             ;
         SetPos(<row>, <col>)                                        ;
         ; Aadd(GetList,                                             ;
                zGECheckNew({|x| iif(x == nil, <var>, <var> := x) }, ;
                     <(var)>, <cStr>,<.lRight.>,<cBox>,<xOn>,        ;
                     <xOff>,<cCheck>, <{bWhen}>, <{bValid}>,         ;
                     [<{bPostEval}>]) )

// Filler group codes
#command @<Row>,<Col> zGet <cVar> COMBO_FILGRP [STRICT <lStrict>]       ;
                                               [EMPTY_ALLOWED <lEmpty>] ;
                                               [COLOR <cColor>]         ;
                                               [FOR <bFor>]             ;
                                               [WHEN <bWhen>]           ;
                                               [VALID <bValid>] =>      ;
          @<Row>,<Col> zGet <cVar>                                      ;
          COMBO BROWSE  {{"GrpCode" ,"Group"      } ,                   ;
                         {"Descript","Description"}}                    ;
          ALIAS          "FilGrp"                                       ;
          DISPLAY_FIELD  "GrpCode"                                      ;
          DISPLAY_TAG    "FilGrpGC"                                     ;
          RETURN_FIELD   "GrpCode"                                      ;
          RETURN_TAG     "FilGrpGC"                                     ;
          PICTURE        "@!"                                           ;
          WIDTH          39                                             ;
          HEIGHT         9                                              ;
          [STRICT        <lStrict>]                                     ;
          [EMPTY_ALLOWED <lEmpty>]                                      ;
          [COLOR         <cColor>]                                      ;
          [FOR           <bFor>]                                        ;
          [WHEN          <bWhen>]                                       ;
          [VALID         <bValid>]                                      ;
          MESSAGE        "Select the filler group code"
          
// SSUsers
#Command @<Row>,<Col> zGet <cVar> COMBO_USER [STRICT <lStrict>]           ;
                                             [COLOR <Color>]              ;
                                             [FOR   <bFor>]               ;
                                             [VALID <bValid>]             ;
                                             [EMPTY_ALLOWED <lEmpty>]     ;
                                             [WHEN <bWhen>] =>            ;
         @<Row>,<Col> zGet <cVar>                                         ;
         COMBO BROWSE   {{"UserId","User ID"},{"Name","User"}}            ;
         ALIAS          "SSUsers"                                         ;
         RETURN_FIELD   "UserId"                                          ;
         RETURN_TAG     "SSUserSU"                                        ;
         DISPLAY_FIELD  "UserId"                                          ;
         DISPLAY_TAG    "SSUserSU"                                        ;
         XTOP           zGetSysId()                                       ;
         XBOTTOM        zGetSysId()                                       ;
         BASE_FILTER    zGetSysId()                                       ;
         WHILE          {|| SSUsers->SysId == zGetSysId()}                ;
         WIDTH          36                                                ;
         HEIGHT         10                                                ;
         PICTURE        "!!!!!!!!!!"                                      ;
         [EMPTY_ALLOWED <lEmpty>]                                         ;
         [STRICT        <lStrict>]                                        ;
         [COLOR         <Color>  ]                                        ;
         [FOR           <bFor>   ]                                        ;
         [VALID         <bValid>]                                         ;
         [WHEN          <bWhen>]                                          ;
         MESSAGE        "Enter the user id"

