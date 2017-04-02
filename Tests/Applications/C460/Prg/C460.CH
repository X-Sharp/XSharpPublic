// Translates for PASSWORD reader
#xtranslate :PassWord    =>  :cargo

#xtranslate :Messages    =>  :cargo\[2\]
#xtranslate :GetType     =>  :cargo\[3\]
#xtranslate :GetPostEval =>  :cargo\[4\]
#define CARGO_ELEMENTS 4

// #translates for radio buttons sub-class
#translate  :radioCargo     => :cargo\[1\]
#translate  :radioGets      => :cargo\[1, 2\]
#translate  :radioRow(<x>)  => :cargo\[1, 2,<x>,1\]
#translate  :radioCol(<x>)  => :cargo\[1, 2,<x>,2\]
#translate  :radioDisp(<x>) => :cargo\[1, 2,<x>,3\]
#translate  :radioGsb(<x>)  => :cargo\[1, 2,<x>,4\]
#xtranslate :radioValue(<x>)=> :cargo\[1, 2,<x>,5\]
#translate  :radioTRow      => :cargo\[1, 3\]
#translate  :radioBRow      => :cargo\[1, 4\]
#translate  :radioLCol      => :cargo\[1, 5\]
#translate  :radioRCol      => :cargo\[1, 6\]
#translate  :radioBoxT      => :cargo\[1, 7\]
#translate  :radioHori      => :cargo\[1, 8\]
#translate  :radioRCPos     => :cargo\[1, 9\]
#translate  :radioTGsB      => :cargo\[1,10\]
#translate  :RadioTitle     => :cargo\[1,11\]
#xtranslate :RadioChanged   => :cargo\[1,12\]

#define RADIO_GETS_IVARS   5
#define RADIO_NUM_IVARS   12


// #translates for push buttons sub-class
#xtranslate  :PushCargo     => :cargo\[1\]
#xtranslate  :PushGets      => :cargo\[1, 2\]
#xtranslate  :PushRow(<x>)  => :cargo\[1, 2,<x>,1\]
#xtranslate  :PushCol(<x>)  => :cargo\[1, 2,<x>,2\]
#xtranslate  :PushDisp(<x>) => :cargo\[1, 2,<x>,3\]
#xtranslate  :PushExe(<x>)  => :cargo\[1, 2,<x>,4\]
#xtranslate  :PushTRow      => :cargo\[1, 3\]
#xtranslate  :PushBRow      => :cargo\[1, 4\]
#xtranslate  :PushLCol      => :cargo\[1, 5\]
#xtranslate  :PushRCol      => :cargo\[1, 6\]
#xtranslate  :PushBoxT      => :cargo\[1, 7\]
#xtranslate  :PushHori      => :cargo\[1, 8\]
#xtranslate  :PushRCPos     => :cargo\[1, 9\]
#xtranslate  :PushWidth     => :cargo\[1,10\]
#xtranslate  :PushHotKeys   => :cargo\[1,11\]

#define Push_GETS_IVARS 4
#define Push_NUM_IVARS  11


// #translates for combo/drop get pseudo sub-class
#xtranslate :dbComboCargo  => :cargo\[1\]
#xtranslate :dbdropOnEnter => :cargo\[1,1\]
#xtranslate :Alias         => :cargo\[1,2\]
#xtranslate :oBrowse       => :cargo\[1,3\]
#xtranslate :dbComboBlock  => :cargo\[1,4\]
#xtranslate :ReturnValue   => :cargo\[1,5\]
#xtranslate :IndexTag      => :cargo\[1,6\]
#xtranslate :xTop          => :cargo\[1,7\]
#xtranslate :xBottom       => :cargo\[1,8\]
#xtranslate :dbcDisplayVar => :cargo\[1,9\]
#xtranslate :DisplayValue  => :cargo\[1,10\]
#xtranslate :DropScr       => :cargo\[1,11\]
#xtranslate :Strict        => :cargo\[1,12\]
#xtranslate :HotKeys       => :cargo\[1,13\]
#xtranslate :dbcBaseFilter => :cargo\[1,14\]
#xtranslate :dbcFor        => :cargo\[1,15\]
#xtranslate :dbcWhile      => :cargo\[1,16\]
#xtranslate :dbcArrBrowse  => :cargo\[1,17\]
#xtranslate :dbcTBTop      => :cargo\[1,18\]
#xtranslate :dbcTBLeft     => :cargo\[1,19\]
#xtranslate :dbcTBBottom   => :cargo\[1,20\]
#xtranslate :dbcTBRight    => :cargo\[1,21\]
#xtranslate :dbcReturnTag  => :cargo\[1,22\]
#xtranslate :dbcBrowseColor=> :cargo\[1,23\]
#xtranslate :dbcFound      => :cargo\[1,24\]
#xtranslate :dbcValid      => :cargo\[1,25\]
#xtranslate :dbcNeverDrop  => :cargo\[1,26\]
#xtranslate :dbcEmptyAllowed => :cargo\[1,27\]

#define DB_COMBO_NUM_IVARS 27

// #translates for memo field sub-class
#xtranslate :MemoTop      => :cargo\[1,1\]
#xtranslate :MemoLeft     => :cargo\[1,2\]
#xtranslate :MemoRight    => :cargo\[1,3\]
#xtranslate :MemoBottom   => :cargo\[1,4\]
#xtranslate :MemoVar      => :cargo\[1,5\]
#xtranslate :MemoGsb      => :cargo\[1,6\]
#xtranslate :MemoMenu     => :cargo\[1,7\]

#define CARGO_MEMO_IVARS 7

#xtranslate :listItems  => :cargo\[1,1\]
#xtranslate :listGsb    => :cargo\[1,2\]
#xtranslate :listBrowse => :cargo\[1,3\]
#xtranslate :listBottom => :cargo\[1,4\]
#xtranslate :listRight  => :cargo\[1,5\]

#define LIST_NUM_IVARS 5


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

// @..GET..RANGE (preprocessed to @..GET..VALID)

#command @ <row>, <col> zGET <var>                                       ;
                        [<clauses,...>]                                 ;
                        RANGE <lo>, <hi>                                ;
                        [<moreClauses,...>]                             ;
                                                                        ;
      => @ <row>, <col> zGET <var>                                       ;
                        [<clauses>]                                     ;
                        VALID {|_1| RangeCheck(_1,, <lo>, <hi>)}        ;
                        [<moreClauses>]


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


// PASSWORD READER
#command @ <row>, <col> zGet <var> ;
                  		[<clauses,...>]  ;
                                PASSWORD ;
                  		[<moreClauses,...>]  ;
     =>  @ <row>, <col> zGET <var> ;
	                  	[<clauses>] ;
                                SEND reader := {|oGet| PWReader(oGet)  } ;
                                [<moreClauses>] 
  



// Step Get
#command @ <row>, <col> zGet <var>   ;
            [<clauses,...>]          ;
            STEP                     ;
            [<moreclauses,...>]      ;
            => @ <row>, <col> zGet <var> ;
            [<clauses>]                  ;
            SEND reader := {|oGet| zGeStepReader(oGet) } ;
            [<moreclauses>]


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


// DROP DOWN PICK LIST
#xcommand @ <row>, <col> zGET <var>                               ;
                        DROP <drops>                              ;
                        [STRICT <lStrict>]                        ;
                        [BOX_TYPE <nBoxType>]                     ;
                        [VALID <valid>]                           ;
                        [WHEN <when>]                             ;
                        [PICTURE <cPic>]                          ;
                        [<lDropOnEnter: ALWAYS>]                  ;
                        [COLOR <cColor>]                          ;
                        [EMPTY_ALLOWED <lEmptyAllowed> ]          ;
                                                                  ;
      =>                                                          ;
         SetPos(<row>, <col>)                                     ;
         ; Aadd(GetList,                                          ;
              zGEDropNew( {|x| iif(x == nil, <var>, <var> := x) }, ;
                          <(var)>, <drops>, <{when}>, <nBoxType>, ;
                          <.lDropOnEnter.>, <{valid}>, <cColor>,;
                          <lStrict>, <cPic>, <lEmptyAllowed> ))


// GET & SAY
#command @ <row>, <col> zSAY <sayxpr>                                    ;
                        [<sayClauses,...>]                              ;
                        zGET <var>                                       ;
                        [<getClauses,...>]                              ;
                                                                        ;
      => @ <row>, <col> zSAY <sayxpr> [<sayClauses>]                     ;
       ; @ Row(), Col()+1 zGET <var> [<getClauses>]


// RADIO BUTTONS
#define RADIO_BUTTON Chr(7) // 
#xcommand @ <row>, <col> zGET <var>                              ;
                        RADIO <radios,...>                       ;
                        [<lReturn: NUMERIC>]                     ;
                        [<horiz: HORIZONTAL>]                    ;
			[BOX_TYPE <nBoxType>]                    ;
                        [WHEN <bWhen>]                           ;
                        [VALID <bValid> ]                        ;
                        [TITLE <cTitle>]                         ;
                        [COLOR <cColor>]                         ;
                                                                 ;
      =>                                                         ;
         SetPos(<row>, <col>)                                    ;
         ; aadd(getlist,zGERadioNEW({|x| iif(x == nil, <var>, <var> := x) },    ;
                     <(var)>, <radios>,<{bWhen}>,<.horiz.>,;
                     <nBoxType>,<.lReturn.>,<cTitle>, getlist,;
                     <{bValid}>, <cColor> ))


// PUSH BUTTONS
#xcommand @ <row>, <col> zPUSH <radios,...>                      ;
			[<horiz: HORIZONTAL>]                    ;
			[BOX_TYPE <nBoxType>]                    ;
                        [WHEN <bWhen>]                           ;
                        [COLOR <cColor>]                         ;
      =>                                                         ;
         SetPos(<row>, <col>)                                    ;
         ; aadd(getlist,zGEPushNew(<radios>, <{bWhen}>,<.horiz.>,<nBoxType>, <cColor>))




// MEMO Reader or long character string reader.
#xcommand @ <row>, <col> zGET <cString> MEMO TO <brow>, <rcol>     ;
               [WHEN <bWhen> ]                                     ;
               [VALID <bValid>]                                    ;
               [MENU  <bMemo> ]                                    ;
               =>                                                  ;
               aadd(getlist, zGEMemoNew(                           ;
                  {|x| iif(x == nil, <cString>, <cString> := x) }, ;
                  <cString>, <(cString)>,                          ;
                  <{bWhen}>, {<row>, <col>, <brow>, <rcol> },[<{bValid}>], <{bMemo}>))
/*
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
*/
// LIST BOX
#command @ <row>, <col> zGET <var>                                  ;
                        LIST <drops>                                ;
                        [WHEN  <bWhen> ]                            ;
                        [VALID <bValid>]                            ;
                        [BOX_TYPE <nBoxType>]                       ;
                                                                    ;
      =>                                                            ;
         SetPos(<row>, <col>)                                       ;
         ; Aadd(GetList,                                            ;
              zGeListNew({|x| iif(x == nil, <var>, <var> := x) },   ;
                          <(var)>, <drops>, ,,getlist, <nBoxType>,  ;
                          <{bValid}>,<{bWhen}> ))

// LIST BOX
#command @ <nTop>, <nLeft> TO <nBottom>, <nRight> zGET <var>        ;
                        LIST <drops>                                ;
                        [WHEN  <bWhen> ]                            ;
                        [VALID <bValid>]                            ;
                        [BOX_TYPE <nBoxType>]                       ;
                                                                    ;
      =>                                                            ;
         SetPos(<nTop>, <nLeft>)                                    ;
         ; Aadd(GetList,                                            ;
              zGEListNew({|x| iif(x == nil, <var>, <var> := x) },   ;
                          <(var)>, <drops>, <nBottom>, <nRight>, getlist,;
                          <nBoxType>,<{bValid}>,<{bWhen}>  ))


#xcommand zBOXGET <var>             ;
          [ PROMPT <prom> ]         ;
          [ PICTURE <pict>  ]      ;
          [ VALID <valid>   ]      ;
          [ BOXCOLOR <boxcolor> ]  ;
          [ COLOR <color>   ]      ;
          [ ROW <row>       ]      ;
          [ COLUMN <column> ]      ;
          [ TITLE <title> ]        ;
          [ BOX_TYPE <nBoxType>]   ;
          [ NOTES <aNotes>]        ;
                               =>  ;
         zSBoxGet(<prom>, <aNotes>, <row>, <column>,           ;
            getnew( maxrow() + 1, maxcol() + 1,               ;
            { | x | if(pcount() = 0, <var>, <var> := x ) },   ;
            <(var)>, <pict>, <color> ), <{valid}>, <boxcolor>,;
            <nBoxType>, <title> )


///////////////////////////////////////
/////////////////                  ////
///////////////////////////////////////
// zREAD will return the value of the key used
// to exit the read.
//
// zREAD will validate all the gets
#xcommand zREAD [<nKey>] [NOVALID <nNoValid>] [MESSAGE <aMessage>] [<lConfirm: CONFIRM>]=> ;
          [zMessage(<aMessage>);];
          ReadModal( getlist) ;
          ; zGeEscapeConfirm(<.lConfirm.>);
          ;[<nKey> := ] zGetValid(getlist,<nNoValid>,.t.) 

