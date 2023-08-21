// 757. error XS9002: Parser: unexpected input 'NUMERIC'
// (it did compile cleanly before)
#pragma warnings(9043, off)
#region UDCs

#xtranslate :Messages    =>  :cargo\[2\]

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
                        [WHEN <WHEN>]                                   ;
                        [SEND <msg>]                                    ;
                                                                        ;
      => SetPos( <row>, <col> )                                         ;
       ; AAdd(                                                          ;
           GetList,                                                     ;
           _GET_( <VAR>, <"var">, <pic>, <{valid}>, <{WHEN}> ):display();
             )                                                          ;
       ;ATail(Getlist):reader := {|x| zGEReader(x) } ;
       ;ATail(Getlist):cargo := ARRAY(CARGO_ELEMENTS) ;
       ;ATail(Getlist):cbSaveValue := {|val|<VAR> := val} ; // cpc: added this line
      [; ATail(GetList):<msg>]




/***
*   @..SAY..GET
*/

#command @ <row>, <col> zSAY <sayxpr>                                    ;
                        [<sayClauses,...>]                              ;
                        zGET <VAR>                                       ;
                        [<getClauses,...>]                              ;
                                                                        ;
      => @ <row>, <col> SAY <sayxpr> [<sayClauses>]                     ;
       ; @ Row(), Col()+1 zGET <VAR> [<getClauses>]

/***
*   fancy GETs...
*/

// @..GET..RANGE (preprocessed to @..GET..VALID)

#command @ <row>, <col> zGET <var>                                       ;
                        [<clauses,...>]                                 ;
                        RANGE <lo>, <hi>                                ;
                        [<moreClauses,...>]                             ;
                                                                        ;
      => @ <row>, <col> zGET <VAR>                                       ;
                        [<clauses>]                                     ;
                        VALID {|_1| RangeCheck(_1,, <lo>, <hi>)}        ;
                        [<moreClauses>]


// @..GET COLOR
#command @ <row>, <col> zGET <var>                                       ;
                        [<clauses,...>]                                 ;
                        COLOR <color>                                   ;
                        [<moreClauses,...>]                             ;
                                                                        ;
      => @ <row>, <col> zGET <VAR>                                       ;
                        [<clauses>]                                     ;
                        SEND colorDisp(<color>)                         ;
                        [<moreClauses>]



// MESSAGE DISPLAY
#command @ <row>, <col> zGET <var> ;
                  		[<clauses,...>]  ;
                  		MESSAGE <msg> ;
                  		[<moreClauses,...>]  ;
     =>  @ <row>, <col> zGET <VAR> ;
	                  	[<clauses>] ;
                                [<moreClauses>] ;
                                ;ATail(getlist):Messages := <msg>


// GET & SAY
#command @ <row>, <col> zSAY <sayxpr>                                    ;
                        [<sayClauses,...>]                              ;
                        zGET <VAR>                                       ;
                        [<getClauses,...>]                              ;
                                                                        ;
      => @ <row>, <col> zSAY <sayxpr> [<sayClauses>]                     ;
       ; @ Row(), Col()+1 zGET <VAR> [<getClauses>]


// RADIO BUTTONS
#define RADIO_BUTTON Chr(7) // 
#xcommand @ <row>, <col> zGET <VAR>                              ;
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
         ; AAdd(getlist,zGERadioNEW({|x| iif(x == NIL, <VAR>, <VAR> := x) },    ;
                     <(VAR)>, <radios>,<{bWhen}>,<.horiz.>,;
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
         ; AAdd(getlist,zGEPushNew(<radios>, <{bWhen}>,<.horiz.>,<nBoxType>, <cColor>))

#endregion


#region helper code

GLOBAL GetList := {} AS ARRAY
FUNCTION zGERadioNEW() CLIPPER
RETURN GetObject{}
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
EXPORT cargo := {1,2,3} AS ARRAY
PROPERTY reader AS CODEBLOCK AUTO
END CLASS

#endregion

// error XS9002: Parser: unexpected input 'NUMERIC'
FUNCTION Start() AS VOID
	LOCAL u := NIL AS USUAL
	@1,2  zGet u RADIO {} NUMERIC MESSAGE "Select the payroll group" COLOR "a"
RETURN

