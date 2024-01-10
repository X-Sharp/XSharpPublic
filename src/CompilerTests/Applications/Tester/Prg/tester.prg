#command XSUM  <x1> [, <xn>]  TO  <v1> [, <vn>]                         ;
         [FOR <lfor>]                                                   ;
         [WHILE <lwhile>]                                               ;
         [NEXT <nnext>]                                                 ;
         [RECORD <rec>]                                                 ;
         [<rest:REST>]                                                  ;
         [<noopt: NOOPTIMIZE>]                                          ;
         [ALL]                                                          ;
                                                                        ;
      => <v1> := [ <vn> := ] 0                                          ;
       ; DbEval(                                                        ;
               {|| <v1> += <x1> [, <vn> += <xn> ]},                     ;
               <{lfor}>, <{lwhile}>, <nnext>, <rec>, <.rest.>, <.noopt.>;
               )

FUNCTION Start() AS VOID STRICT
local b
field a
XSUM a TO b
