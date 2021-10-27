#command REPLACE [ <f1> WITH <x1> [, <fn> WITH <xn>] ]                  ;
         [FOR <lfor>]                                                    ;
         [WHILE <lwhile>]                                                ;
         [NEXT <nnext>]                                                  ;
         [RECORD <rec>]                                                 ;
         [<rest:REST>]                                                  ;
         [<noopt: NOOPTIMIZE>]                                          ;
         [ALL]                                                          ;
                                                                        ;
      => DbEval(                                                        ;
                 {||DbAutoLock(), _FIELD-><f1> := <x1> [, _FIELD-><fn> := <xn>], DbAutoUnLock()},    ;
                 <{lfor}>, <{lwhile}>, <nnext>, <rec>, <.rest.>, <.noopt.>;
               )


#command REPLACE <f1> WITH <v1> [, <fN> WITH <vN> ]                     ;
      => DbAutoLock(); _FIELD-><f1> := <v1> [; _FIELD-><fN> := <vN>]; DbAutoUnLock()



FUNCTION Start( ) AS VOID
TRY
    RddSetDefault("dbfcdx")
    DbCreate("test", {{"test","C",10,0}})
    USE test SHARED
    DbAppend()
    DbAppend()
    DbAppend()
    DbAppend()
    DbGoTop()

    REPLACE ALL Test with "abc"

CATCH e as Exception
? e:ToString()
ENDTRY
WAIT
