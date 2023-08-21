// 692. Compiler hungs with UDC

#command MYSUM  [<x1> [, <xn>]  TO  <v1> [, <vn>] ]                         ;
         [FOR <FOR>]                                                    ;
         [WHILE <WHILE>]                                                ;
         [NEXT <NEXT>]                                                  ;
         [RECORD <rec>]                                                 ;
         [<rest:REST>]                                                  ;
         [ALL]                                                          ;
                                                                        ;
      => <v1> := [ <vn> := ] 0                                          ;
       ; DbEval(                                                        ;
                 {|| <v1> := <v1> + <x1> [, <vn> := <vn> + <xn> ]},     ;
                 <{FOR}>, <{WHILE}>, <NEXT>, <rec>, <.rest.>            ;
               )

FUNCTION Start( ) AS VOID
	LOCAL mysum AS INT
	mysum := 123
	? mysum
	mysum ++
	mysum += 1
	IF mysum == 1
		mysum := mysum + 1
	ENDIF
RETURN

CLASS TestClass
	METHOD mysum() AS VOID
		SELF:mysum()
		mysum()
	RETURN
END CLASS
