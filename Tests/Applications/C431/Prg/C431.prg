// 431. Some issues with #command
#command MyCommand1 <identifier> DOLOCAL  	 => LOCAL <identifier> AS INT
#command MyCommand2 <identifier> DOLOCAL <t> => LOCAL <identifier> AS <t>
#command propcommand <n> as <t> GETSET <v> 	 => PRIVATE <v> as <t>; property <n> as <t>; get; return self:<v>; end get; set; if self:<v> <> value; self:<v> := value; endif; end set; end property
#command PROPERTY <n> as <t> GETSET <v> 	 => PRIVATE <v> as <t>;;
	PROPERTY <n> AS <t>;;
	GET;;
    	RETURN SELF:<v>;;
    END GET;;
    SET;;
	    IF SELF:<v> <> VALUE;;
    	 	SELF:<v> := VALUE;;
    	ENDIF;;
    END SET;;
    END PROPERTY

FUNCTION Start( ) AS VOID
	MyCommand1 n DOLOCAL
	n := 10
	? n
	
	MyCommand2 n2 DOLOCAL STRING
	n2 := "asd"
	? n2
	
	TestClass{}
RETURN

CLASS TestClass
	propcommand testprop1 AS INT GETSET _n1
	PROPERTY testprop2 AS INT GETSET _n2
		
	CONSTRUCTOR()
		SELF:_n1 := 1
		SELF:_n2 := 2
		
		? SELF:testprop1
		? SELF:testprop2
	RETURN
	
END CLASS

