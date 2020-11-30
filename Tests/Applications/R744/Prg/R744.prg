// R744 Constructor Chaining
// the constructor inside TestInherited1 calls the parameterless constructor of System.Object
// and skips the constructors of the Test level. So the field val is not initialized.
FUNCTION Start() AS VOID STRICT
	Console.WriteLine("No initialization of fields. No Warning or error of invalid super call")
    TestInherited1{""}
	
	Console.WriteLine("Initialization of fields in the base class is called after the Super call")
	
	TestInherited2{""}
	
    Wait
    
    RETURN	
    
CLASS Test

HIDDEN val := "initialized" AS STRING

PROTECTED METHOD Print() AS VOID STRICT
	Console.WriteLine(val)
	
	RETURN	

CONSTRUCTOR(test1 AS STRING) AS VOID STRICT
	Print()
	RETURN 


CONSTRUCTOR(test1 AS LONG) AS VOID STRICT
	Print()
	RETURN 
	   
	   
END CLASS

CLASS TestInherited1 INHERIT Test

CONSTRUCTOR(test1 AS STRING) AS VOID STRICT
	
	Print()
	
	
	SUPER()  //invalid super call. This should report an error
	
	RETURN 
	   
END CLASS

CLASS TestInherited2 INHERIT Test

CONSTRUCTOR(test1 AS STRING) AS VOID STRICT
	
	Print()
	
	SUPER(test1)
	
	RETURN 
	   
END CLASS
