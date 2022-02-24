#define TEST 10
#if TEST == 10
#include "R834.xh"    
#endif


FUNCTION Start( ) AS VOID  
    // Comments
	Console.WriteLine(FromHeader())      
	//Console.WriteLine(INCORRECT_DEFINE)
	Console.WriteLine(Foo{}:ToString())
	Console.WriteLine(Bar{}:ToString())             
	Console.ReadLine()     
RETURN 

     
/*
    Multi line comments from the PRG file should be included
*/
     
CLASS Foo
    PROPERTY Name AS STRING GET Nameof(Foo)
    // this includes the ToString() method
    // Debugger should now also step into this method in the header file
    #include "R824a.xh"        
END CLASS

CLASS Bar                                  
    PROPERTY Name AS STRING GET Nameof(Bar)
    // this includes the ToString() method
    // Debugger should now also step into this method in the header file
   #include "R824a.xh"        
END CLASS   



