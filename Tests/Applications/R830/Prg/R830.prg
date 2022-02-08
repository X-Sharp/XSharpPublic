#define TWO 2
#define THREE 3

FUNCTION Start( ) AS VOID
    #if (TWO ^ THREE <= 8) .or. (TRUE)
        #stdout Eight is 8
    	System.Console.WriteLine("Version " + __VERSION__+" or later")
	#else
        #error "Not eight"
	    System.Console.WriteLine("Older version")
	#endif
RETURN
