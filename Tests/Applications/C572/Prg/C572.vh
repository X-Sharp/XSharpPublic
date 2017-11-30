// Application : C572
// C572.vh , Created : 13-10-2017   15:20
// User : robert

#ifdef FILE1
FUNCTION File1() AS STRING   
	? "File1 start"
	RETURN "File1"
#endif

#ifdef FILE2
FUNCTION File2() AS STRING
	? "File2 start"
	RETURN "File2"             
#endif
	
