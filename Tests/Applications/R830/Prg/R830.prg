#define VERSION_MIN 3
#define TARGET_VER  1
FUNCTION Start( ) AS VOID                
    #if 1 ^ 2
    ? "Foo is defined"
    #endif
    #if TARGET_VER < VERSION_MIN  

        ? "You need to upgrade to version" + str(VERSION_MIN) 

     #endif 

 

     #if .F. 

     #stdout This never shows up   
     #else
     #stdout This is always shown because .F. = false

     #endif 

 

     #if .T. .AND. 1>0 .AND. "AA" < "BB" 

     #stdout This is always true 

     #endif 

 

     #if 1 < "2" 

     #stdout A numeric will be converted to string before the comparison. 

     #endif 

 

     #if 1 == .T. 

     #stdout A logic literal will be converted to numeric before the comparison. 

     #endif 

RETURN
