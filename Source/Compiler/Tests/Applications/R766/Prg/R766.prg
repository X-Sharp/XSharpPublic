
FUNCTION Start( ) AS VOID
	VAR oTest := Test{}  
	LOCAL init := 123 AS LONG
	? init        
	? init()
	? oTest:Foo    
	//oTest:Foo := "new value"   // throws a compiler error
	? oTest:Foo    
	? oTest:Bar
RETURN
FUNCTION Init() AS datetime
    RETURN DateTime.Now

CLASS Test                  
    PRIVATE INITONLY _Bar AS LONG
    PROPERTY Bar AS LONG 
        GET  => _Bar 
        INIT => _Bar := Value
    END PROPERTY
    PROPERTY Foo AS STRING AUTO GET INIT
    CONSTRUCTOR()
        Foo := "default"   
        Bar := 42
END CLASS        


// This static class is needed when not compiling against .Net 5
BEGIN NAMESPACE System.Runtime.CompilerServices
    INTERNAL STATIC CLASS IsExternalInit 
    END CLASS
END NAMESPACE
