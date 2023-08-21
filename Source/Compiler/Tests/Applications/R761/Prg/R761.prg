
FUNCTION Start( ) AS VOID
	LOCAL oValue AS ParentClass   
	LOCAL uValue AS USUAL
	oValue := ParentClass{}
    oValue:Dispatch(MyEvent{})
	oValue := ChildClass{}
    oValue:Dispatch(MyEvent{})    
	oValue := GrandChildClass{}
    oValue:Dispatch(MyEvent{})
RETURN                                                    

[XSharp.Internal.TypesChanged];
CLASS ParentClass            
    CONSTRUCTOR()
    METHOD Dispatch(event AS MyEvent) 
        RETURN SELF
END CLASS    


CLASS ChildClass INHERIT ParentClass
    
END CLASS    



CLASS GrandChildClass INHERIT ChildClass            
    METHOD Dispatch(event) 
        RETURN SELF
END CLASS    


CLASS MyEvent
END CLASS    
