
FUNCTION Start( ) AS VOID
	LOCAL oValue AS ParentClass   
	LOCAL uValue AS USUAL
	oValue := ParentClass{}
	? oValue:Caption          
	xAssert(oValue:Caption == "default")
	oValue:Caption := "New Caption"
    ? oValue:Caption	
	xAssert(oValue:Caption == "New Caption")
    oValue:Dispatch(MyChildEvent{})

	oValue := ChildClass{}
	? oValue:Caption     
	uValue := "New Caption"
	oValue:Caption := uValue
	xAssert(oValue:Caption == "Child New Caption")
    ? oValue:Caption	
    
	oValue := GrandChildClass{}
	? oValue:Caption     
	uValue := "New Caption"
	oValue:Caption := uValue
	xAssert(oValue:Caption == "Child GrandChild New Caption")
    ? oValue:Caption	
    oValue:Dispatch(MyChildEvent{})	
RETURN                                                    

[XSharp.Internal.TypesChanged];
CLASS ParentClass            
    PROTECTED _caption AS STRING
    CONSTRUCTOR()
        _caption := "default"
    ACCESS Caption  AS STRING
        RETURN _caption
    ASSIGN Caption (uCaption AS STRING)       
        _caption := uCaption

    METHOD Dispatch(myevent AS MyEvent) AS USUAL     
        ? myevent
        RETURN NIL
END CLASS    


CLASS ChildClass INHERIT ParentClass
    ASSIGN Caption(uValue)
        SUPER:Caption  := "Child " + uValue        
        RETURN     
    
END CLASS    



CLASS GrandChildClass INHERIT ChildClass            
    ASSIGN Caption(uValue )
        SUPER:Caption  := "GrandChild " + uValue        
        RETURN     
     METHOD Dispatch(myevent AS MyEvent) AS USUAL
         SUPER:Dispatch(myevent)
        RETURN NIL        
END CLASS    



CLASS MyEvent
END CLASS    

CLASS MyChildEvent INHERIT MyEvent
END CLASS    


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
