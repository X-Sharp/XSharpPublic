FUNCTION Start AS VOID
    LOCAL lresult  
    PRIVATE mresult
    PUBLIC _result
    LOCAL localvar1, localvar2
    PRIVATE privatevar1, privatevar2
    PUBLIC publicvar1, publicvar2
    publicvar1= "p1"
    publicvar2:= "p2"
    STORE 'a' TO privatevar1
    privatevar2:= 'b'  + 'c'   
    localvar1:= 12
    localvar2:= 4
    namevar:="localvar1"
    &namevar.:= custom{}
    ? "Test macrosubstitution object assign ", Type(namevar) , iif(Type(namevar)="O", "Ok", "Fail")
    &namevar.:= 6         
    ? "Test macrosubstitution assign ",  localvar1, iif(localvar1= 6, "Ok", "Fail li must be change Before 12 After 6")  
    
    lresult:= FALSE             
    lresult:= "asdf"            
    lresult:= datetime()        
    lresult:= 1                 
    store .f. TO lresult, mresult, _result 
    //call 1
    prueba ("localvar1",localvar1,localvar2,@lresult )       
    ? "Test local variable changed ina a procedure by ref with @" , "lresult=", lresult, "Type(""lresult"")="+Type("lresult"), iif(Type("lresult")="N" AND lresult=10, "OK", "Fail ""lresult"" must be changed to 10")
    xAssert(lresult=10)
    // call 2
    prueba ("privatevar1",privatevar1,privatevar2,REF mresult ) 
    ? "Test private variable changed in a procedure by ref"      , "mresult=", mresult, "Type(""mresult"")="+Type("mresult"), iif(Type("mresult")="C" AND mresult="abc" , "OK", "Fail ""mresult"" must be changed to ""abc"" ")
    //xAssert(mresult="abc")
    // call 3 
    prueba ("publicvar1",publicvar1,publicvar2,@_result )    
    ? "Test public variable changed in a procedure by ref with @", "_result=", _result, "Type(""_result"")="+Type("_result"), iif(Type("_result")= "C" AND _result="p1p2", "OK", "Fail ""_result"" must be ""p1p2"", not change in line before ")
    //xAssert(_result="p1p2")
    wait
    RETURN 
//************************************************
PROCEDURE prueba (pVarId,p1, p2, pout REF USUAL  )
  pout:= p1 + p2
  ? "Test visibility of variable " + pVarId + " inside a procedure" , iif(Type(pVarId)="U"," is not visible",  "is visible" )
  RETURN
  
  
PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF  
  
