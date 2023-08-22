FUNCTION Start() AS VOID

LOCAL oTestClassProper AS TestClass
LOCAL oTestClassObject AS OBJECT
LOCAL oTestClassObjectFromArray AS OBJECT

oTestClassProper := TestClass{}
oTestClassObject := TestClass{}
oTestClassObjectFromArray := TestClass{}

oTestClassProper:cb1 := {||TRUE}
oTestClassObject:cb1 := {||TRUE}

//When declared as a strongly typed variable, this works fine                        
IF Eval(oTestClassProper:cb1)
    Console.WriteLine("oTestClassProper is TRUE")
ENDIF 

//When this class is declared as an object, the codeblock always stays null 
TRY 
    IF Eval(oTestClassObject:cb1)
        Console.WriteLine("oTestClassObject is TRUE")
    ENDIF 
CATCH 
    Console.WriteLine("oTestClassObject:cb1 is NULL")
    THROW Exception{"oTestClassObject:cb1 is NULL"}
END TRY

//Interestingly, this works. Assinging the codeblock from an array. 
LOCAL aArray AS ARRAY
aArray := ArrayNew(1)
aArray[1] := {||TRUE}
oTestClassObjectFromArray:cb1 := aArray[1] 

TRY 
    IF Eval(oTestClassObjectFromArray:cb1)
        Console.WriteLine("oTestClassObjectFromArray:cb1 assigned from an Array is now TRUE")
    ENDIF 
CATCH 
    Console.WriteLine("oTestClassObjectFromArray:cb1 is NULL")
    THROW Exception{"oTestClassObjectFromArray:cb1 is NULL"}
END TRY


//Console.WriteLine("Press any key to continue...")
//Console.ReadKey()

CLASS TestClass

EXPORT cb1 AS CODEBLOCK

CONSTRUCTOR 
RETURN 

END CLASS


