﻿USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text


FUNCTION Start() AS VOID STRICT

Console.WriteLine("Hello World!")
Console.WriteLine()

TRY
    LOCAL myArray := { 1, 2, 3 ,4,5} AS ARRAY

    Console.WriteLine("This works - explicit type")
    LOCAL testFunc:= {ow AS OBJECT, a AS OBJECT => TRUE} AS Func<OBJECT, OBJECT, LOGIC>
    myArray[1] := testFunc
    Console.WriteLine("Assignment 1 succeeded")
    Console.WriteLine()
    Console.WriteLine()
    Console.WriteLine()

    Console.WriteLine("This works - explicit cast before assignment")
    myArray[2] := (Func<OBJECT, OBJECT, LOGIC>){ow AS OBJECT, a AS OBJECT => TRUE}
    Console.WriteLine("Assignment 2 succeeded")
    Console.WriteLine()
    Console.WriteLine()
    Console.WriteLine()


    LOCAL FuncWithType := {ow AS OBJECT, a AS OBJECT => TRUE} AS OBJECT
    myArray[3] := FuncWithType
    Console.WriteLine("Assignment 3 succeeded")
    Console.WriteLine()
    Console.WriteLine()
    Console.WriteLine()


    LOCAL FuncNoType := {ow AS OBJECT, a AS OBJECT => TRUE}  // Implicit USUAL
    myArray[4] := FuncNoType
    Console.WriteLine("Assignment 4 succeeded")
    Console.WriteLine()
    Console.WriteLine()
    Console.WriteLine()

    VAR FuncVar := {ow AS OBJECT, a AS OBJECT => TRUE}  // Implicit USUAL
    myArray[5] := FuncVar
    Console.WriteLine("Assignment 5 succeeded")
    Console.WriteLine()
    Console.WriteLine()
    Console.WriteLine()
    
    FOREACH var item IN MyArray
        var o := item ASTYPE Object
        LOCAL t as Func<OBJECT, OBJECT, LOGIC>
        t := o
        ? t:Invoke(1, 2)
        
    NEXT
    WAIT
    

CATCH ex AS Exception
    Console.WriteLine("Exception: " + ex:Message)
    Console.WriteLine("Exception Type: " + ex:GetType():FullName)
    Console.WriteLine("Stack Trace: " + ex:StackTrace)
END TRY

Console.WriteLine()
Console.WriteLine("Press any key to continue...")
Console.ReadKey()

RETURN
